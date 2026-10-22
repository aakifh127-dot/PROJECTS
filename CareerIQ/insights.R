# =====================================================================
# insights.R
# Extra analytics layered on top of the trained model (no re-training
# needed, works with your existing models/trained_model.rds):
#   * percentile rank and readiness level
#   * gap_impact()      - probability gain from closing each skill gap
#   * similar_students()- nearest neighbours in the dataset
#   * goal_plan()       - greedy "what is the shortest path to X%?" planner
#   * build_report_html()- downloadable one-page student report
# =====================================================================

if (!exists("FEATURE_INFO")) source("helper_functions.R")
if (!exists("predict_prob"))  source("recommendation_engine.R")

# ---- Percentile + readiness level -------------------------------------
percentile_rank <- function(x, ref) {
  ref <- ref[!is.na(ref)]
  if (length(ref) == 0 || is.na(x)) return(NA_real_)
  round(100 * mean(ref <= x))
}

readiness_level <- function(p) {
  if (p >= 0.85)      list(name = "Placement Star",  icon = "star",       tone = "good",
                           blurb = "Top-tier profile. Focus on choosing the best offer.")
  else if (p >= 0.70) list(name = "Placement Ready", icon = "rocket",     tone = "good",
                           blurb = "Strong profile. Polish interviews and start applying.")
  else if (p >= 0.50) list(name = "On The Rise",     icon = "chart-line", tone = "warn",
                           blurb = "Good momentum. A few targeted upgrades will tip the odds.")
  else if (p >= 0.30) list(name = "Building Up",     icon = "tools",      tone = "warn",
                           blurb = "Solid base. Follow the gap plan to climb quickly.")
  else                list(name = "Getting Started", icon = "seedling",   tone = "bad",
                           blurb = "Every plan starts here. Use the Goal Planner for a route.")
}

# ---- Impact of closing each gap ------------------------------------------
# For every feature below its benchmark: set it to the benchmark and see
# how much the model's probability would change (percentage points).
gap_impact <- function(bundle, row) {
  base <- predict_prob(bundle$model, row)
  out <- lapply(seq_len(nrow(FEATURE_INFO)), function(i) {
    f <- FEATURE_INFO$feature[i]
    v <- as.numeric(row[[f]]); b <- FEATURE_INFO$benchmark[i]
    below <- if (f == "Number_of_Backlogs") v > b else v < b
    if (!below) return(NULL)
    r2 <- row; r2[[f]] <- b
    data.frame(Feature = f, Skill = FEATURE_INFO$label[i], From = v, To = b,
               Gain = 100 * (predict_prob(bundle$model, r2) - base),
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out)
  if (is.null(d)) return(data.frame(Feature = character(0), Skill = character(0),
                                    From = numeric(0), To = numeric(0), Gain = numeric(0)))
  d <- d[order(-d$Gain), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# ---- Students like you ------------------------------------------------------
similar_students <- function(df, row, k = 15) {
  X   <- as.matrix(df[, NUMERIC_FEATURES, drop = FALSE])
  x   <- as.numeric(unlist(row[1, NUMERIC_FEATURES]))
  rng <- pmax(FEATURE_INFO$max - FEATURE_INFO$min, 1)
  D   <- sweep(sweep(X, 2, x, "-"), 2, rng, "/")
  dist <- sqrt(rowSums(D^2))
  idx <- order(dist)[seq_len(min(k, nrow(df)))]
  nn  <- df[idx, , drop = FALSE]
  nn$Similarity <- round(100 * (1 - dist[idx] / max(dist[idx], 1e-9) * 0.5), 0)
  placed <- nn$Placement_Status == POSITIVE_CLASS
  list(
    neighbours  = nn,
    placed_rate = mean(placed),
    avg_package = if (any(placed)) mean(nn$Placement_Package[placed]) else NA_real_,
    avg_cgpa    = mean(nn$CGPA),
    k           = nrow(nn)
  )
}

# ---- Goal planner -----------------------------------------------------------
# Greedy search: at every step take the single realistic improvement that
# raises the probability the most, until the target is reached.
GOAL_STEPS <- data.frame(
  feature = c("CGPA", "Coding_Score", "Aptitude_Score", "Communication_Score",
              "Technical_Skill_Score", "Problem_Solving_Score", "Attendance_Percentage",
              "Number_of_Internships", "Number_of_Projects", "Number_of_Certifications",
              "Number_of_Backlogs"),
  step    = c(0.2, 5, 5, 5, 5, 5, 5, 1, 1, 1, -1),
  limit   = c(9.5, 95, 95, 95, 95, 95, 95, 3, 6, 6, 0),
  # max_moves keeps routes realistic and balanced: no skill may jump
  # further than (step x max_moves) from where the student starts.
  max_moves = c(3, 4, 4, 4, 4, 4, 2, 2, 2, 2, 3),
  # estimated effort (weeks) for ONE step, used to rank improvements by payoff per week
  weeks   = c(6, 3, 2, 3, 3, 3, 2, 8, 4, 4, 6),
  group   = c("Academics", "Skills", "Academics", "Skills", "Skills", "Skills",
              "Academics", "Experience", "Experience", "Experience", "Academics"),
  stringsAsFactors = FALSE
)

# Effort-aware greedy planner.
#   focus        : "all", "Academics", "Skills" or "Experience"
#   budget_weeks : stop when the next step would exceed this many weeks
# At each step it picks the improvement with the best probability gain PER WEEK of effort.
goal_plan <- function(bundle, row, target = 0.8, focus = "all", budget_weeks = Inf, max_steps = 60) {
  cur  <- row
  p0   <- predict_prob(bundle$model, cur)
  p    <- p0
  path <- data.frame(Step = 0L, Label = "Your current profile", Prob = 100 * p0,
                     Weeks = 0, Feature = NA_character_, stringsAsFactors = FALSE)
  gains <- stats::setNames(numeric(0), character(0))
  wk_by <- stats::setNames(numeric(0), character(0))
  moves <- stats::setNames(integer(nrow(GOAL_STEPS)), GOAL_STEPS$feature)
  allowed <- if (identical(focus, "all")) rep(TRUE, nrow(GOAL_STEPS)) else GOAL_STEPS$group == focus
  stall <- 0L; weeks_used <- 0

  for (i in seq_len(max_steps)) {
    if (p >= target) break
    best <- NULL; best_score <- -Inf
    for (j in which(allowed)) {
      f <- GOAL_STEPS$feature[j]; st <- GOAL_STEPS$step[j]; lim <- GOAL_STEPS$limit[j]
      wk <- GOAL_STEPS$weeks[j]
      if (moves[[f]] >= GOAL_STEPS$max_moves[j]) next
      if (weeks_used + wk > budget_weeks) next
      v <- as.numeric(cur[[f]])
      nv <- if (st > 0) min(v + st, lim) else max(v + st, lim)
      if (isTRUE(all.equal(nv, v)) || (st > 0 && v >= lim) || (st < 0 && v <= lim)) next
      r2 <- cur; r2[[f]] <- nv
      g <- predict_prob(bundle$model, r2) - p
      score <- g / wk
      if (score > best_score) { best_score <- score; best <- list(f = f, nv = nv, v = v, g = g, wk = wk) }
    }
    if (is.null(best)) break
    if (best$g <= 1e-6) { stall <- stall + 1L; if (stall >= 3L) break } else stall <- 0L

    cur[[best$f]] <- best$nv
    moves[[best$f]] <- moves[[best$f]] + 1L
    weeks_used <- weeks_used + best$wk
    p <- predict_prob(bundle$model, cur)
    gains[best$f] <- (if (best$f %in% names(gains)) gains[[best$f]] else 0) + 100 * best$g
    wk_by[best$f] <- (if (best$f %in% names(wk_by)) wk_by[[best$f]] else 0) + best$wk
    path <- rbind(path, data.frame(
      Step = i,
      Label = sprintf("%s: %s -> %s", feature_label(best$f), num_txt(best$v), num_txt(best$nv)),
      Prob = 100 * p, Weeks = weeks_used, Feature = best$f, stringsAsFactors = FALSE))
  }

  changed <- NUMERIC_FEATURES[vapply(NUMERIC_FEATURES, function(f)
    !isTRUE(all.equal(as.numeric(cur[[f]]), as.numeric(row[[f]]))), logical(1))]
  changes <- if (length(changed) == 0) {
    data.frame(Feature = character(0), Skill = character(0), From = numeric(0),
               To = numeric(0), Gain = numeric(0), Weeks = numeric(0))
  } else {
    d <- data.frame(Feature = changed, Skill = feature_label(changed),
                    From = vapply(changed, function(f) as.numeric(row[[f]]), numeric(1)),
                    To   = vapply(changed, function(f) as.numeric(cur[[f]]), numeric(1)),
                    Gain = as.numeric(gains[changed]), Weeks = as.numeric(wk_by[changed]),
                    stringsAsFactors = FALSE)
    d[order(-d$Gain), , drop = FALSE]
  }
  rownames(changes) <- NULL

  list(reached = p >= target, start = p0, final = p, target = target, weeks = weeks_used,
       focus = focus, budget = budget_weeks, path = path, changes = changes, row = cur)
}

# ---- Downloadable report -----------------------------------------------------
html_esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

build_report_html <- function(r) {
  li <- function(v) paste0("<li>", html_esc(v), "</li>", collapse = "")
  careers <- paste0("<tr><td>", seq_len(nrow(r$careers)), "</td><td>", html_esc(r$careers$Career),
                    "</td><td>", html_esc(r$careers$Role), "</td><td>",
                    round(r$careers$Match), "%</td></tr>", collapse = "")
  g <- r$gaps
  gaprows <- paste0("<tr><td>", html_esc(g$Skill), "</td><td>", g$Student, "</td><td>", g$Recommended,
                    "</td><td>", g$Gap, "</td><td>", html_esc(g$Status), "</td></tr>", collapse = "")
  paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>CareerIQ Report</title><style>',
    'body{font-family:Segoe UI,Arial,sans-serif;color:#1B1A3D;max-width:820px;margin:30px auto;padding:0 20px}',
    '.hd{background:linear-gradient(120deg,#6366F1,#06B6D4);color:#fff;border-radius:16px;padding:24px 28px}',
    '.hd h1{margin:0 0 4px}h2{color:#6366F1;border-bottom:2px solid #E6E8FA;padding-bottom:4px;margin-top:28px}',
    'table{border-collapse:collapse;width:100%}td,th{border-bottom:1px solid #E6E8FA;padding:7px 10px;text-align:left}',
    '.big{font-size:44px;font-weight:800;color:#6366F1}li{margin:5px 0}.m{color:#8A90B4;font-size:12px}',
    '</style></head><body>',
    '<div class="hd"><h1>CareerIQ Student Report</h1><div>', html_esc(r$name),
    ' &middot; ', html_esc(as.character(r$row$Department)), '</div></div>',
    '<h2>Placement outlook</h2><div class="big">', round(100 * r$probability, 1), '%</div>',
    '<div>', html_esc(r$status), ' &middot; ', html_esc(r$band), ' probability band</div>',
    '<h2>Top career matches</h2><table><tr><th>#</th><th>Domain</th><th>Role</th><th>Match</th></tr>', careers, '</table>',
    '<h2>Strengths</h2><ul>', li(r$strengths), '</ul>',
    '<h2>Areas to improve</h2><ul>', li(r$weaknesses), '</ul>',
    '<h2>Action plan</h2><ul>', li(r$recommendations), '</ul>',
    '<h2>Skill gap table</h2><table><tr><th>Skill</th><th>You</th><th>Benchmark</th><th>Gap</th><th>Status</th></tr>',
    gaprows, '</table>',
    '<p class="m">Generated by CareerIQ on ', format(Sys.time(), "%d %b %Y %H:%M"),
    '. Predictions are guidance based on a model trained on synthetic data, not a guarantee.</p>',
    '</body></html>')
}

# =====================================================================
# AI / ML additions
#   * fit_package_model()  - supervised REGRESSION (random forest) that
#                            predicts the expected package (LPA) if placed
#   * fit_clusters()       - UNSUPERVISED learning (k-means) that groups
#                            students into data-driven "personas"
#   * risk_factors()       - which weakness would hurt the odds most
#   * coach_summary()      - plain-language summary (template-based NLG,
#                            not a large language model)
# =====================================================================

# ---- Regression: expected package ------------------------------------------
fit_package_model <- function(df) {
  placed <- df[df$Placement_Status == POSITIVE_CLASS & df$Placement_Package > 0, , drop = FALSE]
  if (nrow(placed) < 30) return(NULL)
  set.seed(11)
  fit <- randomForest::randomForest(x = placed[, MODEL_FEATURES, drop = FALSE],
                                    y = placed$Placement_Package, ntree = 300)
  list(fit = fit, n = nrow(placed),
       rmse = sqrt(utils::tail(fit$mse, 1)), r2 = utils::tail(fit$rsq, 1))
}

predict_package <- function(pm, row) {
  if (is.null(pm)) return(NA_real_)
  as.numeric(stats::predict(pm$fit, row[, MODEL_FEATURES, drop = FALSE]))
}

# ---- Unsupervised: k-means student personas -----------------------------------
PERSONA_DIMS <- list(
  Technical  = c("Coding_Score", "Technical_Skill_Score", "Problem_Solving_Score", "Aptitude_Score"),
  Academic   = c("CGPA", "Attendance_Percentage"),
  Social     = c("Communication_Score", "Leadership_Score", "Teamwork_Score"),
  Experience = c("Number_of_Projects", "Number_of_Internships", "Number_of_Certifications")
)
PERSONA_NAMES <- c(Technical = "Tech Builder", Academic = "Academic Achiever",
                   Social = "People Leader", Experience = "Hands-on Explorer")
PERSONA_ICONS <- c("Tech Builder" = "code", "Academic Achiever" = "book",
                   "People Leader" = "users", "Hands-on Explorer" = "tools")

fit_clusters <- function(df, k = 4) {
  X <- scale(as.matrix(df[, NUMERIC_FEATURES, drop = FALSE]))
  set.seed(5)
  km <- stats::kmeans(X, centers = k, nstart = 20)
  cen <- km$centers
  dimz <- vapply(PERSONA_DIMS, function(fs) rowMeans(cen[, fs, drop = FALSE]), numeric(k))
  nm <- character(k); avail <- colnames(dimz)
  for (ci in order(-apply(dimz, 1, max))) {
    j <- which.max(dimz[ci, avail])
    nm[ci] <- PERSONA_NAMES[[avail[j]]]
    avail <- avail[-j]
  }
  lvl <- rowMeans(cen)
  placed <- as.numeric(tapply(df$Placement_Status == POSITIVE_CLASS, km$cluster, mean))
  list(km = km, center = attr(X, "scaled:center"), scale = attr(X, "scaled:scale"),
       summary = data.frame(Persona = nm, Size = as.integer(km$size), PlacementRate = placed,
                            Level = ifelse(lvl < -0.25, "developing", ifelse(lvl > 0.25, "strong", "balanced")),
                            stringsAsFactors = FALSE))
}

assign_persona <- function(cl, row) {
  if (is.null(cl)) return(NULL)
  x <- (as.numeric(unlist(row[1, NUMERIC_FEATURES])) - cl$center) / cl$scale
  i <- which.min(rowSums(sweep(cl$km$centers, 2, x)^2))
  list(index = i, name = cl$summary$Persona[i], rate = cl$summary$PlacementRate[i],
       level = cl$summary$Level[i], size = cl$summary$Size[i])
}

# ---- Risk watch: how much would a slip cost? --------------------------------------
risk_factors <- function(bundle, row) {
  base <- predict_prob(bundle$model, row)
  out <- lapply(seq_len(nrow(GOAL_STEPS)), function(j) {
    f <- GOAL_STEPS$feature[j]; st <- GOAL_STEPS$step[j]
    i <- match(f, FEATURE_INFO$feature)
    v <- as.numeric(row[[f]])
    nv <- if (st > 0) max(FEATURE_INFO$min[i], v - 2 * st) else min(FEATURE_INFO$max[i], v + 2)
    if (isTRUE(all.equal(nv, v))) return(NULL)
    r2 <- row; r2[[f]] <- nv
    data.frame(Feature = f, Skill = feature_label(f), From = v, To = nv,
               Loss = 100 * (base - predict_prob(bundle$model, r2)), stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out)
  if (is.null(d)) return(data.frame(Feature = character(0), Skill = character(0),
                                    From = numeric(0), To = numeric(0), Loss = numeric(0)))
  d <- d[order(-d$Loss), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# ---- Plain-language coach (template-based) -------------------------------------------
coach_summary <- function(r, pkg_lpa = NA_real_, persona = NULL, risks = NULL) {
  p <- r$probability; lvl <- readiness_level(p); top <- r$careers[1, ]
  out <- c(
    sprintf("%s, your placement chance is %s, which is the '%s' level. %s",
            r$name, fmt_pct(p, 0), lvl$name, lvl$blurb),
    sprintf("Your strongest career match is %s (%d%% fit).", top$Career, round(top$Match)))
  if (!is.na(pkg_lpa))
    out <- c(out, sprintf("If you are placed, the regression model expects a package of about %.1f LPA.", pkg_lpa))
  if (!is.null(persona))
    out <- c(out, sprintf("Your profile resembles the '%s' group (%s). About %d%% of that group was placed.",
                          persona$name, persona$level, round(100 * persona$rate)))
  if (!is.null(risks) && nrow(risks) > 0 && risks$Loss[1] > 0.5)
    out <- c(out, sprintf("Protect your %s: if it slipped, your chance could fall by about %.1f points.",
                          tolower(risks$Skill[1]), risks$Loss[1]))
  out <- c(out, paste("Biggest area to improve:", r$weaknesses[1]))
  out
}
