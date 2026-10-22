# =====================================================================
# recommendation_engine.R
# Career recommendation, skill-gap analysis, strengths/weaknesses,
# personalised advice, model prediction helpers and local explanation.
# Sourced by model_training.R and app.R.
# =====================================================================

if (!exists("FEATURE_INFO")) source("helper_functions.R")

# ---- Career knowledge base -----------------------------------------
CAREER_ROLES <- c(
  "Software Development"    = "Software Engineer",
  "Data Analytics"          = "Data Analyst",
  "Data Science"            = "Data Scientist",
  "Artificial Intelligence" = "AI / ML Engineer",
  "Cybersecurity"           = "Security Analyst",
  "Cloud Computing"         = "Cloud Engineer",
  "Web Development"         = "Full-Stack / Web Developer",
  "UI/UX"                   = "UI/UX Designer",
  "Networking"              = "Network Engineer"
)

CAREER_TIPS <- c(
  "Software Development"    = "Strengthen data structures, algorithms and build 2-3 GitHub projects.",
  "Data Analytics"          = "Master SQL, Excel and a BI/visualisation tool; practise storytelling with data.",
  "Data Science"            = "Deepen statistics and machine learning; complete Kaggle-style projects.",
  "Artificial Intelligence" = "Learn ML/deep-learning frameworks and publish an AI mini-project.",
  "Cybersecurity"           = "Practise on labs (CTFs), learn networking and earn a security certification.",
  "Cloud Computing"         = "Earn an entry-level cloud certification and deploy a small app on a cloud platform.",
  "Web Development"         = "Build responsive full-stack projects and host a public portfolio.",
  "UI/UX"                   = "Create a design portfolio with case studies; learn user-research basics.",
  "Networking"              = "Prepare for CCNA-level topics and practise network configuration labs."
)

# Weights use the normalised 0-100 columns from career_norm_matrix()
CAREER_WEIGHTS <- list(
  "Software Development"    = c(Coding = 3, Problem = 2, Technical = 1, Projects = 1, Teamwork = 0.5),
  "Data Analytics"          = c(Aptitude = 2.5, Problem = 2, CGPA = 1.5, Communication = 1.5, Coding = 1, Technical = 0.5),
  "Data Science"            = c(Aptitude = 2, Coding = 2, Problem = 2, CGPA = 2, Technical = 1, Certifications = 0.5),
  "Artificial Intelligence" = c(Coding = 3, Problem = 2, CGPA = 1.5, Technical = 1.5, Projects = 1, Aptitude = 1),
  "Cybersecurity"           = c(Technical = 3, Problem = 2, Coding = 1.5, Certifications = 1.5, Aptitude = 1),
  "Cloud Computing"         = c(Technical = 3, Coding = 1.5, Teamwork = 1, Certifications = 2, Problem = 1, Internships = 1),
  "Web Development"         = c(Coding = 2, Communication = 1.5, Projects = 2, Technical = 1, Teamwork = 1),
  "UI/UX"                   = c(Communication = 3, Teamwork = 1.5, Leadership = 1.5, Projects = 1.5, Problem = 1),
  "Networking"              = c(Technical = 3, Aptitude = 1, Problem = 1, Certifications = 1.5, Attendance = 0.5)
)

REC_TIPS <- c(
  CGPA = "focus on upcoming semester exams and revise weak subjects",
  Attendance_Percentage = "attend classes regularly; many recruiters screen for 75%+ attendance",
  Number_of_Backlogs = "clear all backlogs before the placement season begins",
  Aptitude_Score = "practise quantitative, logical and verbal aptitude tests weekly",
  Coding_Score = "solve coding problems daily (LeetCode / HackerRank) and join coding contests",
  Communication_Score = "join group discussions, public-speaking clubs and mock interviews",
  Technical_Skill_Score = "deepen core technical subjects with hands-on labs",
  Number_of_Projects = "build and publish real-world projects on GitHub",
  Number_of_Internships = "apply for at least one internship or virtual internship",
  Number_of_Certifications = "complete industry-recognised certifications (cloud, data, security)",
  Leadership_Score = "take a coordinator or team-lead role in college clubs and events",
  Teamwork_Score = "collaborate on group projects and hackathons",
  Problem_Solving_Score = "practise algorithmic thinking and logical puzzles regularly"
)

num_txt <- function(x) as.character(round(x, 2))

# ---- Career scoring (vectorised: works for 1 student or the whole dataset)
career_norm_matrix <- function(df) {
  cbind(
    CGPA           = df$CGPA * 10,
    Attendance     = df$Attendance_Percentage,
    Aptitude       = df$Aptitude_Score,
    Coding         = df$Coding_Score,
    Communication  = df$Communication_Score,
    Technical      = df$Technical_Skill_Score,
    Problem        = df$Problem_Solving_Score,
    Leadership     = df$Leadership_Score,
    Teamwork       = df$Teamwork_Score,
    Projects       = pmin(100, df$Number_of_Projects / 4 * 100),
    Internships    = pmin(100, df$Number_of_Internships / 2 * 100),
    Certifications = pmin(100, df$Number_of_Certifications / 4 * 100)
  )
}

career_score_matrix <- function(df) {
  X <- career_norm_matrix(df)
  S <- vapply(DOMAINS, function(d) {
    w <- CAREER_WEIGHTS[[d]]
    as.numeric(X[, names(w), drop = FALSE] %*% w) / sum(w)
  }, numeric(nrow(X)))
  S <- matrix(as.numeric(S), nrow = nrow(X), dimnames = list(NULL, DOMAINS))
  S[S < 1] <- 1
  S[S > 99] <- 99
  S
}

recommend_careers <- function(row, top_n = 3) {
  s <- career_score_matrix(row)[1, ]
  all <- data.frame(Career = DOMAINS, Match = round(as.numeric(s), 1), stringsAsFactors = FALSE)
  all <- all[order(-all$Match), , drop = FALSE]
  all$Role <- unname(CAREER_ROLES[all$Career])
  all$Tip  <- unname(CAREER_TIPS[all$Career])
  rownames(all) <- NULL
  list(top = utils::head(all, top_n), all = all)
}

# ---- Skill gap analysis --------------------------------------------
skill_gap_table <- function(row) {
  out <- lapply(seq_len(nrow(FEATURE_INFO)), function(i) {
    f <- FEATURE_INFO$feature[i]
    v <- as.numeric(row[[f]])
    b <- FEATURE_INFO$benchmark[i]
    if (f == "Number_of_Backlogs") {
      gap <- max(0, v - b); ach <- max(0, 100 - 25 * gap)
    } else {
      gap <- max(0, b - v); ach <- min(100, 100 * v / b)
    }
    data.frame(Feature = f, Skill = FEATURE_INFO$label[i], Student = v,
               Recommended = b, Gap = round(gap, 2), Achieved = round(ach, 1),
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out)
  d$Status <- ifelse(d$Achieved >= 100, "Strong", ifelse(d$Achieved >= 80, "Close", "Needs Work"))
  d
}

identify_strengths <- function(gaps, top_n = 5) {
  s <- gaps[gaps$Status == "Strong", , drop = FALSE]
  if (nrow(s) == 0) return("No area exceeds the benchmark yet - follow the improvement plan below.")
  ratio <- ifelse(s$Feature == "Number_of_Backlogs", 1, s$Student / s$Recommended)
  s <- s[order(-ratio), , drop = FALSE]
  txt <- ifelse(s$Feature == "Number_of_Backlogs", "Clean academic record (no backlogs)",
                paste0(s$Skill, " (", num_txt(s$Student), " vs ", num_txt(s$Recommended), " benchmark)"))
  utils::head(txt, top_n)
}

identify_weaknesses <- function(gaps, top_n = 5) {
  w <- gaps[gaps$Status != "Strong", , drop = FALSE]
  if (nrow(w) == 0) return("No major weaknesses - every area meets the benchmark.")
  w <- w[order(w$Achieved), , drop = FALSE]
  txt <- ifelse(w$Feature == "Number_of_Backlogs",
                paste0("Backlogs (", num_txt(w$Student), " pending)"),
                paste0(w$Skill, " (", num_txt(w$Student), " vs ", num_txt(w$Recommended), " recommended)"))
  utils::head(txt, top_n)
}

generate_recommendations <- function(gaps, careers) {
  w <- gaps[gaps$Gap > 0, , drop = FALSE]
  w <- utils::head(w[order(w$Achieved), , drop = FALSE], 5)
  recs <- character(0)
  for (i in seq_len(nrow(w))) {
    f <- w$Feature[i]
    if (f == "Number_of_Backlogs") {
      recs <- c(recs, sprintf("Backlogs: clear all %s pending backlog(s) - %s.", num_txt(w$Student[i]), REC_TIPS[[f]]))
    } else {
      recs <- c(recs, sprintf("%s: move from %s to at least %s - %s.",
                              w$Skill[i], num_txt(w$Student[i]), num_txt(w$Recommended[i]), REC_TIPS[[f]]))
    }
  }
  if (length(recs) == 0) recs <- "Your profile meets every benchmark. Keep practising and start applying."
  top <- careers[1, ]
  c(recs, paste0("For a ", top$Role, " career: ", top$Tip))
}

probability_band <- function(p) if (p >= 0.7) "High" else if (p >= 0.4) "Moderate" else "Low"

# ---- Model prediction helpers ---------------------------------------
xgb_matrix <- function(df, cols = NULL) {
  mm <- stats::model.matrix(~ . - 1, data = df[, MODEL_FEATURES, drop = FALSE])
  if (!is.null(cols)) {
    missing <- setdiff(cols, colnames(mm))
    if (length(missing) > 0) {
      mm <- cbind(mm, matrix(0, nrow(mm), length(missing), dimnames = list(NULL, missing)))
    }
    mm <- mm[, cols, drop = FALSE]
  }
  mm
}

# `w` is a model wrapper: list(type = , fit = , [cols = ])
predict_prob <- function(w, newdata) {
  newdata <- newdata[, MODEL_FEATURES, drop = FALSE]
  p <- switch(
    w$type,
    logistic      = as.numeric(stats::predict(w$fit, newdata, type = "response")),
    tree          = as.numeric(stats::predict(w$fit, newdata, type = "prob")[, POSITIVE_CLASS]),
    random_forest = as.numeric(stats::predict(w$fit, newdata, type = "prob")[, POSITIVE_CLASS]),
    xgboost       = as.numeric(stats::predict(w$fit, xgboost::xgb.DMatrix(xgb_matrix(newdata, w$cols)))),
    stop("Unknown model type: ", w$type, call. = FALSE)
  )
  pmin(pmax(p, 0), 1)
}

restore_wrapper <- function(w) {
  if (identical(w$type, "xgboost") && is.null(w$fit) && !is.null(w$raw)) {
    # xgboost is optional: if it is not installed, skip this model instead of crashing
    if (!requireNamespace("xgboost", quietly = TRUE)) return(NULL)
    w$fit <- xgboost::xgb.load.raw(w$raw)
  }
  w
}

load_model_bundle <- function(path = MODEL_PATH) {
  if (!file.exists(path)) {
    stop("Trained model '", path, "' not found. Run source('model_training.R') first.", call. = FALSE)
  }
  b <- tryCatch(readRDS(path),
                error = function(e) stop("Could not read model file: ", conditionMessage(e), call. = FALSE))
  needed <- c("models", "final_name", "comparison", "metrics", "roc",
              "importance", "importance_rf", "reference", "threshold")
  miss <- setdiff(needed, names(b))
  if (length(miss) > 0) {
    stop("Model file is outdated (missing: ", paste(miss, collapse = ", "),
         "). Re-run source('model_training.R').", call. = FALSE)
  }
  b$models <- Filter(Negate(is.null), lapply(b$models, restore_wrapper))
  if (!(b$final_name %in% names(b$models))) {
    stop("The final model '", b$final_name, "' needs a package that is not installed. ",
         "Run install.packages('xgboost') or re-run source('model_training.R').", call. = FALSE)
  }
  b$model  <- b$models[[b$final_name]]
  b
}

# ---- Local explanation ----------------------------------------------
# Effect of each feature = P(current) - P(feature replaced by typical value).
# Positive => the feature is pushing the probability UP for this student.
local_explanation <- function(bundle, row) {
  base <- predict_prob(bundle$model, row)
  ref  <- bundle$reference
  eff <- vapply(MODEL_FEATURES, function(f) {
    r2 <- row
    r2[[f]] <- ref[[f]]
    100 * (base - predict_prob(bundle$model, r2))
  }, numeric(1))
  data.frame(Feature = MODEL_FEATURES, Label = feature_label(MODEL_FEATURES),
             Effect = round(as.numeric(eff), 2), stringsAsFactors = FALSE)
}

# ---- One call that produces everything the UI needs -----------------
predict_student <- function(bundle, row) {
  prob <- predict_prob(bundle$model, row)
  thr  <- bundle$threshold %||% 0.5
  rec  <- recommend_careers(row, 3)
  gaps <- skill_gap_table(row)
  list(
    probability     = prob,
    threshold       = thr,
    status          = if (prob >= thr) "Likely to be Placed" else "Not Likely to be Placed",
    band            = probability_band(prob),
    careers         = rec$top,
    all_scores      = rec$all,
    strengths       = identify_strengths(gaps),
    weaknesses      = identify_weaknesses(gaps),
    gaps            = gaps,
    recommendations = generate_recommendations(gaps, rec$top),
    influence       = local_explanation(bundle, row),
    row             = row
  )
}
