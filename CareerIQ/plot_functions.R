# =====================================================================
# plot_functions.R
# Every chart in CareerIQ is built here (pure R).
# ggplot2 charts are wrapped in plotly::ggplotly() by app.R; the radar
# charts at the bottom are native plotly objects.
# =====================================================================

if (!exists("FEATURE_INFO")) source("helper_functions.R")
library(ggplot2)

C <- CAREERIQ_COLORS
STATUS_PAL <- c("Not Placed" = C$bad, "Placed" = C$good)
SCORE_FEATURES <- FEATURE_INFO$feature[FEATURE_INFO$max == 100]

add_status_label <- function(df) {
  df$Status <- factor(gsub("_", " ", as.character(df$Placement_Status)),
                      levels = c("Not Placed", "Placed"))
  df
}

# ---- Analytics dashboard --------------------------------------------
plot_status_bar <- function(df) {
  d <- dplyr::count(add_status_label(df), Status)
  ggplot(d, aes(Status, n, fill = Status)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = n), vjust = -0.4, fontface = "bold") +
    scale_fill_manual(values = STATUS_PAL, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Placement Status", x = NULL, y = "Students") + theme_careeriq()
}

plot_rate_by_dept <- function(df) {
  d <- dplyr::summarise(dplyr::group_by(df, Department),
                        Rate = mean(Placement_Status == POSITIVE_CLASS), N = dplyr::n())
  ggplot(d, aes(reorder(Department, Rate), Rate, fill = Rate)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = scales::percent(Rate, accuracy = 1)), hjust = -0.15, size = 3.5) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
    scale_fill_gradient(low = "#C7D2FE", high = C$violet, guide = "none") +
    labs(title = "Placement Rate by Department", x = NULL, y = "Placement rate") + theme_careeriq()
}

box_by_status <- function(df, var, title, ylab) {
  ggplot(add_status_label(df), aes(Status, .data[[var]], fill = Status)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.4, width = 0.55) +
    scale_fill_manual(values = STATUS_PAL, guide = "none") +
    labs(title = title, x = NULL, y = ylab) + theme_careeriq()
}
plot_cgpa_status   <- function(df) box_by_status(df, "CGPA", "CGPA vs Placement", "CGPA")
plot_coding_status <- function(df) box_by_status(df, "Coding_Score", "Coding Score vs Placement", "Coding score")

plot_aptitude_dist <- function(df) {
  ggplot(add_status_label(df), aes(Aptitude_Score, fill = Status)) +
    geom_histogram(bins = 25, alpha = 0.75, position = "identity", colour = "white") +
    scale_fill_manual(values = STATUS_PAL, name = NULL) +
    labs(title = "Aptitude Score Distribution", x = "Aptitude score", y = "Students") + theme_careeriq()
}

plot_internship_rate <- function(df) {
  d <- dplyr::summarise(dplyr::group_by(df, Number_of_Internships),
                        Rate = mean(Placement_Status == POSITIVE_CLASS), N = dplyr::n())
  ggplot(d, aes(factor(Number_of_Internships), Rate)) +
    geom_col(fill = C$accent, width = 0.6) +
    geom_text(aes(label = paste0(scales::percent(Rate, accuracy = 1), "\n(n=", N, ")")),
              vjust = -0.3, size = 3) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Internships vs Placement Rate", x = "Number of internships", y = "Placement rate") +
    theme_careeriq()
}

plot_career_dist <- function(df) {
  d <- dplyr::count(df, Recommended_Career)
  ggplot(d, aes(reorder(Recommended_Career, n), n)) +
    geom_col(fill = C$primary, width = 0.65) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
    coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Recommended Career Distribution", x = NULL, y = "Students") + theme_careeriq()
}

plot_corr_heatmap <- function(df) {
  num <- df[, NUMERIC_FEATURES, drop = FALSE]
  num$Placed <- as.numeric(df$Placement_Status == POSITIVE_CLASS)
  cm <- stats::cor(num)
  cm_df <- as.data.frame(cm)
  cm_df$Var1 <- rownames(cm)
  long <- tidyr::pivot_longer(cm_df, cols = -Var1, names_to = "Var2", values_to = "Corr")
  lv <- colnames(cm)
  long$Var1 <- factor(long$Var1, levels = lv, labels = feature_label(lv))
  long$Var2 <- factor(long$Var2, levels = lv, labels = feature_label(lv))
  ggplot(long, aes(Var1, Var2, fill = Corr)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", Corr)), size = 2.4) +
    scale_fill_gradient2(low = C$bad, mid = "white", high = C$primary, limits = c(-1, 1), name = "r") +
    labs(title = "Correlation Heatmap", x = NULL, y = NULL) + theme_careeriq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_package_dist <- function(df) {
  d <- df[df$Placement_Status == POSITIVE_CLASS & df$Placement_Package > 0, , drop = FALSE]
  ggplot(d, aes(Placement_Package)) +
    geom_histogram(bins = 25, fill = C$good, colour = "white", alpha = 0.85) +
    labs(title = "Placement Package Distribution", x = "Package (LPA)", y = "Students") + theme_careeriq()
}

# ---- Importance / explanation ----------------------------------------
plot_importance <- function(imp, title = "Feature Importance", top_n = 10) {
  d <- utils::head(imp[order(-imp$Importance), , drop = FALSE], top_n)
  ggplot(d, aes(reorder(Label, Importance), Importance)) +
    geom_col(fill = C$primary, width = 0.65) +
    geom_text(aes(label = sprintf("%.1f%%", Importance)), hjust = -0.1, size = 3.3) +
    coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = title, x = NULL, y = "Relative importance (%)") + theme_careeriq()
}

plot_local_influence <- function(inf, top_n = 8) {
  d <- inf[order(-abs(inf$Effect)), , drop = FALSE]
  d <- utils::head(d, top_n)
  d$Direction <- ifelse(d$Effect >= 0, "Helps", "Hurts")
  ggplot(d, aes(reorder(Label, abs(Effect)), Effect, fill = Direction)) +
    geom_col(width = 0.65) + coord_flip() +
    geom_hline(yintercept = 0, colour = C$muted) +
    scale_fill_manual(values = c(Helps = C$good, Hurts = C$bad), name = NULL) +
    labs(title = "What drives THIS student's probability",
         subtitle = "Change in probability (percentage points) vs a typical student",
         x = NULL, y = "Effect (pp)") + theme_careeriq()
}

# ---- Career / skill gap -----------------------------------------------
plot_career_scores <- function(all_df) {
  ggplot(all_df, aes(reorder(Career, Match), Match)) +
    geom_col(fill = C$accent, width = 0.65) +
    geom_text(aes(label = paste0(round(Match), "%")), hjust = -0.15, size = 3.5) +
    coord_flip() + scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    labs(title = "Match with every career domain", x = NULL, y = "Match (%)") + theme_careeriq()
}

plot_skill_gap_bars <- function(gaps) {
  d <- gaps[gaps$Feature %in% SCORE_FEATURES, , drop = FALSE]
  long <- data.frame(
    Skill = rep(d$Skill, 2),
    Type  = factor(rep(c("Student", "Recommended"), each = nrow(d)), levels = c("Student", "Recommended")),
    Value = c(d$Student, d$Recommended)
  )
  ggplot(long, aes(Skill, Value, fill = Type)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_fill_manual(values = c(Student = C$primary, Recommended = C$warn), name = NULL) +
    labs(title = "Student vs Recommended (0-100 scale skills)", x = NULL, y = "Score") +
    theme_careeriq() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# ---- What-if -------------------------------------------------------------
plot_whatif_compare <- function(cur, new) {
  d <- data.frame(Scenario = factor(c("Current", "After improvement"),
                                    levels = c("Current", "After improvement")),
                  Prob = 100 * c(cur, new))
  ggplot(d, aes(Scenario, Prob, fill = Scenario)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", Prob)), vjust = -0.4, fontface = "bold") +
    scale_fill_manual(values = c("Current" = C$muted, "After improvement" = C$primary), guide = "none") +
    scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    labs(title = "Placement probability: before vs after", x = NULL, y = "Probability (%)") + theme_careeriq()
}

plot_whatif_effects <- function(eff) {
  if (is.null(eff) || nrow(eff) == 0) {
    return(ggplot() + annotate("text", x = 0, y = 0, label = "Move a slider to see the impact of each change", colour = C$muted) +
             theme_void())
  }
  eff$Direction <- ifelse(eff$Change >= 0, "Up", "Down")
  ggplot(eff, aes(reorder(Feature, Change), Change, fill = Direction)) +
    geom_col(width = 0.6) + coord_flip() +
    geom_text(aes(label = sprintf("%+.1f pp", Change)), hjust = ifelse(eff$Change >= 0, -0.1, 1.1), size = 3.3) +
    scale_fill_manual(values = c(Up = C$good, Down = C$bad), guide = "none") +
    scale_y_continuous(expand = expansion(mult = 0.2)) +
    labs(title = "Impact of each change on its own", x = NULL, y = "Change in probability (pp)") + theme_careeriq()
}

# ---- Model performance ------------------------------------------------------
plot_confusion <- function(cm, title = "Confusion Matrix") {
  cm$Actual    <- factor(gsub("_", " ", as.character(cm$Actual)), levels = c("Not Placed", "Placed"))
  cm$Predicted <- factor(gsub("_", " ", as.character(cm$Predicted)), levels = c("Not Placed", "Placed"))
  ggplot(cm, aes(Predicted, Actual, fill = Count)) +
    geom_tile(colour = "white", linewidth = 1.5) +
    geom_text(aes(label = Count), size = 9, fontface = "bold") +
    scale_y_discrete(limits = c("Placed", "Not Placed")) +
    scale_fill_gradient(low = "#EEF2FF", high = "#A5B4FC", guide = "none") +
    labs(title = title, x = "Predicted", y = "Actual") + theme_careeriq()
}

plot_roc <- function(roc, comp) {
  lab <- setNames(sprintf("%s (AUC %.3f)", comp$Model, comp$Test_AUC), comp$Model)
  roc$Label <- unname(lab[roc$Model])
  ggplot(roc, aes(FPR, TPR, colour = Label)) +
    geom_abline(linetype = "dashed", colour = C$muted) +
    geom_line() +
    scale_colour_viridis_d(end = 0.85, name = NULL) +
    coord_equal() +
    labs(title = "ROC Curves (test set)", x = "False positive rate", y = "True positive rate") +
    theme_careeriq()
}


# ====================================================================
# NEW in the animated edition
# ====================================================================

# ---- Peer comparison: you vs department vs placed peers ---------------
plot_peer_bars <- function(row, df) {
  dept <- as.character(row$Department)
  d    <- df[df$Department == dept, , drop = FALSE]
  placed <- d[d$Placement_Status == POSITIVE_CLASS, , drop = FALSE]
  if (nrow(placed) == 0) placed <- d
  feats <- SCORE_FEATURES
  long <- data.frame(
    Skill = rep(feature_label(feats), 3),
    Group = factor(rep(c("You", paste(dept, "average"), paste(dept, "placed avg")), each = length(feats)),
                   levels = c("You", paste(dept, "average"), paste(dept, "placed avg"))),
    Value = c(as.numeric(unlist(row[1, feats])),
              vapply(feats, function(f) mean(d[[f]]), numeric(1)),
              vapply(feats, function(f) mean(placed[[f]]), numeric(1)))
  )
  pal <- stats::setNames(c(C$primary, C$muted, C$good), levels(long$Group))
  ggplot(long, aes(Skill, Value, fill = Group)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    scale_fill_manual(values = pal, name = NULL) +
    labs(title = "You vs your department", x = NULL, y = "Score (0-100)") +
    theme_careeriq() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# ---- Gain from closing each gap -----------------------------------------
plot_gap_impact <- function(impact) {
  if (is.null(impact) || nrow(impact) == 0) {
    return(ggplot() + annotate("text", x = 0, y = 0, colour = C$muted,
                               label = "No gaps - every benchmark is met") + theme_void())
  }
  d <- utils::head(impact, 8)
  ggplot(d, aes(reorder(Skill, Gain), Gain)) +
    geom_col(fill = C$violet, width = 0.62) +
    geom_text(aes(label = sprintf("%+.1f pp", Gain)), hjust = -0.1, size = 3.4) +
    coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(title = "Payoff of closing each gap",
         subtitle = "Probability gain if the skill reaches its benchmark",
         x = NULL, y = "Gain (percentage points)") + theme_careeriq()
}

# ---- Goal planner path ------------------------------------------------------
plot_goal_path <- function(path, target) {
  ggplot(path, aes(Weeks, Prob)) +
    geom_hline(yintercept = 100 * target, linetype = "dashed", colour = C$warn) +
    geom_area(fill = C$primary, alpha = 0.12) +
    geom_step(colour = C$primary, linewidth = 1.2) +
    geom_point(aes(text = Label), colour = "white", fill = C$primary, shape = 21, size = 3.6, stroke = 1.4) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "Weeks of effort (dashed line = target)", y = "Placement probability (%)") + theme_careeriq()
}

# ---- Native plotly radar charts ----------------------------------------------
radar_layout <- function(p, legend = TRUE) {
  plotly::layout(
    p,
    polar = list(
      bgcolor = "rgba(255,255,255,0)",
      radialaxis = list(range = c(0, 105), ticksuffix = "%", angle = 90, tickfont = list(size = 9),
                        gridcolor = "#E6E8FA", linecolor = "#E6E8FA"),
      angularaxis = list(direction = "clockwise", gridcolor = "#E6E8FA", tickfont = list(size = 11))),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    showlegend = legend, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08),
    margin = list(l = 50, r = 50, t = 30, b = 30),
    font = list(family = "Plus Jakarta Sans, Segoe UI, sans-serif", color = "#4B4F7A"))
}

plotly_skill_radar <- function(gaps) {
  d <- gaps
  theta <- c(d$Skill, d$Skill[1])
  you   <- c(pmin(d$Achieved, 100), pmin(d$Achieved[1], 100))
  plotly::plot_ly() %>%
    plotly::add_trace(type = "scatterpolar", mode = "lines", r = rep(100, length(theta)), theta = theta,
                      name = "Benchmark", line = list(color = C$warn, dash = "dot", width = 2),
                      hoverinfo = "skip") %>%
    plotly::add_trace(type = "scatterpolar", mode = "lines+markers", r = you, theta = theta,
                      name = "You", fill = "toself", fillcolor = "rgba(99,102,241,0.25)",
                      line = list(color = C$primary, width = 3), marker = list(size = 7, color = C$primary),
                      hovertemplate = "%{theta}: %{r:.0f}% of benchmark<extra></extra>") %>%
    radar_layout() %>% plotly::config(displayModeBar = FALSE)
}

plotly_career_radar <- function(all_df) {
  theta <- c(all_df$Career, all_df$Career[1])
  r     <- c(all_df$Match, all_df$Match[1])
  plotly::plot_ly() %>%
    plotly::add_trace(type = "scatterpolar", mode = "lines+markers", r = r, theta = theta,
                      name = "Match", fill = "toself", fillcolor = "rgba(6,182,212,0.25)",
                      line = list(color = C$accent, width = 3), marker = list(size = 7, color = C$accent),
                      hovertemplate = "%{theta}: %{r:.0f}%<extra></extra>") %>%
    radar_layout(legend = FALSE) %>% plotly::config(displayModeBar = FALSE)
}
