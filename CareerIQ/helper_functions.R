# =====================================================================
# helper_functions.R
# Shared constants, validation helpers and the ggplot theme.
# Every other file sources this one, so it has NO dependencies on them.
# Working directory must be the CareerIQ project folder.
# =====================================================================

# ---- Paths ----------------------------------------------------------
DATA_PATH  <- "student_data.csv"
MODEL_PATH <- file.path("models", "trained_model.rds")

# ---- Categorical constants -----------------------------------------
DEPARTMENTS <- c("CSE", "IT", "AI&DS", "AI&ML", "ECE", "EEE", "Mechanical")

DOMAINS <- c("Software Development", "Data Analytics", "Data Science",
             "Artificial Intelligence", "Cybersecurity", "Cloud Computing",
             "Web Development", "UI/UX", "Networking")

# Class levels: order matters. The SECOND level is the positive class
# ("Placed"), which is what glm() models as probability of success.
CLASS_LEVELS   <- c("Not_Placed", "Placed")
POSITIVE_CLASS <- "Placed"

# ---- Feature metadata (single source of truth) ---------------------
# min/max  : allowed input range (used for validation + sliders)
# benchmark: recommended value for a strong placement profile
FEATURE_INFO <- data.frame(
  feature = c("CGPA", "Attendance_Percentage", "Number_of_Backlogs",
              "Aptitude_Score", "Coding_Score", "Communication_Score",
              "Technical_Skill_Score", "Number_of_Projects",
              "Number_of_Internships", "Number_of_Certifications",
              "Leadership_Score", "Teamwork_Score", "Problem_Solving_Score"),
  label   = c("CGPA", "Attendance", "Backlogs",
              "Aptitude", "Coding", "Communication",
              "Technical Skills", "Projects",
              "Internships", "Certifications",
              "Leadership", "Teamwork", "Problem Solving"),
  min     = c(4,   0,   0,  0,   0,   0,   0,   0,  0,  0,  0,   0,   0),
  max     = c(10, 100, 10, 100, 100, 100, 100,  15, 10, 20, 100, 100, 100),
  benchmark = c(7.5, 80, 0, 70, 75, 70, 70, 3, 1, 2, 60, 65, 70),
  is_integer = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                 TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

NUMERIC_FEATURES <- FEATURE_INFO$feature
MODEL_FEATURES   <- c("Department", NUMERIC_FEATURES)

# ---- Small utilities ------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

feature_label <- function(f) {
  lab <- FEATURE_INFO$label[match(f, FEATURE_INFO$feature)]
  ifelse(is.na(lab), gsub("_", " ", f), lab)
}

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

fmt_pct <- function(x, digits = 0) paste0(round(100 * x, digits), "%")

# Stop with a friendly message if packages are missing (never auto-installs)
require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         ". Run source('install_packages.R') once, then try again.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Input validation ----------------------------------------------
# `vals` is a named list (e.g. from Shiny inputs). Returns a character
# vector of error messages; length 0 means the input is valid.
validate_student_input <- function(vals) {
  errors <- character(0)

  dept <- vals[["Department"]]
  if (is.null(dept) || length(dept) != 1 || is.na(dept) || !(dept %in% DEPARTMENTS)) {
    errors <- c(errors, "Please select a valid Department.")
  }

  for (i in seq_len(nrow(FEATURE_INFO))) {
    f   <- FEATURE_INFO$feature[i]
    lab <- FEATURE_INFO$label[i]
    v   <- safe_numeric(vals[[f]])
    if (length(v) != 1 || is.na(v)) {
      errors <- c(errors, sprintf("%s is empty or not a valid number.", lab))
    } else if (v < FEATURE_INFO$min[i] || v > FEATURE_INFO$max[i]) {
      errors <- c(errors, sprintf("%s must be between %s and %s.",
                                  lab, FEATURE_INFO$min[i], FEATURE_INFO$max[i]))
    }
  }
  errors
}

# ---- Visual identity -----------------------------------------------
# Light "aurora" palette: indigo -> violet -> cyan, with warm accents.
CAREERIQ_COLORS <- list(
  primary = "#6366F1",   # indigo
  violet  = "#8B5CF6",
  accent  = "#06B6D4",   # cyan
  good    = "#10B981",   # green
  warn    = "#F59E0B",   # amber
  bad     = "#F43F5E",   # rose
  pink    = "#F472B6",
  dark    = "#1B1A3D",
  muted   = "#8A90B4"
)

STATUS_COLORS <- c("Placed" = "#10B981", "Not_Placed" = "#F43F5E",
                   "Not Placed" = "#F43F5E")

# Transparent background so charts sit on the glass cards; soft grid lines.
theme_careeriq <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", colour = CAREERIQ_COLORS$dark),
      plot.subtitle    = ggplot2::element_text(colour = CAREERIQ_COLORS$muted),
      axis.text        = ggplot2::element_text(colour = "#4B4F7A"),
      axis.title       = ggplot2::element_text(colour = "#4B4F7A"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E6E8FA", linewidth = 0.4),
      plot.background  = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.position  = "bottom",
      strip.text       = ggplot2::element_text(face = "bold")
    )
}
