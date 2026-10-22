# =====================================================================
# data_preprocessing.R
# Loading, cleaning, train/test splitting and single-student input
# builders. Used by model_training.R, recommendation_engine.R and app.R.
# =====================================================================

if (!exists("FEATURE_INFO")) source("helper_functions.R")

# ---- Load -----------------------------------------------------------
load_student_data <- function(path = DATA_PATH) {
  if (!file.exists(path)) {
    stop("Dataset '", path, "' not found. Run source('generate_dataset.R') first ",
         "and make sure the working directory is the CareerIQ folder.", call. = FALSE)
  }
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Could not read '", path, "': ", conditionMessage(e), call. = FALSE)
  )
}

# ---- Clean ----------------------------------------------------------
clean_student_data <- function(df) {
  required <- c("Student_ID", "Name", "Gender", "Age", "Department", NUMERIC_FEATURES,
                "Preferred_Domain", "Placement_Status", "Placement_Package")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Dataset is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df <- dplyr::distinct(df, Student_ID, .keep_all = TRUE)

  # numeric features: coerce, impute median, clip to valid range, round counts
  for (i in seq_len(nrow(FEATURE_INFO))) {
    f <- FEATURE_INFO$feature[i]
    v <- safe_numeric(df[[f]])
    v[is.na(v)] <- stats::median(v, na.rm = TRUE)
    v <- pmin(pmax(v, FEATURE_INFO$min[i]), FEATURE_INFO$max[i])
    if (FEATURE_INFO$is_integer[i]) v <- round(v)
    df[[f]] <- v
  }

  df$Age <- safe_numeric(df$Age)
  df$Placement_Package <- safe_numeric(df$Placement_Package)
  df$Placement_Package[is.na(df$Placement_Package)] <- 0

  df$Department <- factor(trimws(as.character(df$Department)), levels = DEPARTMENTS)
  df$Preferred_Domain <- factor(trimws(as.character(df$Preferred_Domain)), levels = DOMAINS)

  status <- gsub("\\s+", "_", trimws(as.character(df$Placement_Status)))
  df$Placement_Status <- factor(status, levels = CLASS_LEVELS)

  df <- df[!is.na(df$Department) & !is.na(df$Placement_Status), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Convenience: load + clean in one call
load_clean_data <- function(path = DATA_PATH) clean_student_data(load_student_data(path))

# ---- Modelling data -------------------------------------------------
prepare_model_data <- function(df) {
  df[, c(MODEL_FEATURES, "Placement_Status"), drop = FALSE]
}

# Stratified split (keeps the placed / not-placed ratio in both sets)
stratified_split <- function(df, prop = 0.8, seed = 42) {
  set.seed(seed)
  train_idx <- unlist(lapply(split(seq_len(nrow(df)), df$Placement_Status), function(idx) {
    sample(idx, size = max(1, round(prop * length(idx))))
  }), use.names = FALSE)
  list(train = df[sort(train_idx), , drop = FALSE],
       test  = df[-train_idx, , drop = FALSE])
}

# ---- Single-student input ------------------------------------------
# `vals` : named list with Department + the 13 numeric features.
# Returns a 1-row data.frame with the same types the models were trained on.
make_student_row <- function(vals) {
  errs <- validate_student_input(vals)
  if (length(errs) > 0) stop(paste(errs, collapse = " "), call. = FALSE)

  row <- lapply(NUMERIC_FEATURES, function(f) safe_numeric(vals[[f]]))
  names(row) <- NUMERIC_FEATURES
  row <- as.data.frame(row, stringsAsFactors = FALSE)
  row$Department <- factor(vals[["Department"]], levels = DEPARTMENTS)
  row[, MODEL_FEATURES, drop = FALSE]
}

# ---- Dashboard KPIs -------------------------------------------------
dataset_summary <- function(df) {
  list(
    total_students  = nrow(df),
    placement_rate  = mean(df$Placement_Status == POSITIVE_CLASS),
    avg_cgpa        = mean(df$CGPA),
    avg_package     = mean(df$Placement_Package[df$Placement_Status == POSITIVE_CLASS]),
    top_department  = names(which.max(table(df$Department)))
  )
}
