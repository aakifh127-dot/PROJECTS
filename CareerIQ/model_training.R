# =====================================================================
# model_training.R  --  RUN ONCE (re-run any time the dataset changes)
# Usage (working directory = CareerIQ folder):
#   source("model_training.R")
#
# Trains Logistic Regression, Decision Tree, Random Forest (+ XGBoost if
# installed) on 80% of the data. The FINAL model is chosen automatically
# by 5-fold cross-validated AUC on the training set (validation
# performance), then reported on the untouched 20% test set.
# Output: models/trained_model.rds
# =====================================================================

source("helper_functions.R")
source("data_preprocessing.R")
source("recommendation_engine.R")
require_pkgs(c("randomForest", "rpart"))

# ---- Metric helpers (base R, no extra packages) ----------------------
auc_score <- function(actual, prob) {
  pos <- actual == POSITIVE_CLASS
  n_pos <- sum(pos); n_neg <- sum(!pos)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  r <- rank(prob)
  (sum(r[pos]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

roc_points <- function(actual, prob, model) {
  pos <- actual == POSITIVE_CLASS
  thr <- sort(unique(c(Inf, prob, -Inf)), decreasing = TRUE)
  data.frame(
    Model = model,
    TPR = vapply(thr, function(t) mean(prob[pos] >= t), numeric(1)),
    FPR = vapply(thr, function(t) mean(prob[!pos] >= t), numeric(1)),
    stringsAsFactors = FALSE
  )
}

classification_metrics <- function(actual, prob, thr = 0.5) {
  pred <- prob >= thr
  pos  <- actual == POSITIVE_CLASS
  tp <- sum(pred & pos); fp <- sum(pred & !pos)
  fn <- sum(!pred & pos); tn <- sum(!pred & !pos)
  sdiv <- function(a, b) if (b == 0) 0 else a / b
  precision <- sdiv(tp, tp + fp)
  recall    <- sdiv(tp, tp + fn)
  list(
    accuracy  = (tp + tn) / length(pos),
    precision = precision,
    recall    = recall,
    f1        = sdiv(2 * precision * recall, precision + recall),
    auc       = auc_score(actual, prob),
    confusion = data.frame(
      Actual    = factor(c("Not_Placed", "Not_Placed", "Placed", "Placed"), levels = CLASS_LEVELS),
      Predicted = factor(c("Not_Placed", "Placed", "Not_Placed", "Placed"), levels = CLASS_LEVELS),
      Count     = c(tn, fp, fn, tp)
    )
  )
}

# ---- Model fitters ----------------------------------------------------
fit_model <- function(type, data) {
  data <- data[, c(MODEL_FEATURES, "Placement_Status")]
  switch(
    type,
    logistic = list(type = type,
                    fit = suppressWarnings(stats::glm(Placement_Status ~ ., data = data, family = stats::binomial()))),
    tree = {
      fit <- rpart::rpart(Placement_Status ~ ., data = data, method = "class",
                          control = rpart::rpart.control(cp = 0.005, minsplit = 20, maxdepth = 6))
      best_cp <- fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"]
      list(type = type, fit = rpart::prune(fit, cp = best_cp))
    },
    random_forest = {
      set.seed(42)
      list(type = type, fit = randomForest::randomForest(Placement_Status ~ ., data = data,
                                                        ntree = 300, importance = TRUE))
    },
    xgboost = {
      X <- xgb_matrix(data)
      y <- as.numeric(data$Placement_Status == POSITIVE_CLASS)
      dtrain <- xgboost::xgb.DMatrix(data = X, label = y)
      params <- list(objective = "binary:logistic", eval_metric = "auc",
                     max_depth = 3, eta = 0.08, subsample = 0.9, colsample_bytree = 0.9)
      fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 150, verbose = 0)
      list(type = type, fit = fit, cols = colnames(X))
    },
    stop("Unknown model type: ", type)
  )
}

# 5-fold stratified cross-validated AUC (used for model SELECTION)
cv_auc <- function(type, data, k = 5, seed = 42) {
  set.seed(seed)
  fold <- integer(nrow(data))
  for (cl in CLASS_LEVELS) {
    idx <- which(data$Placement_Status == cl)
    fold[idx] <- sample(rep_len(seq_len(k), length(idx)))
  }
  aucs <- vapply(seq_len(k), function(i) {
    m <- fit_model(type, data[fold != i, , drop = FALSE])
    va <- data[fold == i, , drop = FALSE]
    auc_score(va$Placement_Status, predict_prob(m, va))
  }, numeric(1))
  mean(aucs)
}

# Model-agnostic importance: drop in test AUC when a feature is shuffled
permutation_importance <- function(w, test, n_rep = 5) {
  set.seed(7)
  base <- auc_score(test$Placement_Status, predict_prob(w, test))
  imp <- vapply(MODEL_FEATURES, function(f) {
    mean(replicate(n_rep, {
      t2 <- test
      t2[[f]] <- sample(t2[[f]])
      base - auc_score(t2$Placement_Status, predict_prob(w, t2))
    }))
  }, numeric(1))
  imp <- pmax(imp, 0)
  pct <- if (sum(imp) > 0) 100 * imp / sum(imp) else imp
  d <- data.frame(Feature = MODEL_FEATURES, Label = feature_label(MODEL_FEATURES),
                  Importance = as.numeric(pct), stringsAsFactors = FALSE)
  d[order(-d$Importance), ]
}

serialize_wrapper <- function(w) {
  if (identical(w$type, "xgboost")) {
    w$raw <- xgboost::xgb.save.raw(w$fit)
    w$fit <- NULL
  }
  w
}

# ---- 1. Data & split ---------------------------------------------------
cat("\n=== CareerIQ model training ===\n")
df       <- load_clean_data()
model_df <- prepare_model_data(df)
sp       <- stratified_split(model_df, prop = 0.8, seed = 42)
train    <- sp$train
test     <- sp$test
cat(sprintf("Records: %d | Train: %d | Test: %d\n", nrow(df), nrow(train), nrow(test)))

# ---- 2. Train, cross-validate, evaluate --------------------------------
model_types <- c("Logistic Regression" = "logistic",
                 "Decision Tree"       = "tree",
                 "Random Forest"       = "random_forest")
if (requireNamespace("xgboost", quietly = TRUE)) {
  model_types <- c(model_types, "XGBoost" = "xgboost")
} else {
  cat("Note: 'xgboost' not installed - skipping XGBoost (optional).\n")
}

models <- list(); metrics <- list(); roc_list <- list(); cv_scores <- c()
for (nm in names(model_types)) {
  cat("Training:", nm, "...\n")
  res <- tryCatch({
    cv   <- cv_auc(model_types[[nm]], train)
    fit  <- fit_model(model_types[[nm]], train)
    prob <- predict_prob(fit, test)
    list(cv = cv, fit = fit, prob = prob)
  }, error = function(e) {
    warning("Model '", nm, "' failed and was skipped: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(res)) next
  models[[nm]]    <- res$fit
  metrics[[nm]]   <- classification_metrics(test$Placement_Status, res$prob)
  roc_list[[nm]]  <- roc_points(test$Placement_Status, res$prob, nm)
  cv_scores[nm]   <- res$cv
}
if (length(models) == 0) stop("No model could be trained. Check the dataset.")

# ---- 3. Automatic model selection ---------------------------------------
final_name <- names(cv_scores)[which.max(cv_scores)]
cat("\nSelected final model:", final_name, sprintf("(CV AUC = %.3f)\n", max(cv_scores)))

comparison <- data.frame(
  Model     = names(models),
  CV_AUC    = as.numeric(cv_scores[names(models)]),
  Accuracy  = vapply(metrics, function(m) m$accuracy, numeric(1)),
  Precision = vapply(metrics, function(m) m$precision, numeric(1)),
  Recall    = vapply(metrics, function(m) m$recall, numeric(1)),
  F1        = vapply(metrics, function(m) m$f1, numeric(1)),
  Test_AUC  = vapply(metrics, function(m) m$auc, numeric(1)),
  stringsAsFactors = FALSE
)
comparison$Status <- ifelse(comparison$Model == final_name, "\u2605 Selected", "")
comparison <- comparison[order(-comparison$CV_AUC), ]
rownames(comparison) <- NULL

# ---- 4. Explainability ------------------------------------------------------
cat("Computing feature importance ...\n")
importance <- permutation_importance(models[[final_name]], test)

importance_rf <- data.frame(Feature = character(0), Label = character(0), Importance = numeric(0))
if ("Random Forest" %in% names(models)) {
  g <- randomForest::importance(models[["Random Forest"]]$fit, type = 2)
  gv <- as.numeric(g[, 1])
  importance_rf <- data.frame(Feature = rownames(g), Label = feature_label(rownames(g)),
                              Importance = 100 * gv / sum(gv), stringsAsFactors = FALSE)
  importance_rf <- importance_rf[order(-importance_rf$Importance), ]
}

# Typical student, used by the local explanation
reference <- as.data.frame(lapply(train[NUMERIC_FEATURES], stats::median))
reference$Department <- factor(names(which.max(table(train$Department))), levels = DEPARTMENTS)
reference <- reference[, MODEL_FEATURES, drop = FALSE]

# ---- 5. Save ------------------------------------------------------------------
bundle <- list(
  models        = lapply(models, serialize_wrapper),
  final_name    = final_name,
  comparison    = comparison,
  metrics       = metrics,
  roc           = do.call(rbind, roc_list),
  importance    = importance,
  importance_rf = importance_rf,
  reference     = reference,
  threshold     = 0.5,
  n_train       = nrow(train),
  n_test        = nrow(test),
  trained_at    = Sys.time()
)
dir.create("models", showWarnings = FALSE)
saveRDS(bundle, MODEL_PATH)

cat("\n--- Model comparison ---\n")
print(format(comparison, digits = 3), row.names = FALSE)
cat("\nTop factors:\n")
print(utils::head(importance[, c("Label", "Importance")], 5), row.names = FALSE)
cat("\nSaved:", MODEL_PATH, "\nNext: shiny::runApp()\n")
