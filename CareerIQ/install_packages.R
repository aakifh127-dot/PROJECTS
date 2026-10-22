# =====================================================================
# install_packages.R  --  RUN ONCE, MANUALLY (never called by the app)
# ---------------------------------------------------------------------
# Installs every package CareerIQ needs. Run it in the RStudio console:
#   source("install_packages.R")
# =====================================================================

required_packages <- c(
  "shiny", "shinydashboard",          # dashboard UI
  "dplyr", "tidyr", "tibble",         # data preprocessing
  "ggplot2", "scales", "plotly",      # graphs (plotly = interactive, still pure R)
  "DT",                               # interactive tables
  "randomForest", "rpart"             # models (ROC/AUC are computed in base R)
)

optional_packages <- c("xgboost")     # optional 4th model; app works without it

to_install <- setdiff(required_packages, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}

opt_missing <- setdiff(optional_packages, rownames(installed.packages()))
if (length(opt_missing) > 0) {
  message("Trying optional packages: ", paste(opt_missing, collapse = ", "))
  try(install.packages(opt_missing), silent = TRUE)
}

message("Setup finished. Next: source('generate_dataset.R')")
