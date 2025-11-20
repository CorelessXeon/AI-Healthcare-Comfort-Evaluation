# ---- 0) Packages to install and load ----
needed <- c(
  "dplyr", "tidyr", "tidyverse", "haven", "janitor",
  "forcats", "gt", "MASS", "broom", "stringr",
  "purrr", "here", "plotly", "glue"
)

# Install missing packages quietly
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, quiet = TRUE)
}

# Load all packages
invisible(lapply(needed, library, character.only = TRUE))

# ---- 1) Create project folders ----
dirs <- c(
  "artifacts/tables",
  "artifacts/plots",
  "artifacts/models",
  "data/data_raw"
)

invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---- 2) Source utility scripts ----
if(file.exists("R/utils/common_utils.R")) {
  source("R/utils/common_utils.R")
} else {
  warning("common_utils.R not found in R/utils/")
}






