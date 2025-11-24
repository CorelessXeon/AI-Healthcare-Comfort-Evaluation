# ---- 0) Packages to install and load ----
needed <- c(
  "dplyr", "tidyr", "tidyverse", "haven", "janitor",
  "forcats", "gt", "MASS", "broom", "stringr",
  "purrr", "here", "plotly", "glue"
)

# Determine which packages are missing in the renv project library
installed <- rownames(installed.packages())
to_install <- setdiff(needed, installed)

# Install only missing packages (not all)
if (length(to_install) > 0) {
  message("[setup] Installing missing packages into renv library:")
  print(to_install)
  renv::install(to_install)
} else {
  message("[setup] All needed packages already installed.")
}

# Load packages
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
utils_path <- "R/utils/common_utils.R"

if (file.exists(utils_path)) {
  source(utils_path)
} else {
  warning("common_utils.R not found in R/utils/")
}

