# run_all.R
# Purpose:
#   Run full pipeline:
#   (A) ATS2021 replication (01-04)
#   (B) 2023/2024 new datasets pipeline (05-06)

message("=== Running full multi-dataset pipeline ===")

# ---- A) Project setup (shared) ----
message("\n[Setup] 00_project_setup.R")
source("R/00_project_setup.R")

# ---- B) ATS2021 replication mainline ----
message("\n[ATS2021] Replication pipeline")
ats2021_scripts <- c(
  "R/01_data_cleaning.R",
  "R/02_descriptives_table1.R",
  "R/03_ordinal_logistic_regression.R",
  "R/04_plots_Q40_to_Q43.R"
)

for (s in ats2021_scripts) {
  message(sprintf("\n--- Running: %s ---", s))
  source(s)
}

# ---- C) 2023/2024 new datasets branch ----
message("\n[ATS2023/2024] New datasets pipeline")
newdata_scripts <- c(
  "R/05_different_dataset.R",
  "R/06_plots_different_dataset.R",
  "R/07_extension_tables.R"
)

for (s in newdata_scripts) {
  message(sprintf("\n--- Running: %s ---", s))
  source(s)
}

message("\n=== All pipelines completed successfully ===")
