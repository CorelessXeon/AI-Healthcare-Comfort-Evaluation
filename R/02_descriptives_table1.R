#############################################
# 02_descriptives_table1.R
# Purpose:
#   Reproduce Table 1 like in the paper:
#   "Descriptive statistics of respondents based on socioeconomic and demographic characteristics"
# Input:
#   data/cleaned_dataset.csv
# Output:
#   artifacts/tables/table1_replication.html
#   artifacts/tables/table1_replication.csv
#############################################

# ---- 1) Load cleaned data ----
model_df <- readRDS("data/cleaned_dataset.rds")


# ---- 2) strict filtering method ----
q_vars <- c("q40_o", "q41_o", "q42_o", "q43_o")

sam_size_std_filter <- nrow(model_df)

# complete.cases: drop any rows that contain NA
keep_idx <- stats::complete.cases(model_df[, q_vars])

model_df_strict <- model_df[keep_idx, , drop = FALSE]

sam_size_strict_filter <- nrow(model_df_strict)

message(
  sprintf("Strict filter on q40_o–q43_o: %d -> %d rows kept.",
          sam_size_std_filter, sam_size_strict_filter)
)


saveRDS(model_df_strict, "data/cleaned_dataset_strict.rds")



# ---- 3) grouping and building frequency tables ----
#     ---- Age (group by gender) ----
tbl_age <- cross_tab_counts(
  df = model_df_strict,
  factor_var = age5,
  factor_levels = lvl_age,
  group_var = gender,
  group_levels = lvl_gender,
  section_title = "Age in years"
)

#     ---- Income (group by gender) ----
tbl_inc <- cross_tab_counts(
  df = model_df_strict,
  factor_var = income,
  factor_levels = lvl_income,
  group_var = gender,
  group_levels = lvl_gender,
  section_title = "Income"
)

#     ---- education (group by gender) ----
tbl_edu <- cross_tab_counts(
  df = model_df_strict,
  factor_var = education,
  factor_levels = lvl_education,
  group_var = gender,
  group_levels = lvl_gender,
  section_title = "Education"
)

#     ---- Combine all tables ----
table1_long <- dplyr::bind_rows(tbl_age, tbl_inc, tbl_edu)

# ---- 3) Create Table 1 with gt: descriptive stats by factor and gender ----
table1_gt <- table1_long %>%
  gt::gt(groupname_col = "Factor") %>%
  gt::tab_header(
    title = "Table 1. Descriptive statistics of respondents based on socioeconomic and demographic characteristics."
  ) %>%
  gt::fmt_number(
    columns = c(`Sample Size (male)`, `Sample Size (female)`, `Sample Size (other)`),
    decimals = 0
  ) %>%
  gt::tab_source_note(
    source_note = "Age uses the five bins present in this export (16–24, 25–34, 35–44, 45–54, 55+). The article used 16–24, 25–34, 35–54, 55–64, 65+."
  )

# ---- 4) Save outputs ----
gt::gtsave(table1_gt, "artifacts/tables/table1_replication.html")
readr::write_csv(table1_long, "artifacts/tables/table1_replication.csv")
