#############################################
# 02_descriptives_table1_strict.R
# Purpose:
#   Reproduce Table 1 like in the paper,
#   but using the STRICT sample (dropped if any of Q40-Q43 is N/A)
# Input:
#   data_clean/clean_data_strict.rds   (from 01_data_cleaning.R strict branch)
# Output:
#   output/table1_age_by_gender_strict.html
#   output/table1_income_by_gender_strict.html
#   output/table1_education_by_gender_strict.html
#   output/table1_combined_strict.html
#############################################

library(tidyverse)
library(here)
library(gt)

# --- 1. load cleaned STRICT data -------------------------
clean_path <- here("data_clean", "clean_data_strict.rds")
df <- readr::read_rds(clean_path)

# create output dir
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
}

# for safety: drop rows without gender
df <- df %>% drop_na(gender)

# make sure gender order is consistent
df <- df %>%
  mutate(gender = factor(gender, levels = c("Male", "Female", "Other")))

# --- 2. AGE × gender ------------------------------------------------
age_by_gender <- df %>%
  count(age_new, gender, name = "n") %>%
  tidyr::pivot_wider(
    names_from = gender,
    values_from = n,
    values_fill = 0
  ) %>%
  rename(
    `Sample Size (Male - 1)`   = Male,
    `Sample Size (Female - 2)` = Female,
    `Sample Size (Other - 3)`  = Other
  ) %>%
  arrange(age_new) %>%
  mutate(`Age Category (Code)` = as.character(age_new), .before = 1)

# turn to HTML
age_gt <- age_by_gender |>
  gt() |>
  tab_header("Table 1A. Age by gender (strict sample)") |>
  sub_missing(everything(), missing_text = "0")

# gtsave(age_gt, here("output", "table1_age_by_gender_strict.html"))

# --- 3. INCOME × gender ---------------------------------------------
income_by_gender <- df %>%
  count(Q55, gender, name = "n") %>%
  tidyr::pivot_wider(
    names_from = gender,
    values_from = n,
    values_fill = 0
  ) %>%
  rename(
    `Sample Size (Male - 1)`   = Male,
    `Sample Size (Female - 2)` = Female,
    `Sample Size (Other - 3)`  = Other
  ) %>%
  arrange(Q55) %>%
  mutate(`Income (Code)` = as.character(Q55), .before = 1)

income_gt <- income_by_gender |>
  gt() |>
  tab_header("Table 1B. Income by gender (strict sample)") |>
  sub_missing(everything(), missing_text = "0")

# gtsave(income_gt, here("output", "table1_income_by_gender_strict.html"))

# --- 4. EDUCATION × gender -------------------------------------------
edu_by_gender <- df %>%
  count(Q66, gender, name = "n") %>%
  tidyr::pivot_wider(
    names_from = gender,
    values_from = n,
    values_fill = 0
  ) %>%
  rename(
    `Sample Size (Male - 1)`   = Male,
    `Sample Size (Female - 2)` = Female,
    `Sample Size (Other - 3)`  = Other
  ) %>%
  arrange(Q66) %>%
  mutate(`Education (Code)` = as.character(Q66), .before = 1)

edu_gt <- edu_by_gender |>
  gt() |>
  tab_header("Table 1C. Education by gender (strict sample)") |>
  sub_missing(everything(), missing_text = "0")

# gtsave(edu_gt, here("output", "table1_education_by_gender_strict.html"))

# --- 5. corrected: combine into a single tidy table -------------------

age_part <- age_by_gender %>%
  transmute(
    Factor = "Age in years",
    Code   = `Age Category (Code)`,
    `Sample Size (Male - 1)`,
    `Sample Size (Female - 2)`,
    `Sample Size (Other - 3)`
  )

income_part <- income_by_gender %>%
  transmute(
    Factor = "Income",
    Code   = `Income (Code)`,
    `Sample Size (Male - 1)`,
    `Sample Size (Female - 2)`,
    `Sample Size (Other - 3)`
  )

edu_part <- edu_by_gender %>%
  transmute(
    Factor = "Education",
    Code   = `Education (Code)`,
    `Sample Size (Male - 1)`,
    `Sample Size (Female - 2)`,
    `Sample Size (Other - 3)`
  )

table1_combined <- bind_rows(
  age_part,
  income_part,
  edu_part
)

combined_gt <- table1_combined |>
  gt() |>
  tab_header("Table 1. Descriptive statistics (strict sample, combined)") |>
  sub_missing(everything(), missing_text = "0")

gtsave(combined_gt, here("output", "table1_combined_strict.html"))
message("Strict-sample HTML tables written to /output")
