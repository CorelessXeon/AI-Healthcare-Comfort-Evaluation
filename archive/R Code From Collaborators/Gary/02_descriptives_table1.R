#############################################
# 02_descriptives_table1.R
# Purpose:
#   Reproduce Table 1 like in the paper:
#   "Descriptive statistics of respondents based on socioeconomic and demographic characteristics"
# Input:
#   data_clean/clean_data.rds   (paper-like cleaned data from 01_data_cleaning.R)
# Output:
#   output/table1_age_by_gender.csv
#   output/table1_income_by_gender.csv
#   output/table1_education_by_gender.csv
#   output/table1_combined.csv
#############################################

library(tidyverse)
library(here)

# --- 1. load cleaned data (paper-like) -------------------------
clean_path <- here("data_clean", "clean_data.rds")
df <- readr::read_rds(clean_path)

# create output dir
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
}

# for safety: drop rows without gender
df <- df %>% drop_na(gender)

# we expect gender levels: Male, Female, Other
# make sure the order is consistent
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
  # rename columns to match table style
  rename(
    `Sample Size (Male - 1)`   = Male,
    `Sample Size (Female - 2)` = Female,
    `Sample Size (Other - 3)`  = Other
  ) %>%
  # keep the factor order
  arrange(age_new) %>%
  # add code column like in the paper (1) (2) ...
  mutate(`Age Category (Code)` = as.character(age_new), .before = 1)

# readr::write_csv(age_by_gender, here("output", "table1_age_by_gender.csv"))

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

# readr::write_csv(income_by_gender, here("output", "table1_income_by_gender.csv"))

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

# readr::write_csv(edu_by_gender, here("output", "table1_education_by_gender.csv"))

# --- 5. optional: combine into a single long table -------------------
# this will stack age / income / education in one CSV
table1_combined <- bind_rows(
  age_by_gender %>%
    mutate(Factor = "Age in years"),
  income_by_gender %>%
    mutate(Factor = "Income"),
  edu_by_gender %>%
    mutate(Factor = "Education")
) %>%
  # put factor as first column
  relocate(Factor, .before = 1)

readr::write_csv(table1_combined, here("output", "table1_combined.csv"))

message("Table 1 descriptive CSVs have been written to /output")
