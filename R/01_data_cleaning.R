#############################################
# 01_data_cleaning.R
# Purpose:
#   - load raw ATS2021 dataset
#   - keep only variables needed for the AI-comfort project
#   - drop unclear responses (96–99)
#   - recode to meaningful factors (according to paper appendix)
#   - save cleaned data to /data_clean
#
#
# Age uses YOUR five bins (from this export):
#   1=16–24, 2=25–34, 3=35–44, 4=45–54, 5=55+
# Article used: 16–24, 25–34, 35–54, 55–64, 65+ (expect mild differences).
#
# Paper exclusions:
# - Outcomes: drop “Don’t know” for Q41, Q42, Q43 (set to NA -> excluded by polr). # nolint: line_length_linter.
# - Predictors: drop “Prefer not to answer” for income (Q55);
#               drop “Other”, “None of the above”, “Prefer not to answer” for education (Q66). # nolint: line_length_linter.
#############################################

# ---- 1) Load SAV & lock variable names (confirmed from CSV) ----
sav_file_path <- here("data", "data_raw", "ATS2021 Dataset_Dataverse posting.sav") # nolint: line_length_linter.
stopifnot(file.exists(sav_file_path))

raw  <- read_sav(sav_file_path)
cdhs <- raw %>% clean_names()

required_cols <- c("q40","q41","q42","q43","age_new","gender","q66","q55")
missing_cols  <- setdiff(required_cols, names(cdhs))
if (length(missing_cols)) stop("Missing columns: ", paste(missing_cols, collapse = ", "))


# ---- 2) Recode per paper (numeric-code driven; robust) ----

#   (a) Outcomes as ordered factors 1<2<3<4; Q41–Q43: 98="Don't know" -> NA
cdhs <- cdhs %>%
  mutate(
    q40_o = factorize_1to4(q40, drop98 = FALSE),
    q41_o = factorize_1to4(q41, drop98 = TRUE),
    q42_o = factorize_1to4(q42, drop98 = TRUE),
    q43_o = factorize_1to4(q43, drop98 = TRUE)
  )


#   (b) Age: keep your five bins (labels for readability)
cdhs <- cdhs %>%
  mutate(
    age_code = as.integer(age_new),
    age5 = case_when(
      age_code == 1 ~ "16–24 years",
      age_code == 2 ~ "25–34 years",
      age_code == 3 ~ "35–44 years",
      age_code == 4 ~ "45–54 years",
      age_code == 5 ~ "55+ years",
      TRUE ~ NA_character_
    ),
    age5 = factor(age5, levels = lvl_age, ordered = TRUE)
  )

#   (c) Gender: 1=male, 2=female, 3=other
cdhs <- cdhs %>%
  mutate(
    gender = case_when(
      as.integer(gender) == 1 ~ "male",
      as.integer(gender) == 2 ~ "female",
      as.integer(gender) == 3 ~ "other",
      TRUE ~ NA_character_
    ),
    gender = forcats::fct_relevel(factor(gender), lvl_gender)
  )

#   (d) Education (Q66): keep 1..7; drop 96/97/99 (Other/PNTA/DK)
cdhs <- cdhs %>%
  mutate(
    q66_code = as.integer(q66),
    q66_code = if_else(q66_code %in% c(96,97,99), NA_integer_, q66_code),
    education = case_when(
      q66_code == 1 ~ "Highschool",
      q66_code == 2 ~ "Apprenticeship/Trades",
      q66_code == 3 ~ "College/CEGEP",
      q66_code == 4 ~ "University degree",
      q66_code == 5 ~ "Masters",
      q66_code == 6 ~ "PhD",
      q66_code == 7 ~ "Medical/paramedical",
      TRUE ~ NA_character_
    ),
    education = forcats::fct_relevel(factor(education), lvl_education)
  )

#   (e) Income (Q55): keep 1..7; drop 99 (PNTA/DK)
cdhs <- cdhs %>%
  mutate(
    q55_code = as.integer(q55),
    q55_code = if_else(q55_code %in% c(99), NA_integer_, q55_code),
    income = case_when(
      q55_code == 1 ~ "< $24,999",
      q55_code == 2 ~ "$25,000-$49,999",
      q55_code == 3 ~ "$50,000-$79,999",
      q55_code == 4 ~ "$80,000-$99,000",
      q55_code == 5 ~ "$100,000-$149,999",
      q55_code == 6 ~ "$150,000-$249,999",
      q55_code == 7 ~ "$250,000+",
      TRUE ~ NA_character_
    ),
    income = forcats::fct_relevel(factor(income), lvl_income)
  )

# ---- 3) Clean and select relevant columns ----
cdhs <- cdhs %>%
  dplyr::select(q40_o, q41_o, q42_o, q43_o, age5, gender, education, income) %>%
  tidyr::drop_na(age5, gender, education, income)
# Note: outcomes with NA (due to DK in Q41–Q43) are removed automatically by polr() # nolint: line_length_linter.

# ---- 4) Save cleaned dataset ----
readr::write_csv(cdhs, "data/cleaned_dataset.csv")
saveRDS(cdhs, "data/cleaned_dataset.rds")

