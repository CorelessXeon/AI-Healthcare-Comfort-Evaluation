#############################################
# 01_data_cleaning.R
# Purpose:
#   - load raw ATS2021 dataset
#   - keep only variables needed for the AI-comfort project
#   - drop unclear responses (96–99)
#   - recode to meaningful factors (according to paper appendix)
#   - save cleaned data to /data_clean
#############################################

# --- 0. libraries -----------------------------------------------------------
# these should already be in your renv; if not, install once:
# renv::install(c("tidyverse", "here", "haven"))

library(tidyverse)
library(here)

# --- 1. paths ---------------------------------------------------------------
# raw RData should be placed under data_raw/
# change the file name below if yours is different
raw_rdata_path <- here("data_raw", "ATS2021 Dataset_Dataverse posting.RData")

# paper-like outputs
clean_rds_path        <- here("data_clean", "clean_data.rds")
clean_csv_path        <- here("data_clean", "clean_data.csv")
summary_csv_path      <- here("data_clean", "clean_summary.csv")


# strict outputs
clean_alt_rds_path    <- here("data_clean", "clean_data_strict.rds")
clean_alt_csv_path    <- here("data_clean", "clean_data_strict.csv")
summary_alt_csv_path  <- here("data_clean", "clean_summary_strict.csv")

# create output dir if missing
if (!dir.exists(here("data_clean"))) {
  dir.create(here("data_clean"), recursive = TRUE)
}

# --- 2. load raw data -------------------------------------------------------
if (!file.exists(raw_rdata_path)) {
  stop("Raw data file not found. Please put the RData file under data_raw/.")
}

# load RData into current environment
load(raw_rdata_path)

# detect which object is the main data frame
all_objs <- ls()
df_candidates <- all_objs[sapply(all_objs, function(x) inherits(get(x), c("data.frame", "tbl_df")))]
if (length(df_candidates) == 0) {
  stop("No data.frame-like object found in the loaded RData.")
}

# pick the data frame with the most columns (very likely the survey data)
df_name <- df_candidates[which.max(sapply(df_candidates, function(x) ncol(get(x))))]
message("Detected data frame object: ", df_name)
raw_df <- get(df_name)

# --- 3. define variables we need -------------------------------------------
# names are based on the CSV you uploaded and the paper's appendix
needed_vars <- c(
  "record",        # id
  "age_new",       # 1-5
  "gender",        # 1-3
  "Q66",           # education 1-7
  "Q55",           # income 1-7
  "Q40", "Q41", "Q42", "Q43"   # 4 AI/comfort questions
)

missing_vars <- setdiff(needed_vars, names(raw_df))
if (length(missing_vars) > 0) {
  stop("Missing columns in raw data: ", paste(missing_vars, collapse = ", "))
}


# --- 4. subset to only needed columns --------------------------------------
df <- raw_df %>%
  select(all_of(needed_vars))

# --- 5. helper to convert labelled/factor to numeric -----------------------
to_numeric_safely <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return(as.numeric(x))
  } else if (is.factor(x)) {
    return(suppressWarnings(as.numeric(as.character(x))))
  } else {
    return(suppressWarnings(as.numeric(x)))
  }
}

df <- df %>%
  mutate(
    age_new = to_numeric_safely(age_new),
    gender  = to_numeric_safely(gender),
    Q66     = to_numeric_safely(Q66),
    Q55     = to_numeric_safely(Q55),
    Q40     = to_numeric_safely(Q40),
    Q41     = to_numeric_safely(Q41),
    Q42     = to_numeric_safely(Q42),
    Q43     = to_numeric_safely(Q43)
  )

# --- 6. drop unclear / invalid codes (96–99) --------------------------------
INVALID <- 96:99

df <- df %>%
  mutate(
    age_new = ifelse(age_new %in% INVALID, NA, age_new),
    gender  = ifelse(gender  %in% INVALID, NA, gender),
    Q66     = ifelse(Q66     %in% INVALID, NA, Q66),
    Q55     = ifelse(Q55     %in% INVALID, NA, Q55),
    Q40     = ifelse(Q40     %in% INVALID, NA, Q40),
    Q41     = ifelse(Q41     %in% INVALID, NA, Q41),
    Q42     = ifelse(Q42     %in% INVALID, NA, Q42),
    Q43     = ifelse(Q43     %in% INVALID, NA, Q43)
  )


# 7A. PAPER-LIKE CLEANING
# drop ONLY invalid demographics; keep Q40–Q43 even if NA
# ---------------------------------------------------------------------------
df_clean <- df %>%
  drop_na(age_new, gender, Q66, Q55)

# recode factors (paper appendix)
df_clean <- df_clean %>%
  mutate(
    age_new = factor(
      age_new,
      levels = 1:5,
      labels = c("16–24", "25–34", "35–54", "55–64", "65+")
    ),
    gender = factor(
      gender,
      levels = c(1, 2, 3),
      labels = c("Male", "Female", "Other")
    ),
    Q66 = factor(
      Q66,
      levels = 1:7,
      labels = c(
        "High school",
        "Apprenticeship/Trades",
        "College/CEGEP",
        "University degree",
        "Masters",
        "PhD",
        "Medical/paramedical"
      )
    ),
    Q55 = factor(
      Q55,
      levels = 1:7,
      labels = c(
        "<$24,999",
        "$25,000–$49,999",
        "$50,000–$79,999",
        "$80,000–$99,999",
        "$100,000–$149,999",
        "$150,000–$249,999",
        "$250,000+"
      )
    ),
    Q40 = factor(Q40, levels = 1:4, ordered = TRUE),
    Q41 = factor(Q41, levels = 1:4, ordered = TRUE),
    Q42 = factor(Q42, levels = 1:4, ordered = TRUE),
    Q43 = factor(Q43, levels = 1:4, ordered = TRUE)
  )

# summary for paper-like dataset
summary_tbl <- bind_rows(
  tibble(variable = "age_new",
         level    = levels(df_clean$age_new),
         n        = as.numeric(table(df_clean$age_new))),
  tibble(variable = "gender",
         level    = levels(df_clean$gender),
         n        = as.numeric(table(df_clean$gender))),
  tibble(variable = "Q66",
         level    = levels(df_clean$Q66),
         n        = as.numeric(table(df_clean$Q66))),
  tibble(variable = "Q55",
         level    = levels(df_clean$Q55),
         n        = as.numeric(table(df_clean$Q55)))
)

# ---------------------------------------------------------------------------
# 7B. STRICT CLEANING
# on top of demographics cleaning, also require Q40–Q43 to be non-missing
# ---------------------------------------------------------------------------
df_clean_alt <- df %>%
  drop_na(age_new, gender, Q66, Q55, Q40, Q41, Q42, Q43)

df_clean_alt <- df_clean_alt %>%
  mutate(
    age_new = factor(
      age_new,
      levels = 1:5,
      labels = c("16–24", "25–34", "35–54", "55–64", "65+")
    ),
    gender = factor(
      gender,
      levels = c(1, 2, 3),
      labels = c("Male", "Female", "Other")
    ),
    Q66 = factor(
      Q66,
      levels = 1:7,
      labels = c(
        "High school",
        "Apprenticeship/Trades",
        "College/CEGEP",
        "University degree",
        "Masters",
        "PhD",
        "Medical/paramedical"
      )
    ),
    Q55 = factor(
      Q55,
      levels = 1:7,
      labels = c(
        "<$24,999",
        "$25,000–$49,999",
        "$50,000–$79,999",
        "$80,000–$99,999",
        "$100,000–$149,999",
        "$150,000–$249,999",
        "$250,000+"
      )
    ),
    Q40 = factor(Q40, levels = 1:4, ordered = TRUE),
    Q41 = factor(Q41, levels = 1:4, ordered = TRUE),
    Q42 = factor(Q42, levels = 1:4, ordered = TRUE),
    Q43 = factor(Q43, levels = 1:4, ordered = TRUE)
  )

summary_tbl_alt <- bind_rows(
  tibble(variable = "age_new",
         level    = levels(df_clean_alt$age_new),
         n        = as.numeric(table(df_clean_alt$age_new))),
  tibble(variable = "gender",
         level    = levels(df_clean_alt$gender),
         n        = as.numeric(table(df_clean_alt$gender))),
  tibble(variable = "Q66",
         level    = levels(df_clean_alt$Q66),
         n        = as.numeric(table(df_clean_alt$Q66))),
  tibble(variable = "Q55",
         level    = levels(df_clean_alt$Q55),
         n        = as.numeric(table(df_clean_alt$Q55)))
)



# --- 8. save all outputs ----------------------------------------------------
# paper-like
saveRDS(df_clean, clean_rds_path)
write_csv(df_clean, clean_csv_path)
write_csv(summary_tbl, summary_csv_path)

# strict
saveRDS(df_clean_alt, clean_alt_rds_path)
write_csv(df_clean_alt, clean_alt_csv_path)
write_csv(summary_tbl_alt, summary_alt_csv_path)

message("Cleaning finished.")
message("Paper-like data saved to: ", clean_rds_path)
message("Strict data saved to: ", clean_alt_rds_path)

# optional, before commit:
# renv::snapshot()


