#############################################
# 03_models_Q40_to_Q43.R
# Purpose:
#   Fit logistic regression models (binary) for Q40–Q43
#   dichotomizing 1-2 = 0 (low), 3-4 = 1 (high)
# Input:
#   data_clean/clean_data.rds
# Output:
#   output/models_Q40_to_Q43.rds        (list of glm objects)
#   output/model_Q40_tidy.csv
#   output/model_Q41_tidy.csv
#   output/model_Q42_tidy.csv
#   output/model_Q43_tidy.csv
#############################################

library(tidyverse)
library(here)
library(broom)

# 1. load data -------------------------------------------------------
df <- readr::read_rds(here("data_clean", "clean_data.rds")) %>%
  rename(
  age = age_new,
  income = Q55,
  education = Q66
  )
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
}

# 2. helper: 4-level -> binary ---------------------------------------
# your scale:
# 1 = not at all knowledgeable
# 2 = 
# 3 = 
# 4 = very knowledgeable
# ==> we define "high" as 3 or 4
recode_4to2 <- function(x) {
  case_when(
    x %in% c(3, 4) ~ 1,
    x %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_     # NA already covers original 96-99
  )
}

# 3. build per-question datasets -------------------------------------
# NOTE: we assume age, gender, income, education are already
#       factors with correct reference levels in the cleaned data.
#       If not, add fct_relevel() here.

# Q40
df_q40 <- df %>%
  mutate(Q40_bin = recode_4to2(Q40)) %>%
  drop_na(Q40_bin, age, gender, income, education)

# Q41
df_q41 <- df %>%
  mutate(Q41_bin = recode_4to2(Q41)) %>%
  drop_na(Q41_bin, age, gender, income, education)

# Q42
df_q42 <- df %>%
  mutate(Q42_bin = recode_4to2(Q42)) %>%
  drop_na(Q42_bin, age, gender, income, education)

# Q43
df_q43 <- df %>%
  mutate(Q43_bin = recode_4to2(Q43)) %>%
  drop_na(Q43_bin, age, gender, income, education)

n_q40 <- nrow(df_q40)
n_q41 <- nrow(df_q41)
n_q42 <- nrow(df_q42)
n_q43 <- nrow(df_q43)

message("N(Q40) = ", n_q40)
message("N(Q41) = ", n_q41)
message("N(Q42) = ", n_q42)
message("N(Q43) = ", n_q43)

# 4. fit logistic models ----------------------------------------------
m_q40 <- glm(Q40_bin ~ age + gender + income + education,
             data = df_q40, family = binomial)

m_q41 <- glm(Q41_bin ~ age + gender + income + education,
             data = df_q41, family = binomial)

m_q42 <- glm(Q42_bin ~ age + gender + income + education,
             data = df_q42, family = binomial)

m_q43 <- glm(Q43_bin ~ age + gender + income + education,
             data = df_q43, family = binomial)

# 5. tidy & export ----------------------------------------------------
tidy_q40 <- tidy(m_q40, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(question = "Q40", N = n_q40)
tidy_q41 <- tidy(m_q41, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(question = "Q41", N = n_q41)
tidy_q42 <- tidy(m_q42, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(question = "Q42", N = n_q42)
tidy_q43 <- tidy(m_q43, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(question = "Q43", N = n_q43)

write_csv(tidy_q40, here("output", "model_Q40_tidy.csv"))
write_csv(tidy_q41, here("output", "model_Q41_tidy.csv"))
write_csv(tidy_q42, here("output", "model_Q42_tidy.csv"))
write_csv(tidy_q43, here("output", "model_Q43_tidy.csv"))

# 6. save models as a list -------------------------------------------
saveRDS(
  list(Q40 = m_q40,
       Q41 = m_q41,
       Q42 = m_q42,
       Q43 = m_q43),
  here("output", "models_Q40_to_Q43.rds")
)

message("03_models_Q40_to_Q43.R finished.")
