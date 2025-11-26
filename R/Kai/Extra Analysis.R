library(readr)

cdhs21 <- readr::read_csv("cdhs_aft_t1.csv")
cdhs23 <- readr::read_csv("outputs/tables/2023/cdhs23_after_t1.csv")
cdhs24 <- readr::read_csv("outputs/tables/2024/cdhs24_after_t1.csv")

models_2021 <- readRDS("outputs/plots/2021/poly_models_2021.rds")
models_2023 <- readRDS("outputs/plots/2023/poly_models_2023.rds")
models_2024 <- readRDS("outputs/plots/2024/poly_models_2024.rds")

nested_q40_2021 <- readr::read_csv("outputs/plots/2021/nested_models/2021_Q40_nested_models.csv")
nested_q41_2021 <- readr::read_csv("outputs/plots/2021/nested_models/2021_Q41_nested_models.csv")
nested_q42_2021 <- readr::read_csv("outputs/plots/2021/nested_models/2021_Q42_nested_models.csv")
nested_q43_2021 <- readr::read_csv("outputs/plots/2021/nested_models/2021_Q43_nested_models.csv")

nested_q31_2023 <- readr::read_csv("outputs/plots/2023/nested_models/2023_Q31_nested_models.csv")
nested_q32_2023 <- readr::read_csv("outputs/plots/2023/nested_models/2023_Q32_nested_models.csv")
nested_q33_2023 <- readr::read_csv("outputs/plots/2023/nested_models/2023_Q33_nested_models.csv")
nested_q34_2023 <- readr::read_csv("outputs/plots/2023/nested_models/2023_Q34_nested_models.csv")

nested_q28_a1_2024 <- readr::read_csv("outputs/plots/2024/nested_models/2024_Q28_A1_nested_models.csv")
nested_q28_a2_2024 <- readr::read_csv("outputs/plots/2024/nested_models/2024_Q28_A2_nested_models.csv")
nested_q28_a3_2024 <- readr::read_csv("outputs/plots/2024/nested_models/2024_Q28_A3_nested_models.csv")
nested_q28_a4_2024 <- readr::read_csv("outputs/plots/2024/nested_models/2024_Q28_A4_nested_models.csv")
nested_q28_a5_2024 <- readr::read_csv("outputs/plots/2024/nested_models/2024_Q28_A5_nested_models.csv")
nested_q28_a6_2024 <- readr::read_csv("outputs/plots/2024/nested_models/2024_Q28_A6_nested_models.csv")
nested_q28_a7_2024 <- readr::read_csv("outputs/plots/2024/nested_models/2024_Q28_A7_nested_models.csv")


# ============================================================

library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(gt)

# ---------- helper: convert factor/character to numeric code ----------
convert_to_num <- function(x, levels_vec) {
  x <- factor(x, levels = levels_vec)
  as.numeric(x)
}

# ---------- helper: fit the 4 nested models & make table ----------
fit_lecture_models <- function(df,
                               yvar,
                               age_var,
                               educ_var = "education",
                               gender_var = "gender",
                               age_levels,
                               educ_levels,
                               gender_levels,
                               wave_tag,
                               outcome_label,
                               out_dir) {
  # Keep only complete cases on the four fields we use
  df_use <- df %>%
    dplyr::select(dplyr::all_of(c(yvar, age_var, educ_var, gender_var))) %>%
    tidyr::drop_na()
  
  # Numeric coding
  df_use <- df_use %>%
    mutate(
      y_num      = as.numeric(.data[[yvar]]),
      age_f      = factor(.data[[age_var]], levels = age_levels),
      educ_f     = factor(.data[[educ_var]], levels = educ_levels),
      gender_f   = factor(.data[[gender_var]], levels = gender_levels),
      age_num    = as.numeric(age_f),
      educ_num   = as.numeric(educ_f),
      gender_num = as.numeric(gender_f)
    )
  
  # 4 nested models (like lecture example)
  m1 <- lm(y_num ~ age_num, data = df_use)
  m2 <- lm(y_num ~ age_num + educ_num, data = df_use)
  m3 <- lm(y_num ~ age_num + educ_num + age_num:educ_num, data = df_use)
  m4 <- lm(y_num ~ age_num + educ_num + gender_num +
             age_num:educ_num + age_num:gender_num, data = df_use)
  
  # Collect coefficients (betas) per model
  get_coefs <- function(mod, model_name) {
    tidy(mod) %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate) %>%
      rename(!!model_name := estimate)
  }
  
  coef_tables <- list(
    get_coefs(m1, "Model_1"),
    get_coefs(m2, "Model_2"),
    get_coefs(m3, "Model_3"),
    get_coefs(m4, "Model_4")
  )
  
  coef_wide <- reduce(coef_tables, full_join, by = "term")
  
  # Add R² row
  r2_row <- tibble(
    term    = "R2",
    Model_1 = summary(m1)$r.squared,
    Model_2 = summary(m2)$r.squared,
    Model_3 = summary(m3)$r.squared,
    Model_4 = summary(m4)$r.squared
  )
  
  tbl <- bind_rows(coef_wide, r2_row) %>%
    mutate(across(starts_with("Model_"), ~ round(.x, 3)))
  
  # Nice labels for terms
  term_labels <- c(
    age_num             = "β(age)",
    educ_num            = "β(education)",
    gender_num          = "β(gender)",
    "age_num:educ_num"  = "β(age × education)",
    "age_num:gender_num"= "β(age × gender)",
    R2                  = "R²"
  )
  
  tbl <- tbl %>%
    mutate(Parameter = term_labels[term] %||% term) %>%
    select(Parameter, Model_1, Model_2, Model_3, Model_4)
  
  # Output paths
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  csv_path <- file.path(out_dir,
                        paste0(wave_tag, "_", yvar, "_lecture_nested_linear.csv"))
  html_path <- file.path(out_dir,
                         paste0(wave_tag, "_", yvar, "_lecture_nested_linear.html"))
  
  readr::write_csv(tbl, csv_path)
  
  gt_tbl <- tbl %>%
    gt() %>%
    tab_header(
      title = md(
        paste0("Nested linear models with interactions — ",
               wave_tag, " ", outcome_label)
      )
    ) %>%
    cols_label(
      Parameter = md("**Parameter**"),
      Model_1   = md("**Model (1)**"),
      Model_2   = md("**Model (2)**"),
      Model_3   = md("**Model (3)**"),
      Model_4   = md("**Model (4)**")
    )
  
  gtsave(gt_tbl, html_path)
  
  invisible(tbl)
}

# ============================================================
# 2021: Q40–Q43
# ============================================================

age_levels_21    <- c("16–24 years","25–34 years","35–44 years","45–54 years","55+ years")
educ_levels_21   <- c("Highschool","Apprenticeship/Trades","College/CEGEP",
                      "University degree","Masters","PhD","Medical/paramedical")
gender_levels_21 <- c("male","female","other")

outcomes_21 <- c("q40_o","q41_o","q42_o","q43_o")
labels_21   <- c("Knowledge of AI (Q40)",
                 "Comfort with AI in healthcare (Q41)",
                 "Comfort with AI using consented data (Q42)",
                 "Comfort with AI using de-identified data (Q43)")

out_dir_21 <- "outputs/nested_linear/2021"

for (i in seq_along(outcomes_21)) {
  fit_lecture_models(
    df           = cdhs21,
    yvar         = outcomes_21[i],
    age_var      = "age5",
    age_levels   = age_levels_21,
    educ_levels  = educ_levels_21,
    gender_levels= gender_levels_21,
    wave_tag     = "2021",
    outcome_label= labels_21[i],
    out_dir      = out_dir_21
  )
}

# ============================================================
# 2023: Q31–Q34
# ============================================================

age_levels_23    <- c("16–24 years","25–34 years","35–54 years",
                      "55–64 years","65+ years")
educ_levels_23   <- c("No certificate","High school","Apprenticeship/Trades",
                      "College/CEGEP","University < bachelor","University ≥ bachelor")
gender_levels_23 <- c("male","female","other")

outcomes_23 <- c("q31_o","q32_o","q33_o","q34_o")
labels_23   <- c("Knowledge of AI (Q31)",
                 "Comfort with AI in healthcare (Q32)",
                 "Comfort with AI using consented data (Q33)",
                 "Comfort with AI using de-identified data (Q34)")

out_dir_23 <- "outputs/nested_linear/2023"

for (i in seq_along(outcomes_23)) {
  fit_lecture_models(
    df           = cdhs23,
    yvar         = outcomes_23[i],
    age_var      = "age5",
    age_levels   = age_levels_23,
    educ_levels  = educ_levels_23,
    gender_levels= gender_levels_23,
    wave_tag     = "2023",
    outcome_label= labels_23[i],
    out_dir      = out_dir_23
  )
}

# ============================================================
# 2024: Q28_A1–Q28_A7
# ============================================================

age_levels_24    <- c("16–17","18–24","25–34","35–44",
                      "45–54","55–64","65–74","75+")
educ_levels_24   <- c("No certificate","High school","Apprenticeship/Trades",
                      "College/CEGEP","University < bachelor",
                      "Bachelor","Graduate+")
gender_levels_24 <- c("male","female","other")

outcomes_24 <- c("q28_a1_o","q28_a2_o","q28_a3_o","q28_a4_o",
                 "q28_a5_o","q28_a6_o","q28_a7_o")
labels_24   <- c("Q28_A1","Q28_A2","Q28_A3","Q28_A4",
                 "Q28_A5","Q28_A6","Q28_A7")

out_dir_24 <- "outputs/nested_linear/2024"

for (i in seq_along(outcomes_24)) {
  fit_lecture_models(
    df           = cdhs24,
    yvar         = outcomes_24[i],
    age_var      = "age_full",
    age_levels   = age_levels_24,
    educ_levels  = educ_levels_24,
    gender_levels= gender_levels_24,
    wave_tag     = "2024",
    outcome_label= labels_24[i],
    out_dir      = out_dir_24
  )
}

