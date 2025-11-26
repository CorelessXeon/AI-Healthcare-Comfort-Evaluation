#############################################
# 07_extension_tables.R
# Purpose:
#   - Generates distributive tables with new datasets
#   - Extent Appendix Table 1A with new dataset — Ordinal logistic regression
# Input:
#   data/cleaned_dataset_2023.csv
#   data/cleaned_dataset_2024.csv
# Output:
#   artifacts/tables/table1_2023.html
#   artifacts/tables/table1_2023.csv
#   artifacts/tables/appendix_table_1A_2023.html
#   artifacts/tables/appendix_table_1A_2023.csv
#   artifacts/models/q40_olr_model_2023.rds
#   artifacts/models/q41_olr_model_2023.rds
#   artifacts/models/q42_olr_model_2023.rds
#   artifacts/models/q43_olr_model_2023.rds
#   artifacts/tables/table1_2024.html
#   artifacts/tables/table1_2024.csv
#   artifacts/tables/appendix_table_1A_2024.html
#   artifacts/tables/appendix_table_1A_2024.csv
#   artifacts/models/q40_olr_model_2024.rds
#   artifacts/models/q41_olr_model_2024.rds
#   artifacts/models/q42_olr_model_2024.rds
#   artifacts/models/q43_olr_model_2024.rds
#############################################


model_df_2023 <- readRDS("data/cleaned_dataset_2023.rds")

# If outcomes are still named q40_un_o etc., rename them to q40_o etc.
if (all(c("q40_un_o", "q41_un_o", "q42_un_o", "q43_un_o") %in% names(model_df_2023))) {
  model_df_2023 <- model_df_2023 %>%
    rename(
      q40_o = q40_un_o,
      q41_o = q41_un_o,
      q42_o = q42_un_o,
      q43_o = q43_un_o
    )
}

# ---- 2) Strict filtering on Q40–Q43 (same logic as 02 script) ----
q_vars_2023 <- c("q40_o", "q41_o", "q42_o", "q43_o")

sam_size_std_filter_2023 <- nrow(model_df_2023)

keep_idx_2023 <- stats::complete.cases(model_df_2023[, q_vars_2023])

model_df_2023_strict <- model_df_2023[keep_idx_2023, , drop = FALSE]

sam_size_strict_filter_2023 <- nrow(model_df_2023_strict)

message(
  sprintf(
    "Strict filter on q40_o–q43_o (2023): %d -> %d rows kept.",
    sam_size_std_filter_2023, sam_size_strict_filter_2023
  )
)

# Save strict-filtered version for possible later reuse
saveRDS(model_df_2023_strict, "data/cleaned_dataset_2023_strict.rds")

# ---- 3) Ensure factors and levels (age5, income, education, gender) ----
# You can adjust these as needed, but this keeps the pattern of the 02 script.

model_df_2023_strict <- model_df_2023_strict %>%
  mutate(
    age5 = case_when(
      age5 == "55–64 years" ~ "65+ years",
      age5 == "45–54 years" ~ "55–64 years",
      age5 == "35–44 years" ~ "35–54 years",
      TRUE ~ age5
    ),
    age5 = factor(
      age5,
      levels = c(
        "16–24 years",
        "25–34 years",
        "35–54 years",
        "55–64 years",
        "65+ years"
      )
    ),
    income    = as.factor(income),
    education = as.factor(education),
    gender    = forcats::fct_relevel(as.factor(gender), "male")  # put male first
  )

lvl_age_2023       <- levels(model_df_2023_strict$age5)
lvl_income_2023    <- levels(model_df_2023_strict$income)
lvl_education_2023 <- levels(model_df_2023_strict$education)
lvl_gender_2023    <- levels(model_df_2023_strict$gender)

# ---- 4) Grouping and building frequency tables (2023) ----
#     ---- Age (group by gender) ----
tbl_age_2023 <- cross_tab_counts(
  df            = model_df_2023_strict,
  factor_var    = age5,
  factor_levels = lvl_age_2023,
  group_var     = gender,
  group_levels  = lvl_gender_2023,
  section_title = "Age in years"
)

#     ---- Income (group by gender) ----
tbl_inc_2023 <- cross_tab_counts(
  df            = model_df_2023_strict,
  factor_var    = income,
  factor_levels = lvl_income_2023,
  group_var     = gender,
  group_levels  = lvl_gender_2023,
  section_title = "Income"
)

#     ---- Education (group by gender) ----
tbl_edu_2023 <- cross_tab_counts(
  df            = model_df_2023_strict,
  factor_var    = education,
  factor_levels = lvl_education_2023,
  group_var     = gender,
  group_levels  = lvl_gender_2023,
  section_title = "Education"
)

#     ---- Combine all tables ----
table1_2023_long <- dplyr::bind_rows(tbl_age_2023, tbl_inc_2023, tbl_edu_2023)

# ---- 5) Create Table 1 with gt: descriptive stats by factor and gender ----
table1_2023_gt <- table1_2023_long %>%
  gt::gt(groupname_col = "Factor") %>%
  gt::tab_header(
    title = "Table 1 (2023). Descriptive statistics of respondents based on socioeconomic and demographic characteristics."
  ) %>%
  gt::fmt_number(
    columns = c(`Sample Size (male)`, `Sample Size (female)`, `Sample Size (other)`),
    decimals = 0
  ) %>%
  gt::tab_source_note(
    source_note = "Age uses the bins present in the CDHS 2023 export. Reference categories may differ from the original 2021 article."
  )

# ---- 6) Save outputs ----
if (!dir.exists("artifacts/tables")) dir.create("artifacts/tables", recursive = TRUE)

gt::gtsave(table1_2023_gt, "artifacts/tables/table1_2023.html")
readr::write_csv(table1_2023_long, "artifacts/tables/table1_2023.csv")

# Ensure factor types and reference categories for 2023
model_df_2023 <- model_df_2023 %>%
  mutate(
    age5 = case_when(
      age5 == "55–64 years" ~ "65+ years",
      age5 == "45–54 years" ~ "55–64 years",
      age5 == "35–44 years" ~ "35–54 years",
      TRUE ~ age5
    ),
    age5 = factor(
      age5,
      levels = c(
        "16–24 years",
        "25–34 years",
        "35–54 years",
        "55–64 years",
        "65+ years"
      )
    ),
    gender    = forcats::fct_relevel(as.factor(gender), "male"),  # male as reference
    education = as.factor(education),
    income    = as.factor(income)
  )

# Level vectors for 2023 (analogous to lvl_age, lvl_gender, etc.)
lvl_age_2023       <- levels(model_df_2023$age5)
lvl_gender_2023    <- levels(model_df_2023$gender)
lvl_education_2023 <- levels(model_df_2023$education)
lvl_income_2023    <- levels(model_df_2023$income)

# Safety checks
stopifnot(
  all(c("q40_o", "q41_o", "q42_o", "q43_o",
        "age5", "gender", "education", "income") %in% names(model_df_2023))
)
stopifnot(
  is.ordered(model_df_2023$q40_o),
  is.ordered(model_df_2023$q41_o),
  is.ordered(model_df_2023$q42_o),
  is.ordered(model_df_2023$q43_o)
)
stopifnot(
  is.factor(model_df_2023$age5),
  is.factor(model_df_2023$gender),
  is.factor(model_df_2023$education),
  is.factor(model_df_2023$income)
)

# ------------------------------------------------------------
# Helper: fit a single OLR model for one outcome in 2023
# ------------------------------------------------------------
fit_olr_simple_2023 <- function(df, yvar_chr) {
  
  # 1) restrict to needed columns and drop any NA
  df_use <- df %>%
    dplyr::select(all_of(c(yvar_chr, "age5", "gender", "education", "income"))) %>%
    tidyr::drop_na()
  
  stopifnot(is.ordered(df_use[[yvar_chr]]))
  
  # 2) build formula and fit polr
  fml   <- as.formula(paste0(yvar_chr, " ~ age5 + gender + education + income"))
  model <- MASS::polr(fml, data = df_use, Hess = TRUE)
  
  # 3) tidy results WITHOUT profile-based confint
  tt <- broom::tidy(model) %>%
    dplyr::filter(grepl("^(age5|gender|education|income)", term)) %>%
    dplyr::mutate(
      OR  = exp(estimate),
      LCL = ifelse(
        is.na(std.error),
        NA_real_,
        exp(estimate - 1.96 * std.error)
      ),
      UCL = ifelse(
        is.na(std.error),
        NA_real_,
        exp(estimate + 1.96 * std.error)
      ),
      p.value = 2 * pnorm(-abs(statistic))
    )
  
  # 4) 
  age_map    <- setNames(lvl_age_2023[-1],       grep("^age5",      tt$term, value = TRUE))
  gender_map <- setNames(lvl_gender_2023[-1],    grep("^gender",    tt$term, value = TRUE))
  edu_map    <- setNames(lvl_education_2023[-1], grep("^education", tt$term, value = TRUE))
  inc_map    <- setNames(lvl_income_2023[-1],    grep("^income",    tt$term, value = TRUE))
  
  Factor <- dplyr::case_when(
    tt$term %in% names(age_map)    ~ "Age",
    tt$term %in% names(gender_map) ~ "Gender",
    tt$term %in% names(edu_map)    ~ "Education",
    tt$term %in% names(inc_map)    ~ "Income",
    TRUE ~ NA_character_
  )
  
  Category <- dplyr::coalesce(
    map_cat(tt$term, age_map,    "Age"),
    map_cat(tt$term, gender_map, "Gender"),
    map_cat(tt$term, edu_map,    "Education"),
    map_cat(tt$term, inc_map,    "Income")
  )
  
  effects <- tibble::tibble(
    Factor   = Factor,
    Category = Category,
    OR       = tt$OR,
    LCL      = tt$LCL,
    UCL      = tt$UCL,
    p.value  = tt$p.value
  ) %>%
    dplyr::filter(!is.na(Factor), !is.na(Category))
  
  # 5) 
  build_factor_block <- function(factor_name, level_vec) {
    tibble::tibble(
      Factor   = factor_name,
      Category = level_vec
    ) %>%
      dplyr::left_join(
        effects %>% dplyr::filter(Factor == factor_name),
        by = c("Factor", "Category")
      ) %>%
      dplyr::mutate(
        is_ref = (Category == level_vec[1])  
      )
  }
  
  block_age  <- build_factor_block("Age",       lvl_age_2023)
  block_sex  <- build_factor_block("Gender",    lvl_gender_2023)
  block_inc  <- build_factor_block("Income",    lvl_income_2023)
  block_edu  <- build_factor_block("Education", lvl_education_2023)
  
  res <- dplyr::bind_rows(block_age, block_sex, block_inc, block_edu) %>%
    dplyr::mutate(
      Factor_f = factor(Factor,
                        levels = c("Age", "Gender", "Income", "Education")),
      Cat_f = dplyr::case_when(
        Factor == "Age"       ~ factor(Category, levels = lvl_age_2023),
        Factor == "Gender"    ~ factor(Category, levels = lvl_gender_2023),
        Factor == "Income"    ~ factor(Category, levels = lvl_income_2023),
        Factor == "Education" ~ factor(Category, levels = lvl_education_2023),
        TRUE                  ~ factor(Category)
      )
    ) %>%
    dplyr::arrange(Factor_f, Cat_f)
  
  # 6) Outcome label
  out_label <- dplyr::case_when(
    yvar_chr == "q40_o" ~ "Knowledge of AI (Q40)",
    yvar_chr == "q41_o" ~ "Comfort with AI (Q41)",
    yvar_chr == "q42_o" ~ "Comfort with AI using consented data (Q42)",
    yvar_chr == "q43_o" ~ "Comfort with AI using de-identified data (Q43)",
    TRUE ~ yvar_chr
  )
  
  # 7) 
  out_table <- res %>%
    dplyr::mutate(
      has_est = !is.na(OR),
      `Odds Ratio (95% CI)` = dplyr::case_when(
        is_ref ~ "1.00 (ref)",
        !has_est ~ "",
        TRUE ~ paste0(fmt2(OR), " (", fmt2(LCL), "–", fmt2(UCL), ")")
      ),
      `p-value` = dplyr::case_when(
        is_ref ~ "",
        is.na(p.value) ~ "",
        TRUE ~ fmt2(p.value)
      ),
      sig = dplyr::case_when(
        is_ref ~ "",
        is.na(p.value) ~ "",
        TRUE ~ stars(p.value)
      ),
      Outcome = out_label
    ) %>%
    dplyr::select(
      Outcome,
      Factor,
      Categories = Category,
      `Odds Ratio (95% CI)`,
      `p-value`,
      sig
    )
  
  list(
    model = model,
    table = out_table
  )
}


# ------------------------------------------------------------
# 5) Fit all four 2023 models on model_df_2023
# ------------------------------------------------------------

q40_2023 <- fit_olr_simple_2023(model_df_2023, "q40_o")
q41_2023 <- fit_olr_simple_2023(model_df_2023, "q41_o")
q42_2023 <- fit_olr_simple_2023(model_df_2023, "q42_o")
q43_2023 <- fit_olr_simple_2023(model_df_2023, "q43_o")

if (!dir.exists("artifacts/models")) dir.create("artifacts/models", recursive = TRUE)

saveRDS(q40_2023$model, "artifacts/models/q40_olr_model_2023.rds")
saveRDS(q41_2023$model, "artifacts/models/q41_olr_model_2023.rds")
saveRDS(q42_2023$model, "artifacts/models/q42_olr_model_2023.rds")
saveRDS(q43_2023$model, "artifacts/models/q43_olr_model_2023.rds")

tbl_q40_2023 <- q40_2023$table
tbl_q41_2023 <- q41_2023$table
tbl_q42_2023 <- q42_2023$table
tbl_q43_2023 <- q43_2023$table

# ------------------------------------------------------------
# 6) Combine result tables into Appendix 1A-style report
# ------------------------------------------------------------

tbl_q40_2023$`Dependent Variable` <- "Q.40 – How knowledgeable are you about what artificial intelligence is?"
tbl_q41_2023$`Dependent Variable` <- "Q.41 – How comfortable are you with AI being used as a tool in healthcare?"
tbl_q42_2023$`Dependent Variable` <- "Q.42 – How comfortable are you with AI using consented data?"
tbl_q43_2023$`Dependent Variable` <- "Q.43 – How comfortable are you with AI using de-identified data?"

appendix_1a_2023_final <- dplyr::bind_rows(
  tbl_q40_2023,
  tbl_q41_2023,
  tbl_q42_2023,
  tbl_q43_2023
) %>%
  dplyr::select(
    `Dependent Variable`,
    Factor,
    Categories,
    `Odds Ratio (95% CI)`,
    `p-value`
  )

# ------------------------------------------------------------
# 7) Build GT table and save HTML + CSV
# ------------------------------------------------------------

if (!dir.exists("artifacts/tables")) dir.create("artifacts/tables", recursive = TRUE)

appendix_1a_2023_gt <- appendix_1a_2023_final %>%
  gt::gt(groupname_col = "Dependent Variable") %>%
  gt::tab_header(
    title = gt::md(
      "**Appendix Table 1A (2023): Ordinal logistic regression — Odds ratios (95% CI) and p-values**"
    )
  ) %>%
  gt::cols_label(
    Factor                = gt::md("**Factor**"),
    Categories            = gt::md("**Categories**"),
    `Odds Ratio (95% CI)` = gt::md("**Odds Ratio (95% CI)**"),
    `p-value`             = gt::md("**p-value**")
  ) %>%
  gt::fmt_number(columns = "p-value", decimals = 3) %>%
  gt::tab_source_note(
    source_note = gt::md(
      paste0(
        "Notes (2023): Reference categories — Age = ", lvl_age_2023[1],
        "; Gender = ", lvl_gender_2023[1],
        "; Education = ", lvl_education_2023[1],
        "; Income = ", lvl_income_2023[1], "."
      )
    )
  )

gt::gtsave(
  appendix_1a_2023_gt,
  "artifacts/tables/appendix_table_1A_2023.html"
)

readr::write_csv(
  appendix_1a_2023_final,
  "artifacts/tables/appendix_table_1A_2023.csv"
)

############################################################
# 2024 extension: Table 1 + Appendix Table 1A (OLR)
############################################################

model_df_2024 <- readRDS("data/cleaned_dataset_2024.rds")

# If outcomes are still named q40_un_o etc., rename them to q40_o etc.
if (all(c("q40_un_o", "q41_un_o", "q42_un_o", "q43_un_o") %in% names(model_df_2024))) {
  model_df_2024 <- model_df_2024 %>%
    dplyr::rename(
      q40_o = q40_un_o,
      q41_o = q41_un_o,
      q42_o = q42_un_o,
      q43_o = q43_un_o
    )
}

# ---- 1) Strict filtering on Q40–Q43 (same logic as 2023) ----
q_vars_2024 <- c("q40_o", "q41_o", "q42_o", "q43_o")

sam_size_std_filter_2024 <- nrow(model_df_2024)

keep_idx_2024 <- stats::complete.cases(model_df_2024[, q_vars_2024])

model_df_2024_strict <- model_df_2024[keep_idx_2024, , drop = FALSE]

sam_size_strict_filter_2024 <- nrow(model_df_2024_strict)

message(
  sprintf(
    "Strict filter on q40_o–q43_o (2024): %d -> %d rows kept.",
    sam_size_std_filter_2024, sam_size_strict_filter_2024
  )
)

# Save strict-filtered version for possible later reuse
saveRDS(model_df_2024_strict, "data/cleaned_dataset_2024_strict.rds")

# ---- 2) Ensure factors and levels (age5, income, education, gender) ----
#   16–17 + 18–24 -> 16–24
#   25–34         -> 25–34
#   35–44 + 45–54 -> 35–54
#   55–64         -> 55–64
#   65–74 + 75+   -> 65+

collapse_age_to_5 <- function(df) {
  df %>%
    dplyr::mutate(
      age5 = dplyr::case_when(
        as.character(age5) %in% c("16–17 years", "18–24 years") ~ "16–24 years",
        as.character(age5) ==  "25–34 years"                     ~ "25–34 years",
        as.character(age5) %in% c("35–44 years", "45–54 years") ~ "35–54 years",
        as.character(age5) ==  "55–64 years"                     ~ "55–64 years",
        as.character(age5) %in% c("65–74 years", "75+ years")   ~ "65+ years",
        TRUE ~ NA_character_
      ),
      age5 = factor(
        age5,
        levels = c(
          "16–24 years",
          "25–34 years",
          "35–54 years",
          "55–64 years",
          "65+ years"
        )
      ),
      income    = as.factor(income),
      education = as.factor(education),
      gender    = forcats::fct_relevel(as.factor(gender), "male")
    )
}

model_df_2024_strict <- collapse_age_to_5(model_df_2024_strict)

lvl_age_2024       <- levels(model_df_2024_strict$age5)
lvl_income_2024    <- levels(model_df_2024_strict$income)
lvl_education_2024 <- levels(model_df_2024_strict$education)
lvl_gender_2024    <- levels(model_df_2024_strict$gender)

# ---- 3) Grouping and building frequency tables (2024) ----
#     ---- Age (group by gender) ----
tbl_age_2024 <- cross_tab_counts(
  df            = model_df_2024_strict,
  factor_var    = age5,
  factor_levels = lvl_age_2024,
  group_var     = gender,
  group_levels  = lvl_gender_2024,
  section_title = "Age in years"
)

#     ---- Income (group by gender) ----
tbl_inc_2024 <- cross_tab_counts(
  df            = model_df_2024_strict,
  factor_var    = income,
  factor_levels = lvl_income_2024,
  group_var     = gender,
  group_levels  = lvl_gender_2024,
  section_title = "Income"
)

#     ---- Education (group by gender) ----
tbl_edu_2024 <- cross_tab_counts(
  df            = model_df_2024_strict,
  factor_var    = education,
  factor_levels = lvl_education_2024,
  group_var     = gender,
  group_levels  = lvl_gender_2024,
  section_title = "Education"
)

#     ---- Combine all tables ----
table1_2024_long <- dplyr::bind_rows(tbl_age_2024, tbl_inc_2024, tbl_edu_2024)

# ---- 4) Create Table 1 (2024) with gt ----
table1_2024_gt <- table1_2024_long %>%
  gt::gt(groupname_col = "Factor") %>%
  gt::tab_header(
    title = "Table 1 (2024). Descriptive statistics of respondents based on socioeconomic and demographic characteristics."
  ) %>%
  gt::fmt_number(
    columns = c(`Sample Size (male)`, `Sample Size (female)`, `Sample Size (other)`),
    decimals = 0
  ) %>%
  gt::tab_source_note(
    source_note = "Age grouped into five categories: 16–24, 25–34, 35–54, 55–64, and 65+ years."
  )

if (!dir.exists("artifacts/tables")) dir.create("artifacts/tables", recursive = TRUE)

gt::gtsave(table1_2024_gt, "artifacts/tables/table1_2024.html")
readr::write_csv(table1_2024_long, "artifacts/tables/table1_2024.csv")

# ---- 5) Prepare full 2024 dataset for OLR ----
model_df_2024 <- collapse_age_to_5(model_df_2024)

# Level vectors for 2024
lvl_age_2024       <- levels(model_df_2024$age5)
lvl_gender_2024    <- levels(model_df_2024$gender)
lvl_education_2024 <- levels(model_df_2024$education)
lvl_income_2024    <- levels(model_df_2024$income)

# Safety checks (2024)
stopifnot(
  all(c("q40_o", "q41_o", "q42_o", "q43_o",
        "age5", "gender", "education", "income") %in% names(model_df_2024))
)
stopifnot(
  is.ordered(model_df_2024$q40_o),
  is.ordered(model_df_2024$q41_o),
  is.ordered(model_df_2024$q42_o),
  is.ordered(model_df_2024$q43_o)
)
stopifnot(
  is.factor(model_df_2024$age5),
  is.factor(model_df_2024$gender),
  is.factor(model_df_2024$education),
  is.factor(model_df_2024$income)
)

# ------------------------------------------------------------
# Helper: fit a single OLR model for one outcome in 2024
# ------------------------------------------------------------
fit_olr_simple_2024 <- function(df, yvar_chr) {
  
  # 1) restrict to needed columns and drop any NA
  df_use <- df %>%
    dplyr::select(all_of(c(yvar_chr, "age5", "gender", "education", "income"))) %>%
    tidyr::drop_na()
  
  stopifnot(is.ordered(df_use[[yvar_chr]]))
  
  # 2) build formula and fit polr
  fml   <- as.formula(paste0(yvar_chr, " ~ age5 + gender + education + income"))
  model <- MASS::polr(fml, data = df_use, Hess = TRUE)
  
  # 3) tidy results WITHOUT profile-based confint
  tt <- broom::tidy(model) %>%
    dplyr::filter(grepl("^(age5|gender|education|income)", term)) %>%
    dplyr::mutate(
      OR  = exp(estimate),
      LCL = ifelse(
        is.na(std.error),
        NA_real_,
        exp(estimate - 1.96 * std.error)
      ),
      UCL = ifelse(
        is.na(std.error),
        NA_real_,
        exp(estimate + 1.96 * std.error)
      ),
      p.value = 2 * pnorm(-abs(statistic))
    )
  
  # 4) term -> category
  age_map    <- setNames(lvl_age_2024[-1],       grep("^age5",      tt$term, value = TRUE))
  gender_map <- setNames(lvl_gender_2024[-1],    grep("^gender",    tt$term, value = TRUE))
  edu_map    <- setNames(lvl_education_2024[-1], grep("^education", tt$term, value = TRUE))
  inc_map    <- setNames(lvl_income_2024[-1],    grep("^income",    tt$term, value = TRUE))
  
  Factor <- dplyr::case_when(
    tt$term %in% names(age_map)    ~ "Age",
    tt$term %in% names(gender_map) ~ "Gender",
    tt$term %in% names(edu_map)    ~ "Education",
    tt$term %in% names(inc_map)    ~ "Income",
    TRUE ~ NA_character_
  )
  
  Category <- dplyr::coalesce(
    map_cat(tt$term, age_map,    "Age"),
    map_cat(tt$term, gender_map, "Gender"),
    map_cat(tt$term, edu_map,    "Education"),
    map_cat(tt$term, inc_map,    "Income")
  )
  
  effects <- tibble::tibble(
    Factor   = Factor,
    Category = Category,
    OR       = tt$OR,
    LCL      = tt$LCL,
    UCL      = tt$UCL,
    p.value  = tt$p.value
  ) %>%
    dplyr::filter(!is.na(Factor), !is.na(Category))
  
  # 5) 
  build_factor_block <- function(factor_name, level_vec) {
    tibble::tibble(
      Factor   = factor_name,
      Category = level_vec
    ) %>%
      dplyr::left_join(
        effects %>% dplyr::filter(Factor == factor_name),
        by = c("Factor", "Category")
      ) %>%
      dplyr::mutate(
        is_ref = (Category == level_vec[1])
      )
  }
  
  block_age  <- build_factor_block("Age",       lvl_age_2024)
  block_sex  <- build_factor_block("Gender",    lvl_gender_2024)
  block_inc  <- build_factor_block("Income",    lvl_income_2024)
  block_edu  <- build_factor_block("Education", lvl_education_2024)
  
  res <- dplyr::bind_rows(block_age, block_sex, block_inc, block_edu) %>%
    dplyr::mutate(
      Factor_f = factor(Factor,
                        levels = c("Age", "Gender", "Income", "Education")),
      Cat_f = dplyr::case_when(
        Factor == "Age"       ~ factor(Category, levels = lvl_age_2024),
        Factor == "Gender"    ~ factor(Category, levels = lvl_gender_2024),
        Factor == "Income"    ~ factor(Category, levels = lvl_income_2024),
        Factor == "Education" ~ factor(Category, levels = lvl_education_2024),
        TRUE                  ~ factor(Category)
      )
    ) %>%
    dplyr::arrange(Factor_f, Cat_f)
  
  # 6) Outcome label
  out_label <- dplyr::case_when(
    yvar_chr == "q40_o" ~ "Knowledge of AI (Q40)",
    yvar_chr == "q41_o" ~ "Comfort with AI (Q41)",
    yvar_chr == "q42_o" ~ "Comfort with AI using consented data (Q42)",
    yvar_chr == "q43_o" ~ "AI use by providers improves confidence in diagnosis (Q43)",
    TRUE ~ yvar_chr
  )
  
  # 7)
  out_table <- res %>%
    dplyr::mutate(
      has_est = !is.na(OR),
      `Odds Ratio (95% CI)` = dplyr::case_when(
        is_ref ~ "1.00 (ref)",
        !has_est ~ "",
        TRUE ~ paste0(fmt2(OR), " (", fmt2(LCL), "–", fmt2(UCL), ")")
      ),
      `p-value` = dplyr::case_when(
        is_ref ~ "",
        is.na(p.value) ~ "",
        TRUE ~ fmt2(p.value)
      ),
      sig = dplyr::case_when(
        is_ref ~ "",
        is.na(p.value) ~ "",
        TRUE ~ stars(p.value)
      ),
      Outcome = out_label
    ) %>%
    dplyr::select(
      Outcome,
      Factor,
      Categories = Category,
      `Odds Ratio (95% CI)`,
      `p-value`,
      sig
    )
  
  list(
    model = model,
    table = out_table
  )
}

# ------------------------------------------------------------
# 6) Fit all four 2024 models on model_df_2024
# ------------------------------------------------------------

q40_2024 <- fit_olr_simple_2024(model_df_2024, "q40_o")
q41_2024 <- fit_olr_simple_2024(model_df_2024, "q41_o")
q42_2024 <- fit_olr_simple_2024(model_df_2024, "q42_o")
q43_2024 <- fit_olr_simple_2024(model_df_2024, "q43_o")

if (!dir.exists("artifacts/models")) dir.create("artifacts/models", recursive = TRUE)

saveRDS(q40_2024$model, "artifacts/models/q40_olr_model_2024.rds")
saveRDS(q41_2024$model, "artifacts/models/q41_olr_model_2024.rds")
saveRDS(q42_2024$model, "artifacts/models/q42_olr_model_2024.rds")
saveRDS(q43_2024$model, "artifacts/models/q43_olr_model_2024.rds")

tbl_q40_2024 <- q40_2024$table
tbl_q41_2024 <- q41_2024$table
tbl_q42_2024 <- q42_2024$table
tbl_q43_2024 <- q43_2024$table

# ------------------------------------------------------------
# 7) Combine 2024 result tables into Appendix 1A-style report
# ------------------------------------------------------------

tbl_q40_2024$`Dependent Variable` <- "Q.40 – How knowledgeable are you about what artificial intelligence is?"
tbl_q41_2024$`Dependent Variable` <- "Q.41 – How comfortable are you with AI being used as a tool in healthcare?"
tbl_q42_2024$`Dependent Variable` <- "Q.42 – How comfortable are you with AI using consented data?"
tbl_q43_2024$`Dependent Variable` <- "Q.43 – AI use by health care providers improves my confidence in my disease diagnosis."

appendix_1a_2024_final <- dplyr::bind_rows(
  tbl_q40_2024,
  tbl_q41_2024,
  tbl_q42_2024,
  tbl_q43_2024
) %>%
  dplyr::select(
    `Dependent Variable`,
    Factor,
    Categories,
    `Odds Ratio (95% CI)`,
    `p-value`
  )

# ------------------------------------------------------------
# 8) Build GT table and save HTML + CSV (2024)
# ------------------------------------------------------------

appendix_1a_2024_gt <- appendix_1a_2024_final %>%
  gt::gt(groupname_col = "Dependent Variable") %>%
  gt::tab_header(
    title = gt::md(
      "**Appendix Table 1A (2024): Ordinal logistic regression — Odds ratios (95% CI) and p-values**"
    )
  ) %>%
  gt::cols_label(
    Factor                = gt::md("**Factor**"),
    Categories            = gt::md("**Categories**"),
    `Odds Ratio (95% CI)` = gt::md("**Odds Ratio (95% CI)**"),
    `p-value`             = gt::md("**p-value**")
  ) %>%
  gt::fmt_number(columns = "p-value", decimals = 3) %>%
  gt::tab_source_note(
    source_note = gt::md(
      paste0(
        "Notes (2024): Reference categories — Age = ", lvl_age_2024[1],
        "; Gender = ", lvl_gender_2024[1],
        "; Education = ", lvl_education_2024[1],
        "; Income = ", lvl_income_2024[1], "."
      )
    )
  )

gt::gtsave(
  appendix_1a_2024_gt,
  "artifacts/tables/appendix_table_1A_2024.html"
)

readr::write_csv(
  appendix_1a_2024_final,
  "artifacts/tables/appendix_table_1A_2024.csv"
)



















