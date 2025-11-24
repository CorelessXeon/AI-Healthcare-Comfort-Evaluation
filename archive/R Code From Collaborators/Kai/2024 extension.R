# ============================================================
# MSE609 — CDHS 2024 Extension
# ============================================================

# ---- 2) Load SAV & clean names ----
raw24  <- haven::read_sav("Infoway CDHS 2024 SPSS Raw Data_for Dataverse.sav")
cdhs24 <- raw24 %>% janitor::clean_names()

# Required variables
req24 <- c("q28_a1","q28_a2","q28_a3","q28_a4","q28_a5","q28_a6","q28_a7",
           "q2_age_groups","q5b_gender","q46","q38")
miss24 <- setdiff(req24, names(cdhs24))
if (length(miss24)) stop("Missing 2024 columns in SAV: ", paste(miss24, collapse=", "))

# ---- Recode outcomes & predictors -------------------------

# Outcomes: Q28_A1–A7, 1–5 ordered, 98 = "I don't know" -> NA
norm_outcome_5 <- function(v){
  x <- suppressWarnings(as.numeric(v))
  x[x == 98] <- NA_real_
  x[!(x %in% 1:5)] <- NA_real_
  factor(x, levels = 1:5, ordered = TRUE)
}

cdhs24 <- cdhs24 %>%
  mutate(
    q28_a1_o = norm_outcome_5(q28_a1),
    q28_a2_o = norm_outcome_5(q28_a2),
    q28_a3_o = norm_outcome_5(q28_a3),
    q28_a4_o = norm_outcome_5(q28_a4),
    q28_a5_o = norm_outcome_5(q28_a5),
    q28_a6_o = norm_outcome_5(q28_a6),
    q28_a7_o = norm_outcome_5(q28_a7)
  )

# Age: Q2_Age_Groups
#   0 = 16–17
#   1 = 18–24
#   2 = 25–34
#   3 = 35–44
#   4 = 45–54
#   5 = 55–64
#   6 = 65–74
#   7 = 75+
#   99 = Prefer not to answer
cdhs24 <- cdhs24 %>%
  mutate(
    age_code_raw = as.integer(q2_age_groups),
    age_code_raw = if_else(age_code_raw == 99L, NA_integer_, age_code_raw),
    age_full = case_when(
      age_code_raw == 0L ~ "16–17",
      age_code_raw == 1L ~ "18–24",
      age_code_raw == 2L ~ "25–34",
      age_code_raw == 3L ~ "35–44",
      age_code_raw == 4L ~ "45–54",
      age_code_raw == 5L ~ "55–64",
      age_code_raw == 6L ~ "65–74",
      age_code_raw == 7L ~ "75+",
      TRUE ~ NA_character_
    ),
    age_full = factor(
      age_full,
      levels = c("16–17","18–24","25–34","35–44",
                 "45–54","55–64","65–74","75+"),
      ordered = TRUE
    )
  )

# Gender: Q5b_Gender
#   1 = Female
#   2 = Male
#   3 = Gender diverse / Non-binary / Another gender
#   4 = Unknown / PNTA
cdhs24 <- cdhs24 %>%
  mutate(
    g_code = as.integer(q5b_gender),
    g_code = if_else(g_code == 4L, NA_integer_, g_code),
    gender = case_when(
      g_code == 2L ~ "male",
      g_code == 1L ~ "female",
      g_code == 3L ~ "other",
      TRUE ~ NA_character_
    ),
    gender = forcats::fct_relevel(factor(gender), "male","female","other")
  )

# Education: Q46
#   1 = No certificate, diploma or degree
#   2 = Secondary (high) school diploma or equivalency
#   3 = Apprenticeship or trades certificate or diploma
#   4 = College/CEGEP/other non-university
#   5 = University cert/diploma below bachelor
#   6 = Bachelor level
#   7 = Graduate level
#   99 = PNTA
edu_levels_24 <- c("No certificate","High school","Apprenticeship/Trades",
                   "College/CEGEP","University < bachelor",
                   "Bachelor","Graduate+")

cdhs24 <- cdhs24 %>%
  mutate(
    edu_code = as.integer(q46),
    edu_code = if_else(edu_code == 99L, NA_integer_, edu_code),
    education = case_when(
      edu_code == 1L ~ "No certificate",
      edu_code == 2L ~ "High school",
      edu_code == 3L ~ "Apprenticeship/Trades",
      edu_code == 4L ~ "College/CEGEP",
      edu_code == 5L ~ "University < bachelor",
      edu_code == 6L ~ "Bachelor",
      edu_code == 7L ~ "Graduate+",
      TRUE ~ NA_character_
    ),
    education = factor(education, levels = edu_levels_24)
  )

# Income: Q38
#   1 = < $40k
#   2 = $40k–59,999
#   3 = $60k–79,999
#   4 = $80k–99,999
#   5 = $100k–149,999
#   6 = $150k+
#   98 = Don’t know
#   99 = Prefer not to answer
inc_levels_24 <- c("<$40k","$40–59,999","$60–79,999",
                   "$80–99,999","$100–149,999","$150k+")

cdhs24 <- cdhs24 %>%
  mutate(
    inc_code = as.integer(q38),
    inc_code = if_else(inc_code %in% c(98L,99L), NA_integer_, inc_code),
    income = case_when(
      inc_code == 1L ~ "<$40k",
      inc_code == 2L ~ "$40–59,999",
      inc_code == 3L ~ "$60–79,999",
      inc_code == 4L ~ "$80–99,999",
      inc_code == 5L ~ "$100–149,999",
      inc_code == 6L ~ "$150k+",
      TRUE ~ NA_character_
    ),
    income = factor(income, levels = inc_levels_24)
  )

# ---- dataset for TABLE 1 ----

predictor_vars_24 <- c("age_full","gender","education","income")
outcome_vars_24   <- c("q28_a1_o","q28_a2_o","q28_a3_o","q28_a4_o",
                       "q28_a5_o","q28_a6_o","q28_a7_o")

# keep only respondents who answered ALL predictors AND ALL outcomes
model24_table1 <- cdhs24 %>%
  dplyr::select(all_of(outcome_vars_24), all_of(predictor_vars_24)) %>%
  tidyr::drop_na(all_of(predictor_vars_24),
                 all_of(outcome_vars_24))

# ============================================================
# TABLE 1 — Descriptive statistics 2024
# ============================================================

mk_section_counts_24 <- function(df, factor_var, factor_levels, section_title) {
  fac <- factor(dplyr::pull(df, {{ factor_var }}), levels = factor_levels)
  gen <- factor(df$gender, levels = c("male","female","other"))
  
  tab <- as.matrix(stats::xtabs(~ fac + gen, drop.unused.levels = FALSE))
  
  if (!"male"   %in% colnames(tab)) tab <- cbind(tab, male   = 0)
  if (!"female" %in% colnames(tab)) tab <- cbind(tab, female = 0)
  if (!"other"  %in% colnames(tab)) tab <- cbind(tab, other  = 0)
  
  tab <- tab[factor_levels, c("male","female","other"), drop = FALSE]
  
  tibble::tibble(
    Factor = section_title,
    `Category (Code)` =
      paste0(factor_levels, " (", seq_along(factor_levels), ")"),
    `Sample Size (Male - 1)`   = as.integer(tab[, "male"]),
    `Sample Size (Female - 2)` = as.integer(tab[, "female"]),
    `Sample Size (Other - 3)`  = as.integer(tab[, "other"])
  )
}

# age levels for 2024 (Q2_Age_Groups -> age_full, 8 bins)
age_lvls_24 <- c("16–17","18–24","25–34","35–44",
                 "45–54","55–64","65–74","75+")

# income levels for 2024 (Q38)
inc_levels_24 <- c("<$40k","$40–59,999","$60–79,999",
                   "$80–99,999","$100–149,999","$150k+")

# education levels for 2024 (Q46)
edu_levels_24 <- c("No certificate","High school","Apprenticeship/Trades",
                   "College/CEGEP","University < bachelor",
                   "Bachelor","Graduate+")

tbl24_age <- mk_section_counts_24(model24_table1, age_full,  age_lvls_24,  "Age in years")
tbl24_inc <- mk_section_counts_24(model24_table1, income,    inc_levels_24, "Income")
tbl24_edu <- mk_section_counts_24(model24_table1, education, edu_levels_24, "Education")

table1_2024_long <- dplyr::bind_rows(tbl24_age, tbl24_inc, tbl24_edu)

table1_2024_gt <- table1_2024_long %>%
  gt::gt(groupname_col = "Factor") %>%
  gt::tab_header(
    title = "Table 1 (2024). Descriptive statistics of respondents based on socioeconomic and demographic characteristics."
  ) %>%
  gt::fmt_number(
    columns = c(`Sample Size (Male - 1)`, `Sample Size (Female - 2)`, `Sample Size (Other - 3)`),
    decimals = 0
  ) %>%
  gt::tab_source_note(
    source_note = "Table 1 uses complete cases on seven outcomes (Q28_A1–Q28_A7) and four predictors (age, gender, education, income). Age uses the full 2024 eight-bin age scale (16–17, 18–24, 25–34, 35–44, 45–54, 55–64, 65–74, 75+)."
  )

gt::gtsave(table1_2024_gt, "outputs/tables/2024/table1_2024_native.html")
readr::write_csv(table1_2024_long, "outputs/tables/2024/table1_2024_native.csv")
readr::write_csv(cdhs24,          "outputs/tables/2024/cdhs24_after_t1.csv")
readr::write_csv(model24_table1,  "outputs/tables/2024/model_df_2024_table1_complete.csv")

# ============================================================
# Sample sizes for each ordered logit
# ============================================================

n_a1 <- cdhs24 %>%
  tidyr::drop_na(q28_a1_o, age_full, gender, education, income) %>%
  nrow()

n_a2 <- cdhs24 %>%
  tidyr::drop_na(q28_a2_o, age_full, gender, education, income) %>%
  nrow()

n_a3 <- cdhs24 %>%
  tidyr::drop_na(q28_a3_o, age_full, gender, education, income) %>%
  nrow()

n_a4 <- cdhs24 %>%
  tidyr::drop_na(q28_a4_o, age_full, gender, education, income) %>%
  nrow()

n_a5 <- cdhs24 %>%
  tidyr::drop_na(q28_a5_o, age_full, gender, education, income) %>%
  nrow()

n_a6 <- cdhs24 %>%
  tidyr::drop_na(q28_a6_o, age_full, gender, education, income) %>%
  nrow()

n_a7 <- cdhs24 %>%
  tidyr::drop_na(q28_a7_o, age_full, gender, education, income) %>%
  nrow()

# ============================================================
# Appendix Table 1A — Ordinal logistic regression (2024)
# ============================================================

# Safety checks on full recoded data
stopifnot(all(c("q28_a1_o","q28_a2_o","q28_a3_o","q28_a4_o",
                "q28_a5_o","q28_a6_o","q28_a7_o",
                "age_full","gender","education","income") %in% names(cdhs24)))
stopifnot(is.ordered(cdhs24$q28_a1_o), is.factor(cdhs24$age_full))
stopifnot(is.factor(cdhs24$gender), is.factor(cdhs24$education), is.factor(cdhs24$income))

fit_polr_table_24 <- function(df, yvar_chr) {
  # Complete cases for THIS outcome + all predictors
  df_use <- df %>%
    tidyr::drop_na(dplyr::all_of(c(
      yvar_chr,
      "age_full","gender","education","income"
    )))
  
  # Age treatment contrasts.
  # Order all 8 bins; use 18–24 as reference (base = 2).
  age_order <- c("16–17","18–24","25–34","35–44",
                 "45–54","55–64","65–74","75+")
  df_use <- df_use %>%
    dplyr::mutate(
      age_full = factor(age_full, levels = age_order, ordered = FALSE)
    )
  contrasts(df_use$age_full) <- contr.treatment(n = length(age_order), base = 2)
  
  # Build and fit model
  fml <- as.formula(paste0(yvar_chr, " ~ age_full + gender + education + income"))
  mod <- MASS::polr(fml, data = df_use, Hess = TRUE, na.action = na.omit)
  
  # Tidy coefficients
  tt <- broom::tidy(mod, conf.int = TRUE, conf.level = 0.95) %>%
    dplyr::filter(!grepl("\\|", term)) %>%  # drop thresholds
    dplyr::mutate(
      z       = estimate / std.error,
      p.value = 2 * pnorm(-abs(z)),
      OR      = exp(estimate),
      LCL     = exp(conf.low),
      UCL     = exp(conf.high)
    )
  
  # Level sets from fitted data
  age_lvls_fit    <- levels(df_use$age_full)
  gender_lvls_fit <- levels(df_use$gender)
  edu_lvls_fit    <- levels(df_use$education)
  inc_lvls_fit    <- levels(df_use$income)
  
  # Maps from term names to category labels
  age_terms <- grep("^age_full", tt$term, value = TRUE)
  age_map   <- stats::setNames(age_lvls_fit[-2], age_terms)  # ref is level 2 (18–24), no coefficient
  
  make_map <- function(prefix, lvls) {
    hits <- grep(paste0("^", prefix), tt$term, value = TRUE)
    raw  <- sub(paste0("^", prefix, "\\s*`?\\(?"), "", hits)
    raw  <- sub("`?\\)?$", "", raw)
    stats::setNames(lvls[match(raw, lvls, nomatch = NA_integer_)], hits)
  }
  
  gender_map <- make_map("gender",    gender_lvls_fit)
  edu_map    <- make_map("education", edu_lvls_fit)
  inc_map    <- make_map("income",    inc_lvls_fit)
  
  Factor <- ifelse(tt$term %in% names(age_map),    "Age",
                   ifelse(tt$term %in% names(gender_map), "Gender",
                          ifelse(tt$term %in% names(edu_map),    "Education",
                                 ifelse(tt$term %in% names(inc_map),    "Income", NA_character_))))
  
  Category <- tt$term
  Category[tt$term %in% names(age_map)]    <- unname(age_map[tt$term[tt$term %in% names(age_map)]])
  Category[tt$term %in% names(gender_map)] <- unname(gender_map[tt$term[tt$term %in% names(gender_map)]])
  Category[tt$term %in% names(edu_map)]    <- unname(edu_map[tt$term[tt$term %in% names(edu_map)]])
  Category[tt$term %in% names(inc_map)]    <- unname(inc_map[tt$term[tt$term %in% names(inc_map)]])
  
  split <- data.frame(Factor = Factor, Category = Category, stringsAsFactors = FALSE)
  
  # Outcome labels
  out_label <- dplyr::case_when(
    yvar_chr == "q28_a1_o" ~ "Q28_A1",
    yvar_chr == "q28_a2_o" ~ "Q28_A2",
    yvar_chr == "q28_a3_o" ~ "Q28_A3",
    yvar_chr == "q28_a4_o" ~ "Q28_A4",
    yvar_chr == "q28_a5_o" ~ "Q28_A5",
    yvar_chr == "q28_a6_o" ~ "Q28_A6",
    yvar_chr == "q28_a7_o" ~ "Q28_A7",
    TRUE ~ yvar_chr
  )
  
  # Reference rows: Age 18–24, Gender male, Income <$40k, Education No certificate
  ref_rows <- tibble::tibble(
    Factor   = c("Age","Gender","Income","Education"),
    Category = c("18–24","male","<$40k","No certificate"),
    OR = 1, LCL = NA_real_, UCL = NA_real_, p.value = NA_real_
  )
  
  # Orders for sorting
  gen_order <- c("male","female","other")
  inc_order <- inc_levels_24
  edu_order <- edu_levels_24
  
  res <- dplyr::bind_cols(split, tt %>% dplyr::select(OR, LCL, UCL, p.value)) %>%
    dplyr::bind_rows(ref_rows) %>%
    dplyr::mutate(
      Factor_f = factor(Factor, levels = c("Age","Gender","Income","Education")),
      Cat_f = dplyr::case_when(
        Factor == "Age"       ~ factor(Category, levels = age_order),
        Factor == "Gender"    ~ factor(Category, levels = gen_order),
        Factor == "Income"    ~ factor(Category, levels = inc_order),
        Factor == "Education" ~ factor(Category, levels = edu_order),
        TRUE ~ factor(Category)
      )
    ) %>%
    dplyr::arrange(Factor_f, Cat_f) %>%
    dplyr::select(Factor, Category, OR, LCL, UCL, p.value)
  
  # Formatting helpers
  fmt2  <- function(x) formatC(x, format = "f", digits = 2)
  stars <- function(p) dplyr::case_when(
    is.na(p) ~ "",
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  )
  
  res %>%
    dplyr::mutate(
      `Odds Ratio (95% CI)` = ifelse(
        is.na(LCL), "1.00 (ref)", paste0(fmt2(OR), " (", fmt2(LCL), "–", fmt2(UCL), ")")
      ),
      `p-value` = ifelse(is.na(p.value), "", fmt2(p.value)),
      sig       = stars(p.value),
      Outcome   = out_label
    ) %>%
    dplyr::select(Outcome, Factor, Categories = Category,
                  `Odds Ratio (95% CI)`, `p-value`, sig)
}

# Fit seven models using full recoded 2024 data
tbl_a1 <- fit_polr_table_24(cdhs24, "q28_a1_o")
tbl_a2 <- fit_polr_table_24(cdhs24, "q28_a2_o")
tbl_a3 <- fit_polr_table_24(cdhs24, "q28_a3_o")
tbl_a4 <- fit_polr_table_24(cdhs24, "q28_a4_o")
tbl_a5 <- fit_polr_table_24(cdhs24, "q28_a5_o")
tbl_a6 <- fit_polr_table_24(cdhs24, "q28_a6_o")
tbl_a7 <- fit_polr_table_24(cdhs24, "q28_a7_o")

appendix_1A_2024 <- dplyr::bind_rows(tbl_a1, tbl_a2, tbl_a3,
                                     tbl_a4, tbl_a5, tbl_a6, tbl_a7)
readr::write_csv(appendix_1A_2024, "outputs/tables/appendix_table_1A_2024_native_raw.csv")

# ------------------------------------------------------------
# Add Dependent Variable labels with n and build final table
# ------------------------------------------------------------

tbl_a1$`Dependent Variable` <- paste0("Q28_A1 (n = ", n_a1, ")")
tbl_a2$`Dependent Variable` <- paste0("Q28_A2 (n = ", n_a2, ")")
tbl_a3$`Dependent Variable` <- paste0("Q28_A3 (n = ", n_a3, ")")
tbl_a4$`Dependent Variable` <- paste0("Q28_A4 (n = ", n_a4, ")")
tbl_a5$`Dependent Variable` <- paste0("Q28_A5 (n = ", n_a5, ")")
tbl_a6$`Dependent Variable` <- paste0("Q28_A6 (n = ", n_a6, ")")
tbl_a7$`Dependent Variable` <- paste0("Q28_A7 (n = ", n_a7, ")")

appendix_1A_2024_final <- dplyr::bind_rows(tbl_a1, tbl_a2, tbl_a3,
                                           tbl_a4, tbl_a5, tbl_a6, tbl_a7) %>%
  dplyr::select(
    `Dependent Variable`,
    Factor,
    Categories,
    `Odds Ratio (95% CI)`,
    `p-value`
  )

appendix_1A_2024_gt <- appendix_1A_2024_final %>%
  gt::gt(groupname_col = "Dependent Variable") %>%
  gt::tab_header(
    title = md("**Appendix Table 1A (2024 native): Ordinal logistic regression — Odds ratios (95% CI) and p-values**")
  ) %>%
  gt::cols_label(
    Factor                = md("**Factor**"),
    Categories            = md("**Categories**"),
    `Odds Ratio (95% CI)` = md("**Odds Ratio (95% CI)**"),
    `p-value`             = md("**p-value**")
  ) %>%
  gt::fmt_number(columns = "p-value", decimals = 3) %>%
  gt::tab_source_note(
    source_note = md("Notes: Models use complete cases for each outcome (Q28_A1–Q28_A7) and four predictors (age, gender, education, income). Reference categories — Age 18–24; Gender male; Education No certificate; Income <$40k.")
  )

gt::gtsave(appendix_1A_2024_gt, "outputs/tables/2024/appendix_table_1A_2024_native.html")
readr::write_csv(appendix_1A_2024_final, "outputs/tables/2024/appendix_table_1A_2024_native.csv")
readr::write_csv(model24_table1, "outputs/tables/2024/model_df_2024_table1_complete.csv")

# ============================================================
# 2024: polynomial regression + surfaces + Pareto
# ============================================================

if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly", quiet = TRUE)
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets", quiet = TRUE)
library(plotly)
library(htmlwidgets)

convert_to_num <- function(x, levels_vec) {
  x <- factor(x, levels = levels_vec)
  as.numeric(x)
}

plot_question_surface_poly <- function(model, age_levels, gender_levels,
                                       question_tag, wave_tag,
                                       edu_fix_index, inc_fix_index,
                                       save_dir = "outputs/plots/2024/surfaces",
                                       file_type = "html") {
  
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  age_vals    <- seq(1, length(age_levels), length.out = 100)
  gender_vals <- seq(1, length(gender_levels), length.out = 100)
  
  grid <- expand.grid(
    age_num    = age_vals,
    gender_num = gender_vals,
    educ_num   = edu_fix_index,
    income_num = inc_fix_index
  )
  
  grid$predicted <- predict(model, newdata = grid)
  zmat <- matrix(grid$predicted,
                 nrow = length(age_vals),
                 ncol = length(gender_vals),
                 byrow = FALSE)
  
  p <- plot_ly(
    x = gender_vals,
    y = age_vals,
    z = zmat,
    type = "surface"
  ) %>%
    layout(
      title = paste0(wave_tag, " ", question_tag,
                     " – Polynomial regression surface"),
      scene = list(
        xaxis = list(
          title   = "Gender",
          range   = c(length(gender_levels), 1),
          tickmode = "array",
          tickvals = 1:length(gender_levels),
          ticktext = gender_levels
        ),
        yaxis = list(
          title   = "Age",
          tickmode = "array",
          tickvals = 1:length(age_levels),
          ticktext = age_levels
        ),
        zaxis = list(title = "Predicted mean score")
      )
    )
  
  file_path <- file.path(save_dir,
                         paste0(wave_tag, "_", question_tag, ".", file_type))
  
  if (file_type == "html") {
    htmlwidgets::saveWidget(p, file_path, selfcontained = FALSE)
  } else if (file_type == "png") {
    plotly::orca(p, file_path)
  }
  
  p
}

fit_poly_for_outcome_24 <- function(df, yvar, age_var,
                                    age_levels, gender_levels,
                                    edu_levels, inc_levels,
                                    question_tag, wave_tag,
                                    edu_fix_index, inc_fix_index,
                                    surface_dir, pareto_dir,
                                    nested_dir) {
  
  vars_needed <- c(yvar, age_var, "gender", "education", "income")
  df_use <- df %>%
    dplyr::select(dplyr::all_of(vars_needed)) %>%
    tidyr::drop_na()
  
  df_use <- df_use %>%
    dplyr::mutate(
      y_num      = as.numeric(.data[[yvar]]),
      age_num    = convert_to_num(.data[[age_var]], age_levels),
      gender_num = convert_to_num(gender,          gender_levels),
      educ_num   = convert_to_num(education,       edu_levels),
      income_num = convert_to_num(income,          inc_levels)
    )
  
  predictors <- c("age_num", "gender_num", "educ_num", "income_num")
  f_poly <- as.formula(
    paste(
      "y_num ~ (", paste(predictors, collapse = " + "), ")^2 +",
      paste(sprintf("I(%s^2)", predictors), collapse = " + ")
    )
  )
  
  mod <- lm(f_poly, data = df_use)
  
  p_surf <- plot_question_surface_poly(
    model         = mod,
    age_levels    = age_levels,
    gender_levels = gender_levels,
    question_tag  = question_tag,
    wave_tag      = wave_tag,
    edu_fix_index = edu_fix_index,
    inc_fix_index = inc_fix_index,
    save_dir      = surface_dir
  )
  
  dir.create(pareto_dir, recursive = TRUE, showWarnings = FALSE)
  coef_tbl <- broom::tidy(mod) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      importance = abs(estimate),
      term       = forcats::fct_reorder(term, importance)
    ) %>%
    dplyr::arrange(dplyr::desc(importance))
  
  p_par <- ggplot2::ggplot(coef_tbl,
                           ggplot2::aes(x = term, y = importance)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0(wave_tag, " ", question_tag,
                     " – Pareto of |polynomial coefficients|"),
      x = "Term",
      y = "|Coefficient|"
    ) +
    ggplot2::theme_minimal()
  
  pareto_path <- file.path(pareto_dir,
                           paste0(wave_tag, "_", question_tag, "_pareto.png"))
  ggplot2::ggsave(pareto_path, p_par, width = 7, height = 4, dpi = 300)
  
  dir.create(nested_dir, recursive = TRUE, showWarnings = FALSE)
  
  m1 <- lm(y_num ~ age_num, data = df_use)
  m2 <- lm(y_num ~ age_num + gender_num, data = df_use)
  m3 <- lm(y_num ~ age_num + gender_num + educ_num, data = df_use)
  m4 <- lm(y_num ~ age_num + gender_num + educ_num + income_num, data = df_use)
  
  nested_tbl <- tibble::tibble(
    Outcome    = question_tag,
    Model      = c("Model 1", "Model 2", "Model 3", "Model 4"),
    Predictors = c("age",
                   "age + gender",
                   "age + gender + education",
                   "age + gender + education + income"),
    R2         = c(summary(m1)$r.squared,
                   summary(m2)$r.squared,
                   summary(m3)$r.squared,
                   summary(m4)$r.squared),
    Adj_R2     = c(summary(m1)$adj.r.squared,
                   summary(m2)$adj.r.squared,
                   summary(m3)$adj.r.squared,
                   summary(m4)$adj.r.squared),
    n          = nrow(df_use)
  )
  
  nested_path <- file.path(
    nested_dir,
    paste0(wave_tag, "_", question_tag, "_nested_models.csv")
  )
  readr::write_csv(nested_tbl, nested_path)
  
  list(model = mod,
       surface = p_surf,
       pareto_data = coef_tbl,
       nested = nested_tbl)
}

# ---------- run polynomial analysis for ALL q28 items in 2024 ----------

age_levels_24    <- levels(cdhs24$age_full)
gender_levels_24 <- levels(cdhs24$gender)
edu_levels_24    <- levels(cdhs24$education)
inc_levels_24    <- levels(cdhs24$income)

surface_dir_24 <- "outputs/plots/2024/surfaces"
pareto_dir_24  <- "outputs/plots/2024/pareto"
nested_dir_24  <- "outputs/plots/2024/nested_models"

# === INCLUDE **ALL SEVEN** OUTCOME VARIABLES ===
outcomes_2024  <- c(
  "q28_a1_o",
  "q28_a2_o",
  "q28_a3_o",
  "q28_a4_o",
  "q28_a5_o",
  "q28_a6_o",
  "q28_a7_o"
)

questions_2024 <- c(
  "Q28_A1",
  "Q28_A2",
  "Q28_A3",
  "Q28_A4",
  "Q28_A5",
  "Q28_A6",
  "Q28_A7"
)

# === Fix reference predictor values (consistent across plots) ===
# Education fixed at "Bachelor" (code 6)
# Income fixed at "$80–99,999" (code 4)
edu_fix_24 <- 6
inc_fix_24 <- 4

models_2024 <- list()

for (i in seq_along(outcomes_2024)) {
  yvar <- outcomes_2024[i]
  qtag <- questions_2024[i]
  
  models_2024[[qtag]] <- fit_poly_for_outcome_24(
    df            = cdhs24,
    yvar          = yvar,
    age_var       = "age_full",
    age_levels    = age_levels_24,
    gender_levels = gender_levels_24,
    edu_levels    = edu_levels_24,
    inc_levels    = inc_levels_24,
    question_tag  = qtag,
    wave_tag      = "2024",
    edu_fix_index = edu_fix_24,
    inc_fix_index = inc_fix_24,
    surface_dir   = surface_dir_24,
    pareto_dir    = pareto_dir_24,
    nested_dir    = nested_dir_24
  )
}

saveRDS(models_2024, "outputs/plots/2024/poly_models_2024.rds")

