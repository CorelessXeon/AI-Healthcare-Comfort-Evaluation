# ============================================================
# MSE609 — CDHS 2023 Extension
# ============================================================

# ---- Load & clean names ----
raw23      <- haven::read_sav("CDHS2023_Dataset_Dataverse-posting.sav")
cdhs23   <- raw23 %>% janitor::clean_names()

# Required variables (2023)
req23 <- c("q31","q32","q33","q34",
           "q1_age_cat","q5_recoded","q48","q40")
miss23 <- setdiff(req23, names(cdhs23))
if (length(miss23)) stop("Missing columns in 2023 SAV: ", paste(miss23, collapse=", "))

# ---- Recode outcomes & predictors -------------------------

# Outcomes: 4-point ordered, 98="Don't know" -> NA
norm_outcome_4 <- function(v, has98 = TRUE){
  x <- suppressWarnings(as.numeric(v))
  if (has98) x[x == 98] <- NA_real_
  x[!(x %in% 1:4)] <- NA_real_
  factor(x, levels = 1:4, ordered = TRUE)
}

cdhs23 <- cdhs23 %>%
  mutate(
    q31_o = norm_outcome_4(q31, has98 = FALSE),  # Knowledge of AI (no 98 code)
    q32_o = norm_outcome_4(q32, has98 = TRUE),   # Comfort with AI in healthcare
    q33_o = norm_outcome_4(q33, has98 = TRUE),   # Comfort with AI using consented data
    q34_o = norm_outcome_4(q34, has98 = TRUE)    # Comfort with AI using de-identified data
  )

# Age: Q1_AGE_CAT: 1=16–24, 2=25–34, 3=35–54, 4=55–64, 5=65+
cdhs23 <- cdhs23 %>%
  mutate(
    age_code = as.integer(q1_age_cat),
    age5 = case_when(
      age_code == 1L ~ "16–24 years",
      age_code == 2L ~ "25–34 years",
      age_code == 3L ~ "35–54 years",
      age_code == 4L ~ "55–64 years",
      age_code == 5L ~ "65+ years",
      TRUE ~ NA_character_
    ),
    age5 = factor(
      age5,
      levels = c("16–24 years","25–34 years","35–54 years","55–64 years","65+ years"),
      ordered = TRUE
    )
  )

# Gender: Q5_recoded: 1=F, 2=M, 3=diverse, 99=PNTA -> NA
cdhs23 <- cdhs23 %>%
  mutate(
    g_code = as.integer(q5_recoded),
    g_code = if_else(g_code == 99L, NA_integer_, g_code),
    gender = case_when(
      g_code == 2L ~ "male",
      g_code == 1L ~ "female",
      g_code == 3L ~ "other",
      TRUE ~ NA_character_
    ),
    gender = forcats::fct_relevel(factor(gender), "male","female","other")
  )

# Education: Q48: 1–6 valid, 7=PNTA -> NA
edu_levels_23 <- c("No certificate","High school","Apprenticeship/Trades",
                   "College/CEGEP","University < bachelor","University ≥ bachelor")

cdhs23 <- cdhs23 %>%
  mutate(
    edu_code = as.integer(q48),
    edu_code = if_else(edu_code == 7L, NA_integer_, edu_code),
    education = case_when(
      edu_code == 1L ~ "No certificate",
      edu_code == 2L ~ "High school",
      edu_code == 3L ~ "Apprenticeship/Trades",
      edu_code == 4L ~ "College/CEGEP",
      edu_code == 5L ~ "University < bachelor",
      edu_code == 6L ~ "University ≥ bachelor",
      TRUE ~ NA_character_
    ),
    education = factor(education, levels = edu_levels_23)
  )

# Income: Q40: 1–8 valid, 9=DK, 10=PNTA -> NA
inc_levels_23 <- c("<$50k","$50–60k","$60–70k","$70–80k",
                   "$80–90k","$90–100k","$100–150k","$150k+")

cdhs23 <- cdhs23 %>%
  mutate(
    inc_code = as.integer(q40),
    inc_code = if_else(inc_code %in% c(9L,10L), NA_integer_, inc_code),
    income = case_when(
      inc_code == 1L ~ "<$50k",
      inc_code == 2L ~ "$50–60k",
      inc_code == 3L ~ "$60–70k",
      inc_code == 4L ~ "$70–80k",
      inc_code == 5L ~ "$80–90k",
      inc_code == 6L ~ "$90–100k",
      inc_code == 7L ~ "$100–150k",
      inc_code == 8L ~ "$150k+",
      TRUE ~ NA_character_
    ),
    income = factor(income, levels = inc_levels_23)
  )

# ---- dataset for TABLE 1 ----

predictor_vars_23 <- c("age5","gender","education","income")
outcome_vars_23   <- c("q31_o","q32_o","q33_o","q34_o")

# keep only respondents who answered ALL predictors AND ALL outcomes
model23_table1 <- cdhs23 %>%
  dplyr::select(all_of(outcome_vars_23), all_of(predictor_vars_23)) %>%
  tidyr::drop_na(all_of(predictor_vars_23),
                 all_of(outcome_vars_23))

# ============================================================
# TABLE 1 — Descriptive statistics 2023
# ============================================================

mk_section_counts_23 <- function(df, factor_var, factor_levels, section_title) {
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

# age levels for 2023 (Q1_AGE_CAT)
age_lvls_23 <- c("16–24 years","25–34 years","35–54 years","55–64 years","65+ years")

# income levels for 2023 (Q40)
inc_levels_23 <- c("<$50k","$50–60k","$60–70k","$70–80k",
                   "$80–90k","$90–100k","$100–150k","$150k+")

# education levels for 2023 (Q48)
edu_levels_23 <- c("No certificate","High school","Apprenticeship/Trades",
                   "College/CEGEP","University < bachelor","University ≥ bachelor")

tbl23_age <- mk_section_counts_23(model23_table1, age5,      age_lvls_23,  "Age in years")
tbl23_inc <- mk_section_counts_23(model23_table1, income,    inc_levels_23, "Income")
tbl23_edu <- mk_section_counts_23(model23_table1, education, edu_levels_23, "Education")

table1_2023_long <- dplyr::bind_rows(tbl23_age, tbl23_inc, tbl23_edu)

table1_2023_gt <- table1_2023_long %>%
  gt::gt(groupname_col = "Factor") %>%
  gt::tab_header(
    title = "Table 1 (2023). Descriptive statistics of respondents based on socioeconomic and demographic characteristics."
  ) %>%
  gt::fmt_number(
    columns = c(`Sample Size (Male - 1)`, `Sample Size (Female - 2)`, `Sample Size (Other - 3)`),
    decimals = 0
  ) %>%
  gt::tab_source_note(
    source_note = "Table 1 uses complete cases on four outcomes (Q31–Q34) and four predictors (age, gender, education, income). Age uses the native 2023 bins (16–24, 25–34, 35–54, 55–64, 65+)."
  )

gt::gtsave(table1_2023_gt, "outputs/tables/2023/table1_2023_native.html")
readr::write_csv(table1_2023_long, "outputs/tables/2023/table1_2023_native.csv")
readr::write_csv(cdhs23,          "outputs/tables/2023/cdhs23_after_t1.csv")
readr::write_csv(model23_table1,  "outputs/tables/2023/model_df_2023_table1_complete.csv")

# ============================================================
# Sample sizes for each ordered logit
# ============================================================

n_q31 <- cdhs23 %>%
  tidyr::drop_na(q31_o, age5, gender, education, income) %>%
  nrow()

n_q32 <- cdhs23 %>%
  tidyr::drop_na(q32_o, age5, gender, education, income) %>%
  nrow()

n_q33 <- cdhs23 %>%
  tidyr::drop_na(q33_o, age5, gender, education, income) %>%
  nrow()

n_q34 <- cdhs23 %>%
  tidyr::drop_na(q34_o, age5, gender, education, income) %>%
  nrow()

# ============================================================
# Appendix Table 1A — Ordinal logistic regression (2023)
# ============================================================

# Safety checks on full recoded data
stopifnot(all(c("q31_o","q32_o","q33_o","q34_o",
                "age5","gender","education","income") %in% names(cdhs23)))
stopifnot(is.ordered(cdhs23$q31_o), is.factor(cdhs23$age5))
stopifnot(is.factor(cdhs23$gender), is.factor(cdhs23$education), is.factor(cdhs23$income))

fit_polr_table_23 <- function(df, yvar_chr) {
  # Complete cases for THIS outcome + all predictors
  df_use <- df %>%
    tidyr::drop_na(dplyr::all_of(c(
      yvar_chr,
      "age5","gender","education","income"
    )))
  
  # Treatment contrasts for age, baseline 16–24 years
  age_order <- c("16–24 years","25–34 years","35–54 years","55–64 years","65+ years")
  df_use <- df_use %>%
    dplyr::mutate(
      age5 = factor(age5, levels = age_order, ordered = FALSE)
    )
  contrasts(df_use$age5) <- contr.treatment(n = length(age_order), base = 1)
  
  # Build and fit model
  fml <- as.formula(paste0(yvar_chr, " ~ age5 + gender + education + income"))
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
  
  # Level sets from the fitted data
  age_lvls_fit    <- levels(df_use$age5)
  gender_lvls_fit <- levels(df_use$gender)
  edu_lvls_fit    <- levels(df_use$education)
  inc_lvls_fit    <- levels(df_use$income)
  
  # Maps from term names to category labels
  age_terms <- grep("^age5", tt$term, value = TRUE)
  age_map   <- stats::setNames(age_lvls_fit[-1], age_terms)  # ref is first
  
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
  
  # Outcome label text
  out_label <- dplyr::case_when(
    yvar_chr == "q31_o" ~ "Knowledge of AI (Q31)",
    yvar_chr == "q32_o" ~ "Comfort with AI in healthcare (Q32)",
    yvar_chr == "q33_o" ~ "Comfort with AI using consented data (Q33)",
    yvar_chr == "q34_o" ~ "Comfort with AI using de-identified data (Q34)",
    TRUE ~ yvar_chr
  )
  
  # Reference rows
  ref_rows <- tibble::tibble(
    Factor   = c("Age","Gender","Income","Education"),
    Category = c("16–24 years","male","<$50k","No certificate"),
    OR = 1, LCL = NA_real_, UCL = NA_real_, p.value = NA_real_
  )
  
  # Orders for sorting
  gen_order <- c("male","female","other")
  inc_order <- inc_levels_23
  edu_order <- edu_levels_23
  
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

# Fit all four models using full recoded 2023 data
tbl_q31 <- fit_polr_table_23(cdhs23, "q31_o")
tbl_q32 <- fit_polr_table_23(cdhs23, "q32_o")
tbl_q33 <- fit_polr_table_23(cdhs23, "q33_o")
tbl_q34 <- fit_polr_table_23(cdhs23, "q34_o")

appendix_1A_2023 <- dplyr::bind_rows(tbl_q31, tbl_q32, tbl_q33, tbl_q34)
readr::write_csv(appendix_1A_2023, "outputs/tables/2023/appendix_table_1A_2023_native_raw.csv")

# ------------------------------------------------------------
# Add Dependent Variable labels with n and build final table
# ------------------------------------------------------------

tbl_q31$`Dependent Variable` <-
  paste0("Q.31 – How knowledgeable are you about artificial intelligence? (n = ", n_q31, ")")

tbl_q32$`Dependent Variable` <-
  paste0("Q.32 – How comfortable are you with AI being used as a tool in healthcare? (n = ", n_q32, ")")

tbl_q33$`Dependent Variable` <-
  paste0("Q.33 – How comfortable are you with AI using consented data? (n = ", n_q33, ")")

tbl_q34$`Dependent Variable` <-
  paste0("Q.34 – How comfortable are you with AI using de-identified data? (n = ", n_q34, ")")

appendix_1A_2023_final <- dplyr::bind_rows(tbl_q31, tbl_q32, tbl_q33, tbl_q34) %>%
  dplyr::select(
    `Dependent Variable`,
    Factor,
    Categories,
    `Odds Ratio (95% CI)`,
    `p-value`
  )

appendix_1A_2023_gt <- appendix_1A_2023_final %>%
  gt::gt(groupname_col = "Dependent Variable") %>%
  gt::tab_header(
    title = md("**Appendix Table 1A (2023 native): Ordinal logistic regression — Odds ratios (95% CI) and p-values**")
  ) %>%
  gt::cols_label(
    Factor                = md("**Factor**"),
    Categories            = md("**Categories**"),
    `Odds Ratio (95% CI)` = md("**Odds Ratio (95% CI)**"),
    `p-value`             = md("**p-value**")
  ) %>%
  gt::fmt_number(columns = "p-value", decimals = 3) %>%
  gt::tab_source_note(
    source_note = md("Notes: Models use complete cases for each outcome and four predictors (age, gender, education, income). Reference categories — Age 16–24 years; Gender male; Education No certificate; Income <$50k.")
  )

gt::gtsave(appendix_1A_2023_gt, "outputs/tables/2023/appendix_table_1A_2023_native.html")
readr::write_csv(appendix_1A_2023_final, "outputs/tables/2023/appendix_table_1A_2023_native.csv")

# ============================================================
# 2023: polynomial regression + surfaces + Pareto
# ============================================================

if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly", quiet = TRUE)
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets", quiet = TRUE)
library(plotly)
library(htmlwidgets)

# helper reused (redefine; environment is cleaned between scripts)
convert_to_num <- function(x, levels_vec) {
  x <- factor(x, levels = levels_vec)
  as.numeric(x)
}

plot_question_surface_poly <- function(model, age_levels, gender_levels,
                                       question_tag, wave_tag,
                                       edu_fix_index, inc_fix_index,
                                       save_dir = "outputs/plots/2023/surfaces",
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

fit_poly_for_outcome_23 <- function(df, yvar, age_var,
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

# ---------- run polynomial analysis ----------

age_levels_23    <- levels(cdhs23$age5)
gender_levels_23 <- levels(cdhs23$gender)
edu_levels_23    <- levels(cdhs23$education)
inc_levels_23    <- levels(cdhs23$income)

surface_dir_23 <- "outputs/plots/2023/surfaces"
pareto_dir_23  <- "outputs/plots/2023/pareto"
nested_dir_23  <- "outputs/plots/2023/nested_models"

outcomes_2023  <- c("q31_o","q32_o","q33_o","q34_o")
questions_2023 <- c("Q31","Q32","Q33","Q34")

# Fix education at "University ≥ bachelor" (code 6), income at "$80–90k" (code 5)
edu_fix_23 <- 6
inc_fix_23 <- 5

models_2023 <- list()
for (i in seq_along(outcomes_2023)) {
  yvar <- outcomes_2023[i]
  qtag <- questions_2023[i]
  
  models_2023[[qtag]] <- fit_poly_for_outcome_23(
    df            = cdhs23,
    yvar          = yvar,
    age_var       = "age5",
    age_levels    = age_levels_23,
    gender_levels = gender_levels_23,
    edu_levels    = edu_levels_23,
    inc_levels    = inc_levels_23,
    question_tag  = qtag,
    wave_tag      = "2023",
    edu_fix_index = edu_fix_23,
    inc_fix_index = inc_fix_23,
    surface_dir   = surface_dir_23,
    pareto_dir    = pareto_dir_23,
    nested_dir    = nested_dir_23
  )
}

saveRDS(models_2023, "outputs/plots/2023/poly_models_2023.rds")

