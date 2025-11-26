# ============================================================
# MSE609 — CDHS 2021 Replication
# ============================================================

# ---- 0) Install & load packages ----
needed <- c("tidyverse","haven","janitor","forcats","gt","MASS","broom","stringr","purrr")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(needed, library, character.only = TRUE))

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ---- 1) Paths ----
sav_path <- "ATS2021 Dataset_Dataverse posting.sav"
stopifnot(file.exists(sav_path))

# ---- 2) Load SAV & lock variable names (confirmed from CSV) ----
raw  <- haven::read_sav(sav_path)
cdhs <- raw %>% janitor::clean_names()

req <- c("q40","q41","q42","q43","age_new","gender","q66","q55")
miss <- setdiff(req, names(cdhs))
if (length(miss)) stop("Missing columns in SAV: ", paste(miss, collapse = ", "))

# ---- 3) Recode per paper (numeric-code driven; robust) ----

# (a) Outcomes as ordered factors 1<2<3<4; Q41–Q43: 98="Don't know" -> NA
norm_outcome <- function(v, drop98 = FALSE){
  x <- suppressWarnings(as.numeric(v))
  if (drop98) x[x == 98] <- NA_real_
  x[!(x %in% 1:4)] <- NA_real_
  factor(x, levels = 1:4, ordered = TRUE)
}
cdhs <- cdhs %>%
  mutate(
    q40_o = norm_outcome(q40, drop98 = FALSE),
    q41_o = norm_outcome(q41, drop98 = TRUE),
    q42_o = norm_outcome(q42, drop98 = TRUE),
    q43_o = norm_outcome(q43, drop98 = TRUE)
  )

# (b) Age: keep your five bins (labels for readability)
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
    age5 = factor(age5,
                  levels = c("16–24 years","25–34 years","35–44 years","45–54 years","55+ years"),
                  ordered = TRUE)
  )

# (c) Gender: 1=male, 2=female, 3=other
cdhs <- cdhs %>%
  mutate(
    gender = case_when(
      as.integer(gender) == 1 ~ "male",
      as.integer(gender) == 2 ~ "female",
      as.integer(gender) == 3 ~ "other",
      TRUE ~ NA_character_
    ),
    gender = forcats::fct_relevel(factor(gender), "male","female","other")
  )

# (d) Education (Q66): keep 1..7; drop 96/97/99 (Other/PNTA/DK)
edu_levels <- c("Highschool","Apprenticeship/Trades","College/CEGEP",
                "University degree","Masters","PhD","Medical/paramedical")
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
    education = forcats::fct_relevel(factor(education), edu_levels)
  )

# (e) Income (Q55): keep 1..7; drop 99 (PNTA/DK)
inc_levels <- c("< $24,999","$25,000-$49,999","$50,000-$79,999",
                "$80,000-$99,000","$100,000-$149,999",
                "$150,000-$249,999","$250,000+")
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
    income = forcats::fct_relevel(factor(income), inc_levels)
  )


# ---- 4) Dataset for TABLE 1 (apply exclusions to predictors + ALL outcomes) ----

# predictors and outcomes
predictor_vars_21 <- c("age5","gender","education","income")
outcome_vars_21   <- c("q40_o","q41_o","q42_o","q43_o")

# For TABLE 1: keep only respondents who answered ALL predictors AND ALL outcomes
model_df <- cdhs %>%
  dplyr::select(all_of(outcome_vars_21), all_of(predictor_vars_21)) %>%
  tidyr::drop_na(all_of(predictor_vars_21),
                 all_of(outcome_vars_21))

# ============================================================
# 5) TABLE 1
# ============================================================

mk_section_counts <- function(df, factor_var, factor_levels, section_title) {
  # Ensure factor & gender have the right levels/order
  fac <- factor(dplyr::pull(df, {{ factor_var }}), levels = factor_levels)
  gen <- factor(df$gender, levels = c("male","female","other"))
  
  # 2D frequency table (no dropping of empty combos)
  tab <- as.matrix(stats::xtabs(~ fac + gen, drop.unused.levels = FALSE))
  
  # Guarantee we have all three gender columns
  if (!"male"   %in% colnames(tab)) tab <- cbind(tab, male   = 0)
  if (!"female" %in% colnames(tab)) tab <- cbind(tab, female = 0)
  if (!"other"  %in% colnames(tab)) tab <- cbind(tab, other  = 0)
  
  # Reorder rows to match factor_levels exactly
  tab <- tab[factor_levels, c("male","female","other"), drop = FALSE]
  
  tibble::tibble(
    Factor = section_title,
    `Age Category (Code)` =
      paste0(factor_levels, " (", seq_along(factor_levels), ")"),
    `Sample Size (Male - 1)`   = as.integer(tab[, "male"]),
    `Sample Size (Female - 2)` = as.integer(tab[, "female"]),
    `Sample Size (Other - 3)`  = as.integer(tab[, "other"])
  )
}

# Age
age_lvls <- c("16–24 years","25–34 years","35–44 years","45–54 years","55+ years")

# Income
inc_levels <- c("< $24,999","$25,000-$49,999","$50,000-$79,999",
                "$80,000-$99,000","$100,000-$149,999",
                "$150,000-$249,999","$250,000+")

# Education
edu_levels <- c("Highschool","Apprenticeship/Trades","College/CEGEP",
                "University degree","Masters","PhD","Medical/paramedical")

tbl_age <- mk_section_counts(model_df, age5,      age_lvls,  "Age in years")
tbl_inc <- mk_section_counts(model_df, income,    inc_levels, "Income")
tbl_edu <- mk_section_counts(model_df, education, edu_levels, "Education")

table1_long <- dplyr::bind_rows(tbl_age, tbl_inc, tbl_edu)

table1_gt <- table1_long %>%
  gt::gt(groupname_col = "Factor") %>%
  gt::tab_header(
    title = "Table 1. Descriptive statistics of respondents based on socioeconomic and demographic characteristics."
  ) %>%
  gt::fmt_number(
    columns = c(`Sample Size (Male - 1)`, `Sample Size (Female - 2)`, `Sample Size (Other - 3)`),
    decimals = 0
  ) %>%
  gt::tab_source_note(
    source_note = "Age uses the five bins present in this export (16–24, 25–34, 35–44, 45–54, 55+). The article used 16–24, 25–34, 35–54, 55–64, 65+."
  )

gt::gtsave(table1_gt, "outputs/tables/table1_replication.html")
readr::write_csv(table1_long, "outputs/tables/table1_replication.csv")
readr::write_csv(cdhs, "cdhs_aft_t1.csv")
readr::write_csv(model_df, "model_df.csv")

# ============================================================
# Sample sizes for each ordered logit
# ============================================================
n_q40 <- cdhs %>%
  tidyr::drop_na(q40_o, age5, gender, education, income) %>%
  nrow()

n_q41 <- cdhs %>%
  tidyr::drop_na(q41_o, age5, gender, education, income) %>%
  nrow()

n_q42 <- cdhs %>%
  tidyr::drop_na(q42_o, age5, gender, education, income) %>%
  nrow()

n_q43 <- cdhs %>%
  tidyr::drop_na(q43_o, age5, gender, education, income) %>%
  nrow()

# ============================================================
# Appendix Table 1A — Ordinal logistic regression (replication)
# ============================================================

# Safety: check the columns we need exist and look like factors (on full recoded data)
stopifnot(all(c("q40_o","q41_o","q42_o","q43_o","age5","gender","education","income") %in% names(cdhs)))
stopifnot(is.ordered(cdhs$q40_o), is.factor(cdhs$age5))
stopifnot(is.factor(cdhs$gender), is.factor(cdhs$education), is.factor(cdhs$income))


# ---------- PATCH: fit_polr_table with treatment contrasts for age ----------
fit_polr_table <- function(df, yvar_chr) {
  # Drop rows where THIS outcome OR ANY predictor is NA (per your rule)
  df_use <- df %>%
    tidyr::drop_na(dplyr::all_of(c(
      yvar_chr,
      "age5","gender","education","income"
    )))
  
  # Ensure treatment (dummy) contrasts for age, with 16–24 as baseline
  age_order <- c("16–24 years","25–34 years","35–44 years","45–54 years","55+ years")
  df_use <- df_use %>%
    dplyr::mutate(
      # use an *unordered* factor here to avoid contr.poly
      age5 = factor(age5, levels = age_order, ordered = FALSE)
    )
  contrasts(df_use$age5) <- contr.treatment(n = length(age_order), base = 1)
  
  # Build and fit
  fml <- as.formula(paste0(yvar_chr, " ~ age5 + gender + education + income"))
  mod <- MASS::polr(fml, data = df_use, Hess = TRUE, na.action = na.omit)
  
  # Tidy (drop thresholds), add Wald p, OR and CI on OR scale
  tt <- broom::tidy(mod, conf.int = TRUE, conf.level = 0.95) %>%
    dplyr::filter(!grepl("\\|", term)) %>%
    dplyr::mutate(
      z       = estimate / std.error,
      p.value = 2 * pnorm(-abs(z)),
      OR      = exp(estimate),
      LCL     = exp(conf.low),
      UCL     = exp(conf.high)
    )
  
  # Use the actual factor levels from the data used in the model
  
  age_lvls_fit    <- levels(df_use$age5)
  gender_lvls_fit <- levels(df_use$gender)
  edu_lvls_fit    <- levels(df_use$education)
  inc_lvls_fit    <- levels(df_use$income)
  
  # Age
  age_terms <- grep("^age5", tt$term, value = TRUE)          # the exact dummy names in your system
  age_map   <- stats::setNames(age_lvls_fit[-1], age_terms)  # reference (level 1) has no coefficient
  
  # Generic mapper for the other factors
  make_map <- function(prefix, lvls) {
    hits <- grep(paste0("^", prefix), tt$term, value = TRUE)
    # Remove the prefix and any backticks/parentheses R might add around level names
    raw <- sub(paste0("^", prefix, "\\s*`?\\(?"), "", hits)
    raw <- sub("`?\\)?$", "", raw)
    stats::setNames(lvls[match(raw, lvls, nomatch = NA_integer_)], hits)
  }
  
  gender_map <- make_map("gender",    gender_lvls_fit)
  edu_map    <- make_map("education", edu_lvls_fit)
  inc_map    <- make_map("income",    inc_lvls_fit)
  
  # Assign Factor + Category using the maps (anything unmapped stays NA for inspection)
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
  
  
  # Outcome label
  out_label <- dplyr::case_when(
    yvar_chr == "q40_o" ~ "Knowledge of AI (Q40)",
    yvar_chr == "q41_o" ~ "Comfort with AI (Q41)",
    yvar_chr == "q42_o" ~ "Comfort with AI using consented data (Q42)",
    yvar_chr == "q43_o" ~ "Comfort with AI using de-identified data (Q43)",
    TRUE ~ yvar_chr
  )
  
  # Reference rows
  ref_rows <- tibble::tibble(
    Factor   = c("Age","Gender","Income","Education"),
    Category = c("16–24 years","male","< $24,999","Highschool"),
    OR = 1, LCL = NA_real_, UCL = NA_real_, p.value = NA_real_
  )
  
  # Orders
  gen_order <- c("male","female","other")
  inc_order <- c("< $24,999","$25,000-$49,999","$50,000-$79,999",
                 "$80,000-$99,000","$100,000-$149,999","$150,000-$249,999","$250,000+")
  edu_order <- c("Highschool","Apprenticeship/Trades","College/CEGEP",
                 "University degree","Masters","PhD","Medical/paramedical")
  
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
  
  # Formatting
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
    dplyr::select(Outcome, Factor, Categories = Category, `Odds Ratio (95% CI)`, `p-value`, sig)
}
# --------------------

# Fit all four using full recoded data; fit_polr_table drops
# to complete cases for that outcome + predictors
tbl_q40 <- fit_polr_table(cdhs, "q40_o")
tbl_q41 <- fit_polr_table(cdhs, "q41_o")
tbl_q42 <- fit_polr_table(cdhs, "q42_o")
tbl_q43 <- fit_polr_table(cdhs, "q43_o")

appendix_1A <- dplyr::bind_rows(tbl_q40, tbl_q41, tbl_q42, tbl_q43)

readr::write_csv(appendix_1A, "outputs/tables/appendix_table_1A_replication.csv")

# ============================================================
# Combine the four precomputed result tables into one report
# ============================================================

# Add a Dependent Variable label column
tbl_q40$`Dependent Variable` <-
  paste0("Q.40 – How knowledgeable are you about what artificial intelligence is? (n = ", n_q40, ")")

tbl_q41$`Dependent Variable` <-
  paste0("Q.41 – How comfortable are you with AI being used as a tool in healthcare? (n = ", n_q41, ")")

tbl_q42$`Dependent Variable` <-
  paste0("Q.42 – How comfortable are you with AI using consented data? (n = ", n_q42, ")")

tbl_q43$`Dependent Variable` <-
  paste0("Q.43 – How comfortable are you with AI using de-identified data? (n = ", n_q43, ")")


# Stack in order
appendix_1A_final <- dplyr::bind_rows(tbl_q40, tbl_q41, tbl_q42, tbl_q43) %>%
  dplyr::select(
    `Dependent Variable`,
    Factor,
    Categories,
    `Odds Ratio (95% CI)`,
    `p-value`
  )

# Create grouped HTML table 
appendix_1A_gt <- appendix_1A_final %>%
  gt::gt(groupname_col = "Dependent Variable") %>%
  gt::tab_header(
    title = md("**Appendix Table 1A (Replication): Ordinal logistic regression — Odds ratios (95% CI) and p-values**")
  ) %>%
  gt::cols_label(
    Factor                = md("**Factor**"),
    Categories            = md("**Categories**"),
    `Odds Ratio (95% CI)` = md("**Odds Ratio (95% CI)**"),
    `p-value`             = md("**p-value**")
  ) %>%
  gt::fmt_number(columns = "p-value", decimals = 3) %>%
  gt::tab_source_note(
    source_note = md("Notes: Reference categories — Age 16–24; Gender male; Education Highschool; Income < $24,999. Age in this replication uses the five bins present in this CDHS export (16–24, 25–34, 35–44, 45–54, 55+).")
  )

# Save the final HTML report and CSV
gt::gtsave(appendix_1A_gt, "outputs/tables/appendix_table_1A_replication.html")
readr::write_csv(appendix_1A_final, "outputs/tables/appendix_table_1A_replication.csv")
readr::write_csv(model_df, "model_df.csv")


# ============================================================
# 2021: polynomial regression + surfaces + Pareto
# ============================================================
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly", quiet = TRUE)
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets", quiet = TRUE)
library(plotly)
library(htmlwidgets)

# convert factor with known levels to 1,2,3,...
convert_to_num <- function(x, levels_vec) {
  x <- factor(x, levels = levels_vec)
  as.numeric(x)
}

# 3D surface plot for one outcome
plot_question_surface_poly <- function(model, age_levels, gender_levels,
                                       question_tag, wave_tag,
                                       edu_fix_index, inc_fix_index,
                                       save_dir = "outputs/plots/2021/surfaces",
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

# fit polynomial model, make surface + Pareto + nested-R2 table
fit_poly_for_outcome <- function(df, yvar, age_var,
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
  
  # numeric encodings
  df_use <- df_use %>%
    dplyr::mutate(
      y_num      = as.numeric(.data[[yvar]]),
      age_num    = convert_to_num(.data[[age_var]], age_levels),
      gender_num = convert_to_num(gender,          gender_levels),
      educ_num   = convert_to_num(education,       edu_levels),
      income_num = convert_to_num(income,          inc_levels)
    )
  
  # polynomial formula
  predictors <- c("age_num", "gender_num", "educ_num", "income_num")
  f_poly <- as.formula(
    paste(
      "y_num ~ (", paste(predictors, collapse = " + "), ")^2 +",
      paste(sprintf("I(%s^2)", predictors), collapse = " + ")
    )
  )
  
  mod <- lm(f_poly, data = df_use)
  
  # 3D surface
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
  
  # Pareto chart of |coefficients|
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
  
  # Nested linear models like the lecture example
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

age_levels_21    <- levels(cdhs$age5)
gender_levels_21 <- levels(cdhs$gender)
edu_levels_21    <- levels(cdhs$education)
inc_levels_21    <- levels(cdhs$income)

surface_dir_21 <- "outputs/plots/2021/surfaces"
pareto_dir_21  <- "outputs/plots/2021/pareto"
nested_dir_21  <- "outputs/plots/2021/nested_models"

outcomes_2021  <- c("q40_o","q41_o","q42_o","q43_o")
questions_2021 <- c("Q40","Q41","Q42","Q43")

# hold education = "University degree" (code 4), income = "$80,000-$99,000" (code 4)
edu_fix_21 <- 4
inc_fix_21 <- 4

models_2021 <- list()
for (i in seq_along(outcomes_2021)) {
  yvar <- outcomes_2021[i]
  qtag <- questions_2021[i]
  
  models_2021[[qtag]] <- fit_poly_for_outcome(
    df            = cdhs,
    yvar          = yvar,
    age_var       = "age5",
    age_levels    = age_levels_21,
    gender_levels = gender_levels_21,
    edu_levels    = edu_levels_21,
    inc_levels    = inc_levels_21,
    question_tag  = qtag,
    wave_tag      = "2021",
    edu_fix_index = edu_fix_21,
    inc_fix_index = inc_fix_21,
    surface_dir   = surface_dir_21,
    pareto_dir    = pareto_dir_21,
    nested_dir    = nested_dir_21
  )
}

saveRDS(models_2021, "outputs/plots/2021/poly_models_2021.rds")




