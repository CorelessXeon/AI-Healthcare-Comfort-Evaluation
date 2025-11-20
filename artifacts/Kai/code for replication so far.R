# ============================================================
# MSE609 — CDHS 2021 Replication (Table 1 & Appendix Table 1A)
# Source: ATS2021 Dataset_Dataverse posting.sav
#
# Age uses YOUR five bins (from this export):
#   1=16–24, 2=25–34, 3=35–44, 4=45–54, 5=55+
# Article used: 16–24, 25–34, 35–54, 55–64, 65+ (expect mild differences).
#
# Paper exclusions:
# - Outcomes: drop “Don’t know” for Q41, Q42, Q43 (set to NA -> excluded by polr).
# - Predictors: drop “Prefer not to answer” for income (Q55);
#               drop “Other”, “None of the above”, “Prefer not to answer” for education (Q66).
# ============================================================

# ---- 0) Install & load packages ----
needed <- c("dplyr","tidyr","tidyverse","haven","janitor","forcats","gt","MASS","broom","stringr","purrr","here","plotly")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(needed, library, character.only = TRUE))

dir.create("artifacts/tables", recursive = TRUE, showWarnings = FALSE)

# ---- 1) Paths ----
sav_path <- here("data_raw", "ATS2021 Dataset_Dataverse posting.sav")
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


# ---- 4) Modeling dataset (apply exclusions by dropping NA in predictors) ----
model_df <- cdhs %>%
  dplyr::select(q40_o, q41_o, q42_o, q43_o, age5, gender, education, income) %>%
  tidyr::drop_na(age5, gender, education, income)
# Note: outcomes with NA (due to DK in Q41–Q43) are removed automatically by polr()

# ============================================================
# 5) TABLE 1 — Descriptive statistics (by gender)
# ============================================================
# --- Replace mk_section_counts() with this robust version ---------------------
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

# --- And replace the three calls that build the sections ----------------------

# Age (uses your 5-bin export labels in *this* order)
age_lvls <- c("16–24 years","25–34 years","35–44 years","45–54 years","55+ years")

# Income (ascending as in the article; codes 1..7 will follow this order)
inc_levels <- c("< $24,999","$25,000-$49,999","$50,000-$79,999",
                "$80,000-$99,000","$100,000-$149,999",
                "$150,000-$249,999","$250,000+")

# Education (exact article order; codes 1..7 will follow this order)
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

gt::gtsave(table1_gt, "artifacts/tables/table1_replication.html")
readr::write_csv(table1_long, "artifacts/tables/table1_replication.csv")
readr::write_csv(cdhs, "cdhs_aft_t1.csv")
readr::write_csv(model_df, "model_df.csv")

# ============================================================
# Appendix Table 1A — Ordinal logistic regression (replication)
# ============================================================

# Safety: check the columns we need exist and look like factors
stopifnot(all(c("q40_o","q41_o","q42_o","q43_o","age5","gender","education","income") %in% names(model_df)))
stopifnot(is.ordered(model_df$q40_o), is.factor(model_df$age5))
stopifnot(is.factor(model_df$gender), is.factor(model_df$education), is.factor(model_df$income))

# Helper to fit one ordered logit (MASS::polr), tidy to OR/CI/p, and
# reshape into the paper’s sectioned layout with reference rows.

# ---------- PATCH: fit_polr_table with treatment contrasts for age ----------
fit_polr_model <- function(df, yvar_chr) {
  # Drop rows where THIS outcome is NA (per paper)
  df_use <- df %>% tidyr::drop_na(dplyr::all_of(yvar_chr))
  
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
  
  # ---- Robust term → (Factor, Category) mapping (replaces old parse_term block) ----
  # Use the actual factor levels from the data used in the model so we never
  # guess the age dummy names (which vary across platforms).
  
  age_lvls_fit    <- levels(df_use$age5)
  gender_lvls_fit <- levels(df_use$gender)
  edu_lvls_fit    <- levels(df_use$education)
  inc_lvls_fit    <- levels(df_use$income)
  
  # Age: build a direct map from the *actual* dummy term names to the age labels
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
  
  out_table <- res %>%
    dplyr::mutate(
      `Odds Ratio (95% CI)` = ifelse(
        is.na(LCL), "1.00 (ref)", paste0(fmt2(OR), " (", fmt2(LCL), "–", fmt2(UCL), ")")
      ),
      `p-value` = ifelse(is.na(p.value), "", fmt2(p.value)),
      sig       = stars(p.value),
      Outcome   = out_label
    ) %>%
    dplyr::select(Outcome, Factor, Categories = Category, `Odds Ratio (95% CI)`, `p-value`, sig)

  list(
    model = mod,
    table = out_table
  )
}
# ---------- end PATCH ----------

# Fit all four (unchanged calls)
q40 <- fit_polr_table(model_df, "q40_o")
q41 <- fit_polr_table(model_df, "q41_o")
q42 <- fit_polr_table(model_df, "q42_o")
q43 <- fit_polr_table(model_df, "q43_o")
tbl_q40 <- q40$table
tbl_q41 <- q41$table
tbl_q42 <- q42$table
tbl_q43 <- q43$table
mod_q40 <- q40$model
mod_q41 <- q41$model
mod_q42 <- q42$model
mod_q43 <- q43$model
dir.create("artifacts/models", recursive = TRUE, showWarnings = FALSE)
saveRDS(mod_q40, "artifacts/models/q40_olr_model.rds")
saveRDS(mod_q41, "artifacts/models/q41_olr_model.rds")
saveRDS(mod_q42, "artifacts/models/q42_olr_model.rds")
saveRDS(mod_q43, "artifacts/models/q43_olr_model.rds")
appendix_1A <- dplyr::bind_rows(tbl_q40, tbl_q41, tbl_q42, tbl_q43)

# Keep the CSV as before
readr::write_csv(appendix_1A, "artifacts/tables/appendix_table_1A_replication.csv")

# ============================================================
# Combine the four precomputed result tables into one report
# ============================================================

# Add a Dependent Variable label column to each block
tbl_q40$`Dependent Variable` <- "Q.40 – How knowledgeable are you about what artificial intelligence is?"
tbl_q41$`Dependent Variable` <- "Q.41 – How comfortable are you with AI being used as a tool in healthcare?"
tbl_q42$`Dependent Variable` <- "Q.42 – How comfortable are you with AI using consented data?"
tbl_q43$`Dependent Variable` <- "Q.43 – How comfortable are you with AI using de-identified data?"

# Stack them in order
appendix_1A_final <- dplyr::bind_rows(tbl_q40, tbl_q41, tbl_q42, tbl_q43) %>%
  dplyr::select(
    `Dependent Variable`,
    Factor,
    Categories,
    `Odds Ratio (95% CI)`,
    `p-value`
  )

# Create the grouped HTML table (no re-parsing, just grouping by Dependent Variable)
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
gt::gtsave(appendix_1A_gt, "artifacts/tables/appendix_table_1A_replication.html")
readr::write_csv(appendix_1A_final, "artifacts/tables/appendix_table_1A_replication.csv")
readr::write_csv(model_df, "data_clean/model_df.csv")
