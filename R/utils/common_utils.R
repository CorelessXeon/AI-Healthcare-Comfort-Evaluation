# =========================================================
# Utility Functions for Variable Encoding & Surface Plots
# =========================================================
# nolint start
# ---------------------------------------------------------
# 1. LEVEL DEFINITIONS
# ---------------------------------------------------------

lvl_income <- c(
  "< $24,999",
  "$25,000-$49,999",
  "$50,000-$79,999",
  "$80,000-$99,000",
  "$100,000-$149,999",
  "$150,000-$249,999",
  "$250,000+"
)

lvl_education <- c(
  "Highschool",
  "Apprenticeship/Trades",
  "College/CEGEP",
  "University degree",
  "Masters",
  "PhD",
  "Medical/paramedical"
)

lvl_age <- c(
  "16–24 years",
  "25–34 years",
  "35–44 years",
  "45–54 years",
  "55+ years"
)

lvl_gender <- c("male", "female", "other")

# ---------------------------------------------------------
# 2. CONVERT TO ORDERED NUMERIC
# ---------------------------------------------------------
convert_to_num <- function(x, levels) {
  x <- factor(x, levels = levels)
  as.numeric(x)
}


# ---------------------------------------------------------
# 3. PLOT SURFACE FOR POLYNOMIAL REGRESSION
# ---------------------------------------------------------
plot_question_surface_poly <- function(model, df, question_name,
                                       save_dir = "artifacts/plots",
                                       file_type = "html", education_level_fix, income_level_fix, message, gender_tick_vals= 1:3, age_tick_vals= 1:5) {

  # --------------------------------------------
  # 1. Build grid for plotting
  # --------------------------------------------
  # age_vals    <- sort(unique(df$age_num))
  # gender_vals <- sort(unique(df$gender_num))
  # to meke the plot smooth, we use irrealistic values in between integers
  if (length(income_level_fix) > 1) {
    df$income_num[df$income_num %in% income_level_fix] <- income_level_fix[1]
  }
  age_vals    <- seq(1, 5, length.out = 100)
  gender_vals <- seq(1, 3, length.out = 100)
  grid <- expand.grid(
    age_num    = age_vals,
    gender_num = gender_vals,
    educ_num   = education_level_fix,
    income_num = income_level_fix[1]
  )
  # --------------------------------------------
  # 2. Predict fitted values from polynomial regression
  # --------------------------------------------
  grid$predicted <- predict(model, newdata = grid)

  # --------------------------------------------
  # 3. Reshape into matrix for surface plot
  # --------------------------------------------
  zmat <- matrix(grid$predicted,
                 nrow = length(age_vals),
                 ncol = length(gender_vals),
                 byrow = FALSE)

  # --------------------------------------------
  # 4. 3D Plotly Surface
  # --------------------------------------------
  p <- plot_ly(
    x = gender_vals,
    y = age_vals,
    z = zmat,
    type = "surface"
  ) %>%
    layout(
      title = paste(question_name, "- Polynomial Regression Surface"),
      scene = list(
        # flip x-axis by setting range from max to min
        xaxis = list(
          title = "Gender (coded)",
          range = c(3, 1),                # <--- explicit reversed range (max -> min) 
          tickmode = "array",
          tickvals = gender_tick_vals,
          ticktext = levels(df$gender)    # will appear left->right as male, female, other
        ),
        yaxis = list(
          title = "Age (coded)",
          tickmode = "array",
          tickvals = age_tick_vals,
          ticktext = levels(df$age5)
        ),
        zaxis = list(title = "Predicted Score")
      ),

        annotations = list(
        list(
          text = message,
          x = 0.02, y = 0.98,
          xref = "paper", yref = "paper",
          align = "left",
          showarrow = FALSE,
          font = list(size = 14)
        )
      )


    )

  # --------------------------------------------
  # 5. Save output
  # --------------------------------------------
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  file_path <- file.path(save_dir, paste0(question_name, ".", file_type))

  if (file_type == "html") {
    htmlwidgets::saveWidget(p, file_path, selfcontained = FALSE)
  } else if (file_type == "png") {
    plotly::orca(p, file_path)
  }

  return(p)
}

# data cleaning funcs
factorize_1to4 <- function(x, drop98 = FALSE, to = 4) {
  x <- suppressWarnings(as.numeric(x))
  if (drop98) x[x == 98] <- NA_real_
  x[!x %in% 1:to] <- NA_real_
  factor(x, levels = 1:to, ordered = TRUE)
}

# descriptive table funcs

# ---- Generalized cross-tab counts for any factor vs any grouping ----
cross_tab_counts <- function(df, factor_var, factor_levels, group_var, group_levels = NULL, section_title) {
  # Pull and ensure factor levels
  fac <- factor(dplyr::pull(df, {{ factor_var }}), levels = factor_levels)
  
  # Pull group variable; optionally specify levels
  grp_levels <- group_levels %||% sort(unique(df[[deparse(substitute(group_var))]]))
  grp <- factor(dplyr::pull(df, {{ group_var }}), levels = grp_levels)
  
  # 2D frequency table
  tab <- xtabs(~ fac + grp, drop.unused.levels = FALSE) %>% as.matrix()
  
  # Ensure all group columns exist
  missing_grps <- setdiff(grp_levels, colnames(tab))
  if(length(missing_grps)) {
    tab <- cbind(tab, setNames(matrix(0, nrow = nrow(tab), ncol = length(missing_grps)), missing_grps))
  }
  
  # Reorder rows and columns
  tab <- tab[factor_levels, grp_levels, drop = FALSE]
  
  # Return tidy tibble
  tibble::tibble(
    Factor = section_title,
    `Category (Code)` = paste0(factor_levels, " (", seq_along(factor_levels), ")"),
    !!!setNames(lapply(seq_along(grp_levels), function(i) as.integer(tab[, i])), 
                paste0("Sample Size (", grp_levels, ")"))
  )
}

# ordinal logistic regression funcs
preprocess_df <- function(df, yvar_chr, age_order = lvl_age) {
  df_use <- df %>% tidyr::drop_na(dplyr::all_of(yvar_chr))
  df_use <- df_use %>%
    dplyr::mutate(
      age5 = factor(age5, levels = age_order, ordered = FALSE)
    )
  contrasts(df_use$age5) <- contr.treatment(n = length(age_order), base = 1)
  df_use
}

# ---- 2. Formula build and Fit POLR ----
fit_polr <- function(yvar_chr,
                     df_use,
                     rhs = "age5 + gender + education + income", ...) {
  fml <- as.formula(paste0(yvar_chr, " ~ ", rhs))
  MASS::polr(fml, data = df_use, Hess = TRUE, na.action = na.omit, ...)
}

# ---- 4. Tidy and add Odds Ratio / CI / p-calue ----
tidy_polr_results <- function(model, conf.level = 0.95) {
  tt <- broom::tidy(model, conf.int = TRUE, conf.level = conf.level)
  # drop thresholds
  tt <- tt %>% dplyr::filter(!grepl("\\|", term)) %>%
    dplyr::mutate(
      z = estimate / std.error,
      p.value = 2 * pnorm(-abs(z)),
      OR = exp(estimate),
      LCL = exp(conf.low),
      UCL = exp(conf.high)
    )
  tt
}

# Helper to map category names
map_cat <- function(term, map, label) {
  ifelse(term %in% names(map), map[term], NA_character_)
}

# ---- 5. Formatting helpers ----

fmt2 <- function(x) formatC(x, format = "f", digits = 2)
stars <- function(p) dplyr::case_when(
  is.na(p) ~ "",
  p < .001 ~ "***",
  p < .01  ~ "**",
  p < .05  ~ "*",
  TRUE     ~ ""
)

# different dataset funcs

# pipelines for different dataset cleaning and modeling
process_different_dataset <- function(dataset_path, question_mapping, gender_levels, income_levels, education_levels, age_levels, filename, ... ) {
  stopifnot(file.exists(dataset_path))
  raw  <- read_sav(dataset_path)
  cdhs <- raw %>% clean_names()
  # print colnames for debugging
  print(colnames(cdhs))
  # rename cdhs colums according to question_mapping
  cdhs <- cdhs %>%
    rename(!!!question_mapping)

  args <- list(...)
  if (!is.null(args$age_adjust)) {
    cdhs$age_new <- cdhs$age_new + 1
  }

  required_cols <- c("q40_un","q41_un","q42_un","q43_un","age_new","gender","q66_un","q55_un")
  missing_cols  <- setdiff(required_cols, names(cdhs))
  if (length(missing_cols)) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

  cdhs <- cdhs %>%
    mutate(
      q40_un_o = factorize_1to4(q40_un, drop98 = FALSE, to = args$factorize_to %||% 4),
      q41_un_o = factorize_1to4(q41_un, drop98 = TRUE, to = args$factorize_to %||% 4),
      q42_un_o = factorize_1to4(q42_un, drop98 = TRUE, to = args$factorize_to %||% 4),
      q43_un_o = factorize_1to4(q43_un, drop98 = TRUE, to = args$factorize_to %||% 4)
    )
  
  # AGE -------------------------------------------------------------
  cdhs <- cdhs %>%
    mutate(
      age_code = as.integer(age_new),
      age5 = map_chr(age_code, ~ {
        i <- .x
        if (i %in% seq_along(age_levels)) age_levels[i] else NA_character_
      }),
      age5 = factor(age5, levels = age_levels, ordered = TRUE)
    )

  # GENDER ----------------------------------------------------------
  cdhs <- cdhs %>%
    mutate(
      gender_code = as.integer(gender),
      gender = map_chr(gender_code, ~ {
        i <- .x
        if (i %in% seq_along(gender_levels)) gender_levels[i] else NA_character_
      }),
      gender = factor(gender, levels = gender_levels)
    )

  # EDUCATION -------------------------------------------------------
  cdhs <- cdhs %>%
    mutate(
      q66_un_code = as.integer(q66_un),
      q66_un_code = if_else(q66_un_code %in% c(96, 97, 99), NA_integer_, q66_un_code),
      education = map_chr(q66_un_code, ~ {
        i <- .x
        if (i %in% seq_along(education_levels)) education_levels[i] else NA_character_
      }),
      education = factor(education, levels = education_levels)
    )

  # INCOME ----------------------------------------------------------
  cdhs <- cdhs %>%
    mutate(
      q55_un_code = as.integer(q55_un),
      q55_un_code = if_else(q55_un_code %in% c(99), NA_integer_, q55_un_code),
      income = map_chr(q55_un_code, ~ {
        i <- .x
        if (i %in% seq_along(income_levels)) income_levels[i] else NA_character_
      }),
      income = factor(income, levels = income_levels)
    )

  cdhs <- cdhs %>%
    dplyr::select(q40_un_o, q41_un_o, q42_un_o, q43_un_o, age5, gender, education, income) %>%
    tidyr::drop_na(age5, gender, education, income)

  if (!is.null(args$drop_columns)) {
    cdhs <- cdhs %>% dplyr::select(-dplyr::all_of(args$drop_columns))
  }

  readr::write_csv(cdhs, glue("data/{filename}.csv"))
  saveRDS(cdhs, glue("data/{filename}.rds"))
}


#-----------------------------------------------------------------
# extenion variable levels
#---------------------------------------------------------

lvl_education_2023 <- c("No certificate, diploma or degree",
  "Secondary (high) school diploma or equivalency certificate",
  "Apprenticeship or trades certificate or diploma",
  "College, CEGEP or other non-university certificate or diploma",
  "University certificate or diploma below bachelor level",
  "University certificate, diploma or degree at bachelor level or above")

lvl_age_2023 <- c("16–24 years", "25–34 years", "35–44 years", "45–54 years", "55–64 years", "65+ years")
lvl_gender_2023 <- c("female", "male", "other")
lvl_income_2023 <- c("< $50,000", "$50,000-$60,000", "$60,000-$70,000", "$70,000-$80,000", "$80,000-$90,000", "$100,000-$150,000", ">$150,000")


lvl_education_2024 <- c("No certificate, diploma or degree",
                        "Secondary (high) school diploma or equivalency certificate",
                        "Apprenticeship or trades certificate or diploma",
                        "College, CEGEP or other non-university certificate or diploma",
                        "University certificate or diploma below bachelor level",
                        "University certificate, diploma or degree at bachelor level",
                        "University certificate, diploma or degree at the graduate level")

lvl_age_2024 <- c("16–17 years", "18–24 years", "25–34 years", "35–44 years", "45–54 years", "55–64 years", "65–74 years", "75+ years")
lvl_gender_2024 <- c("female", "male", "other")
lvl_income_2024 <- c("< $40,000", "$40,000-$59,000", "$60,000-$79,000", "$80,000-$99,000", "$100,000-$149,000", ">$150,000")