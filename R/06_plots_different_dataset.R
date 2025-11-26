#############################################
# 06_plots_different_dataset.R
# Purpose:
#   generate new 3D plots via new dataset
# Input:
#   data_clean/model_df.csv
# Output:
#   artifacts/plot/Q40_2023.html
#   artifacts/plot/Q41_2023.html
#   artifacts/plot/Q42_2023.html
#   artifacts/plot/Q43_2023.html
#   artifacts/plot/Q40_2024.html
#   artifacts/plot/Q41_2024.html
#   artifacts/plot/Q42_2024.html
#   artifacts/plot/Q43_2024.html
#############################################

# ---- 1) Load cleaned dataset  ----
df <- read_csv("data/cleaned_dataset_2023.csv", show_col_types = FALSE)
# we need to predict y-hat values for Q40 to Q43 and save them in the dataframe
# but first the authors used Multivariate polynomial regression so we need to fit that model first

df$income_num    <- convert_to_num(df$income, lvl_income_2023)
df$educ_num      <- convert_to_num(df$education, lvl_education_2023)
df$age_num       <- convert_to_num(df$age5, lvl_age_2023)
# df$gender_num    <- convert_to_num(df$gender, lvl_gender_2023)
df <- df %>%
  dplyr::mutate(
    gender_num = dplyr::case_when(
      gender == "male"   ~ 1,
      gender == "female" ~ 2,
      gender == "other"  ~ 3,
      TRUE               ~ NA_real_
    )
  )
# DOE Pro (SigmaZone) uses Response Surface Methodology (RSM)
predictors <- c("age_num", "gender_num", "educ_num", "income_num")
# y=β0*+i∑ βi*xi (main effects)+i<j∑ βij*xi*xj (interactions)+i∑ βii*xi2 (quadratics)

f_poly <- as.formula(
  paste(
    "y ~ (", paste(predictors, collapse = " + "), ")^2 +",
    paste("I(", predictors, "^2)", collapse = " + ")
  )
)

# fit lm gives the formula
# Model specification:
# y = β0 
#   + β1 * age_num 
#   + β2 * gender_num 
#   + β3 * educ_num 
#   + β4 * income_num
#   + β5 * age_num^2
#   + β6 * gender_num^2
#   + β7 * educ_num^2
#   + β8 * income_num^2
#   + β9  * (age_num * gender_num)
#   + β10 * (age_num * educ_num)
#   + β11 * (age_num * income_num)
#   + β12 * (gender_num * educ_num)
#   + β13 * (gender_num * income_num)
#   + β14 * (educ_num * income_num)
#   + ε


# fitting model for all outcomes
outcomes <- c("q40_un_o", "q41_un_o", "q42_un_o", "q43_un_o")

models <- lapply(outcomes, function(resp) {
  f <- update(f_poly, paste(resp, "~ ."))
  lm(f, data = df)
})

names(models) <- outcomes

plot_q40 <- plot_question_surface_poly(models$q40_un_o, df, "Q40_2023", education_level_fix= 5, income_level_fix = c(5,6), message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")
plot_q41 <- plot_question_surface_poly(models$q41_un_o, df, "Q41_2023", education_level_fix= 5, income_level_fix = c(5,6), message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")
plot_q42 <- plot_question_surface_poly(models$q42_un_o, df, "Q42_2023", education_level_fix= 5, income_level_fix = c(5,6), message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")
plot_q43 <- plot_question_surface_poly(models$q43_un_o, df, "Q43_2023", education_level_fix= 5, income_level_fix = c(5,6), message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")



# ---- 1) Load cleaned dataset  ----
df2 <- read_csv("data/cleaned_dataset_2024.csv", show_col_types = FALSE)
# we need to predict y-hat values for Q40 to Q43 and save them in the dataframe
# but first the authors used Multivariate polynomial regression so we need to fit that model first

df2$income_num    <- convert_to_num(df2$income, lvl_income_2024)
df2$educ_num      <- convert_to_num(df2$education, lvl_education_2024)
df2$age_num       <- convert_to_num(df2$age5, lvl_age_2024)
# df2$gender_num    <- convert_to_num(df2$gender, lvl_gender_2024)
df2 <- df2 %>%
  dplyr::mutate(
    gender_num = dplyr::case_when(
      gender == "male"   ~ 1,
      gender == "female" ~ 2,
      gender == "other"  ~ 3,
      TRUE               ~ NA_real_
    )
  )
# DOE Pro (SigmaZone) uses Response Surface Methodology (RSM)
predictors <- c("age_num", "gender_num", "educ_num", "income_num")
# y=β0*+i∑ βi*xi (main effects)+i<j∑ βij*xi*xj (interactions)+i∑ βii*xi2 (quadratics)

f_poly <- as.formula(
  paste(
    "y ~ (", paste(predictors, collapse = " + "), ")^2 +",
    paste("I(", predictors, "^2)", collapse = " + ")
  )
)

# fit lm gives the formula
# Model specification:
# y = β0 
#   + β1 * age_num 
#   + β2 * gender_num 
#   + β3 * educ_num 
#   + β4 * income_num
#   + β5 * age_num^2
#   + β6 * gender_num^2
#   + β7 * educ_num^2
#   + β8 * income_num^2
#   + β9  * (age_num * gender_num)
#   + β10 * (age_num * educ_num)
#   + β11 * (age_num * income_num)
#   + β12 * (gender_num * educ_num)
#   + β13 * (gender_num * income_num)
#   + β14 * (educ_num * income_num)
#   + ε


# fitting model for all outcomes
outcomes <- c("q40_un_o", "q41_un_o", "q42_un_o", "q43_un_o")

models <- lapply(outcomes, function(resp) {
  f <- update(f_poly, paste(resp, "~ ."))
  lm(f, data = df2)
})

names(models) <- outcomes

plot_q40 <- plot_question_surface_poly(models$q40_un_o, df2, "Q40_2024", education_level_fix= 5, income_level_fix = 4, message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")
plot_q41 <- plot_question_surface_poly(models$q41_un_o, df2, "Q41_2024", education_level_fix= 5, income_level_fix = 4, message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")
plot_q42 <- plot_question_surface_poly(models$q42_un_o, df2, "Q42_2024", education_level_fix= 5, income_level_fix = 4, message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")
plot_q43 <- plot_question_surface_poly(models$q43_un_o, df2, "Q43_2024", education_level_fix= 5, income_level_fix = 4, message = "income= $80,000-$99,000 and education= University certificate or diploma below bachelor level")




