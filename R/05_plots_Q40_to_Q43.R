#############################################
# 05_plots_Q40_to_Q43.R
# Purpose:
#   replicate the 4 3d plots from the paper
# Input:
#   artifacts/cleaned_dataset_used_for_modelling.csv
# Output:
#   output/plot/fig1_Q40.png
#   output/plot/fig2_Q41.png
#   output/plot/fig3_Q42.png
#   output/plot/fig4_Q43.png
#############################################

library(tidyverse)
library(here)
library(broom)
library(plotly)
library(dplyr)
library(tidyr)
library(MASS)
# 1. Load cleaned dataset ---------------------------------------------
data_path <- here("model_df.csv")
df <- read_csv(data_path)
# we need to predict y-hat values for Q40 to Q43 and save them in the dataframe

mod_q40 <- readRDS("artifacts/models/q40_model.rds")
mod_q41 <- readRDS("artifacts/models/q41_model.rds")
mod_q42 <- readRDS("artifacts/models/q42_model.rds")
mod_q43 <- readRDS("artifacts/models/q43_model.rds")

# we just need one to extract the levels of ordinal variables
mf <- mod_q40$model


plot_question_surface <- function(model, df, question_name, save_dir = "artifacts/plots", file_type = "html") {

  # --- 1. Build prediction grid ----------------------------------------------
  pred_grid <- expand.grid(
    age5      = levels(mf$age5),
    gender    = levels(mf$gender),
    education = levels(mf$education)[4], 
    income    = levels(mf$income)[4]     
  )

  # --- 2. Predict probabilities ------------------------------------------------
  probs <- predict(model, newdata = pred_grid, type = "probs")

  # Convert levels (columns) to numeric scores
  levels_y <- seq_len(ncol(probs))
  pred_grid$mean_score <- as.numeric(probs %*% levels_y)


  # sorted unique values for axes
  age_levels    <- levels(pred_grid$age5)     # 5 categories
  gender_levels <- levels(pred_grid$gender)   # 3 categories

  # wide matrix in correct order
  zmat <- pred_grid %>%
    mutate(age5 = factor(age5, levels = age_levels)) %>%
    tidyr::pivot_wider(
      names_from = gender,
      values_from = mean_score
    ) %>%
    arrange(age5)

  z_mat <- as.matrix(zmat[, rev(gender_levels)])
  # numeric positions for surface plot
  x_num <- seq_along(age_levels)       # 1 2 3 4 5
  y_num <- seq_along(gender_levels)    # 1 2 3

  print('z_mat')
  print(z_mat)
  # --- 4. Plot ----------------------------------------------------------------
 p <- plot_ly(
    x = 1:ncol(z_mat),               # 3,2,1 instead of 1,2,3
    y = 1:nrow(z_mat),
    z = z_mat,      # reorder columns of matrix!
    type = "surface"
) %>%
  layout(
    title = paste(question_name, "- Predicted Mean Score"),
    scene = list(
      yaxis = list(
        title = "Age",
        tickmode = "array",
        tickvals = 1:nrow(z_mat),
        ticktext = age_levels
      ),
      xaxis = list(
        title = "Gender",
        tickmode = "array",
        tickvals = 1:ncol(z_mat),
        ticktext = rev(gender_levels)   # flipped labels
      ),
      zaxis = list(title = "Mean Score")
    )
  )
  
    # --- 5. Save plot -----------------------------------------------------------
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

  file_path <- file.path(save_dir, paste0(question_name, ".", file_type))

  if (file_type == "html") {
  htmlwidgets::saveWidget(p, file_path, selfcontained = FALSE)
} else if (file_type == "png") {
  plotly::orca(p, file_path)
} else {
  warning("Unsupported file type: choose 'html' or 'png'")
}

  return(p)
}




plot_q40 <- plot_question_surface(mod_q40, df, "Q40")
plot_q41 <- plot_question_surface(mod_q41, df, "Q41")
plot_q42 <- plot_question_surface(mod_q42, df, "Q42")
plot_q43 <- plot_question_surface(mod_q43, df, "Q43")




































# 2. create function that makes 3d plot for a given question ----------------
make_3d_plot <- function(data, xaxis, yaxis, zaxis,
                         title = NULL, notes = NULL, filename = NULL,
                         x_ticks = NULL, x_labels = NULL,
                         y_ticks = NULL, y_labels = NULL,
                         z_ticks = NULL, z_labels = NULL) {

  # Check columns
  if (!all(c(xaxis, yaxis, zaxis) %in% names(data))) {
    stop("One or more of xaxis / yaxis / zaxis not found in 'data'")
  }

  # Prepare data: use numeric codes for sorting
  df2 <- data %>%
    dplyr::mutate(
      .x = .data[[xaxis]],
      .y = .data[[yaxis]],
      .z = as.numeric(.data[[zaxis]])
    )

  # Get unique sorted values
  x_vals <- sort(unique(df2$.x))
  y_vals <- sort(unique(df2$.y))

  # Create grid of mean z values
  grid <- df2 %>%
    dplyr::group_by(.y, .x) %>%
    dplyr::summarise(mean_z = mean(.z, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(.y = y_vals, .x = x_vals, fill = list(mean_z = NA)) %>%
    dplyr::arrange(.y, .x)

  # Pivot to matrix
  z_matrix <- grid %>%
    tidyr::pivot_wider(names_from = .x, values_from = mean_z) %>%
    dplyr::select(-.y) %>%
    as.matrix()

  # Tick labels
  x_tickvals <- if (is.null(x_ticks)) x_vals else x_ticks
  y_tickvals <- if (is.null(y_ticks)) y_vals else y_ticks

  # Create plot
  p <- plotly::plot_ly(
    x = x_tickvals,
    y = y_tickvals,
    z = z_matrix,
    type = "surface",
    hoverinfo = "x+y+z"
  ) %>%
    plotly::layout(
      title = list(text = title, x = 0.5),
      scene = list(
        xaxis = list(title = xaxis, tickvals = x_tickvals, ticktext = x_labels),
        yaxis = list(title = yaxis, tickvals = y_tickvals, ticktext = y_labels),
        zaxis = list(title = zaxis, tickvals = z_ticks, ticktext = z_labels)
      ),
      annotations = list(
        list(
          text = notes,
          xref = "paper", yref = "paper",
          x = 0.02, y = -0.1,
          showarrow = FALSE
        )
      )
    )

  # Save if needed
  if (!is.null(filename)) {
    file_path <- here::here("artifacts", filename)
    htmlwidgets::saveWidget(p, file_path)
  }

  return(p)
}



# fig1: Q40
make_3d_plot(
  data = df,
  xaxis = "age_code",
  yaxis = "gender_code",
  zaxis = "q40_p",
  title = "Knowledge about AI",
  notes = "Y-hat Surface Plot Age code vs Gender",
  filename = "ai_knowledge_y_hat.html",
  x_labels = c("16–24", "25–34", "35–44", "45–54", "55+"),
  y_labels = c("Male", "Female", "Other"),
  z_ticks = seq(2.2, 3.1, by = 0.1),
  z_labels = as.character(seq(2.2, 3.1, by = 0.1))
)

# fig2: Q41


make_3d_plot(
  data = df,
  xaxis = "age_code",
  yaxis = "gender_code",
  zaxis = "q41_o",
  title = "Comfort with AI use in Healthcare",
  notes = "Y-hat Surface Plot Age code vs Gender",
  filename = "ai_comfort_in_healthcare.html",
  x_labels = c("16–24", "25–34", "35–44", "45–54", "55+"),
  y_labels = c("Male", "Female", "Other"),
  z_ticks = seq(2.7, 3.2, by = 0.1),
  z_labels = as.character(seq(2.70, 3.20, by = 0.1))
)


# fig1: Q42
make_3d_plot(
  data = df,
  xaxis = "age_code",
  yaxis = "gender_code",
  zaxis = "q42_o",
  title = "Comfort with consented data being used for AI research",
  notes = "Y-hat Surface Plot Age code vs Gender",
  filename = "ai_research_comfort_consented.html",
  x_labels = c("16–24", "25–34", "35–44", "45–54", "55+"),
  y_labels = c("Male", "Female", "Other"),
  z_ticks = seq(2.8, 3.3, by = 0.1),
  z_labels = as.character(seq(2.80, 3.30, by = 0.1))
)

# fig4: Q43
make_3d_plot(
  data = df,
  xaxis = "age_code",
  yaxis = "gender_code",
  zaxis = "q42_o",
  title = "Comfort with deidentified data being used for AI research",
  notes = "Y-hat Surface Plot Age code vs Gender",
  filename = "ai_research_comfort_deidentified.html",
  x_labels = c("16–24", "25–34", "35–44", "45–54", "55+"),
  y_labels = c("Male", "Female", "Other"),
  z_ticks = seq(2.3, 2.8, by = 0.1),
  z_labels = as.character(seq(2.30, 2.80, by = 0.1))
)





df_summary <- df %>%
  mutate(q40_p = as.numeric(as.character(q40_p))) %>%
  dplyr::group_by(gender_code, age_code) %>%
  dplyr::summarise(
    mean_q40_p = mean(q40_p, na.rm = TRUE),
    sum_q40_p = sum(q40_p, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  dplyr::ungroup()
