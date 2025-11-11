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

# 1. Load cleaned dataset ---------------------------------------------
data_path <- here("artifacts", "cleaned_dataset_used_for_modelling.csv")
df <- read_csv(data_path)
df <- df %>%
  mutate(age_code = as.numeric(factor(age5, 
    levels = c("16–24 years", "25–34 years", "35–44 years", "45–54 years", "55+ years"),
    ordered = TRUE))) %>%
  mutate(gender_code = as.numeric(factor(gender, 
    levels = c("male", "female", "other"))))

#select only the income = 4 and education = 4 (income == "$80,000-$99,000" and education == "University degree")
df <- df %>% filter(income == "$80,000-$99,000", education == "University degree")

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
  zaxis = "q40_o",
  title = "Knowledge about AI",
  notes = "Y-hat Surface Plot Age code vs Gender",
  filename = "ai_knowledge.html",
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
