#############################################
# 04_export_regression_tables.R
# Purpose:
#   Combine the 4 logistic regression models (Q40–Q43)
#   and export a single Table A1-like CSV file.
# Input:
#   output/models_Q40_to_Q43.rds
# Output:
#   output/tableA1_combined.csv
#############################################

library(tidyverse)
library(here)
library(broom)
library(gt)
library(purrr)

# 1. Load models -----------------------------------------------------
models_path <- here("output", "models_Q40_to_Q43.rds")
models_list <- readRDS(models_path)

# 2. Helper: tidy one model and annotate -----------------------------
tidy_one <- function(model, q_label, q_title) {
  td <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # The 'term' column includes variable names such as:
  # (Intercept), age..., gender..., income..., education...
  # We split it into Factor and Category columns
  td %>%
    mutate(
      dependent_variable = q_title,
      factor = case_when(
        str_starts(term, "age") ~ "Age",
        str_starts(term, "age_") ~ "Age",
        str_starts(term, "gender") ~ "Gender",
        str_starts(term, "Q55") ~ "Income",
        str_starts(term, "income") ~ "Income",
        str_starts(term, "Q66") ~ "Education",
        str_starts(term, "education") ~ "Education",
        term == "(Intercept)" ~ "Intercept",
        TRUE ~ "Other"
      ),
      # Remove the factor prefix to create clean category names
      category = case_when(
        term == "(Intercept)" ~ "1.00 (ref)",
        factor == "Age" ~ str_remove(term, "^age(_new)?"),
        factor == "Gender" ~ str_remove(term, "^gender"),
        factor == "Income" ~ str_remove(term, "^(Q55|income)"),
        factor == "Education" ~ str_remove(term, "^(Q66|education)"),
        TRUE ~ term
      ),
      question = q_label
    )
}

# 3. Tidy all 4 questions --------------------------------------------
q40_title <- "Q.40 - How knowledgeable are you about what artificial intelligence is?"
q41_title <- "Q.41 - How comfortable are you with AI being used as a tool in healthcare?"
q42_title <- "Q.42 - How comfortable are you with scientists using personal health data for AI research as long as informed consent has been provided by the patient?"
q43_title <- "Q.43 - How comfortable are you with scientists using personal health data for AI research without informed consent as long as it is deidentified?"

td_q40 <- tidy_one(models_list$Q40, "Q40", q40_title)
td_q41 <- tidy_one(models_list$Q41, "Q41", q41_title)
td_q42 <- tidy_one(models_list$Q42, "Q42", q42_title)
td_q43 <- tidy_one(models_list$Q43, "Q43", q43_title)

# 4. Combine all into a single dataframe -----------------------------
all_td <- bind_rows(td_q40, td_q41, td_q42, td_q43)

# 5. Format to match Appendix A1 style -------------------------------
export_tbl <- all_td %>%
  # Remove intercept rows (Appendix A1 does not report intercepts)
  filter(term != "(Intercept)") %>%
  transmute(
    `Dependent Variable` = dependent_variable,
    Factor = factor,
    Category = category,
    `Odds Ratio` = estimate,
    `95% CI` = paste0("(", round(conf.low, 2), "-", round(conf.high, 2), ")"),
    `p-value` = if_else(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
    question
  ) %>%
  mutate(
    Factor = factor(Factor, levels = c("Age", "Gender", "Income", "Education", "Other"))
  ) %>%
  arrange(question, Factor)

# 6. Write output ----------------------------------------------------
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
}

readr::write_csv(export_tbl, here("output", "tableA1_combined.csv"))

message("Export completed: output/tableA1_combined.csv")


# --- 7. Also export as an HTML table --------------------------------

# Create a gt table for HTML output

# question titles and Ns – adjust N if your fitted N is different
question_meta <- tribble(
  ~question, ~title,
  "Q40", "Q.40 – How knowledgeable are you about what artificial intelligence is?",
  "Q41", "Q.41 – How comfortable are you with AI being used as a tool in healthcare?",
  "Q42", "Q.42 – How comfortable are you with scientists using personal health data for AI research as long as informed consent has been provided by the patient?",
  "Q43", "Q.43 – How comfortable are you with scientists using personal health data for AI research without informed consent as long as it is deidentified?"
)

# helper: build one block (one question)
build_block <- function(q_id, q_title) {
  # body rows for this question
  body_q <- export_tbl %>%
    filter(question == q_id) %>%
    mutate(
      # glue OR and CI into one column
      `Odds Ratio (95% CI)` = paste0(sprintf("%.2f", `Odds Ratio`), " ", `95% CI`),
      `p-value` = as.character(`p-value`),
      Factor = as.character(Factor),
      Categories = as.character(Category)
    ) %>%
    select(Categories, Factor, `Odds Ratio (95% CI)`, `p-value`)
  
  # reference rows (fixed, to mimic paper) :contentReference[oaicite:1]{index=1}
  ref_q <- tibble(
    Factor = c("Age", "Gender", "Income", "Education"),
    Categories = c("16-24", "Male", "< $24,999", "Highschool"),
    `Odds Ratio (95% CI)` = "1.00 (ref)",
    `p-value` = ""
  )
  
  # combine ref + actual, keep factor order
  bind_rows(ref_q, body_q) %>%
    mutate(
      question_block = q_title,
      Factor = factor(Factor, levels = c("Age", "Gender", "Income", "Education"))
    ) %>%
    arrange(Factor)
}

# build 4 blocks and stack
html_data <- question_meta %>%
  pmap_dfr(~build_block(..1, ..2))

html_tbl <- html_data %>%
  gt(
    rowname_col = "Categories",
    groupname_col = "question_block"
  ) %>%
  tab_header(
    title = md("**Appendix A1 – Logistic regression results (Q40–Q43)**"),
    subtitle = "Odds ratios (95% CI) and p-values"
  ) %>%
  cols_label(
    Factor = "Factor",
    `Odds Ratio (95% CI)` = "Odds Ratio (95% CI)",
    `p-value` = "p-value"
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Odds Ratio (95% CI)`, `p-value`)
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", color = "grey70", weight = px(1)),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(4)
  )

gtsave(html_tbl, here("output", "tableA1_combined.html"))
message("4-block HTML saved to output/tableA1_combined.html")