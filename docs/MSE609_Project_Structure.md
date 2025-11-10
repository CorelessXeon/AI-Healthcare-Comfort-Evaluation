# Project File Structure — MSE609 Group11 Project

This document describes the directory layout of the project and the purpose of the main folders/files.\
It is intended to help collaborators understand where to find raw data, cleaned data, analysis scripts, and outputs.

------------------------------------------------------------------------

## 1. Root Directory

``` text
MSE609-Group11-Project/
│
├── archive/
├── data_clean/
├── data_raw/
├── docs/
├── output/
├── R/
├── renv/
│
├── .gitignore
├── .RData
├── .Rhistory
├── .Rprofile
├── folder_snapshot.txt
├── MSE609-Group11-Project.Rproj
└── renv.lock
```

**Notes**

-   `.gitignore`, `.RData`, `.Rhistory`, `.Rprofile`: standard R / project metadata files.
-   `MSE609-Group11-Project.Rproj`: RStudio project file.
-   `renv.lock`: records R package versions used in this project.
-   `folder_snapshot.txt`: automatically generated snapshot of the folder structure (for sharing/archiving).

------------------------------------------------------------------------

## 2. `archive/` — drafts and alternatives

``` text
archive/
├── Feasibility Outline.md
├── Option 1.md
├── Option 2.md
├── Option 3.md
├── README.md
└── Speaker Note.md
```

Stores early-stage documents, option comparisons, and notes.

------------------------------------------------------------------------

## 3. `data_clean/` — cleaned datasets

``` text
data_clean/
├── clean_data.csv
├── clean_data.rds
├── clean_data_strict.csv
├── clean_data_strict.rds
├── clean_summary.csv
└── clean_summary_strict.csv
```

These are the datasets after cleaning and variable selection.\
They are the main inputs for descriptive tables and regression.\
“strict” versions apply tighter inclusion rules.

------------------------------------------------------------------------

## 4. `data_raw/` — original ATS2021 data

``` text
data_raw/
├── ATS2021 Dataset_Dataverse posting.RData
├── ATS2021 Dataset_Dataverse posting.sav
└── ATS2021 Dataset_Dataverse posting.tab
```

Holds the original survey data in different formats as downloaded from Dataverse.

------------------------------------------------------------------------

## 5. `docs/` — reference documents

``` text
docs/
├── Appendix A. Supplementary data.docx
├── Knowledge is not all you need for comfort in use of AI in healthcare.pdf
├── MSE 609 Project Midterm Notes.txt
└── Supplementary File.docx
```

Contains paper-related materials, appendices, and course notes.

------------------------------------------------------------------------

## 6. `output/` — generated analysis results

``` text
output/
├── appendix_table_1A_replication.html
├── models_Q40_to_Q43.rds
├── model_Q40_tidy.csv
├── model_Q41_tidy.csv
├── model_Q42_tidy.csv
├── model_Q43_tidy.csv
├── table1_age_by_gender.csv
├── table1_age_by_gender_strict.html
├── table1_combined.csv
├── table1_combined_strict.html
├── table1_education_by_gender.csv
├── table1_education_by_gender_strict.html
├── table1_income_by_gender.csv
├── table1_income_by_gender_strict.html
├── table1_replication.html
├── tableA1_combined.csv
└── tableA1_combined.html
```

This folder contains the outputs produced by the R scripts: - descriptive tables (Table 1 variants), - tidy regression results for Q40–Q43, - replication/appendix tables (HTML/CSV).

------------------------------------------------------------------------

## 7. `R/` — main analysis scripts

``` text
R/
├── 01_data_cleaning.R
├── 02_descriptives_table1.R
├── 02_descriptives_table1_strict.R
├── 03_models_Q40_to_Q43.R
├── 04_export_regression_tables.R
└── 05_plots_Q40_to_Q43.R
```

Script roles:

1.  **01_data_cleaning.R** — load raw ATS data and produce cleaned data into `data_clean/`.
2.  **02_descriptives_table1.R** — reproduce descriptive statistics tables (normal version).
3.  **02_descriptives_table1_strict.R** — same as above but with stricter filtering.
4.  **03_models_Q40_to_Q43.R** — run models for survey questions Q40–Q43.
5.  **04_export_regression_tables.R** — export model results to CSV/HTML in `output/`.
6.  **05_plots_Q40_to_Q43.R** — create plots for the Q40–Q43 models.

------------------------------------------------------------------------

## 8. `renv/` — project environment

``` text
renv/
├── .gitignore
├── activate.R
├── settings.json
├── staging/
└── library/   ← auto-generated, not listed in detail
```

-   This project uses **renv** to freeze R package versions.

-   The folder `renv/library/` is created locally on each machine when `renv::restore()` is run.

-   **Because it is machine-specific and can be very large, the contents of `renv/library/` are intentionally not listed here.**\
    When sharing the project, other users can run:

    ``` r
    renv::restore()
    ```

    to download the required packages into their own `renv/library/`.

------------------------------------------------------------------------

## 9. Processing flow (informal)

1.  Raw survey data → `data_raw/`
2.  Run `R/01_data_cleaning.R` → cleaned data → `data_clean/`
3.  Run `R/02_*` → descriptive tables → `output/`
4.  Run `R/03_*`, `R/04_*`, `R/05_*` → models, exports, figures → `output/`
5.  Documentation and drafts stay in `docs/` and `archive/`

------------------------------------------------------------------------

*Document date (ET): 2025-11-08*
