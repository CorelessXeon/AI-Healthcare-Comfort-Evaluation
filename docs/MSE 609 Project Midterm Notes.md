# MSE 609 Project Midterm Notes

## 0. Progress Overview

### 0.1. GitHub Repository Setup – 90%

-   Missing README usage instructions.

### 0.2. Data Cleaning – 100%

-   Completed.
    -   Kai’s questions: resolved (see details below).

### 0.3. Replication of Descriptive Tables (Table 1) – 100%

-   Completed.

### 0.4. Replication of Regression Analysis 3D Figures (Fig. 1–4) – 0%

-   In progress.

### 0.5. Replication of Appendix A1 Table 1 – 80%

-   In progress, with discrepancies between versions.

### 0.6. Integrated Midterm Report – 100%

-   Completed (this document).

### 0.7. Course Extension – 0%

-   Not started, expected completion next week.

### 0.8. Final Project Report – 0%

-   Not started, expected completion the week after next.

------------------------------------------------------------------------

## 1. Introduction

The purpose of this document is to ensure that all group members clearly understand the work and progress completed by Kai and me, as well as the underlying logic and methodology behind it. The goal is to help everyone **stay on the same page** and avoid getting lost during the replication process.

The project’s GitHub repository has been published and can be accessed here:\
-\> [MSE609-Group11-Project Repository](https://github.com/CorelessXeon/MSE609-Group11-Project)

Below is an overview of the current package file structure for both the **local environment** and **cloud environment** (non-uploaded files are noted).

### MSE609 Project File Structure Summary

#### 📁 Local Project File Structure (as of current progress)

```         
MSE609-Group11-Project/
│
├── 📂 data_raw/                         # Raw data files (from Borealis Dataverse)
│   ├── Infoway CDHS 2024 SPSS Raw Data_for Dataverse.RData # New data for extention not used yet
│   ├── ATS2021 Dataset_Dataverse posting.RData # Actually used
│   ├── ATS2021 Dataset_Dataverse posting.sav   # Not used
│   └── ATS2021 Dataset_Dataverse posting.tab   # Not used
│
├── 📂 data_clean/                       # Cleaned and processed data outputs
│   ├── clean_data.csv
│   ├── clean_data.rds
│   ├── clean_data_strict.csv
│   ├── clean_data_strict.rds
│   ├── clean_summary.csv
│   └── clean_summary_strict.csv
│
├── 📂 R/                                # Core R scripts for replication pipeline
│   ├── 01_data_cleaning.R               # Data cleaning script
│   ├── 02_descriptives_table1.R         # Descriptive Table 1 (available-case)
│   ├── 02_descriptives_table1_strict.R  # Descriptive Table 1 (strict-case)
│   ├── 03_models_Q40_to_Q43.R           # Regression model scripts (GLM or Ordinal)
│   ├── 04_export_regression_tables.R    # Export combined regression tables (Appendix A1)
│   └── 05_plots_Q40_to_Q43.R            # Generate regression-based 3D plots (Fig. 1–4)
│
├── 📂 output/                           # Model outputs and replicated tables
│   ├── table1_combined.csv
│   ├── table1_combined_strict.html
│   ├── tableA1_combined.csv
│   ├── tableA1_combined.html
│   ├── model_Q40_tidy.csv
│   ├── model_Q41_tidy.csv
│   ├── model_Q42_tidy.csv
│   ├── model_Q43_tidy.csv
│   └── models_Q40_to_Q43.rds
│
├── 📂 docs/                             # Supporting documents and references
│   ├── Appendix A. Supplementary data.docx
│   ├── Knowledge is not all you need for comfort in use of AI in healthcare.pdf
│   ├── Supplementary File.docx
│   └── MSE 609 Project Midterm Notes.txt
│
├── 📂 archive/                          # Optional archival materials
│   ├── Feasibility Outline.md
│   ├── Option 1.md
│   ├── Option 2.md
│   ├── Option 3.md
│   ├── README.md
│   └── Speaker Note.md
│
├── 📂 renv/                             # R environment management
│   ├── activate.R
│   ├── settings.json
│   ├── staging/
│   ├── .gitignore
│   └── library/                         # auto-generated, not listed in detail
│
├── .gitignore
├── MSE609-Group11-Project.Rproj
├── renv.lock
└── folder_snapshot.txt
```

------------------------------------------------------------------------

#### ☁️ Cloud (GitHub Repository) Structure

```         
MSE609-Group11-Project/
│
├── 📂 data_raw/

│   └── ATS2021 Dataset_Dataverse posting.RData
├── 📁 data_clean/          # Retain cleaned datasets only (no raw data)
│   ├── clean_data.rds
│   ├── clean_data_strict.rds
│   └── clean_summary_strict.csv
│
├── 📁 R/                   # All reproducible R scripts
│   ├── 01_data_cleaning.R
│   ├── 02_descriptives_table1_strict.R
│   ├── 03_models_Q40_to_Q43.R
│   ├── 04_export_regression_tables.R
│   └── 05_plots_Q40_to_Q43.R
│
├── 📁 output/              # Key results for replication
│   ├── table1_combined_strict.html
│   ├── tableA1_combined.html
│   └── appendix_table_1A_replication.html
│
├── 📁 docs/                # Supplementary files for report and reference
│   ├── MSE 609 Project Midterm Notes.md
│   └── Appendix A. Supplementary data.docx
│
├── 📁 archive/             # (Optional) Early drafts and notes
│   ├── Feasibility Outline.md
│   ├── Option 1.md
│   ├── Option 2.md
│   ├── Option 3.md
│   ├── README.md
│   └── Speaker Note.md
│
├── .gitignore
├── renv.lock
└── README.md
```

------------------------------------------------------------------------

## 2. Progress Details

### 2.1. Data Cleaning

We did not use the raw CDHS dataset. Instead, we directly utilized the **pre-processed data** provided by the original researchers.

Dataset link:\
-\> [Borealis Dataverse – ATS2021 Dataset](https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/CEYG42&version=2.2)

This repository also includes clear documentation on **data usage licenses**, which can be referenced when writing the final report.

According to [Section 2.3](https://www.sciencedirect.com/science/article/pii/S0033350624004918?via%3Dihub#sec2) (*Description of Independent Variables*) in the paper, the researchers calculated respondents’ age by subtracting the year of birth from the survey year. Thus, in the dataset provided through the link, the original “year of birth” variable has already been replaced with an `age_new` variable.\
The total sample size (N = 12,052) matches the description in [Section 2.1](https://www.sciencedirect.com/science/article/pii/S0033350624004918?via%3Dihub#sec2) (*Recruitment and Data Collection*).

The meanings of the independent and dependent variables are summarized as follows (see corresponding code documentation for details).

------------------------------------------------------------------------

#### Independent Variables

| Variable Name | Label | Value | Description |
|:----------------|:---------------------|:----------------|:----------------|
| **age_new** | Respondent’s age group | 1 | 16–24 years |
|  |  | 2 | 25–34 years |
|  |  | 3 | 35–54 years |
|  |  | 4 | 55–64 years |
|  |  | 5 | 65+ years |
| **gender** | Respondent’s gender | 1 | Male |
|  |  | 2 | Female |
|  |  | 3 | Other / Non-binary |
| **Q55** | Total annual household income (before taxes) | 1 | \< \$24,999 |
|  |  | 2 | \$25,000–\$49,999 |
|  |  | 3 | \$50,000–\$79,999 |
|  |  | 4 | \$80,000–\$99,999 |
|  |  | 5 | \$100,000–\$149,999 |
|  |  | 6 | \$150,000–\$249,999 |
|  |  | 7 | ≥ \$250,000 |
| **Q66** | Highest level of education completed | 1 | Highschool |
|  |  | 2 | Apprenticeship / Trades |
|  |  | 3 | College / CEGEP |
|  |  | 4 | University degree |
|  |  | 5 | Masters |
|  |  | 6 | PhD |
|  |  | 7 | Medical / Paramedical |

------------------------------------------------------------------------

#### Dependent Variables (Q40–Q43)

All four dependent variables are **four-point Likert scale** items, coded as follows:

> 1 = Not at all\
> 2 = Not very\
> 3 = Somewhat\
> 4 = Very

For binary logistic regression models, responses were recoded as:\
`1/2 → 0 (Low)` and `3/4 → 1 (High)`.

| Variable | Question | Value | Meaning |
|:-----------------|:-------------------|:-----------------|:-----------------|
| **Q40** | *How knowledgeable are you about what artificial intelligence (AI) is?* | 1 | Not at all knowledgeable |
|  |  | 2 | Not very knowledgeable |
|  |  | 3 | Somewhat knowledgeable |
|  |  | 4 | Very knowledgeable |
| **Q41** | *How comfortable are you with AI being used as a tool in healthcare?* | 1 | Not at all comfortable |
|  |  | 2 | Not very comfortable |
|  |  | 3 | Somewhat comfortable |
|  |  | 4 | Very comfortable |
| **Q42** | *How comfortable are you with scientists using personal health data for AI research as long as informed consent has been provided by the patient?* | 1 | Not at all comfortable |
|  |  | 2 | Not very comfortable |
|  |  | 3 | Somewhat comfortable |
|  |  | 4 | Very comfortable |
| **Q43** | *How comfortable are you with scientists using personal health data for AI research without informed consent as long as it is deidentified?* | 1 | Not at all comfortable |
|  |  | 2 | Not very comfortable |
|  |  | 3 | Somewhat comfortable |
|  |  | 4 | Very comfortable |

------------------------------------------------------------------------

#### Missing Value Codes

| Type | Code | Description | Treatment |
|:-----------------|:-----------------|:-----------------|:-----------------|
| Valid responses | 1–4 | Substantive answers | Keep |
| “Other (specify)” | 96 | Respondent provided other answer | `NA` |
| “Prefer not to answer” / “Refused” | 97–98 | Non-response / refusal | `NA` |
| “Don’t know / Not sure” | 99 | Uncertain / unclear | `NA` |

------------------------------------------------------------------------

#### Variable Roles in Regression Models

| Variable | Type | Reference Group | Role in Model |
|:-----------------|:-----------------|:-----------------|:-----------------|
| **age_new** | Factor | 16–24 years | Control for age |
| **gender** | Factor | Male | Control for gender |
| **Q55 (income)** | Factor | \< \$24,999 | Control for income |
| **Q66 (education)** | Factor | Highschool | Control for education |
| **Q40–Q43** | Ordinal (1–4) or Binary (0–1) | — | Dependent variables |

------------------------------------------------------------------------

#### Quick Summary

The cleaned dataset includes four dependent variables (Q40–Q43) representing respondents’ knowledge and comfort levels toward AI, measured on a four-point Likert scale (1 = Not at all, 4 = Very).\
Independent variables include demographic controls: **age group (age_new)**, **gender**, **household income (Q55)**, and **education level (Q66)**.\
All responses coded as 96–99 were treated as missing values (NA) during the data cleaning process.

------------------------------------------------------------------------

#### Data Filtering Methods

There are **two filtering strategies** applied during data cleaning:

1.  **Standard filtering (available-case):**
    -   All four independent variables (age, income, education, gender) must have valid values.\
    -   Responses such as “Other (96)”, “Prefer not to answer (98)”, or “Don’t know (99)” are removed.\
    -   However, for questions **Q40–Q43**, all respondents are initially retained regardless of validity.\
    -   Under this rule, each question yields a different sample size, matching the values shown in Appendix A1 of the paper.
2.  **Strict filtering (complete-case):**
    -   Only keep respondents with valid responses for **all eight variables** (four independent + four dependent).\
    -   **Any record containing invalid** (non-1/2/3/4) answers in Q40–Q43 is **discarded**.\
    -   The resulting sample size is **9,198**, identical to that reported in the paper’s Table 1 (Section 3.1).

We will need to **discuss these two data filtering strategies** in the report to illustrate how they affect the descriptive and regression analyses.

The code implementing these steps is contained in `01_data_cleaning.R`.\
Input: `ATS2021 Dataset_Dataverse posting.RData`\
Output: `clean_data.rds`,`clean_data.csv`, `clean_data_strict.rds` , `clean_data_strict.csv` , `clean_summary.csv` , `clean_summary_strict.csv`.

------------------------------------------------------------------------

### 2.2. Replication of Table 1 (Descriptive Statistics)

We used the script `02_descriptives_table1_strict.R` to replicate the descriptive table in the paper.\
The alternative script `02_descriptives_table1.R` remains relevant for future exploration, as it corresponds to the *available-case* approach mentioned above.

-   Input: `clean_data_strict.rds`\
-   Output: `table1_combined_strict.html` under the `output/` directory.

------------------------------------------------------------------------

### 2.3. Replication of Appendix A1 Table (Regression Analysis)

This step involves two stages, handled by separate scripts as suggested by ChatGPT:

#### Step 1: Model estimation (`03_models_Q40_to_Q43.R`)

We constructed regression models for Q40–Q43 using the **`glm()`** function in R (generalized linear model).\
However, the paper’s authors used **Ordinal Logistic Regression**, while our implementation with `glm(..., family = binomial)` can only handle binary outcomes.

Hence, the four-level responses (1 = Not at all knowledgeable → 4 = Very knowledgeable) were dichotomized: - Responses **1–2 → 0 (Low knowledge)**\
- Responses **3–4 → 1 (High knowledge)**

By contrast, Kai implemented the proper **Ordinal Logistic Regression**, which directly handles ordered multi-category variables — and his approach aligns more precisely with the paper’s methodology.

#### Step 2: Table generation (`04_export_regression_tables.R`)

This script converts model outputs into formatted tables for comparison with Appendix A1.\
However, discrepancies remain: Kai’s regression results still differ from the published Appendix A1 table, indicating further investigation is required to pinpoint the cause.

------------------------------------------------------------------------

### 2.4. Replication of 3D Regression Figures (Fig. 1–4)

This will be implemented in `05_plots_Q40_to_Q43.R`.\
According to the paper, the regression modeling used for these figures is a **multivariate polynomial regression model**, and the visual rendering was originally completed using an Excel plugin.

It remains uncertain whether equivalent 3D visualization can be fully reproduced in R.

------------------------------------------------------------------------

## 3. Summary and Q&A Session

### 3.1. Current Achievements

-   ✅ Acquired the 2021 ATS health survey dataset.\
-   ✅ Completed data cleaning workflow and scripts.\
-   ✅ Successfully replicated descriptive statistics (Table 1).

### 3.2. Issues to Resolve

-   🔹 Understand the methodological differences between the two data filtering strategies.\
-   🔹 Compare and analyze the discrepancy between two modeling approaches (GLM vs. Ordinal Logistic).\
-   🔹 Identify why Kai’s regression results differ from the paper’s Appendix A1 table.\
-   🔹 Optimize variable naming and file output structure.

### 3.3. Next Steps

-   🔸 Continue replication of regression figures (Q40–Q43) using R or equivalent tools.\
-   🔸 Discuss potential **Project Extension directions**, i.e., how the analysis can be expanded beyond replication.

------------------------------------------------------------------------

**Repository:** <https://github.com/CorelessXeon/MSE609-Group11-Project>\
**Prepared by:** *Gary (Wentao Zang)*\
**Course:** *MSE 609 – Quantitative Data Analysis*\
**Date:** *Midterm Notes – November 10, 2025*
