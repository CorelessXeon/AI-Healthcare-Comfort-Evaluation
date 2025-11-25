# Project Overview

This repository contains a complete and reproducible pipeline for replicating and extending the analysis based on the **ATS 2021 dataset**, as well as processing and visualizing **ATS 2023** and **ATS 2024** survey datasets. The project includes:

-   Data cleaning for all datasets (2021, 2023, 2024)
-   Construction of descriptive statistics (Table 1–style outputs)
-   Ordinal logistic regression modeling
-   3D visualizations of predicted relationships
-   Full pipeline automation through `run_all.R`
-   Reproducible computational environment via **renv**

All results (tables, figures, and model outputs) are written to the `artifacts/` directory.

```         
MSE609-Group11-Project/
├── R/
│   ├── 00_project_setup.R
│   ├── 01_data_cleaning.R
│   ├── 02_descriptives_table1.R
│   ├── 03_ordinal_logistic_regression.R
│   ├── 04_plots_Q40_to_Q43.R
│   ├── 05_different_dataset.R
│   └── 06_plots_different_dataset.R
│   └── utils/
│       └── common_utils.R
│
├── data/
│   ├── data_raw/                # Raw 2021 / 2023 / 2024 files (not tracked)
│   ├── cleaned_dataset.rds
│   ├── cleaned_dataset_strict.rds
│   ├── cleaned_dataset_2023.rds
│   └── cleaned_dataset_2024.rds
│
├── artifacts/
│   ├── tables/                  # Table 1 and other tabular outputs
│   ├── plots/                   # 3D figures and visualizations
│   ├── models/                  # Regression outputs
│   └── logs/
│
├── docs/
│   └── images/                  # Figures included in README
│
├── renv/                        # renv environment library
├── renv.lock                    # Locked package versions
├── run_all.R                    # Main pipeline executor
├── MSE609-Group11-Project.Rproj
└── README.md
```

# Quick Start

## 1. Clone the Repository

```         
git clone <https://github.com/CorelessXeon/MSE609-Group11-Project.git>
```

## 2. Restore the R Environment

```         
renv::restore()
```

## 3. Run the Full Pipeline

```         
source("run_all.R")
```

# Analysis Pipeline

## 4.1 00_project_setup.R

Initializes the environment, installs required packages, loads libraries, creates folders, and sources utilities.

## 4.2 01_data_cleaning.R

Cleans the ATS 2021 dataset.

**Output:**\
- cleaned_dataset.rds

## 4.3 02_descriptives_table1.R

Generates: - cleaned_dataset_strict.rds\
- table1_replication.csv\
- table1_replication.html

## 4.4 03_ordinal_logistic_regression.R

Uses cleaned_dataset.rds to fit ordinal logistic regression models.

## 4.5 04_plots_Q40_to_Q43.R

Uses cleaned_dataset.rds to generate 3D plots.

## 4.6 05_different_dataset.R

Cleans ATS 2023 and 2024 datasets: - cleaned_dataset_2023.rds - cleaned_dataset_2024.rds

## 4.7 06_plots_different_dataset.R

Produces 3D plots for 2023 and 2024 datasets.

# Script Dependency Graph

Raw 2021 Data ↓ 01_data_cleaning.R ↓ cleaned_dataset.rds ───→ 03_ordinal_logistic_regression.R → 04_plots_Q40_to_Q43.R ↓ 02_descriptives_table1.R ↓ cleaned_dataset_strict.rds → Table 1 outputs

Raw 2023 Data → 05_different_dataset.R → cleaned_dataset_2023.rds → 06_plots_different_dataset.R Raw 2024 Data → 05_different_dataset.R → cleaned_dataset_2024.rds → 06_plots_different_dataset.R

# Example Outputs

Images may be added under `docs/images/` and referenced using:

# License

(To be added.)
