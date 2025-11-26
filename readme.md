# Project Overview

This repository contains a complete and reproducible pipeline for replicating and extending the analysis based on the **ATS 2021 dataset**, as well as processing and visualizing **CDHS 2023** and **CDHS 2024** datasets. The project includes:

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
│   ├── data_raw/                # Raw 2021 / 2023 / 2024 files
│   ├── cleaned_dataset.rds
│   ├── cleaned_dataset_strict.rds
│   ├── cleaned_dataset_2023.rds
│   └── cleaned_dataset_2024.rds
│
├── artifacts/
│   ├── tables/                  # Table 1 and other tabular outputs
│   ├── plots/                   # 3D figures and visualizations
│   └── models/                  # Regression outputs
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
git clone https://github.com/CorelessXeon/MSE609-Group11-Project.git
cd MSE609-Group11-Project
```

## 2. Restore the R Environment

```         
renv::restore()
```

## 3. Run the Full Pipeline

```         
source("run_all.R")
```

`run_all.R` will execute scripts `00` through `06` in sequence.

------------------------------------------------------------------------

# Analysis Pipeline

## 4.1 `00_project_setup.R`

**Purpose**

Initializes the project environment, installs missing R packages defined in the `needed` vector, loads the required libraries, creates necessary directories under `artifacts/`, and sources utility functions from `utils/common_utils.R`.

## 4.2 `01_data_cleaning.R`

**Purpose**

Cleans the ATS 2021 dataset and produces a fully processed analytical dataset.

**Input**

`data/data_raw/ATS2021 Dataset_Dataverse posting.sav`

**Output**

`data/cleaned_dataset.rds`

## 4.3 `02_descriptives_table1.R`

**Purpose** Produces the descriptive Table 1 used in the ATS 2021 replication.

**Strict filtering logic** This script performs an additional cleaning step not used elsewhere:

All observations with any **NA** in **Q40–Q43** are **removed**.

This yields:

`data/cleaned_dataset_strict.rds`

**Input**

`data/cleaned_dataset.rds`

**Output**

Written to `artifacts/tables/`:

`table1_replication.csv`

`table1_replication.html`

## 4.4 `03_ordinal_logistic_regression.R`

**Purpose**

Fits ordinal logistic regression models for outcomes Q40–Q43 using the cleaned ATS 2021 dataset.

**Input**

-   `data/cleaned_dataset.rds`

**Output**

Written to `artifacts/models/`:

-   Model objects (`q40_olr_model.rds` - `q43_olr_model.rds`)
-   Regression coefficient tables (`appendix_table_1A_replication.csv` / `appendix_table_1A_replication.html`)

**Notes**

Models use `MASS::polr` or equivalent ordinal regression implementations.

------------------------------------------------------------------------

## 4.5 `04_plots_Q40_to_Q43.R`

**Purpose**

Generates 3D visualizations of model-based response surfaces for Q40–Q43.

### Input

-   `data/cleaned_dataset.rds`

**Output**

Written to `artifacts/plots/`:

-   3D rendered figures (`Q40.html` - `Q43.html`)

**Notes**

Uses plotting utilities defined in `utils/common_utils.R`.

------------------------------------------------------------------------

## 4.6 `05_different_dataset.R`

### Purpose

Cleans the CDHS 2023 and CDHS 2024 datasets.

### Input

-   `data/data_raw/CDHS2023_Dataset_Dataverse-posting.sav`
-   `data/data_raw/Infoway CDHS 2024 SPSS Raw Data_for Dataverse.sav`

### Output

-   `data/cleaned_dataset_2023.rds`
-   `data/cleaned_dataset_2024.rds`

------------------------------------------------------------------------

## 4.7 `06_plots_different_dataset.R`

### Purpose

Generates 3D visualizations for the cleaned CDHS 2023 and CDHS 2024 datasets.

### Input

-   `cleaned_dataset_2023.rds`
-   `cleaned_dataset_2024.rds`

### Output

Written to `artifacts/plots/`:

-   3D plots for CDHS 2023 (`Q40_2023.html` - `Q43_2023.html`)
-   3D plots for CDHS 2024 (`Q40_2024.html` - `Q43_2024.html`)

------------------------------------------------------------------------

# Script Dependency Graph

```         
          Raw ATS 2021 Data
                  |
                  v
        01_data_cleaning.R
                  |
                  v
       cleaned_dataset.rds
         |        |         \
         |        |          \
         |        |           \
         |        |            \
         v        v             v
 02_descriptives_table1.R   03_ordinal_logistic_regression.R
         |                       |
         v                       |
cleaned_dataset_strict.rds       |
         |                       |
         v                       v
  (Table 1 outputs)      04_plots_Q40_to_Q43.R
                                 |
                                 v
                         (3D model plots)


          Raw ATS 2023 Data             Raw ATS 2024 Data
                 |                               |
                 v                               v
        05_different_dataset.R (cleaning both datasets)
                 |                               |
                 v                               v
 cleaned_dataset_2023.rds          cleaned_dataset_2024.rds
                 \                               /
                  \                             /
                   \                           /
                    v                         v
                    06_plots_different_dataset.R
                             |
                             v
                     (3D plots for 2023/2024)
```

# Example Outputs

Example figures and tables used in the analysis are provided in:

![](docs/images/example_plot.png)

# License

(To be added.)
