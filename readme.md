 AI Healthcare Comfort Evaluation

A reproducible R pipeline for analyzing attitudes toward digital health services across multiple Canadian survey waves. The project cleans raw survey exports, builds descriptive summaries, trains ordinal logistic regression models, and generates 3D visualizations that highlight comfort levels with virtual care.

Across the project, we validated the key findings of [Li et al. (2025)](https://www.sciencedirect.com/science/article/pii/S0033350624004918) by replicating the core descriptive statistics, ordinal logistic regression, and 3D surfaces (Table 1, Appendix A1, Figures 1–4). That work surfaced the only notable data nuance—age bins in the source files cannot perfectly match the paper’s reported groups—while confirming that the original demographic patterns of AI knowledge and comfort hold up under independent reproduction.

We then extended the analysis to the [2023](https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/5C7HSO) and [2024](https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/MI0HZP) Canadian Digital Health Survey waves to add a longitudinal lens. The new runs show how comfort and knowledge have evolved (with many surfaces flattening), where demographic gradients remain stable (e.g., gender gaps in comfort, higher comfort among higher income/education groups), and where fresh patterns emerge (e.g., 2024 seniors reporting unexpectedly high comfort with de-identified data use). Along the way, we documented filtering decisions, reference category alignment, and end-to-end reproducibility steps to make future reruns reliable and transparent.

## Why this repo?
- **End-to-end pipeline**: One command runs cleaning, modeling, and visualization for the 2021 ATS dataset plus 2023/2024 CDHS waves.
- **Reproducible environment**: `renv` lockfile pins all R dependencies for consistent results.
- **Ready-made outputs**: Tables, plots, and model objects are written to `artifacts/` for quick review.

## Repository layout
```
AI-Healthcare-Comfort-Evaluation/
├── R/                      # Analysis scripts (00–07) and shared utilities
│   └── utils/common_utils.R
├── data/
│   └── data_raw/           # Raw survey files (not included)
├── artifacts/              # Generated tables, plots, and models
├── docs/examples/          # Sample plots
├── run_all.R               # Entry point that orchestrates the full pipeline
├── renv.lock               # Locked R package versions
└── readme.md               # Project guide (this file)
```
## Full report
For methodological details, interpretation of results, and discussion beyond what is shown in the generated tables and figures, please see the accompanying project report `docs/MSE_609_Project_Report_Group_11.pdf`

The report provides the full narrative context, assumptions, and limitations underlying the analyses implemented in this repository.

## Quick start
1. **Clone the repository**
   ```bash
   git clone https://github.com/CorelessXeon/MSE609-Group11-Project.git
   cd MSE609-Group11-Project
   ```
2. **Restore the R environment**
   ```r
   renv::restore()
   ```
3. **Run the full pipeline**
   ```r
   source("run_all.R")
   ```
   `run_all.R` executes scripts `00`–`07` sequentially and writes results to `artifacts/`.

## What each script does
- **00_project_setup.R** – Installs missing packages, loads libraries, creates output folders, and sources `utils/common_utils.R`.
- **01_data_cleaning.R** – Cleans the ATS 2021 dataset and saves `cleaned_dataset.rds`.
- **02_descriptives_table1.R** – Builds Table 1 for the ATS 2021 replication using strict filtering of Q40–Q43 responses.
- **03_ordinal_logistic_regression.R** – Fits ordinal logistic regression models for the ATS 2021 dataset and exports model objects.
- **04_plots_Q40_to_Q43.R** – Creates interactive 3D plots for Q40–Q43 based on the 2021 models.
- **05_different_dataset.R** – Cleans CDHS 2023 and CDHS 2024 survey data and saves `cleaned_dataset_2023.rds` and `cleaned_dataset_2024.rds`.
- **06_plots_different_dataset.R** – Generates 3D visualizations for the 2023 and 2024 datasets.
- **07_extension_tables.R** – Produces distributive and regression coefficient tables for 2023/2024 and writes them to `artifacts/tables/` and `artifacts/models/`.

## Example outputs
Preview figures in `docs/examples/`, such as the Q40 2021 knowledge about AI plot:

![Q40 2021 plot](docs/examples/q40_2021.png)

## Data access
Raw Canadian Digital Health Survey files are **not included** due to licensing. you dan download the dataset directly from [borealisdata.ca](https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/CEYG42). Once obtained, place the files in `data/data_raw/` follow the naming noted in `data/data_raw/readme.md` and rerun the pipeline.

## Contributing
Contributions that improve reproducibility, documentation, or analysis robustness are welcome. Please open an issue or pull request with proposed changes or reach out one of the contributors.
