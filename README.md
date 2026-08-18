# 🌍 Global Biodiversity Data Distribution: Biology, Culture, or Money?

> **Author**: **Siddhant Rattu**  
> **Last Updated**: **August 2026**  
> **Environment**: R (**renv** version control)

---

## 📌 Executive Summary

Our understanding of global species distribution is heavily shaped by our ability to observe, record, and share biodiversity data on public repositories like the **Global Biodiversity Information Facility ([GBIF](https://www.gbif.org/))**. While access to digital occurrence records is greater than ever, data volume varies drastically across nations.

This research project investigates the primary drivers behind global biodiversity data disparities by testing three central hypotheses:
1. **🌿 Biology**: Is data volume driven by intrinsic species richness, biome diversity, protected area coverage, or geographic latitude?
2. **🏛️ Culture & History**: Is data volume shaped by colonial legacies, dominant languages, or open-knowledge governance policies?
3. **💵 Money & Infrastructure**: Is data volume constrained by national GDP, R&D intensity (% of GDP), total research expenditure, or scientific workforce density?

---

## 📂 Project Architecture

```
What-drives-global-biodiversity-data-distribution-biology-culture-or_money/
├── README.md                                                 # Project documentation & guide
├── What-drives-global-biodiversity-data-distribution...Rproj # RStudio Project file
├── renv.lock & renv/                                         # Dependency environment lockfile
├── docs/                                                     # Standardized schemas & metadata
│   ├── column_definitions.md                                 # Standardized variable documentation
│   └── data_dictionary.csv                                   # Machine-readable data dictionary
├── data/
│   ├── raw/                                                  # Immutable source data (CSVs, Shapefiles)
│   ├── processed/                                            # Processed intermediate datasets
│   └── results/                                              # Final Outputs (Final_Dataset_Complete.xlsx & Final_DATASET.csv)
└── scripts/                                                  # Sequential R analytical pipeline
    ├── 00_setup_libraries.R                                  # Environment & package initialization
    ├── 01_data_cleaning.R                                    # Ingestion & ISO3 country harmonization
    ├── 02_check_missing_data.R                               # Quality assurance & NA audit
    ├── 03_outlier_detection.R                                # Multivariate Z-score outlier checks (>3 SD)
    ├── 04_exploratory_plots.R                                # Distribution histograms & EDA
    ├── 05_standardize_variables_and_pa_pct.R                 # Protected Area % & unified `_pct` criteria
    ├── 06_calculate_biome_diversity.R                        # Spatial ecoregion & protected area calculation
    ├── 07_gbif_record_fetcher.R                              # Live GBIF API species record retrieval
    ├── 08_geoboundaries_fetcher.R                            # Administrative boundary metadata retrieval
    ├── 09_worldbank_wdi_fetcher.R                            # World Bank API data retrieval (**2005-2025**)
    ├── 10_final_output_aggregation.R                         # Data synthesis & Excel report builder
    └── 11_final_data_explorations.R                         # Correlation matrices & multicollinearity checks
```

---

## 🛠️ Data Pipeline Execution Order

To reproduce the analysis from raw data ingestion to final reporting, run the scripts in `scripts/` in numerical order (**00** through **11**):

| Script | Purpose & Description | Output / Target |
| :--- | :--- | :--- |
| **`00_setup_libraries.R`** | Installs and loads required R packages (`tidyverse`, `sf`, `WDI`, `rgbif`, `janitor`, `openxlsx`, `here`). | Environment Ready |
| **`01_data_cleaning.R`** | Standardizes country names to ISO3C codes, categorizes colonial history, and bridges auxiliary datasets. | `biodiversity_clean_v1.csv` |
| **`02_check_missing_data.R`** | Audits dataset for missing values (NAs) and generates QA logs. | QA Audit Log |
| **`03_outlier_detection.R`** | Identifies multivariate outliers (>3 SD) across log-scaled demographic metrics. | Outlier Validation |
| **`04_exploratory_plots.R`** | Generates initial distribution histograms and exploratory summary statistics. | Distribution Plots |
| **`05_standardize_variables_and_pa_pct.R`** | Converts Protected Area (PA) to percentage (`terrestrial_protected_area_pct`) and unifies `_pct` column criteria. | `Final_DATASET.csv` |
| **`06_calculate_biome_diversity.R`** | Computes unique ecoregions (`Ecoregions2017`) and protected area coverage % via `sf` spatial overlays. | Spatial Feature Metrics |
| **`07_gbif_record_fetcher.R`** | Queries the GBIF API (`rgbif`) to fetch updated occurrence counts per country. | `GBIF_records.csv` |
| **`08_geoboundaries_fetcher.R`** | Fetches standardized administrative boundary metadata from the GeoBoundaries API. | Boundary Metadata |
| **`09_worldbank_wdi_fetcher.R`** | Queries World Bank WDI API for indicators spanning **2005** to **2025** (GDP, R&D %, Urbanization %). | Economic Indicators |
| **`10_final_output_aggregation.R`** | Aggregates all indicators into a color-coded Excel report (`Final_Dataset_Complete.xlsx`). | `Final_Dataset_Complete.xlsx` |
| **`11_final_data_explorations.R`** | Evaluates log-log relationships, correlation matrices, and prunes multicollinearity via `caret::findCorrelation(cutoff = 0.6)`. | Model Data Diagnostics |

---

## 📊 Standardized Variable Schema (`_pct` Criteria)

All percentage variables strictly follow the **`_pct`** suffix criteria:

| Category | Database Variable Name | Description | Standardized Type |
| :--- | :--- | :--- | :--- |
| **Money** | `wb_gdp_per_capita` | GDP per capita in current USD | Continuous |
| **Money** | `wb_gdp_total` | Total GDP in current USD | Continuous |
| **Money** | `wb_research_gdp_pct` | R&D expenditure as % of GDP | Continuous (`_pct`) |
| **Money** | `wb_researchers_per_million_people` | Researchers per million population | Continuous |
| **Money** | `wb_total_research_spending_usd` | Total national R&D spending in USD | Continuous |
| **Money** | `wb_total_researchers_count` | Absolute count of researchers | Continuous |
| **Money** | `foreign_aid_oda` | Biodiversity aid received (USD) | Continuous |
| **Culture** | `main_language` | Primary national language | Categorical |
| **Culture** | `is_english_main` | English primary language indicator | Binary |
| **Culture** | `main_religion_pct` | Adherence % to main religion | Continuous (`_pct`) |
| **Culture** | `atheists_non_religious_pct` | Secular / Atheist population % | Continuous (`_pct`) |
| **Culture** | `ever_colonized` / `colonizer` | Colonial status & colonizing power | Categorical |
| **Biology** | `n_records_gbif` | Total registered GBIF occurrence records | Response Variable |
| **Biology** | `biome_diversity_count` | Count of unique terrestrial biomes | Continuous |
| **Biology** | `terrestrial_protected_area_pct` | Terrestrial Protected Area % | Continuous (`_pct`) |
| **Biology** | `forest_cover_pct` | Forest area % of total land | Continuous (`_pct`) |
| **Biology** | `mean_latitude` | Absolute centroid latitude (degrees) | Continuous |
| **Biology** | `endemic_tetrapods_total` | Endemic species count | Continuous |
| **Structure** | `wb_population` | Total national population | Continuous |
| **Structure** | `area_total_km2` | Total surface area ($\text{km}^2$) | Continuous |
| **Structure** | `wb_population_density` | Inhabitants per $\text{km}^2$ | Continuous |
| **Structure** | `wb_urbanisation_pct_urban` | Urban population % | Continuous (`_pct`) |

---

## 🚀 Getting Started

### Prerequisites & Environment Setup
This repository uses [`renv`](https://rstudio.github.io/renv/) to guarantee reproducible package installation.

1. **Clone the repository**:
   ```bash
   git clone https://github.com/SiddhantRattu/What-drives-global-biodiversity-data-distribution-biology-culture-or_money.git
   cd What-drives-global-biodiversity-data-distribution-biology-culture-or_money
   ```

2. **Restore the environment in R**:
   ```r
   renv::restore()
   ```

3. **Run the complete pipeline**:
   ```r
   source("scripts/00_setup_libraries.R")
   source("scripts/05_standardize_variables_and_pa_pct.R")
   source("scripts/10_final_output_aggregation.R")
   source("scripts/11_final_data_explorations.R")
   ```

---

## 👤 Author & Attribution

* **Author**: **Siddhant Rattu**
* **Project**: Global Biodiversity Data Distribution Research (**August 2026**)
* **Repository**: [GitHub Workspace](https://github.com/SiddhantRattu/What-drives-global-biodiversity-data-distribution-biology-culture-or_money)
