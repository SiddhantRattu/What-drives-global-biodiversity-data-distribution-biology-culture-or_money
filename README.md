# 🌍 Global Biodiversity Data Distribution: Biology, Culture, or Money?

> **Author**: **Siddhant Rattu**  
> **Environment**: R (**renv** version control, R **4.6.0**)  
> **Repository**: `What-drives-global-biodiversity-data-distribution-biology-culture-or_money`

---

## 📌 Executive Summary

Our understanding of global species distribution is fundamentally shaped by public scientific repositories—most notably the **Global Biodiversity Information Facility ([GBIF](https://www.gbif.org/))**. While digital species occurrence records exceed two billion entries worldwide, geographic sampling density is severely skewed across national borders.

This research codebase, authored by **Siddhant Rattu**, conducts a macro-level quantitative investigation across **200+ countries** to evaluate the primary structural drivers of global biodiversity data distribution by testing three core hypotheses:

1. **🌿 The Biological Hypothesis**: Biodiversity data volume is driven by intrinsic ecological richness (biome diversity, species endemicity, protected area coverage %, and tropical latitude).
2. **🏛️ The Cultural & Historical Hypothesis**: Biodiversity data volume is shaped by colonial history, official language, and open-knowledge governance policies.
3. **💵 The Financial & Infrastructure Hypothesis**: Biodiversity data volume is constrained by national wealth (GDP, GDP per capita), R&D intensity (% of GDP), absolute research spending, and active scientific workforce density.

---

> [!IMPORTANT]
> ### 💡 Key Methodological Considerations & Data Issues
>
> The following **6 critical data and statistical issues** must be accounted for in the empirical modeling methodology:
>
> 1. 📊 **Extreme Right-Skewness of GBIF Records**: Raw species occurrence counts span six orders of magnitude ($10^2$ to $10^8$). Linear models on raw counts violate normality assumptions. **Solution**: Apply logarithmic transformations ($\log_{10}(\text{GBIF Records})$) or fit count-based GLMs (Negative Binomial / Poisson).
> 2. 📐 **Land Area Confounding (Missing Offset Term)**: Larger countries naturally accumulate more records simply due to surface area (`area_total_km2`). **Solution**: Incorporate $\log(\text{area\_total\_km2})$ as an **offset term** in regression models or compute sampling density ($\text{Records} / \text{km}^2$).
> 3. 🌐 **The Latitudinal Data Inversion Bias**: Species richness peaks in tropical low-latitude countries, whereas GBIF record volume peaks in temperate high-latitude northern nations. **Solution**: Include interaction terms ($\text{Latitude} \times \text{GDP}$) to decouple sampling effort from biological richness.
> 4. 🔀 **Multicollinearity in Economic Predictors**: Socio-economic indicators (`wb_gdp_total`, `wb_gdp_per_capita`, `wb_research_gdp_pct`, `wb_total_researchers_count`, `hdi_proxy_gdp`) exhibit high pairwise correlations ($r > 0.70$). **Solution**: Script [`scripts/11_final_data_explorations.R`](file:///C:/Users/Siddhant/User/Documents/GitHub/What-drives-global-biodiversity-data-distribution-biology-culture-or_money/scripts/11_final_data_explorations.R) applies `caret::findCorrelation(cutoff = 0.6)` to prune collinear predictors and prevent Variance Inflation (VIF > 5).
> 5. 🗺️ **Spatial Autocorrelation**: Neighboring countries share ecological biomes and geopolitical history, inducing spatial autocorrelation in model residuals. **Solution**: Compute **Moran's I** on spatial residuals or fit Spatial Error / Spatial Lag Models.
> 6. 📉 **Sample Size Reduction via Missing Data**: Small island developing states (SIDS) frequently lack reported World Bank R&D indicators (`wb_research_gdp_pct`, `foreign_aid_oda`). Complete-case analysis reduces sample size from $217$ to $\sim 140$ nations. **Solution**: Discuss multiple imputation or report complete-case sensitivity analyses.

---

## 📂 Project Architecture

```
What-drives-global-biodiversity-data-distribution-biology-culture-or_money/
├── 📄 README.md                                                 # Master documentation (Author: Siddhant Rattu)
├── ⚙️ What-drives-global-biodiversity-data-distribution...Rproj # RStudio Project configuration
├── 📦 renv.lock & renv/                                         # Reproducible virtual environment
│
├── 📁 data/                                                     # DATA ASSETS
│   ├── 📁 raw/                                               # Immutable raw datasets (COLDAT, Forest Data, WDI)
│   ├── 📁 processed/                                         # Clean intermediate CSVs (Final_DATASET.csv)
│   └── 📁 results/                                           # Final Results (Final_Dataset_Complete.xlsx & Final_DATASET.csv)
│
├── 📁 docs/                                                     # SCHEMAS & METADATA
│   ├── column_definitions.md                                 # Human-readable variable schema & Protected Area %
│   └── data_dictionary.csv                                   # Machine-readable data dictionary (24 indicators)
│
├── 📁 scripts/                                                  # SEQUENTIAL R PIPELINE (**00** - **11**)
│   ├── 00_setup_libraries.R                                  # Environment & package initialization
│   ├── 01_data_cleaning.R                                    # Ingestion & ISO3 country harmonization
│   ├── 02_check_missing_data.R                               # Quality assurance NA audit
│   ├── 03_outlier_detection.R                                # Multivariate Z-score outlier checks (>3 SD)
│   ├── 04_exploratory_plots.R                                # Exploratory histograms & EDA
│   ├── 05_standardize_variables_and_pa_pct.R                 # Protected Area % & unified `_pct` criteria
│   ├── 06_calculate_biome_diversity.R                        # Ecoregion & protected area calculation (`sf`)
│   ├── 07_gbif_record_fetcher.R                              # Live GBIF API species record retrieval (`rgbif`)
│   ├── 08_geoboundaries_fetcher.R                            # Administrative boundary metadata retrieval
│   ├── 09_worldbank_wdi_fetcher.R                            # World Bank API data retrieval (**2005-2025**)
│   ├── 10_final_output_aggregation.R                         # Data synthesis & Excel report builder
│   └── 11_final_data_explorations.R                         # Correlation matrices & caret correlation pruning
│
└── 📁 drafts/                                                   # LEGACY ARCHIVES
    └── code_archive/                                         # Archived code drafts (purged of redundant raw files)
```

---

## 🛠️ Data Pipeline Execution Order

To replicate the analytical workflow, execute the R scripts in `scripts/` in numerical sequence (**00** through **11**):

| Step | Script | Function & Purpose | Primary Output |
| :--- | :--- | :--- | :--- |
| **00** | **`00_setup_libraries.R`** | Environment initialization and package loading (`tidyverse`, `sf`, `WDI`, `rgbif`, `janitor`, `openxlsx`). | Environment Ready |
| **01** | **`01_data_cleaning.R`** | Standardizes country names to ISO3C codes, categorizes colonial history, and bridges auxiliary datasets. | `biodiversity_clean_v1.csv` |
| **02** | **`02_check_missing_data.R`** | Audits dataset for missing values (NAs) and logs quality assurance metrics. | QA Audit Log |
| **03** | **`03_outlier_detection.R`** | Identifies multivariate Z-score outliers (>3 SD) across log-scaled demographic metrics. | Outlier Validation |
| **04** | **`04_exploratory_plots.R`** | Generates initial distribution histograms and exploratory summary statistics. | Distribution Plots |
| **05** | **`05_standardize_variables_and_pa_pct.R`** | Base R script converting Protected Area (PA) to percentage (`terrestrial_protected_area_pct`) and unifying `_pct` criteria. | `data/results/Final_DATASET.csv` |
| **06** | **`06_calculate_biome_diversity.R`** | Computes unique ecoregions (`Ecoregions2017`) and protected area coverage % via `sf` spatial overlays (`ESRI:54009`). | Spatial Feature Metrics |
| **07** | **`07_gbif_record_fetcher.R`** | Queries GBIF API (`rgbif`) to fetch live species occurrence counts per country. | `GBIF_records.csv` |
| **08** | **`08_geoboundaries_fetcher.R`** | Fetches standardized administrative boundary metadata from the GeoBoundaries API. | Boundary Metadata |
| **09** | **`09_worldbank_wdi_fetcher.R`** | Queries World Bank WDI API for indicators spanning **2005** to **2025** (GDP, R&D %, Urbanization %). | Economic Indicators |
| **10** | **`10_final_output_aggregation.R`** | Aggregates all indicators into a color-coded Excel workbook ([`data/results/Final_Dataset_Complete.xlsx`](file:///C:/Users/Siddhant/User/Documents/GitHub/What-drives-global-biodiversity-data-distribution-biology-culture-or_money/data/results/Final_Dataset_Complete.xlsx)). | `Final_Dataset_Complete.xlsx` |
| **11** | **`11_final_data_explorations.R`** | Evaluates log-log relationships, correlation matrices, and prunes multicollinearity via `caret::findCorrelation(cutoff = 0.6)`. | Model Diagnostics |

---

## 📊 Standardized Variable Schema (`_pct` Criteria)

All percentage indicators strictly adhere to the unified **`_pct`** suffix convention:

| Category | Database Variable Name | Description | Type | Expected Effect | Does it follow expectation? |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **Money** | `wb_gdp_per_capita` | GDP per capita in current USD | Continuous | Countries with larger GDP per capita have more records | **YES** ($r > 0.55$) |
| **Money** | `wb_gdp_total` | Total GDP in current USD | Continuous | Countries with larger total GDP have more records | **YES** ($r > 0.60$) |
| **Money** | `wb_research_gdp_pct` | R&D expenditure as % of GDP | Continuous (`_pct`) | Countries investing more in R&D have more records | **YES** |
| **Money** | `wb_researchers_per_million_people` | Researchers per million inhabitants | Continuous | Countries with more researchers produce more records | **YES** |
| **Money** | `foreign_aid_oda` | Biodiversity aid received (USD) | Continuous | Countries receiving more biodiversity aid have more records | **PARTIALLY** |
| **Money** | `hdi_proxy_gdp` / `hdi_proxy_life` | HDI proxies (Living & Health) | Continuous | Countries with higher HDI have more records | **YES** |
| **Culture** | `main_language` | Most widely spoken language | Categorical | Language affects GBIF records | **YES** |
| **Culture** | `is_english_main` | English primary language indicator | Binary | English main language countries have more records | **YES** |
| **Culture** | `main_religion` / `main_religion_pct` | Main religion & adherence % | Categorical / Continuous | Religion affects GBIF records | **NO** |
| **Culture** | `atheists_non_religious_pct` | Secular / Atheist population % | Continuous (`_pct`) | Higher non-religiosity correlates with scientific data | **PARTIALLY** |
| **Culture** | `ever_colonized` / `colonizer` | Colonial status & colonizing power | Categorical | Colonizers have more records; colonized face bias | **YES** |
| **Culture** | `open_knowledge_score` | Open Knowledge Index score | Continuous | Higher open knowledge scores lead to more records | **YES** |
| **Culture** | `is_main_english_spanish_arabic` | Government / major language classification | Categorical | Open / democratic systems have more records | **YES** |
| **Biology** | `n_records_gbif` | Registered GBIF occurrence records | Response Variable | Target outcome variable | **Target Outcome** |
| **Biology** | `biome_diversity_count` | Count of unique ecoregions | Continuous | Countries with more biomes have more records | **YES** |
| **Biology** | `terrestrial_protected_area_pct` | Terrestrial Protected Area % | Continuous (`_pct`) | More protected area % leads to more records | **YES** |
| **Biology** | `forest_cover_pct` | Forest area % of total land area | Continuous (`_pct`) | Higher forest cover leads to more records | **YES** |
| **Biology** | `mean_latitude` | Absolute centroid latitude (degrees) | Continuous | Tropical lower latitude nations have more records | **NO** (Sampling Bias) |
| **Biology** | `endemic_tetrapods_total` | Count of endemic species | Continuous | More endemic species attract more research | **PARTIALLY** |
| **Structure** | `wb_population` | Total national population | Continuous | Larger populations provide more observers | **YES** |
| **Structure** | `area_total_km2` | Total surface area ($\text{km}^2$) | Continuous | Larger countries have more habitats and records | **YES** |
| **Structure** | `wb_population_density` | Inhabitants per $\text{km}^2$ | Continuous | Density correlates with observer availability | **PARTIALLY** |
| **Structure** | `wb_urbanisation_pct_urban` | Urban population % | Continuous (`_pct`) | Urbanized countries have more records (citizen science) | **YES** |
| **Structure** | `mean_latitude` / `climate_zone` | Köppen climate classification proxy | Categorical | Temperate zone countries contribute more records | **YES** |

---

## 🚀 Reproduction Instructions

To reproduce the analysis on your local machine:

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/SiddhantRattu/What-drives-global-biodiversity-data-distribution-biology-culture-or_money.git
   cd What-drives-global-biodiversity-data-distribution-biology-culture-or_money
   ```

2. **Restore Package Environment**:
   ```r
   renv::restore()
   ```

3. **Execute the Standardized Pipeline**:
   ```r
   source("scripts/00_setup_libraries.R")
   source("scripts/05_standardize_variables_and_pa_pct.R")
   source("scripts/10_final_output_aggregation.R")
   source("scripts/11_final_data_explorations.R")
   ```
