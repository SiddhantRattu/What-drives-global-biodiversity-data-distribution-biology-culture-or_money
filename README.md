# Global Biodiversity Data Analysis

Our understanding of global biodiversity distribution is shaped by our ability to observe, identify, and share species data. While access to biodiversity records (particularly through public repositories like GBIF) is greater than ever, the volume of data varies widely across countries. This disparity may partly reflect differences in taxonomic richness, but it is also influenced by each country’s cultural history, or its capacity to fund fieldwork, manage data, and support open access. Yet, the factors driving these differences remain understudied.

---

## 🗺️ The Map Room: Navigation

Welcome to the project. Think of this repository as a data observatory, designed to ingest raw global statistics and refine them into clear insights about biodiversity, economics, and history. Here is your map to the facility:

*   **`scripts/`**: The Engine Room. Where the analysis happens.
*   **`data/`**: The Vault. Where raw inputs and refined outputs are stored.
*   **`docs/`**: The Blueprints. Definitions and dictionaries.
*   **`output/`**: The Gallery. Visualizations and tables ready for display.
*   **`renv/`**: The Life Support System. Ensures the R environment is consistent.
*   **`drafts/`**: The Scrapyard. Old code and experiments.

---

## ⚙️ The Engine Room: Scripts

The analysis pipeline is divided into two main sectors: the **Standard Protocol** (ordered for clarity) and the **Heavy Machinery** (specialized, comprehensive tools).

### 🔹 The Standard Protocol (Core Pipeline)
These scripts are numbered to guide you through the fundamental cleaning and exploration process.

*   **`00_setup_libraries.R`**
    *   *The Toolkit.* Installs and loads all necessary R packages (`tidyverse`, `WDI`, `janitor`, etc.) to ensure the environment is ready for work.
*   **`01_data_cleaning.R`**
    *   *The Filter.* The first line of defense. It ingests the master dataset, harmonizes country names (unifying "Congo, Rep." and friends), processes colonial history flags, and merges everything into a clean CSV (`biodiversity_clean_v1.csv`).
*   **`02_check_missing_data.R`**
    *   *The Inspector.* Scans the clean data for holes. It counts missing values (NAs) and logs the report to `docs/missing_values_log.txt`.
*   **`03_outlier_detection.R`**
    *   *The Radar.* Scans for anomalies. It plots Area vs. Population to visually spot outliers and calculates Z-scores to flag data points that deviate significantly from the norm.
*   **`04_exploratory_plots.R`**
    *   *The Sketchpad.* Generates initial visual distributions (histograms of population and area) and summary statistics to give a "pulse check" of the data.

### 🔸 The Heavy Machinery (Advanced Tools)
These scripts handle specific, heavy-duty tasks or run alternative, full-scale pipelines.

*   **`Final_output.R`**
    *   *The Master Builder.* A comprehensive script that merges specific datasets (Forest, Biome, WDI Research Data, GBIF) and produces a color-coded Excel file (`Final_Dataset_Complete.xlsx`). It categorizes variables into Money, Human Capital, Structure, and Bio for easy reading.
*   **`variables_raw_data.R`**
    *   *The Harvester.* A massive script designed to fetch fresh data from the outside world. It connects to APIs (World Bank, GeoBoundaries) to download, clean, and compile huge datasets for GDP, HDI, and Climate zones.
*   **`GBIF_Record_Code.R`**
    *   *The Census.* Connects to the GBIF API to count the number of biodiversity records for every country on Earth.
*   **`Calculate Diversity...biome_diversity.R`**
    *   *The Surveyor.* A spatial analysis tool that uses shapefiles (`sf`) to count unique ecoregions and calculate protected area percentages per country.
*   **`Geo-B-Countries.R`**
    *   *The Cartographer.* Fetches precise country boundary metadata from the GeoBoundaries API.

---

## 🔐 The Vault: Data

*   **`data/raw/`**: The raw minerals. Contains the original `biodiversity_dataset_master.csv`, shapefiles, and downloaded API data. **Do not edit these manually.**
*   **`data/processed/`**: The refined ingots. Contains the cleaned, merged, and ready-to-analyze CSV files (e.g., `biodiversity_clean_v1.csv`).

---

## 📜 The Blueprints: Documentation

Located in `docs/`:
*   **`column_definitions.md`**: The dictionary explaining what every variable means.
*   **`data_dictionary.csv`**: A structured format of the definitions.

---

## 🏗️ The Archives & Environment

*   **`renv/`**: Managed by the `renv` package. This ensures that if you run this code in 5 years, it will install the exact versions of the libraries we used today.
*   **`drafts/code_archive/`**: A storage unit for code that was replaced but might be needed for reference later.
