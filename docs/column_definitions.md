# Column Definitions & Database Schema

All percentage variables in the project have been unified under the standard `_pct` naming convention to ensure consistency across data ingestion, R modeling pipelines, and exported tables.

---

## 🌿 Biological & Ecological Variables (`vars_bio`)

- `terrestrial_protected_area_pct`: Percentage of total national land area designated as protected (converted from raw protected area surface boundaries using Mollweide equal-area projection `(PA_km2 / Area_km2) * 100`).
- `forest_cover_pct`: Percentage of total national land area covered by forest (sourced from World Bank / FAO).
- `biome_diversity_count`: Count of unique terrestrial ecoregions present within country borders (derived from `Ecoregions2017` spatial overlays).
- `mean_latitude`: Absolute latitude of country geographic centroid (in degrees).
- `endemic_tetrapods_total`: Absolute count of endemic species found exclusively within national borders.
- `n_records_gbif`: Total registered species occurrence records on GBIF (response variable).

---

## 💵 Money, Economy & Research Infrastructure (`vars_money`)

- `wb_gdp_per_capita`: Gross Domestic Product per capita in current USD.
- `wb_gdp_total`: Total Gross Domestic Product in current USD.
- `wb_research_gdp_pct`: Research & Development spending as a percentage of GDP (unified from `wb_percent_gdp_for_research`).
- `wb_researchers_per_million_people`: Full-time equivalent active scientific researchers per million population.
- `wb_total_research_spending_usd`: Derived total R&D investment (`wb_gdp_total * (wb_research_gdp_pct / 100)`).
- `wb_total_researchers_count`: Derived absolute researcher count (`(wb_population / 1,000,000) * wb_researchers_per_million_people`).
- `foreign_aid_oda`: Official Development Assistance specifically allocated to biodiversity in USD.
- `hdi_proxy_gdp`: Human Development Index proxy sub-index measuring economic standard of living.
- `hdi_proxy_life`: Human Development Index proxy sub-index measuring life expectancy.

---

## 🏛️ Culture, History & Governance (`vars_hc`)

- `main_language`: Primary language spoken in the country.
- `is_english_main`: Boolean flag (1 = Yes, 0 = No) indicating if English is the primary official or spoken language.
- `ever_colonized`: Indicator for historical colonization status (1 = Yes, 0 = No).
- `colonizer`: Primary colonizing power (Belgium, Britain, France, Germany, Italy, Netherlands, Portugal, Spain, Multiple, or Never Colonized).
- `main_religion_pct`: Percentage of total population adhering to the primary national religion (unified from `main_religion_percentage`).
- `atheists_non_religious_pct`: Percentage of population identifying as atheist or non-religious (unified from `% atheists/non-religious`).
- `open_knowledge_score`: Quantitative measure of open science, open data policy, and government openness.

---

## 📐 Structural & Demographic Controls (`vars_structure`)

- `wb_population`: Total national population count.
- `area_total_km2`: Total national land surface area in square kilometers.
- `wb_population_density`: Population density (inhabitants per square kilometer).
- `wb_urbanisation_pct_urban`: Percentage of population living in urban areas (unified from `wb_urbanisation_percent_urban`).
