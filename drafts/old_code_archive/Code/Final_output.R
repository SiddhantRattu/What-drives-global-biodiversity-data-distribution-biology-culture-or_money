# ==============================================================================
# SETUP & LIBRARIES
# ==============================================================================
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("readxl")) install.packages("readxl")
if (!require("stringr")) install.packages("stringr")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("WDI")) install.packages("WDI")

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)
library(WDI)

# ==============================================================================
# 1. LOAD DATASETS
# ==============================================================================

# Main Dataset
final_df <- read_excel("Final_Dataset_Cleaned_Colored.xlsx")
# Ensure iso3c is clean
final_df <- final_df %>% mutate(iso3c = str_trim(iso3c))

# Auxiliary Files
colonies_df     <- read_csv("COLDAT_colonies.csv", show_col_types = FALSE)
vars_updated_df <- read_csv("variables_per_country_UPDATED.csv", show_col_types = FALSE)
gbif_df         <- read_csv("GBIF_records_full_with_names.csv", show_col_types = FALSE)
forest_df       <- read_csv("Forest_Data..csv", skip = 4, show_col_types = FALSE)
biodiv_vars_df  <- read_csv("Biodiversity_Variables.csv", show_col_types = FALSE) # New File

# ==============================================================================
# 2. PREPARE DATA SOURCES
# ==============================================================================

# --- A. Forest Data (2021) ---
forest_clean <- forest_df %>%
  select(iso3c = `Country Code`, forest_cover_pct_new = `2021`) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>% slice(1) %>% ungroup()

# --- B. Biome Count (From Biodiversity_Variables.csv) ---
# Columns expected: name, iso_a3, area_km2, biome_count, pa_pct, forest_pct
biome_clean <- biodiv_vars_df %>%
  select(iso3c = iso_a3, biome_count_new = biome_count) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>% slice(1) %>% ungroup()

# --- C. Colonizer Logic ---
col_cols <- c("col.belgium", "col.britain", "col.france", "col.germany", 
              "col.italy", "col.netherlands", "col.portugal", "col.spain")

colonies_processed <- colonies_df %>%
  rowwise() %>%
  mutate(
    colonizers = list(c("Belgium", "Britain", "France", "Germany", "Italy", 
                        "Netherlands", "Portugal", "Spain")[c_across(all_of(col_cols)) == 1]),
    colonizer = if (length(colonizers) == 0) "Never Colonized" 
    else if (length(colonizers) > 1) "Multiple" 
    else colonizers[[1]]
  ) %>%
  ungroup() %>%
  mutate(country_clean = str_trim(country))

# Bridge Colonizer Name -> ISO3
iso_bridge <- vars_updated_df %>%
  mutate(country_clean = str_trim(country)) %>%
  select(country_clean, iso3c) %>%
  filter(!is.na(iso3c)) %>%
  distinct(country_clean, .keep_all = TRUE)

colonies_iso <- colonies_processed %>%
  left_join(iso_bridge, by = "country_clean") %>%
  filter(!is.na(iso3c)) %>%
  distinct(iso3c, .keep_all = TRUE) %>%
  select(iso3c, colonizer)

# --- D. Fetch & Calculate Research Data (WDI) ---
message("Fetching Research Data from World Bank...")
wdi_data <- WDI(
  indicator = c("wb_percent_gdp_for_research" = "GB.XPD.RSDV.GD.ZS", 
                "wb_researchers_per_million_people" = "SP.POP.SCIE.RD.P6"),
  start = 2010, end = 2024, extra = TRUE
)

wdi_clean <- wdi_data %>%
  filter(!is.na(iso3c) & iso3c != "") %>%
  group_by(iso3c) %>%
  arrange(desc(year)) %>%
  summarise(
    wb_percent_gdp_for_research_new = first(na.omit(wb_percent_gdp_for_research)),
    wb_researchers_per_million_people_new = first(na.omit(wb_researchers_per_million_people))
  ) %>% ungroup()

# ==============================================================================
# 3. MERGE EVERYTHING
# ==============================================================================

final_complete <- final_df %>%
  # 1. Join Updated Variables (GDP, HDI, etc)
  left_join(vars_updated_df %>% 
              select(iso3c, wb_gdp_per_capita_new = wb_gdp_per_capita, wb_gdp_total_new = wb_gdp_total, 
                     foreign_aid_oda_new = foreign_aid_oda, hdi_proxy_gdp_new = hdi_proxy_gdp, 
                     hdi_proxy_life_new = hdi_proxy_life, wb_population_new = wb_population, 
                     wb_urbanisation_percent_urban_new = wb_urbanisation_percent_urban,
                     ever_colonized_new = ever_colonized, mean_latitude_new = mean_latitude), 
            by = "iso3c") %>%
  # 2. Join GBIF
  left_join(gbif_df %>% select(country_code_iso3, n_records_gbif_new = n_records_gbif) %>% distinct(country_code_iso3, .keep_all=TRUE), 
            by = c("iso3c" = "country_code_iso3")) %>%
  # 3. Join Forest
  left_join(forest_clean, by = "iso3c") %>%
  # 4. Join Biome Count
  left_join(biome_clean, by = "iso3c") %>%
  # 5. Join Colonizer
  left_join(colonies_iso, by = "iso3c") %>%
  # 6. Join WDI Research Data
  left_join(wdi_clean, by = "iso3c") %>%
  
  # --- COALESCE & CALCULATE ---
  mutate(
    # Basic fills
    wb_gdp_per_capita = coalesce(wb_gdp_per_capita, wb_gdp_per_capita_new),
    wb_gdp_total = coalesce(wb_gdp_total, wb_gdp_total_new),
    foreign_aid_oda = coalesce(foreign_aid_oda, foreign_aid_oda_new),
    hdi_proxy_gdp = coalesce(hdi_proxy_gdp, hdi_proxy_gdp_new),
    hdi_proxy_life = coalesce(hdi_proxy_life, hdi_proxy_life_new),
    wb_population = coalesce(as.numeric(wb_population), as.numeric(wb_population_new)),
    wb_urbanisation_percent_urban = coalesce(wb_urbanisation_percent_urban, wb_urbanisation_percent_urban_new),
    ever_colonized = coalesce(ever_colonized, ever_colonized_new),
    mean_latitude = coalesce(mean_latitude, mean_latitude_new),
    n_records_gbif = coalesce(n_records_gbif, n_records_gbif_new),
    
    # New Data Sources
    forest_cover_pct = coalesce(forest_cover_pct, forest_cover_pct_new),
    biome_diversity_count = coalesce(biome_diversity_count, biome_count_new),
    wb_percent_gdp_for_research = coalesce(wb_percent_gdp_for_research, wb_percent_gdp_for_research_new),
    wb_researchers_per_million_people = coalesce(wb_researchers_per_million_people, wb_researchers_per_million_people_new),
    
    # Calculations
    wb_total_research_spending_usd = wb_gdp_total * (wb_percent_gdp_for_research / 100),
    wb_total_researchers_count = (wb_population / 1000000) * wb_researchers_per_million_people,
    
    # Colonizer Logic
    colonizer = case_when(
      ever_colonized == 0 ~ "Never Colonized",
      !is.na(colonizer) ~ colonizer,
      TRUE ~ "Unknown"
    )
  ) %>%
  # Cleanup
  select(-ends_with("_new"), -matches("forest_cover_pct_new|biome_count_new"))

# ==============================================================================
# 4. COLORING & EXPORT
# ==============================================================================

# Categorization
vars_money <- c("wb_gdp_per_capita", "wb_gdp_total", "wb_percent_gdp_for_research", 
                "wb_researchers_per_million_people", "foreign_aid_oda", 
                "hdi_proxy_gdp", "hdi_proxy_life", 
                "wb_total_research_spending_usd", "wb_total_researchers_count")

vars_hc <- c("main_language_code", "is_english_main", "ever_colonized", 
             "colonizer", "open_knowledge_score")

vars_structure <- c("wb_population", "area_total_km2", "wb_population_density", 
                    "wb_urbanisation_percent_urban")

vars_bio <- c("n_records_gbif", "biome_diversity_count", "pa_coverage_pct", 
              "forest_cover_pct", "mean_latitude")

vars_basic <- c("iso3c", "country")

# Create Workbook
wb <- createWorkbook()
addWorksheet(wb, "Final Data")
writeData(wb, "Final Data", final_complete)

# Styles
style_money     <- createStyle(fgFill = "#C6EFCE", textDecoration = "bold", border = "TopBottomLeftRight") # Green
style_hc        <- createStyle(fgFill = "#FFCC99", textDecoration = "bold", border = "TopBottomLeftRight") # Orange
style_structure <- createStyle(fgFill = "#99CCFF", textDecoration = "bold", border = "TopBottomLeftRight") # Blue
style_bio       <- createStyle(fgFill = "#CC99FF", textDecoration = "bold", border = "TopBottomLeftRight") # Purple
style_basic     <- createStyle(fgFill = "#D3D3D3", textDecoration = "bold", border = "TopBottomLeftRight") # Grey
style_def       <- createStyle(textDecoration = "bold", border = "TopBottomLeftRight")

# Apply Styles
for (i in seq_along(names(final_complete))) {
  col <- names(final_complete)[i]
  if (col %in% vars_money) addStyle(wb, "Final Data", style = style_money, rows = 1, cols = i)
  else if (col %in% vars_hc) addStyle(wb, "Final Data", style = style_hc, rows = 1, cols = i)
  else if (col %in% vars_structure) addStyle(wb, "Final Data", style = style_structure, rows = 1, cols = i)
  else if (col %in% vars_bio) addStyle(wb, "Final Data", style = style_bio, rows = 1, cols = i)
  else if (col %in% vars_basic) addStyle(wb, "Final Data", style = style_basic, rows = 1, cols = i)
  else addStyle(wb, "Final Data", style = style_def, rows = 1, cols = i)
}

saveWorkbook(wb, "Final_Dataset_Complete.xlsx", overwrite = TRUE)
message("Processing Complete. File saved as 'Final_Dataset_Complete.xlsx'")