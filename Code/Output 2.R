# --- INSTALL MISSING PACKAGES ---
# Run these lines once if you haven't installed them yet
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("readxl")) install.packages("readxl")
if (!require("stringr")) install.packages("stringr")
if (!require("openxlsx")) install.packages("openxlsx")

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)

# ==============================================================================
# 1. LOAD DATASETS
# ==============================================================================

# Load the main Excel file (using readxl for better stability)
final_df <- read_excel("Final_Dataset_Cleaned_Colored.xlsx")

# Ensure the key column exists and is clean
if (!"iso3c" %in% names(final_df)) {
  stop("ERROR: 'iso3c' column not found in Final_Dataset_Cleaned_Colored.xlsx. Please check the column headers in the Excel file.")
}
final_df <- final_df %>% mutate(iso3c = str_trim(iso3c))

# Load auxiliary CSVs
colonies_df <- read_csv("COLDAT_colonies.csv", show_col_types = FALSE)
vars_updated_df <- read_csv("variables_per_country_UPDATED.csv", show_col_types = FALSE)
biodiv_df <- read_csv("World_Biodiversity_Variables.csv", show_col_types = FALSE)
gbif_df <- read_csv("GBIF_records_full_with_names.csv", show_col_types = FALSE)
forest_df <- read_csv("Forest_Data..csv", skip = 4, show_col_types = FALSE) # Skip metadata rows

# ==============================================================================
# 2. CLEAN & PREPARE AUXILIARY DATA
# ==============================================================================

# --- A. Process Forest Data (Using Year 2021) ---
forest_clean <- forest_df %>%
  select(iso3c = `Country Code`, forest_cover_pct_new = `2021`) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  slice(1) %>% # Deduplicate
  ungroup()

# --- B. Process Colonizer Data ---
col_cols <- c("col.belgium", "col.britain", "col.france", "col.germany", 
              "col.italy", "col.netherlands", "col.portugal", "col.spain")

colonies_processed <- colonies_df %>%
  rowwise() %>%
  mutate(
    # Identify colonizers
    colonizers = list(c("Belgium", "Britain", "France", "Germany", "Italy", 
                        "Netherlands", "Portugal", "Spain")[c_across(all_of(col_cols)) == 1]),
    # Categorize safely
    colonizer = if (length(colonizers) == 0) {
      "Never Colonized"
    } else if (length(colonizers) > 1) {
      "Multiple"
    } else {
      colonizers[[1]]
    }
  ) %>%
  ungroup() %>%
  mutate(country_clean = str_trim(country))

# Bridge Colony Names to ISO3 Codes
# We use vars_updated_df to map names to ISO codes
iso_bridge <- vars_updated_df %>%
  mutate(country_clean = str_trim(country)) %>%
  select(country_clean, iso3c) %>%
  filter(!is.na(iso3c)) %>%
  distinct(country_clean, .keep_all = TRUE) # Ensure unique bridge

colonies_iso <- colonies_processed %>%
  left_join(iso_bridge, by = "country_clean") %>%
  filter(!is.na(iso3c)) %>%
  distinct(iso3c, .keep_all = TRUE) %>%
  select(iso3c, colonizer)

# --- C. Deduplicate Other Sources ---
# It is crucial to deduplicate these before joining to avoid row explosion
vars_updated_unique <- vars_updated_df %>%
  filter(!is.na(iso3c)) %>%
  distinct(iso3c, .keep_all = TRUE)

gbif_unique <- gbif_df %>%
  filter(!is.na(country_code_iso3)) %>%
  distinct(country_code_iso3, .keep_all = TRUE)

biodiv_unique <- biodiv_df %>%
  filter(!is.na(iso_a3)) %>%
  distinct(iso_a3, .keep_all = TRUE)

# ==============================================================================
# 3. MERGE & FIX MISSING VALUES
# ==============================================================================

final_fixed <- final_df %>%
  # Join updated economic/social data
  left_join(vars_updated_unique %>% 
              select(iso3c, 
                     wb_gdp_per_capita_new = wb_gdp_per_capita, 
                     wb_gdp_total_new = wb_gdp_total, 
                     foreign_aid_oda_new = foreign_aid_oda, 
                     hdi_proxy_gdp_new = hdi_proxy_gdp, 
                     hdi_proxy_life_new = hdi_proxy_life, 
                     wb_population_new = wb_population, 
                     wb_urbanisation_percent_urban_new = wb_urbanisation_percent_urban,
                     ever_colonized_new = ever_colonized,
                     mean_latitude_new = mean_latitude), 
            by = "iso3c") %>%
  # Join GBIF
  left_join(gbif_unique %>% 
              select(country_code_iso3, n_records_gbif_new = n_records_gbif), 
            by = c("iso3c" = "country_code_iso3")) %>%
  # Join Area
  left_join(biodiv_unique %>% 
              select(iso_a3, area_total_km2_new = area_total_km2), 
            by = c("iso3c" = "iso_a3")) %>%
  # Join Forest
  left_join(forest_clean, by = "iso3c") %>%
  # Join Colonizer
  left_join(colonies_iso, by = "iso3c") %>%
  
  # COALESCE (Fill missing values)
  mutate(
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
    area_total_km2 = coalesce(area_total_km2, area_total_km2_new),
    
    # Use new forest data if original is missing
    forest_cover_pct = coalesce(forest_cover_pct, forest_cover_pct_new),
    
    # Fix Colonizer logic
    colonizer = case_when(
      ever_colonized == 0 ~ "Never Colonized",
      !is.na(colonizer) ~ colonizer,
      TRUE ~ "Unknown"
    )
  ) %>%
  select(-ends_with("_new"), -matches("forest_cover_pct_new"))

# ==============================================================================
# 4. EXPORT WITH COLORED HEADERS
# ==============================================================================

# Define Categories
vars_money <- c("wb_gdp_per_capita", "wb_gdp_total", "wb_percent_gdp_for_research", 
                "wb_researchers_per_million_people", "foreign_aid_oda", 
                "hdi_proxy_gdp", "hdi_proxy_life")
vars_hc <- c("main_language_code", "is_english_main", "ever_colonized", 
             "colonizer", "open_knowledge_score")
vars_structure <- c("wb_population", "area_total_km2", "wb_population_density", 
                    "wb_urbanisation_percent_urban")
vars_bio <- c("n_records_gbif", "biome_diversity_count", "pa_coverage_pct", 
              "forest_cover_pct", "mean_latitude")
vars_basic <- c("iso3c", "country")

# Create Excel Workbook
wb <- createWorkbook()
addWorksheet(wb, "Final Data")
writeData(wb, "Final Data", final_fixed)

# Define Color Styles
style_money     <- createStyle(fgFill = "#C6EFCE", textDecoration = "bold", border = "TopBottomLeftRight") # Green
style_hc        <- createStyle(fgFill = "#FFCC99", textDecoration = "bold", border = "TopBottomLeftRight") # Orange
style_structure <- createStyle(fgFill = "#99CCFF", textDecoration = "bold", border = "TopBottomLeftRight") # Blue
style_bio       <- createStyle(fgFill = "#CC99FF", textDecoration = "bold", border = "TopBottomLeftRight") # Purple
style_basic     <- createStyle(fgFill = "#D3D3D3", textDecoration = "bold", border = "TopBottomLeftRight") # Grey
style_default   <- createStyle(textDecoration = "bold", border = "TopBottomLeftRight")

# Apply Styles
col_names <- names(final_fixed)
for (i in seq_along(col_names)) {
  col <- col_names[i]
  if (col %in% vars_money) {
    addStyle(wb, "Final Data", style = style_money, rows = 1, cols = i)
  } else if (col %in% vars_hc) {
    addStyle(wb, "Final Data", style = style_hc, rows = 1, cols = i)
  } else if (col %in% vars_structure) {
    addStyle(wb, "Final Data", style = style_structure, rows = 1, cols = i)
  } else if (col %in% vars_bio) {
    addStyle(wb, "Final Data", style = style_bio, rows = 1, cols = i)
  } else if (col %in% vars_basic) {
    addStyle(wb, "Final Data", style = style_basic, rows = 1, cols = i)
  } else {
    addStyle(wb, "Final Data", style = style_default, rows = 1, cols = i)
  }
}

# Save
saveWorkbook(wb, "Final_Dataset_Fixed_Colored.xlsx", overwrite = TRUE)
message("Processing complete. File saved as 'Final_Dataset_Fixed_Colored.xlsx'.")