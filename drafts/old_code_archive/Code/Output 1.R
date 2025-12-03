# Install/Load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, jsonlite, countrycode, openxlsx)

# ==============================================================================
# STEP 1: PREPARE LANGUAGE DATA (Fixing the missing columns)
# ==============================================================================
print("--- Step 1: Fetching Language Data ---")
temp_json <- tempfile(fileext = ".json")
url_json <- "https://raw.githubusercontent.com/mledoze/countries/master/countries.json"

tryCatch({
  download.file(url_json, temp_json, mode = "wb")
  countries_json <- fromJSON(temp_json, flatten = FALSE) # flatten=FALSE is crucial
  
  social_data <- data.frame(
    iso3c = countries_json$cca3,
    main_language_code = sapply(countries_json$languages, function(x) {
      if (is.list(x) && length(x) > 0) names(x)[1] else NA
    }),
    is_english_main = sapply(countries_json$languages, function(x) {
      if (is.list(x) && length(x) > 0) "eng" %in% names(x) else FALSE
    })
  )
  print("✓ Language data ready.")
}, error = function(e) {
  message("! Error fetching languages: ", e$message)
  social_data <- data.frame(iso3c = character())
})

# ==============================================================================
# STEP 2: IMPORT DATA
# ==============================================================================
print("--- Step 2: Select Files to Merge ---")

# 1. Load Main Variables
print(">> Please select 'variables_per_country_UPDATED.csv'")
df_main <- read_csv(file.choose(), show_col_types = FALSE) %>%
  select(-any_of(c("main_language_code", "is_english_main"))) # Remove empty cols

# 2. Load Biodiversity Variables
print(">> Please select 'World_Biodiversity_Variables.csv'")
df_bio <- read_csv(file.choose(), show_col_types = FALSE) %>%
  select(iso_a3, area_total_km2, biome_diversity_count, pa_coverage_pct, forest_cover_pct) %>%
  rename(iso3c = iso_a3)

# 3. Load GBIF Records
print(">> Please select 'GBIF_records_full_with_names.csv'")
df_gbif <- read_csv(file.choose(), show_col_types = FALSE) %>%
  select(country_code_iso3, n_records_gbif) %>%
  rename(iso3c = country_code_iso3)

# ==============================================================================
# STEP 3: MERGE DATASETS
# ==============================================================================
print("--- Step 3: Merging ---")

final_df <- df_main %>%
  left_join(social_data, by = "iso3c") %>%  # Add fixed languages
  left_join(df_bio, by = "iso3c") %>%       # Add Bio vars
  left_join(df_gbif, by = "iso3c")          # Add GBIF count

# Cleanup duplicates created by join (e.g. country.x, country.y)
final_df <- final_df %>% 
  rename_with(~ gsub("\\.x$", "", .)) %>% 
  select(-matches("\\.y$"))

# ==============================================================================
# STEP 4: CLEANING (Remove unnecessary rows)
# ==============================================================================
print("--- Step 4: Cleaning Data ---")
initial_rows <- nrow(final_df)

final_df_clean <- final_df %>%
  # 1. Remove rows where ISO code or Country Name is missing
  filter(!is.na(iso3c) & iso3c != "") %>%
  filter(!is.na(country) & country != "") %>%
  
  # 2. Remove Aggregates (e.g., "World", "Arab World", "OECD members")
  # We check if the ISO code maps to a valid region name. 
  # Real countries have a valid region; Aggregates usually return NA in 'countrycode'
  filter(!is.na(countrycode(iso3c, "iso3c", "region")))

removed_rows <- initial_rows - nrow(final_df_clean)
print(paste("✓ Cleaned data. Removed", removed_rows, "rows (missing names or aggregates)."))

# ==============================================================================
# STEP 5: ORGANIZE COLUMNS BY CATEGORY
# ==============================================================================
# 1. Money (Gold)
vars_money <- c("wb_gdp_per_capita", "wb_gdp_total", "wb_percent_gdp_for_research", 
                "wb_researchers_per_million_people", "foreign_aid_oda", "hdi_proxy_gdp", "hdi_proxy_life")

# 2. Structure (Blue)
vars_structure <- c("wb_population", "area_total_km2", "wb_population_density", "wb_urbanisation_percent_urban")

# 3. H&C (History & Culture) (Purple)
vars_hc <- c("main_language_code", "is_english_main", "ever_colonized", "open_knowledge_score")

# 4. Biological (Green)
vars_bio <- c("n_records_gbif", "biome_diversity_count", "pa_coverage_pct", "forest_cover_pct", "mean_latitude")

# Reorder DataFrame
df_ordered <- final_df_clean %>%
  select(
    iso3c, country,           # Identifiers
    any_of(vars_money),       # Money
    any_of(vars_structure),   # Structure
    any_of(vars_hc),          # H&C
    any_of(vars_bio)          # Biological
  )

# ==============================================================================
# STEP 6: SAVE COLORED EXCEL
# ==============================================================================
print("--- Step 6: Saving Colored Excel ---")

wb <- createWorkbook()
addWorksheet(wb, "Final_Data")
writeData(wb, "Final_Data", df_ordered)

# Define Colors
style_id    <- createStyle(fgFill = "#D9D9D9", fontColour = "black", textDecoration = "bold", border = "TopBottomLeftRight") # Gray
style_money <- createStyle(fgFill = "#FFEB9C", fontColour = "#9C5700", textDecoration = "bold", border = "TopBottomLeftRight") # Gold
style_struc <- createStyle(fgFill = "#BDD7EE", fontColour = "#1F4E78", textDecoration = "bold", border = "TopBottomLeftRight") # Blue
style_hc    <- createStyle(fgFill = "#E1D5E7", fontColour = "#963634", textDecoration = "bold", border = "TopBottomLeftRight") # Purple
style_bio   <- createStyle(fgFill = "#C6EFCE", fontColour = "#006100", textDecoration = "bold", border = "TopBottomLeftRight") # Green

# Apply Styles Function
apply_col_style <- function(style, var_names) {
  cols <- which(colnames(df_ordered) %in% var_names)
  if(length(cols) > 0) addStyle(wb, "Final_Data", style, rows=1, cols=cols, gridExpand=TRUE)
}

apply_col_style(style_id, c("iso3c", "country"))
apply_col_style(style_money, vars_money)
apply_col_style(style_struc, vars_structure)
apply_col_style(style_hc, vars_hc)
apply_col_style(style_bio, vars_bio)

# Auto-width
setColWidths(wb, "Final_Data", cols = 1:ncol(df_ordered), widths = "auto")

saveWorkbook(wb, "Final_Dataset_Cleaned_Colored.xlsx", overwrite = TRUE)

print("✓ DONE! File saved as: 'Final_Dataset_Cleaned_Colored.xlsx'")