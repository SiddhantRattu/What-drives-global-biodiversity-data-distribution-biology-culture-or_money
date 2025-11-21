# Load necessary libraries
library(WDI)
library(rnaturalearth)
library(sf)
library(dplyr)
library(countrycode)
library(readr)
library(jsonlite)

# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(WDI, rnaturalearth, sf, dplyr, countrycode, readr, jsonlite)

# ==============================================================================
# 1. LOAD LOCAL COLONIAL HISTORY DATA (Manual Selection)
# ==============================================================================
print("--- Step 1: Load Colonial Data ---")
print("A window will open. Please select the 'COLDAT_colonies.csv' file.")

# File selection window
colonial_path <- file.choose() 
colonial_raw <- read_csv(colonial_path, show_col_types = FALSE)

# Process
colonial_data <- colonial_raw %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    # If sum of col.X columns > 0, then they were colonized
    ever_colonized = if_else(rowSums(select(., starts_with("col.") & !contains("start") & !contains("end")), na.rm = TRUE) > 0, 1, 0)
  ) %>%
  select(iso3c, country, ever_colonized) %>% 
  filter(!is.na(iso3c))

print("✓ Loaded and processed COLDAT_colonies.csv")

# ==============================================================================
# 2. GEOGRAPHIC VARIABLES (Mean Latitude)
# ==============================================================================
print("--- Step 2: Download Geographic Data ---")
world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map$mean_latitude <- st_coordinates(st_centroid(world_map))[,2]

geo_data <- world_map %>%
  select(iso_a3, mean_latitude) %>%
  st_drop_geometry() %>%
  rename(iso3c = iso_a3)

print("✓ Downloaded Geography data (Mean Latitude)")

# ==============================================================================
# 3. POLITICAL & ECONOMIC VARIABLES (WDI)
# ==============================================================================
print("--- Step 3: Download World Bank Data ---")
wdi_indicators <- c(
  foreign_aid_oda = "DT.ODA.ODAT.CD",
  hdi_proxy_gdp   = "NY.GDP.PCAP.PP.CD",
  hdi_proxy_life  = "SP.DYN.LE00.IN"
)

wdi_data <- WDI(indicator = wdi_indicators, start = 2020, end = 2023, extra = FALSE) %>%
  group_by(iso2c) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
  select(iso3c, foreign_aid_oda, hdi_proxy_gdp, hdi_proxy_life)

print("✓ Downloaded World Bank (WDI) data")

# ==============================================================================
# 4. SOCIAL VARIABLES (Language) - CORRECTED
# ==============================================================================
print("--- Step 4: Download Language Data ---")
temp_json <- tempfile(fileext = ".json")

tryCatch({
  download.file("https://raw.githubusercontent.com/mledoze/countries/master/countries.json", temp_json, mode = "wb")
  
  # FIX: Use flatten = FALSE to preserve the 'languages' list structure
  countries_json <- fromJSON(temp_json, flatten = FALSE)
  
  # Helper functions
  get_main_lang <- function(lang_list) {
    if (is.list(lang_list) && length(lang_list) > 0) {
      return(names(lang_list)[1]) 
    } else {
      return(NA)
    }
  }
  
  check_english <- function(lang_list) {
    if (is.list(lang_list) && length(lang_list) > 0) {
      return("eng" %in% names(lang_list)) 
    } else {
      return(FALSE)
    }
  }
  
  # Apply helpers
  countries_json$main_language_code <- sapply(countries_json$languages, get_main_lang)
  countries_json$is_english_main <- sapply(countries_json$languages, check_english)
  
  social_data <- countries_json %>%
    select(cca3, main_language_code, is_english_main) %>%
    rename(iso3c = cca3)
  
  print("✓ Downloaded Social data (Language)")
  
}, error = function(e) {
  message("! Error downloading/processing language data: ", e$message)
  # Create empty fallback to allow code to continue
  social_data <<- data.frame(iso3c = character(), main_language_code = character(), is_english_main = logical())
})

# ==============================================================================
# 5. OPEN KNOWLEDGE INDEX
# ==============================================================================
print("--- Step 5: Download Open Knowledge Index ---")
okfn_data <- tryCatch({
  df <- read_csv("http://index.okfn.org/api/places.csv", show_col_types = FALSE)
  df %>% select(id, score) %>% rename(iso3c = id, open_knowledge_score = score)
}, error = function(e) {
  print("! Note: OKFN URL failed. Creating empty columns.")
  data.frame(iso3c = character(), open_knowledge_score = numeric())
})

# ==============================================================================
# 6. MERGE ALL DATA
# ==============================================================================
print("--- Step 6: Merge and Save ---")
print("Please select your 'variables_per_country.csv' file now.")
base_path <- file.choose()
base_df <- read_csv(base_path, show_col_types = FALSE)

# Merge
final_df <- base_df %>%
  left_join(colonial_data, by = "iso3c") %>%
  left_join(geo_data, by = "iso3c") %>%
  left_join(wdi_data, by = "iso3c") %>%
  left_join(social_data, by = "iso3c") %>%
  left_join(okfn_data, by = "iso3c")

# Cleanup
final_df <- final_df %>% select(!matches("\\.y$")) %>% rename_with(~sub("\\.x$", "", .))

# Save
write_csv(final_df, "variables_per_country_UPDATED.csv")
print("✓ DONE! Saved to 'variables_per_country_UPDATED.csv'")