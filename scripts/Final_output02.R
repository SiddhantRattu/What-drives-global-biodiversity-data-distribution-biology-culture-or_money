library(tidyverse)
library(countrycode)
library(janitor)
library(readxl)

# -------------------------------------------------------------------------
# SMART LOADER
# -------------------------------------------------------------------------
load_data_smart <- function(prompt_text) {
  print(paste(">>> ACTION:", prompt_text))
  f_path <- file.choose()
  ext <- tools::file_ext(f_path)
  
  if (tolower(ext) %in% c("xlsx", "xls")) {
    df <- read_excel(f_path)
  } else {
    df <- read_csv(f_path, show_col_types = FALSE)
    if (ncol(df) <= 1) df <- read_csv2(f_path, show_col_types = FALSE)
  }
  
  # Standardize names
  df <- df %>% clean_names()
  
  # Find 'country' column
  if (!"country" %in% names(df)) {
    matches <- grep("country", names(df), value = TRUE)
    if (length(matches) > 0) {
      print(paste("Renaming", matches[1], "to 'country'"))
      df <- df %>% rename(country = all_of(matches[1]))
    } else {
      # Stop and show user what columns exist
      print("Columns found in file:")
      print(names(df))
      stop("Could not find a 'country' column in the file above.")
    }
  }
  return(df)
}

# -------------------------------------------------------------------------
# 1. LOAD FILES
# -------------------------------------------------------------------------
rl_data    <- load_data_smart("Select RED LIST file")
lang_data  <- load_data_smart("Select LANGUAGE file")
final_data <- load_data_smart("Select FINAL DATASET file")

# -------------------------------------------------------------------------
# 2. PROCESS RED LIST
# -------------------------------------------------------------------------
rl_clean <- rl_data %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  mutate(
    across(contains(c("total", "threatened")), ~ as.numeric(as.character(.))),
    total_species_count = rowSums(across(c(mammals_total, birds_total, reptiles_total, amphibians_total)), na.rm = TRUE),
    total_threatened_count = rowSums(across(c(mammals_threatened, birds_threatened, reptiles_threatened, amphibians_threatened)), na.rm = TRUE),
    percent_threatened = ifelse(total_species_count > 0, (total_threatened_count / total_species_count) * 100, 0)
  ) %>%
  select(iso3c, rl_region = region, total_species_count, total_threatened_count, percent_threatened)

# -------------------------------------------------------------------------
# 3. PROCESS LANGUAGE (FIXED)
# -------------------------------------------------------------------------
# Check if the column exists with or without underscore
lang_col_name <- "percent_of_population"
if (!"percent_of_population" %in% names(lang_data)) {
  # If not found, try to find it dynamically
  lang_col_name <- grep("percent", names(lang_data), value = TRUE)[1]
  print(paste("Using column for percentage:", lang_col_name))
}

lang_clean <- lang_data %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  # Use the detected column name dynamically
  mutate(language_pop_percent = parse_number(as.character(.data[[lang_col_name]]))) %>%
  select(iso3c, predominant_language_family = language_family, 
         language_pop_percent, primary_language = language_name_for_popups)

# -------------------------------------------------------------------------
# 4. MERGE AND SAVE
# -------------------------------------------------------------------------
merged_dataset <- final_data %>%
  clean_names() %>%
  rename_with(~"biome_diversity_count", contains("unique_ecosystems")) %>%
  left_join(rl_clean, by = "iso3c") %>%
  left_join(lang_clean, by = "iso3c")

write_csv(merged_dataset, "Merged_Final_Dataset_Corrected.csv")
print("SUCCESS! Saved as 'Merged_Final_Dataset_Corrected.csv'")