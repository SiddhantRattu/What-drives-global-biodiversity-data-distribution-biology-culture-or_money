

# Date: October 10/2025 




library(tidyverse)
library(WDI)
library(janitor)
library(here)
library(readxl) # Added for reading .xlsx files

# Define file paths and directories
data_dir <- here("data")
raw_data_dir <- here("data", "raw_data")
clean_data_dir <- here("data", "clean_data")

# Create directories if they don't exist
if (!dir.exists(data_dir)) dir.create(data_dir)
if (!dir.exists(raw_data_dir)) dir.create(raw_data_dir)
if (!dir.exists(clean_data_dir)) dir.create(clean_data_dir)

# Keep track of the download date
download_date <- Sys.Date()
print(paste("Data download date:", download_date))


# --- 2. CORE FUNCTIONS ---

#' Extracts World Bank indicator codes from the variables metadata CSV.
#'
#' @param file_path The path to the metadata file (e.g., 'Variables.xlsx').
#' @return A named vector where names are clean R variable names and values are WDI codes.
get_wdi_indicators <- function(file_path) {
  # Robustly read the metadata file
  variables_metadata <- tryCatch({
    # Using read_excel for .xlsx files, assuming data is on the first sheet
    readxl::read_excel(file_path, sheet = 1) %>% janitor::clean_names()
  }, error = function(e) {
    stop(paste("Error reading metadata file:", file_path, "Please ensure the file exists and is accessible. Original error:", e$message))
  })
  
  # Identify World Bank indicators and extract their codes using regex
  indicator_codes <- variables_metadata %>%
    filter(str_detect(source, "World Bank")) %>%
    mutate(
      # Extract the indicator code using lookbehind for 'indicator: '
      indicator_code = str_extract(source, "(?<=indicator:\\s)[A-Z0-9\\.]+")
    ) %>%
    filter(!is.na(indicator_code)) %>%
    select(variable, indicator_code)
  
  # Convert to a named vector for WDI function call
  wdi_indicator_list <- setNames(
    indicator_codes$indicator_code,
    janitor::make_clean_names(indicator_codes$variable)
  )
  
  return(wdi_indicator_list)
}


#' Standardizes country names to prevent duplication (e.g., Congo, Rep. vs. Republic of Congo).
#'
#' @param df The dataframe containing a country column.
#' @param country_col The name of the country column.
#' @return The dataframe with harmonized country names.
harmonize_country_names <- function(df, country_col = "country") {
  df <- df %>%
    mutate(
      # Standardize country names using case_match for known World Bank variations
      country = case_match(.data[[country_col]],
                           "Congo, Rep." ~ "Republic of Congo",
                           "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
                           "Egypt, Arab Rep." ~ "Egypt",
                           "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
                           "Slovak Republic" ~ "Slovakia",
                           "Venezuela, RB" ~ "Venezuela",
                           "Yemen, Rep." ~ "Yemen",
                           .default = .data[[country_col]]
      ),
      # Remove any remaining World Bank list formatting
      country = str_replace(country, ", (The|The Kingdom of)", "")
    )
  return(df)
}


# --- 3. DATA PIPELINE EXECUTION ---

# 3.1. IDENTIFY INDICATORS
# The function now uses the simple filename "Variables.xlsx"
wdi_indicator_list <- get_wdi_indicators("Variables.xlsx")

print(paste("Identified indicators to download:", paste(unname(wdi_indicator_list), collapse = ", ")))
print(paste("Clean variable names for final table:", paste(names(wdi_indicator_list), collapse = ", ")))

# 3.2. DOWNLOAD RAW DATA
# UPDATED: Changed start year to 2010 and end year to 2025
raw_data_wdi <- WDI(
  indicator = wdi_indicator_list,
  country = "all",
  start = 2010,
  end = 2025,
  extra = TRUE,
  cache = NULL
)

# Save the raw data download
raw_filename <- paste0("raw_world_bank_data_", download_date, ".rds")
saveRDS(raw_data_wdi, here(raw_data_dir, raw_filename))
print(paste("Raw data downloaded and saved to:", here(raw_data_dir, raw_filename)))


# 3.3. CLEANING AND HARMONIZATION
cleaned_wdi_data <- raw_data_wdi %>%
  # Select relevant columns and clean names
  select(iso3c, country, year, all_of(names(wdi_indicator_list))) %>%
  # Harmonize country names
  harmonize_country_names() %>%
  # Filter for the latest available data point per country
  group_by(iso3c, country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  # Drop the year column as we now have the latest value
  select(-year)

# Check for duplicates of countries
duplicates_check <- cleaned_wdi_data %>%
  count(country) %>%
  filter(n > 1)

if (nrow(duplicates_check) > 0) {
  warning("Duplicated country names found after harmonization. Inspect manually.")
  print(duplicates_check)
} else {
  print("Country names successfully harmonized (0 duplicates by name/iso3c).")
}

# Store the clean data as an RDS file (for preservation of R objects)
clean_filename <- "clean_world_bank_data.rds"
saveRDS(cleaned_wdi_data, here(clean_data_dir, clean_filename))
print(paste("Clean data saved to:", here(clean_data_dir, clean_filename)))


# --- 4. FINAL COMPILATION (variables_per_country) ---

variables_per_country <- cleaned_wdi_data %>%
  # Drop the World Bank 'iso2c' code (if present from raw data) and keep only the standard 'iso3c'
  select(-any_of("iso2c")) %>%
  # Prefix the variable columns for clarity in the final table
  rename_with(~ paste0("wb_", .x), .cols = all_of(names(wdi_indicator_list)))

# Save the final compiled dataframe as CSV (as requested)
write_csv(variables_per_country, here("variables_per_country.csv"))
print(paste("Final compiled dataframe saved as:", here("variables_per_country.csv")))

# The dataframe 'variables_per_country' is now available in the R environment.