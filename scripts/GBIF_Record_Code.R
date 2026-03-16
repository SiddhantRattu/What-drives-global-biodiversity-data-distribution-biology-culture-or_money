# Load necessary libraries
library(rgbif)
library(tibble)
library(dplyr)
library(countrycode)
library(readr)
library(writexl)

get_gbif_counts_final <- function(csv_file = "GBIF_records.csv", excel_file = "GBIF_records.xlsx") {
  
  message("Step 1: Fetching counts from GBIF...")
  
  # Fetch all country counts in one request
  res <- occ_search(limit = 0, facet = "country", facetLimit = 300)
  
  # Extract the data frame from the list
  raw_counts <- res$facets$country
  
  message("Step 2: Processing and cleaning data...")
  
  # Process the results
  processed_data <- raw_counts %>%
    as_tibble() %>%
    rename(country_code_iso2 = name, n_records_gbif = count) %>%
    mutate(
      # Convert ISO2 to ISO3
      country_code_iso3 = countrycode(country_code_iso2, 
                                      origin = "iso2c", 
                                      destination = "iso3c", 
                                      warn = FALSE),
      # Convert ISO2 to Country Name
      country_name = countrycode(country_code_iso2, 
                                 origin = "iso2c", 
                                 destination = "country.name", 
                                 warn = FALSE)
    ) %>%
    # Manual fix for Kosovo (XK) which countrycode often misses
    mutate(
      country_name = ifelse(country_code_iso2 == "XK", "Kosovo", country_name),
      country_code_iso3 = ifelse(country_code_iso2 == "XK", "XKX", country_code_iso3)
    ) %>%
    # Remove rows where we couldn't resolve a country (like 'Unknown' or 'ZZ')
    filter(!is.na(country_name)) %>%
    select(country_name, country_code_iso3, country_code_iso2, n_records_gbif) %>%
    arrange(desc(n_records_gbif))
  
  # Step 3: Save Files
  message("Step 3: Saving files...")
  
  # Save CSV
  write_csv(processed_data, csv_file)
  
  # Save Excel
  write_xlsx(processed_data, excel_file)
  
  message("Done! Files created: ", csv_file, " and ", excel_file)
  
  return(processed_data)
}

# Run the fixed function
final_results <- get_gbif_counts_final()

# Preview the top 10
print(head(final_results, 10))