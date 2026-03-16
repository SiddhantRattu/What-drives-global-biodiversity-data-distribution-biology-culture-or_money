# Load necessary libraries
library(dplyr)
library(readr)

# 1. Load the datasets
# Assuming files are in the current working directory
final_dataset <- read_csv(file.choose())
gbif_records <- read_csv(file.choose())

# 2. Update the GBIF records in the final dataset
# We use left_join to bring in the new records based on the ISO3 code
updated_dataset <- final_dataset %>%
  left_join(gbif_records %>% select(country_code_iso3, n_records_gbif_new = n_records_gbif), 
            by = c("iso3c" = "country_code_iso3")) %>%
  mutate(n_records_gbif = ifelse(!is.na(n_records_gbif_new), n_records_gbif_new, n_records_gbif)) %>%
  select(-n_records_gbif_new)

# 3. Save the updated dataset
write_csv(updated_dataset, "Updated_Final_Dataset.csv")

# Check the results
head(updated_dataset %>% select(iso3c, country, n_records_gbif))