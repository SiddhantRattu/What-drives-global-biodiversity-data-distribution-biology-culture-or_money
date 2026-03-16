# 1. Install and load necessary packages
if (!require("readxl")) install.packages("readxl")
if (!require("WDI")) install.packages("WDI")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("countrycode")) install.packages("countrycode")

library(readxl)
library(WDI)
library(tidyverse)
library(countrycode)

# --- STEP 1: UPLOAD FILES ---
cat("Select the 'Final_Dataset_complete.xlsx' file\n")
# Using read_xlsx to handle Excel files correctly
final_df <- read_xlsx(file.choose())

cat("Select the '2025-1_RL_Table_8a.xlsx' file\n")
rl_df <- read_xlsx(file.choose())

# --- STEP 2: PROCESS RED LIST DATA ---
# We sum the tetrapod groups and create a matching ISO code
rl_processed <- rl_df %>%
  mutate(
    endemic_tetrapods_total = Mammals_Total + Birds_Total + 
      Reptiles_Total + Amphibians_Total,
    # Standardize country name to ISO3C for perfect merging
    iso3c = countrycode(Country, "country.name", "iso3c")
  ) %>%
  select(iso3c, endemic_tetrapods_total) %>%
  filter(!is.na(iso3c))

# --- STEP 3: FETCH WDI DATA (API) ---
# Indicator ER.LND.PTLD.ZS = Terrestrial protected areas (% of total land area)
cat("Fetching latest Protected Area data from World Bank API...\n")
wdi_data <- WDI(indicator = "ER.LND.PTLD.ZS", start = 2020, end = 2023) %>%
  filter(!is.na(ER.LND.PTLD.ZS)) %>%
  group_by(iso3c) %>%
  filter(year == max(year)) %>% # Get the most recent available year
  select(iso3c, terrestrial_protected_area_pct = ER.LND.PTLD.ZS)

# --- STEP 4: MERGE EVERYTHING ---
updated_dataset <- final_df %>%
  left_join(rl_processed, by = "iso3c") %>%
  left_join(wdi_data, by = "iso3c")

# --- STEP 5: SAVE RESULT ---
write.csv(updated_dataset, "Updated_Final_Dataset_with_New_Columns.csv", row.names = FALSE)
cat("Done! Your new file is saved as 'Updated_Final_Dataset_with_New_Columns.csv' in your Documents folder.\n")

# View the first few rows
head(updated_dataset %>% select(country, endemic_tetrapods_total, terrestrial_protected_area_pct))