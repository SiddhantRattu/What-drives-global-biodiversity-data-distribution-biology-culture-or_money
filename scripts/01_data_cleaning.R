# 01_data_cleaning.R
# Logic: Rename Country, Colonizers, Language, Merge Datasets

library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(stringr)

# Ensure output directory exists
if (!dir.exists(here("data/processed"))) {
  dir.create(here("data/processed"), recursive = TRUE)
}

# Load data
# Master: biodiversity variables
biodiv_master <- read_csv(here("data/raw/biodiversity_dataset_master.csv"), show_col_types = FALSE)

# Aux: Colonies
colonies_df <- read_csv(here("data/raw/COLDAT_colonies.csv"), show_col_types = FALSE)

# Aux: Updated Variables (GDP, HDI, etc)
vars_updated_df <- read_csv(here("data/raw/variables_per_country_UPDATED.csv"), show_col_types = FALSE)


# --- 1. Clean Colonizers ---
col_cols <- c("col.belgium", "col.britain", "col.france", "col.germany",
              "col.italy", "col.netherlands", "col.portugal", "col.spain")

if(all(col_cols %in% names(colonies_df))) {
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

  print("Colonizers cleaned.")
} else {
  warning("Colonizer columns not found in colonies dataset.")
  colonies_processed <- colonies_df
}

# --- 2. Harmonize Country Names ---
harmonize_country_names <- function(df, country_col = "country") {
  if(!country_col %in% names(df)) return(df)

  df <- df %>%
    mutate(
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
      country = str_replace(country, ", (The|The Kingdom of)", "")
    )
  return(df)
}

if("name" %in% names(biodiv_master)) {
  biodiv_master <- biodiv_master %>% rename(country = name)
}
biodiv_master <- harmonize_country_names(biodiv_master, "country")

# --- 3. Merge Datasets ---
# Using iso3c (iso_a3 in master) as key
biodiv_master <- biodiv_master %>% rename(iso3c = iso_a3)

# Bridge Colonizer Name -> ISO3 (using vars_updated_df as bridge if needed, or colonies_df if it has iso3)
# colonies_df doesn't seem to have iso3, so we use country names or bridge via vars_updated_df

# Prepare Colonizers with ISO3
iso_bridge <- vars_updated_df %>%
  mutate(country_clean = str_trim(country)) %>%
  select(country_clean, iso3c) %>%
  filter(!is.na(iso3c)) %>%
  distinct(country_clean, .keep_all = TRUE)

colonies_iso <- colonies_processed %>%
  left_join(iso_bridge, by = "country_clean") %>%
  filter(!is.na(iso3c)) %>%
  distinct(iso3c, .keep_all = TRUE) %>%
  select(iso3c, colonizer, ever_colonized)

# Merge
final_df <- biodiv_master %>%
  left_join(vars_updated_df, by = "iso3c") %>%
  left_join(colonies_iso, by = "iso3c")

print("Datasets merged.")

# Save processed data
write_csv(final_df, here("data/processed/biodiversity_clean_v1.csv"))
print("Cleaned data saved to data/processed/biodiversity_clean_v1.csv")
