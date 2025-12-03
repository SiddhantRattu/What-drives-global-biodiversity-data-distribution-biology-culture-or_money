# 02_check_missing_data.R
# Logic: Identify NA values vs. script errors

library(tidyverse)
library(here)

data_clean <- read_csv(here("data/processed/biodiversity_clean_v1.csv"), show_col_types = FALSE)

# Check for NAs
na_counts <- data_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0)

print("Missing Values Summary:")
print(na_counts)

# Log to file
log_file <- here("docs/missing_values_log.txt")
write_lines("Missing Values Summary:", log_file)
write_csv(na_counts, log_file, append = TRUE)

print(paste("Missing values logged to", log_file))
