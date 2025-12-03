# 03_outlier_detection.R
# Logic: Area vs Population plots

library(tidyverse)
library(here)
library(ggplot2)

# Ensure output directories exist
if (!dir.exists(here("output/figures/scatterplots"))) {
  dir.create(here("output/figures/scatterplots"), recursive = TRUE)
}
if (!dir.exists(here("data/processed"))) {
  dir.create(here("data/processed"), recursive = TRUE)
}

data_clean <- read_csv(here("data/processed/biodiversity_clean_v1.csv"), show_col_types = FALSE)

# Check if columns exist
# Using names from variables_per_country_UPDATED which are likely wb_population, area_total_km2 (or from master)
# Master has area_total_km2. vars_updated has wb_population.

if("area_total_km2" %in% names(data_clean) & "wb_population" %in% names(data_clean)) {

  # Scatterplot
  p <- ggplot(data_clean, aes(x = area_total_km2, y = wb_population)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Area vs Population (Outlier Detection)", x = "Area (km2)", y = "Population")

  ggsave(here("output/figures/scatterplots/area_vs_pop_outliers.png"), plot = p)
  print("Outlier plot saved.")

  # Basic outlier detection (e.g., Z-score > 3)
  outliers <- data_clean %>%
    mutate(
      z_area = (area_total_km2 - mean(area_total_km2, na.rm = TRUE)) / sd(area_total_km2, na.rm = TRUE),
      z_pop = (wb_population - mean(wb_population, na.rm = TRUE)) / sd(wb_population, na.rm = TRUE)
    ) %>%
    filter(abs(z_area) > 3 | abs(z_pop) > 3)

  write_csv(outliers, here("data/processed/outliers_removed.csv"))
  print("Outliers saved to data/processed/outliers_removed.csv")

} else {
  warning("Columns for outlier detection (area_total_km2, wb_population) not found.")
  print(names(data_clean))
}
