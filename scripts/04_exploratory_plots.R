# 04_exploratory_plots.R
# Logic: Histograms for distributions

library(tidyverse)
library(here)
library(ggplot2)

# Ensure output directories exist
if (!dir.exists(here("output/figures/histograms"))) {
  dir.create(here("output/figures/histograms"), recursive = TRUE)
}
if (!dir.exists(here("output/tables"))) {
  dir.create(here("output/tables"), recursive = TRUE)
}

data_clean <- read_csv(here("data/processed/biodiversity_clean_v1.csv"), show_col_types = FALSE)

# Histogram: Population
if("wb_population" %in% names(data_clean)) {
  p_pop <- ggplot(data_clean, aes(x = wb_population)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    theme_minimal() +
    labs(title = "Distribution of Population", x = "Population", y = "Count")

  ggsave(here("output/figures/histograms/dist_population.png"), plot = p_pop)
}

# Histogram: Area
if("area_total_km2" %in% names(data_clean)) {
  p_area <- ggplot(data_clean, aes(x = area_total_km2)) +
    geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
    theme_minimal() +
    labs(title = "Distribution of Area", x = "Area (km2)", y = "Count")

  ggsave(here("output/figures/histograms/dist_area.png"), plot = p_area)
}

# Summary Stats Table
summary_stats <- data_clean %>%
  summarise(across(where(is.numeric), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "value")

write_csv(summary_stats, here("output/tables/summary_stats.csv"))
print("Exploratory plots and summary stats generated.")
