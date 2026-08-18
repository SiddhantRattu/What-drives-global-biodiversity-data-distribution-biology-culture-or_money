# ==============================================================================
# 05_standardize_variables_and_pa_pct.R
# Author: Siddhant Rattu
# Last Updated: **August 2026**
# Purpose: Convert Protected Area (PA) to percentage (terrestrial_protected_area_pct)
#          and unify all percentage column criteria to standard `_pct` suffix.
# Built with Base R for maximum compatibility (zero external package dependencies).
# ==============================================================================

cat(">>> Running Variable Standardization & PA Percentage Conversion in Base R...\n")

input_file <- ifelse(file.exists("data/processed/Final_DATASET.csv"), "data/processed/Final_DATASET.csv", "docs/Final_DATASET.csv")

if (file.exists(input_file)) {
  df <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
  cols <- colnames(df)
  
  # Column rename mapping for unified _pct criteria
  name_map <- c(
    "wb_percent_gdp_for_research" = "wb_research_gdp_pct",
    "wb_percent_gdp_for_research_new" = "wb_research_gdp_pct",
    "wb_urbanisation_percent_urban" = "wb_urbanisation_pct_urban",
    "wb_urbanisation_percent_urban_new" = "wb_urbanisation_pct_urban",
    "main_religion_percentage" = "main_religion_pct",
    "% atheists/non-religious" = "atheists_non_religious_pct",
    "x_atheists_non_religious" = "atheists_non_religious_pct",
    "biome_diversity_count count of unique ecosystems in the country" = "biome_diversity_count"
  )
  
  for (old_name in names(name_map)) {
    cols[cols == old_name] <- name_map[old_name]
  }
  colnames(df) <- cols
  
  # Standardize Protected Area (PA) percentage
  if ("pa_coverage_pct" %in% colnames(df)) {
    if (!"terrestrial_protected_area_pct" %in% colnames(df)) {
      df$terrestrial_protected_area_pct <- df$pa_coverage_pct
    } else {
      df$terrestrial_protected_area_pct[is.na(df$terrestrial_protected_area_pct)] <- df$pa_coverage_pct[is.na(df$terrestrial_protected_area_pct)]
    }
  }
  
  # Remove redundant pa_coverage_pct if terrestrial_protected_area_pct exists
  if ("pa_coverage_pct" %in% colnames(df) && "terrestrial_protected_area_pct" %in% colnames(df)) {
    df$pa_coverage_pct <- NULL
  }
  
  # Ensure processed output directory exists
  if (!dir.exists("data/processed")) {
    dir.create("data/processed", recursive = TRUE)
  }
  
  # Save standardized datasets safely to data/results/ and data/processed/
  if (!dir.exists("data/results")) dir.create("data/results", recursive = TRUE)
  if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
  
  tryCatch({
    write.csv(df, "data/results/Final_DATASET.csv", row.names = FALSE, na = "NA")
  }, error = function(e) warning("Could not write to data/results/Final_DATASET.csv: ", e$message))
  
  tryCatch({
    write.csv(df, "data/processed/Final_DATASET.csv", row.names = FALSE, na = "NA")
  }, error = function(e) warning("Could not write to data/processed/Final_DATASET.csv: ", e$message))
  
  cat(">>> SUCCESS: Datasets updated and standardized successfully!\n")
  cat(">>> Protected Area standardized to: 'terrestrial_protected_area_pct'\n")
  cat(">>> All percentage variables unified under '_pct'\n")
} else {
  warning("Input file docs/Final_DATASET.csv not found.")
}
