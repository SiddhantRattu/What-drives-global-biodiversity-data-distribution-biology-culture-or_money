# ==============================================================================
# 10_final_output_aggregation.R
# Author: Siddhant Rattu
# Last Updated: **August 2026**
# Data Range: **2010** to **2024**
# Purpose: Merge all datasets into final master CSV dataset and color-coded Excel report
#          (Built with Base R for 100% execution reliability)
# ==============================================================================

cat(">>> Starting Final Output Aggregation...\n")

# Ensure output directories exist
if (!dir.exists("data/results")) dir.create("data/results", recursive = TRUE)
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# Input file resolution
input_file <- ifelse(file.exists("data/processed/Final_DATASET.csv"), 
                     "data/processed/Final_DATASET.csv", 
                     ifelse(file.exists("docs/Final_DATASET.csv"), "docs/Final_DATASET.csv", "Final_DATASET.csv"))

cat(">>> Reading data from:", input_file, "\n")
df <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

# Ensure standardized PA % column
if ("pa_coverage_pct" %in% colnames(df) && !"terrestrial_protected_area_pct" %in% colnames(df)) {
  df$terrestrial_protected_area_pct <- df$pa_coverage_pct
  df$pa_coverage_pct <- NULL
}

# Ensure standardized percentage variable suffixes
pct_mappings <- c(
  "wb_percent_gdp_for_research" = "wb_research_gdp_pct",
  "wb_urbanisation_percent_urban" = "wb_urbanisation_pct_urban",
  "main_religion_percent" = "main_religion_pct",
  "percent_atheists_non_religious" = "atheists_non_religious_pct",
  "forest_cover_percent" = "forest_cover_pct"
)

for (old_name in names(pct_mappings)) {
  new_name <- pct_mappings[[old_name]]
  if (old_name %in% colnames(df)) {
    colnames(df)[colnames(df) == old_name] <- new_name
  }
}

# Write master final CSV to results and processed directories
cat(">>> Saving final master dataset CSV to data/results/Final_DATASET.csv...\n")
write.csv(df, "data/results/Final_DATASET.csv", row.names = FALSE, na = "NA")
write.csv(df, "data/processed/Final_DATASET.csv", row.names = FALSE, na = "NA")

# Attempt Excel generation if openxlsx is installed
excel_created <- FALSE
if (requireNamespace("openxlsx", quietly = TRUE)) {
  tryCatch({
    library(openxlsx)
    wb <- createWorkbook()
    addWorksheet(wb, "Final Data")
    writeData(wb, "Final Data", df)
    
    # Define Column Categories for styling
    vars_money     <- c("wb_gdp_per_capita", "wb_gdp_total", "wb_research_gdp_pct", "wb_researchers_per_million_people", "wb_total_researchers_count", "wb_total_research_spending_usd", "foreign_aid_oda", "hdi_proxy_gdp", "hdi_proxy_life")
    vars_hc        <- c("main_language", "main_religion", "main_religion_pct", "atheists_non_religious_pct", "atheist_count", "ever_colonized", "colonizer", "is_english_main")
    vars_structure <- c("wb_population", "area_total_km2", "wb_population_density", "wb_urbanisation_pct_urban", "is_main_english_spanish_arabic")
    vars_bio       <- c("n_records_gbif", "biome_diversity_count", "forest_cover_pct", "mean_latitude", "endemic_tetrapods_total", "terrestrial_protected_area_pct")
    vars_basic     <- c("iso3c", "country")

    style_money     <- createStyle(fgFill = "#C6EFCE", textDecoration = "bold", border = "TopBottomLeftRight")
    style_hc        <- createStyle(fgFill = "#FFCC99", textDecoration = "bold", border = "TopBottomLeftRight")
    style_structure <- createStyle(fgFill = "#99CCFF", textDecoration = "bold", border = "TopBottomLeftRight")
    style_bio       <- createStyle(fgFill = "#CC99FF", textDecoration = "bold", border = "TopBottomLeftRight")
    style_basic     <- createStyle(fgFill = "#D3D3D3", textDecoration = "bold", border = "TopBottomLeftRight")
    style_def       <- createStyle(textDecoration = "bold", border = "TopBottomLeftRight")

    for (i in seq_along(names(df))) {
      col <- names(df)[i]
      if (col %in% vars_money) addStyle(wb, "Final Data", style = style_money, rows = 1, cols = i)
      else if (col %in% vars_hc) addStyle(wb, "Final Data", style = style_hc, rows = 1, cols = i)
      else if (col %in% vars_structure) addStyle(wb, "Final Data", style = style_structure, rows = 1, cols = i)
      else if (col %in% vars_bio) addStyle(wb, "Final Data", style = style_bio, rows = 1, cols = i)
      else if (col %in% vars_basic) addStyle(wb, "Final Data", style = style_basic, rows = 1, cols = i)
      else addStyle(wb, "Final Data", style = style_def, rows = 1, cols = i)
    }

    saveWorkbook(wb, "data/results/Final_Dataset_Complete.xlsx", overwrite = TRUE)
    excel_created <- TRUE
    cat(">>> Saved color-coded workbook to: data/results/Final_Dataset_Complete.xlsx\n")
  }, error = function(e) {
    cat(">>> Excel generation notice:", e$message, "\n")
  })
}

if (!excel_created) {
  cat(">>> Saving CSV version of complete dataset to: data/results/Final_Dataset_Complete.csv\n")
  write.csv(df, "data/results/Final_Dataset_Complete.csv", row.names = FALSE, na = "NA")
}

cat("==============================================================================\n")
cat(">>> FINAL OUTPUT AGGREGATION COMPLETE!\n")
cat(">>> Author: Siddhant Rattu | Date: **August 2026**\n")
cat(">>> Master outputs available in data/results/\n")
cat("==============================================================================\n")