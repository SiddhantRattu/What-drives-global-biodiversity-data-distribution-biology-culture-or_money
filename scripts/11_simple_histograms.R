# ==============================================================================
# 11_simple_histograms.R
# Author: Siddhant Rattu
# Last Updated: **August 2026**
# Purpose: Generate simple, intuitive histograms showing the distribution of key
#          variables across countries.
# Results saved to: data/results/simple_histograms.png
# ==============================================================================

cat(">>> Generating Simple Exploratory Histograms...\n")

# 1. Load Data
data_path <- ifelse(file.exists("data/results/Final_DATASET.csv"), 
                    "data/results/Final_DATASET.csv", 
                    "data/processed/Final_DATASET.csv")

cat(">>> Loading data from:", data_path, "\n")
df <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)

# Ensure results directory exists
results_dir <- "data/results"
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# Coerce columns to numeric safely
num_cols <- c(
  "n_records_gbif",
  "wb_gdp_total",
  "wb_gdp_per_capita",
  "wb_population",
  "area_total_km2",
  "biome_diversity_count",
  "terrestrial_protected_area_pct",
  "forest_cover_pct"
)

for (col in num_cols) {
  if (col %in% colnames(df)) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
}

# 2. Generate Simple Histograms Grid
png(file.path(results_dir, "simple_histograms.png"), width = 1200, height = 800, res = 120)
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))

# Simple Histograms
hist(log10(df$n_records_gbif + 1), main = "GBIF Records (Log10)", xlab = "log10(GBIF Records)", col = "#8E44AD", border = "white", breaks = 20)
hist(log10(df$wb_gdp_total + 1), main = "Total GDP (Log10)", xlab = "log10(Total GDP USD)", col = "#27AE60", border = "white", breaks = 20)
hist(log10(df$wb_gdp_per_capita + 1), main = "GDP Per Capita (Log10)", xlab = "log10(GDP/Capita USD)", col = "#2ECC71", border = "white", breaks = 20)
hist(log10(df$wb_population + 1), main = "Population (Log10)", xlab = "log10(Population)", col = "#3498DB", border = "white", breaks = 20)
hist(log10(df$area_total_km2 + 1), main = "Surface Area (Log10)", xlab = "log10(Area km2)", col = "#2980B9", border = "white", breaks = 20)
hist(df$biome_diversity_count, main = "Biome Diversity Count", xlab = "Unique Ecoregions Count", col = "#E67E22", border = "white", breaks = 15)
hist(df$terrestrial_protected_area_pct, main = "Protected Area %", xlab = "Protected Area %", col = "#16A085", border = "white", breaks = 15)
hist(df$forest_cover_pct, main = "Forest Cover %", xlab = "Forest Cover %", col = "#D35400", border = "white", breaks = 15)

dev.off()

cat("==============================================================================\n")
cat(">>> SIMPLE HISTOGRAMS GENERATED SUCCESSFULLY!\n")
cat(">>> Author: Siddhant Rattu | Date: **August 2026**\n")
cat(">>> Output saved to: data/results/simple_histograms.png\n")
cat("==============================================================================\n")
