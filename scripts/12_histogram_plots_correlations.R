# ==============================================================================
# 12_histogram_plots_correlations.R
# Author: Siddhant Rattu
# Last Updated: **August 2026**
# Purpose: Simple, clear exploratory data analysis for University Students.
#          Creates simple histograms, 2-variable scatter plots, and a clear
#          correlation bar chart showing what drives GBIF biodiversity records.
# Saved to: data/results/
# ==============================================================================

cat("==============================================================================\n")
cat("  SIMPLE EXPLORATORY DATA ANALYSIS & CORRELATIONS FOR UNIVERSITY STUDENTS     \n")
cat("==============================================================================\n\n")

# STEP 1: Load the final dataset
data_file <- ifelse(file.exists("data/results/Final_DATASET.csv"), 
                    "data/results/Final_DATASET.csv", 
                    "data/processed/Final_DATASET.csv")

cat("Step 1: Reading dataset from:", data_file, "\n")
df <- read.csv(data_file, stringsAsFactors = FALSE)

# Ensure results folder exists
if (!dir.exists("data/results")) dir.create("data/results", recursive = TRUE)

# STEP 2: Create Simple Histograms (How are the values distributed?)
cat("Step 2: Creating simple histograms...\n")

png("data/results/simple_histograms.png", width = 900, height = 600, res = 100)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Histogram 1: Log of GBIF Records
hist(log10(df$n_records_gbif + 1), 
     main = "1. GBIF Biodiversity Records", 
     xlab = "log10(GBIF Records)", 
     col = "#8E44AD", border = "white", breaks = 20)

# Histogram 2: Log of Total GDP
hist(log10(df$wb_gdp_total + 1), 
     main = "2. Total GDP (Wealth)", 
     xlab = "log10(GDP in USD)", 
     col = "#27AE60", border = "white", breaks = 20)

# Histogram 3: Biome Diversity Count
hist(df$biome_diversity_count, 
     main = "3. Biome Diversity (Ecoregions)", 
     xlab = "Number of Unique Ecoregions", 
     col = "#E67E22", border = "white", breaks = 15)

# Histogram 4: Protected Area Percentage
hist(df$terrestrial_protected_area_pct, 
     main = "4. Protected Area %", 
     xlab = "Protected Area % of Land", 
     col = "#16A085", border = "white", breaks = 15)

dev.off()
cat("  -> Saved: data/results/simple_histograms.png\n\n")


# STEP 3: Create Simple 2-Variable Scatter Plots
cat("Step 3: Creating 2-variable scatter plots with trendlines...\n")

png("data/results/simple_scatter_plots.png", width = 900, height = 600, res = 100)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Plot 1: GDP vs GBIF Records
valid1 <- !is.na(df$wb_gdp_total) & !is.na(df$n_records_gbif) & df$wb_gdp_total > 0 & df$n_records_gbif > 0
plot(log10(df$wb_gdp_total[valid1]), log10(df$n_records_gbif[valid1]),
     pch = 19, col = "#27AE60",
     main = "Wealth (GDP) vs Biodiversity Records",
     xlab = "log10(Total GDP USD)", ylab = "log10(GBIF Records)")
abline(lm(log10(df$n_records_gbif[valid1]) ~ log10(df$wb_gdp_total[valid1])), col = "black", lwd = 2)

# Plot 2: Population vs GBIF Records
valid2 <- !is.na(df$wb_population) & !is.na(df$n_records_gbif) & df$wb_population > 0 & df$n_records_gbif > 0
plot(log10(df$wb_population[valid2]), log10(df$n_records_gbif[valid2]),
     pch = 19, col = "#3498DB",
     main = "Population vs Biodiversity Records",
     xlab = "log10(Population)", ylab = "log10(GBIF Records)")
abline(lm(log10(df$n_records_gbif[valid2]) ~ log10(df$wb_population[valid2])), col = "black", lwd = 2)

# Plot 3: Biome Diversity vs GBIF Records
valid3 <- !is.na(df$biome_diversity_count) & !is.na(df$n_records_gbif) & df$n_records_gbif > 0
plot(df$biome_diversity_count[valid3], log10(df$n_records_gbif[valid3]),
     pch = 19, col = "#E67E22",
     main = "Biome Diversity vs Biodiversity Records",
     xlab = "Number of Unique Biomes", ylab = "log10(GBIF Records)")
abline(lm(log10(df$n_records_gbif[valid3]) ~ df$biome_diversity_count[valid3]), col = "black", lwd = 2)

# Plot 4: Protected Area % vs GBIF Records
valid4 <- !is.na(df$terrestrial_protected_area_pct) & !is.na(df$n_records_gbif) & df$n_records_gbif > 0
plot(df$terrestrial_protected_area_pct[valid4], log10(df$n_records_gbif[valid4]),
     pch = 19, col = "#16A085",
     main = "Protected Area % vs Biodiversity Records",
     xlab = "Protected Area %", ylab = "log10(GBIF Records)")
abline(lm(log10(df$n_records_gbif[valid4]) ~ df$terrestrial_protected_area_pct[valid4]), col = "black", lwd = 2)

dev.off()
cat("  -> Saved: data/results/simple_scatter_plots.png\n\n")


# STEP 4: Calculate Simple Correlations & Make Bar Chart
cat("Step 4: Calculating simple correlation coefficients (r)...\n")

# List of key variables to compare with GBIF records
vars_to_test <- c(
  "wb_gdp_total",
  "wb_population",
  "area_total_km2",
  "biome_diversity_count",
  "wb_research_gdp_pct",
  "wb_gdp_per_capita",
  "wb_researchers_per_million_people",
  "endemic_tetrapods_total",
  "wb_urbanisation_pct_urban",
  "terrestrial_protected_area_pct",
  "forest_cover_pct",
  "mean_latitude",
  "wb_population_density"
)

# Friendly names for students
friendly_names <- c(
  "Total GDP (Money)",
  "Population Size",
  "Surface Area (km2)",
  "Biome Diversity Count",
  "R&D Spending %",
  "GDP per Capita",
  "Researchers Density",
  "Endemic Species Count",
  "Urbanisation %",
  "Protected Area %",
  "Forest Cover %",
  "Latitude (Distance from Equator)",
  "Population Density"
)

r_values <- numeric(length(vars_to_test))
names(r_values) <- friendly_names

# Compute Pearson correlation r (log-scaling skewed variables)
log_gbif <- log10(df$n_records_gbif + 1)

for (i in 1:length(vars_to_test)) {
  var_col <- vars_to_test[i]
  x_val <- df[[var_col]]
  
  if (var_col %in% c("wb_gdp_total", "wb_population", "area_total_km2", "wb_gdp_per_capita", "wb_population_density", "endemic_tetrapods_total")) {
    x_val <- log10(x_val + 1)
  }
  
  valid <- !is.na(log_gbif) & !is.na(x_val)
  r_values[i] <- round(cor(log_gbif[valid], x_val[valid]), 2)
}

# Sort correlations from highest to lowest
sorted_r <- sort(r_values, decreasing = TRUE)

# Save Correlation Bar Plot for students
png("data/results/simple_correlation_bar_plot.png", width = 1000, height = 650, res = 100)
par(mar = c(5, 14, 4, 2))

# Color bars: Green for positive, Red for negative
bar_colors <- ifelse(sorted_r > 0, "#27AE60", "#E74C3C")

bars <- barplot(sorted_r, 
                horiz = TRUE, 
                las = 1, 
                col = bar_colors, 
                border = "white",
                main = "What Drives Global Biodiversity Data Distribution? (Correlation r)",
                xlab = "Pearson Correlation (r) with GBIF Record Volume",
                xlim = c(-0.3, 1.0))

# Add exact r value labels next to bars
text(x = sorted_r + ifelse(sorted_r >= 0, 0.04, -0.04), 
     y = bars, 
     labels = sprintf("%.2f", sorted_r), 
     cex = 0.9, font = 2)

grid(nx = NULL, ny = NA, col = "gray80", lty = "dotted")
abline(v = 0, col = "black", lwd = 2)

dev.off()
cat("  -> Saved: data/results/simple_correlation_bar_plot.png\n\n")

# Print Summary Table for Students in Terminal
cat("------------------------------------------------------------------------------\n")
cat("  SIMPLE SUMMARY TABLE FOR UNIVERSITY STUDENTS                                \n")
cat("------------------------------------------------------------------------------\n")
student_summary <- data.frame(
  Variable = names(sorted_r),
  Correlation_r = unname(sorted_r),
  Interpretation = ifelse(sorted_r > 0.6, "Very Strong Driver",
                   ifelse(sorted_r > 0.4, "Strong Driver",
                   ifelse(sorted_r > 0.2, "Moderate Driver", "Weak / Inverse Driver")))
)
print(student_summary, row.names = FALSE)

cat("==============================================================================\n")
cat(">>> DONE! Student-friendly plots saved in data/results/                       \n")
cat("==============================================================================\n")
