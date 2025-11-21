# ---------------------------------------------------------
# Load Libraries
# ---------------------------------------------------------
library(tidyverse)
library(readxl)
library(rvest)

# ---------------------------------------------------------
# 1. Load Dataset (Excel)
# ---------------------------------------------------------
file_path <- "Final_Dataset_Complete.xlsx"

df <- read_excel(file_path)

cat("\nColumns found in dataset:\n")
print(names(df))

# ---------------------------------------------------------
# 2. Validate Required Columns
# ---------------------------------------------------------
required_cols <- c("country", "wb_population", "area_total_km2")

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(paste("ERROR: These required columns are missing from your Excel file:", 
             paste(missing_cols, collapse = ", ")))
}

# ---------------------------------------------------------
# 3. Compute Population Density
# ---------------------------------------------------------
df <- df %>%
  mutate(
    wb_population_density = wb_population / area_total_km2
  )

# ---------------------------------------------------------
# 4. Scrape Open Knowledge Score from ODIN
# ---------------------------------------------------------
cat("\nFetching ODIN data...\n")

url <- "https://odin.opendatawatch.com/report/rankings"

page <- read_html(url)

odin_table <- page %>%
  html_node("table") %>%
  html_table()

odin_clean <- odin_table %>%
  select(
    country = Country,
    open_knowledge_score = Overall
  ) %>%
  mutate(
    open_knowledge_score = as.numeric(as.character(open_knowledge_score))
  )

cat("Successfully fetched scores for", nrow(odin_clean), "countries.\n")

# ---------------------------------------------------------
# 5. Merge ODIN Scores into Main Dataset
# ---------------------------------------------------------

# Only remove open_knowledge_score if it already exists
if ("open_knowledge_score" %in% names(df)) {
  df <- df %>% select(-open_knowledge_score)
}

df_updated <- df %>%
  left_join(odin_clean, by = "country")

# ---------------------------------------------------------
# 6. Preview Results
# ---------------------------------------------------------
cat("\nPreview of Updated Data:\n")
print(
  df_updated %>%
    select(country, wb_population, wb_population_density, open_knowledge_score) %>%
    head(15)
)

# ---------------------------------------------------------
# 7. Save Output File
# ---------------------------------------------------------
output_file <- "Final_Dataset_Calculated.csv"
write_csv(df_updated, output_file)

cat("\nFile successfully saved as:", output_file, "\n")
