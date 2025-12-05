library(WDI)
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(countrycode)
library(sf)
library(terra)
library(rnaturalearth)

# --- 1.2: Define File Structure & Timestamp ---
download_timestamp <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm")
data_dir <- "data"
raw_dir <- file.path(data_dir, "raw_data")
clean_dir <- file.path(data_dir, "clean_data")

# Create directories
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(clean_dir, showWarnings = FALSE, recursive = TRUE)

# --- 1.3: Create Master Country List ---
# We will use this to standardize all datasets.
# 'iso3c' is the best universal join key.
master_countries <- codelist %>%
  select(country.name.en, iso3c) %>%
  distinct() %>%
  filter(!is.na(iso3c))

print(paste(nrow(master_countries), "master countries loaded for standardization."))


#
# ======= SECTION 2: AUTOMATED DATA DOWNLOAD & CLEANING =======
#
# This section downloads, saves raw data, cleans, and saves clean data.
#

# --- 2.1: World Bank Data ---
print("--- Processing World Bank Data ---")
tryCatch({
  # Indicators to download (using a named vector for easy renaming)
  wb_indicators <- c(
    gdp_per_capita = "NY.GDP.PCAP.CD",
    gdp_total = "NY.GDP.MKTP.CD",
    gdp_research_pct = "GB.XPD.RSDV.GD.ZS",
    researchers_per_mil = "SP.POP.SCIE.RD.P6",
    population = "SP.POP.TOTL",
    area_surface = "AG.SRF.TOTL.K2",
    population_density = "EN.POP.DNST",
    urbanisation_pct = "SP.URB.TOTL.IN.ZS",
    forest_cover_pct = "AG.LND.FRST.ZS",
    biodiversity_aid_received = "DT.ODA.ODAT.BD.CD" # ODA received for biodiversity
  )
  
  # Download data from last 20 years
  wb_data_raw <- WDI(
    country = "all",
    indicator = wb_indicators,
    start = 2005,
    end = 2025,
    extra = TRUE
  )
  
  # Save raw data
  raw_wb_path <- file.path(raw_dir, paste0(download_timestamp, "_wb_raw.rds"))
  saveRDS(wb_data_raw, raw_wb_path)
  print(paste("Raw World Bank data saved to", raw_wb_path))
  
  # Clean data: Get the single *latest available* value for each indicator
  wb_data_clean <- wb_data_raw %>%
    filter(!is.na(iso3c)) %>% # Filter out aggregates (like 'World', 'Africa')
    pivot_longer(cols = all_of(names(wb_indicators)), names_to = "indicator", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(iso3c, indicator) %>%
    arrange(desc(year)) %>%
    slice_head(n = 1) %>% # Get the most recent non-NA year
    ungroup() %>%
    select(iso3c, indicator, value) %>%
    pivot_wider(names_from = "indicator", values_from = "value")
  
  # Save clean data
  clean_wb_path <- file.path(clean_dir, "wb_clean.rds")
  saveRDS(wb_data_clean, clean_wb_path)
  print(paste("Clean World Bank data saved to", clean_wb_path))
  
}, error = function(e) {
  print(paste("Error downloading World Bank data:", e$message))
})


# --- 2.2: Human Development Index (HDI) ---
print("--- Processing HDI Data ---")
tryCatch({
  hdi_url <- "https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv"
  raw_hdi_path <- file.path(raw_dir, paste0(download_timestamp, "_hdi_raw.csv"))
  
  # Download using httr for error checking
  response <- GET(hdi_url)
  stop_for_status(response) # Check for download errors
  write_file(response, raw_hdi_path)
  print(paste("Raw HDI data saved to", raw_hdi_path))
  
  # Clean data: Read the CSV and select the latest HDI value (2022)
  hdi_data_clean <- read_csv(raw_hdi_path, show_col_types = FALSE) %>%
    select(iso3c = iso3, hdi = hdi_2022) %>% # 2023/24 report has 2022 data
    filter(!is.na(iso3c))
  
  # Save clean data
  clean_hdi_path <- file.path(clean_dir, "hdi_clean.rds")
  saveRDS(hdi_data_clean, clean_hdi_path)
  print(paste("Clean HDI data saved to", clean_hdi_path))
  
}, error = function(e) {
  print(paste("Error downloading HDI data:", e$message))
})


# --- 2.3: Colonial Past (COLDAT) ---
print("--- Processing Colonial Past Data ---")
tryCatch({
  coldat_url <- "https://dataverse.harvard.edu/api/access/datafile/3476840"
  raw_coldat_path <- file.path(raw_dir, paste0(download_timestamp, "_coldat_raw.tab"))
  
  response <- GET(coldat_url)
  stop_for_status(response)
  write_file(response, raw_coldat_path)
  print(paste("Raw COLDAT data saved to", raw_coldat_path))
  
  # Clean data: Read the .tab file, map country codes, and aggregate history
  coldat_data_clean <- read_delim(raw_coldat_path, delim = "\t", show_col_types = FALSE) %>%
    # Standardize country using Correlates of War code
    mutate(iso3c = countrycode(cowcode, "cown", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    # Consolidate history (e.g., if colonized by multiple powers)
    group_by(iso3c) %>%
    summarise(colonial_past = paste(unique(col_hist), collapse = "; ")) %>%
    ungroup()
  
  # Save clean data
  clean_coldat_path <- file.path(clean_dir, "coldat_clean.rds")
  saveRDS(coldat_data_clean, clean_coldat_path)
  print(paste("Clean COLDAT data saved to", clean_coldat_path))
  
}, error = function(e) {
  print(paste("Error downloading COLDAT data:", e$message))
})


# --- 2.4: Open Knowledge Index (Discontinued) ---
print("--- Processing Open Knowledge Index Data ---")
tryCatch({
  oki_url <- "http://index.okfn.org/api/entries.csv"
  raw_oki_path <- file.path(raw_dir, paste0(download_timestamp, "_oki_raw.csv"))
  
  response <- GET(oki_url)
  stop_for_status(response)
  write_file(response, raw_oki_path)
  print(paste("Raw OKI data saved to", raw_oki_path))
  
  # Clean data: Calculate the average score per country
  oki_data_clean <- read_csv(raw_oki_path, show_col_types = FALSE) %>%
    # Standardize country using 2-letter code
    mutate(iso3c = countrycode(place, "iso2c", "iso3c")) %>%
    filter(!is.na(iso3c)) %>%
    group_by(iso3c) %>%
    summarise(open_knowledge_index = mean(score, na.rm = TRUE)) %>%
    ungroup()
  
  # Save clean data
  clean_oki_path <- file.path(clean_dir, "oki_clean.rds")
  saveRDS(oki_data_clean, clean_oki_path)
  print(paste("Clean OKI data saved to", clean_oki_path))
  
}, error = function(e) {
  print(paste("Error downloading OKI data:", e$message))
})


#
# ======= SECTION 3: SEMI-AUTOMATED (MANUAL DOWNLOAD) DATA PROCESSING =======
#
# This section will *process* the large files you downloaded manually.
# It checks if the files exist in 'data/raw_data/' before running.
#

# --- 3.1: Get Master Country Shapes ---
# We need this for all spatial processing
print("Loading master country shapes...")
world_countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3) %>%
  rename(iso3c = iso_a3) %>%
  filter(iso3c %in% master_countries$iso3c) %>%
  st_make_valid() # Ensure geometries are valid


# --- 3.2: Biome Diversity (Ecoregions) ---
print("--- Processing Biome (Ecoregion) Data ---")
biome_shapefile_path <- file.path(raw_dir, "ecoregions", "Ecoregions2017.shp")

if (file.exists(biome_shapefile_path)) {
  tryCatch({
    print("Found Biome shapefile, processing...")
    biomes_sf <- st_read(biome_shapefile_path) %>% st_make_valid()
    
    # Project both layers to a common CRS (WGS84)
    biomes_sf_wgs84 <- st_transform(biomes_sf, st_crs(world_countries_sf))
    
    # Intersect (this can take a few minutes)
    country_biomes <- st_intersection(world_countries_sf, biomes_sf_wgs84)
    
    # Clean data: Count the number of distinct ecoregions per country
    biomes_clean <- country_biomes %>%
      as.data.frame() %>% # Convert from sf to dataframe
      group_by(iso3c) %>%
      summarise(biome_diversity_count = n_distinct(ECO_ID)) %>% # ECO_ID is the unique biome ID
      ungroup()
    
    # Save clean data
    clean_biomes_path <- file.path(clean_dir, "biomes_clean.rds")
    saveRDS(biomes_clean, clean_biomes_path)
    print(paste("Clean Biome data saved to", clean_biomes_path))
    
  }, error = function(e) {
    print(paste("Error processing Biome data:", e$message))
  })
} else {
  print(paste("SKIPPING Biomes: File not found at", biome_shapefile_path))
  print("Please download and place it in 'data/raw_data/ecoregions/'")
}


# --- 3.3: Climatic Zone (Köppen-Geiger) ---
print("--- Processing Climate (Köppen-Geiger) Data ---")
climate_raster_path <- file.path(raw_dir, "climate", "Beck_KG_V1_present_0p0083.tif") # Update if your filename differs

if (file.exists(climate_raster_path)) {
  tryCatch({
    print("Found Climate raster, processing...")
    climate_raster <- rast(climate_raster_path)
    
    # Project countries to match raster CRS
    world_countries_proj <- st_transform(world_countries_sf, st_crs(climate_raster))
    
    # Clean data: Find the dominant (modal) climate zone value per country
    # This function is fast and efficient
    climate_zones <- terra::extract(climate_raster, vect(world_countries_proj), fun = 'modal', na.rm = TRUE)
    
    climate_clean <- data.frame(
      iso3c = world_countries_proj$iso3c[climate_zones$ID],
      dominant_climate_zone = climate_zones[, 2] # The second column has the zone value
    )
    
    # Save clean data
    clean_climate_path <- file.path(clean_dir, "climate_clean.rds")
    saveRDS(climate_clean, clean_climate_path)
    print(paste("Clean Climate data saved to", clean_climate_path))
    
  }, error = function(e) {
    print(paste("Error processing Climate data:", e$message))
  })
} else {
  print(paste("SKIPPING Climate: File not found at", climate_raster_path))
  print("Please download and place it in 'data/raw_data/climate/'")
}


# --- 3.4: Protected Area (WDPA) Coverage ---
# **WARNING: THIS IS EXTREMELY SLOW (hours) and memory-intensive.**
# It is commented out by default. Set RUN_WDPA_ANALYSIS to TRUE to run it.

RUN_WDPA_ANALYSIS <- FALSE 
print("--- Processing Protected Area (WDPA) Data ---")
wdpa_shapefile_path <- file.path(raw_dir, "wdpa", "WDPA_Nov2025_Public.shp") # Update to your filename

if (RUN_WDPA_ANALYSIS && file.exists(wdpa_shapefile_path)) {
  tryCatch({
    print("Found WDPA shapefile, processing... (This will take a very long time)")
    wdpa_sf <- st_read(wdpa_shapefile_path) %>% st_make_valid()
    
    # Ensure CRS matches
    wdpa_sf_proj <- st_transform(wdpa_sf, st_crs(world_countries_sf))
    
    # Calculate country areas first (in an equal-area projection)
    world_countries_ea <- st_transform(world_countries_sf, "+proj=moll")
    country_areas <- world_countries_ea %>%
      mutate(country_area_m2 = st_area(.)) %>%
      as.data.frame() %>%
      select(iso3c, country_area_m2)
    
    # Intersect (THE SLOW PART)
    pa_by_country <- st_intersection(world_countries_sf, wdpa_sf_proj)
    
    # Calculate area of protected areas per country
    pa_areas <- pa_by_country %>%
      st_transform("+proj=moll") %>% # Transform to equal-area
      mutate(area_m2 = st_area(.)) %>%
      as.data.frame() %>%
      group_by(iso3c) %>%
      summarise(total_pa_area_m2 = sum(area_m2, na.rm = TRUE))
    
    # Clean data: Combine and calculate percentage
    wdpa_clean <- country_areas %>%
      left_join(pa_areas, by = "iso3c") %>%
      mutate(total_pa_area_m2 = tidyr::replace_na(total_pa_area_m2, 0)) %>%
      mutate(pa_coverage_pct = (total_pa_area_m2 / country_area_m2) * 100) %>%
      select(iso3c, pa_coverage_pct)
    
    # Save clean data
    clean_wdpa_path <- file.path(clean_dir, "wdpa_clean.rds")
    saveRDS(wdpa_clean, clean_wdpa_path)
    print(paste("Clean WDPA data saved to", clean_wdpa_path))
    
  }, error = function(e) {
    print(paste("Error processing WDPA data:", e$message))
  })
} else {
  print(paste("SKIPPING WDPA Analysis: File not found or RUN_WDPA_ANALYSIS is FALSE."))
}


#
# ======= SECTION 4: FINAL COMPILATION =======
#

print("--- Compiling Final Dataset ---")

# Get list of all clean .rds files
clean_files <- list.files(clean_dir, full.names = TRUE, pattern = "\\.rds$")

# Read all clean files into a list, skipping any that failed
list_of_dfs <- lapply(clean_files, function(f) {
  tryCatch({
    readRDS(f)
  }, error = function(e) {
    print(paste("Could not read", f, ":", e$message))
    return(NULL)
  })
})

# Filter out any NULLs from failed reads
list_of_dfs <- Filter(Negate(is.null), list_of_dfs)

# Start with the master list and join everything
variables_per_country <- Reduce(
  function(x, y) full_join(x, y, by = "iso3c"),
  list_of_dfs,
  init = master_countries
)

# Standardize the final country name to the one from our master list
variables_per_country <- variables_per_country %>%
  select(-country.name.en) %>%
  left_join(master_countries, by = "iso3c") %>%
  select(iso3c, country_name = country.name.en, everything())

# Save the final compiled dataframe
final_rds_path <- file.path(data_dir, "variables_per_country.rds")
final_csv_path <- file.path(data_dir, "variables_per_country.csv")

saveRDS(variables_per_country, final_rds_path)
write_csv(variables_per_country, final_csv_path)

print(paste("FINAL COMPILATION COMPLETE!"))
print(paste("Data saved to", final_rds_path, "and", final_csv_path))

# Display a summary of the final data
print(head(variables_per_country))
