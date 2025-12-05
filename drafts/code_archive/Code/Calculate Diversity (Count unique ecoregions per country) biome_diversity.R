# ==============================================================================
# DIAGNOSTIC & REPAIR SCRIPT
# ==============================================================================
library(sf)
library(dplyr)
library(rnaturalearth)
library(readr)
library(units)

# 1. SETUP
# ------------------------------------------------------------------------------
# Define your main folder
base_dir <- "C:/Users/Asus/Desktop/Thesis Code R/Biodiversity data/Data/Polygons"

# Create if missing
if(!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

message("----------------------------------------------------------------")
message(">>> STARTING DIAGNOSTICS IN: ", base_dir)
message("----------------------------------------------------------------")

# List all files in the folder to see what R sees
all_files <- list.files(base_dir, recursive = TRUE, full.names = TRUE)
message("Found ", length(all_files), " files in total.")

# 2. LOAD BASE MAP
# ------------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, iso_a3, geometry) %>%
  st_make_valid()
world$area_km2 <- drop_units(st_area(st_transform(world, "ESRI:54009"))) / 1e6

# Initialize columns with 0 or NA
world$biome_count <- NA
world$pa_pct <- NA
world$forest_pct <- NA

# 3. FIX BIOMES (Smart Search)
# ------------------------------------------------------------------------------
message("\n>>> LOOKING FOR ECOREGIONS...")

# Search for ANY shapefile with "Eco" in the name
eco_candidates <- all_files[grep("Eco.*\\.shp$", all_files, ignore.case = TRUE)]

if (length(eco_candidates) > 0) {
  target_file <- eco_candidates[1] # Take the first one found
  message("   [SUCCESS] Found file: ", basename(target_file))
  
  try({
    eco <- st_read(target_file, quiet = TRUE)
    eco <- st_make_valid(eco)
    eco <- st_transform(eco, st_crs(world))
    
    # Check for the ID column (it changes names in different versions)
    # Usually ECO_ID, eco_id, or OBJECTID
    id_col <- names(eco)[grep("ID|CODE", names(eco), ignore.case=TRUE)][1]
    
    if(!is.na(id_col)) {
      message("   Calculated using column: ", id_col)
      eco_int <- st_intersection(world, eco)
      stats <- eco_int %>% 
        group_by(name) %>% 
        summarise(count = n_distinct(.data[[id_col]])) %>% 
        st_drop_geometry()
      
      world <- left_join(world, stats, by = "name") %>%
        mutate(biome_count = ifelse(is.na(count), 0, count)) %>%
        select(-count)
    }
  })
} else {
  message("   [FAILURE] No 'Ecoregions' shapefile found.")
  message("   ACTION: Download Ecoregions2017.zip, EXTRACT it, and put .shp in the folder.")
}

# 4. FIX PROTECTED AREAS (Natural Earth Download)
# ------------------------------------------------------------------------------
message("\n>>> ATTEMPTING PROTECTED AREAS DOWNLOAD...")

tryCatch({
  # Download directly
  pa <- ne_download(scale = 10, type = "parks_and_protected_lands", 
                    category = "cultural", returnclass = "sf")
  
  if(!is.null(pa)) {
    message("   [SUCCESS] Downloaded Natural Earth PA data.")
    pa <- st_make_valid(pa)
    pa_ea <- st_transform(pa, "ESRI:54009")
    world_ea <- st_transform(world, "ESRI:54009")
    
    pa_int <- st_intersection(world_ea, pa_ea)
    
    stats_pa <- pa_int %>%
      group_by(name) %>%
      summarise(pa_area = sum(drop_units(st_area(geometry)), na.rm=TRUE)/1e6) %>%
      st_drop_geometry()
    
    world <- left_join(world, stats_pa, by = "name") %>%
      mutate(pa_pct = (ifelse(is.na(pa_area), 0, pa_area) / area_km2) * 100) %>%
      select(-pa_area)
    
    world$pa_pct[world$pa_pct > 100] <- 100
  }
}, error = function(e) {
  message("   [FAILURE] Could not download PA data. Check internet.")
  message("   Error: ", e$message)
})

# 5. FIX FOREST (Smart CSV Search)
# ------------------------------------------------------------------------------
message("\n>>> LOOKING FOR FOREST CSV...")

# Search for ANY CSV with "Forest" or "API" (World bank default name)
csv_candidates <- all_files[grep("Forest|API_AG", all_files, ignore.case = TRUE)]
# Filter only .csv
csv_candidates <- csv_candidates[grep("\\.csv$", csv_candidates)]

if(length(csv_candidates) > 0) {
  target_csv <- csv_candidates[1]
  message("   [SUCCESS] Found CSV: ", basename(target_csv))
  
  try({
    # World Bank CSVs have 4 header rows to skip
    df <- read_csv(target_csv, skip = 4, show_col_types = FALSE)
    
    # Find the last column automatically (usually the most recent year)
    last_col_idx <- ncol(df)
    last_col_name <- names(df)[last_col_idx]
    message("   Using data from year/column: ", last_col_name)
    
    # Clean and Join
    clean_forest <- df %>%
      select(iso_a3 = `Country Code`, val = all_of(last_col_idx))
    
    world <- left_join(world, clean_forest, by = "iso_a3") %>%
      mutate(forest_pct = val) %>%
      select(-val)
  })
} else {
  message("   [FAILURE] No Forest CSV found.")
  message("   ACTION: Download World Bank Forest CSV and put it in the folder.")
}

# 6. SAVE CHECK
# ------------------------------------------------------------------------------
message("\n>>> SAVING FINAL RESULTS...")
final_path <- file.path(base_dir, "Biodiversity_Variables_FIXED.csv")
write.csv(st_drop_geometry(world), final_path, row.names = FALSE)

message("----------------------------------------------------------------")
message("Check the file: ", final_path)
message("If columns are still NA, read the [FAILURE] messages above.")
message("----------------------------------------------------------------")