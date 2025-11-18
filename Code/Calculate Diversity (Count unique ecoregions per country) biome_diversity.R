library(sf)             
library(terra)          # For raster handling (forest cover)
library(dplyr)          # For data manipulation
library(rnaturalearth)  # To get country boundaries
library(units)          # To handle area units (m^2 to km^2)

# Ensure spherical geometry is off if you experience issues, 
# though usually keeping it on is better for accuracy.
sf_use_s2(FALSE)

# 1. Load world countries
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, iso_a3, geometry) %>%
  st_make_valid() # Fix potential topology errors

# Check the CRS (Coordinate Reference System). 
# For area calculations, an Equal Area projection (like Mollweide) is preferred, 
# but for simple intersections, WGS84 (EPSG:4326) is often used initially.

# Load your Ecoregions shapefile
# ecoregions <- st_read("path/to/Ecoregions2017.shp") 

# --- DUMMY DATA FOR DEMONSTRATION ---
# Creating a fake ecoregion polygon to show the logic
p1 <- st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))))
p2 <- st_polygon(list(rbind(c(10,0), c(20,0), c(20,10), c(10,10), c(10,0))))
ecoregions <- st_sf(ECO_ID = c(1, 2), geometry = st_sfc(p1, p2), crs = st_crs(world))
# ------------------------------------

# 1. Ensure CRS matches
ecoregions <- st_transform(ecoregions, st_crs(world))

# 2. Perform Spatial Intersection
# This splits the ecoregions by country borders
intersection_eco <- st_intersection(world, ecoregions)

# 3. Calculate Diversity (Count unique ecoregions per country)
biome_diversity <- intersection_eco %>%
  group_by(name) %>% # Group by Country Name
  summarise(
    num_ecoregions = n_distinct(ECO_ID) # Count unique IDs
  ) %>%
  st_drop_geometry()

# 4. Merge back to main world data
world_data <- left_join(world, biome_diversity, by = "name")

# NAs mean 0 ecoregions found (likely small islands or mismatch), replace with 0
world_data$num_ecoregions[is.na(world_data$num_ecoregions)] <- 0