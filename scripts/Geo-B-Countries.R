
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("purrr")) install.packages("purrr")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")

library(httr)
library(jsonlite)
library(purrr)
library(readr)
library(dplyr)


url <- "https://www.geoboundaries.org/api/current/gbOpen/ALL/ADM0/"
response <- GET(url)


if (http_error(response)) {
  stop("Failed to retrieve data from the geoBoundaries API. Status code: ", status_code(response))
}


json_content <- content(response, "text", encoding = "UTF-8")
metadata_list <- fromJSON(json_content, flatten = TRUE)


print("Structure of metadata_list:")
print(str(metadata_list))
print("Names of metadata_list:")
print(names(metadata_list))

# 4. Extract country data - the response is likely a data frame or named list
# Try different approaches based on the actual structure
if (is.data.frame(metadata_list)) {
  
  country_data_df <- metadata_list %>%
    select(any_of(c("boundaryName", "boundaryID", "shapeName", "shapeID", "iso3", "boundaryISO"))) %>%
    rename(
      Country = if("boundaryName" %in% names(.)) "boundaryName" else if("shapeName" %in% names(.)) "shapeName" else names(.)[1],
      BoundaryID = if("boundaryID" %in% names(.)) "boundaryID" else if("shapeID" %in% names(.)) "shapeID" else if("boundaryISO" %in% names(.)) "boundaryISO" else names(.)[2]
    )
} else if (is.list(metadata_list) && length(metadata_list) > 0) {
  
  if (length(metadata_list) == 1 && is.list(metadata_list[[1]])) {
   
    country_data_list <- metadata_list[[1]]
  } else {
    country_data_list <- metadata_list
  }
  
  # Extract country information using a more robust approach
  country_data_df <- map_dfr(country_data_list, function(x) {
    if (is.list(x)) {
      
      country_name <- x$boundaryName %||% x$shapeName %||% x$name %||% x$iso3 %||% NA_character_
      boundary_id <- x$boundaryID %||% x$shapeID %||% x$boundaryISO %||% x$iso3 %||% NA_character_
      
      tibble(
        Country = country_name,
        BoundaryID = boundary_id
      )
    } else {
     
      tibble(
        Country = NA_character_,
        BoundaryID = NA_character_
      )
    }
  })
} else {
  stop("Unexpected data structure returned from API")
}

# 5. Filter out rows with NA values
country_data_df <- country_data_df %>%
  filter(!is.na(Country) & !is.na(BoundaryID))

# 6. Display some results for verification
print(paste("Total countries found:", nrow(country_data_df)))
print("First 10 countries:")
print(head(country_data_df, 10))

# 7. Save the data frame to a CSV file
write_csv(country_data_df, "geoboundaries_countries.csv")
print("Successfully downloaded and saved the country data to geoboundaries_countries.csv")


if (nrow(country_data_df) == 0 || all(is.na(country_data_df$Country))) {
  cat("\nTrying alternative approach with known ISO codes...\n")
  
  # Common ISO3 codes for testing
  test_countries <- c("USA", "GBR", "FRA", "DEU", "JPN", "AUS", "CAN", "BRA", "IND", "CHN")
  
  country_data_df_alt <- map_dfr(test_countries, function(iso) {
    country_url <- paste0("https://www.geoboundaries.org/api/current/gbOpen/", iso, "/ADM0/")
    country_response <- GET(country_url)
    
    if (!http_error(country_response)) {
      country_json <- fromJSON(content(country_response, "text", encoding = "UTF-8"))
      
      if (is.list(country_json) && length(country_json) > 0) {
        country_info <- country_json[[1]]
        return(tibble(
          Country = country_info$boundaryName %||% country_info$shapeName %||% iso,
          BoundaryID = country_info$boundaryID %||% country_info$boundaryISO %||% iso
        ))
      }
    }
    return(NULL)
  })
  
  if (!is.null(country_data_df_alt) && nrow(country_data_df_alt) > 0) {
    country_data_df <- country_data_df_alt
    write_csv(country_data_df, "geoboundaries_countries_alt.csv")
    print("Alternative approach successful - data saved to geoboundaries_countries_alt.csv")
  }
}