

library(rgbif)
library(tibble)
library(readr)
library(countrycode)


recordsPerCountryGBIF <- function(list_of_country_codes, output_file = NULL){
  
  records_per_country <- tibble(
    country_code_iso3 = character(),
    n_records_gbif = numeric()
  )
  
  for(code3 in list_of_country_codes){
    
  
    code2 <- tryCatch(
      {
        countrycode(code3, origin = "iso3c", destination = "iso2c")
      },
      error = function(e) NA
    )
    
   
    if(is.na(code2)){
      warning(paste("Skipping invalid country code:", code3))
      n_records <- NA
    } else {
      
      n_records <- tryCatch(
        {
          occ_count(country = code2)
        },
        error = function(e){
          warning(paste("GBIF error for code:", code3, "-", e$message))
          NA
        }
      )
    }
    
  
    records_per_country <- rbind(
      records_per_country,
      tibble(
        country_code_iso3 = code3,
        n_records_gbif = n_records
      )
    )
  }
  

  records_per_country$country_name <- countrycode(
    records_per_country$country_code_iso3,
    origin = "iso3c",
    destination = "country.name"
  )
  

  if(!is.null(output_file)){
    write_csv(records_per_country, output_file)
    message("Results saved to: ", output_file)
  }
  
  return(records_per_country)
}


country_codes_iso3 <- c(
  "ABW","AFG","AGO","AIA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATG","AUS",
  "AUT","AZE","BDI","BEL","BEN","BES","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ",
  "BMU","BOL","BRA","BRB","BRN","BTN","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV",
  "CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CUW","CYM","CYP","CZE","DEU",
  "DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESP","EST","ETH","FIN","FJI","FLK",
  "FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC",
  "GRD","GRL","GTM","GUF","GUM","GUY","HND","HRV","HTI","HUN","IDN","IMN","IRL","IRN","IRQ",
  "ISL","ISR","ITA","JAM","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO",
  "LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAR","MCO","MDA","MDG","MDV",
  "MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI",
  "MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN",
  "PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRK","PRT","PRY","PSE","PYF","QAT","REU",
  "ROU","RUS","RWA","SAU","SDN","SEN","SGP","SHN","SLB","SLE","SLV","SMR","SOM","SRB","SSD",
  "STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM",
  "TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","URY","USA","UZB","VAT","VCT",
  "VEN","VGB","VIR","VNM","VUT","WLF","WSM","XKX","YEM","ZAF","ZMB","ZWE"
)


result <- recordsPerCountryGBIF(
  country_codes_iso3,
  output_file = "GBIF_records_all.csv"
)


print(result)




