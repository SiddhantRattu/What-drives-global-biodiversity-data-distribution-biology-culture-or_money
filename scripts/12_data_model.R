library(tidyverse)
data_countries <- read_csv("data/processed/Final_DATASET.csv")

data_countries %>% 
  summarise_all(~ sum(is.na(.))) %>% t()


data_countries %>% 
  select(n_records_gbif) %>% summary()

data_countries %>% 
  mutate(english_main_language = ifelse(main))  select(where(is.numeric) & where(~max(., na.rm = TRUE) > 100000)) %>%   summary()



data_countries_clean <- data_countries %>% 
  mutate(english_main_language = ifelse(main_language == 'English', 'yes', 'no'),
         
         spanish_main_language = ifelse(main_language == 'Spanish', 'yes', 'no'),
         
         arabic_main_language = ifelse(main_language == 'Arabic', 'yes', 'no')) %>% 

  #remove many NA
  select(-c(wb_research_gdp_pct,
            wb_researchers_per_million_people,
            wb_total_researchers_count, 
            wb_total_research_spending_usd,
            foreign_aid_oda)) %>% 
  #remove extra variables
  
  select(-c(hdi_proxy_gdp,
            wb_population_density,
            main_religion_pct,
            main_language,
            atheist_count,
            is_main_english_spanish_arabic)) %>% 
  mutate(ever_colonized = ifelse(ever_colonized == 1, 'yes', 'no' )) %>%
  
  drop_na() %>% 
  
  #
  mutate(main_religion = as.factor(main_religion),
         ever_colonized = as.factor(ever_colonized),
         colonizer = as.factor(colonizer), 
         english_main_language = as.factor(english_main_language),
         spanish_main_language = as.factor(spanish_main_language),
         arabic_main_language = as.factor(arabic_main_language)) %>%
  
  # log and scale the response variable
  mutate(n_records_gbif = log10(n_records_gbif)) %>%
  mutate(n_records_gbif = scale(n_records_gbif)[,1]) %>% 
  relocate(n_records_gbif, .after = 'country')

data_countries_clean %>%  nrow()


data_countries_clean  %>% ncol()

#corelation

data_countries_clean %>% 
  select_if(is.numeric) %>% 
  cor()
  
  
