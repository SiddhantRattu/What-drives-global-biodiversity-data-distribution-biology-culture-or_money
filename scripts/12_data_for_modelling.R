library(tidyverse)

data_countries <- read_csv('data/processed/Final_DATASET.csv')

###### EXPLORE THE DATA

# how many `NA` values are in each column?
data_countries %>% 
  summarise_all(~ sum(is.na(.))) %>% t()

# check which categorical variables need to be standardised
data_countries %>% 
  select(where(is.character))

# check if the response needs to be scaled
data_countries %>% 
  select(n_records_gbif) %>% summary()

# and other variables? we will do this as part of the model
data_countries %>% 
  select(where(is.numeric) & where(~max(., na.rm = TRUE) > 100000)) %>% 
  names()

data_countries %>% 
  select(where(is.numeric) & where(~max(., na.rm = TRUE) > 100000)) %>% 
  summary()

###### CLEAN THE DATA

data_countries_clean <- data_countries %>% 
  # remove variables with many NAs
  select(-c(wb_research_gdp_pct, 
            wb_researchers_per_million_people,
            wb_total_researchers_count,
            wb_total_research_spending_usd,
            foreign_aid_oda)) %>% 
  # create language columns
  mutate(english_main_language = ifelse(main_language == 'English', 'yes', 'no'),
         spanish_main_language = ifelse(main_language == 'Spanish', 'yes', 'no'),
         arabic_main_language = ifelse(main_language == 'Arabic', 'yes', 'no')) %>%
  # remove extra variables 
  select(-c(hdi_proxy_gdp, 
            wb_population_density,
            main_language,
            main_religion_pct,
            atheist_count,
            is_main_english_spanish_arabic)) %>%
  # standardise categorical variable
  mutate(ever_colonized = ifelse(ever_colonized == 1, 'yes', 'no')) %>% 
  # drop countries with NAs
  drop_na() %>%
  # characters as factors
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
  
# how many countries
data_countries_clean %>% nrow()
# how many variables
data_countries_clean %>% ncol()

###### IDENTIFY CORRELATED VARIABLES

# correlation of variables as a matrix
data_countries_clean %>% 
  select_if(is.numeric) %>% 
  cor()

# correlation of variables as a plot
data_countries_clean %>% 
  select_if(is.numeric) %>% 
  select(-n_records_gbif) %>% 
  cor(use = 'complete.obs') %>% 
  corrplot::corrplot(method = 'color', #
                     diag = FALSE, 
                     type = 'upper',
                     order = 'alphabet', 
                     tl.col = 'grey15') 

# identify those variables that have a correlation higher than 0.6
data_countries_clean %>% 
  select_if(is.numeric) %>% 
  select(-n_records_gbif) %>% 
  cor() %>% 
  caret::findCorrelation(., cutoff=0.6, names=T)

# remove those variables that have a correlation higher than 0.6 and check the correlation again
data_countries_clean %>% 
  select(-n_records_gbif) %>%
  select_if(is.numeric) %>% 
  select(-c(biome_diversity_count, area_total_km2, 
         wb_gdp_per_capita, hdi_proxy_life)) %>% 
  cor(use = 'complete.obs') %>% 
  corrplot::corrplot(method = 'color', #
                     diag = FALSE, 
                     type = 'upper',
                     order = 'alphabet', 
                     tl.col = 'grey15')

# store the final dataset for modelling
data_model <- data_countries_clean %>% 
  select(-c(biome_diversity_count, area_total_km2, 
            wb_gdp_per_capita, hdi_proxy_life)) 

# final number of variables in the data
data_model %>% ncol()



