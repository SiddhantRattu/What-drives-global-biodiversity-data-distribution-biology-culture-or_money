###

library(janitor)
library(patchwork)
library(sf)
sf_use_s2(FALSE)
library(tmap)
tmap_mode('plot')
library(tidyverse)

options(knitr.kable.NA = '') 
options(scipen=999)


data_countries_variables <- readxl::read_xlsx('docs/Final_Dataset.xlsx', na = 'NA') %>% 
  janitor::clean_names() %>% 
  rename(biome_diversity_count=
           biome_diversity_count_count_of_unique_ecosystems_in_the_country)
data_countries_variables %>% head(n=25) 

##################################################
# NAs

data_countries_variables %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  t()

data_countries_variables %>% 
  filter(if_any(everything(), is.na)) %>% view()

data_countries_variables %>% 
  select(-c(wb_percent_gdp_for_research,
            wb_researchers_per_million_people, 
            wb_total_researchers_count,
            wb_total_research_spending_usd,
            foreign_aid_oda)) %>% 
  drop_na(-pa_coverage_pct) %>% nrow()

data_countries_variables %>% view()
  filter(country == 'India') %>% view()

data_countries_variables %>% 
  filter(is.na(n_records_gbif))

data_countries_variables %>% 
  distinct(main_religion)

##################################################
# FINAL VARIABLES SELECTED

data_models <- data_countries_variables %>% 
  select(-c(wb_percent_gdp_for_research,
            wb_researchers_per_million_people, 
            wb_total_researchers_count,
            wb_total_research_spending_usd,
            foreign_aid_oda,
            pa_coverage_pct)) %>% 
  drop_na() %>% 
  mutate(ever_colonized = as.factor(ever_colonized))

data_models %>% 
  select_if(is.numeric) %>% 
  summary()

data_models %>% view()

##################################################
# PLOTS

data_models %>% names()

# HISTOGRAMS

plotly::ggplotly(
  ggplot(data_models, aes(biome_diversity_count)) +
    geom_histogram(bins = 50) +
    scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    # labs(x='Population of the country', y='') +
    theme_bw()
)

plotly::ggplotly(
  ggplot(data_models, aes(wb_gdp_total)) +
    geom_histogram(bins = 50) +
    scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    # labs(x='Population of the country', y='') +
    theme_bw()
)

plotly::ggplotly(
  ggplot(data_models, aes(mean_latitude)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
  # labs(x='Population of the country', y='') +
  theme_bw()
)

# POINTS

plotly::ggplotly(
  ggplot(data_models, aes(wb_population, area_total_km2, label = country)) +
  geom_point( size=2, show.legend = F) +
  geom_smooth(method='lm') +
  labs(x='Population of the country',
       y='Area of the country') +
  scale_x_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) + 
  scale_y_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) +
  theme_bw()
  )

plotly::ggplotly(
  ggplot(data_models, aes(hdi_proxy_life, wb_gdp_per_capita, label = country)) +
    geom_point( size=2, show.legend = F) +
    geom_smooth(method='lm') +
    scale_x_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) + 
    scale_y_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    theme_bw()
)


plotly::ggplotly(
  ggplot(data_models, aes(hdi_proxy_gdp, hdi_proxy_life, label = country)) +
    geom_point( size=2, show.legend = F) +
    geom_smooth(method='lm') +
    scale_x_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) + 
    scale_y_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    theme_bw()
)

plotly::ggplotly(
  ggplot(data_models, aes(wb_population, n_records_gbif, label = country)) +
    geom_point(aes(col=country), size=2, show.legend = F) +
    geom_smooth(method='lm') +
    scale_x_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) + 
    scale_y_log10(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    theme_bw()
)

##################################################
# CORRELATIONS

data_models %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  kableExtra::kbl(digits=3, 
                  format.args = list(big.mark = ',')) %>%
  kableExtra::kable_material('striped') 

# correlation plot
data_models %>% 
  select_if(is.numeric) %>% 
  select(-n_records_gbif) %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square', #
                     diag = FALSE, 
                     type = 'upper',
                     order = 'alphabet', 
                     tl.col = 'grey15') 

# predictors to remove
data_models %>% 
  select_if(is.numeric) %>% 
  select(-n_records_gbif) %>% 
  cor() %>% caret::findCorrelation(., cutoff=0.6, names=T)
