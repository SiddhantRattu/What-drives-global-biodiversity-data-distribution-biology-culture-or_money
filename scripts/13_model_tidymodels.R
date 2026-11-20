# ==============================================================================
# 13_model_tidymodels.R
# Author: Siddhant Rattu | Supervisor: Florencia Grattarola
# Purpose: Set up the tidymodels workflow — stratified train/test split,
#          5-fold cross-validation, preprocessing recipe, linear regression fit,
#          and held-out test set evaluation.
# ==============================================================================

library(tidyverse)
library(tidymodels)

tidymodels_prefer()

###### 1. LOAD FINAL MODELLING DATASET

data_model <- read_csv('data/processed/Final_data_for_model.csv') %>%
  mutate(
    main_religion         = as.factor(main_religion),
    ever_colonized        = as.factor(ever_colonized),
    colonizer             = as.factor(colonizer),
    english_main_language = as.factor(english_main_language),
    spanish_main_language = as.factor(spanish_main_language),
    arabic_main_language  = as.factor(arabic_main_language)
  )

# Confirm dataset dimensions (144 countries x 17 variables)
dim(data_model)
glimpse(data_model)

###### 2. TRAIN / TEST SPLIT (STRATIFIED 80/20 SPLIT)

set.seed(123)

data_split <- initial_split(
  data_model,
  prop   = 0.80,
  strata = n_records_gbif
)

data_train <- training(data_split)
data_test  <- testing(data_split)

# Check split sizes and response balance across sets
nrow(data_train)
nrow(data_test)

data_train %>% select(n_records_gbif) %>% summary()
data_test  %>% select(n_records_gbif) %>% summary()

###### 3. RESAMPLING FOLDS (5-FOLD STRATIFIED CV ON TRAINING SET)

set.seed(123)

data_folds <- vfold_cv(
  data_train,
  v      = 5,
  strata = n_records_gbif
)

data_folds

