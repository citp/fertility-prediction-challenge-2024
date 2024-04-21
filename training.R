# This is an example script to train your model given the (cleaned) input dataset.
# 
# This script will not be run on the holdout data, 
# but the resulting model model.joblib will be applied to the holdout data.
# 
# It is important to document your training steps here, including seed, 
# number of folds, model, et cetera

# Load packages
library(dplyr)
library(tidyr)
library(tidymodels)

train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).

  set.seed(1)

  # Combine cleaned_df and outcome_df
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr") %>%
    select(-nomem_encr, -outcome_available) %>% 
    mutate(new_child = factor(new_child))

  # Logistic regression model
  model_to_fit <- logistic_reg()
  
  # Set up cross validation
  # v <- 5
  # test_folds <- vfold_cv(model_df, v = v)
  # train_folds <- test_folds
  # test_folds <- map(test_folds$splits, assessment)
  # train_folds <- map(train_folds$splits, analysis)
  # map(train_folds, ~ fit(model_to_fit, new_child ~ ., .x)) %>%
  #   map2(test_folds, predict) %>%
  #   map2(test_folds, bind_cols) %>%
  #   map(~ f_meas(.x,
  #     truth = new_child, estimate = .pred_class,
  #     event_level = "second"
  #   )) %>%
  #   map_dbl(~ .x$.estimate) %>%
  #   mean()
  
  model <- fit(model_to_fit, new_child ~ ., model_df)

  # Save the model
  saveRDS(model, "model.rds")
}
