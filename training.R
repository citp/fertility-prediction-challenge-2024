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
library(xgboost)

train_save_model <- function(cleaned_train_2021to2023, outcome_2021to2023, 
                             cleaned_train_2018to2020, outcome_2018to2020,
                             background) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters (all of these are dataframes):
  # cleaned_train_2021to2023: PreFer_train_data.csv after it has gone through the clean_df function 
  # outcome_2021to2023: PreFer_train_outcome.csv 
  # cleaned_train_2018to2020: A "time-shifted" dataframe of feature data, after it has gone through the clean_df function
  # outcome_2018to2020: Outcome data for fertility in 2018-2020
  # background: PreFer_train_background_data.csv

  set.seed(0)

  # Combine cleaned training data and outcome_df
  model_df_2021to2023 <- merge(cleaned_train_2021to2023, outcome_2021to2023, by = "nomem_encr") %>%
    mutate(new_child = factor(new_child))
  
  model_df_2018to2020 <- merge(cleaned_train_2018to2020, outcome_2018to2020, by = "nomem_encr") %>%
    mutate(new_child = factor(new_child)) %>%
    select(-nomem_encr)
  
  original_plus_timeshifted_model_df <- bind_rows(model_df_2021to2023, model_df_2018to2020) %>%
    select(-nomem_encr)

  # Dummy-encode the categorical variables and mean impute everything
  recipe <- recipe(new_child ~ ., model_df_2021to2023) %>%
    step_dummy(oplmet_2020, one_hot = TRUE) %>%
    step_impute_mean(everything(), -new_child)

  recipe <- recipe(new_child ~ ., model_df_2018to2020) %>%
    step_dummy(oplmet_2020, one_hot = TRUE) %>%
    step_impute_mean(everything(), -new_child)

  # Tune an xgboost model using grid search and cross validation
  model_to_tune <- boost_tree(
    mode = "classification",
    mtry = tune(), trees = tune(), tree_depth = tune(), learn_rate = tune()
  ) %>%
    set_engine("xgboost", counts = FALSE)
  # Set up cross-validation folds
    # Identify the household each person was a member of at the last time that person
    # was observed, up through December 2020
    household_linkage <- background %>% 
      arrange(desc(wave)) %>%
      group_by(nomem_encr) %>%
      slice_head %>%
      select(nomem_encr, nohouse_encr)
    # Merge the household ID with model_df_2021to2023
    model_df_2021to2023 <- left_join(model_df_2021to2023, household_linkage)
    # Set up CV folds within the original data 
    n_folds <- 5
    folds <- group_vfold_cv(data = model_df_2021to2023, 
                            group = nohouse_encr, # Puts household members in same fold as each other
                            balance = "observations",
                            v = n_folds)
    # Within each CV fold, remove ID numbers and append time-shifted data.
    # Note: We are appending time-shifted data here rather than prior to creating the
    # CV folds because we only want time-shifted data in training folds, not in test folds.
    for(i in 1:n_folds) {
      # Remove the personal and household ID numbers
      folds$splits[[i]][[1]] <- folds$splits[[i]][[1]] %>%
        select(-c(nomem_encr, nohouse_encr))
      # Identify what index the first time-shifted observation will be placed at
      start_index <- nrow(folds$splits[[i]][[1]]) + 1
      # Append the time-shifted data
      folds$splits[[i]][[1]] <- bind_rows(folds$splits[[i]][[1]], model_df_2018to2020)
      # Add the indices for time-shifted data to the vector of train fold indices 
      end_index <- nrow(folds$splits[[i]][[1]])
      time_shifted_data_indices <- c(start_index:end_index)
      folds$splits[[i]][[2]] <- c(folds$splits[[i]][[2]], time_shifted_data_indices)
    } 
  
  # Grid search for hyperparameter tuning
  grid <- expand.grid(
    mtry = c(.05, .1, .15, .2, .25, .3, .35, .4),
    trees = c(10, 50, 100, 300, 600, 900, 1200),
    tree_depth = 3:7,
    learn_rate = c(.1, .3, .5, .7, .9, 1.1)
  )
  best <- tune_grid(model_to_tune, recipe, folds,
    grid = grid,
    metrics =
      metric_set(metric_tweak("f_meas", f_meas, event_level = "second"))
  ) %>%
    collect_metrics() %>%
    filter(n == 5) %>%
    arrange(desc(mean)) %>%
    head(1)
  model <- boost_tree(
    mode = "classification",
    mtry = best$mtry,
    trees = best$trees,
    tree_depth = best$tree_depth,
    learn_rate = best$learn_rate
  ) %>%
    set_engine("xgboost", counts = FALSE)
  model <- workflow() %>%
    add_model(model) %>%
    add_recipe(recipe) %>%
    fit(original_plus_timeshifted_model_df)

  # Save the model
  saveRDS(model, "model.rds")
}
