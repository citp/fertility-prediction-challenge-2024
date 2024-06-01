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
                             cleaned_train_2018to2020, outcome_2018to2020) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters (all of these are dataframes):
  # cleaned_train_2021to2023: PreFer_train_data.csv after it has gone through the clean_df function 
  # outcome_2021to2023: PreFer_train_outcome.csv 
  # cleaned_train_2018to2020: A "time-shifted" dataframe of feature data, after it has gone through the clean_df function
  # outcome_2018to2020: Outcome data for fertility in 2018-2020

  set.seed(0)

  # Combine cleaned training data and outcome_df
  model_df_2021to2023 <- merge(cleaned_train_2021to2023, outcome_2021to2023, by = "nomem_encr") %>%
    mutate(new_child = factor(new_child))
  
  model_df_2018to2020 <- merge(cleaned_train_2018to2020, outcome_2018to2020, by = "nomem_encr") %>%
    mutate(new_child = factor(new_child))
  
  original_plus_timeshifted_model_df <- bind_rows(model_df_2021to2023, model_df_2018to2020)
  
  # Set up a recipe that remove the ids, dummy-encode the categorical variables 
  # and mean impute everything
  recipe <- recipe(new_child ~ ., original_plus_timeshifted_model_df) %>%
    step_rm(nomem_encr, nohouse_encr) %>%
    step_dummy(c(belbezig_2020, migration_background_bg, oplmet_2020,
                 cf18k027, cf19l027, cf20m027,
                 cf08a128, cf09b128, cf10c128, cf11d128, cf12e128,  
                 cf13f128, cf14g128, cf15h128, cf16i128, cf17j128,
                 cf18k128, cf19l128, cf20m128), # Some of the *128 are binary but it varies by year, so since we are doing a time-shift, I am one-hot encoding them all for simplicity
      one_hot = TRUE
    ) %>%
    step_impute_mean(everything(), -new_child)

  # Tune an xgboost model using grid search and cross validation
  model_to_tune <- boost_tree(
    mode = "classification",
    mtry = tune(), trees = tune(), tree_depth = tune(), learn_rate = tune()
  ) %>%
    set_engine("xgboost", counts = FALSE)
  # Set up cross-validation folds

  # Set up CV folds within the original data
  n_folds <- 5
  folds <- filter(original_plus_timeshifted_model_df, time_shifted_data == 0
  ) %>%
    group_vfold_cv(
      group = nohouse_encr, # Puts household members in same fold as each other
      balance = "observations",
      v = n_folds
    )
  # Within each CV fold, append time-shifted data.
  # Note: We are appending time-shifted data here rather than prior to creating the
  # CV folds because we only want time-shifted data in training folds, not in test folds.
  # We then make sure that the time-shifted people we append do not come from the same
  # households as those in the test folds
  for (i in 1:n_folds) {
    # Identify what index the first time-shifted observation will be placed at
    start_index <- nrow(folds$splits[[i]][[1]]) + 1
    # Append the time-shifted data but exclude those in the same households as
    # in the test fold
    test_fold <- folds$splits[[i]][[1]][-folds$splits[[i]][[2]], ]
    folds$splits[[i]][[1]] <- bind_rows(
      folds$splits[[i]][[1]],
      filter(original_plus_timeshifted_model_df,
        time_shifted_data == 1,
        !nohouse_encr %in% test_fold$nohouse_encr
      )
    )
    # Add the indices for time-shifted data to the vector of train fold indices
    end_index <- nrow(folds$splits[[i]][[1]])
    time_shifted_data_indices <- c(start_index:end_index)
    folds$splits[[i]][[2]] <-
      c(folds$splits[[i]][[2]], time_shifted_data_indices)
  }
  
  # Grid search for hyperparameter tuning
  grid <- expand.grid(
    mtry = c(.05, .1, .2, .3, .4, .6, .8, 1),
    trees = c(10, 50, 100, 200),
    tree_depth = 1:7,
    learn_rate = c(.01, .1, .3, .5, .7, .9, 1.1)
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
