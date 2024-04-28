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

train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).
  set.seed(0)

  # Combine cleaned_df and outcome_df
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr") %>%
    select(-nomem_encr) %>%
    mutate(new_child = factor(new_child))

  # Dummy-encode the categorical variables and mean impute everything
  recipe <- recipe(new_child ~ ., model_df) %>%
    step_dummy(oplmet_2020, one_hot = TRUE) %>%
    step_impute_mean(everything(), -new_child)

  # Tune an xgboost model using grid search and cross validation
  model_to_tune <- boost_tree(
    mode = "classification",
    mtry = tune(), trees = tune(), tree_depth = tune(), learn_rate = tune()
  ) %>%
    set_engine("xgboost", counts = FALSE)
  folds <- vfold_cv(model_df, v = 5)
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
    fit(model_df)

  # Save the model
  saveRDS(model, "model.rds")
}
