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

train_save_model <- function(cleaned_train_2021to2023, outcome_2021to2023) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters (all of these are dataframes):
  # cleaned_train_2021to2023: PreFer_train_data.csv after it has gone through the clean_df function 
  # outcome_2021to2023: PreFer_train_outcome.csv 

  set.seed(0)

  # Combine cleaned training data and outcome_df
  model_df_2021to2023 <- merge(cleaned_train_2021to2023, outcome_2021to2023, by = "nomem_encr") %>%
    mutate(new_child = factor(new_child))
  
  # Set up a recipe that remove the ids, dummy-encode the categorical variables 
  # and mean impute everything
  recipe <- recipe(new_child ~ ., model_df_2021to2023) %>%
    step_rm(nomem_encr, nohouse_encr) %>%
    step_mutate(across(c(cf18k128, cf19l128, cf20m128,
        cf20m128_PartnerSurvey, cf19l128_PartnerSurvey,
        belbezig_2020, oplmet_2020,
        migration_background_bg
      ),
      factor
    )) %>%
    step_dummy(all_factor_predictors(), one_hot = TRUE)

  # Tune an xgboost model using grid search and cross validation
  model_to_tune <- boost_tree(
    mode = "classification",
    sample_size = tune(), trees = tune(), tree_depth = tune(), learn_rate = tune()
  ) %>%
    set_engine("xgboost", counts = FALSE)
  # Set up cross-validation folds

  # Set up CV folds
  n_folds <- 5
  folds <- model_df_2021to2023 %>%
    group_vfold_cv(
      group = nohouse_encr, # Puts household members in same fold as each other
      balance = "observations",
      v = n_folds
    )
  
  # Grid search for hyperparameter tuning
  grid <- expand.grid(
    sample_size = c(.4, .5, .6, .7, .8, .9, 1),
    trees = c(1:150),
    tree_depth = c(1, 2, 4, 6, 8, 10, 12),
    learn_rate = c(.01, .03, .05, .1, .3, .5)
  )
  best <- tune_grid(model_to_tune, recipe, folds,
    grid = grid,
    metrics =
      metric_set(metric_tweak("f_meas", f_meas, event_level = "second"))
  ) %>%
    collect_metrics() %>%
    filter(n == 5) %>%
    arrange(desc(mean), trees) %>%
    head(1)
  model <- boost_tree(
    mode = "classification",
    sample_size = best$sample_size,
    trees = best$trees,
    tree_depth = best$tree_depth,
    learn_rate = best$learn_rate
  ) %>%
    set_engine("xgboost", counts = FALSE)
  model <- workflow() %>%
    add_model(model) %>%
    add_recipe(recipe) %>%
    fit(model_df_2021to2023)

  # Save the model
  saveRDS(model, "model.rds")
}
