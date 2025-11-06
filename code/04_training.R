# This file is based on training.R from the PreFer Challenge. It fits models.

# Load packages
library(groundhog)
groundhog.library(
  c("here", "tidyverse", "tidymodels", "xgboost"),
  "2024-04-23"
)
here() %>%
  setwd()

# This function outputs the predictions from a particular fold, suing the best
# model emerging from CV tests
get_best_predictions <- function(fold, tuning, best, name) {
  output <- tuning[[5]][[fold]] %>%
    filter(.config == best$.config) %>%
    select(.pred_1)
  colnames(output) <- name
  data.frame(output)
}

# Trains a model using the cleaned dataframe and saves the model to a file.
train_save_model <- function(cleaned_train_2021to2023, outcome_2021to2023, 
                             cleaned_train_2018to2020, outcome_2018to2020,
                             feature_set, partner, time_shift, model, tune,
                             name, preds_cv) {
  # Parameters:
  # cleaned_train_2021to2023 (dataframe): PreFer_train_data.csv after it has gone through the clean_df function 
  # outcome_2021to2023 (dataframe): PreFer_train_outcome.csv 
  # cleaned_train_2018to2020 (dataframe): A "time-shifted" dataframe of feature data, after it has gone through the clean_df function
  # outcome_2018to2020 (dataframe): Outcome data for fertility in 2018-2020
  # feature_set (character): "full_features"--full cleaning and partner linkage
  #                         "seven_features"--seven baseline features hand-chosen by the organizers of the PreFer Challenge
  #                         "three_features"--just time shift indicator plus two fertility intention variables
  # partner (logical): whether partner linkage is turned on
  # time_shift (logical): whether time shift is turned on
  # model (character): "xgboost" or "glm"
  # tune (logical): whether to tune hyperparameters
  # name (character): "final"--full features, partner, time shift, xgboost, tune (winning model)
  #                   "partner"--full features, partner, no time shift, xgboost, tune
  #                   "time_shift"--full features, no partner, time shift,xgboost, tune
  #                   "original"--full features, no partner, no time shift, xgboost, tune
  #                   "glm_full_final"--full features, partner, time shift, glm, no tune
  #                   "glm_full_partner"--full features, partner, no time shift, glm, no tune
  #                   "glm_full_time_shift"--full features, no partner, time shift, glm, no tune
  #                   "glm_full_original"--full features, no partner, no time shift, glm, no tune
  #                   "glm_seven_final"--seven features, partner, time shift, glm, no tune
  #                   "glm_seven_partner"--seven features, partner, no time shift, glm, no tune
  #                   "glm_seven_time_shift"--seven features, no partner, time shift, glm, no tune
  #                   "glm_seven_original"--seven features, no partner, no time shift, glm, no tune
  #                   "no_tuning"--full features, partner, time shift, xgboost, no tune
  #                   "three_features"--three features, no partner, time shift, xgboost, tune
  set.seed(0)

  # Combine cleaned training data and outcome_df. Combine two time periods if requested
  original_train <- merge(cleaned_train_2021to2023, outcome_2021to2023, by = "nomem_encr") %>%
    mutate(new_child = factor(new_child))
  train <- original_train
  if(time_shift) {
    model_df_2018to2020 <- merge(cleaned_train_2018to2020, outcome_2018to2020, by = "nomem_encr") %>%
      mutate(new_child = factor(new_child))
    train <- bind_rows(train, model_df_2018to2020)
  }
  
  # Set up a recipe that remove the ids and dummy-encode the categorical
  # variables.
  # With the full winning model, we mean impute everything, including one-hot 
  # dummies
  # But for factor variables in 7-variable logistic regression, we impute  
  # medians for number of children and modes for categoricals. This imitates 
  # the baseline approach by PreFer organizers. But we impute means for age.
  # For glm models, there will be a reference level for the categorical dummies
  recipe <- recipe(new_child ~ ., train) %>%
    step_rm(nomem_encr, nohouse_encr)
  if (feature_set != "seven_features") {
    recipe <- recipe %>%
      step_mutate(across(any_of(c("cf18k128", "cf19l128", "cf20m128",
              "cf20m128_PartnerSurvey", "cf19l128_PartnerSurvey",
              "belbezig_2020", "oplmet_2020",
              "migration_background_bg"
            )
          ),
          factor
        )
      )
    if (model != "glm") {
      recipe <- step_dummy(recipe, all_factor_predictors(), one_hot = TRUE)
    } else {
      recipe <- recipe %>%
        step_dummy(all_factor_predictors()) %>%
        step_impute_mean(all_predictors())
    }
  } else {
    recipe <- recipe %>%
      step_impute_mode(any_of(c("oplcat_2020", "burgstat_2020",
            "oplcat_2020_PartnerSurvey", "burgstat_2020_PartnerSurvey",
            "fert_intentions_2020_PartnerSurvey",
            "gender_bg_PartnerSurvey"
          )
        )
      ) %>%
      step_impute_median(
        any_of(c("num_children_2020", "num_children_2020_PartnerSurvey"))
      )
    if(partner) {
      recipe <- recipe %>%
        step_impute_mean(age_bg_PartnerSurvey) %>%
        step_mutate(
          age_bg_squared = age_bg ^ 2,
          age_bg_PartnerSurvey_squared = age_bg_PartnerSurvey ^ 2
        )
    } else {
      recipe <- recipe %>%
        step_mutate(age_bg_squared = age_bg ^ 2) 
    }
    recipe <- step_dummy(recipe, all_factor_predictors())
  }

  # Set up cross-validation folds. 
  # Set up CV folds within the original data.
  # For glm, we load pre-saved CV folds because there were extra
  # recipe steps involved (see above), which messed up the random seed. We want 
  # to keep the splits exactly the same
  n_folds <- 5
  if(model == "xgboost") {
    folds <- original_train %>%
      group_vfold_cv(
        group = nohouse_encr, # Puts household members in same fold as each other
        balance = "observations",
        v = n_folds
      )
    if(name == "final") {
      dir.create("data/intermediate_files")
      saveRDS(folds, "data/intermediate_files/folds.RDS")
    }
  } else {
    folds <- readRDS("data/intermediate_files/folds.RDS")
    for (i in 1:n_folds) {
      folds$splits[[i]][[1]] <- original_train
    }
  }
  
  # Within each CV fold, append time-shifted data.
  # Note: We are appending time-shifted data here rather than prior to creating the
  # CV folds because we only want time-shifted data in training folds, not in test folds.
  # We then make sure that the time-shifted people we append do not come from the same
  # households as those in the test folds
  if (time_shift) {
    for (i in 1:n_folds) {
      # Identify what index the first time-shifted observation will be placed at
      start_index <- nrow(folds$splits[[i]][[1]]) + 1
      # Append the time-shifted data but exclude those in the same households as
      # in the test fold
      test_fold <- folds$splits[[i]][[1]][-folds$splits[[i]][[2]], ]
      folds$splits[[i]][[1]] <- bind_rows(
        folds$splits[[i]][[1]],
        filter(model_df_2018to2020,
          !nohouse_encr %in% test_fold$nohouse_encr
        )
      )
      # Add the indices for time-shifted data to the vector of train fold indices
      end_index <- nrow(folds$splits[[i]][[1]])
      time_shifted_data_indices <- c(start_index:end_index)
      folds$splits[[i]][[2]] <-
        c(folds$splits[[i]][[2]], time_shifted_data_indices)
    }
  }

    # Set up the model
  if (model == "xgboost") {
    if (tune) {
      model_to_tune <- boost_tree(
        # Tune an xgboost model using grid search and cross validation
        mode = "classification",
        sample_size = tune(),
        trees = tune(),
        tree_depth = tune(),
        learn_rate = tune()
      )
    } else {
      model_to_tune <- boost_tree(
        mode = "classification"
      )
    }
  } else {
    model_to_tune <- logistic_reg() %>%
      set_engine("glm")
  }
  model_to_tune <- set_engine(model_to_tune, model)
  
  # Set up tuning grid
  if (tune) {
    grid <- expand.grid(
      sample_size = c(.4, .5, .6, .7, .8, .9, 1),
      trees = c(1:150),
      tree_depth = c(1, 2, 4, 6, 8, 10, 12),
      learn_rate = c(.01, .03, .05, .1, .3, .5)
    )
  } 
  # Grid search for hyperparameter tuning
  if (tune) {
    tuning <- model_to_tune %>%
      tune_grid(recipe, folds,
        grid = grid,
        metrics = metric_set(brier_class, 
          metric_tweak("f_meas", f_meas, event_level = "second")
        ),
        control = control_grid(save_pred = TRUE)
      ) 
  } else {
    tuning <- model_to_tune %>%
      tune_grid(recipe, folds,
        metrics = metric_set(brier_class, 
          metric_tweak("f_meas", f_meas, event_level = "second")
        ),
        control = control_grid(save_pred = TRUE)
      )
  }
  best <- tuning %>%
    collect_metrics() %>%
    filter(.metric == "f_meas", n == 5) 
  if (tune) {
    best <- best %>%
      arrange(desc(mean), trees) %>%
      head(1)
  }
  
  # Save CV predictions for best pipeline
  if (preds_cv) {
    1:n_folds %>%
      map(~ get_best_predictions(.x, tuning, best, name)) %>%
      list_rbind() %>%
      saveRDS(paste0(
          "data/intermediate_files/preds_cv/preds_cv_", name, ".RDS"
        )
      )
  }
  
  
  # Refit model for best pipeline on all of the training set
  if (model == "xgboost") {
    if (tune) {
      model_to_save <- boost_tree(
        mode = "classification",
        sample_size = best$sample_size,
        trees = best$trees,
        tree_depth = best$tree_depth,
        learn_rate = best$learn_rate
      )
    } else {
      model_to_save <- boost_tree(
        mode = "classification"
      )
    }
  } else {
    model_to_save <- logistic_reg() %>%
      set_engine("glm")
  }
  model_to_save <- set_engine(model_to_save, model)
  model_to_save <- workflow() %>%
    add_model(model_to_save) %>%
    add_recipe(recipe) %>%
    fit(train)
  # Save the model
  model_to_save %>%
    saveRDS(paste0("data/intermediate_files/models/model_", name, ".RDS"))
}

# Given a model name, this function reads the data and sets up the variables 
# for train_save_model()
read_train_save <- function(name, preds_cv) {
  start <- Sys.time()
  # Read cleaned_df
  if (name %in% c("final", "glm_full_final", "no_tuning")) {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_full_features_final.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2018to2020_full_features_final.RDS") %>%
      readRDS()
  }
  if (name %in% c("partner", "glm_full_partner")) {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_full_features_partner.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- NULL
  }
  if (name %in% c("time_shift", "glm_full_time_shift")) {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_full_features_time_shift.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2018to2020_full_features_time_shift.RDS") %>%
      readRDS()
  }
  if (name %in% c("original", "glm_full_original")) {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_full_features_original.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- NULL
  }
  if (name == "glm_seven_final") {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_seven_features_final.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2018to2020_seven_features_final.RDS") %>%
      readRDS()
  }
  if (name == "glm_seven_partner") {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_seven_features_partner.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- NULL
  }
  if (name == "glm_seven_time_shift") {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_seven_features_time_shift.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2018to2020_seven_features_time_shift.RDS") %>%
      readRDS()
  }
  if (name == "glm_seven_original") {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_seven_features_original.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- NULL
  }
  if (name == "three_features") {
    cleaned_train_2021to2023 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2021to2023_three_features_time_shift.RDS") %>%
      readRDS()
    cleaned_train_2018to2020 <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_train_2018to2020_three_features_time_shift.RDS") %>%
      readRDS()
  }

  # Read 21-23 outcomes
  outcome_2021to2023 <- "data/PreFer_train_outcome.csv" %>%
    read.csv(encoding = "latin1")

  # Decide whether to read time shifted outcomes
  if (!(grepl("original", name) | grepl("partner", name))) {
    outcome_2018to2020 <- "data/intermediate_files/outcome_2018to2020.csv" %>%
      read.csv(encoding = "latin1")
  } else {
    outcome_2018to2020 <- NULL
  }

  # Decide on feature set
  if (!(grepl("seven", name) | grepl("three", name))) {
    feature_set <- "full_features"
  } else {
    if (grepl("seven", name)) {
      feature_set <- "seven_features"
    } else {
      feature_set <- "three_features"
    }
  }

  # Is there partner data?
  partner <- grepl("final", name) |
    grepl("partner", name) |
    grepl("tuning", name)

  # Is there time shift?
  time_shift <- !(grepl("original", name) | grepl("partner", name))

  # Which model to use?
  if (!grepl("glm", name)) {
    model <- "xgboost"
  } else {
    model <- "glm"
  }

  # Is there tuning?
  tune <- !(grepl("glm", name) | grepl("tuning", name))

  # Train and save the model!
  train_save_model(cleaned_train_2021to2023, outcome_2021to2023,
    cleaned_train_2018to2020, outcome_2018to2020,
    feature_set, partner, time_shift, model, tune,
    name, preds_cv
  )
  Sys.time() %>%
    difftime(start, units = "mins") %>%
    write(paste0("numbers/timing/04_training_", name, ".tex"))
}

# read_train_save for all 14 models
dir.create("data/intermediate_files/preds_cv", recursive = TRUE)
dir.create("data/intermediate_files/models")
"final" %>%
  c("partner", "time_shift", "original",
    "glm_full_final", "glm_full_partner",
    "glm_full_time_shift", "glm_full_original",
    "glm_seven_final", "glm_seven_partner",
    "glm_seven_time_shift", "glm_seven_original",
    "no_tuning", "three_features"
  ) %>%
  walk(read_train_save, preds_cv)