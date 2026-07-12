# This file puts together a dataframe that contains all predictions by all
# models. It outputs cv predictions, holdout predictions, or both, depending
# on command line settings.
# In addition, the CV contains a train_means column that contains the mean of 
# the training set for each fold and/or the holdout set.
# The CV also contains a column that indicates the outcome

# Set up
start <- Sys.time()

# Load packages if they weren't already loaded via the run_all script
if (!isTRUE(getOption("run_all_executed"))) {
  library(groundhog)
  groundhog.library(c("here", "tidyverse"), "2024-04-23")
}

here() %>%
  setwd()

preds <- NULL

# This function gets train_mean and outcomes for a particular fold
get_train_means_and_new_child <- function(fold, folds) {
  data <- folds[[1]][[fold]][["data"]] %>%
    mutate(
      new_child = as.character(new_child),
      new_child = as.numeric(new_child)
    ) %>%
    select(new_child)
  
  tibble(
    train_means =
      rep(mean(data[folds[[1]][[fold]][["in_id"]], ]), 
          nrow(folds[[1]][[fold]][["data"]]) -
        length(folds[[1]][[fold]][["in_id"]])),
    new_child = data[!row.names(data) %in% folds[[1]][[fold]][["in_id"]], ]
  )
}
if (preds_cv) {
  folds <- readRDS("data/intermediate_files/folds.RDS")
  train_means_and_new_child <- 1:5 %>%
    map(~ get_train_means_and_new_child(.x, folds)) %>%
    list_rbind()
  
  # Read predictions
  preds_cv_final <- "data/intermediate_files/preds_cv/preds_cv_final.RDS" %>%
    readRDS()
  preds_cv_partner <- 
    "data/intermediate_files/preds_cv/preds_cv_partner.RDS" %>%
    readRDS()
  preds_cv_time_shift <- 
    "data/intermediate_files/preds_cv/preds_cv_time_shift.RDS" %>%
    readRDS()
  preds_cv_original <- 
    "data/intermediate_files/preds_cv/preds_cv_original.RDS" %>%
    readRDS()
  preds_cv_glm_full_final <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_full_final.RDS" %>%
    readRDS()
  preds_cv_glm_full_partner <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_full_partner.RDS" %>% 
    readRDS()
  preds_cv_glm_full_time_shift <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_full_time_shift.RDS" %>%
    readRDS()
  preds_cv_glm_full_original <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_full_original.RDS" %>%
    readRDS()
  preds_cv_glm_seven_final <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_seven_final.RDS" %>%
    readRDS()
  preds_cv_glm_seven_partner <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_seven_partner.RDS" %>%
    readRDS()
  preds_cv_glm_seven_time_shift <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_seven_time_shift.RDS" %>%
    readRDS()
  preds_cv_glm_seven_original <- 
    "data/intermediate_files/preds_cv/preds_cv_glm_seven_original.RDS" %>%
    readRDS()
  preds_cv_no_tuning <- 
    "data/intermediate_files/preds_cv/preds_cv_no_tuning.RDS" %>%
    readRDS()
  preds_cv_three_features <- 
    "data/intermediate_files/preds_cv/preds_cv_three_features.RDS" %>%
    readRDS()

  # Create single dataframe
  preds <- preds_cv_final %>%
    bind_cols(preds_cv_partner, preds_cv_time_shift, preds_cv_original,
      preds_cv_glm_full_final, preds_cv_glm_full_partner,
      preds_cv_glm_full_time_shift, preds_cv_glm_full_original,
      preds_cv_glm_seven_final, preds_cv_glm_seven_partner,
      preds_cv_glm_seven_time_shift, preds_cv_glm_seven_original,
      preds_cv_no_tuning, preds_cv_three_features,
      train_means_and_new_child
    ) %>%
    bind_rows(preds)
}


# Given a model name, this function reads the data and sets up the variables 
# for train_save_model()
get_holdout_preds <- function(name) {
  # Read cleaned_df
  if (name %in% c("final", "glm_full_final", "no_tuning")) {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_full_features_final.RDS") %>%
      readRDS()
  }
  if (name %in% c("partner", "glm_full_partner")) {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_full_features_partner.RDS") %>%
      readRDS()
  }
  if (name %in% c("time_shift", "glm_full_time_shift")) {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_full_features_time_shift.RDS") %>%
      readRDS()
  }
  if (name %in% c("original", "glm_full_original")) {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_full_features_original.RDS") %>%
      readRDS()
  }
  if (name == "glm_seven_final") {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_seven_features_final.RDS") %>%
      readRDS()
  }
  if (name == "glm_seven_partner") {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_seven_features_partner.RDS") %>%
      readRDS()
  }
  if (name == "glm_seven_time_shift") {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_seven_features_time_shift.RDS") %>%
      readRDS()
  }
  if (name == "glm_seven_original") {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_seven_features_original.RDS") %>%
      readRDS()
  }
  if (name == "three_features") {
    cleaned_holdout <- "data/intermediate_files/cleaned_dfs/" %>%
      paste0("cleaned_holdout_three_features_time_shift.RDS") %>%
      readRDS()
  }
  
  # Read model and make and output predictions
  output <- "data/intermediate_files/models/model_" %>% 
    paste0(name, ".RDS") %>%
    readRDS() %>%
    predict(cleaned_holdout, type = "prob") %>%
    select(.pred_1)
  colnames(output) <- name
  output
}
if (preds_holdout) {
  holdout <- read.csv("data/PreFer_holdout_data.csv")
  outcome_holdout <- read.csv("data/PreFer_holdout_outcome.csv") %>%
    filter(!is.na(new_child))
  outcome_2021to2023 <- read.csv("data/PreFer_train_outcome.csv")
  preds <- c("final", "partner", "time_shift", "original",
    "glm_full_final", "glm_full_partner",
    "glm_full_time_shift", "glm_full_original",
    "glm_seven_final", "glm_seven_partner",
    "glm_seven_time_shift", "glm_seven_original",
    "no_tuning", "three_features"
  ) %>%
    map(get_holdout_preds) %>%
    list_cbind() %>%
    mutate(train_means = mean(outcome_2021to2023$new_child, na.rm = TRUE)) %>%
    bind_cols(outcome_holdout) %>%
    select(-nomem_encr) %>%
    bind_rows(preds)
}
dir.create("data/intermediate_files")
saveRDS(preds, "data/intermediate_files/preds.RDS")
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/08_all_preds.tex")