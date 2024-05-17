# This is an example script to generate the outcome variable given the input dataset.
# 
# This script should be modified to prepare your own submission that predicts 
# the outcome for the benchmark challenge by changing the clean_df and predict_outcomes function.
# 
# The predict_outcomes function takes a data frame. The return value must
# be a data frame with two columns: nomem_encr and outcome. The nomem_encr column
# should contain the nomem_encr column from the input data frame. The outcome
# column should contain the predicted outcome for each nomem_encr. The outcome
# should be 0 (no child) or 1 (having a child).
# 
# clean_df should be used to clean (preprocess) the data.
# 
# run.R can be used to test your submission.

# List your packages here. Don't forget to update packages.R!
library(dplyr)
library(tidyr)
library(tidymodels)
library(xgboost)

clean_df <- function(df, background_df) {
  # Preprocess the input dataframe to feed the model.
  ### If no cleaning is done (e.g. if all the cleaning is done in a pipeline) leave only the "return df" command

  # Parameters:
  # df (dataframe): The input dataframe containing the raw data (e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background (dataframe): PreFer_train_background_data.csv or PreFer_fake_background_data.csv 

  # Returns:
  # data frame: The cleaned dataframe with only the necessary columns and processed variables.

  # TIME-SHIFTED DATA INDICATOR
  # The time shifted data already has a column called time_shifted_data, where
  # time_shifted_data = 1. For the regular data, we need to create time_shifted_data = 0.
  if (!"time_shifted_data" %in% colnames(df)) {
    df <- df %>%
      mutate(time_shifted_data = 0)
  }

  # Selecting variables for modelling

  keepcols <- c("nomem_encr", # ID variable required for predictions,
    "outcome_available", # Is there an outcome to predict?
    "time_shifted_data", # Indicates whether this is original data or time-shifted data
    # Expected kids reported in 2020
    "cf20m128", "cf20m129", "cf20m130",
    # Expected kids reported in 2019
    "cf19l128", "cf19l129", "cf19l130"
  )

  ## Keeping data with variables selected
  df <- df[, keepcols]

  ## Keep only rows with available outcomes
  df <- filter(df, outcome_available == 1) %>%
    rowwise() %>%
    mutate(
      # If no expected kids, then expected number of kids is 0
      cf20m129 = ifelse(cf20m128 == 2, 0, cf20m129),
      cf19l129 = ifelse(cf19l128 == 2, 0, cf19l129),
      # If no expected kids, then a lower-bound estimate for the number of
      # years within which to have kids is 31,
      cf20m130 = case_when(cf20m128 == 2 ~ 31, cf20m130 == 2025 ~ 5,
                           TRUE ~ cf20m130
      ),
      cf19l130 = case_when(cf19l128 == 2 ~ 31,
        TRUE ~ cf19l130)
      ) %>%
    select(-outcome_available
           ) %>%
    mutate(
      across(everything(), as.numeric),
      across(cf20m128, factor) # Options for cf20m128 were yes/no/IDK, but for cf19l128, options were only yes/no
    )
  
  # Append household ID
  # Identify the household each person was a member of at the last time that person
  # was observed, up through December 2020
  household_linkage <- background_df %>% 
    arrange(desc(wave)) %>%
    group_by(nomem_encr) %>%
    slice_head() %>%
    select(nomem_encr, nohouse_encr)
  # Merge the household ID with original_plus_timeshifted_model_df
  df <- left_join(df, household_linkage)

  return(df)
}

predict_outcomes <- function(df, background_df, model_path = "./model.rds"){
  # Generate predictions using the saved model and the input dataframe.
    
  # The predict_outcomes function accepts a dataframe as an argument
  # and returns a new dataframe with two columns: nomem_encr and
  # prediction. The nomem_encr column in the new dataframe replicates the
  # corresponding column from the input dataframe The prediction
  # column contains predictions for each corresponding nomem_encr. Each
  # prediction is represented as a binary value: '0' indicates that the
  # individual did not have a child during 2021-2023, while '1' implies that
  # they did.
  
  # Parameters:
  # df (dataframe): The data dataframe for which predictions are to be made.
  # background_df (dataframe): The background data dataframe for which predictions are to be made.
  # model_path (str): The path to the saved model file (which is the output of training.R).

  # Returns:
  # dataframe: A dataframe containing the identifiers and their corresponding predictions.
  
  # Test for presence of nomem_encr
  if( !("nomem_encr" %in% colnames(df)) ) {
    warning("The identifier variable 'nomem_encr' should be in the dataset")
  }

  # Load the model
  model <- readRDS(model_path)
    
  # Preprocess the fake / holdout data
  df <- clean_df(df, background_df)
  
  # Generate predictions from model
  predictions <- predict(model, df) %>% 
    mutate(across(.pred_class, ~ as.numeric(.x) - 1))
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return( df_predict )
}

