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

clean_df <- function(df, background_df = NULL){
  # Preprocess the input dataframe to feed the model.
  ### If no cleaning is done (e.g. if all the cleaning is done in a pipeline) leave only the "return df" command

  # Parameters:
  # df (dataframe): The input dataframe containing the raw data (e.g., from PreFer_train_data.csv or PreFer_fake_data.csv).
  # background (dataframe): Optional input dataframe containing background data (e.g., from PreFer_train_background_data.csv or PreFer_fake_background_data.csv).

  # Returns:
  # data frame: The cleaned dataframe with only the necessary columns and processed variables.

  # Selecting variables for modelling

  keepcols = c("nomem_encr", # ID variable required for predictions,
               "outcome_available", # Is there an outcome to predict?
               "cf20m024",
               "cf20m025",
               "cf20m030",
               "cf20m031",
               "cf20m129",
               "cf20m130",
               "cf20m166",
               "cf20m454",
               "cf20m455",
               "cf20m513",
               "cf20m514",
               "cf20m515",
               "cf20m516",
               "cf20m517",
               "cf20m518",
               "cf20m519",
               "cf20m520",
               "cf20m521",
               "ca20g012",
               "ca20g013",
               "cv20l109",
               "cv20l110",
               "cv20l111",
               "cv20l112",
               "cv20l113",
               "cv20l114",
               "cv20l115",
               "cv20l124",
               "cv20l125",
               "cv20l126",
               "cv20l127",
               "cv20l128",
               "cv20l129",
               "cv20l130",
               "cv10c135",
               "cv10c136",
               "cv10c137",
               "cv10c138",
               "cv20l143",
               "cv20l144",
               "cv20l145",
               "cv20l151",
               "cv20l152",
               "cv20l153",
               "cv20l154",
               "cr18k101",
               "cr18k102",
               "cr18k103",
               "cr18k104",
               "cr18k105",
               "cr20m162",
               "cw20m440",
               "cw20m529",
               "cw20m587",
               "cw20m592",
               "birthyear_bg",
               "gender_bg",
               "migration_background_bg",
               "nettohh_f_2020",
               "oplmet_2020",
               "sted_2020",
               "woning_2020")
  
  ## Keeping data with variables selected
  df <- df[ , keepcols ]
  
  ## Keep only rows with available outcomes
  df <- filter(df, outcome_available == 1)

  return(df)
}

predict_outcomes <- function(df, background_df = NULL, model_path = "./model.rds"){
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

  # Exclude the variable nomem_encr if this variable is NOT in your model
  vars_without_id <- colnames(df)[colnames(df) != "nomem_encr"]
  
  # Generate predictions from model
  predictions <- predict(model, 
                         subset(df, select = vars_without_id)) %>% 
    mutate_all(~ as.numeric(.x) - 1)
  
  # Create predictions that should be 0s and 1s rather than, e.g., probabilities
  predictions <- ifelse(predictions > 0.5, 1, 0)  
  
  # Output file should be data.frame with two columns, nomem_encr and predictions
  df_predict <- data.frame("nomem_encr" = df[ , "nomem_encr" ], "prediction" = predictions)
  # Force columnnames (overrides names that may be given by `predict`)
  names(df_predict) <- c("nomem_encr", "prediction") 
  
  # Return only dataset with predictions and identifier
  return( df_predict )
}
