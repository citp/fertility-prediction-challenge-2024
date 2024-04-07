# This is an example script to train your model given the (cleaned) input dataset.
# 
# This script will not be run on the holdout data, 
# but the resulting model model.joblib will be applied to the holdout data.
# 
# It is important to document your training steps here, including seed, 
# number of folds, model, et cetera

train_save_model <- function(cleaned_df, outcome_df) {
  # Trains a model using the cleaned dataframe and saves the model to a file.

  # Parameters:
  # cleaned_df (dataframe): The cleaned data from clean_df function to be used for training the model.
  # outcome_df (dataframe): The data with the outcome variable (e.g., from PreFer_train_outcome.csv or PreFer_fake_outcome.csv).

  ## This script contains a bare minimum working example
  set.seed(1) # not useful here because logistic regression deterministic

  # Combine cleaned_df and outcome_df
  model_df <- merge(cleaned_df, outcome_df, by = "nomem_encr") %>%
    filter(!is.na(new_child)) %>%
    mutate(new_child = factor(new_child))

  # Given a data frame with two columns "obs" and "pred" featuring observed and
  # predicted values, this function produces an F1 score. However, if no
  # positive cases are predicted, F1 will be undefined, so we'll return
  # recall = 0 as f1 instead. The lev and model arguments are not used here but
  # placeholders are required so that summaryFunction could work.
  f1_summary <- function(data, lev = NULL, model = NULL) {
    f1 <- F1_Score(data$obs, data$pred, positive = "1")
    if (length(which(data$pred == "1")) == 0) {
      warning(
        "No positive cases predicted. Returning recall = 0 as f1 instead."
      )
      c(f1 = Recall(data$obs, data$pred, positive = "1"))
    } else {
      c(f1 = F1_Score(data$obs, data$pred, positive = "1"))
    }
  }

  # Set up cross validation
  train_control <- trainControl(
    method = "cv", number = 5,
    summaryFunction = f1_summary
  )

  # Logistic regression model
  model <- train(new_child ~ age, model_df,
    family = "binomial", method = "glm",
    trControl = train_control, metric = "f1"
  )

  # Save the model
  saveRDS(model, "model.rds")
}
