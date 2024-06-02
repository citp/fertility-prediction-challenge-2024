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
    # Savings
    "ca20g012", "ca20g013", "ca20g078",
    # Number of rooms
    "cd20m034",
    # Data about partner from 2020. We thank Sayash Kapoor and Benedikt Strobl's L1
    # regression for directing our attention towards cf20m029
    "cf20m024", "cf20m025", "cf20m029", "cf20m030", "cf20m031", "cf20m032", # I skipped feature on country of origin because almost all are from Netherlands
    # Data about partner from 2019
    "cf19l024", "cf19l025", "cf19l029", "cf19l030", "cf19l031", "cf19l032",
    # Data about partner from 2018
    "cf18k024", "cf18k025", "cf18k029", "cf18k030", "cf18k031", "cf18k032",
    # Data about partner's birth year (we need to coalesce data across years to find the most recently reported value)
    "cf20m026", "cf19l026", "cf18k026", "cf17j026", "cf16i026", "cf15h026", "cf14g026", "cf13f026", "cf12e026", "cf11d026", "cf10c026", "cf09b026", "cf08a026",
    # Data about year relationship began (we need to coalesce data across years to find the most recently reported value)
    "cf20m028", "cf19l028", "cf18k028", "cf17j028", "cf16i028", "cf15h028", "cf14g028", "cf13f028", "cf12e028", "cf11d028", "cf10c028", "cf09b028", "cf08a028",
    # Birth year of first child
    "cf18k456", "cf19l456", "cf20m456", 
    # Birth year of second child 
    "cf18k457", "cf19l457", "cf20m457",
    # Birth year of third child
    "cf18k458", "cf19l458", "cf20m458",
    # Birth year of fourth child
    "cf18k459", "cf19l459", "cf20m459",
    # Do you think you will have (more) children in the future?
    "cf08a128", "cf09b128", "cf10c128", "cf11d128", "cf12e128", "cf13f128", 
    "cf14g128", "cf15h128", "cf16i128", "cf17j128", "cf18k128", "cf19l128", "cf20m128",
    # How many children do you think you will have in the future?
    "cf08a129", "cf09b129", "cf10c129", "cf11d129", "cf12e129", "cf13f129", 
    "cf14g129", "cf15h129", "cf16i129", "cf17j129", "cf18k129", "cf19l129", "cf20m129",
    # Within how many years do you hope to have your (first-next) child?
    "cf08a130", "cf09b130", "cf10c130", "cf11d130", "cf12e130", "cf13f130", "cf14g130", 
    "cf15h130", "cf16i130", "cf17j130", "cf18k130", "cf19l130", "cf20m130",
    # Feelings about being single
    "cf20m166",
    # Existing children
    "cf20m454", "cf20m455",
    "cf19l454", "cf19l455",
    "cf18k454", "cf18k455",
    # Relationship with child
    "cf20m513",
    "cf20m514",
    "cf20m515",
    "cf20m516",
    "cf20m517",
    "cf20m518",
    "cf20m519",
    "cf20m520",
    "cf20m521",
    # Health
    "ch20m004",
    # Gynaecologist. We thank Sayash Kapoor and Benedikt Strobl's L1
    # regression for directing our attention towards this variable
    "ch20m219",
    # Gendered religiosity
    "cr18k101", "cr18k102", "cr18k103", "cr18k104", "cr18k105",
    # Religiosity
    "cr20m162",
    # Traditional fertility
    "cv10c135", "cv10c136", "cv10c137", "cv10c138",
    # Traditional motherhood
    "cv20l109", "cv20l110", "cv20l111",
    # Traditional fatherhood
    "cv20l112", "cv20l113", "cv20l114", "cv20l115",
    # Traditional marriage
    "cv20l124",
    "cv20l125",
    "cv20l126",
    "cv20l127",
    "cv20l128",
    "cv20l129",
    "cv20l130",
    # Against working mothers
    "cv20l143", "cv20l144", "cv20l145", "cv20l146",
    # Sexism
    "cv20l151", "cv20l152", "cv20l153", "cv20l154",
    # Birth year
    "birthyear_bg",
    # Primary occupation. We thank Sayash Kapoor and Benedikt Strobl's L1
    # regression for directing our attention towards this variable
    "belbezig_2020",
    # Gender
    "gender_bg",
    # Origins
    "migration_background_bg",
    # Houshold Income
    "nettohh_f_2020",
    # Education
    "oplmet_2020",
    # Urban
    "sted_2020",
    # Dwelling type
    "woning_2020"
  )

  ## Keeping data with variables selected
  df <- df[, keepcols]

  ## Keep only rows with available outcomes
  df <- filter(df, outcome_available == 1) %>%
    rowwise() %>%
    mutate(
      # Impute savings with range midpoints. Two exceptions: We impute -1200
      # for those in the smallest category. -1200 is roughly the average
      # savings of those who are in that category. Similarly we impute 62500
      # for those in the largest category.
      # Also, if one does not have accounts, then one does not have any savings
      ca20g012 = case_when(ca20g078 == 0 ~ 0,
        ca20g013 == 1 ~ -1200,
        ca20g013 == 2 ~ 150,
        ca20g013 == 3 ~ 375,
        ca20g013 == 4 ~ 625,
        ca20g013 == 5 ~ 875,
        ca20g013 == 6 ~ 1750,
        ca20g013 == 7 ~ 3750,
        ca20g013 == 8 ~ 6250,
        ca20g013 == 9 ~ 8750,
        ca20g013 == 10 ~ 10750,
        ca20g013 == 11 ~ 12750,
        ca20g013 == 12 ~ 15500,
        ca20g013 == 13 ~ 18500,
        ca20g013 == 14 ~ 22500,
        ca20g013 == 15 ~ 62500,
        ca20g013 == 999 ~ NA,
        ca20g012 < -9999999997 ~ NA,
        TRUE ~ ca20g012
      ),
      # If no partner, then one is not living together with partner
      cf20m025 = ifelse(cf20m024 == 2, 2, cf20m025),
      cf19l025 = ifelse(cf19l024 == 2, 2, cf19l025),
      cf18k025 = ifelse(cf18k024 == 2, 2, cf18k025),
      # If no partner, then one is not married to partner
      cf20m030 = ifelse(cf20m024 == 2, 2, cf20m030),
      cf19l030 = ifelse(cf19l024 == 2, 2, cf19l030),
      cf18k030 = ifelse(cf18k024 == 2, 2, cf18k030),
      # Identify partner's birth year based on most recent wave in which it was reported
      partner_birth_year = coalesce(cf20m026, cf19l026, cf18k026, cf17j026, cf16i026, cf15h026, cf14g026, cf13f026, cf12e026, cf11d026, cf10c026, cf09b026, cf08a026),
      # Identify year relationship began based on most recet wave in which it was reported
      year_relationship_began = coalesce(cf20m028, cf19l028, cf18k028, cf17j028, cf16i028, cf15h028, cf14g028, cf13f028, cf12e028, cf11d028, cf10c028, cf09b028, cf08a028),
      # If no expected kids, then expected number of kids is 0
      # Note: in some years, "I don't know" was an option for *128; we don't use that info here, so the recoded *129 may not contain all info from *128
      cf08a129 = ifelse(cf08a128 == 2, 0, cf08a129),
      cf09b129 = ifelse(cf09b128 == 2, 0, cf09b129),
      cf10c129 = ifelse(cf10c128 == 2, 0, cf10c129),
      cf11d129 = ifelse(cf11d128 == 2, 0, cf11d129),
      cf12e129 = ifelse(cf12e128 == 2, 0, cf12e129),
      cf13f129 = ifelse(cf13f128 == 2, 0, cf13f129),
      cf14g129 = ifelse(cf14g128 == 2, 0, cf14g129),
      cf15h129 = ifelse(cf15h128 == 2, 0, cf15h129),
      cf16i129 = ifelse(cf16i128 == 2, 0, cf16i129),
      cf17j129 = ifelse(cf17j128 == 2, 0, cf17j129),
      cf18k129 = ifelse(cf18k128 == 2, 0, cf18k129),
      cf19l129 = ifelse(cf19l128 == 2, 0, cf19l129),
      cf20m129 = ifelse(cf20m128 == 2, 0, cf20m129),
      # If no expected kids, then a lower-bound estimate for the number of years
      # within which to have kids is 31 (since the largest value actually reported is 30)
      cf08a130 = ifelse(cf08a128 == 2, 31, cf08a130),
      cf09b130 = ifelse(cf09b128 == 2, 31, cf09b130),
      cf10c130 = ifelse(cf10c128 == 2, 31, cf10c130),
      cf11d130 = ifelse(cf11d128 == 2, 31, cf11d130),
      cf12e130 = ifelse(cf12e128 == 2, 31, cf12e130),
      cf13f130 = ifelse(cf13f128 == 2, 31, cf13f130),
      cf14g130 = ifelse(cf14g128 == 2, 31, cf14g130),
      cf15h130 = ifelse(cf15h128 == 2, 31, cf15h130),
      cf16i130 = ifelse(cf16i128 == 2, 31, cf16i130),
      cf17j130 = ifelse(cf17j128 == 2, 31, cf17j130),
      cf18k130 = ifelse(cf18k128 == 2, 31, cf18k130),
      cf19l130 = ifelse(cf19l128 == 2, 31, cf19l130),
      cf20m130 = ifelse(cf20m128 == 2, 31, cf20m130),
      # Correct a value where calendar year was reported instead of number of years
      cf20m130 = ifelse(cf20m130 == 2025, 5, cf20m130),
      # Feeling about being single
      cf20m166 = ifelse(cf20m166 == 99, NA, cf20m166),
      # If one never had children, then one does not have any living children
      cf20m455 = ifelse(cf20m454 == 2, 0, cf20m455),
      cf19l455 = ifelse(cf19l454 == 2, 0, cf19l455),
      cf18k455 = ifelse(cf18k454 == 2, 0, cf18k455),
      # Scale for feeling towards child
      across(c(cf20m515, cf20m516, cf20m518, cf20m519, cf20m520, cf20m521),
        ~ 8 - .x
      ),
      child_feeling = mean(c(cf20m513,
          cf20m514,
          cf20m515,
          cf20m516,
          cf20m517,
          cf20m518,
          cf20m519,
          cf20m520,
          cf20m521
        ),
        na.rm = TRUE
      ),
      # Scale on gendered religiosity
      across(c(cr18k101, cr18k102, cr18k103, cr18k104, cr18k105),
        ~ case_when(.x == 1 ~ 3, .x == 2 ~ 1, .x > 2 ~ 2)
      ),
      across(c(cr18k102, cr18k105), ~ 4 - .x),
      gendered_religiosity = mean(
        c(cr18k101, cr18k102, cr18k103, cr18k104, cr18k105),
        na.rm = TRUE
      ),
      # Religiosity
      cr20m162 = ifelse(cr20m162 == -9, NA, cr20m162),
      # Scale on traditional fertility
      traditional_fertility = mean(c(cv10c135, cv10c136, cv10c137, cv10c138),
        na.rm = TRUE
      ),
      # Scale on traditional motherhood
      cv20l109 = 6 - cv20l109,
      traditional_motherhood = mean(c(cv20l109, cv20l110, cv20l111),
        na.rm = TRUE
      ),
      # Scale on traditional fatherhood
      across(c(cv20l112, cv20l114, cv20l115), ~ 6 - .x),
      traditional_fatherhood = mean(c(cv20l112, cv20l113, cv20l114, cv20l115),
        na.rm = TRUE
      ),
      # Scale on traditional marriage
      across(c(cv20l126, cv20l127, cv20l128, cv20l129, cv20l130), ~ 6 - .x),
      traditional_marriage = mean(c(
          cv20l124, cv20l125, cv20l126, cv20l127, cv20l128, cv20l129, cv20l130
        ),
        na.rm = TRUE
      ),
      # Scale on being against working mothers
      working_mother = mean(c(cv20l143, cv20l144, cv20l145, cv20l146),
        na.rm = TRUE
      ),
      # Scale on sexism
      sexism = mean(c(cv20l151, cv20l152, cv20l153, cv20l154), na.rm = TRUE),
      # Primary occupations: employees, freelancers, seeking lost jobs,
      # students, homemakers, work disability
      belbezig_2020 = ifelse(belbezig_2020 %in% c(1, 3, 4, 7, 8, 10),
        belbezig_2020, NA
      ),
      # Distinguish first- and second- non-Western migrants from others
      migration_background_bg =
        case_when(
          migration_background_bg %in% c(0, 101, 201) ~ 0, # Western origin
          migration_background_bg %in% c(102, 202) ~ migration_background_bg
        ),
      # Combine the lowest levels of education
      oplmet_2020 = case_when(oplmet_2020 > 7 ~ 0, oplmet_2020 == 7 ~ NA,
        TRUE ~ oplmet_2020
      ),
      # Distinguish between home owners and non-home owners
      woning_2020 = case_when(woning_2020 == 1 ~ 1, woning_2020 %in% 2:4 ~ 0)
    ) %>%
    select(-outcome_available,
      -cf20m026, -cf19l026, -cf18k026, -cf17j026, -cf16i026, -cf15h026, -cf14g026, -cf13f026, -cf12e026, -cf11d026, -cf10c026, -cf09b026, -cf08a026,
      -cf20m028, -cf19l028, -cf18k028, -cf17j028, -cf16i028, -cf15h028, -cf14g028, -cf13f028, -cf12e028, -cf11d028, -cf10c028, -cf09b028, -cf08a028,
      -ca20g078, -ca20g013,
      -cf20m454,
      -cf19l454, 
      -cf18k454,
      -cf20m513,
      -cf20m514,
      -cf20m515,
      -cf20m516,
      -cf20m517,
      -cf20m518,
      -cf20m519,
      -cf20m520,
      -cf20m521,
      -cr18k101, -cr18k102, -cr18k103, -cr18k104, -cr18k105,
      -cv10c135, -cv10c136, -cv10c137, -cv10c138,
      -cv20l109, -cv20l110, -cv20l111,
      -cv20l112, -cv20l113, -cv20l114, -cv20l115,
      -cv20l124,
      -cv20l125,
      -cv20l126,
      -cv20l127,
      -cv20l128,
      -cv20l129,
      -cv20l130,
      -cv20l143, -cv20l144, -cv20l145, -cv20l146,
      -cv20l151, -cv20l152, -cv20l153, -cv20l154,
    ) %>%
    mutate(
      across(everything(), as.numeric),
      across(c(belbezig_2020, migration_background_bg, oplmet_2020,
               cf08a128, cf09b128, cf10c128, cf11d128, cf12e128,  
               cf13f128, cf14g128, cf15h128, cf16i128, cf17j128,
               cf18k128, cf19l128, cf20m128), factor) # Some of the *128 are binary but it varies by year, so since we are doing a time-shift, I am one-hot encoding them all for simplicity
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

