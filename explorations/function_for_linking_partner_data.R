# Draft of function of for merging in data from the partner's survey
library(tidyverse)

# This is a draft of the code for linking partner's data with the primary respondent's data, 
# in order to merge in the partner's fertility intention data. 
# I started with the code in exploration_of_linking_partner_data.R, and turned it into a function, 
# with some edits for clarity.

# Read in the data
train_full <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/training_data/PreFer_train_data.csv")
background_full <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/other_data/PreFer_train_background_data.csv")

# Create function to merge in partner data
merge_in_partner_data <- function(train_data, background_data) { 
  
  # Make a vector of features to merge in from the partner's survey, for use in modeling
  features_to_use_as_partner_data_in_model <- c(
    # Fertility expectations in 2020
    "cf20m128", "cf20m129", "cf20m130",
    # Fertility expectations in 2019
    "cf19l128", "cf19l129", "cf19l130", 
    # Number of kids reported in 2019 and 2020
    "cf19l455", "cf20m455")
  
  # Make vectors of features that will be coalesced across waves, for use in the merging process
  # Note: Must list the more recent features first in order for the coalesce function to work
  raw_features_about_living_with_partner <- c("cf20m025", "cf19l025", "cf18k025", "cf17j025", "cf16i025", "cf15h025", 
                                              "cf14g025", "cf13f025", "cf12e025", "cf11d025", "cf10c025", "cf09b025", "cf08a025")
  raw_features_about_partner_birth_year <- c("cf20m026", "cf19l026", "cf18k026", "cf17j026", "cf16i026", "cf15h026", 
                                              "cf14g026", "cf13f026", "cf12e026", "cf11d026", "cf10c026", "cf09b026", "cf08a026")
  raw_features_about_partner_gender <- c("cf20m032", "cf19l032", "cf18k032", "cf17j032", "cf16i032", "cf15h032", 
                                         "cf14g032", "cf13f032", "cf12e032", "cf11d032", "cf10c032", "cf09b032", "cf08a032")  
        
  # Select a few features of interest, plus features that will help us double-check that the merged-in person is really the partner
  train_subsetted_columns <- train_data %>% 
        select("nomem_encr", 
               "gender_bg", 
               "birthyear_bg",
               all_of(features_to_use_as_partner_data_in_model),
               all_of(raw_features_about_living_with_partner),
               all_of(raw_features_about_partner_birth_year), 
               all_of(raw_features_about_partner_gender)
        ) %>%
        # Collect the most recent response to whether they live with a partner in a single variable 
        mutate(live_with_partner = coalesce(!!!syms(raw_features_about_living_with_partner))) %>%
        # Collect the most recently reported partner birth year in a single variable 
        mutate(partner_birth_year = coalesce(!!!syms(raw_features_about_partner_birth_year))) %>%
        # Collect the most recent indicator of partner's gender in a single variable
        mutate(partner_gender = coalesce(!!!syms(raw_features_about_partner_gender))) %>%
        # Remove raw data that was used in the coalesced variables
        select(-all_of(raw_features_about_living_with_partner), 
               -all_of(raw_features_about_partner_birth_year),
               -all_of(raw_features_about_partner_gender))
  
  # If this is time-shifted data, filter the background data to 2017 and earlier
  if("time_shifted_data" %in% colnames(train_data)) { 
    background_data <- background_data %>%
      filter(wave <= 201712)
  }
  
  # For each person, filter to only the most recent wave in which they appeared
  background_most_recent_wave <- background_data %>%
    group_by(nomem_encr) %>%
    arrange(desc(wave)) %>%
    slice_head() %>%
    ungroup() %>%
    select(nomem_encr, nohouse_encr, positie)
  
  # Merge household ID and household position data with training data
  train_subsetted_columns <- left_join(train_subsetted_columns, background_most_recent_wave, by = "nomem_encr")
  
  # Create a copy of "train_subsetted_columns" to represent possible partners
  train_partner <- train_subsetted_columns %>%
    rename_with(~ paste0(., "_PartnerSurvey"), -nohouse_encr)
  
  # Merge train_subsetted_columns with train_partner
  # This produces a dataframe that only contains people whose partner also responded to the survey
  subsetted_train_linked_with_partner <- train_subsetted_columns %>%
    left_join(train_partner, by = "nohouse_encr", relationship = "many-to-many") %>%
    filter(# Remove rows where person was linked to self
           nomem_encr != nomem_encr_PartnerSurvey,
           # Filter to only people who are head of household, wedded partner, or unwedded partner in most recent wave where they appeared
           positie %in% c(1,2,3), 
           positie_PartnerSurvey %in% c(1,2,3), 
           # Filter to people from households where at least one supposed partner reported living together with a partner 
           ((live_with_partner == 1) | (live_with_partner_PartnerSurvey ==1)),
           # Remove rows where reported birthyears are mismatched 
           (partner_birth_year == birthyear_bg_PartnerSurvey | is.na(partner_birth_year) | is.na(birthyear_bg_PartnerSurvey)),
           (partner_birth_year_PartnerSurvey == birthyear_bg | is.na(partner_birth_year_PartnerSurvey) | is.na(birthyear_bg)),
           # Remove rows where reported genders are mismatched
           (partner_gender == gender_bg_PartnerSurvey | is.na(partner_gender) | is.na(gender_bg_PartnerSurvey)), 
           (partner_gender_PartnerSurvey == gender_bg | is.na(partner_gender_PartnerSurvey) | is.na(gender_bg))
           ) 
  
  # Select only the data about the partner (we'll merge this into the full training data, which already has data on the ego)  
  partner_variables_to_keep <- paste0(features_to_use_as_partner_data_in_model, "_PartnerSurvey")
  subsetted_train_linked_with_partner <- subsetted_train_linked_with_partner %>%
    select(nomem_encr, all_of(partner_variables_to_keep))
  
  # Merge the data about the partner with the full train data
  # This produces a dataframe with everyone from the training data, even if they don't have a partner
  full_train_linked_with_partner <- left_join(train_data, subsetted_train_linked_with_partner, by = "nomem_encr")
  
  # Create an indicator for whether there is partner survey data
  ids_that_have_partner_survey <- subsetted_train_linked_with_partner$nomem_encr
  full_train_linked_with_partner <- full_train_linked_with_partner %>%
    mutate(partner_survey_available = ifelse(nomem_encr %in% ids_that_have_partner_survey, 1, 0))
  
  return(full_train_linked_with_partner)
  }

#### QUALITY ASSURANCE CHECKS ####

# Check that responses are usually reasonably aligned between ego and partner
toy <- merge_in_partner_data(train_full, background_full)

# How many kids do you have?
table(toy$cf20m455, toy$cf20m455_PartnerSurvey, useNA = "ifany")
table(toy$cf19l455, toy$cf19l455_PartnerSurvey, useNA = "ifany")

# Do you expect to have more kids? (3 = "I don't know")
table(toy$cf20m128, toy$cf20m128_PartnerSurvey, useNA = "ifany")
table(toy$cf19l128, toy$cf19l128_PartnerSurvey, useNA = "ifany")

# How many more kids do you expect to have?
table(toy$cf20m129, toy$cf20m129_PartnerSurvey, useNA = "ifany")
table(toy$cf19l129, toy$cf19l129_PartnerSurvey, useNA = "ifany")

# Within how many years do you expect to have more kids?
table(toy$cf20m130, toy$cf20m130_PartnerSurvey, useNA = "ifany")
table(toy$cf19l130, toy$cf19l130_PartnerSurvey, useNA = "ifany")

# Test the function on time-shifted data
train_2018to2020 <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/time_shifted_data/train_data_for_2018to2020.csv")
toy_time_shifted <- merge_in_partner_data(train_2018to2020, background_full)

# How many kids do you have?
table(toy_time_shifted$cf20m455, toy_time_shifted$cf20m455_PartnerSurvey, useNA = "ifany")
table(toy_time_shifted$cf19l455, toy_time_shifted$cf19l455_PartnerSurvey, useNA = "ifany")