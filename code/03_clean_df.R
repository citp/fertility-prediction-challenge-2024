# This file is based on submission.R from the PreFer Challenge but only
# contains the clean_df function. The function cleans the data and selects
# features. This is also where partner linkage happens.

# Set up
start <- Sys.time()

# Load packages if they weren't already loaded via the run_all script
if (!isTRUE(getOption("run_all_executed"))) {
  library(groundhog)
  groundhog.library(c("here", "tidyverse"), "2024-04-23")
}

here() %>% 
  setwd()

clean_df <- function(df, background_df, feature_set, partner, time_shift) {
  # Preprocess the input dataframe to feed the model.

  # Parameters:
  # df (dataframe): The input dataframe containing the raw data (e.g., from PreFer_train_data.csv or PreFer_test_data.csv).
  # background (dataframe): PreFer_train_background_data.csv or PreFer_test_background_data.csv 
  # feature_set (character): "full_features"--full cleaning and partner linkage
  #                          "seven_features"--seven baseline features hand-chosen by the organizers of the PreFer Challenge
  #                          "three_features"--just time shift indicator plus two fertility intention variables
  # partner (logical): whether partner linkage is turned on
  # time_shift (logical): whether time shift is turned on
  
  # Returns:
  # data frame: The cleaned dataframe with only the necessary columns and processed variables.

  #### TIME-SHIFTED DATA INDICATOR ####
  # The time shifted data already has a column called time_shifted_data, where
  # time_shifted_data = 1. For the regular data, we need to create time_shifted_data = 0.
  if (time_shift) {
    if (!"time_shifted_data" %in% colnames(df)) {
      df <- df %>%
        mutate(time_shifted_data = 0)
    }
  }
  
  #### GET HOUSEHOLD IDs AND SIZE FROM BACKGROUND DATA ####
  # Save a copy of background_df before any possible filtering based on time. 
  # Otherwise background_df would not have household IDs for some individuals 
  # who got a new household number since 2018. Those household IDs are
  # necessary for proper CV splits
  background_df20 <- background_df
  
  # Nothing in the following if statement is relevant to the three-feature
  # setting because there would be no household data.
  if (feature_set != "three_features") {
    # If this is time-shifted data, filter the background data to 2017 and 
    # earlier
    if (time_shift) {
      if (unique(df$time_shifted_data) == 1) { 
        background_df <- background_df %>%
          filter(wave <= 201712)
      }
    }
    # For each person, filter to only the most recent wave in which they 
    # appeared. The household IDs in this filtered background file are used for
    # partner matching.
    background_most_recent_wave <- background_df %>%
      group_by(nomem_encr) %>%
      arrange(desc(wave)) %>%
      slice_head() %>%
      ungroup()
    # Merge data about household size with the full train data
    if (feature_set == "full_features") {
      background_most_recent_wave_aantalhh <-
        select(background_most_recent_wave, nomem_encr, aantalhh)
      df <- 
        left_join(df, background_most_recent_wave_aantalhh, by = "nomem_encr")
    }
    
    #### MERGE IN PARTNER DATA IF THE PARTNER ALSO PARTICIPATED IN THE SURVEY ####
    if (partner) {
      # Make a vector of features to merge in from the partner's survey, for use in modeling
      # The set of features depends on whether we use full features or just seven features
      if (feature_set == "full_features") {
        features_to_use_as_partner_data_in_model <- c(
          # Fertility expectations in 2020
          "cf20m128", "cf20m129", "cf20m130",
          # Fertility expectations in 2019
          "cf19l128", "cf19l129", "cf19l130", 
          # Whether ever had kids in 2019 and 2020
          "cf19l454", "cf20m454", 
          # Number of kids reported in 2019 and 2020
          "cf19l455", "cf20m455",
          # Birth year of first child in 2019 and 2020
          "cf19l456", "cf20m456", 
          # Birth year of second child in 2019 and 2020
          "cf19l457", "cf20m457",
          # Birth year of third child in 2019 and 2020
          "cf19l458", "cf20m458",
          # Birth year of fourth child in 2019 and 2020
          "cf19l459", "cf20m459",
          # Birth year of fifth child in 2019 and 2020
          "cf19l460", "cf20m460",
          # Birth year of sixth child in 2019 and 2020
          "cf19l461", "cf20m461",
          # Birth year of seventh child in 2019 and 2020
          "cf19l462", "cf20m462",
          # Birth year of eighth child in 2019 and 2020
          "cf19l463", "cf20m463",
          # Birth year of ninth child in 2019 and 2020
          "cf19l464", "cf20m464",
          # Birth year of tenth child in 2019 and 2020
          "cf19l465", "cf20m465",
          # Birth year of eleventh child in 2019 and 2020
          "cf19l466", "cf20m466",
          # Birth year of twelfth child in 2019 and 2020
          "cf19l467", "cf20m467",
          # Birth year of thirteenth child in 2019 and 2020
          "cf19l468", "cf20m468",
          # Birth year of fourteenth child in 2019 and 2020
          "cf19l469", "cf20m469",
          # Birth year of fifteenth child in 2019 and 2020
          "cf19l470", "cf20m470",
          # Gynecologist
          "ch20m219",
          # Birthyear
          "birthyear_bg"
        )
      } else {
        features_to_use_as_partner_data_in_model <- c(
          # Fertility expectations in 2020
          "cf20m128", "cf20m130",
          # Age
          "age_bg",
          # Gender
          "gender_bg",
          # Marital Status,
          "burgstat_2020",
          # Education
          "oplcat_2020",
          # Whether ever had kids in 2020
          "cf20m454", 
          # Number of kids reported in 2019 and 2020
          "cf20m455"
        )
      }
      
      # Make vectors of features that will be coalesced across waves, for use in the merging process
      # Note: Must list the more recent features first in order for the coalesce function to work
      raw_features_about_living_with_partner <- c("cf20m025", "cf19l025", "cf18k025", "cf17j025", "cf16i025", "cf15h025", 
                                                  "cf14g025", "cf13f025", "cf12e025", "cf11d025", "cf10c025", "cf09b025", "cf08a025")
      raw_features_about_partner_birth_year <- c("cf20m026", "cf19l026", "cf18k026", "cf17j026", "cf16i026", "cf15h026", 
                                                 "cf14g026", "cf13f026", "cf12e026", "cf11d026", "cf10c026", "cf09b026", "cf08a026")
      raw_features_about_partner_gender <- c("cf20m032", "cf19l032", "cf18k032", "cf17j032", "cf16i032", "cf15h032", 
                                             "cf14g032", "cf13f032", "cf12e032", "cf11d032", "cf10c032", "cf09b032", "cf08a032")  
      
      # Select a few features of interest, plus features that will help us double-check that the merged-in person is really the partner
      train_subsetted_columns <- df %>% 
        select("nomem_encr", "gender_bg", "birthyear_bg", "outcome_available",
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
               -all_of(raw_features_about_partner_gender)
        )
      
      # Partner's background data
      background_most_recent_wave_partner <-
        select(background_most_recent_wave, nomem_encr, nohouse_encr, positie)
      
      # Merge household ID and household position data with training data
      train_subsetted_columns <- left_join(train_subsetted_columns, background_most_recent_wave_partner, by = "nomem_encr")
      
      # Create a copy of "train_subsetted_columns" to represent possible partners
      train_partner <- train_subsetted_columns %>%
        rename_with(~ paste0(., "_PartnerSurvey"), -nohouse_encr)
      
      # Merge train_subsetted_columns with train_partner
      # This produces a dataframe that only contains people whose partner also responded to the survey
      subsetted_train_linked_with_partner <- train_subsetted_columns %>%
        left_join(train_partner, by = "nohouse_encr", relationship = "many-to-many") %>%
        filter(
          # Only look at partners for whom outcome is available, as this probably 
          # has to be the case for the test set
          outcome_available_PartnerSurvey == 1,
          # Remove rows where person was linked to self
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
      
      # Select only the columns about the partner (we'll merge this into the full training data, which already has data from self)  
      partner_variables_to_keep <- paste0(features_to_use_as_partner_data_in_model, "_PartnerSurvey")
      subsetted_train_linked_with_partner <- subsetted_train_linked_with_partner %>%
        select(nomem_encr, all_of(partner_variables_to_keep))
      
      # Merge the data about the partner with the full train data
      # This produces a dataframe with everyone from the training data, even if they don't have a partner
      df <- left_join(df, subsetted_train_linked_with_partner, by = "nomem_encr")
      
      # Create an indicator for whether there is partner survey data
      ids_that_have_partner_survey <- subsetted_train_linked_with_partner$nomem_encr
      df <- df %>%
        mutate(partner_survey_available = ifelse(nomem_encr %in% ids_that_have_partner_survey, 1, 0))
    }
  }
  
  
  #### KEEP ONLY ROWS WITH AVAILABLE OUTCOMES, CONDUCT FEATURE ENGINEERING ####
  df <- filter(df, outcome_available == 1)
  if (feature_set != "seven_features") {
    
    # Feature set for the three_features setting
    df <- df %>%
      mutate(
        cf20m130 = ifelse(cf20m128 == 2, 31, cf20m130),
        # Correct a value where calendar year was reported instead of number of 
        # years
        cf20m130 = ifelse(cf20m130 == 2025, 5, cf20m130)
      )
    keepcols <- c("nomem_encr", "time_shifted_data", "cf20m128", "cf20m130")
    
    # Feature set for the full_features setting
    if (feature_set == "full_features") {
      df <- df %>%
        mutate(
          # Impute savings with range midpoints. Two exceptions: We impute 
          # -1200 for those in the smallest category. -1200 is roughly the 
          # average savings of those who are in that category. Similarly we 
          # impute 62500 for those in the largest category. Also, if one does 
          # not have accounts, then one does not have any savings
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
          partner_birth_year18 = ifelse(cf18k024 == 2, NA, coalesce(cf18k026, cf17j026, cf16i026, cf15h026, cf14g026, cf13f026, cf12e026, cf11d026, cf10c026, cf09b026, cf08a026)),
          partner_birth_year19 = ifelse(cf19l024 == 2, NA, coalesce(cf19l026, partner_birth_year18)),
          # Identify year relationship began based on most recent wave in which it was reported
          year_relationship_began18 = ifelse(cf18k024 == 2, NA, coalesce(cf18k028, cf17j028, cf16i028, cf15h028, cf14g028, cf13f028, cf12e028, cf11d028, cf10c028, cf09b028, cf08a028)),
          year_relationship_began19 = ifelse(cf19l024 == 2, NA, coalesce(cf19l028, year_relationship_began18)),
          year_relationship_began20 = ifelse(cf20m024 == 2, NA, coalesce(cf20m028, year_relationship_began19)),
          # If no expected kids, then expected number of kids is 0
          # Note: in some years, "I don't know" was an option for *128; we don't use that info here, so the recoded *129 may not contain all info from *128
          cf18k129 = ifelse(cf18k128 == 2, 0, cf18k129),
          cf19l129 = ifelse(cf19l128 == 2, 0, cf19l129),
          cf20m129 = ifelse(cf20m128 == 2, 0, cf20m129),
          # If no expected kids, then a lower-bound estimate for the number of years
          # within which to have kids is 31 (since the largest value actually reported is 30)
          cf18k130 = ifelse(cf18k128 == 2, 31, cf18k130),
          cf19l130 = ifelse(cf19l128 == 2, 31, cf19l130),
          # Feeling about being single
          cf20m166 = ifelse(cf20m166 == 99, NA, cf20m166),
          # If one never had children, then one does not have any living children
          cf20m455 = ifelse(cf20m454 == 2, 0, cf20m455),
          cf19l455 = ifelse(cf19l454 == 2, 0, cf19l455),
          cf18k455 = ifelse(cf18k454 == 2, 0, cf18k455),
          # Year the most recent child was born
          most_recent_child18 = coalesce(cf18k470, cf18k469, cf18k468, cf18k467, cf18k466, cf18k465, cf18k464, cf18k463, cf18k462, cf18k461, cf18k460, cf18k459, cf18k458, cf18k457, cf18k456),
          most_recent_child19 = coalesce(cf19l470, cf19l469, cf19l468, cf19l467, cf19l466, cf19l465, cf19l464, cf19l463, cf19l462, cf19l461, cf19l460, cf19l459, cf19l458, cf19l457, cf19l456),
          most_recent_child20 = coalesce(cf20m470, cf20m469, cf20m468, cf20m467, cf20m466, cf20m465, cf20m464, cf20m463, cf20m462, cf20m461, cf20m460, cf20m459, cf20m458, cf20m457, cf20m456),
          # Religiosity
          cr20m162 = ifelse(cr20m162 == -9, NA, cr20m162),
          # Scale for feeling towards child
          across(c(cf20m515, cf20m516, cf20m518, cf20m519, cf20m520, cf20m521),
            ~ 8 - .x
          ),
          # Scale on gendered religiosity
          across(c(cr18k101, cr18k102, cr18k103, cr18k104, cr18k105),
            ~ case_when(.x == 1 ~ 3, .x == 2 ~ 1, .x > 2 ~ 2)
          ),
          across(c(cr18k102, cr18k105), ~ 4 - .x),
          # Scale on traditional motherhood
          cv20l109 = 6 - cv20l109,
          # Scale on traditional fatherhood
          across(c(cv20l112, cv20l114, cv20l115), ~ 6 - .x),
          # Scale on traditional marriage
          across(c(cv20l126, cv20l127, cv20l128, cv20l129, cv20l130), ~ 6 - .x),
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
          woning_2020 = case_when(woning_2020 == 1 ~ 1, woning_2020 %in% 2:4 ~ 0),
          # Household income per capita
          hhinc_per_capita20 = nettohh_f_2020 / aantalhh
        ) %>%
        rowwise() %>%
        mutate(
          # Scale for feeling towards child
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
          gendered_religiosity = mean(
            c(cr18k101, cr18k102, cr18k103, cr18k104, cr18k105),
            na.rm = TRUE
          ),
          # Scale on traditional fertility
          traditional_fertility = mean(c(cv10c135, cv10c136, cv10c137, cv10c138),
            na.rm = TRUE
          ),
          # Scale on traditional motherhood
          traditional_motherhood = mean(c(cv20l109, cv20l110, cv20l111),
            na.rm = TRUE
          ),
          # Scale on traditional fatherhood
          traditional_fatherhood = mean(c(cv20l112, cv20l113, cv20l114, cv20l115),
            na.rm = TRUE
          ),
          # Scale on traditional marriage
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
          sexism = mean(c(cv20l151, cv20l152, cv20l153, cv20l154), na.rm = TRUE)
        ) %>%
        ungroup()
      
      if (partner) {
        df <- df %>%
          mutate(
            partner_birth_year20 = ifelse(cf20m024 == 2, NA, coalesce(cf20m026, partner_birth_year19, birthyear_bg_PartnerSurvey)),
            cf19l129_PartnerSurvey = ifelse(cf19l128_PartnerSurvey == 2, 0, cf19l129_PartnerSurvey),
            cf20m129_PartnerSurvey = ifelse(cf20m128_PartnerSurvey == 2, 0, cf20m129_PartnerSurvey),
            cf19l130_PartnerSurvey = ifelse(cf19l128_PartnerSurvey == 2, 31, cf19l130_PartnerSurvey),
            cf20m130_PartnerSurvey = ifelse(cf20m128_PartnerSurvey == 2, 31, cf20m130_PartnerSurvey),
            # Remove some very small categories for 128 variables
            cf20m128_PartnerSurvey = ifelse(cf20m128_PartnerSurvey == 3, NA, cf20m128_PartnerSurvey),
            cf20m455_PartnerSurvey = ifelse(cf20m454_PartnerSurvey == 2, 0, cf20m455_PartnerSurvey),
            cf19l455_PartnerSurvey = ifelse(cf19l454_PartnerSurvey == 2, 0, cf19l455_PartnerSurvey),
            most_recent_child19_PartnerSurvey = coalesce(cf19l470_PartnerSurvey, cf19l469_PartnerSurvey, cf19l468_PartnerSurvey, cf19l467_PartnerSurvey, cf19l466_PartnerSurvey, cf19l465_PartnerSurvey, cf19l464_PartnerSurvey, cf19l463_PartnerSurvey, cf19l462_PartnerSurvey, cf19l461_PartnerSurvey, cf19l460_PartnerSurvey, cf19l459_PartnerSurvey, cf19l458_PartnerSurvey, cf19l457_PartnerSurvey, cf19l456_PartnerSurvey),
            most_recent_child20_PartnerSurvey = coalesce(cf20m470_PartnerSurvey, cf20m469_PartnerSurvey, cf20m468_PartnerSurvey, cf20m467_PartnerSurvey, cf20m466_PartnerSurvey, cf20m465_PartnerSurvey, cf20m464_PartnerSurvey, cf20m463_PartnerSurvey, cf20m462_PartnerSurvey, cf20m461_PartnerSurvey, cf20m460_PartnerSurvey, cf20m459_PartnerSurvey, cf20m458_PartnerSurvey, cf20m457_PartnerSurvey, cf20m456_PartnerSurvey)
          )
      } else {
        df <- df %>%
          mutate(partner_birth_year20 = ifelse(cf20m024 == 2, NA, coalesce(cf20m026, partner_birth_year19)))
      }
      
      # Reduce columns to only those we need
      keepcols <- c(
        "nomem_encr", "time_shifted_data", "partner_survey_available",
        "ca20g012", "cd20m034",
        "cf20m024", "cf20m025", "cf20m029", "cf20m030", "cf20m031",
        "cf19l024", "cf19l025", "cf19l029", "cf19l030", "cf19l031",
        "cf18k024", "cf18k025", "cf18k029", "cf18k030", "cf18k031",
        "cf20m456",
        "cf18k128", "cf19l128", "cf20m128",
        "cf18k129", "cf19l129", "cf20m129",
        "cf18k130", "cf19l130", "cf20m130",
        "cf20m166",
        "cf20m454", "cf20m455", "cf19l454", "cf19l455", "cf18k454", "cf18k455",
        "ch20m004", "ch20m219", "cr20m162",
        "birthyear_bg",
        "belbezig_2020",
        "gender_bg",
        "migration_background_bg",
        "nettohh_f_2020", "nettoink_f_2020",
        "oplmet_2020", "sted_2020", "woning_2020",
        "cf19l180", "cf20m180", "cf19l181", "cf20m181",
        "cf20m128_PartnerSurvey",
        "cf20m129_PartnerSurvey", "cf20m130_PartnerSurvey",
        "cf19l128_PartnerSurvey",
        "cf19l129_PartnerSurvey", "cf19l130_PartnerSurvey",
        "cf19l454_PartnerSurvey", "cf20m454_PartnerSurvey",
        "cf19l455_PartnerSurvey", "cf20m455_PartnerSurvey",
        "cf20m456_PartnerSurvey", "ch20m219_PartnerSurvey",
        "partner_birth_year18", "partner_birth_year19", "partner_birth_year20",
        "year_relationship_began18",
        "year_relationship_began19",
        "year_relationship_began20",
        "most_recent_child18", "most_recent_child19", "most_recent_child20",
        "most_recent_child19_PartnerSurvey",
        "most_recent_child20_PartnerSurvey",
        "child_feeling", "gendered_religiosity", "traditional_fertility",
        "traditional_motherhood", "traditional_fatherhood",
        "traditional_marriage", "working_mother", "sexism",
        "hhinc_per_capita20"
      )
    }
  } else {
    df <- df %>%
      mutate(
        cf20m130 = ifelse(cf20m130 == 2025, 5, cf20m130),
        fert_intentions_2020 = case_when(
          cf20m130 == 0 ~ 0, # currently pregnant/partner is pregnant
          cf20m130 == 1 ~ 1, # want a child within 1 year
          cf20m130 == 2 ~ 2, # want a child within 2 years
          cf20m130 == 3 ~ 3, # want a child within 3 years
          cf20m130 >= 4 & cf20m130 <= 6 ~ 4, # want a child within 4-6 years
          cf20m130 >= 7 & cf20m130 <= 9 ~ 5, # want a child within 7-9 years
          cf20m130 >= 10 ~ 6, # want a child within 10 years or later
          cf20m128 == 2 ~ 7, # don't want children
          TRUE ~ 8 # one category for missing values and "don't know". This way, fert intentions variable can be constructed the same way for all years (in 2016-2019, the category 'don't know' was coded as missing value - i.e. there is no category '3' in the data for some reason) 
        ),
        fert_intentions_2020 = as.factor(fert_intentions_2020),
        # create variable with number of children in 2020
        num_children_2020 = case_when(
          # if never had children - 0
          cf20m454 == 2 ~ 0,
          # if num of children available
          !is.na(cf20m455) ~ cf20m455
          #(in all other cases it's missing)
        ),
        # turning to factors
        # in burgstat_2020, 2 (Separated) and 4 (Widow) rarely occur
        # from codebook: Married; Separated; Divorced; Widow or widower; Never been married
        # combine them into "other"
        burgstat_2020 = case_when(burgstat_2020 == 1 ~ "Married",
          burgstat_2020 == 3 ~ "Divorced",
          burgstat_2020 == 5 ~ "Never been married",
          burgstat_2020 %in% c(2,4) ~ "Separated or widowed"
        ),
        burgstat_2020 = factor(burgstat_2020),
        oplcat_2020 = as.factor(oplcat_2020),
        gender_bg = as.factor(gender_bg)
      )
    if (partner) {
      df <- df %>%
        mutate(
          fert_intentions_2020_PartnerSurvey = case_when(
            cf20m130_PartnerSurvey == 0 ~ 0, # currently pregnant/partner is pregnant
            cf20m130_PartnerSurvey == 1 ~ 1, # want a child within 1 year
            cf20m130_PartnerSurvey == 2 ~ 2, # want a child within 2 years
            cf20m130_PartnerSurvey == 3 ~ 3, # want a child within 3 years
            cf20m130_PartnerSurvey >= 4 & cf20m130_PartnerSurvey <= 6 ~ 4, # want a child within 4-6 years
            cf20m130_PartnerSurvey >= 7 & cf20m130_PartnerSurvey <= 9 ~ 5, # want a child within 7-9 years
            cf20m130_PartnerSurvey >= 10 ~ 6, # want a child within 10 years or later
            cf20m128_PartnerSurvey == 2 ~ 7, # don't want children
            partner_survey_available == 1 ~ 8 # one category for missing values and "don't know". This way, fert intentions variable can be constructed the same way for all years (in 2016-2019, the category 'don't know' was coded as missing value - i.e. there is no category '3' in the data for some reason) 
          ),
          fert_intentions_2020_PartnerSurvey = as.factor(fert_intentions_2020_PartnerSurvey),
          num_children_2020_PartnerSurvey = case_when(
            # if never had children - 0
            cf20m454_PartnerSurvey == 2 ~ 0,
            # if num of children available
            !is.na(cf20m455_PartnerSurvey) ~ cf20m455_PartnerSurvey
            #(in all other cases it's missing)
          ),
          burgstat_2020_PartnerSurvey = case_when(burgstat_2020_PartnerSurvey == 1 ~ "Married",
            burgstat_2020_PartnerSurvey == 3 ~ "Divorced",
            burgstat_2020_PartnerSurvey == 5 ~ "Never been married",
            burgstat_2020_PartnerSurvey %in% c(2,4) ~ "Separated or widowed"
          ),
          burgstat_2020_PartnerSurvey = factor(burgstat_2020_PartnerSurvey),
          oplcat_2020_PartnerSurvey = as.factor(oplcat_2020_PartnerSurvey),
          gender_bg_PartnerSurvey = as.factor(gender_bg_PartnerSurvey)
        )
    }
    
    # Reduce columns to only those we need
    keepcols <- c(
      "nomem_encr", "time_shifted_data", "partner_survey_available",
      "age_bg", "gender_bg", "burgstat_2020", "oplcat_2020",
      "age_bg_PartnerSurvey", "gender_bg_PartnerSurvey",
      "burgstat_2020_PartnerSurvey", "oplcat_2020_PartnerSurvey",
      "fert_intentions_2020", "fert_intentions_2020_PartnerSurvey",
      "num_children_2020", "num_children_2020_PartnerSurvey"
    )
  }
  df <- select(df, any_of(keepcols))
  if (feature_set != "seven_features") {
    df <- mutate(df, across(everything(), as.numeric))
  } else {
    df <- mutate(df, across(where(is.integer), as.numeric))
  }
  
  #### APPEND HOUSEHOLD ID ####
  # We need most recent household IDs to create CV splits, which will be used 
  # by other models. Identify the household each person was a member of at the 
  # last time that person was observed, up through December 2020
  household_linkage <- background_df20 %>% 
    arrange(desc(wave)) %>%
    group_by(nomem_encr) %>%
    slice_head() %>%
    select(nomem_encr, nohouse_encr)
  # Merge the household ID with original_plus_timeshifted_model_df
  df <- left_join(df, household_linkage)
  return(df)
}



# This function applies all relevant combinations of cleaning procedures to a 
# dataframe
clean_all <- function(df, background_df, df_name) {
  df %>%
    clean_df(background_df, "full_features", TRUE, TRUE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_full_features_final.RDS"
    ))
  df %>%
    clean_df(background_df, "full_features", TRUE, FALSE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_full_features_partner.RDS"
    ))
  df %>%
    clean_df(background_df, "full_features", FALSE, TRUE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_full_features_time_shift.RDS"
    ))
  df %>%
    clean_df(background_df, "full_features", FALSE, FALSE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_full_features_original.RDS"
    ))
  df %>%
    clean_df(background_df, "seven_features", TRUE, TRUE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_seven_features_final.RDS"
    ))
  df %>%
    clean_df(background_df, "seven_features", TRUE, FALSE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_seven_features_partner.RDS"
    ))
  df %>%
    clean_df(background_df, "seven_features", FALSE, TRUE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_seven_features_time_shift.RDS"
    ))
  df %>%
    clean_df(background_df, "seven_features", FALSE, FALSE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_seven_features_original.RDS"
    ))
  df %>%
    clean_df(background_df, "three_features", FALSE, TRUE) %>%
    saveRDS(paste0("data/intermediate_files/cleaned_dfs/cleaned_",
      df_name,
      "_three_features_time_shift.RDS"
    ))
}

# Actually clean the dfs
train_2021to2023 <- read.csv("data/PreFer_train_data.csv", encoding = "latin1")
background_df <- "data/PreFer_train_background_data.csv" %>%
  read.csv(encoding = "latin1")
dir.create("data/intermediate_files/cleaned_dfs", recursive = TRUE)
clean_all(train_2021to2023, background_df, "train_2021to2023")
train_2018to2020 <- "data/intermediate_files/train_data_for_2018to2020.csv" %>%
  read.csv(encoding = "latin1")
clean_all(train_2018to2020, background_df, "train_2018to2020")
if(preds_holdout) {
  holdout <- read.csv("data/PreFer_holdout_data.csv", encoding = "latin1")
  holdout_background <- "data/PreFer_holdout_background_data.csv" %>%
    read.csv(encoding = "latin1")
  clean_all(holdout, holdout_background, "holdout")
}
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/03_clean_df.tex")