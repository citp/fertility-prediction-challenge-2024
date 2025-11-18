# PreFer Challenge: Code for time-shifting feature data
# Emily Cantrell

# PURPOSE OF THIS FILE
# This file creates "more" observations for the LISS fertility
# prediction challenge by repurposing feature data as outcome data.
# Think of the challenge as: "Predict whether the respondent will have a new child in 
# years [t, t+1, t+2]." Our submissions will be scored on t = 2021, but we can also set 
# "t" to other years to create more outcome observations, and we can get the data for those 
# years by observing it in the feature set.
 
start <- Sys.time()

# Load packages if they weren't already loaded via the run_all script
if (!isTRUE(getOption("run_all_executed"))) {
  library(groundhog)
  groundhog.library(c("here", "tidyverse"), "2024-04-23")
}

setwd(here())

# Load data
# Main training data file
train_data <- read.csv('data/PreFer_train_data.csv')
# Data file for people born prior to 1975 and after 2002 
supplementary_data <- read.csv('data/PreFer_train_supplementary_data.csv')

###### STEP 0: COMBINE THE TRAIN DATA AND SUPPLEMENTARY DATA ########

#### Make sure it's okay to rbind them #####
# Make sure the same person never appears in both dataframes
any(train_data$nomem_encr %in% supplementary_data$nomem_encr) # should be false
any(supplementary_data$nomem_encr %in% train_data$nomem_encr) # should be false
# Make sure they have the same columns 
unique(colnames(train_data) == colnames(supplementary_data)) # only value should be true
# Check whether all corresponding columns are of the same type in both dataframes
train_data_column_types <- sapply(train_data, class)
supplementary_data_column_types <- sapply(supplementary_data, class)
column_type_match <- train_data_column_types == supplementary_data_column_types
examine_mismatches <- cbind.data.frame(colnames(train_data), column_type_match)
table(examine_mismatches$column_type_match)
# I found that 2085 out of 31634 have a column type mismatch. 
# bind_rows is able to handle this the column type mismatches in most cases; 
# however, I need to handle cw14g164 because it causes bind_rows to throw an error.
train_data <- train_data %>%
  mutate(cw14g164 = as.character(cw14g164))

# Bind the two dataframes together
features <- bind_rows(train_data, supplementary_data)

# Check that the row and column numbers are as expected
ncol(features) == ncol(train_data) # should be true
nrow(features) == nrow(train_data) + nrow(supplementary_data) # should be true

# Remove the original df
rm(train_data)
rm(supplementary_data)
gc()

######### HOW ARE THE VARIABLE NAMES STRUCTURED? #########
# "cf" at the start refers to the Family and Household module. 
# The question about "have you ever had children" always ends in 454. 
# The question about "how many children" always ends in 455. 
# The letter before 454/455 progresses alphabetically. For example: 
# Number of kids 2016 (Wave 9): cf16i455
# Number of kids 2017 (Wave 10): cf17j455
# Number of kids 2018 (Wave 11): cf18k455
# Number of kids 2019 (Wave 12): cf19l455

###### STEP 1: MARK FEATURES WE DON'T WANT TO TIME SHIFT AS NA ######

# The following features are not associated with a specific wave, and it's unclear 
# how to time-shift them in a meaningful way. We are unlikely to use them, so I am 
# recoding them as NA.
features <- features %>%
  mutate_at(vars(groep, Total, herinnering, 
                 starts_with("Datum"), 
                 starts_with("Tijd"), 
                 starts_with("maand")), 
            ~NA)

# The following features are not associated with a specific wave, and will
# require special attention later in the code: 
# "nomem_encr" (this is the ID number; need to edit it to distinguish the person from the same person in another year)
# "outcome_available" (need to create this based on the shifted outcome variable)
# "birthyear_bg" (change the value to reflect the time shift)
# "cf11d026" (partner birth year 2011)
# "cf12e026" (partner birth year 2012)
# "cf13f026" (partner birth year 2013)
# "cf14g026" (partner birth year 2014)
# "cf15h026" (partner birth year 2015)
# "cf16i026" (partner birth year 2016)
# "cf17j026" (partner birth year 2017)
# "cf18k026" (partner birth year 2018)
# "cf19l026" (partner birth year 2019)
# "cf20m026" (partner birth year 2020)
# "cf11d028" (year relationship began 2011)
# "cf12e028" (year relationship began 2012)
# "cf13f028" (year relationship began 2013)
# "cf14g028" (year relationship began 2014)
# "cf15h028" (year relationship began 2015)
# "cf16i028" (year relationship began 2016)
# "cf17j028" (year relationship began 2017)
# "cf18k028" (year relationship began 2018)
# "cf19l028" (year relationship began 2019)
# "cf20m028" (year relationship began 2020)
# "cf18k029" (year started living together 2018)
# "cf19l029" (year started living together 2019)
# "cf20m029" (year started living together 2020)
# "cf18k031" (year married 2018)
# "cf19l031" (year married 2019)
# "cf20m031" (year married 2020)
# "cf18k456" (first child birthyear 2018)
# "cf19l456" (first child birthyear 2019)
# "cf20m456" (first child birthyear 2020)
# "cf18k457" (second child birthyear 2018)
# "cf19l457" (second child birthyear 2019)
# "cf20m457" (second child birthyear 2020)
# "cf18k458" (third child birthyear 2018)
# "cf19l458" (third child birthyear 2019)
# "cf20m458" (third child birthyear 2020)
# "cf18k459" (fourth child birthyear 2018)
# "cf19l459" (fourth child birthyear 2019)
# "cf20m459" (fourth child birthyear 2020)
# "cf18k460" (fifth child birthyear 2018)
# "cf19l460" (fifth child birthyear 2019)
# "cf20m460" (fifth child birthyear 2020)
# "cf18k461" (sixth child birthyear 2018)
# "cf19l461" (sixth child birthyear 2019)
# "cf20m461" (sixth child birthyear 2020)
# "cf18k462" (seventh child birthyear 2018)
# "cf19l462" (seventh child birthyear 2019)
# "cf20m462" (seventh child birthyear 2020)
# "cf18k463" (eighth child birthyear 2018)
# "cf19l463" (eighth child birthyear 2019)
# "cf20m463" (eighth child birthyear 2020)
# "cf18k464" (ninth child birthyear 2018)
# "cf19l464" (ninth child birthyear 2019)
# "cf20m464" (ninth child birthyear 2020)
# "cf18k465" (tenth child birthyear 2018)
# "cf19l465" (tenth child birthyear 2019)
# "cf20m465" (tenth child birthyear 2020)
# "cf18k466" (eleventh child birthyear 2018)
# "cf19l466" (eleventh child birthyear 2019)
# "cf20m466" (eleventh child birthyear 2020)
# "cf18k467" (twelfth child birthyear 2018)
# "cf19l467" (twelfth child birthyear 2019)
# "cf20m467" (twelfth child birthyear 2020)
# "cf18k468" (thirteenth child birthyear 2018)
# "cf19l468" (thirteenth child birthyear 2019)
# "cf20m468" (thirteenth child birthyear 2020)
# "cf18k469" (fourteenth child birthyear 2018)
# "cf19l469" (fourteenth child birthyear 2019)
# "cf20m469" (fourteenth child birthyear 2020)
# "cf18k470" (fifteenth child birthyear 2018)
# "cf19l470" (fifteenth child birthyear 2019)
# "cf20m470" (fifteenth child birthyear 2020)
# "age_bg" (age, change in direction opposite to birthyears)

# The following features are not associated with a specific wave, and can be left as-is
# because they are typically time-invariant: 
# "gender_bg"
# "migration_background_bg"

###### STEP 2: ALIGN THE FEATURES FOR THE 2018-2020 COHORT WITH THE FEATURES FOR THE 2021-2023 COHORT #######

# Rename the features so that they have the same names as the features from corresponding years 
# in the 2021 cohort. The goal is for features to have the same name if they represent the 
# same construct and are the same amount of time prior to year t. For example, for the 2018 
# fertility cohort, positie2017 refers to year t-1. For the the 2021 fertility cohort, 
# positie2020 refers to year t-1. I will therefore rename positie2017 to be positie2020, to mimic 
# what that variable would have been called if everything for the 2018 cohort were 
# shifted three years forward to match the 2021 cohort.

# This step also removes features from years t and later (we only want features from 
# prior to year t).

# There are two ways the year can appear in the variable name: 
# 1) The last four characters (e.g., nohouse_encr2007) (background variables)
# 2) The second and third character after "cf" (e.g., cf08a_m) (family module variables)

# Identify the naming structure of each variable (i.e., how it indicates the year)
variable_names <- as.data.frame(colnames(features)) %>%
  rename(original_var_name = `colnames(features)`)
variable_names <- variable_names %>%
  mutate(naming_structure= case_when(
    str_detect(original_var_name, "[0-9]{4}$") ~ "Last 4 characters", 
    str_detect(original_var_name, "^c[a-z][0-9]{2}") ~ "3rd and 4th characters",
    TRUE ~ NA_character_ # Personal ID number, birth year, and gender have no year
  )) 

# Identify the year to which each variable refers, and the letter associated with that year
# (see next code chunk for more info about the special letter)
variable_names <- variable_names %>%
  mutate(variable_year = case_when(
    naming_structure == "Last 4 characters" ~ str_sub(original_var_name, start = -2), 
    naming_structure == "3rd and 4th characters" ~ str_sub(original_var_name, start = 3, end = 4),
    TRUE ~ NA_character_ # Personal ID number, birth year, and gender have no year
  ))  %>%
  mutate(variable_year = as.numeric(variable_year)) %>%
  mutate(special_letter = case_when(
    naming_structure == "3rd and 4th characters" ~ str_sub(original_var_name, start = 5, end = 5)
  ))

# For variables with the "3rd and 4th digit" naming structure, there is also a letter
# as the fifth digit that varies by year. 2007 and 2008 both have 'a' as their letter. 
# Then the letters proceed alphabetically by year, e.g., 2009 = 'b', 2010 = 'c', 2011 = 'd'.
# Here I create a function to "add" the shift to a letter. For example, the letter "a" plus 
# three returns the letter "d". 
addition_for_letters <- function(my_letter, my_addition) { 
  letters[match(my_letter, letters) + my_addition]
  } 

# Determine how many years to shift the names of the features forward
year_t <- 2018 # Indicates that the outcome comes from 2018-2020; we might do other years later
years_to_shift = 2021 - year_t 

# Create an column that lists the "shifted" variable names
variable_names <- variable_names %>%
  mutate(shifted_year = variable_year + years_to_shift) %>%
  mutate(shifted_letter = addition_for_letters(special_letter, years_to_shift)) %>%
  mutate(new_var_name = case_when(
    naming_structure == "Last 4 characters" ~ paste0(str_sub(original_var_name, start=1, end=-3), 
                                                     shifted_year), 
    naming_structure == "3rd and 4th characters" ~ paste0(str_sub(original_var_name, start=1, end=2),
                                                          shifted_year,
                                                          shifted_letter,
                                                          str_sub(original_var_name, start=6)), 
    TRUE ~ original_var_name # For variables with no year in the name, keep the original name
    ))

# Record the names of the real features (i.e., before time-shifting the names)
real_feature_names <- colnames(features)

# Rename the columns to "time-shift" them to align with the column names for the 2021 cohort
colnames(features) <- variable_names$new_var_name

# Select only features that exist in the feature set for the 2020 cohort 
# This does two things: (1) Restricts the data to only features from years t-1 and earlier; 
# (2) restricts the data to only features that measure constructs also measured in the 
# feature set for the 2020 cohort (I think they asked the same questions every year, but
# there may have been changes I'm not aware of)
features <- features %>%
  select(any_of(real_feature_names))

# Shift the content of year features by the appropriate number of years
# The variable name will remain the same. 
# For year variables, we want to know what the equivalent yyear would be for someone 
# in the 2021 cohort. Therefore, we ADD the number of years by which we are time-shifting.
features <- features %>% 
  mutate(across(c("birthyear_bg",
                  "cf11d026", "cf12e026", "cf13f026", "cf14g026", "cf15h026", 
                  "cf16i026", "cf17j026", "cf18k026", "cf19l026", "cf20m026",
                  "cf11d028", "cf12e028", "cf13f028", "cf14g028", "cf15h028", 
                  "cf16i028", "cf17j028", "cf18k028", "cf19l028", "cf20m028",
                  "cf18k029", "cf19l029", "cf20m029",
                  "cf18k031", "cf19l031", "cf20m031",
                  "cf18k456", "cf19l456", "cf20m456",
                  "cf18k457", "cf19l457", "cf20m457",
                  "cf18k458", "cf19l458", "cf20m458",
                  "cf18k459", "cf19l459", "cf20m459",
                  "cf18k460", "cf19l460", "cf20m460",
                  "cf18k461", "cf19l461", "cf20m461",
                  "cf18k462", "cf19l462", "cf20m462",
                  "cf18k463", "cf19l463", "cf20m463",
                  "cf18k464", "cf19l464", "cf20m464",
                  "cf18k465", "cf19l465", "cf20m465",
                  "cf18k466", "cf19l466", "cf20m466",
                  "cf18k467", "cf19l467", "cf20m467",
                  "cf18k468", "cf19l468", "cf20m468",
                  "cf18k469", "cf19l469", "cf20m469",
                  "cf18k470", "cf19l470", "cf20m470",),
                ~ .x + years_to_shift),
         age_bg = age_bg - years_to_shift)

# Handle other special features
# nettohh_f_2020 (net household income in euros) and nettoink_f_2020 (net individual income in euros) needs to be adjusted for inflation.
# Inflation in euros Sept. 2017 to Sept. 2020 was 5.6% according to https://tools.csb.gov.lv/cpi_calculator/en/2017M09-2020M09/0/100
# I got 5.7% inflation when I calculated inflation in the Netherlands 9/2017 to 9/2020 based 
# on https://www.cbs.nl/en-gb/news/2022/40/inflation-rate-up-to-14-5-percent-in-september (1.019*1.026*1.011)
# (I chose Sept because it looks like the survey was done in Sept/Oct)
features <- features %>%
  mutate(nettohh_f_2020 = nettohh_f_2020*1.057,
         nettoink_f_2020 = nettoink_f_2020*1.057)

############ STEP 3: INDICATE THAT DATA IS TIME-SHIFTED AND RENAME ID #############

# For the 2018-2020 cohort, rename ID number and create a feature that indicates 
# that this is time-shifted data
features_for_2018to2020 <- features %>%
  mutate(time_shifted_data = 1)

######## STEP 4: CREATE "OUTCOME AVAILABLE" INDICATOR AND RESTRICT AGE RANGE ######## 

# Remove the original outcome_available column
features_for_2018to2020 <- features_for_2018to2020 %>%
  select(-outcome_available)

# Read in the time-shifted outcome data
outcome_2018to2020 <- read.csv("data/intermediate_files/outcome_2018to2020.csv")

# Create indicator for whether outcome is available
outcome_available_df <- outcome_2018to2020 %>%
  mutate(outcome_available = ifelse(!is.na(new_child), 1, 0)) %>%
  select(-new_child)

# Merge the indicator of whether the outcome is available with the features. 
# This code puts the outcome_available column as the second column, just like in train_data.
# By doing left_join, I restrict to only people who are in the correct age range in 
# years 2018-2020, because the outcome data is already restricted to only people in 
# the correct age range. 
features_for_2018to2020 <- left_join(outcome_available_df, features_for_2018to2020, by = "nomem_encr")

# Check that ages are between approx. 18 and 45 
# (there could be a few 17 and 46 years old, depending where in the year their birthday falls)
table(features$age_bg)

####### STEP 5: CREATE COLUMNS THAT ARE MISSING FROM TIME-SHIFTED DATA ####### 
# There are some columns that we are using in our model that don't exist in the 
# time-shifted data. There are various reasons these don't exist in the time-shifted data, 
# as documented in https://github.com/citp/fertility-prediction-challenge-2024/issues/10#issuecomment-2101075180
# Creating these columns with a value of "NA" will allow the data cleaning function to work as intended.
features_for_2018to2020 <- features_for_2018to2020 %>% 
  mutate(ca20g012 = NA, 
         ca20g013 = NA, 
         ca20g078 = NA, 
         cr20m162 = NA, 
         cv10c135 = NA,
         cv10c136 = NA, 
         cv10c137 = NA, 
         cv10c138 = NA,
         cf08a128 = NA, 
         cf09b128 = NA, 
         cf10c128 = NA,
         cf08a129 = NA,
         cf09b129 = NA,
         cf10c129 = NA, 
         cf08a130 = NA, 
         cf09b130 = NA, 
         cf10c130 = NA, 
         cf08a026 = NA, 
         cf09b026 = NA, 
         cf10c026 = NA, 
         cf08a028 = NA, 
         cf09b028 = NA, 
         cf10c028 = NA, 
         cf08a025 = NA, 
         cf09b025 = NA, 
         cf10c025 = NA,
         cf08a032 = NA, 
         cf09b032 = NA, 
         cf10c032 = NA
  )

######## STEP 6: SAVE THE FILES! ######## 
dir.create("data/intermediate_files")
write_csv(features_for_2018to2020, "data/intermediate_files/train_data_for_2018to2020.csv")
dir.create("numbers/timing", recursive = TRUE)
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/02_feature_time_shift.tex")