# Emily Cantrell
# Exploration of merging in data from the partner's survey
library(tidyverse)

# This is a draft of the code for linking partner's data with the primary respondent's data, 
# in order to merge in the partner's fertility intention data. 
# The actual merge happens in submission.R, but I am posting this code because it includes
# quality assurance checks and plots.

# Read in the data
train_full <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/training_data/PreFer_train_data.csv")
outcome <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/training_data/PreFer_train_outcome.csv")
household_full <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/other_data/PreFer_train_background_data.csv")
supplementary_full <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/other_data/PreFer_train_supplementary_data.csv")

#### IF A PERSON HAS A PARTNER IN THE DATA, MERGE THE PARTNER'S DATA INTO THEIR ROW ####

# Select a few features of interest, plus features that will help us double-check that the merged in person is the partner
train <- train_full %>% 
  select(
    "nomem_encr",
    # Expected kids reported in 2020
    "cf20m128", "cf20m129", "cf20m130",
    # Expected kids reported in 2019
    "cf19l128", "cf19l129", "cf19l130", 
    # Demographics
    "gender_bg", "birthyear_bg",
    # Do you live with partner?
    "cf08a025", "cf09b025", "cf10c025", "cf11d025", "cf12e025", "cf13f025", "cf14g025", "cf15h025", "cf16i025", "cf17j025", "cf18k025", "cf19l025", "cf20m025",
    # Partner's birth year
    "cf08a026", "cf09b026", "cf10c026", "cf11d026", "cf12e026", "cf13f026", "cf14g026", "cf15h026", "cf16i026", "cf17j026", "cf18k026", "cf19l026", "cf20m026", 
    # Partner's gender
    "cf08a032", "cf09b032", "cf10c032", "cf11d032", "cf12e032", "cf13f032", "cf14g032", "cf15h032", "cf16i032", "cf17j032", "cf18k032", "cf19l032", "cf20m032"
    ) %>%
  # Collect the most recent response to whether they live with a partner in a single variable 
  mutate(live_with_partner = coalesce(cf20m025, cf19l025, cf18k025, cf17j025, cf16i025, cf15h025, cf14g025, cf13f025, cf12e025, cf11d025, cf10c025, cf09b025, cf08a025)) %>%
  # Collect the most recently reported partner birth year in a single variable 
  mutate(partner_birth_year = coalesce(cf20m026, cf19l026, cf18k026, cf17j026, cf16i026, cf15h026, cf14g026, cf13f026, cf12e026, cf11d026, cf10c026, cf09b026, cf08a026)) %>%
  # Collect the most recent indicator of partner's gender in a single variable
  mutate(partner_gender = coalesce(cf20m032, cf19l032, cf18k032, cf17j032, cf16i032, cf15h032, cf14g032, cf13f032, cf12e032, cf11d032, cf10c032, cf09b032, cf08a032)) %>%
  # Remove raw data that was used in the coalesced variables
  select(-c("cf08a025", "cf09b025", "cf10c025", "cf11d025", "cf12e025", "cf13f025", "cf14g025", "cf15h025", "cf16i025", "cf17j025", "cf18k025", "cf19l025", "cf20m025",
            "cf08a026", "cf09b026", "cf10c026", "cf11d026", "cf12e026", "cf13f026", "cf14g026", "cf15h026", "cf16i026", "cf17j026", "cf18k026", "cf19l026", "cf20m026", 
            "cf08a032", "cf09b032", "cf10c032", "cf11d032", "cf12e032", "cf13f032", "cf14g032", "cf15h032", "cf16i032", "cf17j032", "cf18k032", "cf19l032", "cf20m032"))

# Select the same features from the supplementary data
# Note: the supplementary data is people outside the age range 18-45 during the outcome time window
supplementary <- supplementary_full %>%
  select(
    "nomem_encr",
    # Expected kids reported in 2020
    "cf20m128", "cf20m129", "cf20m130",
    # Expected kids reported in 2019
    "cf19l128", "cf19l129", "cf19l130", 
    # Demographics
    "gender_bg", "birthyear_bg",
    # Do you live with partner?
    "cf08a025", "cf09b025", "cf10c025", "cf11d025", "cf12e025", "cf13f025", "cf14g025", "cf15h025", "cf16i025", "cf17j025", "cf18k025", "cf19l025", "cf20m025",
    # Partner's birth year
    "cf08a026", "cf09b026", "cf10c026", "cf11d026", "cf12e026", "cf13f026", "cf14g026", "cf15h026", "cf16i026", "cf17j026", "cf18k026", "cf19l026", "cf20m026", 
    # Partner's gender
    "cf08a032", "cf09b032", "cf10c032", "cf11d032", "cf12e032", "cf13f032", "cf14g032", "cf15h032", "cf16i032", "cf17j032", "cf18k032", "cf19l032", "cf20m032"
  ) %>%
  # Collect the most recent response to whether they live with a partner in a single variable 
  mutate(live_with_partner = coalesce(cf20m025, cf19l025, cf18k025, cf17j025, cf16i025, cf15h025, cf14g025, cf13f025, cf12e025, cf11d025, cf10c025, cf09b025, cf08a025)) %>%
  # Collect the most recently reported partner birth year in a single variable 
  mutate(partner_birth_year = coalesce(cf20m026, cf19l026, cf18k026, cf17j026, cf16i026, cf15h026, cf14g026, cf13f026, cf12e026, cf11d026, cf10c026, cf09b026, cf08a026)) %>%
  # Collect the most recent indicator of partner's gender in a single variable
  mutate(partner_gender = coalesce(cf20m032, cf19l032, cf18k032, cf17j032, cf16i032, cf15h032, cf14g032, cf13f032, cf12e032, cf11d032, cf10c032, cf09b032, cf08a032)) %>%
  # Remove raw data that was used in the coalesced variables
  select(-c("cf08a025", "cf09b025", "cf10c025", "cf11d025", "cf12e025", "cf13f025", "cf14g025", "cf15h025", "cf16i025", "cf17j025", "cf18k025", "cf19l025", "cf20m025",
            "cf08a026", "cf09b026", "cf10c026", "cf11d026", "cf12e026", "cf13f026", "cf14g026", "cf15h026", "cf16i026", "cf17j026", "cf18k026", "cf19l026", "cf20m026", 
            "cf08a032", "cf09b032", "cf10c032", "cf11d032", "cf12e032", "cf13f032", "cf14g032", "cf15h032", "cf16i032", "cf17j032", "cf18k032", "cf19l032", "cf20m032"))

# For each person, filter to only the most recent wave in which they appeared
household <- household_full %>%
  group_by(nomem_encr) %>%
  arrange(desc(wave)) %>%
  slice_head() %>%
  ungroup() %>%
  select(nomem_encr, nohouse_encr, positie)

# Merge household ID and position data with training data
train <- left_join(train, household)
supplementary <- left_join(supplementary, household)

# Create a copy of "train" merged with "supplementary" to represent the partner
# Some partners may be in the supplementary data because they are outside the 18-45 age range
train_partner <- rbind.data.frame(train, supplementary) %>%
  rename_with(~ paste0(., "_PartnerSurvey"), -nohouse_encr)

# Merge train with train_partner
train_linked_with_partner <- train %>%
  left_join(train_partner, by = "nohouse_encr", relationship = "many-to-many") %>%
  filter(nomem_encr != nomem_encr_PartnerSurvey,
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

# Filter to only people with a non-missing outcome
train_linked_with_partner <- train_linked_with_partner %>%
  left_join(outcome) %>%
  filter(!is.na(new_child))

#### QUALITY ASSURANCE CHECKS ####

# Manually look at the responses
train_linked_with_partner %>% 
  select(live_with_partner, live_with_partner_PartnerSurvey, 
         partner_birth_year, birthyear_bg_PartnerSurvey, 
         partner_birth_year_PartnerSurvey, birthyear_bg, 
         partner_gender, gender_bg_PartnerSurvey, 
         partner_gender_PartnerSurvey, gender_bg) %>%
  head()

# Some households should appear once, and some should appear twice
train_linked_with_partner %>% 
  count(nohouse_encr) %>%
  group_by(n) %>% # Count the number of times a household appears
  count() # counts the number of households that appear a given number of times

# Check that the proportion of same-sex and different-sex couples is roughly aligned
# with the expected proportion based on population rates for same-sex households
train_linked_with_partner %>% 
  group_by(nohouse_encr) %>%
  slice_head() %>%
  ungroup() %>%
  mutate(same_sex = gender_bg == gender_bg_PartnerSurvey) %>%
  count(same_sex) %>% 
  mutate(proportion = n / sum(n))

# Check that partners are usually of similar ages
train_linked_with_partner %>%
  group_by(nohouse_encr) %>%
  slice_head() %>% 
  ungroup() %>%
  mutate(age_gap = birthyear_bg - birthyear_bg_PartnerSurvey) %>%
  count(age_gap) %>% 
  mutate(proportion = n / sum(n)) %>%
  print(n = "Inf")

# Check that all partners are at least 18
train_linked_with_partner %>% 
  filter(birthyear_bg_PartnerSurvey > 2002)

#### MERGE PARTNERED PEOPLE AND NON-PARTNERED PEOPLE BACK INTO SAME DATAFRAME ####
full_train_linked_with_partner <- left_join(train, train_linked_with_partner)

#### EXAMINE PATTERNS ####

# Correction to data: Change the response "2025" to "5" for "within how many years will you have kids?"
train_linked_with_partner <- train_linked_with_partner %>%
  mutate(cf20m130 = ifelse(cf20m130 == 2025, 5, cf20m130),
         cf20m130_PartnerSurvey = ifelse(cf20m130_PartnerSurvey == 2025, 5, cf20m130_PartnerSurvey))

# HOW MANY PEOPLE HAVE FERTILITY QUESTION RESPONSES FROM THE PARTNER? 
train_linked_with_partner %>%
  group_by(is.na(cf20m128_PartnerSurvey)) %>%
  count()
# We get partner data on 2020 fertility intentions for 298 people (among those with non-missing outcomes).
# In total, 987 people have non-missing outcome data. 
# That means we have data on partner's 2020 fertility intentions for 30% of people.

# HOW MANY PEOPLE HAVING MISSING DATA FOR FERTILITY QUESTIONS, BUT THEIR PARTNER ANSWERED IT? 
train_linked_with_partner %>%
  group_by(is.na(cf20m128), is.na(cf20m128_PartnerSurvey)) %>%
  count()
# There are 22 people for whom the ego didn't answer 2020 fertility questions, but partner did answer 2020 fertility questions

# HOW WELL DO PARTNERS' ANSWERS ALIGN? 

# Compare "do you think you will have kids" for self vs. partner
table(train_linked_with_partner$cf20m128, train_linked_with_partner$cf20m128_PartnerSurvey, useNA = "ifany")

# Compare "how many kids" for self vs. partner
table(train_linked_with_partner$cf20m129, train_linked_with_partner$cf20m129_PartnerSurvey, useNA = "ifany")

# Compare "within how many years will you have kids" for self vs. partner
table(train_linked_with_partner$cf20m130, train_linked_with_partner$cf20m130_PartnerSurvey, useNA = "ifany")

# Plot for different-sex couples
train_linked_with_partner %>%
  filter(gender_bg == 2, # Filter to different-sex couples, with woman as the primary person
         gender_bg_PartnerSurvey == 1) %>% 
  ggplot(aes(x = cf20m130, y = cf20m130_PartnerSurvey)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  xlab("Woman's answer") + 
  ylab("Man's answer") + 
  ggtitle("Different-sex couples: Within how many years will you have kids?")

# Same plot as above, but remove the outlier so we can see the other points better
train_linked_with_partner %>%
  filter(gender_bg == 2, # Filter to different-sex couples, with woman as the primary person
         gender_bg_PartnerSurvey == 1) %>% 
  filter(cf20m130_PartnerSurvey < 40) %>%
  ggplot(aes(x = cf20m130, y = cf20m130_PartnerSurvey)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  xlab("Woman's answer") + 
  ylab("Man's answer") + 
  ggtitle("Different-sex couples: Within how many years will you have kids?")

# Same plot as above, with outlier removed, jittered
train_linked_with_partner %>%
  filter(gender_bg == 2, # Filter to different-sex couples, with woman as the primary person
         gender_bg_PartnerSurvey == 1) %>% 
  filter(cf20m130_PartnerSurvey < 40) %>%
  ggplot(aes(x = cf20m130, y = cf20m130_PartnerSurvey)) + 
  geom_jitter(alpha = 0.5) + 
  geom_smooth() + 
  xlab("Woman's answer") + 
  ylab("Man's answer") + 
  ggtitle("Different-sex couples: Within how many years will you have kids?
(Jittered points)")

# Almost all data on fertility intentions is missing among the few same-sex couples, 
# so I didn't make a plot for them.

# HOW MANY PARTNERS COME FROM THE SUPPLEMENTARY DATA?
nrow(train_linked_with_partner)
min(train_full$birthyear_bg)
train_linked_with_partner %>%
  group_by(new_child, birthyear_bg_PartnerSurvey < 1975) %>%
  count()