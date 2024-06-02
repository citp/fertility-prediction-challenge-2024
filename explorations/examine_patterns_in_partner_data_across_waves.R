# Emily Cantrell
# Exploration of data about partner from LISS 
library(tidyverse)

# The purpose of this code is to determine which features about the partner requires us to 
# coalesce data across waves. For example: 
# (1) Partner's birthyear is only reported once. If the person remains with the same partner, the birthyear
# is not reported again in subsequent waves. Therefore, we need to coalesce data across waves.
# (2) Partner's gender is recorded repeatedly across waves even if there is no change. Therefore, 
# we can use data from just the most recent wave (or a few waves), and don't need to coalesce across waves.

# Read in the data
train_full <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/training_data/PreFer_train_data.csv")
outcome <- read.csv("/Users/ecantrell/Documents/PreFer\ 2024/prefer_data/training_data/PreFer_train_outcome.csv")

# Filter to only people with non-missing outcomes
train <- train_full %>%
  left_join(outcome) %>%
  filter(!is.na(new_child))
  
# Do you currently have a partner?
train %>% 
  select(cf08a024, cf09b024, cf10c024, cf11d024, cf12e024, cf13f024, cf14g024, cf15h024, cf16i024, cf17j024, cf18k024, cf19l024, cf20m024) %>%
  View()
# Decision: no need to coalesce across years; the same value is recorded repeatedly across waves

# Do you live with that partner? 
train %>%
  select(cf08a025, cf09b025, cf10c025, cf11d025, cf12e025, cf13f025, cf14g025, cf15h025, cf16i025, cf17j025, cf18k025, cf19l025, cf20m025) %>% 
  View()
# Decision: no need to coalesce across years; the same value is recorded repeatedly across waves

# What is his or her year of birth?
train %>% 
  select(cf20m024, cf08a026, cf09b026, cf10c026, cf11d026, cf12e026, cf13f026, cf14g026, cf15h026, cf16i026, cf17j026, cf18k026, cf19l026, cf20m026) %>%
  View()
# Decision: the partner's birthyear seems to be only collected in one wave, but the wave varies by person depending 
# when they started the survey or got together with the partner, so we should make a variable that takes the most 
# recently reported partner birthyear (use the "coalesce" function). 

# In what country was your partner born?
train %>% 
  select(cf08a027, cf09b027, cf10c027, cf11d027, cf12e027, cf13f027, cf14g027, cf15h027, cf16i027, cf17j027, cf18k027, cf19l027, cf20m027) %>%
  View()
train %>% group_by(cf08a027) %>% count()
# Decision: almost all partners were born in Netherlands or have NA for this question, so I won't use it

# In what year did relationship begin? 
train %>% 
  select(cf08a028, cf09b028, cf10c028, cf11d028, cf12e028, cf13f028, cf14g028, cf15h028, cf16i028, cf17j028, cf18k028, cf19l028, cf20m028) %>%
  View()
# Decision: coalesce the data across years to get the most recently reported value

# In what year did you start living with partner?
train %>%
  select(cf08a029, cf09b029, cf10c029, cf11d029, cf12e029, cf13f029, cf14g029, cf15h029, cf16i029, cf17j029, cf18k029, cf19l029, cf20m029) %>%
  View()
# Decision: no need to coalesce across years; the same value is recorded repeatedly across waves

# Are you married?
train %>% 
  select(cf08a030, cf09b030, cf10c030, cf11d030, cf12e030, cf13f030, cf14g030, cf15h030, cf16i030, cf17j030, cf18k030, cf19l030, cf20m030) %>%
  View()
# Decision: no need to coalesce across years; the same value is recorded repeatedly across waves

# In what year did you marry? 
train %>%
  select(cf08a031, cf09b031, cf10c031, cf11d031, cf12e031, cf13f031, cf14g031, cf15h031, cf16i031, cf17j031, cf18k031, cf19l031, cf20m031) %>% 
  View()
# Decision: no need to coalesce across years; the same value is recorded repeatedly across waves

# What is your partner's gender? 
train %>%
  select(cf20m024, cf08a032, cf09b032, cf10c032, cf11d032, cf12e032, cf13f032, cf14g032, cf15h032, cf16i032, cf17j032, cf18k032, cf19l032, cf20m032) %>%
  View()
train %>%
  group_by(cf20m024, is.na(cf20m032)) %>%
  count()
train %>%
  select(cf20m024, cf08a032, cf09b032, cf10c032, cf11d032, cf12e032, cf13f032, cf14g032, cf15h032, cf16i032, cf17j032, cf18k032, cf19l032, cf20m032) %>%
  filter(cf20m024 == 1) %>% # Filter to people who currently have partner
  View()
# Everyone who said they currently have a partner in 2020 has partner's gender reported in 2020
# Decision: no need to coalesce across years

#### Draft code for creating the variables ####
# I will insert a version of this code into submission.R
toy <- train %>% 
  mutate(partner_birth_year = coalesce(cf20m026, cf19l026, cf18k026, cf17j026, cf16i026, cf15h026, cf14g026, cf13f026, cf12e026, cf11d026, cf10c026, cf09b026, cf08a026))
toy %>%
  select(cf20m024, cf08a026, cf09b026, cf10c026, cf11d026, cf12e026, cf13f026, cf14g026, cf15h026, cf16i026, cf17j026, cf18k026, cf19l026, cf20m026, most_recent) %>%
  View()

toy <- train %>%
  mutate(year_relationship_began = coalesce(cf20m028, cf19l028, cf18k028, cf17j028, cf16i028, cf15h028, cf14g028, cf13f028, cf12e028, cf11d028, cf10c028, cf09b028, cf08a028))
toy %>%
  select(cf20m028, cf19l028, cf18k028, cf17j028, cf16i028, cf15h028, cf14g028, cf13f028, cf12e028, cf11d028, cf10c028, cf09b028, cf08a028, year_relationship_began) %>%
  View()