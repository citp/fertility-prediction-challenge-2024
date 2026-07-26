# This file counts the number of observations for Figure 2 and Table 1. 

# Set up
start <- Sys.time()

# Load packages if they weren't already loaded via the run_all script
if (!isTRUE(getOption("run_all_executed"))) {
  library(groundhog)
  groundhog.library(c("here", "tidyverse", "kableExtra"), "2024-04-23")
}

here() %>%
  setwd()

# Read data
# This function reads an outcome file and gets rid of any missing rows
read_outcome <- function(file) {
  file %>% 
    read.csv() %>% 
    filter(!is.na(new_child))
}
outcome_2021to2023 <- read_outcome("data/PreFer_train_outcome.csv")
outcome_2018to2020 <- "data/intermediate_files/outcome_2018to2020.csv" %>%
  read_outcome()
if(preds_holdout) {
  outcome_holdout <- read_outcome("data/PreFer_holdout_outcome.csv")
}
features_2018to2020 <- 
  "data/intermediate_files/train_data_for_2018to2020.csv" %>%
  read.csv()

# Count the number of observations for Figure 2, distinguishing between
# observations from the two periods as well as three types of time-shfited
# data
n_train_2021to2023 <- nrow(outcome_2021to2023)
n_train_2018to2020 <- nrow(outcome_2018to2020)
if(preds_holdout) {
  n_train_holdout <- nrow(outcome_holdout)
}
n_train_appeared_in_both_periods <- outcome_2021to2023 %>%
  filter(nomem_encr %in% outcome_2018to2020$nomem_encr) %>%
  nrow()
n_train_too_old_2021to2023 <- outcome_2018to2020 %>%
  filter(!nomem_encr %in% outcome_2021to2023$nomem_encr) %>%
  left_join(features_2018to2020, by = "nomem_encr") %>%
  filter(birthyear_bg < 1978) %>%
  nrow()
n_train_attrited <- n_train_2018to2020 - 
  n_train_appeared_in_both_periods - n_train_too_old_2021to2023
dir.create("figures")
tibble(
  n_train_2021to2023 = n_train_2021to2023,
  n_train_appeared_in_both_periods = n_train_appeared_in_both_periods,
  n_train_too_old_2021to2023 = n_train_too_old_2021to2023,
  n_train_attrited = n_train_attrited
) %>%
  write.csv("figures/Fig2_data.csv")

# Create Table 1, a contingency table of individuals with and without new
# children from either time period
n_train_2021to2023_0 <- outcome_2021to2023 %>% 
  filter(new_child == 0) %>% 
  nrow()
n_train_2021to2023_1 <- outcome_2021to2023 %>% 
  filter(new_child == 1) %>% 
  nrow()
n_train_2018to2020_0 <- outcome_2018to2020 %>% 
  filter(new_child == 0) %>% 
  nrow()
n_train_2018to2020_1 <- outcome_2018to2020 %>% 
  filter(new_child == 1) %>% 
  nrow()
n_train_2018to2023 <- n_train_2021to2023 + n_train_2018to2020
n_train_2018to2023_0 <- n_train_2021to2023_0 + n_train_2018to2020_0
n_train_2018to2023_1 <- n_train_2021to2023_1 + n_train_2018to2020_1
table1 <- tibble(
  source = c("Original data (2021--2023)", "Time-shifted data (2018--2020)",
             "\\midrule\n\\textbf{Total \\textit{\\textbf{n}}}"
  ),
  outcome0 = c(n_train_2021to2023_0, n_train_2018to2020_0,
               paste0("\\textbf{", n_train_2018to2023_0, "}")
  ),
  outcome1 = c(n_train_2021to2023_1, n_train_2018to2020_1,
               paste0("\\textbf{", n_train_2018to2023_1, "}")
  ),
  total = c(
    paste0("\\textbf{", n_train_2021to2023, "}"),
    paste0("\\textbf{", n_train_2018to2020, "}"),
    paste0("\\textbf{", n_train_2018to2023, "}")
  )
)
dir.create("tables")
table1 %>%
  kable("latex",
        col.names = c(
          "\\textbf{Data source}",
          "\\textbf{\\shortstack[c]{Outcome = 0\\\\(no new child)}}",
          "\\textbf{\\shortstack[c]{Outcome = 1\\\\(new child)}}",
          "\\textit{\\textbf{n}}"
        ),
        align = "lcccc",
        caption = paste0(
          "Distribution of Outcomes in Original and Time-Shifted Data}",
          "\\label{table1:distribution_of_outcomes"
        ),
        escape = FALSE,
        booktabs = TRUE
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  write("tables/table1.tex")

# The two numbers mentioned in Section 2.2 illustrating how sample size
# increased
dir.create("numbers/section2_2", recursive = TRUE)
n_train_2021to2023 %>%
  format(big.mark = ",") %>%
  write("numbers/section2_2/01_n_train_2021to2023.tex")
n_train_2018to2023 %>%
  format(big.mark = ",") %>%
  write("numbers/section2_2/02_n_train_2018to2023.tex")

# The original training set size was mentioned in Section 4.3
dir.create("numbers/section4_3")
n_train_2021to2023 %>%
  format(big.mark = ",") %>%
  write("numbers/section4_3/02_n_train_2021to2023.tex")

# The two numbers mentioned in Section 4.7.1 illustrating the size of the
# training and holdout set
dir.create("numbers/section4_7_1")
n_train_2021to2023 %>%
  format(big.mark = ",") %>%
  write("numbers/section4_7_1/01_n_train_2021to2023.tex")
if(preds_holdout) {
  n_train_holdout %>%
    format(big.mark = ",") %>%
    write("numbers/section4_7_1/02_n_train_holdout.tex")
}

# The two numbers mentioned in Section 2.3 on percentage with new children in
# both time periods
fertility_2018to2020 <- n_train_2018to2020_1 / n_train_2018to2020
fertility_2021to2023 <- n_train_2021to2023_1 / n_train_2021to2023
dir.create("numbers/section2_3")

# This function implements rounding half up rule
round_half_up <- function(x, n = 0) {
  posneg = sign(x)
  output <- abs(x) * 10 ^ n
  output = output + 0.5 + sqrt(.Machine$double.eps)
  output = trunc(output)
  output = output / 10 ^ n
  output * posneg
}
(fertility_2018to2020 * 100) %>%
  round_half_up() %>%
  write("numbers/section2_3/04_fertility_2018to2020.tex")
(fertility_2021to2023 * 100) %>%
  round_half_up() %>%
  write("numbers/section2_3/05_fertility_2021to2023.tex")
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/05_observation_count.tex")