# This file produces summary statistics on missing data

# Set up
# install.packages("groundhog")
start <- Sys.time()
library(groundhog)
groundhog.library(c("here", "tidyverse", "ggthemes", "ggflowchart"),
  "2024-04-23"
)
here() %>%
  setwd()

# Read data
train_data <- read.csv("data/PreFer_train_data.csv")
supplementary_data <- read.csv("data/PreFer_train_supplementary_data.csv")
train_2021to2023 <- "data/intermediate_files/" %>%
  paste0("cleaned_dfs/cleaned_train_2021to2023_full_features_final.RDS") %>%
  readRDS()
train_2018to2020 <- "data/intermediate_files/" %>%
  paste0("cleaned_dfs/cleaned_train_2018to2020_full_features_final.RDS") %>%
  readRDS()
outcome_2021to2023 <- read.csv("data/PreFer_train_outcome.csv")
outcome_2018to2020 <- "data/intermediate_files/outcome_2018to2020.csv" %>%
  read.csv()

# Count number of features (not counting ID) in main training file
dir.create("numbers/section4_3", recursive = TRUE)
(ncol(train_data) - 1) %>%
  format(big.mark = ",") %>%
  write("numbers/section4_3/01_n_features.tex")

# Count number of people in time shifted data without a core survey up to 2017
all_data <- rbind(train_data, supplementary_data)
n_new_after_2017 <- train_2018to2020 %>%
  select(nomem_encr) %>%
  left_join(all_data) %>%
  select(contains("_m")) %>% # These variables are NA if no participation
  select(-contains(c("18", "19", "20"))) %>%
  rowMeans(na.rm = TRUE) %>%
  is.na() %>% # if means(na.rm = T) produces no mean, the person has not 
  sum()       # participated
dir.create("numbers/section2_3")
write(n_new_after_2017, "numbers/section2_3/03_n_new_after_2017.tex")

# Create four datasets that differ in terms of whether there is time shift and
# whether the new child outcome is positive or negative
data_2021to2023 <- train_2021to2023 %>%
  left_join(outcome_2021to2023, by = "nomem_encr")
data_2018to2020 <- train_2018to2020 %>%
  left_join(outcome_2018to2020, by = "nomem_encr")
positive_2021to2023 <- filter(data_2021to2023, new_child == 1) %>%
  select(-nomem_encr, -nohouse_encr, -new_child)
negative_2021to2023 <- filter(data_2021to2023, new_child == 0) %>%
  select(-nomem_encr, -nohouse_encr, -new_child)
positive_2018to2020 <- filter(data_2018to2020, new_child == 1) %>%
  select(-nomem_encr, -nohouse_encr, -new_child)
negative_2018to2020 <- filter(data_2018to2020, new_child == 0) %>%
  select(-nomem_encr, -nohouse_encr, -new_child)
dfs <- list(
  positive_2021to2023 = positive_2021to2023,
  negative_2021to2023 = negative_2021to2023,
  positive_2018to2020 = positive_2018to2020,
  negative_2018to2020 = negative_2018to2020
)

# This fuction gets the missing rate for each variable in the dataset
get_missing_rate <- function(df, dfs) {
  output <- dfs[[df]] %>%
    modify(is.na) %>%
    colSums()
  name_text <- strsplit(names(dfs)[[df]], "_")
  tibble(
    missing = output / nrow(dfs[[df]]),
    time_shift = name_text[[1]][[2]], new_child = name_text[[1]][[1]]
  )
}

# This function make things look pretty",
theme_tommy <- function() {
  theme_foundation() +
    theme(
      text = element_text(family = "Helvetica"),
      panel.background = element_rect(color = NA),
      plot.background = element_rect(color = NA),
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = NA),
      panel.grid.major = element_line(color="#f0f0f0"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour="black"),
      legend.key =element_rect(color = NA),
      strip.background = element_rect(color= NA)
    )
}

# Make histograms of missingness rates
# This function saves a ggplot image as both a png file and a tiff file
double_save <- function(plot, fig_num) {
  "Figures/Fig" %>%
    paste0(filename) %>%
    paste0(".png") %>%
    ggsave(plot = plot, width = 7, height = 5, dpi = 1200)
  "Figures/Fig" %>%
    paste0(filename) %>%
    paste0(".tiff") %>%
    ggsave(plot = plot, width = 7, height = 5, dpi = 1200)
}

# Make histogram
figure_a1 <- 1:4 %>%
  map(~ get_missing_rate(.x, dfs)) %>%
  list_rbind() %>%
  mutate(
    new_child = factor(
      if_else(new_child == "positive", "New Child", "No New Child"),
      levels = c("No New Child", "New Child")
    ),
    time_shift = factor(if_else(time_shift == "2018to2020",
        "2018-2020\n(Time-shifted Data)", "2021-2023\n(Original Time Period)"
      ),
      levels =
        c("2021-2023\n(Original Time Period)", 
          "2018-2020\n(Time-shifted Data)"
        )
    )
  ) %>%
  group_by(time_shift, new_child) %>%
  mutate(mean_missing = mean(missing)) %>%
  ggplot(aes(x = missing)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean_missing)) +
  facet_grid(rows = vars(time_shift), cols = vars(new_child)) +
  labs(
    x = "Missingness Rate", y = "Count",
    caption = "Vertical line indicates mean missingness"
  ) +
  theme_tommy() +
  theme(panel.border = element_rect(color = "black")) %>%
  double_save("_a1")

# Get missingness rates when one combines positive and negative outcomes
data_2021to2023 <- data_2021to2023 %>%
  select(-nomem_encr, -nohouse_encr, -new_child)
data_2018to2020 <- data_2018to2020 %>%
  select(-nomem_encr, -nohouse_encr, -new_child)
dfs_pos_neg <- list(
  data_2021to2023 = data_2021to2023,
  data_2018to2020 = data_2018to2020
)
dfs_pos_neg <- 1:2 %>%
  map(~ get_missing_rate(.x, dfs_pos_neg)) 
(mean(dfs_pos_neg[[2]]$missing) * 100) %>%
  round() %>%
  write("numbers/section2_3/01_n_missing_2018to2020.tex")
(mean(dfs_pos_neg[[1]]$missing) * 100) %>%
  round() %>%
  write("numbers/section2_3/02_n_missing_2021to2023.tex")


# This function counts the number of rows in the data and adds commas in large 
get_n <- function(df) {
  df %>%
    select(nomem_encr) %>%
    unlist() %>%
    length() %>%
    format(big.mark = ",")
}
# All LISS Participants Ever
n_all_data <- get_n(all_data)
# Born 1972-1999
all_data_right_age_2018to2020 <- all_data %>%
  filter(birthyear_bg > 1971, birthyear_bg < 2000)
n_all_data_right_age_2018to2020 <- get_n(all_data_right_age_2018to2020)
# Participated at Least Once 2018-2020
participating_right_age_2018to2020 <- all_data_right_age_2018to2020 %>%
  filter(!(is.na(cf18k_m) & is.na(cf19l_m) & is.na(cf20m_m)))
n_participating_right_age_2018to2020 <- participating_right_age_2018to2020 %>%
  get_n()
# Outcome Available
n_2018to2020 <- get_n(train_2018to2020)

# Make a flowchart
figure_a2_edges <- tibble(from = c("a", "b", "c"), to = c("b", "c", "d"))
figure_a2_nodes <- tibble(
  name = c("a", "b", "c", "d"),
  label = c(paste("All Participants Ever\n", n_all_data),
    paste("Born 1972-1999\n", n_all_data_right_age_2018to2020),
    paste("Participated at Least Once 2018-2020\n",
      n_participating_right_age_2018to2020
    ),
    paste("Outcome Available\n", n_2018to2020)
  )
)
figure_a2 <- figure_a2_edges %>%
  ggflowchart(figure_a2_nodes) +
  theme(text = element_text(family = "Helvetica")) %>%
  double_save("_a2")
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/06_missingness_count.tex")