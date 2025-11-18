# This file takes a hand-prepared CSV file on variable descriptions and make it
# look nice in latex.

# Set up
start <- Sys.time()

# Load packages if they weren't already loaded via the run_all script
if (!isTRUE(getOption("run_all_executed"))) {
  library(groundhog)
  groundhog.library(c("here", "tidyverse", "kableExtra"), "2024-04-23")
}

here() %>%
  setwd()

# Make the table
dir.create("tables")
table_a1 <- "data/table_a1.csv" %>%
  read.csv() %>%
  mutate(
    Variable.Name = gsub("_", "\\\\_", Variable.Name),
    Description = gsub("_", "\\\\_", Description),
    Description = paste("\\raggedright", Description),
    Source = paste("\\raggedright", Source)
  ) %>%
  kable("latex",
    col.names = c("\\#", "Variable name", "Description", "Source"),
    align = "p{.5cm}p{5cm}p{3cm}p{2cm}",
    caption =
      "Features in the Winning Model",
    escape = FALSE,
    longtable = TRUE,
    booktabs = TRUE,
    linesep = ""
  ) %>%
  kable_styling(latex_options = c("repeat_header"))
"\\tablebodyfont\n" %>%
  paste0(table_a1) %>%
  write("tables/table_a1.tex")
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/07_feature_table.tex")