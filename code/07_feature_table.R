# This file takes a hand-prepared CSV file on variable descriptions and make it
# look nice in latex.

# Set up
# install.packages("groundhog")
start <- Sys.time()
library(groundhog)
groundhog.library(c("tidyverse", "kableExtra"), "2024-04-23")

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
    col.names = c("", "Variable Name", "Description", "Source"),
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