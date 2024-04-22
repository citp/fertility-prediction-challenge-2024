#!/usr/bin/env Rscript

install.packages(c("dplyr", "data.table", "tidyr", "tidymodels", "xgboost"),
  repos = "https://cran.r-project.org",
  dependencies = TRUE
)
