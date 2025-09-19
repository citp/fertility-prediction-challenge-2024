# Set up
# install.packages("groundhog")
library(groundhog)
groundhog.library(c("here", "tidyverse", "rmarkdown"), "2024-04-23")
setwd(here())

# One must indicate in command line whether we are generating results using the
# cv data and/or the holdout set.
args <- commandArgs()
preds_cv <- as.logical(args[7])
preds_holdout <- as.logical(args[9])

# Time shift
render('code/01_outcome_time_shift.Rmd', output_format = 'pdf_document')
source("code/02_feature_time_shift.R")

# Modelling
source("code/03_clean_df.R")
source("code/04_training.R")

# Keeping Track of Rows and Columns
source("code/05_observation_count.R")
source("code/06_missingness_count.R")
source("code/07_feature_table.R")

# Predictive Performance
source("code/08_all_preds.R")
source("code/09_metrics.R")
source("code/10_eval.R")