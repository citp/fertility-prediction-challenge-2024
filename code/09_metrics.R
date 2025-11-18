# This file contains several performance metrics function in tidymodels
# style

# Set up
start <- Sys.time()

# Load packages if they weren't already loaded via the run_all script
if (!isTRUE(getOption("run_all_executed"))) {
  library(groundhog)
  groundhog.library(c("tidyverse", "tidymodels"), "2024-04-23")
}

# R-squared holdout. Note that the dataframe must have a column called train_means
rsq_holdout_impl <- function(truth, estimate, train_means) {
  1 - sum((estimate - truth) ^ 2) / sum((train_means - truth) ^ 2)
}

rsq_holdout_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, train_means, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  rsq_holdout_impl(truth, estimate, train_means)
}

rsq_holdout <- function(data, ...) {
  UseMethod("rsq_holdout")
}

rsq_holdout <- new_numeric_metric(rsq_holdout, direction = "maximize")

rsq_holdout.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "rsq_holdout",
    fn = rsq_holdout_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    fn_options = list(train_means = data$train_means)
  )
}

# MSE
mse_impl <- function(truth, estimate) {
  brier_class_vec(factor(truth, levels = c(1, 0)), estimate)
}

mse_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  mse_impl(truth, estimate)
}

mse <- function(data, ...) {
  UseMethod("mse")
}

mse <- new_numeric_metric(mse, direction = "minimize")

mse.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "mse",
    fn = mse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

# Log Loss
log_loss_impl <- function(truth, estimate) {
  mn_log_loss_vec(factor(truth, levels = c(1, 0)), estimate)
}

log_loss_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  log_loss_impl(truth, estimate)
}

log_loss <- function(data, ...) {
  UseMethod("log_loss")
}

log_loss <- new_numeric_metric(log_loss, direction = "minimize")

log_loss.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "log_loss",
    fn = log_loss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

# AUC
auc_roc_impl <- function(truth, estimate) {
  roc_auc_vec(factor(truth, levels = c(1, 0)), estimate)
}

auc_roc_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  auc_roc_impl(truth, estimate)
}

auc_roc <- function(data, ...) {
  UseMethod("auc_roc")
}

auc_roc <- new_numeric_metric(auc_roc, direction = "minimize")

auc_roc.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "auc_roc",
    fn = auc_roc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

# F1
f1_impl <- function(truth, estimate) {
  f_meas_vec(
    factor(truth, levels = c(1, 0)),
    factor(if_else(estimate >= .5, 1, 0), levels = c(1, 0))
  )
}

f1_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  f1_impl(truth, estimate)
}

f1 <- function(data, ...) {
  UseMethod("f1")
}

f1 <- new_numeric_metric(f1, direction = "minimize")

f1.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "f1",
    fn = f1_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

# Accuracy
accuracy_prob_impl <- function(truth, estimate) {
  accuracy_vec(
    factor(truth, levels = c(1, 0)),
    factor(if_else(estimate >= .5, 1, 0), levels = c(1, 0))
  )
}

accuracy_prob_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  accuracy_prob_impl(truth, estimate)
}

accuracy_prob <- function(data, ...) {
  UseMethod("accuracy_prob")
}

accuracy_prob <- new_numeric_metric(accuracy_prob, direction = "minimize")

accuracy_prob.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "accuracy_prob",
    fn = accuracy_prob_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

# Precision
precision_prob_impl <- function(truth, estimate) {
  precision_vec(
    factor(truth, levels = c(1, 0)),
    factor(if_else(estimate >= .5, 1, 0), levels = c(1, 0))
  )
}

precision_prob_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  precision_prob_impl(truth, estimate)
}

precision_prob <- function(data, ...) {
  UseMethod("precision_prob")
}

precision_prob <- new_numeric_metric(precision_prob, direction = "minimize")

precision_prob.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "precision_prob",
    fn = precision_prob_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

# recall
recall_prob_impl <- function(truth, estimate) {
  recall_vec(
    factor(truth, levels = c(1, 0)),
    factor(if_else(estimate >= .5, 1, 0), levels = c(1, 0))
  )
}

recall_prob_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  recall_prob_impl(truth, estimate)
}

recall_prob <- function(data, ...) {
  UseMethod("recall_prob")
}

recall_prob <- new_numeric_metric(recall_prob, direction = "minimize")

recall_prob.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  numeric_metric_summarizer(
    name = "recall_prob",
    fn = recall_prob_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/09_metrics.tex")