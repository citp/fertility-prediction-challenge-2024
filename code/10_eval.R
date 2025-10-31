# This file bootstraps preds.RDS to produce confidence intervals for evaluation
# metrics

# Set up
# install.packages("groundhog")
start <- Sys.time()
library(groundhog)
groundhog.library(
  c("tidyverse", "tidymodels", "ggridges", "ggthemes", "kableExtra"),
  "2024-04-23"
)
preds <- readRDS("data/intermediate_files/preds.RDS")

# This function calculates nine metrics associated with a particular model
# name
get_metrics_for_name <- function(name, bootstrapped_preds, get_metrics) {
  bootstrapped_preds %>%
    get_metrics(new_child, all_of(name)) %>%
    mutate(name = name)
}

# This function constructs a bootstrap sample and calculate metrics for all
# models, as specified by model names
bootstrap <- function(i_bootstrap, preds, names, get_metrics) {
  bootstrapped_preds <- preds
  if (i_bootstrap != 0) { # i=0 is a special sample where we just get the 
                          # original estimates with no bootstrap
    bootstrapped_preds <- bootstrapped_preds %>%
      slice_sample(prop = 1, replace = TRUE)
  }
  names %>%
    map(~ get_metrics_for_name(.x, bootstrapped_preds, get_metrics)) %>%
    list_rbind() %>%
    mutate(i_bootstrap = i_bootstrap)
}
set.seed(0)
results <- 0:2000 %>%
  map_dfr(~ bootstrap(
    .x, preds,
    c("final", "partner", "time_shift", "original",
      "glm_full_final", "glm_full_partner",
      "glm_full_time_shift", "glm_full_original",
      "glm_seven_final", "glm_seven_partner",
      "glm_seven_time_shift", "glm_seven_original",
      "no_tuning", "three_features"
    ),
    metric_set(rsq_holdout, rsq_trad, auc_roc, mse, log_loss,
      f1, accuracy_prob, precision_prob, recall_prob
    )
  ))

# This function rounds raw numbers
clean_num <- function(x) {
  sub("0\\.", ".", format(round(x, 3), nsmall = 3))
}

# Given bootstrap results, this function summarizes the estimated performance 
# for each model and adds a confidence interval. The model should be indicated
# through a variable called "name."
# When given bootstrap results on performance difference between pairs of 
# models, it can also calculate the estimated performance difference for each 
# given model pair and adds a confidence interval. The model pair should also
# be indicated via the "name" variable
get_summary_data <- function(results) {
  estimates <- results %>%
    select(-any_of(".estimator")) %>%
    group_by(name, .metric) %>%
    filter(i_bootstrap == 0) %>%
    ungroup() %>%
    select(-i_bootstrap)
  results %>%
    group_by(name, .metric) %>%
    filter(i_bootstrap != 0) %>%
    summarize(
      lo = quantile(.estimate, 0.025),
      hi = quantile(.estimate, 0.975)
    ) %>%
    ungroup() %>%
    full_join(estimates, by = c("name", ".metric")) %>%
    mutate(cleaned_estimate = clean_num(.estimate))
}
results_summary <- get_summary_data(results)

# This function make things look pretty",
theme_tommy <- function() {
  theme_foundation() +
  theme(
    panel.background = element_rect(color = NA),
    plot.background = element_rect(color = NA),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = NA),
    panel.grid.major = element_line(color="#f0f0f0"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour="black"),
    legend.key =element_rect(color = NA),
    strip.background=element_rect(color= NA)
  )
}

# This function plots the distributions of a bootstrapped metric (R2-holdout)
# with for multiple models
make_plot <- function(results, results_summary,
                      these_names, labels) {
  filtered_results <- results %>%
    filter(name %in% these_names, .metric == "rsq_holdout") %>%
    mutate(name = factor(name, levels = these_names)) %>%
    filter(i_bootstrap != 0)
  filtered_results_summary <- results_summary %>%
    filter(name %in% these_names, .metric == "rsq_holdout") %>%
    mutate(name = factor(name, levels = these_names))
  plot <- ggplot(mapping = aes(y = name)) +
    geom_errorbarh(
      aes(xmin = lo, xmax = hi),
      data = filtered_results_summary,
      height = .1
    ) +
    geom_density_ridges(aes(x = .estimate), data = filtered_results) +
    geom_label(
      aes(x = .estimate, label = cleaned_estimate),
      data = filtered_results_summary,
      nudge_y = .93
    ) +
    scale_x_continuous(labels = function(x) gsub("0\\.", ".", number(x))) +
    scale_y_discrete(
      labels = labels,
      expand = expansion(mult = c(0, .6), add = 0)
    ) +
    labs(x = x, y = "") +
    theme_tommy()
}
figure5 <- make_plot(
  results, results_summary,
  c("final", "time_shift", "partner", "original"),
  c("Partner +\nTime Shift", "Time Shift", "Partner", "Original")
)
dir.create("figures")
ggsave("figures/figure5.png", plot = figure5, width = 7, height = 5, dpi = 300)
figure7 <- make_plot(
  results, results_summary,
  c("glm_seven_final", "glm_seven_time_shift",
    "glm_seven_partner", "glm_seven_original"
  ),
  c("Partner +\nTime Shift", "Time Shift", "Partner", "Original")
)
ggsave("figures/figure7.png", plot = figure7, width = 7, height = 5, dpi = 300)
figure8 <- make_plot(
  results, results_summary,
  c("glm_full_final", "glm_full_time_shift",
    "glm_full_partner", "glm_full_original"
  ),
  c("Partner +\nTime Shift", "Time Shift", "Partner", "Original")
)
ggsave("figures/figure8.png", plot = figure8, width = 7, height = 5, dpi = 300)
figure9 <- make_plot(
  results, results_summary,
  c("three_features", "original", "no_tuning", "final"),
  c("Fertility\nIntentions", "No Partner +\nNo Time Shift",
    "No Tuning", "Final Model"
  )
)
ggsave("figures/figure9.png", plot = figure9, width = 7, height = 5, dpi = 300)

# This function takes bootstrap results and subtract performance estimates for
# one model from those for another model
get_contrast <- function(name1, name2, results) {
  results_name1 <- filter(results, name == name1) %>%
    select(-name) %>%
    rename(.estimate1 = .estimate)
  results_name2 <- filter(results, name == name2) %>%
    select(-name) %>%
    rename(.estimate2 = .estimate)
  full_join(results_name1, results_name2, 
            by = c("i_bootstrap", ".metric")) %>%
    mutate(name = paste0(name2, "__", name1),
           .estimate = .estimate2 - .estimate1) %>%
    select(-.estimate1, -.estimate2)
}
contrasts_results <- tibble(
  name1 = "original",
  name2 = c("partner", "time_shift", "final")
) %>%
  rbind(tibble(name1 = "glm_full_original",
               name2 = c("glm_full_partner", "glm_full_time_shift", "glm_full_final"))) %>%
  rbind(tibble(name1 = "glm_seven_original",
               name2 = c("glm_seven_partner", "glm_seven_time_shift", "glm_seven_final"))) %>%
  rbind(tibble(name1 = "final",
               name2 = c("no_tuning", "original", "three_features"))) %>%
  pmap(~ get_contrast(.x, .y, results)) %>%
  list_rbind()
contrasts_results_summary <- get_summary_data(contrasts_results)

# Plot Figure 6: difference between original models and other models using
# tuned xgboost model on the full feature set.
figure6_filtered_results <- contrasts_results %>%
  filter(grepl("__original", name), .metric == "rsq_holdout") %>%
  mutate(name = factor(name,
    levels = c("final__original", "time_shift__original", "partner__original")
  )) %>%
  filter(i_bootstrap != 0)
figure6_filtered_results_summary <- contrasts_results_summary %>%
  filter(grepl("__original", name), .metric == "rsq_holdout") %>%
  mutate(
    name = factor(name,
      levels = 
        c("final__original", "time_shift__original", "partner__original")
    )
  )
# Determine x-axis label based on whether we are using CV and/or holdout preds
if (preds_cv) {
  if (preds_holdout) {
    x <- bquote(italic(R)[CV-Holdout]^2)
    r2_table <- "$R^2_\\text{CV-Holdout}$"
  } else {
    x <- bquote(italic(R)[CV]^2)
    r2_table <- "$R^2_\\text{CV}$"
  }
} else {
  x <- bquote(italic(R)[Holdout]^2)
  r2_table <- "$R^2_\\text{Holdout}$"
}
figure6 <- ggplot(mapping = aes(y = name)) +
  geom_vline(xintercept = 0) +
  geom_errorbarh(
    aes(xmin = lo, xmax = hi),
    data = figure6_filtered_results_summary,
    height = .1
  ) +
  geom_density_ridges(aes(x = .estimate), data = figure6_filtered_results) +
  geom_label(
    aes(x = .estimate, label = cleaned_estimate),
    data = figure6_filtered_results_summary,
    nudge_y = 1
  ) +
  scale_x_continuous(labels = function(x) gsub("0\\.", ".", number(x))) +
  scale_y_discrete(
    labels = c("Partner +\nTime Shift", "Time Shift", "Partner"),
    expand = expansion(mult = c(0, .9), add = 0)
  ) +
  labs(x = x, y = "") +
  theme_tommy()
ggsave("figures/figure6.png", plot = figure6, width = 7, height = 5, dpi = 300)

# This function selects numbers of type ".estimate", "lo", or "hi" from
# filtered_results_summary
get_num_of_type <- function(filtered_results_summary, type) {
  filtered_results_summary %>%
    select(-.metric) %>%
    select(starts_with(type))
}

# This function cleans up the numbers that appear in each table. It also saves
# the raw numbers for the performance estimates in original, non-bootstrapped
# data. This is useful for bolding the best performance
get_table_data <- function(results_summary, these_names) {
  filtered_results_summary <- results_summary %>%
    filter(name %in% these_names) %>%
    mutate(
      name = factor(name, levels = these_names),
      .metric = factor(.metric,
        levels = c(
          "rsq_holdout", "rsq_trad", "auc_roc", "log_loss", "mse",
          "f1", "precision_prob", "recall_prob", "accuracy_prob"
        )
      )
    ) %>%
    arrange(.metric, name) %>%
    pivot_wider(
      names_from = name, 
      values_from = c(.estimate, cleaned_estimate, lo, hi)
    )
  estimate <- get_num_of_type(filtered_results_summary, ".estimate")
  estimate_for_best <- estimate
  estimate <- get_num_of_type(filtered_results_summary, "cleaned_estimate")
  lo <- filtered_results_summary %>%
    get_num_of_type("lo") %>%
    modify(~ map(.x, clean_num))
  hi <- filtered_results_summary %>%
    get_num_of_type("hi") %>%
    modify(~ map(.x, clean_num))
  list(
    estimate_for_best = estimate_for_best,
    estimate = estimate, lo = lo, hi = hi
  )
}

# Save model performances from Figure 5, as mentioned in Section 5.1
table_data_b1 <- results_summary %>%
  get_table_data(c("original", "partner", "time_shift", "final"))
dir.create("numbers/section5_1", recursive = TRUE)
table_data_b1[["estimate"]][["cleaned_estimate_original"]][[1]] %>%
  write("numbers/section5_1/01_estimate_original.tex")
table_data_b1[["lo"]][["lo_original"]][[1]] %>%
  write("numbers/section5_1/02_lo_original.tex")
table_data_b1[["hi"]][["hi_original"]][[1]] %>%
  write("numbers/section5_1/03_hi_original.tex")
table_data_b1[["estimate"]][["cleaned_estimate_partner"]][[1]] %>%
  write("numbers/section5_1/04_estimate_partner.tex")
table_data_b1[["lo"]][["lo_partner"]][[1]] %>%
  write("numbers/section5_1/05_lo_partner.tex")
table_data_b1[["hi"]][["hi_partner"]][[1]] %>%
  write("numbers/section5_1/06_hi_partner.tex")
table_data_b1[["estimate"]][["cleaned_estimate_time_shift"]][[1]] %>%
  write("numbers/section5_1/07_estimate_time_shift.tex")
table_data_b1[["lo"]][["lo_time_shift"]][[1]] %>%
  write("numbers/section5_1/08_lo_time_shift.tex")
table_data_b1[["hi"]][["hi_time_shift"]][[1]] %>%
  write("numbers/section5_1/09_hi_time_shift.tex")
table_data_b1[["estimate"]][["cleaned_estimate_final"]][[1]] %>%
  write("numbers/section5_1/16_estimate_final.tex")
table_data_b1[["lo"]][["lo_final"]][[1]] %>%
  write("numbers/section5_1/17_lo_final.tex")
table_data_b1[["hi"]][["hi_final"]][[1]] %>%
  write("numbers/section5_1/18_hi_final.tex")

# Save model performance improvement from Figure 6, as mentioned in Section 5.1
table_data_b2 <- contrasts_results_summary %>%
  get_table_data(
    c("partner__original", "time_shift__original", "final__original")
  )
table_data_b2[["estimate"]][["cleaned_estimate_partner__original"]][[1]] %>%
  write("numbers/section5_1/10_estimate_partner__original.tex")
table_data_b2[["lo"]][["lo_partner__original"]][[1]] %>%
  write("numbers/section5_1/11_lo_partner__original.tex")
table_data_b2[["hi"]][["hi_partner__original"]][[1]] %>%
  write("numbers/section5_1/12_hi_partner__original.tex")
table_data_b2[["estimate"]][["cleaned_estimate_time_shift__original"]][[1]] %>%
  write("numbers/section5_1/13_estimate_time_shift__original.tex")
table_data_b2[["lo"]][["lo_time_shift__original"]][[1]] %>%
  write("numbers/section5_1/14_lo_time_shift__original.tex")
table_data_b2[["hi"]][["hi_time_shift__original"]][[1]] %>%
  write("numbers/section5_1/15_hi_time_shift__original.tex")
table_data_b2[["estimate"]][["cleaned_estimate_final__original"]][[1]] %>%
  write("numbers/section5_1/19_estimate_final__original.tex")
table_data_b2[["lo"]][["lo_final__original"]][[1]] %>%
  write("numbers/section5_1/20_lo_final__original.tex")
table_data_b2[["hi"]][["hi_final__original"]][[1]] %>%
  write("numbers/section5_1/21_hi_final__original.tex")

# Save model performance improvement from Figure 9, as mentioned in Section 5.3
dir.create("numbers/section5_3")
table_data_b8 <- contrasts_results_summary %>%
  get_table_data(
    c("no_tuning__final", "original__final", "three_features__final")
  )
table_data_b8[["estimate"]][[
  "cleaned_estimate_three_features__final"]][[1]] %>%
  write("numbers/section5_3/01_estimate_three_features__final.tex")
table_data_b8[["lo"]][["lo_three_features__final"]][[1]] %>%
  write("numbers/section5_3/02_lo_three_features__final.tex")
table_data_b8[["hi"]][["hi_three_features__final"]][[1]] %>%
  write("numbers/section5_3/03_hi_three_features__final.tex")
table_data_b8[["estimate"]][["cleaned_estimate_original__final"]][[1]] %>%
  write("numbers/section5_3/04_estimate_original__final.tex")
table_data_b8[["lo"]][["lo_original__final"]][[1]] %>%
  write("numbers/section5_3/05_lo_original__final.tex")
table_data_b8[["hi"]][["hi_original_final"]][[1]] %>%
  write("numbers/section5_3/06_hi_original__final.tex")
table_data_b8[["estimate"]][["cleaned_estimate_no_tuning__final"]][[1]] %>%
  write("numbers/section5_3/07_estimate_no_tuning__final.tex")
table_data_b8[["lo"]][["lo_no_tuning__final"]][[1]] %>%
  write("numbers/section5_3/08_lo_no_tuning__final.tex")
table_data_b8[["hi"]][["hi_no_tuning__final"]][[1]] %>%
  write("numbers/section5_3/09_hi_no_tuning__final.tex")
table_data_b8[["estimate"]][["cleaned_estimate_original__final"]][[6]] %>%
  write("numbers/section5_3/10_estimate_original__final_f1.tex")
table_data_b8[["lo"]][["lo_original__final"]][[6]] %>%
  write("numbers/section5_3/11_lo_original__final_f1.tex")
table_data_b8[["hi"]][["hi_original_final"]][[6]] %>%
  write("numbers/section5_3/12_hi_original__final_f1.tex")

# This function creates the latex tables in the appendix
make_table <- function(table_data, labels, caption) {
  ci <- table_data$lo %>%
    map2(table_data$hi, ~ str_c("[", .x, ", ", .y, "]")) %>%
    as_tibble()
  n_cols <- ncol(table_data$estimate)
  col_order <- as.vector(rbind(1:n_cols, (n_cols + 1):(2 * n_cols)))
  output <- table_data$estimate %>%
    bind_cols(ci) %>%
    select(all_of(col_order)) %>%
    data.frame()
  
  # Bold best model of each row
  for (row in 1:9) {
    if (!row %in% c(4, 5)) {
      best <- which.max(table_data$estimate_for_best[row, ])
    } else {
      best <- which.min(table_data$estimate_for_best[row, ])
    }
    output[row, 2 * best - 1] <-
      paste0("\\textbf{", output[row, 2 * best - 1], "}")
    output[row, 2 * best] <- paste0("\\textbf{", output[row, 2 * best], "}")
  }
  
  # Tidy up the table
  rownames(output) <- c(
    r2_table, "$R^2_\\text{Traditional}$", "AUC", "Log Loss",
    "MSE", "F1", "Precision", "Recall", "Accuracy"
  )
  ci_labels <- paste(labels, "\\\\ 95\\% CI")
  col.names <- labels %>%
    rbind(ci_labels) %>%
    c()
  col.names <- paste0("\\parbox{1.5cm}{\\centering ", col.names, "}")
  output <- output %>%
    kable("latex",
          col.names = col.names,
          align = "c",
          caption = caption,
          escape = FALSE,
          booktabs = TRUE,
          linesep = ""
    ) %>%
    str_replace_all("table", "sidewaystable")
  paste0(
    substr(output, 1, nchar(output) - 18),
    paste0("footnotetext{\\textit{Note}. ",
           "Best performance of each row in bold. ",
           "F1, precision, recall, and accuracy used $\\geq .5$ as threshold.}",
           "\n\\end{sidewaystable}"
    )
  )
}
dir.create("tables")
table_data_b1 %>%
  make_table(
    c("Original", "Partner", "Time Shift", "Partner + \\\\ Time Shift"),
    "Performance of Tuned XGBoost Models"
  ) %>%
  write("tables/table_b1.tex")
table_data_b3 <- results_summary %>%
  get_table_data(c("glm_seven_original", "glm_seven_partner",
    "glm_seven_time_shift", "glm_seven_final"
  )
)
table_data_b3 %>%
  make_table(
    c("Original", "Partner", "Time Shift", "Partner + \\\\ Time Shift"),
    "Performance of Logistic Regression Models on Seven Baseline Features"
  ) %>%
  write("tables/table_b3.tex")
table_data_b5 <- results_summary %>%
  get_table_data(c("glm_full_original", "glm_full_partner",
    "glm_full_time_shift", "glm_full_final"
  )
)
table_data_b5 %>%
  make_table(
    c("Original", "Partner", "Time Shift", "Partner + \\\\ Time Shift"),
    paste("Performance of Logistic Regression Models",
      "on All Features in Winning Model"
    )
  ) %>%
  write("tables/table_b5.tex")
table_data_b7 <- results_summary %>%
  get_table_data(c("final", "no_tuning", "original", "three_features"))
table_data_b7 %>%
  make_table(c("Final Model", "No Tuning", 
      "No Partner + \\\\ No Time Shift", "Fertility \\\\ Intentions"
    ),
    "Performance of Final and Less Engineered XGBoost Models"
  ) %>%
  write("tables/table_b7.tex")

# Create tables for improvement in performance
table_data_b2 %>%
  make_table(
    c("Partner", "Time Shift", "Partner + \\\\ Time Shift"),
    paste(
      "Performance of Engineered Data \\textbf{Relative to Original Data},",
      "Tuned XGBoost Models"
    )
  ) %>%
  write("tables/table_b2.tex")
table_data_b4 <- contrasts_results_summary %>%
  get_table_data(c("glm_seven_partner__glm_seven_original",
    "glm_seven_time_shift__glm_seven_original",
    "glm_seven_final__glm_seven_original"
  )
)
table_data_b4 %>%
  make_table(
    c("Partner", "Time Shift", "Partner + \\\\ Time Shift"),
    paste("Performance of Engineered Data Relative to Original Data,",
      "Logistic Regression Models on Seven Baseline Features"
    )
  ) %>%
  write("tables/table_b4.tex")
table_data_b6 <- contrasts_results_summary %>%
  get_table_data(c("glm_full_partner__glm_full_original",
    "glm_full_time_shift__glm_full_original",
    "glm_full_final__glm_full_original"
  )
)
table_data_b6 %>%
  make_table(
    c("Partner", "Time Shift", "Partner + \\\\ Time Shift"),
    paste(
      "Performance of Engineered Data \\textbf{Relative to Original Data},",
      "Logistic Regression Models on All Features in Winning Model"
    )
  ) %>%
  write("tables/table_b6.tex")
table_data_b8 %>%
  make_table(
    c("No Tuning", 
      "No Partner + \\\\ No Time Shift",
      "Fertility \\\\ Intentions"
    ),
    paste("Performance of Less Engineered XGBoost Models",
      "\\textbf{Relative to Original Data}"
    )
  ) %>%
  write("tables/table_b8.tex")
Sys.time() %>%
  difftime(start, units = "mins") %>%
  write("numbers/timing/10_eval.tex")