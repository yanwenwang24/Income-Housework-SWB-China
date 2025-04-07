## ------------------------------------------------------------------------
##
## Script name: 05_model_hybrid.r
## Purpose: Hybrid models (Mundlak specification)
## Author: Yanwen Wang
## Date Created: 2025-04-06
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## Hybrid models using Mundlak specification to separate within- and between-
## couple effects of household roles on life satisfaction.
## The script:
## 1. Creates dummy variables for combined roles
## 2. Calculates within-couple means and deviations
## 3. Fits hybrid models for women and men
## 4. Generates predictions for different role categories
## 5. Saves models and predictions for visualization
##
## ------------------------------------------------------------------------

# 1 Prepare data ----------------------------------------------------------

# 1.1 Create dummy variables for combined_role ----------------------------

# Create dummy variables
role_dummies_mat <- model.matrix(
  ~combined_role,
  data = sample_df
)[, -1, drop = FALSE]

# Clean up names
dummy_col_names <- make.names(colnames(role_dummies_mat))
colnames(role_dummies_mat) <- dummy_col_names

# Add to sample_df
sample_df <- cbind(sample_df, role_dummies_mat)

# 1.2 Calculate mean and deviation ----------------------------------------

# Within-couple means (time spent in each role category)
mean_vars <- sample_df %>%
  group_by(pid) %>%
  summarise(
    across(
      .cols = all_of(dummy_col_names),
      .fns = \(x) mean(x, na.rm = TRUE),
      .names = "{.col}_mean"
    )
  ) %>%
  ungroup()

# Join with sample_df
sample_df <- sample_df %>%
  left_join(mean_vars, by = "pid")

# Deviation from within-couple means (change over time)
for (col_name in dummy_col_names) {
  mean_col_name <- paste0(col_name, "_mean")
  dev_col_name <- paste0("dev_", col_name)
  sample_df <- sample_df %>%
    mutate(!!dev_col_name := .data[[col_name]] - .data[[mean_col_name]])
}

# 2 Hybrid models ---------------------------------------------------------

# 2.1 Define formula ------------------------------------------------------

dev_predictors <- names(sample_df)[
  startsWith(names(sample_df), "dev_combined_role")
]

mean_predictors <- names(sample_df)[
  endsWith(names(sample_df), "_mean") &
    startsWith(names(sample_df), "combined_role")
]

role_predictors <- paste(c(dev_predictors, mean_predictors), collapse = " + ")

f_women <- as.formula(paste(
  "lsat ~",
  role_predictors,
  " + age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

f_men <- as.formula(paste(
  "lsat_sp ~",
  role_predictors,
  " + age_std + age_sp_std + age_std_sq + age_sp_std_sq + educ + educ_sp +
  hukou + hukou_sp + migrant + migrant_sp + chronic + chronic_sp +
  n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

# 2.2 Fit models ----------------------------------------------------------

mod_women <- lmer(f_women, data = sample_df)
mod_men <- lmer(f_men, data = sample_df)

summ(mod_women, digits = 3)
summ(mod_men, digits = 3)

performance::check_collinearity(mod_women)
performance::check_collinearity(mod_men)

# 2.3 Save models ----------------------------------------------------------

# Save models
saveRDS(
  list(
    mod_women = mod_women,
    mod_men = mod_men
  ),
  "models/mods_hybrid.rds"
)

# Read models
mod_women <- readRDS("models/mods_hybrid.rds")$mod_women
mod_men <- readRDS("models/mods_hybrid.rds")$mod_men

# 3 Prediction ------------------------------------------------------------

# 3.1 Predictions for women -----------------------------------------------

# Extract predictors of interest
mean_predictors <- names(coef(mod_women)$pid) %>%
  str_subset("_mean$") %>%
  str_subset("^combined_role")

# Calculate predictions
pred_women_list <- map(
  mean_predictors,
  ~ {
    term_formula <- paste0(.x, " [0, 1]")
    ggpredict(mod_women, terms = term_formula) %>%
      as.data.frame() %>%
      mutate(role = .x)
  }
)

all_preds_women <- bind_rows(pred_women_list)

# Process predictions: reference level
baseline_women <- all_preds_women %>%
  filter(x == 0) %>%
  arrange(predicted) %>%
  slice(1) %>%
  mutate(role = "Egal/Egal") %>%
  select(role, predicted, std.error, conf.low, conf.high)

# Process predictions: other levels
level_women <- all_preds_women %>%
  filter(x == 1) %>% # Only keep the prediction for each level
  mutate(
    role = role %>%
      str_remove("_mean$") %>%
      str_remove("^combined_role") %>%
      str_replace_all("\\.", "/")
  ) %>%
  select(role, predicted, std.error, conf.low, conf.high)

# Combine reference and other levels
pred_women <- bind_rows(baseline_women, level_women) %>%
  mutate(gender = "women")

# 3.2 Predictions for men -------------------------------------------------

# Extract predictors of interest
mean_predictors <- names(coef(mod_men)$pid) %>%
  str_subset("_mean$") %>%
  str_subset("^combined_role")

# Calculate predictions
pred_men_list <- map(
  mean_predictors,
  ~ {
    term_formula <- paste0(.x, " [0, 1]")
    ggpredict(mod_men, terms = term_formula) %>%
      as.data.frame() %>%
      mutate(role = .x)
  }
)

all_preds_men <- bind_rows(pred_men_list)

# Process predictions: reference level
baseline_men <- all_preds_men %>%
  filter(x == 0) %>%
  arrange(predicted) %>%
  slice(1) %>%
  mutate(role = "Egal/Egal") %>%
  select(role, predicted, std.error, conf.low, conf.high)

# Process predictions: other levels
level_men <- all_preds_men %>%
  filter(x == 1) %>% # Only keep the prediction for each level
  mutate(
    role = role %>%
      str_remove("_mean$") %>%
      str_remove("^combined_role") %>%
      str_replace_all("\\.", "/")
  ) %>%
  select(role, predicted, std.error, conf.low, conf.high)

# Combine reference and other levels
pred_men <- bind_rows(baseline_men, level_men) %>%
  mutate(gender = "men")

# 3.3 Save predictions -----------------------------------------------------

pred_df <- bind_rows(pred_women, pred_men)

saveRDS(
  pred_df,
  "outputs/pred_hybrid.rds"
)
