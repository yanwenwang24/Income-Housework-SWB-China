## ------------------------------------------------------------------------
##
## Script name: 01_dual_earner.r
## Purpose: Dual-earner analysis
## Author: Yanwen Wang
## Date Created: 2025-04-06
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Focus on dual-earner couples
##
## ------------------------------------------------------------------------

# 1 Prepare data ----------------------------------------------------------

# 1.1 Create sample --------------------------------------------------------

# Load data
sample_df <- readRDS("data_clean/sample_df.rds")

dual_earner_df <- sample_df %>%
  filter(income > 0 & income_sp > 0)

# 1.2 Create dummy variables for combined_role ----------------------------

# Create dummy variables
role_dummies_mat <- model.matrix(
  ~combined_role,
  data = dual_earner_df
)[, -1, drop = FALSE]

# Clean up names
dummy_col_names <- make.names(colnames(role_dummies_mat))
colnames(role_dummies_mat) <- dummy_col_names

# Add to dual_earner_df
dual_earner_df <- cbind(dual_earner_df, role_dummies_mat)

# 1.3 Calculate mean and deviation ----------------------------------------

# Within-couple means (time spent in each role category)
mean_vars <- dual_earner_df %>%
  group_by(pid) %>%
  summarise(
    across(
      .cols = all_of(dummy_col_names),
      .fns = \(x) mean(x, na.rm = TRUE),
      .names = "{.col}_mean"
    )
  ) %>%
  ungroup()

# Join with dual_earner_df
dual_earner_df <- dual_earner_df %>%
  left_join(mean_vars, by = "pid")

# Deviation from within-couple means (change over time)
for (col_name in dummy_col_names) {
  mean_col_name <- paste0(col_name, "_mean")
  dev_col_name <- paste0("dev_", col_name)
  dual_earner_df <- dual_earner_df %>%
    mutate(!!dev_col_name := .data[[col_name]] - .data[[mean_col_name]])
}

# 2 Hybrid models ---------------------------------------------------------

# 2.1 Define formula ------------------------------------------------------

dev_predictors <- names(dual_earner_df)[
  startsWith(names(dual_earner_df), "dev_combined_role")
]

mean_predictors <- names(dual_earner_df)[
  endsWith(names(dual_earner_df), "_mean") &
    startsWith(names(dual_earner_df), "combined_role")
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
  " + age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

# 2.2 Fit models ----------------------------------------------------------

message("Fitting models...")

mod_women <- lmer(f_women, data = dual_earner_df)
mod_men <- lmer(f_men, data = dual_earner_df)

summ(mod_women, digits = 2)
summ(mod_men, digits = 2)

performance::check_collinearity(mod_women)
performance::check_collinearity(mod_men)

# 2.3 Save models ----------------------------------------------------------

# Save models
saveRDS(
  list(
    mod_women = mod_women,
    mod_men = mod_men
  ),
  "outputs/models/mods_dual_earner.rds"
)

# Read models
# mod_women <- readRDS("outputs/models/mods_dual_earner.rds")$mod_women # nolint
# mod_men <- readRDS("outputs/models/mods_dual_earner.rds")$mod_men # nolint

message("✓ Models saved.")
