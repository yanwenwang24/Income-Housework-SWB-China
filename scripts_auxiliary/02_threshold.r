## ------------------------------------------------------------------------
##
## Script name: 02_threshold.r
## Purpose: Apply different thresholds
## Author: Yanwen Wang
## Date Created: 2025-04-07
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## 0.35 and 0.65 are the thresholds for the income/housework role
##
## ------------------------------------------------------------------------

# 1 Prepare data ----------------------------------------------------------

# 1.1 Apply different thresholds ------------------------------------------

# Load data
sample_df <- readRDS("data_clean/sample_df.rds")

# Define thresholds
low_band <- 0.40
high_band <- 0.60

sample_df <- sample_df %>%
  # Income
  mutate(
    income_role = case_when(
      income_w_prop > high_band ~ "NonTrad",
      income_w_prop >= low_band & income_w_prop <= high_band ~ "Egal",
      income_w_prop < low_band ~ "Trad"
    )
  ) %>%
  # Housework
  mutate(
    housework_role = case_when(
      housework_w_prop < low_band ~ "NonTrad",
      housework_w_prop >= low_band & housework_w_prop <= high_band ~ "Egal",
      housework_w_prop > high_band ~ "Trad"
    )
  ) %>%
  mutate(
    combined_role = paste(income_role, housework_role, sep = "-"),
    combined_role = factor(
      combined_role,
      levels = c(
        "Egal-Egal",
        "Egal-Trad",
        "Egal-NonTrad",
        "Trad-Egal",
        "Trad-Trad",
        "Trad-NonTrad",
        "NonTrad-Egal",
        "NonTrad-Trad",
        "NonTrad-NonTrad"
      )
    )
  )

# 1.2 Create dummy variables for combined_role ----------------------------

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

# 1.3 Calculate mean and deviation ----------------------------------------

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
  " + age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

# 2.2 Fit models ----------------------------------------------------------

message("Fitting models with alternative thresholds...")

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
  "outputs/models/mods_threshold.rds"
)

# Read models
# mod_women <- readRDS("outputs/models/mods_threshold.rds")$mod_women # nolint
# mod_men <- readRDS("outputs/models/mods_threshold.rds")$mod_men # nolint

message("✓ Models saved.")
