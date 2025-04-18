## ------------------------------------------------------------------------
##
## Script name: 03_delta.r
## Purpose: Remove samples within exclusion bands
## Author: Yanwen Wang
## Date Created: 2025-04-18
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Remove couples within exclusion bands
##
## ------------------------------------------------------------------------

# 1 Prepare data ----------------------------------------------------------

# 1.1 Create sample --------------------------------------------------------

# Load data
sample_df <- readRDS("data/processed/sample_df.rds")

# Set exclusion band
delta <- 0.02

sample_df <- sample_df %>%
  filter(
    (income_w_prop < 0.35 - delta) |
      (income_w_prop > 0.35 + delta & income_w_prop < 0.65 - delta) |
      (income_w_prop > 0.65 + delta)
  ) %>%
  filter(
    housework_w_prop < 0.35 - delta |
      (housework_w_prop > 0.35 + delta & housework_w_prop < 0.65 - delta) |
      housework_w_prop > 0.65 + delta
  )

# 1.2 Create dummy variables for role ----------------------------

# Create dummy variables
role_dummies_mat <- model.matrix(
  ~role,
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
  startsWith(names(sample_df), "dev_role")
]

mean_predictors <- names(sample_df)[
  endsWith(names(sample_df), "_mean") &
    startsWith(names(sample_df), "role")
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

message("Fitting models with samples within exclusion bands removed...")

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
  "outputs/models/mods_exclude.rds"
)

# Read models
# mod_women <- readRDS("outputs/models/mods_dual_earner.rds")$mod_women # nolint
# mod_men <- readRDS("outputs/models/mods_dual_earner.rds")$mod_men # nolint

message("✓ Models saved.")
