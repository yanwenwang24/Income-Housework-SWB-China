## ------------------------------------------------------------------------
##
## Script name: 02_fixed.r
## Purpose: Fixed effects analysis
## Author: Yanwen Wang
## Date Created: 2025-04-07
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Fixed effects models
##
## ------------------------------------------------------------------------

# 1 Prepare data ----------------------------------------------------------

# Load data
sample_df <- readRDS("data_clean/sample_df.rds")

fixed_effects_df <- sample_df %>%
  add_count(pid, name = "n_obs")

# Filter for individuals with 2 or more observations
fixed_effects_df <- fixed_effects_df %>%
  filter(n_obs >= 2)

# 2 Fixed effects models --------------------------------------------------

# 2.1 Define formula ------------------------------------------------------

f_women <- as.formula(paste(
  "lsat ~",
  "income_w_prop + housework_w_prop +",
  " age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  cohabit + n_children + homeownership + hh_income_p_log"
))

f_men <- as.formula(paste(
  "lsat_sp ~",
  "income_w_prop + housework_w_prop +",
  " age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  cohabit + n_children + homeownership + hh_income_p_log"
))

# 2.2 Fit models ----------------------------------------------------------

mod_women <- feols(f_women, data = fixed_effects_df, fixef = "pid")
mod_men <- feols(f_men, data = fixed_effects_df, fixef = "pid")

summary(mod_women)
summary(mod_men)

performance::check_collinearity(mod_women)
performance::check_collinearity(mod_men)

# 2.3 Save models ----------------------------------------------------------

# Save models
saveRDS(
  list(
    mod_women = mod_women,
    mod_men = mod_men
  ),
  "models/mods_fixed.rds"
)

# Read models
mod_women <- readRDS("models/mods_fixed.rds")$mod_women
mod_men <- readRDS("models/mods_fixed.rds")$mod_men
