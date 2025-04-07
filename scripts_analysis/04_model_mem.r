## ------------------------------------------------------------------------
##
## Script name: 04_model_mem.r
## Purpose: Mixed-effects models
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## Mixed-effects modeling for women and men, examining household roles and
## life satisfaction.
## The script:
## 1. Defines model formulas with random effects for individual IDs
## 2. Fits mixed-effects models using lmer()
## 3. Generates summary statistics
## 4. Saves fitted models
## 5. Creates predictions for different role categories
## 6. Saves prediction results for visualization
##
## ------------------------------------------------------------------------

# 1 Mixed-effects models --------------------------------------------------

# 1.1 Define formula ------------------------------------------------------

f_women <- as.formula(
  "lsat ~ combined_role +
  age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  n_children + homeownership + hh_income_p_log +
  (1 | pid)"
)

f_men <- as.formula(
  "lsat_sp ~ combined_role +
  age_std + age_sp_std + age_std_sq + age_sp_std_sq + educ + educ_sp +
  hukou + hukou_sp + migrant + migrant_sp + chronic + chronic_sp +
  n_children + homeownership + hh_income_p_log +
  (1 | pid)"
)

# 1.2 Fit model -----------------------------------------------------------

mod_women <- lmer(f_women, data = sample_df)
mod_men <- lmer(f_men, data = sample_df)

summ(mod_women, digits = 3)
summ(mod_men, digits = 3)

# 1.3 Save models ---------------------------------------------------------

# Save models
saveRDS(
  list(
    mod_women = mod_women,
    mod_men = mod_men
  ),
  "models/mods_mem.rds"
)

# Read models
mod_women <- readRDS("models/mods_mem.rds")$mod_women
mod_men <- readRDS("models/mods_mem.rds")$mod_men

# 2 Prediction ------------------------------------------------------------

pred_women <- predict_response(mod_women, terms = "combined_role") %>%
  as.data.frame() %>%
  rename(role = x) %>%
  mutate(gender = "women") %>%
  select(-group)

pred_men <- predict_response(mod_men, terms = "combined_role") %>%
  as.data.frame() %>%
  rename(role = x) %>%
  mutate(gender = "men") %>%
  select(-group)

pred_df <- bind_rows(pred_women, pred_men)

# Save predictions
saveRDS(
  pred_df,
  "outputs/pred_mem.rds"
)
