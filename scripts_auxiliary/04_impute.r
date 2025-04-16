## ------------------------------------------------------------------------
##
## Script name: 02_impute.r
## Purpose: Impute missing values
## Author: Yanwen Wang
## Date Created: 2025-04-06
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Impute missing values
##
## ------------------------------------------------------------------------

# 1 Sample restriction ----------------------------------------------------

message("Preparing data for imputation...")

# Load data
cfps_1022 <- readRDS("data_clean/cfps_1022.rds") %>%
  filter(year != 2012) # Remove 2012 data for missing housework hours

# Define initial filter functions
initial_steps <- list(
  list(
    description = "Select women",
    filter = function(d) d %>% filter(female == 1)
  ),
  list(
    description = "Continuously married",
    filter = function(d) d %>% filter(marital == "married")
  ),
  list(
    description = "Same partner during married periods",
    filter = function(d) {
      # Drop married years with missing/blank spouse id
      d_nonmissing <- d %>% filter(!is.na(pid_s), pid_s != "")

      valid_pids <- d_nonmissing %>%
        group_by(pid) %>%
        summarize(
          unique_partners = n_distinct(pid_s)
        ) %>%
        filter(unique_partners == 1) %>%
        pull(pid)

      different_sex <- d_nonmissing %>%
        filter(female != female_sp) %>%
        pull(pid)

      d %>%
        filter(
          !is.na(pid_s),
          pid_s != "",
          pid %in% valid_pids,
          pid %in% different_sex
        )
    }
  ),
  list(
    description = "Age range 20-60",
    filter = function(d) d %>% filter(age >= 20 & age <= 60)
  )
)

# Apply initial filters
for_impute_df <- restrict_sample(cfps_1022, initial_steps)

message("✓ Data prepared.")

# 2 Multiple imputation ---------------------------------------------------

# Define variables for imputation
vars_for_mice <- c(
  "pid", "year",
  "lsat_h", "lsat_w",
  "income_h", "income_w",
  "housework_hour_h", "housework_hour_w",
  "age_h", "age_w",
  "female", "female_sp",
  "educ_h", "educ_w",
  "hukou_h", "hukou_w",
  "migrant_h", "migrant_w",
  "chronic_h", "chronic_w",
  "n_children",
  "homeownership",
  "hh_income_p_log"
)

# Select variables for imputation
imputation_subset <- for_impute_df %>%
  select(all_of(vars_for_mice)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(
    educ_h = factor(
      educ_h,
      levels = c(
        "primary or less",
        "middle school",
        "high school",
        "college or higher"
      )
    ),
    educ_w = factor(
      educ_w,
      levels = c(
        "primary or less",
        "middle school",
        "high school",
        "college or higher"
      )
    )
  ) %>%
  # Categorize number of children
  mutate(
    n_children = ifelse(n_children >= 3, 3, n_children),
    n_children = factor(
      n_children,
      levels = c(0, 1, 2, 3)
    )
  ) %>%
  mutate(
    across(
      c(
        year,
        hukou_h, hukou_w,
        migrant_h, migrant_w,
        chronic_h, chronic_w,
        homeownership
      ),
      as.factor
    )
  )

# Create the default predictor matrix
pred_matrix <- make.predictorMatrix(imputation_subset)

# Exclude variables that should not be used as predictors
vars_to_exclude_as_predictors <- c("pid")

vars_to_exclude_as_predictors <- intersect(
  vars_to_exclude_as_predictors,
  colnames(pred_matrix)
)

if (length(vars_to_exclude_as_predictors) > 0) {
  pred_matrix[, vars_to_exclude_as_predictors] <- 0
}

# Exclude variables that should not be imputed
vars_not_to_impute <- c("pid", "year", "female", "female_sp")

vars_not_to_impute <- intersect(vars_not_to_impute, colnames(pred_matrix))

if (length(vars_not_to_impute) > 0) {
  pred_matrix[, vars_not_to_impute] <- 0
}

# Run mice
set.seed(123)
n_imputations <- 20

message("Imputing missing values...")

mice_obj <- mice(
  imputation_subset,
  m = n_imputations,
  maxit = 10,
  method = "pmm", # Predictive Mean Matching
  predictorMatrix = pred_matrix
)

# Save mice object
saveRDS(
  mice_obj,
  "outputs/models/mice_obj.rds"
)

message("✓ Imputed missing values.")

# 3 Post-imputation processing --------------------------------------------

message("Processing imputed data...")

long_df <- mice::complete(mice_obj, "long", include = TRUE)

processed_long_df <- long_df %>%
  filter(.imp > 0) %>% # Only process imputed datasets
  group_by(.imp) %>%
  nest() %>%
  mutate(
    processed_data = map(data, ~ process_imputed_data(.x))
  ) %>%
  select(.imp, processed_data) %>%
  filter(map_int(processed_data, nrow) > 0) %>%
  unnest(processed_data)

# Check before modeling
cat("Dimensions of processed long data:", dim(processed_long_df), "\n")
cat("Unique imputation numbers:", unique(processed_long_df$.imp), "\n")

message("✓ Processed imputed data.")

# 4 Modeling --------------------------------------------------------------

# 4.1 Define formula ------------------------------------------------------

# Split into list of datasets
list_of_datasets <- split(processed_long_df, processed_long_df$.imp)
cat("Created list of", length(list_of_datasets), "datasets for modeling.\n")

# Role predictors
one_dataset <- list_of_datasets[[1]]

dev_predictors <- names(one_dataset)[
  startsWith(names(one_dataset), "dev_combined_role")
]

mean_predictors <- names(one_dataset)[
  endsWith(names(one_dataset), "_mean") &
    startsWith(names(one_dataset), "combined_role")
]

role_predictors <- paste(c(dev_predictors, mean_predictors), collapse = " + ")

# Define formula
f_women <- as.formula(paste(
  "lsat_w ~",
  role_predictors,
  " + age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

f_men <- as.formula(paste(
  "lsat_h ~",
  role_predictors,
  " + age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

# 4.2 Fit models ----------------------------------------------------------

message("Fitting models to each imputed dataset...")

mod_women_fits <- map(list_of_datasets, ~ {
  tryCatch(
    {
      lmer(f_women, data = .x)
    },
    error = function(e) {
      message("Error fitting women's model for imputation: ", unique(.x$.imp))
      message("Error: ", e$message)
      NULL
    }
  )
})

mod_men_fits <- map(list_of_datasets, ~ {
  tryCatch(
    {
      lmer(f_men, data = .x)
    },
    error = function(e) {
      message("Error fitting men's model for imputation: ", unique(.x$.imp))
      message("Error: ", e$message)
      NULL
    }
  )
})

# Remove NULLs from the lists if any models failed
mod_women_fits <- mod_women_fits[!sapply(mod_women_fits, is.null)]
mod_men_fits <- mod_men_fits[!sapply(mod_men_fits, is.null)]

message(
  "Successfully fitted models for",
  length(mod_women_fits),
  "women's datasets and",
  length(mod_men_fits),
  "men's datasets."
)

# 4.3 Pool results --------------------------------------------------------

message("Pooling results...")

pooled_women <- pool(mod_women_fits)
pooled_men <- pool(mod_men_fits)

# Save pooled models
saveRDS(
  list(
    pooled_women = pooled_women,
    pooled_men = pooled_men
  ),
  "outputs/models/mods_mice.rds"
)

message("✓ Pooled results saved.")

# Read pooled models
# pooled_women <- readRDS("outputs/models/mods_mice.rds")$pooled_women # nolint
# pooled_men <- readRDS("outputs/models/mods_mice.rds")$pooled_men # nolint

# Summary
summary_women <- summary(pooled_women, conf.int = TRUE)
summary_men <- summary(pooled_men, conf.int = TRUE)

# Print fomatted summary
tidy_women <- tidy(pooled_women, conf.int = TRUE)
tidy_men <- tidy(pooled_men, conf.int = TRUE)

# Format the summary
tidy_women <- tidy_women %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3)),
    p.value = scales::pvalue(p.value, accuracy = 0.001, add_p = FALSE)
  )

tidy_men <- tidy_men %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3)),
    p.value = scales::pvalue(p.value, accuracy = 0.001, add_p = FALSE)
  )
