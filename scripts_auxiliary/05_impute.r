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
    description = "Continuously married or cohabiting",
    filter = function(d) d %>% filter(marital %in% c("married", "cohabiting"))
  ),
  list(
    description = "Same partner during married/cohabiting periods",
    filter = function(d) {
      valid_pids <- d %>%
        group_by(pid) %>%
        summarize(
          has_valid_partner = all(!is.na(pid_s) & pid_s != ""),
          unique_partners = n_distinct(pid_s)
        ) %>%
        filter(has_valid_partner & unique_partners == 1) %>%
        pull(pid)

      different_sex <- d %>%
        filter(female != female_sp) %>%
        pull(pid)

      d %>% filter(pid %in% valid_pids & pid %in% different_sex)
    }
  ),
  list(
    description = "Age range 20-60",
    filter = function(d) d %>% filter(age >= 20 & age <= 60)
  )
)

# Apply initial filters
for_impute_df <- restrict_sample(cfps_1022, initial_steps)

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
  "cohabit",
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
vars_not_to_impute <- c("pid", "year", "female", "female_sp", "cohabit")

vars_not_to_impute <- intersect(vars_not_to_impute, colnames(pred_matrix))

if (length(vars_not_to_impute) > 0) {
  pred_matrix[, vars_not_to_impute] <- 0
}

# Run mice
set.seed(123)
n_imputations <- 10

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
  "models/mice_obj.rds"
)

# 3 Post-imputation processing --------------------------------------------

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
  cohabit + n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

f_men <- as.formula(paste(
  "lsat_h ~",
  role_predictors,
  " + age_h_std + age_w_std + age_h_std_sq + age_w_std_sq + educ_h + educ_w +
  hukou_h + hukou_w + migrant_h + migrant_w + chronic_h + chronic_w +
  cohabit + n_children + homeownership + hh_income_p_log +",
  "(1 | pid)"
))

# 4.2 Fit models ----------------------------------------------------------

cat("Fitting models to each imputed dataset...\n")

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

cat(
  "Successfully fitted models for",
  length(mod_women_fits),
  "women's datasets and",
  length(mod_men_fits),
  "men's datasets.\n"
)

# 4.3 Pool results --------------------------------------------------------

cat("Pooling results...\n")

pooled_women <- pool(mod_women_fits)
pooled_men <- pool(mod_men_fits)

# Save pooled models
saveRDS(
  list(
    pooled_women = pooled_women,
    pooled_men = pooled_men
  ),
  "models/mods_imputed.rds"
)

cat("Pooled results saved.\n")

# Read pooled models
pooled_women <- readRDS("models/mods_imputed.rds")$pooled_women
pooled_men <- readRDS("models/mods_imputed.rds")$pooled_men

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

# 5 Predictions -----------------------------------------------------------

# 5.1 Prediction for women ------------------------------------------------

cat("Generating predictions for Women...\n")

# Extract predictors of interest (mean effects) from the POOLED summary
mean_predictors_women <- summary_women %>%
  filter(str_detect(term, "_mean$") & str_detect(term, "^combined_role")) %>%
  pull(term)

all_preds_list_women <- list()

for (i in seq_along(mod_women_fits)) {
  fit <- mod_women_fits[[i]]
  imp_num_label <- names(mod_women_fits)[i] %||% as.character(i)

  # Skip if fit is NULL (in case a model failed)
  if (is.null(fit)) {
    warning("Skipping NULL model fit for women, imputation: ", imp_num_label)
    next
  }

  # Predict for each mean term using this specific fit
  preds_i <- map_dfr(mean_predictors_women, ~ {
    term_formula <- paste0(.x, " [0, 1]") # Predict at 0 and 1
    tryCatch(
      {
        # Check if the term actually exists
        if (!.x %in% names(fixef(fit))) {
          message(
            "Term '",
            .x,
            "' not found in women's model for imputation ",
            imp_num_label, ". Skipping prediction for this term."
          )
          NULL
        }
        ggpredict(fit, terms = term_formula) %>%
          as.data.frame() %>%
          mutate(role = .x)
      },
      error = function(e) {
        message(
          "ggpredict failed for term '",
          .x,
          "' in women's model, imputation ",
          imp_num_label,
          ": ",
          e$message
        )
        NULL
      }
    )
  })

  # Add imputation number
  if (!is.null(preds_i) && nrow(preds_i) > 0) {
    all_preds_list_women[[length(all_preds_list_women) + 1]] <- preds_i %>%
      mutate(.imp = imp_num_label)
  }
}

all_raw_preds_women <- bind_rows(all_preds_list_women)

# Average predictions across imputations
avg_preds_women <- all_raw_preds_women %>%
  group_by(role, x) %>%
  summarise(
    predicted = mean(predicted, na.rm = TRUE),
    std.error = mean(std.error, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE),
    n_imputations = n(),
    .groups = "drop"
  )

# Process predictions
baseline_women <- avg_preds_women %>%
  filter(x == 0) %>%
  arrange(predicted) %>%
  slice(1) %>%
  mutate(role = "Egal/Egal") %>%
  select(role, predicted, std.error, conf.low, conf.high)

level_women <- avg_preds_women %>%
  filter(x == 1) %>%
  mutate(
    role = role %>%
      str_remove("_mean$") %>%
      str_remove("^combined_role") %>%
      str_replace_all("\\.", "/")
  ) %>%
  select(role, predicted, std.error, conf.low, conf.high)

pred_women <- bind_rows(baseline_women, level_women) %>%
  mutate(gender = "women")

# 5.2 Prediction for men --------------------------------------------------

cat("Generating predictions for Men...\n")

# Extract predictors of interest (mean effects) from the POOLED summary
mean_predictors_men <- summary_men %>%
  filter(str_detect(term, "_mean$") & str_detect(term, "^combined_role")) %>%
  pull(term)

all_preds_list_men <- list()

for (i in seq_along(mod_men_fits)) {
  fit <- mod_men_fits[[i]]
  imp_num_label <- names(mod_men_fits)[i] %||% as.character(i)

  # Skip if fit is NULL (in case a model failed)
  if (is.null(fit)) {
    warning("Skipping NULL model fit for men, imputation: ", imp_num_label)
    next
  }

  # Predict for each mean term using this specific fit
  preds_i <- map_dfr(mean_predictors_men, ~ {
    term_formula <- paste0(.x, " [0, 1]") # Predict at 0 and 1
    tryCatch(
      {
        # Check if the term actually exists
        if (!.x %in% names(fixef(fit))) {
          message(
            "Term '",
            .x,
            "' not found in women's model for imputation ",
            imp_num_label, ". Skipping prediction for this term."
          )
          NULL
        }
        ggpredict(fit, terms = term_formula) %>%
          as.data.frame() %>%
          mutate(role = .x)
      },
      error = function(e) {
        message(
          "ggpredict failed for term '",
          .x,
          "' in women's model, imputation ",
          imp_num_label,
          ": ",
          e$message
        )
        NULL
      }
    )
  })

  # Add imputation number
  if (!is.null(preds_i) && nrow(preds_i) > 0) {
    all_preds_list_men[[length(all_preds_list_men) + 1]] <- preds_i %>%
      mutate(.imp = imp_num_label)
  }
}

all_raw_preds_men <- bind_rows(all_preds_list_men)

# Average predictions across imputations
avg_preds_men <- all_raw_preds_men %>%
  group_by(role, x) %>%
  summarise(
    predicted = mean(predicted, na.rm = TRUE),
    std.error = mean(std.error, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE),
    n_imputations = n(),
    .groups = "drop"
  )

# Process predictions
baseline_men <- avg_preds_men %>%
  filter(x == 0) %>%
  arrange(predicted) %>%
  slice(1) %>%
  mutate(role = "Egal/Egal") %>%
  select(role, predicted, std.error, conf.low, conf.high)

level_men <- avg_preds_men %>%
  filter(x == 1) %>%
  mutate(
    role = role %>%
      str_remove("_mean$") %>%
      str_remove("^combined_role") %>%
      str_replace_all("\\.", "/")
  ) %>%
  select(role, predicted, std.error, conf.low, conf.high)

pred_men <- bind_rows(baseline_men, level_men) %>%
  mutate(gender = "men")

# Combine predictions
pred_df <- bind_rows(pred_women, pred_men)

# Save predictions
saveRDS(
  pred_df,
  "outputs/pred_imputed.rds"
)

cat("Predictions saved.\n")

# 6 Plot ------------------------------------------------------------------

cat("Plotting predictions...\n")

# Read predictions
pred_df <- readRDS("outputs/pred_imputed.rds")

pred_df <- pred_df %>%
  mutate(
    role = factor(
      role,
      levels = c(
        "Egal/Egal",
        "Egal/Trad",
        "Egal/NonTrad",
        "Trad/Egal",
        "Trad/Trad",
        "Trad/NonTrad",
        "NonTrad/Egal",
        "NonTrad/Trad",
        "NonTrad/NonTrad"
      )
    )
  )

# Reference bands
ref_bands <- pred_df %>%
  filter(role == "Egal/Egal") %>%
  select(gender, conf.low, conf.high) %>%
  rename(ref_low = conf.low, ref_high = conf.high) %>%
  mutate(x_min = 1, x_max = nlevels(pred_df$role))

plot_imputed <- ggplot(
  pred_df,
  aes(x = role)
) +
  geom_rect(
    data = ref_bands,
    aes(
      xmin = x_min, xmax = x_max,
      ymin = ref_low, ymax = ref_high,
      x = NULL
    ),
    fill = "lightgray",
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  geom_point(aes(y = predicted), size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  facet_wrap(~gender) +
  labs(
    x = "Household role (income/housework)",
    y = "Predicted life satisfaction"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave(
  "outputs/pred_imputed.png",
  plot_imputed,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Plots saved.\n")
