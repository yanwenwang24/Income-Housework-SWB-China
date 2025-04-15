## ------------------------------------------------------------------------
##
## Script name: functions.r
## Purpose: Functions
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# Sample restriction
restrict_sample <- function(data, steps) {
  # Initialize variables
  initial_obs <- nrow(data)
  current_obs <- initial_obs

  # Print initial observations
  cat(sprintf("Initial observations: %d\n\n", initial_obs))

  # Apply each filtering step
  for (i in seq_along(steps)) {
    step <- steps[[i]]

    # Apply the filter
    filtered_data <- step$filter(data)

    # Calculate statistics
    new_obs <- nrow(filtered_data)
    dropped <- current_obs - new_obs
    dropped_percent <- (dropped / current_obs) * 100

    # Print statistics
    cat(sprintf("Step %d: %s\n", i, step$description))
    cat(sprintf("  Observations before: %d\n", current_obs))
    cat(sprintf("  Observations after: %d\n", new_obs))
    cat(sprintf("  Dropped: %d (%.2f%%)\n\n", dropped, dropped_percent))

    # Update data and current observations
    data <- filtered_data
    current_obs <- new_obs
  }

  # Print final summary
  total_dropped <- initial_obs - current_obs
  total_dropped_percent <- (total_dropped / initial_obs) * 100

  cat(sprintf("Final Summary:\n"))
  cat(sprintf("  Initial observations: %d\n", initial_obs))
  cat(sprintf("  Final observations: %d\n", current_obs))
  cat(sprintf(
    "  Total dropped: %d (%.2f%%)\n",
    total_dropped,
    total_dropped_percent
  ))

  # Return the filtered data
  data
}

process_imputed_data <- function(d) {
  # Apply filters based on imputed values
  d_filtered <- d %>%
    filter(
      # At least one does housework
      (housework_hour_h > 0 | housework_hour_w > 0),
      # At least one income > 0
      ((!is.na(income_h) & income_h > 0) | (!is.na(income_w) & income_w > 0)),
      # Non-missing life satisfaction and control variables
      !is.na(age_h), !is.na(age_w),
      !is.na(lsat_h), !is.na(lsat_w),
      !is.na(educ_h), !is.na(educ_w),
      !is.na(hukou_h), !is.na(hukou_w),
      !is.na(migrant_h), !is.na(migrant_w),
      !is.na(chronic_h), !is.na(chronic_w),
      !is.na(n_children),
      !is.na(homeownership),
      !is.na(hh_income_p_log)
    )

  # Variable construction
  d_processed <- d_filtered %>%
    # Calculate quantiles over the *entire current imputation dataset*
    mutate(
      income_h_q99 = quantile(income_h, 0.99, na.rm = TRUE),
      income_w_q99 = quantile(income_w, 0.99, na.rm = TRUE),
      housework_h_q99 = quantile(housework_hour_h, 0.99, na.rm = TRUE),
      housework_w_q99 = quantile(housework_hour_w, 0.99, na.rm = TRUE)
    ) %>%
    # Income Role
    mutate(
      # Cap income
      income_h_capped = pmin(income_h, income_h_q99),
      income_w_capped = pmin(income_w, income_w_q99),
      total_income = income_h_capped + income_w_capped,
      income_h_prop = case_when(
        total_income > 0 ~ income_h_capped / total_income,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      income_role = case_when(
        income_h_prop > 0.65 ~ "Trad", # Husband earns > 65%
        income_h_prop >= 0.35 & income_h_prop <= 0.65 ~ "Egal",
        income_h_prop < 0.35 ~ "NonTrad", # Husband earns < 35%
        is.na(income_h_prop) ~ NA_character_
      )
    ) %>%
    # Housework Role
    mutate(
      # Cap housework
      housework_hour_h_capped = pmin(housework_hour_h, housework_h_q99),
      housework_hour_w_capped = pmin(housework_hour_w, housework_w_q99),
      total_housework = housework_hour_h_capped + housework_hour_w_capped,
      # Husband's proportion of housework
      housework_h_prop = case_when(
        total_housework > 0 ~ housework_hour_h_capped / total_housework,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      housework_role = case_when(
        housework_h_prop < 0.35 ~ "Trad", # Husband does < 35%
        housework_h_prop >= 0.35 & housework_h_prop <= 0.65 ~ "Egal",
        housework_h_prop > 0.65 ~ "NonTrad", # Husband does > 65%
        is.na(housework_h_prop) ~ NA_character_
      )
    ) %>%
    # Select needed columns, remove intermediate ones if desired
    select(
      -ends_with("_q99"),
      -ends_with("_capped"),
      -total_income, -total_housework
    ) %>%
    # Combined Role
    mutate(
      combined_role = if_else(
        !is.na(income_role) & !is.na(housework_role),
        paste(income_role, housework_role, sep = "-"),
        NA_character_
      ),
      income_role = factor(
        income_role,
        levels = c("Egal", "Trad", "NonTrad")
      ),
      housework_role = factor(
        housework_role,
        levels = c("Egal", "Trad", "NonTrad")
      ),
      combined_role = factor(
        combined_role,
        levels = c(
          "Egal-Egal", "Egal-Trad", "Egal-NonTrad",
          "Trad-Egal", "Trad-Trad", "Trad-NonTrad",
          "NonTrad-Egal", "NonTrad-Trad", "NonTrad-NonTrad"
        )
      )
    ) %>%
    # Filter out rows where combined_role is NA
    filter(!is.na(combined_role)) %>%
    # Other Variables (standardization, factoring)
    mutate(
      year = factor(year),
      age_h_std = scale(age_h)[, 1],
      age_w_std = scale(age_w)[, 1],
      age_h_std_sq = age_h_std^2,
      age_w_std_sq = age_w_std^2
    ) %>%
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
      ),
      # Factor Children
      n_children = factor(n_children, levels = c(0, 1, 2, 3))
    )

  # Prepare for Hybrid Model (Dummies, Means, Deviations)
  # Create dummy variables for the current imputation dataset
  # Check if combined_role has any levels before creating matrix
  if (n_distinct(d_processed$combined_role) > 1) {
    role_dummies_mat <- model.matrix(~combined_role, data = d_processed)
    role_dummies_mat <- role_dummies_mat[, -1, drop = FALSE] # Drop intercept
    dummy_col_names <- make.names(colnames(role_dummies_mat))
    colnames(role_dummies_mat) <- dummy_col_names

    # Check if any dummy columns were actually created
    if (ncol(role_dummies_mat) > 0) {
      d_processed <- cbind(d_processed, role_dummies_mat)

      # Calculate within-couple means and deviations *within this imputation*
      # Group by pid ONLY.
      d_final <- d_processed %>%
        group_by(pid) %>%
        # Calculate means
        mutate(
          across(
            all_of(dummy_col_names),
            list(mean = ~ mean(., na.rm = TRUE)),
            .names = "{.col}_{.fn}"
          )
        ) %>%
        ungroup() %>%
        # Calculate deviations
        mutate(
          across(
            all_of(dummy_col_names),
            ~ . - get(paste0(cur_column(), "_mean")),
            .names = "dev_{.col}"
          )
        )
    } else {
      warning(
        "No dummy columns created for combined_role in this imputation.\n
         Skipping mean/deviation calculation."
      )
      d_final <- d_processed
    }
  } else {
    warning(
      "Only one level of combined_role present in this imputation.\n
       Skipping dummy/mean/deviation calculation."
    )
    d_final <- d_processed
  }


  # Select necessary columns for modeling
  base_level <- "Egal-Egal"
  role_levels <- levels(d_final$combined_role)
  non_base_levels <- setdiff(role_levels, base_level)
  expected_dummy_names <- make.names(paste0("combined_role", non_base_levels))
  expected_mean_names <- paste0(expected_dummy_names, "_mean")
  expected_dev_names <- paste0("dev_", expected_dummy_names)

  # Define all final columns needed
  final_cols <- c(
    ".id",
    "pid", "year",
    "lsat_h", "lsat_w",
    expected_dev_names, expected_mean_names,
    "age_h_std", "age_w_std",
    "age_h_std_sq", "age_w_std_sq",
    "educ_h", "educ_w",
    "hukou_h", "hukou_w",
    "migrant_h", "migrant_w",
    "chronic_h", "chronic_w",
    "n_children",
    "homeownership",
    "hh_income_p_log"
  )

  # Select ONLY the necessary columns, ensuring order/presence
  d_selected <- d_final %>%
    select(all_of(final_cols))

  d_selected
}
