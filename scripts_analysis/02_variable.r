## ------------------------------------------------------------------------
##
## Script name: 02_variable.r
## Purpose: Construct variables (income and housework division)
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## Variable construction:
## 1. Income: Capped at 99th percentile, categorized roles
## 2. Housework: Capped at 99th percentile, categorized roles
## 3. Combined roles: Created factor combining income and housework roles
## 4. Other variables:
##    - Standardized age (and squared)
##    - Categorized education
##    - Categorized number of children
##
## ------------------------------------------------------------------------

# 1 Income ----------------------------------------------------------------

# Top cap income at 99th percentile
income_capped <- quantile(sample_df$income, 0.99, na.rm = TRUE)
income_sp_capped <- quantile(sample_df$income_sp, 0.99, na.rm = TRUE)

# Division of income
sample_df <- sample_df %>%
  # Top cap income at 99th percentile
  mutate(
    income_capped = pmin(income, income_capped),
    income_sp_capped = pmin(income_sp, income_sp_capped)
  ) %>%
  # Replace missing income with 0
  mutate(
    income_capped = ifelse(is.na(income_capped), 0, income_capped),
    income_sp_capped = ifelse(is.na(income_sp_capped), 0, income_sp_capped)
  ) %>%
  # Calculate income division
  mutate(
    income_prop = income_capped / (income_capped + income_sp_capped)
  ) %>%
  # Categorize income division
  mutate(
    income_role = case_when(
      income_prop > 0.6 ~ "NonTrad",
      income_prop >= 0.4 & income_prop <= 0.6 ~ "Egal",
      income_prop < 0.4 ~ "Trad"
    )
  ) %>%
  # Remove intermediate variables
  select(-income_capped, -income_sp_capped)

# 2 Housework -------------------------------------------------------------

# Top cap housework hours at 99th percentile
housework_hour_capped <- quantile(
  sample_df$housework_hour, 0.99
)

housework_hour_sp_capped <- quantile(
  sample_df$housework_hour_sp, 0.99
)

# Division of housework
sample_df <- sample_df %>%
  # Top cap housework hours at 99th percentile
  mutate(
    housework_hour_capped = pmin(housework_hour, housework_hour_capped),
    housework_hour_sp_capped = pmin(housework_hour_sp, housework_hour_sp_capped)
  ) %>%
  # Calculate housework division
  mutate(
    housework_prop = housework_hour_capped /
      (housework_hour_capped + housework_hour_sp_capped)
  ) %>%
  # Categorize housework division
  mutate(
    housework_role = case_when(
      housework_prop < 0.4 ~ "NonTrad",
      housework_prop >= 0.4 & housework_prop <= 0.6 ~ "Egal",
      housework_prop > 0.6 ~ "Trad"
    )
  ) %>%
  # Remove intermediate variables
  select(-housework_hour_capped, -housework_hour_sp_capped)

# 3 Combined division -----------------------------------------------------

sample_df <- sample_df %>%
  mutate(
    combined_role = paste(income_role, housework_role, sep = "-")
  ) %>%
  mutate(
    income_role = factor(
      income_role,
      levels = c(
        "Egal",
        "Trad",
        "NonTrad"
      )
    ),
    housework_role = factor(
      housework_role,
      levels = c(
        "Egal",
        "Trad",
        "NonTrad"
      )
    ),
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

# 4 Other variables -------------------------------------------------------

sample_df <- sample_df %>%
  # Categorize year
  mutate(
    year = factor(year)
  ) %>%
  # Standardize age
  mutate(
    age_std = scale(age),
    age_sp_std = scale(age_sp),
    age_std_sq = age_std^2,
    age_sp_std_sq = age_sp_std^2,
    age_h_std = scale(age_h),
    age_w_std = scale(age_w),
    age_h_std_sq = age_h_std^2,
    age_w_std_sq = age_w_std^2
  ) %>%
  # Categorize education
  mutate(
    educ = factor(
      educ,
      levels = c(
        "primary or less",
        "middle school",
        "high school",
        "college or higher"
      )
    ),
    educ_sp = factor(
      educ_sp,
      levels = c(
        "primary or less",
        "middle school",
        "high school",
        "college or higher"
      )
    ),
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
    n_children = ifelse(n_children >= 3, 3, n_children)
  ) %>%
  mutate(
    n_children = factor(
      n_children,
      levels = c(0, 1, 2, 3)
    )
  )

# 5 Save ------------------------------------------------------------------

# Save the sample
saveRDS(sample_df, "data_clean/sample_df.rds")
