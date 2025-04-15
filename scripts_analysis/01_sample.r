## ------------------------------------------------------------------------
##
## Script name: 01_sample.r
## Purpose: Sample selection
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## Sample restriction steps:
## 1. Select women (since each relationship is identified twice)
## 2. Continuously married
## 3. Same partner during married periods
## 4. Age range: 20-60
## 5. Non-missing housework and at least one does housework (self and spouse)
## 6. At least one income (self or spouse) above 0
## 7. Non-missing life satisfaction (self and spouse)
## 8. Non-missing control variables:
##    self and spouse: education, hukou, migrant, chronic conditions
##    couple: number of children, homeownership, household income per capita
##
## ------------------------------------------------------------------------

# Define filtering steps
steps <- list(
  # Select women (since each relationship is identified twice)
  list(
    description = "Select women",
    filter = function(d) d %>% filter(female == 1)
  ),
  # Married
  list(
    description = "Continuously married",
    filter = function(d) d %>% filter(marital == "married")
  ),
  # With the same partner
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
  # Age range
  list(
    description = "Age range 20-60",
    filter = function(d) d %>% filter(age >= 20 & age <= 60)
  ),
  # Housework hours (self and spouse)
  list(
    description = "Non-missing housework hours and at least one does housework",
    filter = function(d) {
      d %>% filter(
        !is.na(housework_hour),
        !is.na(housework_hour_sp),
        (housework_hour > 0 | housework_hour_sp > 0)
      )
    }
  ),
  # Income (self and spouse)
  list(
    description = "At least one income (self or spouse) above 0",
    filter = function(d) {
      d %>% filter(
        ((!is.na(income) & income > 0) | (!is.na(income_sp) & income_sp > 0))
      )
    }
  ),
  # Life satisfaction (self and spouse)
  list(
    description = "Non-missing life satisfaction",
    filter = function(d) d %>% filter(!is.na(lsat), !is.na(lsat_sp))
  ),
  # Control variables
  list(
    description = "Non-missing control variables",
    filter = function(d) {
      d %>% filter(
        !is.na(age_h), !is.na(age_w),
        !is.na(educ), !is.na(educ_sp),
        !is.na(hukou), !is.na(hukou_sp),
        !is.na(migrant), !is.na(migrant_sp),
        !is.na(chronic), !is.na(chronic_sp),
        !is.na(n_children),
        !is.na(homeownership),
        !is.na(hh_income_p_log)
      )
    }
  )
)

# Apply stepwise restriction
sample_df <- restrict_sample(cfps_1022, steps)
