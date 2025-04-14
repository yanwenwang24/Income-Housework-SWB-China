## ------------------------------------------------------------------------
##
## Script name: 01_spouse.r
## Purpose: Identify spousal information
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Prepare spousal data --------------------------------------------------

message("Preparing spousal data...")

# Variables for spousal information
sp_vars <- c(
  "female",
  "birth_year",
  "age", "hukou",
  "migrant",
  "marital",
  "educ",
  "income",
  "current_work",
  "employ",
  "work_hour",
  "housework_hour",
  "lsat",
  "chronic"
)

# Select spousal variables
sp_df <- cfps_1022 %>%
  select(
    pid_s,
    year,
    all_of(sp_vars)
  ) %>%
  rename_with(~ paste0(.x, "_sp"), -c(pid_s, year))

# Join with spousal information
cfps_1022 <- cfps_1022 %>%
  left_join(sp_df, by = c("pid" = "pid_s", "year" = "year"))

# Create variables for husband and wife based on gender
for (var in sp_vars) {
  h_var <- paste0(var, "_h")
  w_var <- paste0(var, "_w")
  sp_var <- paste0(var, "_sp")

  # For husband variables:
  # - If respondent is male (female = 0), use respondent's data
  # - If respondent is female (female = 1), use spouse's data
  cfps_1022[[h_var]] <- ifelse(
    cfps_1022$female == 0,
    cfps_1022[[var]],
    cfps_1022[[sp_var]]
  )

  # For wife variables:
  # - If respondent is female (female=1), use respondent's data
  # - If respondent is male (female=0), use spouse's data
  cfps_1022[[w_var]] <- ifelse(
    cfps_1022$female == 1,
    cfps_1022[[var]],
    cfps_1022[[sp_var]]
  )
}

# 2 Save merged data ------------------------------------------------------

saveRDS(cfps_1022, "data_clean/cfps_1022.rds")

message("✓ Added spouse variables")
