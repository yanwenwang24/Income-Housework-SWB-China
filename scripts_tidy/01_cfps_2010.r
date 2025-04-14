## ------------------------------------------------------------------------
##
## Script name: 01_cfps_2010.r
## Purpose: Clean CFPS 2010 data
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Load data -------------------------------------------------------------

message("Cleaning 2010 CFPS data...")

cfps_2010_adult <- read_stata("data_raw/cfps_2010_adult.dta")
cfps_2010_family <- read_stata("data_raw/cfps_2010_family.dta")

# 2 Individual variables --------------------------------------------------

# Number of children (alive)
n_children_df <- cfps_2010_adult %>%
  select(
    pid,
    starts_with("alive_a_c")
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .))) %>%
  mutate(n_children = rowSums(select(., starts_with("alive_a_c")))) %>%
  select(pid, n_children)

individual_df <- cfps_2010_adult %>%
  # ID
  mutate(
    pid_s = ifelse(pid_s < 0, NA_real_, pid_s)
  ) %>%
  # Interview year
  mutate(
    year = 2010
  ) %>%
  # Weight
  mutate(
    weight = rswt_nat
  ) %>%
  # Province and urban
  mutate(
    province = zap_labels(provcd),
    urban = urban
  ) %>%
  # Gender
  mutate(
    female = 1 - gender
  ) %>%
  # Birth year and age
  mutate(
    birth_year = qa1y_best,
    age = qa1age
  ) %>%
  # Hukou
  mutate(
    hukou = case_when(
      qa2 == 1 ~ "agri",
      qa2 == 3 ~ "non-agri",
      TRUE ~ NA_character_
    )
  ) %>%
  # Migrant status
  mutate(
    ind_mig = ifelse(ind_mig < 0, NA_real_, ind_mig),
    migrant = 1 - ind_mig
  ) %>%
  # Marital status
  mutate(
    marital = case_when(
      qe1_best == 1 ~ "never-married",
      qe1_best == 2 ~ "married",
      qe1_best == 3 ~ "cohabiting",
      qe1_best == 4 ~ "divorced",
      qe1_best == 5 ~ "widowed",
      TRUE ~ NA_character_
    ),
    cohabit = ifelse(marital == "cohabiting", 1, 0)
  ) %>%
  # Education
  mutate(
    educ = case_when(
      cfps2010edu_best == 1 ~ "primary or less",
      cfps2010edu_best == 2 ~ "primary or less",
      cfps2010edu_best == 3 ~ "middle school",
      cfps2010edu_best == 4 ~ "high school",
      cfps2010edu_best == 5 ~ "college or higher",
      cfps2010edu_best == 6 ~ "college or higher",
      cfps2010edu_best == 7 ~ "college or higher",
      cfps2010edu_best == 8 ~ "college or higher",
      TRUE ~ NA_character_
    )
  ) %>%
  # Income
  mutate(
    income = ifelse(income < 0, NA_real_, income)
  ) %>%
  # Whether currently working
  mutate(
    current_work = case_when(
      qg3 == 1 ~ 1,
      qg3 == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Employment status
  mutate(
    employ = case_when(
      qg3 == 1 ~ "employed",
      qg3 == 0 & qj1 == 1 ~ "unemployed",
      qg3 == 0 & qj1 == 0 ~ "inactive"
    )
  ) %>%
  # Work hour per week
  mutate(
    kt2_a_1 = ifelse(kt2_a_1 < 0, 0, kt2_a_1),
    kt2_a_2 = ifelse(kt2_a_2 < 0, 0, kt2_a_2),
    work_hour = kt2_a_1 * 5 + kt2_a_2
  ) %>%
  # Housework hour per week
  mutate(
    kt104_a_1 = ifelse(kt104_a_1 < 0, 0, kt104_a_1),
    kt104_a_2 = ifelse(kt104_a_2 < 0, 0, kt104_a_2),
    housework_hour = kt104_a_1 * 5 + kt104_a_2
  ) %>%
  # Life satisfaction
  mutate(
    lsat = ifelse(qm403 < 0, NA_real_, qm403)
  ) %>%
  # Chronic illness
  mutate(
    chronic = ifelse(qp5 < 0, NA_real_, qp5)
  ) %>%
  # Select variables
  select(
    pid,
    pid_s,
    fid,
    year,
    weight,
    province,
    urban,
    female,
    birth_year,
    age,
    hukou,
    migrant,
    marital,
    cohabit,
    educ,
    income,
    current_work,
    employ,
    work_hour,
    housework_hour,
    lsat,
    chronic
  )

# 3 Household variables ---------------------------------------------------

household_df <- cfps_2010_family %>%
  # Homeownership
  mutate(
    fd1 = ifelse(fd1 < 0, NA_real_, fd1),
    homeownership = ifelse(fd1 == 1, 1, 0)
  ) %>%
  # Household size
  mutate(
    hh_size = ifelse(familysize < 0, NA_real_, familysize)
  ) %>%
  # Household income
  mutate(
    hh_income = ifelse(faminc < 0, NA_real_, faminc),
    hh_income_p = hh_income / (hh_size * hh_size),
    hh_income_p_log = log(hh_income_p + 1)
  ) %>%
  # Select variables
  select(
    fid,
    homeownership,
    hh_size,
    hh_income,
    hh_income_p,
    hh_income_p_log
  )

# 4 Merge and save data ---------------------------------------------------

# Merge
cfps_2010 <- individual_df %>%
  left_join(n_children_df, by = "pid") %>%
  left_join(household_df, by = "fid")

# Save
saveRDS(cfps_2010, "data_clean/cfps_2010.rds")

message("✓ Cleaned 2010 CFPS data")
