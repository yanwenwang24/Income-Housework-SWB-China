## ------------------------------------------------------------------------
##
## Script name: 03_cfps_2014.r
## Purpose: Clean CFPS 2014 data
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

cfps_2014_adult <- read_stata("data_raw/cfps_2014_adult.dta")
cfps_2014_family <- read_stata("data_raw/cfps_2014_family.dta")
cfps_2014_roster <- read_stata("data_raw/cfps_2014_roster.dta")

# 2 Individual variables --------------------------------------------------

# Number of children (alive)
n_children_df <- cfps_2014_roster %>%
  select(
    pid,
    starts_with("alive_a14_c")
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .))) %>%
  mutate(n_children = rowSums(select(., starts_with("alive_a14_c")))) %>%
  select(pid, n_children)

# Information from the roster file
demography_df <- cfps_2014_roster %>%
  # ID
  mutate(
    pid_s = ifelse(pid_s < 0, NA_real_, pid_s)
  ) %>%
  # Interview year
  mutate(
    year = 2014
  ) %>%
  # Gender
  mutate(
    tb2_a_p = ifelse(tb2_a_p < 0, NA_real_, tb2_a_p),
    female = 1 - tb2_a_p
  ) %>%
  # Birth year and age
  mutate(
    tb1y_a_p = ifelse(tb1y_a_p < 0, NA_real_, tb1y_a_p),
    birth_year = tb1y_a_p,
    age = 2014 - birth_year
  ) %>%
  # Hukou
  mutate(
    qa301_a14_p = ifelse(qa301_a14_p < 0, NA_real_, qa301_a14_p),
    hukou = case_when(
      qa301_a14_p == 1 ~ "agri",
      qa301_a14_p == 3 ~ "non-agri",
      TRUE ~ NA_character_
    )
  ) %>%
  # Migrant status
  mutate(
    qa302_a14_p = ifelse(qa302_a14_p < 0, NA_real_, qa302_a14_p),
    migrant = case_when(
      qa302_a14_p >= 4 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # Marital status
  mutate(
    tb3_a14_p = ifelse(tb3_a14_p < 0, NA_real_, tb3_a14_p),
    marital = case_when(
      tb3_a14_p == 1 ~ "never-married",
      tb3_a14_p == 2 ~ "married",
      tb3_a14_p == 3 ~ "cohabiting",
      tb3_a14_p == 4 ~ "divorced",
      tb3_a14_p == 5 ~ "widowed",
      TRUE ~ NA_character_
    ),
    cohabit = ifelse(marital == "cohabiting", 1, 0)
  ) %>%
  # Education
  mutate(
    tb4_a14_p = ifelse(tb4_a14_p < 0, NA_real_, tb4_a14_p),
    educ = case_when(
      tb4_a14_p == 1 ~ "primary or less",
      tb4_a14_p == 2 ~ "primary or less",
      tb4_a14_p == 3 ~ "middle school",
      tb4_a14_p == 4 ~ "high school",
      tb4_a14_p == 5 ~ "college or higher",
      tb4_a14_p == 6 ~ "college or higher",
      tb4_a14_p == 7 ~ "college or higher",
      tb4_a14_p == 8 ~ "college or higher",
      tb4_a14_p == 9 ~ "primary or less",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    pid,
    pid_s,
    year,
    female,
    birth_year,
    age,
    hukou,
    migrant,
    marital,
    cohabit,
    educ
  )

# Information from the survey file
individual_df <- cfps_2014_adult %>%
  # Household ID
  mutate(
    fid = fid14
  ) %>%
  # Weight (cross-sectional and panel)
  mutate(
    weight = rswt_natcs14,
    weight_panel = rswt_natpn1014
  ) %>%
  # Province and urban
  mutate(
    province = zap_labels(provcd14),
    urban = ifelse(urban14 < 0, NA_real_, urban14),
  ) %>%
  # Income
  mutate(
    income = ifelse(income < 0, NA_real_, income)
  ) %>%
  # Whether currently working
  mutate(
    employ2014 = ifelse(employ2014 < 0, NA_real_, employ2014),
    current_work = case_when(
      employ2014 == 1 ~ 1,
      employ2014 == 0 ~ 0,
      employ2014 == 3 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Work hour per week
  mutate(
    work_hour = ifelse(qg6 < 0, NA_real_, qg6)
  ) %>%
  # Housework hour per week
  mutate(
    qq9010 = ifelse(qq9010 < 0, NA_real_, qq9010),
    qq9011 = ifelse(qq9011 < 0, NA_real_, qq9011),
    qq9012 = ifelse(qq9012 < 0, NA_real_, qq9012),
    housework_hour_nojob = qq9010 * 7,
    housework_hour_job = qq9011 * 5 + qq9012 * 2
  ) %>%
  mutate(
    housework_hour = coalesce(housework_hour_nojob, housework_hour_job)
  ) %>%
  # Life satisfaction
  mutate(
    lsat = ifelse(qn12012 < 0 | qn12012 == 79, NA_real_, qn12012)
  ) %>%
  # Chronic illness
  mutate(
    chronic = ifelse(qp401 < 0, NA_real_, qp401)
  ) %>%
  # Select variables
  select(
    pid,
    fid,
    weight,
    province,
    urban,
    income,
    current_work,
    work_hour,
    housework_hour,
    lsat,
    chronic
  )

# 3 Household variables ---------------------------------------------------

household_df <- cfps_2014_family %>%
  # Household ID
  mutate(
    fid = fid14
  ) %>%
  # Homeownership
  mutate(
    fq2 = ifelse(fq2 < 0, NA_real_, fq2),
    homeownership = ifelse(fq2 == 1, 1, 0)
  ) %>%
  # Household size
  mutate(
    hh_size = ifelse(familysize < 0, NA_real_, familysize)
  ) %>%
  # Household income
  mutate(
    hh_income = ifelse(fincome1 < 0, NA_real_, fincome1),
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

# Remove duplicates
demography_df <- demography_df %>%
  group_by(pid) %>%
  filter(row_number() == 1) %>%
  ungroup()

n_children_df <- n_children_df %>%
  group_by(pid) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Merge
cfps_2014 <- individual_df %>%
  left_join(n_children_df, by = "pid") %>%
  left_join(demography_df, by = "pid") %>%
  left_join(household_df, by = "fid")

# Save
saveRDS(cfps_2014, "data_clean/cfps_2014.rds")
