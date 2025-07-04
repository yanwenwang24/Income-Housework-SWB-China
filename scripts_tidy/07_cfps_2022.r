## ------------------------------------------------------------------------
##
## Script name: 07_cfps_2022.r
## Purpose: Clean CFPS 2022 data
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

message("Cleaning 2022 CFPS data...")

cfps_2022_person <- read_stata("data/raw/cfps_2022_person.dta")
cfps_2022_family <- read_stata("data/raw/cfps_2022_family.dta")
cfps_2022_roster <- read_stata("data/raw/cfps_2022_roster.dta")

# 2 Individual variables --------------------------------------------------

# Number of children (alive)
n_children_df <- cfps_2022_roster %>%
  select(
    pid,
    starts_with("alive_a22_c")
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .))) %>%
  mutate(n_children = rowSums(select(., starts_with("alive_a22_c")))) %>%
  select(pid, n_children)

# Information from the roster file
demography_df <- cfps_2022_roster %>%
  # ID
  mutate(
    pid_s = ifelse(pid_s < 0, NA_real_, pid_s)
  ) %>%
  # Interview year
  mutate(
    year = 2022
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
    age = 2022 - birth_year
  ) %>%
  # Hukou
  mutate(
    hukou_a22_p = ifelse(hukou_a22_p < 0, NA_real_, hukou_a22_p),
    hukou = case_when(
      hukou_a22_p == 1 ~ "agri",
      hukou_a22_p == 3 ~ "non-agri",
      hukou_a22_p == 7 ~ "non-agri",
      TRUE ~ NA_character_
    )
  ) %>%
  # Migrant status
  mutate(
    outpers_r_where22_p = ifelse(
      outpers_r_where22_p < 0,
      NA_real_,
      outpers_r_where22_p
    ),
    migrant = case_when(
      outpers_r_where22_p >= 5 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # Marital status
  mutate(
    tb3_a22_p = ifelse(tb3_a22_p < 0, NA_real_, tb3_a22_p),
    marital = case_when(
      tb3_a22_p == 1 ~ "never-married",
      tb3_a22_p == 2 ~ "married",
      tb3_a22_p == 3 ~ "cohabiting",
      tb3_a22_p == 4 ~ "divorced",
      tb3_a22_p == 5 ~ "widowed",
      TRUE ~ NA_character_
    ),
    cohabit = ifelse(marital == "cohabiting", 1, 0)
  ) %>%
  # Education
  mutate(
    tb4_a22_p = ifelse(tb4_a22_p < 0, NA_real_, tb4_a22_p),
    educ = case_when(
      tb4_a22_p == 1 ~ "primary or less",
      tb4_a22_p == 2 ~ "primary or less",
      tb4_a22_p == 3 ~ "middle school",
      tb4_a22_p == 4 ~ "high school",
      tb4_a22_p == 5 ~ "college or higher",
      tb4_a22_p == 6 ~ "college or higher",
      tb4_a22_p == 7 ~ "college or higher",
      tb4_a22_p == 8 ~ "college or higher",
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
individual_df <- cfps_2022_person %>%
  # Household ID
  mutate(
    fid = fid20
  ) %>%
  # Weight (cross-sectional and panel)
  mutate(
    weight = rswt_natcs22n,
    weight_panel = rswt_natpn1022n
  ) %>%
  # Province and urban
  mutate(
    province = zap_labels(provcd22),
    urban = ifelse(urban22 < 0, NA_real_, urban22),
  ) %>%
  # Income
  mutate(
    income = ifelse(emp_income < 0, NA_real_, emp_income)
  ) %>%
  # Whether currently working
  mutate(
    employ = ifelse(employ < 0, NA_real_, employ),
    current_work = case_when(
      employ == 1 ~ 1,
      employ == 0 ~ 0,
      employ == 3 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Employment status
  mutate(
    employ = case_when(
      employ == 0 ~ "unemployed",
      employ == 1 ~ "employed",
      employ == 3 ~ "inactive"
    )
  ) %>%
  # Work hour per week
  mutate(
    work_hour = ifelse(qg6 < 0, NA_real_, qg6)
  ) %>%
  # Housework hour per week
  mutate(
    qq9010n = ifelse(qq9010n < 0, NA_real_, qq9010n),
    qq9011n = ifelse(qq9011n < 0, NA_real_, qq9011n),
    qq9012 = ifelse(qq9012 < 0, NA_real_, qq9012),
    housework_hour_nojob = qq9010n * 7,
    housework_hour_job = qq9011n * 5 + qq9012 * 2
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
    employ,
    work_hour,
    housework_hour,
    lsat,
    chronic
  )

# 3 Household variables ---------------------------------------------------

household_df <- cfps_2022_family %>%
  # Household ID
  mutate(
    fid = fid22
  ) %>%
  # Homeownership
  mutate(
    fq2 = ifelse(fq2 < 0, NA_real_, fq2),
    homeownership = ifelse(fq2 == 1, 1, 0)
  ) %>%
  # Household size
  mutate(
    hh_size = ifelse(familysize22 < 0, NA_real_, familysize22)
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
cfps_2022 <- individual_df %>%
  left_join(n_children_df, by = "pid") %>%
  left_join(demography_df, by = "pid") %>%
  left_join(household_df, by = "fid")

message("✓ Cleaned 2022 CFPS data")
