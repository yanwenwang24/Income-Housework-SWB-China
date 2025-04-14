## ------------------------------------------------------------------------
##
## Script name: 02_cfps_2012.r
## Purpose: Clean CFPS 2012 data
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

cfps_2012_adult <- read_stata("data_raw/cfps_2012_adult.dta")
cfps_2012_family <- read_stata("data_raw/cfps_2012_family.dta")
cfps_2012_roster <- read_stata("data_raw/cfps_2012_roster.dta")

# 2 Individual variables --------------------------------------------------

# Number of children (alive)
n_children_df <- cfps_2012_roster %>%
  select(
    pid,
    starts_with("alive_a_c")
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .))) %>%
  mutate(n_children = rowSums(select(., starts_with("alive_a_c")))) %>%
  select(pid, n_children)

# Information from the roster file
demography_df <- cfps_2012_roster %>%
  # ID
  mutate(
    pid_s = ifelse(pid_s < 0, NA_real_, pid_s)
  ) %>%
  # Interview year
  mutate(
    year = 2012
  ) %>%
  # Gender
  mutate(
    tb2_a_p = ifelse(tb2_a_p < 0, NA_real_, tb2_a_p),
    female = 1 - tb2_a_p
  ) %>%
  # Birth year and age
  mutate(
    tb1y_a_p = ifelse(tb1y_a_p < 0, NA_real_, tb1y_a_p),
    tb1b_a_p = ifelse(tb1b_a_p < 0, NA_real_, tb1b_a_p),
    birth_year = tb1y_a_p,
    age = tb1b_a_p
  ) %>%
  # Hukou
  mutate(
    qa301_a12_p = ifelse(qa301_a12_p < 0, NA_real_, qa301_a12_p),
    hukou = case_when(
      qa301_a12_p == 1 ~ "agri",
      qa301_a12_p == 3 ~ "non-agri",
      TRUE ~ NA_character_
    )
  ) %>%
  # Migrant status
  mutate(
    qa302_a12_p = ifelse(qa302_a12_p < 0, NA_real_, qa302_a12_p),
    migrant = case_when(
      qa302_a12_p >= 4 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # Marital status
  mutate(
    tb3_a12_p = ifelse(tb3_a12_p < 0, NA_real_, tb3_a12_p),
    marital = case_when(
      tb3_a12_p == 1 ~ "never-married",
      tb3_a12_p == 2 ~ "married",
      tb3_a12_p == 3 ~ "cohabiting",
      tb3_a12_p == 4 ~ "divorced",
      tb3_a12_p == 5 ~ "widowed",
      TRUE ~ NA_character_
    ),
    cohabit = ifelse(marital == "cohabiting", 1, 0)
  ) %>%
  # Education
  mutate(
    tb4_a12_p = ifelse(tb4_a12_p < 0, NA_real_, tb4_a12_p),
    educ = case_when(
      tb4_a12_p == 1 ~ "primary or less",
      tb4_a12_p == 2 ~ "primary or less",
      tb4_a12_p == 3 ~ "middle school",
      tb4_a12_p == 4 ~ "high school",
      tb4_a12_p == 5 ~ "college or higher",
      tb4_a12_p == 6 ~ "college or higher",
      tb4_a12_p == 7 ~ "college or higher",
      tb4_a12_p == 8 ~ "college or higher",
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
individual_df <- cfps_2012_adult %>%
  # Household ID
  mutate(
    fid = fid12
  ) %>%
  # Weight (cross-sectional and panel)
  mutate(
    weight = rswt_natcs12,
    weight_panel = rswt_natpn1012
  ) %>%
  # Province and urban
  mutate(
    province = zap_labels(provcd),
    urban = ifelse(urban12 < 0, NA_real_, urban12),
  ) %>%
  # Income
  mutate(
    income = ifelse(income_adj < 0, NA_real_, income_adj)
  ) %>%
  # Whether currently working
  mutate(
    qg101 = ifelse(qg101 < 0, NA_real_, qg101),
    current_work = case_when(
      qg101 == 1 ~ 1,
      qg101 == 5 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Employment status
  mutate(
    employ = case_when(
      employ == 0 ~ "unemployed",
      employ == 1 ~ "employed",
      employ == 8 ~ NA
    )
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
    lsat,
    chronic
  )

# 3 Household variables ---------------------------------------------------

household_df <- cfps_2012_family %>%
  # Household ID
  mutate(
    fid = fid12
  ) %>%
  # Homeownership
  mutate(
    fq1 = ifelse(fq1 < 0, NA_real_, fq1),
    homeownership = ifelse(fq1 == 1, 1, 0)
  ) %>%
  # Household size
  mutate(
    hh_size = ifelse(familysize < 0, NA_real_, familysize)
  ) %>%
  # Household income
  mutate(
    hh_income = ifelse(fincome1_adj < 0, NA_real_, fincome1_adj),
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
cfps_2012 <- individual_df %>%
  left_join(n_children_df, by = "pid") %>%
  left_join(demography_df, by = "pid") %>%
  left_join(household_df, by = "fid")

# Save
saveRDS(cfps_2012, "data_clean/cfps_2012.rds")
