## ------------------------------------------------------------------------
##
## Script name: 03_describe.r
## Purpose: Describe the sample
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Sample size -----------------------------------------------------------

nrow(sample_df)
length(unique(sample_df$pid))

sample_df %>%
  group_by(pid) %>%
  summarise(
    n_obs = n()
  ) %>%
  tabyl(n_obs) %>%
  adorn_pct_formatting(2)

# 2 Income and housework roles --------------------------------------------

tabyl(sample_df, combined_role) %>%
  adorn_pct_formatting(2)

tabyl(sample_df, income_role) %>%
  adorn_pct_formatting(2)

tabyl(sample_df, housework_role) %>%
  adorn_pct_formatting(2)

# 3 Other variables --------------------------------------------------------

# Life satisfaction
mean(sample_df$lsat)
sd(sample_df$lsat)

mean(sample_df$lsat_sp)
sd(sample_df$lsat_sp)

# Income
mean(sample_df$income, na.rm = TRUE)
sd(sample_df$income, na.rm = TRUE)

mean(sample_df$income_sp, na.rm = TRUE)
sd(sample_df$income_sp, na.rm = TRUE)

# Housework hour
mean(sample_df$housework_hour)
sd(sample_df$housework_hour)

mean(sample_df$housework_hour_sp)
sd(sample_df$housework_hour_sp)

# Age
mean(sample_df$age)
sd(sample_df$age)

# Age squared
mean(sample_df$age_sp, na.rm = TRUE)
sd(sample_df$age_sp, na.rm = TRUE)

# Education
tabyl(sample_df, educ) %>%
  adorn_pct_formatting(2)

tabyl(sample_df, educ_sp) %>%
  adorn_pct_formatting(2)

# Hukou
tabyl(sample_df, hukou) %>%
  adorn_pct_formatting(2)

tabyl(sample_df, hukou_sp) %>%
  adorn_pct_formatting(2)

# Migrant
tabyl(sample_df, migrant) %>%
  adorn_pct_formatting(2)

tabyl(sample_df, migrant_sp) %>%
  adorn_pct_formatting(2)

# Chronic
tabyl(sample_df, chronic) %>%
  adorn_pct_formatting(2)

# Chronic spouse
tabyl(sample_df, chronic_sp) %>%
  adorn_pct_formatting(2)

# Number of children
tabyl(sample_df, n_children) %>%
  adorn_pct_formatting(2)

# Homeownership
tabyl(sample_df, homeownership) %>%
  adorn_pct_formatting(2)

# Household income per capita
mean(sample_df$hh_income_p)
sd(sample_df$hh_income_p)
