## ------------------------------------------------------------------------
##
## Script name: 08_merge.r
## Purpose: Merge all CFPS data
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Merge all CFPS data ---------------------------------------------------

message("Merging all CFPS data...")

cfps_1022 <- bind_rows(
  cfps_2010,
  cfps_2012,
  cfps_2014,
  cfps_2016,
  cfps_2018,
  cfps_2020,
  cfps_2022
)

message("✓ Merged all CFPS data")
