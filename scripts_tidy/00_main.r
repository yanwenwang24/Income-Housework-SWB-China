## ------------------------------------------------------------------------
##
## Script name: 00_main.r
## Purpose: Main script for cleaning data
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# Load packages
library(haven)
library(tidyverse)

# Source scripts
source("scripts_tidy/01_cfps_2010.r")
message("✓ Cleaned 2010 CFPS data")

source("scripts_tidy/02_cfps_2012.r")
message("✓ Cleaned 2012 CFPS data")

source("scripts_tidy/03_cfps_2014.r")
message("✓ Cleaned 2014 CFPS data")

source("scripts_tidy/04_cfps_2016.r")
message("✓ Cleaned 2016 CFPS data")

source("scripts_tidy/05_cfps_2018.r")
message("✓ Cleaned 2018 CFPS data")

source("scripts_tidy/06_cfps_2020.r")
message("✓ Cleaned 2020 CFPS data")

source("scripts_tidy/07_cfps_2022.r")
message("✓ Cleaned 2022 CFPS data")

source("scripts_tidy/08_merge.r")
message("✓ Merged all CFPS data")
