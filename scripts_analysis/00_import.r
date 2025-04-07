## ------------------------------------------------------------------------
##
## Script name: 00_import.r
## Purpose: Import packages, functions, and data
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
library(ggeffects)
library(haven)
library(janitor)
library(jtools)
library(lme4)
library(patchwork)
library(tidyverse)

theme_set(theme_bw())

# Load functions
source("scripts_analysis/functions.r")
message("✓ Functions loaded successfully.")

# Load data
cfps_1022 <- readRDS("data_clean/cfps_1022.rds") %>%
  filter(year != 2012) # Remove 2012 data for missing housework hours
message("✓ Data imported successfully.")

# Source all analysis scripts in order
message("\n--- Running analysis scripts ---\n")

# 01_sample.r - Sample selection
source("scripts_analysis/01_sample.r")
message("✓ Sample selection completed.")

# 02_variable.r - Variable construction
source("scripts_analysis/02_variable.r")
message("✓ Variable construction completed.")

# 03_describe.r - Descriptive statistics
source("scripts_analysis/03_describe.r")
message("✓ Descriptive statistics completed.")

# 04_model.r - Model
source("scripts_analysis/04_model.r")
message("✓ Model completed.")

# 05_plot.r - Plot results
source("scripts_analysis/05_plot.r")
message("✓ Plot results completed.")

message("\n--- All analysis scripts ran successfully ---\n")
