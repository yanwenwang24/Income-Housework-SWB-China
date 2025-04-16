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
## Notes: For auxiliary analysis
##
## ------------------------------------------------------------------------

# Load packages
library(broom)
library(ggeffects)
library(haven)
library(janitor)
library(jtools)
library(lme4)
library(mice)
library(patchwork)
library(tidyverse)

theme_set(theme_bw())

# Load functions
source("scripts_auxiliary/functions.r")
message("✓ Functions loaded successfully.")

# Source all auxiliary scripts in order
message("\n--- Running auxiliary scripts ---\n")

# 01_binary.r - Binary models
source("scripts_auxiliary/01_binary.r")
message("✓ Binary models completed.")

# 02_threshold.r - Models with alternative thresholds
source("scripts_auxiliary/02_threshold.r")
message("✓ Models with alternative thresholds completed.")

# 03_dual_earner.r - Dual-earner
source("scripts_auxiliary/03_dual_earner.r")
message("✓ Dual-earner analysis completed.")

# 04_impute.r - Impute missing values
source("scripts_auxiliary/04_impute.r")
message("✓ Imputation completed.")

message("\n--- All auxiliary scripts ran successfully ---\n")
