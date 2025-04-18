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
message("Loading required libraries...")

# List of packages to check/install
packages_to_install <- c(
  "broom",
  "ggeffects",
  "haven",
  "janitor",
  "jtools",
  "lme4",
  "mice",
  "patchwork",
  "performance",
  "tidyverse"
)

# Install if needed
for (pkg in packages_to_install) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    message(sprintf("Installed package: %s", pkg))
  }
}

# Load packages after installation
invisible(lapply(packages_to_install, library, character.only = TRUE))
message("Libraries loaded successfully.")

# Set theme
theme_set(theme_bw())

# Load functions
source("scripts_auxiliary/functions.r")

# Source all auxiliary scripts in order
message("\n--- Running auxiliary scripts ---\n")

# Models with binary dependent variables
source("scripts_auxiliary/01_binary.r")

# Models with alternative thresholds
source("scripts_auxiliary/02_threshold.r")

# Exclusion bands
source("scripts_auxiliary/03_delta.r")

# Dual-earner
source("scripts_auxiliary/04_dual_earner.r")

# Impute missing values
source("scripts_auxiliary/05_impute.r")

message("\n--- All auxiliary scripts ran successfully ---\n")
