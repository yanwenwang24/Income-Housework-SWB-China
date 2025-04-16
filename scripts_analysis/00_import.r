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
message("Loading required libraries...")

# List of packages to check/install
packages_to_install <- c(
  "ggeffects",
  "haven",
  "janitor",
  "jtools",
  "lme4",
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
source("scripts_analysis/functions.r")

# Load data
message("Loading data...")
cfps_1022 <- readRDS("data_clean/cfps_1022.rds") %>%
  filter(year != 2012) # Remove 2012 data for missing housework hours
message("✓ Data loaded successfully.")

# Source all analysis scripts in order
message("\n--- Running analysis scripts ---\n")

# Sample selection
source("scripts_analysis/01_sample.r")

# Variable construction
source("scripts_analysis/02_variable.r")

# Descriptive statistics
source("scripts_analysis/03_describe.r")

# Model
source("scripts_analysis/04_model.r")

# Plot results
source("scripts_analysis/05_plot.r")

message("\n--- All analysis scripts ran successfully ---\n")
