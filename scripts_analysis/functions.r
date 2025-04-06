## ------------------------------------------------------------------------
##
## Script name: functions.r
## Purpose: Functions
## Author: Yanwen Wang
## Date Created: 2025-04-05
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# Sample restriction
restrict_sample <- function(data, steps) {
  # Initialize variables
  initial_obs <- nrow(data)
  current_obs <- initial_obs

  # Print initial observations
  cat(sprintf("Initial observations: %d\n\n", initial_obs))

  # Apply each filtering step
  for (i in seq_along(steps)) {
    step <- steps[[i]]

    # Apply the filter
    filtered_data <- step$filter(data)

    # Calculate statistics
    new_obs <- nrow(filtered_data)
    dropped <- current_obs - new_obs
    dropped_percent <- (dropped / current_obs) * 100

    # Print statistics
    cat(sprintf("Step %d: %s\n", i, step$description))
    cat(sprintf("  Observations before: %d\n", current_obs))
    cat(sprintf("  Observations after: %d\n", new_obs))
    cat(sprintf("  Dropped: %d (%.2f%%)\n\n", dropped, dropped_percent))

    # Update data and current observations
    data <- filtered_data
    current_obs <- new_obs
  }

  # Print final summary
  total_dropped <- initial_obs - current_obs
  total_dropped_percent <- (total_dropped / initial_obs) * 100

  cat(sprintf("Final Summary:\n"))
  cat(sprintf("  Initial observations: %d\n", initial_obs))
  cat(sprintf("  Final observations: %d\n", current_obs))
  cat(sprintf(
    "  Total dropped: %d (%.2f%%)\n",
    total_dropped,
    total_dropped_percent
  ))

  # Return the filtered data
  data
}
