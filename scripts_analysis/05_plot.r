## ------------------------------------------------------------------------
##
## Script name: 06_plot.r
## Purpose: Plot model results
## Author: Yanwen Wang
## Date Created: 2025-04-06
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: Plots predictions from hybrid models for visualization.
##
## ------------------------------------------------------------------------

# 1 Load data --------------------------------------------------------------

pred_df <- readRDS("outputs/tables/pred_lsat.rds")

# 2 Plot -------------------------------------------------------------------

# Reference bands
ref_bands <- pred_df %>%
  filter(role == "Egal/Egal") %>%
  select(gender, conf.low, conf.high) %>%
  rename(ref_low = conf.low, ref_high = conf.high) %>%
  mutate(x_min = 1, x_max = nlevels(pred_df$role))

plot_lsat <- ggplot(
  pred_df,
  aes(x = role)
) +
  geom_rect(
    data = ref_bands,
    aes(
      xmin = x_min, xmax = x_max,
      ymin = ref_low, ymax = ref_high,
      x = NULL
    ),
    fill = "lightgray",
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  geom_point(aes(y = predicted), size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  facet_wrap(~gender) +
  labs(
    x = "Household role (income/housework)",
    y = "Predicted life satisfaction"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave(
  "outputs/figures/pred_lsat.png",
  plot_lsat,
  width = 10,
  height = 6,
  dpi = 300
)

message("✓ Plot saved.")
