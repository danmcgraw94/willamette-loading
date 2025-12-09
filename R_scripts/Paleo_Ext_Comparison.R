## Paleoflood - EXT data visualization
# Add in csv data instead of hard coding later

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo)


# Theme and plot directory
theme_set(theme_bw())

# Standard dataset
standard <- data.frame(
  start = c(-300, 380, 1275, 1280, 1710, 1862, 1928, 1931, 1937, 1939, 1949, 1951, 1955),
  end = c(379, 1274, 1279, 1709, 1861, 1926, 1929, 1933, 1937, 1939, 1949, 1951, 1955),
  value = c(60000, 45000, 37000, 27000, 24000, 18000, 18000, 18000, 18000, 18000, 18000, 18000, 18000),
  dataset = "Standard"
)

# Extended dataset
extended <- data.frame(
  start = c(-300, 1275, 1710, 1862, 1928, 1931, 1937, 1939, 1949, 1951, 1955),
  end = c(1274, 1709, 1861, 1926, 1929, 1933, 1937, 1939, 1949, 1951, 1955),
  value = c(60000, 45000, 37000, 18000, 18000, 18000, 18000, 18000, 18000, 18000, 18000),
  dataset = "Extended"
)

# Combine
combined <- bind_rows(standard, extended)

# Create plot
# Compare timeline
ggplot(combined, aes(xmin = start, xmax = end, ymin = 0, ymax = value/1000, fill = factor(value))) +
  geom_rect(color = "black", linewidth = 0.3) +
  facet_wrap(~dataset, ncol = 1) +
  scale_fill_manual(values = c("60000" = "#d73027", "45000" = "#fc8d59", 
                               "37000" = "#fee090", "27000" = "#e0f3f8",
                               "24000" = "#91bfdb", "18000" = "#4575b4"),
                    name = "Threshold\n(cfs)") +
  labs(x = "Index", y = "Threshold (thousand cfs)",
       title = "Comparison of Non-Exceedance Bound Datasets",
       subtitle = "Extended dataset consolidates upper thresholds into longer segments") +
  theme(panel.grid.minor = element_blank())

# compare magnitude
ggplot(combined, aes(xmin = start, xmax = end, ymin = 0, ymax = value/1000, fill = factor(value))) +
  geom_rect(color = "black", linewidth = 0.3) +
  facet_wrap(~dataset, ncol = 2) +
  scale_fill_manual(values = c("60000" = "#d73027", "45000" = "#fc8d59", 
                               "37000" = "#fee090", "27000" = "#e0f3f8",
                               "24000" = "#91bfdb", "18000" = "#4575b4"),
                    name = "Threshold\n(cfs)") +
  labs(x = "Index", y = "Threshold (thousand cfs)",
       title = "Comparison of Non-Exceedance Bound Datasets",
       subtitle = "Extended dataset consolidates upper thresholds into longer segments") +
  theme(panel.grid.minor = element_blank())

# Overlaid
ggplot(combined, aes(xmin = start, xmax = end, ymin = 0, ymax = value/1000)) +
  geom_rect(aes(color = dataset), fill = NA, linewidth = 0.5) +
  labs(x = "Index", y = "Threshold (thousand cfs)",
       title = "Comparison of Non-Exceedance Bound Datasets",
       subtitle = "Extended dataset consolidates upper thresholds into longer segments")

  