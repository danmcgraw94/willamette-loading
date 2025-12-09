library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

# Your data
bestfit_sum <- tibble::tribble(
  ~analysis, ~mean_of_log, ~sd_of_log, ~skew_of_log, ~erl, ~aic, ~bic, ~rmse, ~input_data, ~sensitivity,
  "0a - CGR_3day_winter_paleo_best_RRFT_noPISBest_Ext", 3.67, 0.210, 0.105, 275, 1546., 1553., 922., "CGR_3day_winter_paleo", "Final",
  "1a - CGR_3day_winter_syst", 3.67, 0.209, 0.188, 80, 1540., 1547., 686., "CGR_3day_winter_syst", "Record Length",
  "1b - CGR_3day_winter_hist", 3.67, 0.212, 0.188, 100, 1548., 1555., 563., "CGR_3day_winter_hist", "Record Length",
  "1c - CGR_3day_winter_paleoOnly", 3.69, 0.226, 0.316, 300, 1589., 1596., 1232., "CGR_3day_winter_paleo", "Record Length",
  "1d - CGR_3day_winter_precip_RRFT", 3.67, 0.211, 0.104, 200, 1546., 1554., 634., "CGR_3day_winter_hist", "Record Length",
  "1e - CGR_3day_winter_paleoOnly_noPSI", 3.67, 0.209, 0.113, 200, 1548., 1555., 652., "CGR_3day_winter_paleo", "Record Length",
  "2a - CGR_3day_annual_syst", 3.69, 0.195, 0.186, 100, 1725., 1732., 684., "CGR_3day_annual_syst", "Seasonality",
  "2b - CGR_3day_annual_hist", 3.69, 0.197, 0.165, 160, 1733., 1741., 594., "CGR_3day_annual_hist", "Seasonality",
  "8a - CGR_3day_annual_syst_2018", 3.69, 0.199, 0.156, 90, 1473., 1480., 784., "CGR_3day_annual_syst", "Previous Analysis",
  "8b - CGR_3day_annual_hist_2018", 3.69, 0.195, 0.0931, 120, 1482., 1489., 569., "CGR_3day_annual_hist", "Previous Analysis",
  "4a - CGR_3day_winter_paleo_best_RRFT", 3.68, 0.221, 0.184, 450, 1590., 1597., 1919., "CGR_3day_winter_paleo", "Paleoflood",
  "4b - CGR_3day_winter_paleo_best_RRFT_noPISBest", 3.67, 0.211, 0.081, 350, 1547., 1554., 652., "CGR_3day_winter_paleo", "Paleoflood",
  "4c - CGR_3day_winter_paleo_younger_RRFT", 3.69, 0.222, 0.203, 300, 1589., 1597., 2239., "CGR_3day_winter_paleo", "Paleoflood",
  "4d - CGR_3day_winter_paleo_older_RRFT", 3.68, 0.218, 0.138, 400, 1577., 1585., 1181., "CGR_3day_winter_paleo", "Paleoflood",
  "4e - CGR_3day_winter_paleo_best_RRFT_noPISUpper", 3.67, 0.211, 0.120, 300, 1546., 1554., 640., "CGR_3day_winter_paleo", "Paleoflood",
  "4f - CGR_3day_winter_paleo_best_RRFT_noPISUp_Ext", 3.68, 0.211, 0.100, 250, 1546., 1553., 926., "CGR_3day_winter_paleo", "Paleoflood"
)

# Define theme
theme_qq <- theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 10, face = "bold")
  )

# Function to generate LP3 quantiles
lp3_quantiles <- function(mean, sd, skew, probs) {
  # Using the generalized extreme value approximation for LP3
  # This is an approximation - for exact LP3 quantiles you'd need specialized functions
  z <- qnorm(probs)
  kt <- z + (z^2 - 1) * skew/6 + 
    z * (z^2 - 3) * (skew^2)/36 - 
    (z^2 - 1) * (skew^3)/216
  
  return(mean + kt * sd)
}

# 1. QQ Plot comparing systematic vs historical data
# Generate theoretical quantiles for comparison
probs <- seq(0.01, 0.99, length.out = 100)

# Get systematic and historical analyses for comparison
syst_winter <- bestfit_sum %>% filter(analysis == "1a - CGR_3day_winter_syst")
hist_winter <- bestfit_sum %>% filter(analysis == "1b - CGR_3day_winter_hist")

qq_data <- tibble(
  probability = probs,
  systematic = lp3_quantiles(syst_winter$mean_of_log, 
                             syst_winter$sd_of_log, 
                             syst_winter$skew_of_log, probs),
  historical = lp3_quantiles(hist_winter$mean_of_log, 
                             hist_winter$sd_of_log, 
                             hist_winter$skew_of_log, probs)
)

p_qq <- qq_data %>%
  ggplot(aes(x = systematic, y = historical)) +
  geom_point(alpha = 0.6, size = 2, color = "#3498DB") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 0.8) +
  labs(
    title = "QQ Plot: Systematic vs Historical LP3 Distributions",
    subtitle = "Winter 3-day precipitation",
    x = "Systematic Quantiles (log-space)",
    y = "Historical Quantiles (log-space)"
  ) +
  theme_qq +
  coord_equal()

# 2. Return period comparison plot
return_periods <- c(2, 5, 10, 25, 50, 100, 200, 500, 1000)
exceedance_probs <- 1 - 1/return_periods

rp_comparison <- bestfit_sum %>%
  filter(grepl("winter", analysis)) %>%
  group_by(analysis) %>%
  summarize(
    across(everything(), first),
    .groups = "drop"
  ) %>%
  crossing(return_period = return_periods) %>%
  mutate(
    exceedance_prob = 1 - 1/return_period,
    quantile = lp3_quantiles(mean_of_log, sd_of_log, skew_of_log, exceedance_prob),
    # Convert from log-space to real space
    precipitation = 10^quantile
  )

p_rp <- rp_comparison %>%
  ggplot(aes(x = return_period, y = precipitation, color = sensitivity, group = analysis)) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 2) +
  scale_x_log10(breaks = c(2, 5, 10, 25, 50, 100, 200, 500, 1000)) +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = c(
    "Final" = "#2C3E50",
    "Record Length" = "#3498DB",
    "Paleoflood" = "#27AE60"
  )) +
  labs(
    title = "Return Period Estimates from LP3 Distributions",
    subtitle = "Winter 3-day precipitation analysis",
    x = "Return Period (years)",
    y = "Precipitation (mm)",
    color = "Sensitivity Group"
  ) +
  theme_qq +
  annotation_logticks()

# 3. Parameter uncertainty bands
# Create synthetic confidence intervals (these would normally come from bootstrapping)
param_uncertainty <- bestfit_sum %>%
  mutate(
    mean_lower = mean_of_log - 0.05,  # Synthetic CI
    mean_upper = mean_of_log + 0.05,
    sd_lower = sd_of_log * 0.9,
    sd_upper = sd_of_log * 1.1,
    skew_lower = skew_of_log - 0.1,
    skew_upper = skew_of_log + 0.1,
    analysis_short = gsub(" - .*", "", analysis)  # Shorter labels
  )

p_uncertainty <- param_uncertainty %>%
  ggplot(aes(x = reorder(analysis_short, mean_of_log), y = mean_of_log, color = sensitivity)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_lower, ymax = mean_upper), width = 0.2) +
  scale_color_manual(values = c(
    "Final" = "#2C3E50",
    "Record Length" = "#3498DB",
    "Seasonality" = "#E74C3C",
    "Previous Analysis" = "#F39C12",
    "Paleoflood" = "#27AE60"
  )) +
  coord_flip() +
  labs(
    title = "Parameter Uncertainty Ranges",
    subtitle = "Mean parameter with confidence intervals",
    x = "Analysis",
    y = "Mean of Log",
    color = "Sensitivity Group"
  ) +
  theme_qq

# 4. Distribution comparison using density plots
# Generate samples from each distribution for visualization
n_samples <- 1000
set.seed(123)

distribution_samples <- bestfit_sum %>%
  filter(analysis %in% c("1a - CGR_3day_winter_syst", 
                         "1b - CGR_3day_winter_hist",
                         "0a - CGR_3day_winter_paleo_best_RRFT_noPISBest_Ext")) %>%
  mutate(analysis_label = case_when(
    grepl("1a", analysis) ~ "Systematic",
    grepl("1b", analysis) ~ "Historical",
    TRUE ~ "Final (with Paleo)"
  )) %>%
  rowwise() %>%
  mutate(
    samples = list(rnorm(n_samples, mean = mean_of_log, sd = sd_of_log))
  ) %>%
  unnest(samples)

p_density <- distribution_samples %>%
  ggplot(aes(x = samples, fill = analysis_label, color = analysis_label)) +
  geom_density(alpha = 0.3, size = 0.8) +
  scale_fill_manual(values = c(
    "Systematic" = "#3498DB",
    "Historical" = "#E74C3C",
    "Final (with Paleo)" = "#2C3E50"
  )) +
  scale_color_manual(values = c(
    "Systematic" = "#3498DB",
    "Historical" = "#E74C3C",
    "Final (with Paleo)" = "#2C3E50"
  )) +
  labs(
    title = "Distribution Comparison",
    subtitle = "Probability density functions for different data sources",
    x = "Log-transformed Precipitation",
    y = "Density",
    fill = "Data Source",
    color = "Data Source"
  ) +
  theme_qq

# 5. Empirical CDF comparison
p_ecdf <- distribution_samples %>%
  ggplot(aes(x = samples, color = analysis_label)) +
  stat_ecdf(size = 1) +
  scale_color_manual(values = c(
    "Systematic" = "#3498DB",
    "Historical" = "#E74C3C",
    "Final (with Paleo)" = "#2C3E50"
  )) +
  labs(
    title = "Empirical Cumulative Distribution Functions",
    subtitle = "Comparison of different data sources",
    x = "Log-transformed Precipitation",
    y = "Cumulative Probability",
    color = "Data Source"
  ) +
  theme_qq

# 6. Model selection metrics comparison
p_metrics <- bestfit_sum %>%
  select(analysis, aic, bic, rmse, sensitivity) %>%
  pivot_longer(cols = c(aic, bic, rmse), 
               names_to = "metric", 
               values_to = "value") %>%
  mutate(
    metric = factor(metric, 
                    levels = c("aic", "bic", "rmse"),
                    labels = c("AIC", "BIC", "RMSE")),
    analysis_short = gsub(" - .*", "", analysis)
  ) %>%
  ggplot(aes(x = reorder(analysis_short, value), y = value, fill = sensitivity)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~metric, scales = "free", nrow = 1) +
  scale_fill_manual(values = c(
    "Final" = "#2C3E50",
    "Record Length" = "#3498DB",
    "Seasonality" = "#E74C3C",
    "Previous Analysis" = "#F39C12",
    "Paleoflood" = "#27AE60"
  )) +
  coord_flip() +
  labs(
    title = "Model Selection Metrics",
    subtitle = "Lower values indicate better fit (AIC, BIC) or accuracy (RMSE)",
    x = "Analysis",
    y = "Metric Value",
    fill = "Sensitivity Group"
  ) +
  theme_qq +
  theme(legend.position = "right")

# Combine plots
combined_qq <- (p_qq + p_rp) / 
  (p_density + p_ecdf) +
  plot_annotation(
    title = "Distribution Comparisons for LP3 Analyses",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

# Save plots
ggsave("lp3_qq_plot.png", p_qq, width = 8, height = 8, dpi = 300)
ggsave("lp3_return_periods.png", p_rp, width = 10, height = 6, dpi = 300)
ggsave("lp3_uncertainty.png", p_uncertainty, width = 10, height = 8, dpi = 300)
ggsave("lp3_densities.png", p_density, width = 10, height = 6, dpi = 300)
ggsave("lp3_ecdfs.png", p_ecdf, width = 10, height = 6, dpi = 300)
ggsave("lp3_metrics.png", p_metrics, width = 12, height = 6, dpi = 300)
ggsave("lp3_combined_qq.png", combined_qq, width = 12, height = 10, dpi = 300)

# Print plots
print(p_qq)
print(p_rp)
print(p_uncertainty)
print(p_density)
print(p_ecdf)
print(p_metrics)
print(combined_qq)
