# Cant get macros unblocked. This is my workaround for now
# Gave VBA script to Claude and had it send back something I could use

# Log-Pearson Type III Uncertainty Simulation in R
# Translation of VBA LP3_Uncertainty_Simulation macro

library(tidyverse)
library(moments)

# LP3 Distribution Functions -----------------------------------------------

# LP3 CDF (equivalent to LP3_DIST in VBA)
lp3_cdf <- function(x, mean_log, sd_log, skew_log) {
  # Convert to log space
  log_x <- log10(x)
  
  # Wilson-Hilferty transformation for skewed normal
  if (abs(skew_log) > 0.01) {
    k <- skew_log
    t <- (log_x - mean_log) / sd_log
    z <- (2/k) * ((1 + k*t/2)^3 - 1)
  } else {
    z <- (log_x - mean_log) / sd_log
  }
  
  return(pnorm(z))
}

# LP3 Inverse CDF (equivalent to LP3_INV in VBA)
lp3_inv <- function(p, mean_log, sd_log, skew_log) {
  # Get standard normal quantile
  z <- qnorm(p)
  
  # Wilson-Hilferty inverse transformation
  if (abs(skew_log) > 0.01) {
    k <- skew_log
    t <- (2/k) * (((k*z/2) + 1)^(1/3) - 1) + (k/6)
    log_x <- mean_log + t * sd_log
  } else {
    log_x <- mean_log + z * sd_log
  }
  
  return(log_x)  # Returns in log space
}

# Significant Figures Rounding
sigfig_round <- function(x, n) {
  ifelse(x == 0, 0, signif(x, n))
}
# Main Simulation Function -------------------------------------------------

lp3_uncertainty_simulation <- function(
    mean_log,           # Mean of logarithms
    sd_log,             # Standard deviation of logarithms
    skew_log,           # Skew of logarithms
    erl,                # Effective Record Length
    num_realizations = 10000,  # Number of bootstrap realizations
    ci = 0.90,          # Confidence interval (e.g., 0.90 for 90%)
    num_sigfigs = 4,    # Number of significant figures for output
    seed = 123456       # Random seed (set to NULL for random)
) {
  
  # Set random seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Define AEP values to analyze
  aep_values <- c(
    0.5, 0.2, 0.1, 0.04, 0.02, 0.01, 0.005, 0.002, 0.001,
    0.0005, 0.0002, 0.0001, 0.00005, 0.00002, 0.00001,
    0.000005, 0.000002, 0.000001, 0.0000005, 0.0000002,
    0.0000001, 0.00000005, 0.00000002, 0.00000001,
    0.000000005, 0.000000002, 0.000000001, 0.0000000005,
    0.0000000002, 0.0000000001, 0.00000000005, 0.00000000001
  )
  
  n_aep <- length(aep_values)
  
  # Calculate confidence limits
  upper_cl <- 1 - (1 - ci) / 2
  lower_cl <- (1 - ci) / 2
  
  # Initialize storage arrays
  x_sample <- matrix(NA, nrow = num_realizations, ncol = n_aep)
  mean_samples <- numeric(num_realizations)
  sd_samples <- numeric(num_realizations)
  skew_samples <- numeric(num_realizations)
  
  # Bootstrap realizations
  cat("Running simulation...\n")
  pb <- txtProgressBar(min = 0, max = num_realizations, style = 3)
  
  for (i in 1:num_realizations) {
    # Generate bootstrap sample in log space
    probs <- runif(erl)
    x_boot <- sapply(probs, function(p) lp3_inv(p, mean_log, sd_log, skew_log))
    
    # Calculate sample parameters (in log space)
    mean_samples[i] <- mean(x_boot)
    sd_samples[i] <- sd(x_boot)
    skew_samples[i] <- skewness(x_boot)
    
    # Calculate X for each AEP value
    for (j in 1:n_aep) {
      x_sample[i, j] <- lp3_inv(1 - aep_values[j], 
                                mean_samples[i], 
                                sd_samples[i], 
                                skew_samples[i])
    }
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Post-processing: Calculate curves
  cat("\nPost-processing results...\n")
  
  # Calculate quantile curves
  q_upper <- apply(x_sample, 2, quantile, probs = upper_cl, na.rm = TRUE)
  q_lower <- apply(x_sample, 2, quantile, probs = lower_cl, na.rm = TRUE)
  q_median <- apply(x_sample, 2, quantile, probs = 0.5, na.rm = TRUE)
  
  # Computed curve (from original parameters)
  q_computed <- sapply(aep_values, function(aep) {
    lp3_inv(1 - aep, mean_log, sd_log, skew_log)
  })
  
  # Expected curve calculation
  x_min <- min(x_sample, na.rm = TRUE)
  x_max <- max(x_sample, na.rm = TRUE)
  num_x <- 100
  x_values <- seq(x_min, x_max, length.out = num_x)
  
  # Calculate AEP for each realization at each X value
  aep_sample <- matrix(NA, nrow = num_realizations, ncol = num_x)
  for (i in 1:num_realizations) {
    for (j in 1:num_x) {
      aep_sample[i, j] <- 1 - lp3_cdf(10^x_values[j], 
                                      10^mean_samples[i], 
                                      sd_samples[i], 
                                      skew_samples[i])
    }
  }
  
  # Calculate expected AEP for each X value
  aep_expected <- colMeans(aep_sample, na.rm = TRUE)
  
  # Interpolate to get expected curve
  q_expected <- numeric(n_aep)
  for (i in 1:n_aep) {
    if (aep_values[i] > max(aep_expected)) {
      q_expected[i] <- x_values[1]
    } else if (aep_values[i] < min(aep_expected)) {
      q_expected[i] <- x_max
    } else {
      # Linear interpolation in normal quantile space
      x_norm <- qnorm(aep_values[i])
      x_norm_expected <- qnorm(aep_expected)
      q_expected[i] <- approx(x_norm_expected, x_values, xout = x_norm)$y
    }
  }
  
  # Convert from log space to real space and round
  results <- tibble(
    aep = aep_values,
    return_period = 1 / aep_values,
    upper = sigfig_round(10^q_upper, num_sigfigs),
    expected = sigfig_round(10^q_expected, num_sigfigs),
    computed = sigfig_round(10^q_computed, num_sigfigs),
    median = sigfig_round(10^q_median, num_sigfigs),
    lower = sigfig_round(10^q_lower, num_sigfigs)
  )
  
  return(list(
    results = results,
    parameters = list(
      mean_log = mean_log,
      sd_log = sd_log,
      skew_log = skew_log,
      erl = erl,
      ci = ci,
      num_realizations = num_realizations
    ),
    bootstrap_samples = list(
      mean = mean_samples,
      sd = sd_samples,
      skew = skew_samples
    )
  ))
}

# Example usage ------------------------------------------------------------

# Run simulation with your parameters
results_paleo_old <- lp3_uncertainty_simulation(
  mean_log = 3.68174,      # Replace with your mean of logs
  sd_log = 0.21814,       # Replace with your SD of logs
  skew_log = 0.13765,     # Replace with your skew of logs
  erl = 200,           # Replace with your effective record length
  num_realizations = 10000,
  ci = 0.90,
  num_sigfigs = 4,
  seed = 123456        # Use NULL for random seed
)

results_paleo_young <- lp3_uncertainty_simulation(
  mean_log = 3.68728,      # Replace with your mean of logs
  sd_log = 0.22213,       # Replace with your SD of logs
  skew_log = 0.20343,     # Replace with your skew of logs
  erl = 350,           # Replace with your effective record length
  num_realizations = 10000,
  ci = 0.90,
  num_sigfigs = 4,
  seed = 123456        # Use NULL for random seed
)

# View results
print(results_paleo_old$results)
print(results_paleo_young$results)

# # Export to CSV
# write_csv(results$results, "lp3_results.csv")

# Plot results
ggplot(results_paleo_old$results %>% filter(aep >= 0.00001, aep <= 0.5)) +
  geom_ribbon(aes(x = qnorm(1 - aep), ymin = lower, ymax = upper),
              fill = "lightblue", alpha = 0.5) +
  geom_line(aes(x = qnorm(1 - aep), y = computed), linewidth = 1) +
  geom_line(aes(x = qnorm(1 - aep), y = expected), 
            color = "red", linewidth = 0.8, linetype = "dashed") +
  scale_y_log10() +
  scale_x_continuous(
    breaks = qnorm(1 - c(0.5, 0.2, 0.1, 0.04, 0.02, 0.01, 0.002, 0.0001)),
    labels = c("0.5", "0.2", "0.1", "0.04", "0.02", "0.01", "0.002", "0.0001")
  ) +
  labs(
    x = "Annual Exceedance Probability",
    y = "Flow (log scale)",
    title = "Log-Pearson Type III Frequency Curve with Uncertainty"
  ) +
  theme_bw()
