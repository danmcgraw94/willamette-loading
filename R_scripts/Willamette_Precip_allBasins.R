# =============================================================================
# MSP Bootstrap Precipitation Frequency Analysis
# Willamette Loading Study - 2025
# =============================================================================
# Modernized script for computing Estimated Record Lengths (ERLs) using 
# L-moment bisection and GEV bootstrap confidence intervals
# =============================================================================

# Setup ------------------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, scales, ggh4x, ggrepel, patchwork, fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise, lubridate, purrr, zoo, RColorBrewer
)

# Hosking L-Moments library - load AFTER lmomco for precedence ---
library(lmomco)
library(lmom)

# Parallel processing --- 
plan(multisession)

# Directories and settings ---
theme_set(theme_bw())
today <- format(Sys.Date(), "%d-%b-%Y")

plotdir <- file.path(getwd(), "outputs", "Figures", "PF", "MSP_Bootstrap")
outdir <- file.path(getwd(), "outputs", "PF", "MSP_Bootstrap")
dir_create(plotdir)
dir_create(outdir)

# AEPs for computing curves and confidence intervals ---
AEPs <- c(1e-8, 1e-7, 1e-06, 2e-06, 5e-06, 1e-05, 2e-05, 5e-05, 1e-04,
          2e-04, 5e-04, 1e-03, 2e-03, 5e-03, 0.01, 0.02, 0.04, 0.05, 
          0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99)

# AEPs for ERL bisection ---
selected_AEPs <- c(1e-01, 1e-02, 1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08, 1e-09)

# Confidence interval percentiles ---
CIs <- c(0.05, 0.95)

# Site definitions -------------------------------------------------------------
sites <- tribble(~code, ~name,
                 "BLU", "Blue River",
                 "CGR", "Cougar",
                 "FCK", "Fall Creek",
                 "FOS", "Foster",
                 "GPR", "Green Peter",
                 "HCK", "Hills Creek",
                 "LOP", "Lookout Point")

# Helper functions -------------------------------------------------------------

#' Process MSP precipitation frequency data for a single site
#' @param code Site code (e.g., "BLU")
#' @param name Site name (e.g., "Blue River")
#' @return Tibble with processed precipitation frequency data
process_site <- function(code, name) {
  pattern <- glue("*{code}_PrecipFreq_OCT27_2022.csv*")
  
  dir_ls("data/MSP_PF/", glob = pattern, recurse = TRUE) %>%
    read_csv(show_col_types = FALSE) %>% 
    arrange(desc(Z)) %>%
    mutate(
      site_name = name,
      AEP = pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE), 
      ANEP = 1 - pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE),,
      Gumbel = -log(-log(ANEP)),
      .before = everything()
    )
}

#' Bootstrap GEV confidence interval width at specified AEP
#' @param erl Effective record length for resampling
#' @param realz Number of bootstrap realizations
#' @param seed Random seed for reproducibility
#' @param parms GEV parameters (xi, alpha, kappa)
#' @param aep Annual exceedance probability
#' @return Width of 90% confidence interval (95th - 5th percentile)
bootstrap_gev_ci <- function(erl, realz, seed, parms, aep) {
  set.seed(seed)
  
  boot_quantiles <- map_dbl(seq_len(realz), ~ {
    boot_sample <- quagev(runif(n = erl), parms)
    lmom <- samlmu(boot_sample)
    boot_parms <- pelgev(lmom)
    quagev(f = 1 - aep, para = boot_parms)
  })
  
  quantile(boot_quantiles, 0.95) - quantile(boot_quantiles, 0.05)
}

#' Bisection method to find ERL that matches MSP confidence interval width
#' @param target_width Target CI width from MSP data
#' @param parms GEV parameters
#' @param aep Annual exceedance probability
#' @param lower_bound Lower bound for bisection (default 50)
#' @param upper_bound Upper bound for bisection (default 1000)
#' @param tol Convergence tolerance (default 0.1)
#' @param max_iter Maximum iterations (default 100)
#' @return Estimated record length
bisect_erl <- function(target_width, parms, aep, 
                       lower_bound = 50, upper_bound = 1000, 
                       tol = 0.1, max_iter = 100) {
  a <- lower_bound
  b <- upper_bound
  
  for (i in seq_len(max_iter)) {
    c <- 0.5 * (a + b)
    fc <- bootstrap_gev_ci(erl = c, realz = 1000, seed = 12345, 
                           parms = parms, aep = aep) - target_width
    
    if (abs(fc) <= tol) break
    
    fa <- bootstrap_gev_ci(erl = a, realz = 1000, seed = 12345, 
                           parms = parms, aep = aep) - target_width
    
    if (sign(fc) == sign(fa)) {
      a <- c
    } else {
      b <- c
    }
  }
  round(c, 0)
}

#' Compute ERLs for a single site across multiple AEPs
#' #' Uses interpolation to get MSP CI widths at exact target AEPs
#' @param site_data Tibble with site precipitation data
#' @param site_name Name of site
#' @param gev_parms GEV parameters for site
#' @param aeps Vector of AEPs for ERL computation
#' @return Tibble with ERLs for each AEP
compute_site_erls <- function(site_data, site_name, gev_parms, target_aeps) {
  
  cli_progress_step("Computing ERLs for {site_name}")
  
  
  # Interpolate MSP upper and lower bounds at target AEPs
  # (site_data is already sorted by descending Z / ascending AEP)
  upper_interp <- approx(x = site_data$AEP, y = site_data$U95.600, 
                         xout = target_aeps, rule = 2)
  lower_interp <- approx(x = site_data$AEP, y = site_data$L95.600, 
                         xout = target_aeps, rule = 2)
  
  # Target CI widths at each AEP
  target_widths <- tibble(
    AEP = target_aeps,
    U95 = upper_interp$y,
    L95 = lower_interp$y,
    CI_width = U95 - L95
  )
  
  # Compute ERL for each target AEP
  
  map_dfr(seq_len(nrow(target_widths)), function(i) {
    aep <- target_widths$AEP[i]
    width <- target_widths$CI_width[i]
    
    erl <- bisect_erl(
      target_width = width,
      parms = gev_parms,
      aep = aep
    )
    
    tibble(
      site_name = site_name,
      AEP_used = aep,
      target_CI_width = width,
      ERL = erl
    )
  })
}

#' Run full bootstrap analysis for a site with given ERL
#' @param site_data Tibble with site precipitation data
#' @param site_name Name of site
#' @param gev_parms GEV parameters
#' @param erl Effective record length
#' @param aep_for_erl AEP used to derive ERL (for labeling)
#' @param aeps Vector of AEPs for output curve
#' @param realz Number of bootstrap realizations
#' @return Tibble with bootstrap results
run_bootstrap <- function(site_data, site_name, gev_parms, erl, aep_for_erl, 
                          aeps = AEPs, realz = 1000) {
  set.seed(12345)
  
  # Bootstrap parameter and quantile storage
  boot_params <- matrix(nrow = realz, ncol = 3)
  boot_quantiles <- matrix(nrow = realz, ncol = length(aeps))
  
  max_x <- -Inf
  min_x <- Inf
  
  # Generate bootstrap samples
  for (i in seq_len(realz)) {
    boot_sample <- quagev(runif(n = erl), gev_parms)
    lmom <- samlmu(boot_sample)
    parms <- pelgev(lmom)
    boot_params[i, ] <- parms
    
    for (j in seq_along(aeps)) {
      x_val <- quagev(f = 1 - aeps[j], parms)
      boot_quantiles[i, j] <- x_val
      max_x <- max(max_x, x_val)
      min_x <- min(min_x, x_val)
    }
  }
  
  # Compute confidence intervals
  boot_cis <- map_dfc(seq_along(aeps), function(j) {
    tibble(!!paste0("aep_", j) := quantile(boot_quantiles[, j], probs = c(0.05, 0.95)))
  })
  
  # Expected probability curve
  bins <- 100
  boot_exp <- tibble(
    x = 10^seq(log10(min_x), log10(max_x), length.out = bins)
  )
  
  boot_exp <- boot_exp %>%
    mutate(
      AEP = map_dbl(x, function(x_val) {
        p_vals <- map_dbl(seq_len(realz), function(j) {
          cdfgev(x = x_val, para = c(boot_params[j, 1], boot_params[j, 2], boot_params[j, 3]))
        })
        1 - mean(p_vals)
      })
    )
  
  # Interpolate expected curve at target AEPs
  expected_curve <- approx(x = boot_exp$AEP, y = boot_exp$x, xout = aeps)
  
  tibble(
    AEP = expected_curve$x,
    ExpectedCurve = expected_curve$y,
    LowerLimit = as.numeric(boot_cis[1, ]),
    UpperLimit = as.numeric(boot_cis[2, ]),
    Gumbel_ANEP = -log(-log(1 - AEP)),
    Group = glue("ERL = {erl} from AEP = {aep_for_erl}"),
    ERL = erl,
    site_name = site_name
  )
}

# Load and process data --------------------------------------------------------
cli_h1("Loading MSP Precipitation Frequency Data")

all_sites <- map2_dfr(sites$code, sites$name, process_site)
cgr <- all_sites %>% filter(site_name == "Cougar")

cli_alert_success("Loaded {nrow(all_sites)} records across {n_distinct(all_sites$site_name)} sites")

# Fit parent GEV distributions -------------------------------------------------
cli_h1("Fitting Parent GEV Distributions")

parent_GEVs <- all_sites %>% 
  group_by(site_name) %>%
  reframe(
    fit = list(lmomco::disfitqua(FittedP.600, ANEP, type = "gev"))
  ) %>%
  mutate(
    GEV_xi = map_dbl(fit, ~ .x$para["xi"]),
    GEV_alpha = map_dbl(fit, ~ .x$para["alpha"]), 
    GEV_kappa = map_dbl(fit, ~ .x$para["kappa"])
  ) %>%
  select(-fit)

cli_alert_success("Fitted GEV parameters for {nrow(parent_GEVs)} sites")

# Compute ERLs for each site ---------------------------------------------------
cli_h1("Computing Estimated Record Lengths via L-Moment Bisection")

site_erls <- parent_GEVs %>%
  left_join(
    all_sites %>% nest(data = -site_name),
    by = "site_name"
  ) %>%
  mutate(
    erls = pmap(
      list(data, site_name, GEV_xi, GEV_alpha, GEV_kappa),
      function(data, site_name, xi, alpha, kappa) {
        compute_site_erls(
          site_data = data,
          site_name = site_name,
          gev_parms = c(xi, alpha, kappa),
          target_aeps = selected_AEPs
        )
      }
    )
  )

site_erls_flat <- site_erls %>%
  select(erls) %>%
  unnest(erls)

# Plot ERL results -------------------------------------------------------------
log_brks <- function(base,exps){
  breaks = base^exps
  return(breaks)
}

cli_h1("Generating ERL Sensitivity Plot")

erl_plot <- ggplot(site_erls_flat, aes(x = AEP_used, y = ERL, color = site_name)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(trans = c("log10", "reverse"),
                     breaks = log_brks(10,c(-1,-2,-3,-4,-5,-6,-7,-8)),
                     labels = scales::scientific,
                     minor_breaks = scales::minor_breaks_log(detail = 1))+
  scale_y_continuous(breaks = seq(0, 500, by = 100), 
                     minor_breaks = seq(0, 500, by = 25)) +
  ggsci::scale_color_aaas() +
  labs(title = "ERL for at selected AEPs",
       subtitle = "ERL estimated using L-Moment Bisection",
       x = "AEP Used for Bisection", 
       y = "Estimated Record Length (ERL)",
       color = "Site",
       caption = today) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(file.path(plotdir, "AllSites_ERL_Results.png"), erl_plot, height = pht, width = pwidth, dpi = 500)

cli_alert_success("Saved ERL plot to {plotdir}")

# Run bootstrap for all sites and ERLs -----------------------------------------
cli_h1("Running Bootstrap Analysis")

# Prepare data for bootstrap
bootstrap_inputs <- site_erls_flat %>%
  left_join(parent_GEVs, by = "site_name") %>%
  left_join(all_sites %>% nest(data = -site_name),by = "site_name")

# Run bootstrap for each site-ERL combination
all_bootstrap_results <- bootstrap_inputs %>%
  mutate(
    boot_results = pmap(
      list(data, site_name, GEV_xi, GEV_alpha, GEV_kappa, ERL, AEP_used),
      function(data, site_name, xi, alpha, kappa, erl, aep_used) {
        cli_progress_step("Bootstrap: {site_name}, ERL = {erl}")
        run_bootstrap(
          site_data = data,
          site_name = site_name,
          gev_parms = c(xi, alpha, kappa),
          erl = erl,
          aep_for_erl = aep_used)
      },
      .progress = TRUE
    )
  ) #%>%
  # select(site_name, boot_results) %>%
  # unnest(boot_results)

# Extract Boot Results
all_bootstraps <- all_bootstrap_results %>% 
  select(boot_results) %>% 
  unnest(boot_results) %>% 
  mutate(ANEP = 1-AEP,.before = ExpectedCurve) %>% 
  relocate(Gumbel_ANEP,.before = ExpectedCurve) %>% 
  select(-Group) %>% 
  mutate(Z = qnorm(AEP, lower.tail = TRUE),.before = everything()) #%>% 
  #mutate(Data = "GEV")

# Add MSP parent curve for comparison ------------------------------------------
cli_h1("Adding MSP Parent Curves")
bootstrap_AEP <- unique(all_bootstraps$AEP)
bootstrap_Z <- unique(all_bootstraps$Z)

MSP_all_sites <- all_sites %>%
  select(site_name, AEP, ANEP, Gumbel, Z,FittedP.450, FittedP.600, L95.600, U95.600) %>% 
  rename(Gumbel_ANEP = Gumbel,
         MSP_expected = FittedP.600,
         MSP_LL = L95.600,
         MSP_UL = U95.600) %>% 
  group_by(site_name) %>% 
  reframe(bootstrap_Z = bootstrap_Z,
          MSP_approx_expected = approx(Z, MSP_expected, xout = bootstrap_Z)$y,
          MSP_approx_UL = approx(Z, MSP_UL, xout = bootstrap_Z)$y,
          MSP_approx_LL = approx(Z, MSP_LL, xout = bootstrap_Z)$y) %>% 
  # mutate(MSP_spline_expected = na.spline(MSP_approx_expected, x = bootstrap_Z, na.rm = FALSE),
  #        MSP_spline_UL = na.spline(MSP_approx_UL, x = bootstrap_Z, na.rm = FALSE),
  #         MSP_spline_LL = na.spline(MSP_approx_LL, x = bootstrap_Z, na.rm = FALSE)) %>% 
  rename(Z = bootstrap_Z)


# # msp_curves <- 
# all_sites %>%
#   group_by(site_name) %>%
#   reframe(
#     msp = list(approx(x = AEP, y = FittedP.600, xout = AEPs)),
#     msp_ul = list(approx(x = AEP, y = U95.600, xout = AEPs)),
#     msp_ll = list(approx(x = AEP, y = L95.600, xout = AEPs))
#   ) %>%
#   mutate(
#     msp_df = pmap(list(msp, msp_ll, msp_ul, site_name), function(m, ll, ul, sn) {
#       tibble(
#         AEP = m$x,
#         ExpectedCurve = m$y,
#         LowerLimit = ll$y,
#         UpperLimit = ul$y,
#         Gumbel_ANEP = -log(-log(1 - AEP)),
#         Group = "Parent GEV",
#         ERL = NA_real_,
#         site_name = sn
#       )
#     })
#   ) %>%
#   select(msp_df) %>%
#   unnest(msp_df)

# Combine bootstrap and MSP results
#all_results <- bind_rows(all_bootstraps, MSP_all_sites)
all_results <- left_join(all_bootstraps,MSP_all_sites, join_by(site_name,Z))

# remove 9.9E-1 NAs ---
all_results <- all_results %>% filter(!is.na(MSP_approx_expected))

# Compute comparison metrics ---------------------------------------------------
cli_h1("Computing Comparison Metrics")

all_results <- all_results %>% 
  mutate(UL_resid = MSP_approx_UL - UpperLimit,
         Exp_resid = MSP_approx_expected - ExpectedCurve,
         UL_PerDiff = UL_resid/MSP_approx_UL,
         Exp_PerDiff = Exp_resid/MSP_approx_expected,
         UL_sq_error = UL_resid^2,
         Exp_sq_error = Exp_resid^2)

n_data <- length(all_results$UL_sq_error)

# Summary statistics
summary_results <- all_results %>%
  group_by(site_name, ERL) %>%
  summarize(
    Mean_UL_resid = max(UL_resid),
    Mean_Exp_resid = max(Exp_resid),
    Mean_UL_PerDiff = mean(UL_PerDiff),
    Mean_Exp_PerDiff = mean(Exp_PerDiff),
    SSE_UL = sum(UL_sq_error),
    SSE_Exp = sum(Exp_sq_error),
    MSE_UL = SSE_UL/n_data,
    MSE_Exp = SSE_Exp/n_data,
    RMSE_UL = (MSE_UL)^0.5,
    RMSE_Exp = (MSE_Exp)^0.5,
    .groups = "drop")

# Residuals ----------------------
resid_UL_plot <- ggplot(summary_results, aes(x = ERL, y = Mean_UL_resid, color = site_name)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 500, by = 100), 
                     minor_breaks = seq(0, 500, by = 10)) +
  ggsci::scale_color_aaas() +
  labs(title = "Mean of Residuals - Upper Limits",
       subtitle = "mean(MSP UL - GEV UL)",
       x = "ERL", 
       y = expression(paste(Delta, " Precip(in.)")),
       color = "Site",
       caption = today) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(plotdir,"UL_Residuals.png"), resid_UL_plot, height = 6, width = 8, dpi = 500)

resid_exp_plot <- ggplot(summary_results, aes(x = ERL, y = Mean_Exp_resid, color = site_name)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 500, by = 100), 
                     minor_breaks = seq(0, 500, by = 10)) +
  ggsci::scale_color_aaas() +
  labs(title = "Mean of Residuals - Expected",
       subtitle = "mean(MSP Expect - GEV Expect)",
       x = "ERL", 
       y = expression(paste(Delta, " Precip(in.)")),
       color = "Site",
       caption = today) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(plotdir,"Exp_Residuals.png"), resid_exp_plot, height = 6, width = 8, dpi = 500)

# SSE plot
sse_UL <- ggplot(summary_results, aes(x = ERL, y = SSE_UL, color = site_name)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 500, by = 100), 
                     minor_breaks = seq(0, 500, by = 10)) +
  ggsci::scale_color_aaas() +
  labs(title = "Sum of Square Errors - Upper Limits",
       subtitle = "Residuals  = MSP UL - GEV UL",
       x = "ERL", 
       y = "SSE",
       color = "Site",
       caption = today) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(plotdir,"SSE_UL.png"), sse_UL, height = 6, width = 8, dpi = 500)

sse_EXP <- ggplot(summary_results, aes(x = ERL, y = SSE_Exp, color = site_name)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 500, by = 100), 
                     minor_breaks = seq(0, 500, by = 10)) +
  ggsci::scale_color_aaas() +
  labs(title = "Sum of Square Errors - Expected",
       subtitle = "Residuals  = MSP Exp - GEV Exp",
       x = "ERL", 
       y = "SSE",
       color = "Site",
       caption = today) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(plotdir,"SSE_Exp.png"), sse_EXP, height = 6, width = 8, dpi = 500)

# MSE ----
mse_UL <- ggplot(summary_results, aes(x = ERL, y = MSE_UL, color = site_name)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 500, by = 100), 
                     minor_breaks = seq(0, 500, by = 10)) +
  ggsci::scale_color_aaas() +
  labs(title = "Mean Square Error - Upper Limits",
       subtitle = "Residuals  = MSP UL - GEV UL",
       x = "ERL", 
       y = "MSE",
       color = "Site",
       caption = today) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(plotdir,"MSE_UL.png"), mse_UL, height = 6, width = 8, dpi = 500)

sse_EXP <- ggplot(summary_results, aes(x = ERL, y = MSE_Exp, color = site_name)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 500, by = 100), 
                     minor_breaks = seq(0, 500, by = 10)) +
  ggsci::scale_color_aaas() +
  labs(title = "Mean Square Error - Expected",
       subtitle = "Residuals  = MSP Exp - GEV Exp",
       x = "ERL", 
       y = "MSE",
       color = "Site",
       caption = today) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(file.path(plotdir,"MSE_Exp.png"), sse_EXP, height = 6, width = 8, dpi = 500)

# Select optimal ERLs (visually) -----------------------------------------------
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))
Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))
cgr_msp_og <- all_sites %>% filter(site_name == "Cougar")

# Upper
cgr_upper <- all_results %>% 
  mutate(ERL_yr = factor(ERL)) %>% 
  filter(site_name == "Cougar") %>% 
  ggplot() + 
  geom_line(data = cgr_msp_og, aes(x = Gumbel, y = U95.600, linetype = "MSP-Upper"), color = "#1B1919FF",linewidth = 0.7) +
  geom_line(aes(Gumbel_ANEP, UpperLimit, color = ERL_yr), linewidth = 0.4) +
  scale_color_aaas() +
  scale_linetype_manual(values = c("MSP-Upper" = "dashed"),
                        breaks = c("MSP-Upper")) +
  scale_x_continuous(breaks = Gumbel_AEP_breaks,minor_breaks = Gumbel_AEP_breaks_minor,labels = aep_breaks) +
  labs(title = "Cougar Dam - Upper Limit",
       subtitle = "Parent MSP Data Shown",
       x = "AEP", 
       y = "Precip (in.)",
       color = "ERL (yr)",
       linetype = "Parent MSP")

ggsave(file.path(plotdir,"Cougar_Upper_ERL.png"), cgr_upper, height = 6, width = 8, dpi = 500)

# Expected
cgr_expected <- all_results %>% 
  mutate(ERL_yr = factor(ERL)) %>% 
  filter(site_name == "Cougar") %>% 
  ggplot() + 
  geom_line(data = cgr_msp_og, aes(x = Gumbel, y = FittedP.600, linetype = "MSP-Expected"), color = "#1B1919FF",linewidth = 0.7) +
  geom_line(aes(Gumbel_ANEP, ExpectedCurve, color = ERL_yr), linewidth = 0.4) +
  scale_color_aaas() +
  scale_linetype_manual(values = c("MSP-Expected" = "dashed"),
                        breaks = c("MSP-Expected"))+
  scale_x_continuous(breaks = Gumbel_AEP_breaks,minor_breaks = Gumbel_AEP_breaks_minor,labels = aep_breaks)+
  labs(title = "Cougar Dam - Expected",
       subtitle = "Parent MSP Data Shown",
       x = "AEP", 
       y = "Precip (in.)",
       color = "ERL (yr)",
       linetype = "Parent MSP")

ggsave(file.path(plotdir,"Cougar_Expected_ERL.png"), cgr_expected, height = 6, width = 8, dpi = 500)

# Select Optimal ERLS ----------------------------------------------------------
selected_erls <- summary_results %>%
  arrange(site_name, abs(Mean_Exp_PerDiff), abs(Mean_Exp_PerDiff)) %>%
  group_by(site_name) %>%
  slice_head(n = 3) %>%
  ungroup()

# Join for results -------------------------------------------------------------
result_export <- parent_GEVs %>% left_join(selected_erls,join_by(site_name))

# Export results ---------------------------------------------------------------
cli_h1("Exporting Results")

# GEV parameters
write_csv(result_export, file.path(outdir, "SELECTED_Site_GEV_ERL_Parameters.csv"))

# All ERLs
all_erl <- parent_GEVs %>% left_join(summary_results,join_by(site_name)) %>% 
  arrange(site_name, abs(Mean_Exp_PerDiff), abs(Mean_Exp_PerDiff))

write_csv(all_erl, file.path(outdir, "ALL_GEV_ERL_Parameters.csv"))

cli_alert_info("Plots saved to: {plotdir}")
cli_alert_info("Data saved to: {outdir}")
