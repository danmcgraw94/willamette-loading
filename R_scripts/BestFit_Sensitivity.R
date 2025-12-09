# Cougar Volume-Frequency Sensitivity
# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,janitor)


# Theme and plot directory
theme_set(theme_bw())
plotdir <- file.path(getwd(),"outputs","Figures","Sensitivity","Volume_Frequency")
dir_create(plotdir)

# Read Data --------------------------------------------------------------------
# Distribution summaries
bestfit_sum <- dir_ls("data/Cougar/",glob = "*BF_analysis_summary.csv*",recurse = T) %>%
  read_csv() %>% 
  janitor::clean_names()

# Volume-Frequency Curves
vfcs <- dir_ls("data/Cougar/",glob = "*BF_Sensitivities.csv*",recurse = T) %>%
  read_csv()

# Add Z field
vfcs <- vfcs %>% 
  mutate(Z_aep = qnorm(1 - Probability),.before = everything())

# Get Final ------
final_sum <- bestfit_sum %>% filter(sensitivity == "Final")
final <- vfcs %>% filter(Analysis == final_sum$analysis)

# Parameter Distributions ------------------------------------------------------
plot_theme <- theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 8))

final_color <- "#BB0021FF"
#"#d02b28"
#print(pal_aaas("default")(10))

# Create individual plots
p1 <- ggplot(bestfit_sum, aes(x = mean_of_log)) + 
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  geom_vline(xintercept = final_sum$mean_of_log, 
             color = final_color, linetype = "dashed", linewidth = 0.75) +
  labs(title = "Mean of Log", x = "Mean of Log", y = "Density") +
  plot_theme

p2 <- ggplot(bestfit_sum, aes(x = sd_of_log)) + 
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  geom_vline(xintercept = final_sum$sd_of_log, 
             color = final_color, linetype = "dashed", linewidth = 0.75) +
  labs(title = "Standard Deviation of Log", x = "SD of Log", y = "Density") +
  plot_theme

p3 <- ggplot(bestfit_sum, aes(x = skew_of_log)) + 
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  geom_vline(xintercept = final_sum$skew_of_log, 
             color = final_color, linetype = "dashed", linewidth = 0.75) +
  labs(title = "Skew of Log", x = "Skew of Log", y = "Density") +
  plot_theme

p4 <- ggplot(bestfit_sum, aes(x = erl)) + 
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  geom_vline(xintercept = final_sum$erl, 
             color = final_color, linetype = "dashed", linewidth = 0.75) +
  labs(title = "Effective Record Length", x = "ERL (years)", y = "Density") +
  plot_theme

p5 <- ggplot(bestfit_sum, aes(x = rmse)) + 
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  geom_vline(xintercept = final_sum$rmse, 
             color = final_color, linetype = "dashed", linewidth = 0.75) +
  labs(title = "Root Mean Square Error", x = "RMSE", y = "Density") +
  plot_theme

# Combine with patchwork
combined_plot <- (p1 | p2 | p3) / (p4 | p5) +
  plot_annotation(
    title = "LP3 Parameter Distributions Across Sensitivity Analyses",
    subtitle = "Green dashed line indicates final analysis values",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(combined_plot)
ggsave(file.path(plotdir,"Parameter_Summary.png"), combined_plot, height = 6, width = 10, dpi = 500)

# PMF Volume -------------------------------------------------------------------
pmf_3day <- 42700

# Join with Sensitivity Field --------------------------------------------------
vfcs_df <- vfcs %>% 
  left_join(bestfit_sum, join_by(Analysis == analysis))

# Estimate PMF AEP -------------------------------------------------------------
vfc_pmf_aep <- vfcs_df %>% 
  group_by(Analysis) %>%
  summarise(Name = unique(nice_name),
            Sensitivity = unique(sensitivity),
            AEP_PMF = 1 - pnorm(approx(x = Posterior_Predictive, y = Z_aep, xout = pmf_3day)$y))

# Compute OOM From Final -------------------------------------------------------
final_AEP_PMF <- vfc_pmf_aep %>% filter(Sensitivity == "Final") %>% pull(AEP_PMF)

vfc_pmf_aep <- vfc_pmf_aep %>% 
  mutate(PMF_OOM = round(log10(final_AEP_PMF) - log10(AEP_PMF),2)) %>% 
  mutate(OOM_Direction = ifelse(PMF_OOM > 0,
                                paste0(abs(PMF_OOM),"x more frequent"),
                                paste0(abs(PMF_OOM),"x less frequent")),
         Curve_Shift = ifelse(PMF_OOM > 0,"Right","Left"))

outdir <- file.path(getwd(),"outputs","VFC_Sensitivity")
fs::dir_create(outdir)

outfile <- file.path(outdir,"PMF_Order_of_Mag.csv")
write_csv(vfc_pmf_aep,outfile)

# Plot by Sensitivity Group ----------------------------------------------------
# Z ANEP Plot set up ----
vfcs_df <- vfcs_df %>% 
  mutate(Gumbel_AEP = -log(-log(1-Probability)),.before = everything())

# AEP breaks for plotting -----
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

# Define colors -----
final_color <- "black"
final_fill <- "grey80"

# Final data (used in all plots) -----
final_data <- vfcs_df %>% 
  filter(sensitivity == "Final")

# Plot Function -----
pmf_3day_y <- 42700
aep_position <- 0.1

sensitivity_plot <- function(data, sensitivity_name) {
  
  # Filter sensitivity data
  sens_data <- data %>% 
    filter(sensitivity == sensitivity_name)
  
  #is_paleo <- grepl(glob2rx("*Paleoflood*"),sensitivity_name)
  
  # Create plot
  p <- ggplot() +
    # Final: Confidence interval (shaded area)
    geom_ribbon(data = final_data, aes(x = Z_aep, ymin = CI_5, ymax = CI_95), fill = final_fill, alpha = 0.35) +
    
    # Sensitivity lines
    geom_line(data = sens_data, aes(x = Z_aep, y = Posterior_Predictive, group = nice_name, color = nice_name), linewidth = 0.5) +
    
    # Final: Posterior Predictive (dashed line) - add to legend
    geom_line(data = final_data, aes(x = Z_aep, y = Posterior_Predictive, 
                                     linetype = "3-Day Final (Predictive)"), color = final_color, linewidth = 0.85) +
    
    # Add NEJM color scale
    ggsci::scale_color_nejm() +
    
    # Manual linetype scale for final lines only
    scale_linetype_manual(values = c("3-Day Final (Predictive)" = "solid"), 
                          breaks = c("3-Day Final (Predictive)")) +
    # Scales
    scale_x_continuous(breaks = z_breaks, minor_breaks = z_breaks_minor, labels = aep_breaks) +
    scale_y_log10(breaks = scales::log_breaks(n = 5),
                  minor_breaks = scales::minor_breaks_log(detail = 1),
                  labels = scales::comma) +
    
    # coord cartesian - gumbel
    #coord_cartesian(xlim = c(-log(-log(1-0.99)), -log(-log(1- 1e-7)))) +
    
    # coord cartesian - z variate
    coord_cartesian(xlim = c(qnorm(1 - 0.99), qnorm(1 - 1e-7))) +
    
    # Labels
    labs(x = "AEP (Normal)", 
         y = "Inflow Volume (cfs)", 
         title = paste("Sensitivity Analysis:", sensitivity_name), 
         linetype = "Final Model",
         color = "Sensitivity Analyses") +
    
    guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
    
    # PMF Line
    geom_hline(yintercept = pmf_3day_y, linetype = "dashed") + 
    annotate("label", x = qnorm(1-aep_position), y = pmf_3day_y + 1000, label = "3-Day PMF = 42,700 cfs", size = 3) +
    
    # Theme
    theme_bw() +
    theme(legend.position = "inside", 
          legend.position.inside = c(0.98, 0.02), 
          legend.justification = c(1, 0),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 7, face = "bold"),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          plot.title = element_text(size = 11, face = "bold"))
  
  return(p)
}

# Create individual plots ------------------------------------------------------
# unique(vfcs_df$sensitivity)
record_length <- sensitivity_plot(vfcs_df, "Record Length")
seasonality <- sensitivity_plot(vfcs_df, "Seasonality")
paleo <- sensitivity_plot(vfcs_df, "Paleoflood Application")
previous <- sensitivity_plot(vfcs_df, "Previous Analysis")

# Display plots
print(record_length)
print(seasonality)
print(paleo)
print(previous)

# Save Figures -----------------------------------------------------------------
ggsave(file.path(plotdir,"Record_Length.png"), record_length, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Seasonality.png"), seasonality, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Paleoflood_Application.png"), paleo, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Previous_Analysis.png"), previous, height = 6, width = 8, dpi = 500)

