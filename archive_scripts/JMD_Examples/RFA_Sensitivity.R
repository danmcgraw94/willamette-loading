# RFA Sensitivity - DQC --------------------------------------------------------
# 11/3/2025

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(gt)
library(lubridate)
library(scales)
library(zoo)
library(patchwork)
library(fs)
library(ggsci)
library(stringr)

theme_set(theme_bw())
plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"

# Import Sensitivity CSVs -------------------------------------------------------
file_dir <- dir_ls("D:/0.RMC/JohnMartin/",glob = "*RFA_Sensitivities",recurse = T)
sensit_codes <- dir_ls(file_dir, glob = "*JMD_RFA_Sensitivity_codes.csv") %>% read_csv()
rfa <- dir_ls(file_dir, glob = "*JMD_RFA_Sensitivity.csv") %>% read_csv()

empirical_stages <- "D:/0.RMC/JohnMartin/data/Sensitivity/Empirical_Stages.csv" %>% read_csv()
empirical_stages <- empirical_stages %>% rename(AEP = PP)

# Add Z and Gumbel variate to AEPs ---------------------------------------------
rfa <- rfa %>%
  mutate(Z = qnorm(1-AEP),
         Gumb = -log(-log(1-AEP)),.before = everything())

empirical_stages <- empirical_stages %>%
  mutate(Z = qnorm(1-AEP),
         Gumb = -log(-log(1-AEP)),.before = everything())

# Critical Elevations ----------------------------------------------------------
crit_elevs <- tibble(
  Name = c("Upper PMF", "Recommended PMF", "Top of Dam", "Record Pool", "Flood Control Pool", "Spillway Crest"),
  Elev = c(3893.8,3890.9,3881.8,3862.2,3871.8,3841.8))

crit_elevs <- crit_elevs %>% 
  mutate(label_text = paste(Name,"=",Elev))

# Pivot Sensitivity results longer and join with codes -------------------------
rfa_long <- rfa %>% 
  pivot_longer(cols = -c(Gumb,Z,AEP),
               names_to = "Sensit_Code",
               values_to = "Stage_ft")

rfa_long <- rfa_long %>% 
  left_join(sensit_codes,join_by(Sensit_Code))

# Estimate Top of Dam AEP ------------------------------------------------------
TOD <- crit_elevs$Elev[crit_elevs$Name == "Top of Dam"]

rfa_aep <- rfa_long %>% 
  group_by(Analysis) %>% 
  summarise(Sensitivity = unique(Sensitivity),
            TOD_Elev = TOD,
            AEP_TOD = 1 - pnorm(approx(x = Stage_ft, y = Z, xout = TOD)$y)) %>% 
  ungroup()

# Compare OOM against Final ----------------------------------------------------
final_AEP_TOD <- rfa_aep %>% filter(Sensitivity == "Final") %>% pull(AEP_TOD)
rfa_aep <- rfa_aep %>% 
  mutate(TOD_OOM = round(log10(final_AEP_TOD) - log10(AEP_TOD),2)) %>%
  mutate(OOM_Direction = ifelse(TOD_OOM > 0,
                                paste0(abs(TOD_OOM),"x less frequent"),
                                paste0(abs(TOD_OOM),"x more frequent")),
         Curve_Shift = ifelse(TOD_OOM > 0,"Right","Left"))

rfa_aep <- rfa_aep %>% arrange(Sensitivity)

# Export ToD Sensitivities -----------------------------------------------------
write_csv(rfa_aep,paste0(file_dir,"/TOD_Order_of_Mag.csv"))

# Plot by Sensitivity Group ----------------------------------------------------

# 1. AEP breaks for plotting -----
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

gumb_limit1 <- -log(-log(1-(9.9e-1)))
gumb_limit2 <- -log(-log(1-(1e-7)))

# 2. Define colors -----
final_color <- "black"
final_fill <- "grey80"

# 3. Final data (used in all plots) ----
final_data <- rfa_long %>% 
  filter(Sensitivity == "Final")

# 5. Function to create plot ------------------------------------------------
create_rfa_sensitivity_plot <- function(data, sensitivity_name) {
  
  # Filter sensitivity data
  sens_data <- data %>% 
    filter(Sensitivity == sensitivity_name)
  
  #is_paleo <- grepl(glob2rx("*Paleoflood*"),sensitivity_name)
  
  # Create plot
  p <- ggplot() +
    
    # Sensitivity lines (now colored and solid)
    geom_line(
      data = sens_data,
      aes(x = Gumb, y = Stage_ft, group = Analysis, 
          color = Analysis),  # Changed to color
      linewidth = 0.5) +
    
    # Final - Expected only. Add to legend
    geom_line(
      data = final_data,
      aes(x = Gumb, y = Stage_ft, linetype = "2-Day Final"),
      color = final_color,
      linewidth = 0.85
    ) +
    
    # Add AAAS color scale
    ggsci::scale_color_nejm() +
    
    # Manual linetype scale for final lines only
    scale_linetype_manual(
      values = c("2-Day Final" = "solid"), breaks = c("2-Day Final")
    ) +
    
    # Observed Stage points
    geom_point(data = empirical_stages, aes(x = Gumb, y = Stage_ft, shape = "Obs. Stages (WY 1948-2020)"),size = 1.5,alpha = 0.7) +
    
    scale_shape_manual(values = c("Obs. Stages (WY 1948-2020)" = 16))+
    
    # Scales
    scale_x_continuous(
      breaks = Gumbel_AEP_breaks,
      minor_breaks = Gumbel_AEP_breaks_minor,
      labels = aep_breaks
    ) +
    scale_y_continuous(
      breaks = seq(3700,3910,10),
      minor_breaks = seq(3700,3910,5),
      labels = scales::comma
    ) +
    
    # Labels
    labs(
      x = "AEP",
      y = "Stage (ft-NAVD88)",
      title = paste("Sensitivity Analysis:", sensitivity_name),
      linetype = "Final Model",
      shape = "Observations",
      color = paste0(sensitivity_name) #"Sensitivity Analyses"  # Added color legend title
    ) +
    guides(
      linetype = guide_legend(order = 1),
      color = guide_legend(order = 2)
    ) +

    geom_hline(yintercept = crit_elevs$Elev) +
    annotate("text",x = -log(-log(1 - 0.999)), y = crit_elevs$Elev + 1, label = crit_elevs$label_text, size = 2.5, hjust = 0) +
    
    # Theme
    theme_bw() +
    theme(
      legend.position = "inside",
      #legend.position.inside = c(0.7, 0.6),
      legend.justification = c(0.95, 0.1),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7, face = "bold"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.key.width = unit(1, "cm"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 11, face = "bold")
    )+
    coord_cartesian(xlim = c(gumb_limit1, gumb_limit2), ylim = c(3800,3900))
  
  return(p)
}

# 6. Plot ----------------------------------------------------------------------
unique(rfa_long$Sensitivity)

# Critical Duration
p_critical <- create_rfa_sensitivity_plot(rfa_long, "Critical Duration")

# Critical Duration
p_operations <- create_rfa_sensitivity_plot(rfa_long, "Operations")

# Critical Duration
p_hydroshape <- create_rfa_sensitivity_plot(rfa_long, "Hydrograph Shape")

# Critical Duration
p_prior_analysis <- create_rfa_sensitivity_plot(rfa_long, "Prior Analysis")

# Paleoflood with priors
p_paleo <- create_rfa_sensitivity_plot(rfa_long, "Paleoflood Application")

# Record Length
p_record <- create_rfa_sensitivity_plot(rfa_long, "Record Length")

# 1921 Hist
p_hist1921 <- create_rfa_sensitivity_plot(rfa_long, "1921 as Historic Interval")

# 7. Display plots ----------------------------------------------------------
print(p_critical)
print(p_operations)
print(p_hydroshape)
print(p_prior_analysis)
print(p_paleo)
print(p_hist1921)
print(p_record)

#Save plots
ggsave(paste0(plot_dir,"Stage_Sensitivity/critical_duration.png"), p_critical, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"Stage_Sensitivity/operations.png"), p_operations, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"Stage_Sensitivity/hydrograph_shape.png"), p_hydroshape, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"Stage_Sensitivity/record_length.png"), p_record, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"Stage_Sensitivity/x_prior_analysis.png"), p_prior_analysis, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"Stage_Sensitivity/paleo.png"), p_paleo, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"Stage_Sensitivity/hist1921.png"), p_hist1921, height = 6, width = 8, dpi = 500)


