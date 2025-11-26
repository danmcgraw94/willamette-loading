# JMD Volume-Freq Sensitivity
# DQC 11/2/2025

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

# Import Results CSV -----------------------------------------------------------
file_dir <- dir_ls("D:/0.RMC/JohnMartin/",glob = "*BestFit_Sensitivities",recurse = T)
bestfit <- dir_ls(file_dir, glob = "*BestFit_Sensitivity.csv") %>% read_csv()

# Add Duration Field for PMF Vol
bestfit <- bestfit %>% 
  mutate(Duration = as.numeric(str_extract(Analysis, "(?<=_)\\d+(?=_Day_)")))

# Add Z field -
#bestfit %>% 
#   mutate(Z_aep = qnorm(1 - AEP),.before = everything()) %>% 
#   mutate(test = 1-pnorm(Z_aep),.before = everything())

bestfit <- bestfit %>%
  mutate(Z_aep = qnorm(1 - AEP),.before = everything())

# PMF Volumes ------------------------------------------------------------------
pmf <- ("D:/0.RMC/JohnMartin/DQC/data/RFA_Hydrographs/PMF2024/PMF_RAS.csv") %>% read_csv()
pmf <- pmf %>% rename(Hour = Time_hrs)
pmf <- pmf %>% 
  mutate(Q_1day = rollmean(Inflow_cfs, k = 24, align = "right", fill = NA),
         Q_2day = rollmean(Inflow_cfs, k = 24*2, align = "right", fill = NA),
         Q_3day = rollmean(Inflow_cfs, k = 24*3, align = "right", fill = NA),
         Q_4day = rollmean(Inflow_cfs, k = 24*4, align = "right", fill = NA),
         Q_5day = rollmean(Inflow_cfs, k = 24*5, align = "right", fill = NA))

colMax <- function(data) sapply(data, max, na.rm = TRUE)
maxQ <- colMax(pmf %>% select(Q_1day,Q_2day,Q_3day,Q_4day,Q_5day))

pmf_sum <- pmf %>% 
  summarise(Q_1day = max(Q_1day,na.rm = T),
            Q_2day = max(Q_2day,na.rm = T),
            Q_3day = max(Q_3day,na.rm = T),
            Q_4day = max(Q_4day,na.rm = T),
            Q_5day = max(Q_5day,na.rm = T)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Q_Dur",
               values_to = "PMF_Inflow_Vol")

pmf_sum <- pmf_sum %>% 
  mutate(Duration = c(1,2,3,4,5),.before = PMF_Inflow_Vol)

# Estimate PMF AEP -------------------------------------------------------------
bestfit <- bestfit %>% 
  left_join(pmf_sum %>% select(Duration,PMF_Inflow_Vol), join_by(Duration))

bestfit_sensit_aep <- bestfit %>% 
  group_by(Analysis) %>% 
  summarise(Sensitivity = unique(Sensitivity),
            Dur_PMF_Inflow_Vol = unique(PMF_Inflow_Vol),
            AEP_PMF = 1 - pnorm(approx(x = Posteror_Pred,y = Z_aep,xout = Dur_PMF_Inflow_Vol)$y))

# Export PMF Sensitivities -----------------------------------------------------
# Compare OOM against Final
final_AEP_PMF <- bestfit_sensit_aep %>% filter(Sensitivity == "Final") %>% pull(AEP_PMF)
bestfit_sensit_aep <- bestfit_sensit_aep %>% mutate(final_AEP_PMF = final_AEP_PMF)

bestfit_sensit_aep <- bestfit_sensit_aep %>% 
  mutate(PMF_OOM = round(log10(final_AEP_PMF) - log10(AEP_PMF),2)) %>% 
  mutate(OOM_Direction = ifelse(PMF_OOM > 0,
                                paste0(abs(PMF_OOM),"x more frequent"),
                                paste0(abs(PMF_OOM),"x less frequent")),
         Curve_Shift = ifelse(PMF_OOM > 0,"Right","Left"))

write_csv(bestfit_sensit_aep,paste0(file_dir,"/PMF_Order_of_Mag.csv"))

# Plot by Sensitivity Group ----------------------------------------------------
# Gumbel ANEP Plot set up
bestfit <- bestfit %>% 
  mutate(Gumbel_AEP = -log(-log(1-AEP)),.before = everything())


# 1. Create nice analysis names (if not already done) -----------------------
bestfit <- bestfit %>%
  mutate(
    Analysis_Label = case_when(
      # Critical Duration
      str_detect(Analysis, "^2024_1_Day") ~ "1-Day Hist + Paleo + Priors",
      str_detect(Analysis, "^2024_3_Day") ~ "3-Day Hist + Paleo + Priors",
      str_detect(Analysis, "^2024_4_Day") ~ "4-Day Hist + Paleo + Priors",
      str_detect(Analysis, "^2024_5_Day") ~ "5-Day Hist + Paleo + Priors",
      
      # Record Length
      str_detect(Analysis, "Syst$") ~ "Systematic Only",
      str_detect(Analysis, "Hist$") ~ "Historical Only",
      
      # 1921 Hist
      str_detect(bestfit$Analysis, "1921$") ~ "1921 Event modeled as Interval",
      
      # Paleoflood without priors
      str_detect(Analysis, "Paleo_YH$") ~ "Paleo Young High",
      str_detect(Analysis, "Paleo_YB$") ~ "Paleo Young Best",
      str_detect(Analysis, "Paleo_YL$") ~ "Paleo Young Low",
      str_detect(Analysis, "Paleo_BH$") ~ "Paleo Best High",
      str_detect(Analysis, "Paleo_BL$") ~ "Paleo Best Low",
      str_detect(Analysis, "Paleo_OH$") ~ "Paleo Old High",
      str_detect(Analysis, "Paleo_OB$") ~ "Paleo Old Best",
      str_detect(Analysis, "Paleo_OL$") ~ "Paleo Old Low",
      str_detect(Analysis, "Paleo$") ~ "Paleo Best Estimate",
      
      # Paleoflood with priors
      str_detect(Analysis, "Priors_YH$") ~ "Paleo + Priors Young High",
      str_detect(Analysis, "Priors_YB$") ~ "Paleo + Priors Young Best",
      str_detect(Analysis, "Priors_YL$") ~ "Paleo + Priors Young Low",
      str_detect(Analysis, "Priors_BH$") ~ "Paleo + Priors Best High",
      str_detect(Analysis, "Priors_BL$") ~ "Paleo + Priors Best Low",
      str_detect(Analysis, "Priors_OH$") ~ "Paleo + Priors Old High",
      str_detect(Analysis, "Priors_OB$") ~ "Paleo + Priors Old Best",
      str_detect(Analysis, "Priors_OL$") ~ "Paleo + Priors Old Low",
      
      # Final
      str_detect(Analysis, "2024_2_Day_Ext_Max_Inflow_Hist_Paleo_Priors$") ~ "2-Day Hist + Paleo + Priors (Final)",
      
      TRUE ~ Analysis
    )
  )

# 2. AEP breaks for plotting ------------------------------------------------
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))
Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

# 3. Define colors ----------------------------------------------------------
final_color <- "black"
final_fill <- "grey80"

# 4. Final data (used in all plots) -----------------------------------------
final_data <- bestfit %>% 
  filter(Sensitivity == "Final")

# 5. Function to create plot ------------------------------------------------
create_sensitivity_plot <- function(data, sensitivity_name) {
  
  # Filter sensitivity data
  sens_data <- data %>% 
    filter(Sensitivity == sensitivity_name)
  
  is_paleo <- grepl(glob2rx("*Paleoflood*"),sensitivity_name)

  # Create plot
  p <- ggplot() +
    
    # Final: Confidence interval (shaded area)
    geom_ribbon(
      data = final_data,
      aes(x = Z_aep, ymin = CI_5, ymax = CI_95),
      fill = final_fill,
      alpha = 0.35
    ) +
    
    # Sensitivity lines (now colored and solid)
    geom_line(
      data = sens_data,
      aes(x = Z_aep, y = Posteror_Pred, group = Analysis_Label, 
          color = Analysis_Label),  # Changed to color
      linewidth = 0.5
    ) +
    
    # # Final: Mode (solid line) - add to legend
    # geom_line(
    #   data = final_data,
    #   aes(x = Z_aep, y = Posterior_Mode, linetype = "2-Day Final (Mode)"),
    #   color = final_color,
    #   linewidth = 0.65
    # ) +
    
    # Final: Posterior Predictive (dashed line) - add to legend
    geom_line(
      data = final_data,
      aes(x = Z_aep, y = Posteror_Pred, linetype = "2-Day Final (Predictive)"),
      color = final_color,
      linewidth = 0.85
    ) +
    
    # Add AAAS color scale
    ggsci::scale_color_nejm() +
    
    # Manual linetype scale for final lines only
    scale_linetype_manual(
      values = c(
#        "2-Day Final (Mode)" = "solid",
        "2-Day Final (Predictive)" = "solid"
      ), breaks = c("2-Day Final (Predictive)")
#      breaks = c("2-Day Final (Mode)", "2-Day Final (Predictive)")

    ) +
    
    # Scales
    scale_x_continuous(
      breaks = z_breaks,
      minor_breaks = z_breaks_minor,
      labels = aep_breaks
    ) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 5),
      minor_breaks = scales::minor_breaks_log(detail = 1),
      labels = scales::comma
    ) +
    coord_cartesian(xlim = c(qnorm(1 - 0.99), qnorm(1 - 1e-7))) +
    
    # Labels
    labs(
      x = "AEP (Normal)",
      y = "Inflow Volume (cfs)",
      title = paste("Sensitivity Analysis:", sensitivity_name),
      linetype = "Final Model",
      color = "Sensitivity Analyses"  # Added color legend title
    ) +
    
    guides(
      linetype = guide_legend(order = 1),
      color = guide_legend(order = 2)
    ) +
    
    # Theme
    theme_bw() +
    theme(
      legend.position = "inside",
      #legend.position.inside = c(0.17, lege_y),
      legend.position.inside = c(0.05, 0.95),
      legend.justification = c(0, 1),  # Anchor at top-left corner of legend box
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7, face = "bold"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.key.width = unit(1, "cm"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 11, face = "bold")
    )
  
  return(p)
}

# 6. Create individual plots ------------------------------------------------
pmf_2day_y <- pmf_sum %>% filter(Duration == 2) %>% pull(PMF_Inflow_Vol)
aep_position <- 0.1

# Critical Duration
p_critical <- create_sensitivity_plot(bestfit, "Critical Duration")

# Paleoflood with priors
p_paleo_priors <- create_sensitivity_plot(bestfit, "Paleoflood (with priors)") + 
  geom_hline(yintercept = pmf_2day_y, linetype = "dashed") +
  annotate("label",
           x = qnorm(1-aep_position), 
           y = pmf_2day_y + 1000, 
           label = "2-Day PMF = 784,600",
           size = 3)

# Paleoflood without priors
p_paleo_no_priors <- create_sensitivity_plot(bestfit, "Paleoflood (without priors)") + 
  geom_hline(yintercept = pmf_2day_y, linetype = "dashed") +
  annotate("label",
           x = qnorm(1-aep_position), 
           y = pmf_2day_y + 1000, 
           label = "2-Day PMF = 784,600",
           size = 3)

# Record Length
p_record <- create_sensitivity_plot(bestfit, "Record Length") + 
  geom_hline(yintercept = pmf_2day_y, linetype = "dashed") +
  annotate("label",
           x = qnorm(1-aep_position), 
           y = pmf_2day_y + 1000, 
           label = "2-Day PMF = 784,600",
           size = 3)

# 1921 Hist
p_hist1921 <- create_sensitivity_plot(bestfit, "1921 Event modeled as Historic Interval") + 
  geom_hline(yintercept = pmf_2day_y, linetype = "dashed") +
  annotate("label",
           x = qnorm(1-aep_position), 
           y = pmf_2day_y + 1000, 
           label = "2-Day PMF = 784,600",
           size = 3)


# 7. Display plots ----------------------------------------------------------
print(p_critical)
print(p_paleo_priors)
print(p_paleo_no_priors)
print(p_record)
print(p_hist1921)

#Save plots
ggsave(paste0(plot_dir,"VFA_Sensitivity/critical_duration.png"), p_critical, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"VFA_Sensitivity/paleoflood_with_priors.png"), p_paleo_priors, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"VFA_Sensitivity/paleoflood_without_priors.png"), p_paleo_no_priors, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"VFA_Sensitivity/record_length.png"), p_record, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"VFA_Sensitivity/Hist1921.png"), p_hist1921, height = 6, width = 8, dpi = 500)


