# Cougar Stage-Frequency Sensitivity
# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,janitor)

# Theme and plot directory
theme_set(theme_bw())
plotdir <- file.path(getwd(),"outputs","Figures","Sensitivity","Stage_Frequency")
dir_create(plotdir)

# Read Data --------------------------------------------------------------------
# Sensitivity Key
rfa_key <- dir_ls("data/Cougar/",glob = "*RFA_Sensitivity_Key.csv*",recurse = T) %>%
  read_csv() %>% 
  janitor::clean_names()

# Stage-Frequency Curves
rfa_results <- dir_ls("data/Cougar/",glob = "*RFA_Sensitivity_Results.csv*",recurse = T) %>%
  read_csv()

# Empirical Stages
empirical_stages <- dir_ls("data/Cougar/",glob = "*CGR_Empirical_StageFreq.csv*",recurse = T) %>%
  read_csv()

empirical_stages <- empirical_stages %>%
  rename(AEP = Plotting_Position) %>% 
  mutate(DT = mdy(Date))

# Final - Full Uncertainty
final_full <- dir_ls("data/Cougar/",glob = "*Final_FullUncert.csv*",recurse = T) %>%
  read_csv()

# Add Z and Gumbel variate to AEPs ---------------------------------------------
rfa_results <- rfa_results %>%
  mutate(Z_AEP = qnorm(1-AEP),
         Gumbel_AEP = -log(-log(1-AEP)),.before = everything())

empirical_stages <- empirical_stages %>%
  mutate(Z_AEP = qnorm(1-AEP),
         Gumbel_AEP = -log(-log(1-AEP)),.before = everything())

final_full <- final_full %>%
  mutate(Z_AEP = qnorm(1-AEP),
         Gumbel_AEP = -log(-log(1-AEP)),.before = everything())

# Pivot Sensitivity results longer and join with codes -------------------------
rfa_long <- rfa_results %>% 
  pivot_longer(cols = -c(Gumbel_AEP, Z_AEP, AEP),
               names_to = "sensit_id",
               values_to = "Stage_ft")

rfa_long <- rfa_long %>% 
  left_join(rfa_key,join_by(sensit_id))

# Critical Elevs ---------------------------------------------------------------
pmf_ngvd29 <- 1710.2 - 4.38

cgr_elevs <- tibble(
  Level = c("Top of Dam","PMF","Full Pool", "Max. Con. Pool", "Top of Spillway Gates","Spillway Crest","Min. Con. Pool","Min. Power Pool"),
  Elev_29 = c(1705,pmf_ngvd29,1699,1690,1699,1656.75,1532,1516),
  Elev_88 = Elev_29 + 4.38) %>% 
  filter(!Level %in% c("Full Pool","Min. Con. Pool","Min. Power Pool")) %>% 
  arrange(desc(Elev_88)) %>%
  mutate(elev_label = paste0(Level, ": ",round(Elev_88,1),"-ft"))

# Estimate Top of Dam AEP ------------------------------------------------------
TOD <- cgr_elevs %>% filter(Level == "Top of Dam") %>% pull(Elev_88)

rfa_aep <- rfa_long %>% 
  group_by(simulation) %>% 
  summarise(nice_name = unique(description),
            Sensitivity = unique(sensitivity),
            TOD_Elev = TOD,
            AEP_TOD = 1 - pnorm(approx(x = Stage_ft, y = Z_AEP, xout = TOD)$y)) %>% 
  ungroup()

# Estimate Full Uncertainty AEP ------------------------------------------------
final_TOD_AEP <- 1 - pnorm(approx(x = final_full$Expected, y = final_full$Z_AEP, xout = TOD)$y)

# Compute OOM From Final -------------------------------------------------------
rfa_aep <- rfa_aep %>% 
  mutate(TOD_OOM = round(log10(final_TOD_AEP) - log10(AEP_TOD),2)) %>% 
  mutate(OOM_Direction = ifelse(TOD_OOM > 0,
                                paste0(abs(TOD_OOM),"x more frequent"),
                                paste0(abs(TOD_OOM),"x less frequent")),
         Curve_Shift = ifelse(TOD_OOM > 0,"Right","Left"))

outdir <- file.path(getwd(),"outputs","StageFreq_Sensitivity")
fs::dir_create(outdir)

outfile <- file.path(outdir,"TOD_Order_of_Mag.csv")
write_csv(rfa_aep, outfile)

# Plot by Sensitivity Group ----------------------------------------------------
# Use Gumbel variate for Stage
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

# Gumbel breaks
Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

# Z breaks
z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

# X-limits
gumb_limit1 <- -log(-log(1-(9.9e-1)))
gumb_limit2 <- -log(-log(1-(1e-7)))

# Y-limits
stage_1 <- 1660
stage_2 <- 1720

# Critical Elevation label - X coordinate
label_aep <- 0.05

# Define colors -----
final_color <- "black"
final_fill <- "grey80"

# Final data (used in all plots) -----
if(exists("final_full")) {print("Final Full Uncert Loaded")}else{print("Final Full Uncert is NOT Loaded")}

# Drop Final Expected
rfa_long <- rfa_long %>% 
  filter(sensitivity != "Final")

# Plot Function -----
# note Reservoir ops plot is separate below exports
rfa_sensitivity_plot <- function(data, sensitivity_name) {
  
  # Filter sensitivity data
  sens_data <- data %>% 
    filter(sensitivity == sensitivity_name)
  
  # Create plot
  p <- ggplot() +
    
    # Final: Confidence interval (shaded area)
    geom_ribbon(data = final_full, aes(x = Gumbel_AEP, ymin = Lower, ymax = Upper), fill = final_fill, alpha = 0.35) +
    
    # Sensitivity lines (now colored and solid)
    geom_line(data = sens_data, aes(x = Gumbel_AEP, y = Stage_ft, group = description, color = description), linewidth = 0.5) +
    
    # Final - Expected only. Add to legend
    geom_line(data = final_full, aes(x = Gumbel_AEP, y = Expected, linetype = "3-Day Final"), color = final_color,linewidth = 0.85) +
    
    # Add NEJM color scale
    ggsci::scale_color_nejm() +
    
    # Manual linetype scale for final lines only
    scale_linetype_manual(values = c("3-Day Final" = "solid"),
                          breaks = c("3-Day Final")) +
    
    # Observed Stage points
    geom_point(data = empirical_stages, aes(x = Gumbel_AEP, y = Stage_ft, shape = "Obs. Stages (Summer AMS 1971-2001)"),size = 1.5,alpha = 0.7) +
    scale_shape_manual(values = c("Obs. Stages (Summer AMS 1971-2001)" = 16))+
    
    # Scales
    scale_x_continuous(breaks = Gumbel_AEP_breaks,minor_breaks = Gumbel_AEP_breaks_minor,labels = aep_breaks) +
    scale_y_continuous(breaks = seq(1600,1800,10), minor_breaks = seq(1600,1800,5),labels = scales::comma) +
    
    # Labs
    labs(x = "AEP",
         y = "Stage (ft-NAVD88)",
         title = paste("Sensitivity Analysis:", sensitivity_name),
         linetype = "Final Model",
         shape = "Observations",
         color = paste0(sensitivity_name)) +
    # Legend Guides
    guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
    # Crit Elevation lines
    geom_hline(yintercept = cgr_elevs$Elev_88) +
    annotate("label", x = -log(-log(1 - label_aep)), 
             cgr_elevs$Elev_88[cgr_elevs$Level != "PMF"], 
             label = cgr_elevs$elev_label[cgr_elevs$Level != "PMF"], size = 2.5) +
    
    annotate("label", x = -log(-log(1 - 0.001)), 
             cgr_elevs$Elev_88[cgr_elevs$Level == "PMF"], 
             label = cgr_elevs$elev_label[cgr_elevs$Level == "PMF"], size = 2.5) +
    
    # Theme
    theme_bw() +
    theme(legend.position = "inside",
          legend.justification = c(0.95, 0.1),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 7, face = "bold"),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.key.width = unit(1, "cm"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          plot.title = element_text(size = 11, face = "bold")) +
    coord_cartesian(xlim = c(gumb_limit1, gumb_limit2), ylim = c(stage_1, stage_2),expand = F)
  
  return(p)
}

# Create individual plots ------------------------------------------------------
unique(rfa_long$sensitivity)

stage_record_length <- rfa_sensitivity_plot(rfa_long, "Record Length")
stage_ams_seasonality <- rfa_sensitivity_plot(rfa_long, "Inflow AMS Seasonality")
stage_paleo <- rfa_sensitivity_plot(rfa_long, "Paleoflood Application")
stage_res_condition <- rfa_sensitivity_plot(rfa_long, "Reservoir Operations")
stage_max_discharge <- rfa_sensitivity_plot(rfa_long, "Maximum Discharge Capacity")
stage_seasonality <- rfa_sensitivity_plot(rfa_long, "Seasonality")
stage_previous <- rfa_sensitivity_plot(rfa_long, "Previous VFC Analysis")

# Display plots
print(stage_record_length)
print(stage_ams_seasonality)
print(stage_paleo)
print(stage_res_condition)
print(stage_max_discharge)
print(stage_seasonality)
print(stage_previous)

# Save Figures -----------------------------------------------------------------
ggsave(file.path(plotdir,"Record_Length.png"), stage_record_length, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"All_Season_Inflow_AMS.png"), stage_ams_seasonality, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Paleoflood_Application.png"), stage_paleo, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Reservoir_Operations.png"), stage_res_condition, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Max_Discharge_Capacity.png"), stage_max_discharge, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Seasonality.png"), stage_seasonality, height = 6, width = 8, dpi = 500)
ggsave(file.path(plotdir,"Previous.png"), stage_previous, height = 6, width = 8, dpi = 500)

# Discharge Capacity Plot ------------------------------------------------------
# Requires setting reduction % to factor
# Subset reservoir ops
ops_data <- rfa_long %>% 
  filter(sensitivity == "Discharge Capacity")

# use name for levels
unique(ops_data$name)

# Color Palette ------
# blue_red <- c("#290AD8FF", "#264DFFFF", "#3FA0FFFF", "#72D9FFFF",
#               "#AAF7FFFF", "#E0FFFFFF", "#FFFFBFFF", "#FFE099FF",
#               "#FFAD72FF", "#F76D5EFF", "#D82632FF", "#A50021FF")

# Factor Level -----------------------------------------------------------------
ops_name_order <- c("Spillway Reduction 10%",
                    "Spillway Reduction 25%",
                    "Spillway Reduction 50%",
                    "Spillway Reduction 75%",
                    "Spillway Reduction 100%",
                    "No Gate Operations",
                    "Delayed Operations - Best Estimate",
                    "Delayed Operations with 10% reduction",
                    "Delayed Operations with 25% reduction",
                    "Delayed Operations with 50% reduction")

# Set Colors -------------------------------------------------------------------
ops_colors <- c("Spillway Reduction 10%" = "#72D9FFFF",
                "Spillway Reduction 25%" = "#AAF1FFFF",
                "Spillway Reduction 50%" = "#FFAD72FF",
                "Spillway Reduction 75%" = "#F76D5EFF",
                "Spillway Reduction 100%" = "#D82632FF",
                "No Gate Operations" = "#A50021FF",
                "Delayed Operations - Best Estimate" = "#808180FF",
                "Delayed Operations with 10% reduction" = "#3FA0FFFF",
                "Delayed Operations with 25% reduction" = "#72D9FFFF",
                "Delayed Operations with 50% reduction" = "#FFAD72FF")

# Set Linetype -----------------------------------------------------------------
ops_lines <- c("Spillway Reduction 10%" = "solid",
               "Spillway Reduction 25%" = "solid",
               "Spillway Reduction 50%" = "solid",
               "Spillway Reduction 75%" = "solid",
               "Spillway Reduction 100%" = "solid",
               "No Gate Operations" = "solid",
               "Delayed Operations - Best Estimate" = "dashed",
               "Delayed Operations with 10% reduction" = "dashed",
               "Delayed Operations with 25% reduction" = "dashed",
               "Delayed Operations with 50% reduction" = "dashed")

# Set Name to Factor -----------------------------------------------------------
ops_data <- ops_data %>% mutate(Spillway_Reduction = factor(name,levels = ops_name_order),.before = name)

# Plot -------------------------------------------------------------------------
stage_discharge_cap <- ggplot() +
  # Final: Confidence interval (shaded area)
  geom_ribbon(data = final_full, aes(x = Gumbel_AEP, ymin = Lower, ymax = Upper), fill = final_fill, alpha = 0.35) +
  
  # Sensitivity lines (now colored and solid)
  geom_line(data = ops_data, aes(x = Gumbel_AEP, y = Stage_ft, color = Spillway_Reduction, linetype = Spillway_Reduction), linewidth = 0.5) +
  
  # Final - Expected only. Add to legend
  geom_line(data = final_full, aes(x = Gumbel_AEP, y = Expected, color = "3-Day Final", linetype = "3-Day Final"), linewidth = 0.85) +
  
  # Add color and linetype scales
  scale_color_manual(values = c(ops_colors, "3-Day Final" = final_color), breaks = c(ops_name_order, "3-Day Final")) +
  scale_linetype_manual(values = c(ops_lines, "3-Day Final" = "solid"), breaks = c(ops_name_order, "3-Day Final")) +
  
  # Observed Stage points
  geom_point(data = empirical_stages, aes(x = Gumbel_AEP, y = Stage_ft, shape = "Obs. Stages (Summer AMS 1971-2001)"), size = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c("Obs. Stages (Summer AMS 1971-2001)" = 16)) +
  
  # Scales
  scale_x_continuous(breaks = Gumbel_AEP_breaks, minor_breaks = Gumbel_AEP_breaks_minor, labels = aep_breaks) +
  scale_y_continuous(breaks = seq(1600, 1800, 10), minor_breaks = seq(1600, 1800, 5), labels = scales::comma) +
  
  # Labs
  labs(x = "AEP",
       y = "Stage (ft-NAVD88)",
       title = paste("Sensitivity Analysis: Reservoir Operations"),
       linetype = "Reservoir Operations",
       shape = "Observations",
       color = "Reservoir Operations") +
  
  # Legend Guides
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crit Elevation lines
  geom_hline(yintercept = cgr_elevs$Elev_88) +
  geom_hline(yintercept = cgr_elevs$Elev_88) +
  annotate("label", x = -log(-log(1 - label_aep)), 
           cgr_elevs$Elev_88[cgr_elevs$Level != "PMF"], 
           label = cgr_elevs$elev_label[cgr_elevs$Level != "PMF"], size = 2.5) +
  
  annotate("label", x = -log(-log(1 - 0.001)), 
           cgr_elevs$Elev_88[cgr_elevs$Level == "PMF"], 
           label = cgr_elevs$elev_label[cgr_elevs$Level == "PMF"], size = 2.5) +
  
  # Theme
  theme_bw() +
  theme(legend.position = "inside",
        legend.justification = c(0.95, 0.1),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7, face = "bold"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.width = unit(1, "cm"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold")) +
  coord_cartesian(xlim = c(gumb_limit1, gumb_limit2), ylim = c(stage_1, stage_2), expand = F)

# Save -------------------------------------------------------------------------
ggsave(file.path(plotdir,"Discharge_Capacity.png"), stage_discharge_cap, height = 6, width = 8, dpi = 500)
