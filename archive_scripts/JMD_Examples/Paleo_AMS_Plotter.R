# ===============================================
# SIMPLIFIED FLOOD FREQUENCY ANALYSIS WITH ERAS
# ===============================================

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  # Core
  tidyverse, data.table, dtplyr,  # Fast data manipulation
  # Plotting
  ggplot2, scales, ggh4x, ggrepel, 
  patchwork,                       # Combine plots
  # ggsci,                        # Scientific color palettes (optional)
  ggtext,                         # Rich text in plots
  # Utilities
  glue,                           # String interpolation
  cli,                            # Better console output
  assertthat,                     # Input validation
  plotly,                         # Interactive plots
  # Performance
  future, furrr,                  # Parallel processing
  memoise,                        # Caching
  fs
)

# Read bestfit inputs
obs <- fs::dir_ls("D:/0.RMC/JohnMartin/DQC/data/BestFit_2day_input",glob = "*syst*") %>% read_csv()
historic <- fs::dir_ls("D:/0.RMC/JohnMartin/DQC/data/BestFit_2day_input",glob = "*interval*") %>% read_csv()
thresh <- fs::dir_ls("D:/0.RMC/JohnMartin/DQC/data/BestFit_2day_input",glob = "*thresh*") %>% read_csv()

# 1. Compute eras
thresh_start <- min(thresh$Year_start)
thresh_end <- max(thresh$Year_End)

hist_start <- min(historic$Year)
hist_end <- max(historic$Year)

start_sys <- min(obs$Year)
end_sys <- max(obs$Year)

obs <- obs %>% 
  mutate(Era = ifelse(Year >= hist_start,"Hist","Paleo"))

historic <- historic %>% 
  mutate(Era = ifelse(Year >= hist_start,"Hist","Paleo"))

thresh <- thresh %>% 
  mutate(Era = ifelse(Year_End >= hist_start,"Hist","Paleo"))

# For Axis scales
max_hist <- ceiling(max(max(obs$Q_2day),max(historic$Q_2day_Upper))/1e5)*1e5
hist_axis_scale <- 1e4

max_thresh <- ceiling(max(thresh$Q_2day_thresh)/1e5)*1e5
thresh_axis_scale <- 1e5

# Paleo plot - no legend, with horizontal line
paleo <- ggplot(data = thresh %>% filter(Era == "Paleo")) +
  geom_rect(aes(xmin = Year_start, 
                xmax = Year_End, 
                ymin = 0, 
                ymax = Q_2day_thresh), 
            fill = "lightcoral",
            color = "white",
            alpha = 0.45) +
  
  annotate("text",
           x = 0, y = 600000,
           label = "NEB2: -480 - 519\n0 - 727,000 cfs",
           vjust = -0.5,
           hjust = 0.5,
           size = 2.7) +
  
  annotate("text",
           x = 1240, y = 75000,
           label = "NEB1: 520-1893\n0 - 175,000 cfs",
           vjust = -0.5,
           hjust = 0.5,
           size = 2.7) +
  
  scale_y_continuous(breaks = seq(0, max_thresh, thresh_axis_scale), 
                     labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(-500,2000,500),1885),
                     minor_breaks = seq(-500,2000,100))+
  coord_cartesian(xlim = c(-500,1890),ylim = c(0,800000),expand = F)+
  labs(x = "Year", y = "2-Day Volume (cfs)") +
  theme_bw() +
  theme(legend.position = "none")

# Historic plot - with legend inside (same as before)
hist <- ggplot() +
  geom_rect(data = thresh %>% filter(Era == "Hist"),
            aes(xmin = Year_start, 
                xmax = Year_End, 
                ymin = 0, 
                ymax = Q_2day_thresh, 
                fill = "Perception Thresholds"), 
            alpha = 0.45) +
  
  geom_point(data = obs, 
             aes(x = Year, y = Q_2day, fill = "Systematic Record"), 
             shape = 21,
             size = 1.5,
             color = "black",
             alpha = 0.7) +
  
  geom_errorbar(data = historic %>% filter(Era == "Hist"),
                aes(x = Year, ymin = Q_2day_Lower, ymax = Q_2day_Upper),
                width = 2,
                linewidth = 0.75,
                color = "black") + 
  
  geom_point(data = historic,
             aes(x = Year, y = Q_2day_Best, fill = "Historic Floods"),
             size = 1.5,
             shape = 21,
             color = "black",
             alpha = 0.7) +
  
  scale_fill_manual(
    values = c("Systematic Record" = "grey40",
               "Historic Floods" = "lightblue",
               "Perception Thresholds" = "lightcoral"),
    breaks = c("Perception Thresholds", "Historic Floods", "Systematic Record")
  ) + 
  scale_y_continuous(breaks = seq(0, max_hist, hist_axis_scale), 
                     labels = scales::comma) +
  scale_x_continuous(breaks = seq(1870,2030,10),
                     minor_breaks = seq(1870,2030,2))+
  coord_cartesian(xlim = c(1890,2025),ylim = c(0,80000),expand = FALSE)+
  labs(x = "Year", y = "", fill = NULL) +
  guides(fill = guide_legend(
    override.aes = list(
      shape = c(22, 21, 21),
      fill = c("lightcoral", "lightblue", "grey40"),
      alpha = c(0.45, 1, 1),
      color = c(NA, "black", "black"),
      size = c(4, 2, 2)
    )
  )) +
  theme_bw() +
  theme(
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.8, "cm")
  )

# Combine plots
bf_2day_neat <- paleo + hist + plot_layout(widths = c(0.4, 0.6))

plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"
ggsave(paste0(plot_dir,"BestFit_Neat/AMS_2day_6x8.png"), bf_2day_neat, height = 6, width = 8, dpi = 500)
ggsave(paste0(plot_dir,"BestFit_Neat/AMS_2day_7x11.png"), bf_2day_neat, height = 7, width = 11, dpi = 500)
