# Daily Stage
# ---- Package Management ----
pacman::p_load(
  # Core
  tidyverse,
  zoo,
  patchwork,                      # Combine plots
  ggsci,                          # Scientific color palettes (optional)
  ggtext,                         # Rich text in plots
  plotly,                         # Interactive plots
  # Performance
  future, furrr,                  # Parallel processing
  memoise,                         # Caching
  paletteer
)

theme_set(theme_bw())

# Read Data --------------------------------------------------------------------
daily_stage <- fs::dir_ls("D:/0.RMC/JohnMartin/", glob = "*DailyStage.csv") %>% 
  read_csv()

dam_top <- 3880 +1.8
fc <- 3870 +1.8
max_con <- 3851.84 +1.8
spillway <- 3840 +1.8
zero_stor <- 3770 +1.8
conduit <- 3776 +1.8

ggplot() + 
  # Shaded region for pre-operation period
  annotate("rect", 
           xmin = as.Date("1943-01-01"), 
           xmax = as.Date("1979-01-01"), 
           ymin = 3760, 
           ymax = 3900, 
           alpha = 0.15, 
           fill = 'grey80') +
  
  # Stage data line
  geom_line(data = daily_stage,
            aes(x = Date, y = Elevft_NAVD88),
            linewidth = 0.3,
            color = "black")+
  geom_hline(yintercept = dam_top, color = "#d73027", linewidth = 0.8, linetype = "solid") +
  geom_hline(yintercept = fc, color = "#fc8d59", linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = max_con, color = "#4575b4", linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = zero_stor, color = "#4a5f4a", linewidth = 0.8, linetype = "dotted") +
  annotate("text", 
           x = as.Date("1945-01-01"), 
           y = dam_top + 3.5,
           label = "Top of Dam = 3,881.8 ft",
           size = 3,
           hjust = 0,
           fontface = "bold",
           color = "#d73027") +
  
  annotate("text", 
           x = as.Date("1945-01-01"), 
           y = fc + 3.5,
           label = "Flood Control Pool = 3,871.8 ft",
           size = 3,
           hjust = 0,
           color = "#fc8d59") +
  
  annotate("text", 
           x = as.Date("1945-01-01"), 
           y = max_con + 3.5,
           label = "Max Conservation Pool = 3,853.6 ft",
           size = 3,
           hjust = 0,
           color = "#4575b4") +
  
  annotate("text", 
           x = as.Date("1980-01-01"), 
           y = zero_stor - 3,
           label = "Zero Storage = 3,771.8 ft",
           size = 3,
           hjust = 0,
           color = "#4a5f4a") +
  
  # Label for pre-operation period
  annotate("text",
           x = as.Date("1961-01-01"),
           y = 3895,
           label = "Operations: Pre-WCM (1983)",
           size = 3,
           fontface = "italic",
           color = "grey30") +
  
  # Scales
  scale_x_date(limits = c(as.Date("1943-01-01"), as.Date("2025-12-31")),
               breaks = seq(as.Date("1940-01-01"), as.Date("2030-01-01"), by = "5 years"),
               date_labels = "%Y",
               minor_breaks = seq(as.Date("1940-01-01"), as.Date("2030-01-01"), by = "1 years"),
               expand = c(0.01, 0)) + 
  
  scale_y_continuous(breaks = seq(3760, 3900, 20),
                     minor_breaks = seq(3760, 3900, 10)) +
  
  # Theme
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.2),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.title = element_text(face = "bold"),
    panel.border = element_rect(color = "grey60", linewidth = 0.8)
  ) +
  
  # Labels
  labs(
#    title = "Reservoir Stage History",
#    subtitle = "Daily water surface elevations (1943-2025)",
    x = "Year", 
    y = "Stage Elevation (ft, NAVD88)"
  ) +
  coord_cartesian(ylim = c(3760, 3900), expand = FALSE)

plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"
ggsave(paste0(plot_dir,"Daily_Stage.png"), height = 5, width = 8, dpi = 500)


