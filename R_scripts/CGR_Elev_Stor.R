# Courgar Elev Stor

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo)


# Theme and plot directory
theme_set(theme_bw())
plotdir <- "/outputs/Figures/Operations/"

# Read Data --------------------------------------------------------------------
all_stor <- dir_ls("data/Cougar/",glob = "*ElevStor.csv*",recurse = T) %>% read_csv()

# Convert To DT and NAVD88
cgr_stor <- all_stor %>% 
  filter(grepl("Cougar",Reservoir)) %>% 
  mutate(Elev_88 = Elev + 4.38,.before = Stor)

# Set Pertnit Elevs
cgr_elevs <- tibble(
  Level = c("Top of Dam","Full Pool", "Max. Con Pool", "Top of Spillway Gates","Spillway Crest","Min. Con Pool","Min. Power Pool"),
  Elev_29 = c(1705,1699,1690,1699,1656.75,1532,1516),
  Elev_88 = Elev_29 + 4.38,
  Storage_acft = (c(NA,200,189,200,151.2,52,43.5))*1000)

cgr_elevs <- cgr_elevs %>% 
  filter(Level != c("Full Pool")) %>% 
  arrange(desc(Elev_88))

zone_colors <- c(
  "Surcharge" = "#d73027",
  "Flood Control" = "#fc8d59", 
  "Conservation" = "#91bfdb",
  "Inactive" = "#4575b4"
)

# Add storage values for key elevations by interpolating from cgr_stor
cgr_elevs <- cgr_elevs %>%
  mutate(Storage_interp = approx(cgr_stor$Elev_88, cgr_stor$Stor, xout = Elev_88)$y)

# Recreate Figure
cgr_storage <- ggplot() +
  # Shaded zones
  annotate("rect", 
           xmin = 0, xmax = max(cgr_stor$Stor), 
           ymin = cgr_elevs$Elev_88[2], ymax = cgr_elevs$Elev_88[1], 
           alpha = 0.15, fill = zone_colors["Surcharge"]) +
  annotate("rect", 
           xmin = 0, xmax = max(cgr_stor$Stor), 
           ymin = cgr_elevs$Elev_88[3], ymax = cgr_elevs$Elev_88[2], 
           alpha = 0.15, fill = zone_colors["Flood Control"]) +
  annotate("rect", 
           xmin = 0, xmax = max(cgr_stor$Stor), 
           ymin = cgr_elevs$Elev_88[5], ymax = cgr_elevs$Elev_88[3], 
           alpha = 0.15, fill = zone_colors["Conservation"]) +
  annotate("rect", 
           xmin = 0, xmax = max(cgr_stor$Stor), 
           ymin = cgr_elevs$Elev_88[6], ymax = cgr_elevs$Elev_88[5], 
           alpha = 0.15, fill = zone_colors["Inactive"]) +
  # Horizontal reference lines
  geom_hline(yintercept = cgr_elevs$Elev_88, 
             linetype = "dashed", color = "grey40", linewidth = 0.3) +
  # Storage-elevation curve
  geom_line(data = cgr_stor, aes(x = Stor, y = Elev_88), 
            color = "black", linewidth = 1.5) +
  # Elevation labels
  annotate("text", x = 100000, y = cgr_elevs$Elev_88[1] + 3, 
           label = paste0(cgr_elevs$Level[1], " = ", round(cgr_elevs$Elev_88[1], 1), " ft"), 
           size = 3, hjust = .5, fontface = "bold", color = "grey20") +
  
  annotate("text", x = 100000, y = cgr_elevs$Elev_88[2] - 4, 
           label = paste0(cgr_elevs$Level[2], " = ", round(cgr_elevs$Elev_88[2], 1), " ft"), 
           size = 3, hjust = 0.5, color = "grey20") +
  
  annotate("text", x = 100000, y = cgr_elevs$Elev_88[3] - 4, 
           label = paste0(cgr_elevs$Level[3], " = ", round(cgr_elevs$Elev_88[3], 1), " ft"), 
           size = 3, hjust = 0.5, color = "grey20") +
  
  annotate("text", x = 100000, y = cgr_elevs$Elev_88[4] + 4, 
           label = paste0(cgr_elevs$Level[4], " = ", round(cgr_elevs$Elev_88[4], 1), " ft"), 
           size = 3, hjust = 0.5, color = "grey20") +
  
  annotate("text", x = 100000, y = cgr_elevs$Elev_88[5] + 4, 
           label = paste0(cgr_elevs$Level[5], " = ", round(cgr_elevs$Elev_88[5], 1), " ft"), 
           size = 3, hjust = 0.5, color = "grey20") +
  
  annotate("text", x = 100000, y = cgr_elevs$Elev_88[6] + 7, 
           label = paste0(cgr_elevs$Level[6], " = ", round(cgr_elevs$Elev_88[6], 1), " ft"), 
           size = 3, hjust = 0.5, color = "grey20") +
  # Zone labels
  annotate("text", x = max(cgr_stor$Stor) - 20000, y = 1707, 
           label = "Surcharge", size = 2.75, fontface = "italic", 
           color = zone_colors["Surcharge"]) +
  annotate("text", x = max(cgr_stor$Stor) - 20000, y = 1698, 
           label = "Flood Control", size = 2.75, fontface = "italic", 
           color = zone_colors["Flood Control"]) +
  annotate("text", x = max(cgr_stor$Stor) - 20000, y = 1620, 
           label = "Conservation", size = 2.75, fontface = "italic", 
           color = zone_colors["Conservation"]) +
  annotate("text", x = max(cgr_stor$Stor) - 20000, y = 1524, 
           label = "Inactive", size = 2.75, fontface = "italic", 
           color = zone_colors["Inactive"]) +
  # Scales
  scale_y_continuous(
    breaks = seq(1500, 1725, 25), 
    minor_breaks = seq(1500, 1725, 5), 
    limits = c(1500, 1725),
    labels = scales::comma,
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(0, 200000, 50000),
    minor_breaks = seq(0, 200000, 10000),
    labels = scales::comma,
    expand = c(0.01, 0.01)
  ) +
  # Labels and theme
  labs(
    #title = "Cougar Dam Storage-Elevation Curve",
    x = "Storage (ac-ft)", 
    y = "Elevation (ft, NAVD88)",
    caption = "Bold line indicates storage-elevation relationship"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    #plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  coord_cartesian(xlim = c(0, max(cgr_stor$Stor)), expand = FALSE)


print(cgr_storage)
ggsave(paste0(getwd(),plotdir,"Cougar_Stor_navd88.png"), cgr_storage, height = 5, width = 7, dpi = 500)
