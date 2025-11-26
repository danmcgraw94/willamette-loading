# Willamette Climate Normals
# Basin/Raster Analysis already completed
# Creating climate normal bar plots
#
#
#
# Only creating figure for precip!

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo)

# Theme and plot directory
theme_set(theme_bw())

plot_dir <- file.path(getwd(),"outputs","Figures","Climate_Normal")
dir_create(plot_dir)

# Functions --------------------------------------------------------------------
temp_c_to_f <- function(tempC){
  tempF = (tempC*(9/5)) + 32
  return(tempF)
}

mm_to_in <- function(L_mm){
  L_in = L_mm/25.4
  return(L_in)
}

# Read Data --------------------------------------------------------------------
climate_norm_all <- dir_ls("data/",glob = "*climate_normals_summary.csv*",recurse = T) %>% read_csv()

norm_ppt <- climate_norm_all %>% filter(variable == "ppt")
norm_temp <- climate_norm_all %>% filter(variable == "tmean")

# Convert mm to inches ---------------------------------------------------------
ppt_in <- norm_ppt %>% 
  mutate(across(c(min, max, mean, sd, median, q25, q75), 
                mm_to_in, 
                .names = "{col}_IN")) %>% 
  select(variable, month, ends_with("_IN"))

# Water Year Ordering/orientation  -------
wy_order <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08", "09")
wy_months <- as.character(c(lubridate::month(as.numeric(wy_order), label = TRUE, abbr = TRUE)))

ppt_monthly <- ppt_in %>%
  filter(month != "annual") %>%
  mutate(month = factor(month, levels = wy_order, ordered = TRUE))

# Box and Whisker plot of Precip Normals -------
ppt_boxplot <- ggplot(ppt_monthly, aes(x = month)) +
  geom_segment(aes(y = min_IN, yend = q25_IN, xend = month), 
               linewidth = 0.4, color = "grey40") +
  geom_segment(aes(y = q75_IN, yend = max_IN, xend = month), 
               linewidth = 0.4, color = "grey40") +
  geom_rect(aes(xmin = as.numeric(month) - 0.3, 
                xmax = as.numeric(month) + 0.3,
                ymin = q25_IN, 
                ymax = q75_IN),
            fill = "#91bfdb", color = "black", linewidth = 0.3) +
  
  # Median line
  geom_segment(aes(y = median_IN, yend = median_IN,
                   x = as.numeric(month) - 0.3,
                   xend = as.numeric(month) + 0.3),
               linewidth = 0.8, color = "black") +
  
  # Mean as a point (optional - shows center tendency)
  geom_point(aes(y = mean_IN), shape = 18, size = 1.25, color = "red") +
  
  # Min/max caps
  geom_segment(aes(y = min_IN, yend = min_IN,
                   x = as.numeric(month) - 0.15,
                   xend = as.numeric(month) + 0.15),
               linewidth = 0.4, color = "grey40") +
  geom_segment(aes(y = max_IN, yend = max_IN,
                   x = as.numeric(month) - 0.15,
                   xend = as.numeric(month) + 0.15),
               linewidth = 0.4, color = "grey40") +
  
  # Scales and labels
  scale_x_discrete(labels = wy_months) +
  scale_y_continuous(breaks = seq(0, 35, 5), 
                     minor_breaks = seq(0, 35, 1),
                     expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = c(0, 35)) +
  
  labs(x = NULL,
       y = "Monthly Precipitation (in)") +
  
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4)
  )

ggsave(file.path(plot_dir,"PPT_boxplot.png"), ppt_boxplot, height = 5, width = 7, dpi = 500)

# Using ribbon to display IQR
# Create continuous month values for smooth curves
ppt_ribbon <- ppt_monthly %>%
  mutate(month_num = as.numeric(month))

# Asked Claude.ai to beef this one up:
ppt_ribbon <- ggplot(ppt_ribbon, aes(x = month_num)) +
  # Shaded quantile ranges with subtle gradient effect
  geom_ribbon(aes(ymin = q25_IN, ymax = q75_IN), 
              fill = "#91bfdb", alpha = 0.5) +
  
  # Whiskers (min-max range)
  geom_errorbar(aes(ymin = min_IN, ymax = max_IN),
                width = 0.2, linewidth = 0.35, color = "grey50") +
  
  # Median line - smooth and prominent
  geom_line(aes(y = median_IN), 
            color = "black", linewidth = 1.2) +
  geom_point(aes(y = median_IN), 
             color = "black", fill = "white", shape = 21, 
             size = 2.5, stroke = 1) +
  
  # Mean points - distinct but complementary
  geom_point(aes(y = mean_IN), 
             color = "#d73027", shape = 18, size = 2.5) +
  
  # Scales with better formatting
  scale_x_continuous(
    breaks = 1:12, 
    labels = wy_months,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(0, 35, 5), 
    minor_breaks = seq(0, 35, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 35)) +
  
  # Labels with subtitle
  labs(
    x = NULL, 
    y = "Monthly Precipitation (in)",
    caption = "Shaded area: interquartile range (25th–75th percentile) | Lines: min–max range\nBlack circles: median | Red diamonds: mean"
  ) +
  
  # Refined theme
  theme_bw(base_size = 11) +
  theme(
    # Grid
    panel.grid.minor.y = element_line(color = "grey92", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # Border
    panel.border = element_rect(color = "grey60", linewidth = 0.5),
    
    # Background
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Text
    axis.title.y = element_text(margin = margin(r = 8), size = 11),
    axis.text = element_text(color = "grey20"),
    plot.caption = element_text(
      hjust = 0, 
      size = 8, 
      color = "grey40", 
      margin = margin(t = 8),
      lineheight = 1.2
    ),
    
    # Spacing
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(file.path(plot_dir,"PPT_ribbon.png"), ppt_ribbon, height = 5, width = 7, dpi = 500)
