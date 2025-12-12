# Cougar Stage POR

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,
  tcltk,grid,ggrepel,paletteer)

# Theme set
theme_set(theme_bw())

# Plot Dir
plotdir <- "/outputs/Figures/Stage_POR/"

# Datum Function ---------------------------------------------------------------
ngvd29_to_navd88 <- function(ngvd29){
  navd88 = ngvd29 + 4.38
  return(navd88)
}

# Stage Duration Function ------------------------------------------------------
stage_duration <- function(stage_timeseries){
  Rank <- rank(stage_timeseries)
  Prob <- Rank/(length(Rank) + 1)
  return(Prob)
}

# Cougar Elevation -------------------------------------------------------------
cgr_elevs <- tibble(
  Level = c("Top of Dam","Full Pool", "Max. Con Pool", "Top of Spillway Gates","Spillway Crest","Min. Con Pool","Min. Power Pool"),
  Elev_29 = c(1705,1699,1690,1699,1656.75,1532,1516),
  Elev_88 = Elev_29 + 4.38,
  Storage_acft = (c(NA,200,189,200,151.2,52,43.5))*1000)

cgr_elevs <- cgr_elevs %>% 
  filter(Level != c("Full Pool")) %>% 
  arrange(desc(Elev_88))

# Read POR --------------------------------------------------------------------
por_daily <- dir_ls("data/",glob = "*CGR_Daily_Stage.csv*",recurse = T) %>%
  read_csv()

por_hourly <- dir_ls("data/",glob = "*CGR_Hourly_Stage.csv*",recurse = T) %>%
  read_csv() %>% select(-Ord)

lapply(por_daily,summary)
lapply(por_hourly,summary)

# Convert to NAV88 -----------
por_daily <- por_daily %>% mutate(Stage_navd88 = ngvd29_to_navd88(Stage_ngvd29))

por_hourly <- por_hourly %>% mutate(Stage_navd88 = ngvd29_to_navd88(Stage_ngvd29))

# Confirm Datetime -----------
por_daily <- por_daily %>% mutate(DT = dmy_hms(Date_Time,truncated = 3),.before = everything())

por_hourly <- por_hourly %>% mutate(DT = ymd_hms(paste0(Date," ",Time),truncated = 3),.before = everything())

# Integer Datetime Column ------------
por_daily <- por_daily %>% mutate(Days = seq(1:length(por_daily$DT)),.before = everything())
por_hourly <- por_hourly %>% mutate(Hours = seq(1:length(por_hourly$DT)),.before = everything())

# Water Year Column ------------------------------------------------------------
por_daily <- por_daily %>% mutate(WY = ifelse(month(DT) > 9, year(DT) + 1, year(DT)),.before = everything())
por_hourly <- por_hourly %>% mutate(WY = ifelse(month(DT) > 9, year(DT) + 1, year(DT)),.before = everything())

# Annual Max Stage -------------------------------------------------------------
por_daily_ams <- por_daily %>% 
  group_by(WY) %>%
  slice_max(Stage_navd88, n = 1, with_ties = FALSE) %>% 
  ungroup()

por_hourly_ams <- por_hourly %>% 
  group_by(WY) %>%
  slice_max(Stage_navd88, n = 1, with_ties = FALSE) %>% 
  ungroup()

# Check
# ggplot() +
#   geom_point(data = por_daily_ams, aes(x = DT, y = Stage_navd88, color = "Daily AMS"),size = 2,shape = 5) +
#   geom_point(data = por_hourly_ams, aes(x = DT, y = Stage_navd88, color = "Hourly AMS"),size = 1) + 
#   geom_line(data = por_hourly, aes(x = DT, y = Stage_navd88),linewidth = 0.2)+
#   scale_color_manual(values = c("Daily AMS" = "green4",
#                                 "Hourly AMS" = "lightblue2"))
# 
# ggplot() +
#   geom_line(data = por_hourly_ams, aes(x = WY, y = Stage_navd88, color = "Hourly AMS" ),linewidth = 0.5)+
#   geom_line(data = por_daily_ams, aes(x = WY, y = Stage_navd88, color = "Daily AMS"),linewidth = 0.2)+
#   scale_color_aaas()

# Cougar high pools ------------------------------------------------------------
Cougar_Top10 <- por_hourly_ams %>% 
  slice_max(Stage_navd88, n = 10, with_ties = F) %>% 
  arrange(desc(Stage_navd88))

# POR Stage Plot ---------------------------------------------------------------
dt_breaks <- seq(ymd_hms("1950-01-01 00:00:00"),
                 ymd_hms("2030-01-01 00:00:00"),
                 "5 years")

# Define colors for zones
zone_colors <- c(
  "Surcharge" = "#d73027",
  "Flood Control" = "#fc8d59", 
  "Conservation" = "#91bfdb",
  "Inactive" = "#4575b4"
)

# Create POR plot with zones
daily_por_plot <- ggplot() +
  # Stage time series
  geom_line(data = por_daily, 
            aes(x = DT, y = Stage_navd88), 
            linewidth = 0.8, color = "black") +
  
  annotate("rect", 
           xmin = ymd_hms("1960-01-01 00:00:00"), 
           xmax = ymd_hms("2030-01-01 00:00:00"), 
           ymin = cgr_elevs$Elev_88[3], ymax = cgr_elevs$Elev_88[1], 
           alpha = 0.15, fill = zone_colors["Flood Control"]) +
  annotate("rect", 
           xmin = ymd_hms("1960-01-01 00:00:00"), 
           xmax = ymd_hms("2030-01-01 00:00:00"), 
           ymin = cgr_elevs$Elev_88[5], ymax = cgr_elevs$Elev_88[3], 
           alpha = 0.15, fill = zone_colors["Conservation"]) +
  annotate("rect", 
           xmin = ymd_hms("1960-01-01 00:00:00"), 
           xmax = ymd_hms("2030-01-01 00:00:00"), 
           ymin = cgr_elevs$Elev_88[6], ymax = cgr_elevs$Elev_88[5], 
           alpha = 0.15, fill = zone_colors["Inactive"]) +
  
  # Horizontal reference lines
  geom_hline(yintercept = cgr_elevs$Elev_88, 
             linetype = "dashed", color = "grey40", linewidth = 0.3) +
  
  # Elevation labels (positioned on right side)
  annotate("text", x = ymd_hms("1966-01-01 00:00:00"), 
           y = cgr_elevs$Elev_88[1] + 2, 
           label = paste0(cgr_elevs$Level[1], " = ", round(cgr_elevs$Elev_88[1], 1), " ft"), 
           size = 2.5, hjust = 0, fontface = "bold", color = "grey20") +
  
  annotate("text", x = ymd_hms("1966-01-01 00:00:00"), 
           y = cgr_elevs$Elev_88[3] + 3, 
           label = paste0(cgr_elevs$Level[3], " = ", round(cgr_elevs$Elev_88[3], 1), " ft"), 
           size = 2.5, hjust = 0, color = "grey20") +
  
  annotate("text", x = ymd_hms("1966-01-01 00:00:00"), 
           y = cgr_elevs$Elev_88[4] - 3, 
           label = paste0(cgr_elevs$Level[4], " = ", round(cgr_elevs$Elev_88[4], 1), " ft"), 
           size = 2.5, hjust = 0, color = "grey20") +
  
  annotate("text", x = ymd_hms("1966-01-01 00:00:00"), 
           y = cgr_elevs$Elev_88[5] + 3, 
           label = paste0(cgr_elevs$Level[5], " = ", round(cgr_elevs$Elev_88[5], 1), " ft"), 
           size = 2.5, hjust = 0, color = "grey20") +
  
  annotate("text", x = ymd_hms("1966-01-01 00:00:00"), 
           y = cgr_elevs$Elev_88[6] + 3, 
           label = paste0(cgr_elevs$Level[6], " = ", round(cgr_elevs$Elev_88[6], 1), " ft"), 
           size = 2.5, hjust = 0, color = "grey20") +
  
  # Zone labels (right side)
  annotate("text", x = ymd_hms("2026-06-01 00:00:00"), y = 1698, 
           label = "Flood Control", size = 3, fontface = "italic", 
           color = zone_colors["Flood Control"]) +
  annotate("text", x = ymd_hms("2026-06-01 00:00:00"), y = 1620, 
           label = "Conservation", size = 3, fontface = "italic", 
           color = zone_colors["Conservation"]) +
  annotate("text", x = ymd_hms("2026-06-01 00:00:00"), y = 1524, 
           label = "Inactive", size = 3, fontface = "italic", 
           color = zone_colors["Inactive"]) +
  
  # Scales
  scale_x_datetime(breaks = dt_breaks, date_labels = "%Y",
                   expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = seq(1400, 1750, 50), minor_breaks = seq(1400, 1750, 10),
                     labels = scales::comma) +
  
  # Coordinate limits
  coord_cartesian(xlim = c(ymd_hms("1963-01-01 00:00:00"), ymd_hms("2030-01-01 00:00:00")),
                  ylim = c(1400, 1710)) +
  
  # Labels and theme
  labs(x = "Year", y = "Reservoir Stage (ft-NAVD88)") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.4))

# Save
ggsave(paste0(getwd(),plotdir,"Stage_POR_Daily.png"), daily_por_plot, height = 6, width = 9, dpi=300)

# Truncated ------------------------------------------------------------------
# NA Approx
por_daily <- por_daily %>%
 mutate(Stage_navd88 = na.approx(Stage_navd88, na.rm = FALSE))

start_trunc <- lubridate::mdy("10-01-1969")
end_trunc <- lubridate::mdy("10-01-2000")

CGR_trunc <- por_daily %>% 
  dplyr::filter(DT >= start_trunc) %>% 
  dplyr::filter(DT <=end_trunc)

x_label <- ymd_hms("1985-05-01 00:00:00")

daily_por_plot_truncated <- daily_por_plot +
  annotate("rect", 
           xmin = CGR_trunc$DT[1], 
           xmax = CGR_trunc$DT[length(CGR_trunc$DT)], 
           ymin = 1750,
           ymax = 1200,
           alpha = 0.25, fill = "grey") +
  annotate(geom = "text",x = x_label, y = 1450, label = "Truncated Stage Data",
           fontface = "bold.italic", size = 4, color = "grey", hjust = 0.5, vjust = 1.2)

ggsave(paste0(getwd(),plotdir,"Stage_POR_truncated.png"), daily_por_plot_truncated, height = 6, width = 9, dpi=300)

# Empirical Stage of Truncated -------------------------------------------------

# Weibull
weibull_pp <- function(x) {
  n <- length(x)
  i <- rank(x)
  return(i / (n+1))
}

CGR_trunc_AMS <- CGR_trunc %>% 
  group_by(WY) %>%
  slice_max(Stage_navd88, n=1, with_ties = F) %>%
  ungroup %>% 
  mutate(pp = weibull_pp(Stage_navd88),
         AEP = 1 - pp,
         Series = "Truncated POR")

CGR_POR_AMS <- por_daily_ams %>% 
  mutate(pp = weibull_pp(Stage_navd88),
         AEP = 1 - pp,
         Series = "Full POR")

ams_data <- bind_rows(CGR_POR_AMS,CGR_trunc_AMS)

# plot ------
truncated_full_ecdf <- ggplot(ams_data, aes(x = AEP, y = Stage_navd88, color = Series, shape = Series)) +
  geom_point(alpha = 0.85, size = 1.5) +
  scale_color_manual(breaks = c("Truncated POR","Full POR"),
                     values = c("Truncated POR" = "#3B4992FF", "Full POR" = "#EE0000FF"),
                     name = NULL) +
  scale_shape_manual(breaks = c("Truncated POR","Full POR"),
                     values = c("Truncated POR" = 16, "Full POR" = 15),
                     name = NULL) +
  scale_x_continuous(trans = scales::trans_new("reverse_log", transform = function(x) -log10(x),inverse = function(x) 10^(-x)),
                     breaks = scales::log_breaks(n = 10, base = 10),
                     minor_breaks = scales::minor_breaks_log(),
                     labels = scales::label_number(accuracy = 0.001),
                     limits = c(1,0.01)) +
  scale_y_continuous(breaks = seq(1600,1720,10),minor_breaks = seq(1600,1720,5),limits = c(1600,1715)) + 
  geom_hline(yintercept = cgr_elevs$Elev_88[1], linetype = "dashed", color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = cgr_elevs$Elev_88[4], linetype = "dashed", color = "grey40", linewidth = 0.3) +
  annotate("text", x = 0.5, 
           y = cgr_elevs$Elev_88[1] + 2, 
           label = paste0(cgr_elevs$Level[1], " = ", round(cgr_elevs$Elev_88[1], 1), " ft"), 
           size = 2.5, hjust = 0, fontface = "bold", color = "grey20") +
  annotate("text", x = 0.5, 
           y = cgr_elevs$Elev_88[4] + 2, 
           label = paste0(cgr_elevs$Level[4], " = ", round(cgr_elevs$Elev_88[1], 1), " ft"), 
           size = 2.5, hjust = 0, fontface = "bold", color = "grey20") +
  labs(y = "Stage (ft-NAVD88)", x ="Empirical AEP")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.2))

ggsave(paste0(getwd(),plotdir,"Truncated_vs_Full_ECDF.png"), truncated_full_ecdf, height = 5, width = 7, dpi=300)

# Export Truncation ------------------------------------------------------------
dir_create(paste0(getwd(),"/outputs/Stage_POR/"))

write_csv(CGR_trunc,paste0(getwd(),"/outputs/Stage_POR/CGR_Truncated_StagePOR_daily.csv"))
write_csv(por_daily,paste0(getwd(),"/outputs/Stage_POR/CGR_StagePOR_daily.csv"))
