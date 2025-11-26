# Cougar Guide Curve
# Digitizing the copy from the WCM

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
guidecurve <- dir_ls("data/Cougar/",glob = "*CGR_GuideCurve.csv*",recurse = T) %>% read_csv()

# Convert To DT and NAVD88
cgr_guide <- cgr_guide %>% 
  mutate(Elev_88 = Elev_29 + 4.38)

cgr_guide <- cgr_guide %>% 
  mutate(DT = dmy(Date), .before =  Date)

# Set Pertnit Elevs
cgr_elevs <- tibble(
  Level = c("Top of Dam","Full Pool", "Max. Con Pool", "Top of Spillway Gates","Spillway Crest","Min. Con Pool","Min. Power Pool"),
  Elev_29 = c(1705,1699,1690,1699,1656.75,1532,1516),
  Elev_88 = Elev_29 + 4.38,
  Storage_acft = (c(NA,200,189,200,151.2,52,43.5))*1000)

cgr_elevs <- cgr_elevs %>% 
  filter(Level != c("Full Pool"))

# Recreate Figure
ggplot()+
  geom_hline(yintercept = cgr_elevs$Elev_88, linetype = "dashed")+
  annotate("text",x = mdy("10-01-2020"), y = cgr_elevs$Elev_88 + 3, label = cgr_elevs$Level, size = 2.5, hjust = 0) +
  geom_line(data = cgr_guide, aes(x = DT, y = Elev_88), color = "green3") +
  scale_y_continuous(breaks = seq(1500,1800,25), minor_breaks = seq(1500,1800,5), limits = c(1500,1725)) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2021-10-01")),date_breaks = "1 month", date_labels =  "%b")+
  labs(x = "Month", y = "Elev (ft-NAVD88)")

filename <- "D:/0.RMC/Willamette/2025-Report/Figures/Guide Curves/Cougar_Guide_navd88.png"
ggsave(filename,height = plot_height, width = plot_width, dpi = 300)

  
  
  