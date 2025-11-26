# Courgar Elev Stor

# 7/21/2025
rm(list = ls(all.names = TRUE))

# Libraries
library(tidyverse)
library(lubridate)

# Theme set
theme_set(theme_bw())
source("D:/R/theme_USACE.r")
today = format(Sys.Date(),"%d-%b-%Y")
plot_height = 5
plot_width = 7

# Load Elev-Storage
all_stor <- read.csv("D:/0.RMC/Willamette/2025-Report/data/NWP/ReservoirInfo/ElevStor.csv")

# Convert To DT and NAVD88
cgr_stor <- all_stor %>% 
  filter(grepl("Cougar",Reservoir))

cgr_stor <- cgr_stor %>% 
  mutate(Elev_88 = Elev + 4.38,.before = Stor)

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
  annotate("text",x = 0, y = cgr_elevs$Elev_88 + 3, label = cgr_elevs$Level, size = 2.2, hjust = 0) +
  geom_line(data = cgr_stor, aes(x = Stor, y = Elev_88), color = "blue2") +
  scale_y_continuous(breaks = seq(1500,1800,25), minor_breaks = seq(1500,1800,5), limits = c(1500,1725)) +
  scale_x_continuous(breaks = seq(0,300000,50000),minor_breaks = seq(0,300000,10000))+
  labs(x = "Storage (ac-ft)", y = "Elev (ft-NAVD88)")

filename <- "D:/0.RMC/Willamette/2025-Report/Figures/ReservoirGeometry/Cougar_Stor_navd88.png"
ggsave(filename,height = plot_height, width = plot_width, dpi = 300)
