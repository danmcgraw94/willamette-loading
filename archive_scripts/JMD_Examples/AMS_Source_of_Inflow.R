## Figure of AMS with Inflow Source

library(tidyverse)
library(lubridate)
theme_set(theme_bw())

out_dir <- "D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"

# Read Sys Inflow Data
inflow_vol_AMS <- read_csv("D:/0.RMC/JohnMartin/DQC/Best_Fit/Systematic_Inflow.csv")

# Approximate Data Windows
inflow_vol_AMS <- inflow_vol_AMS %>% 
  mutate(Source = case_when(
    Year < 1913 ~ "USGS La Junta - Extension",
    Year >= 1913 & Year < 1943 ~ "USGS Streamflow",
    Year >= 1943 ~ "Reservoir Inflow Data"
  ))

# Add column for window shading
inflow_vol_AMS <- inflow_vol_AMS %>% 
  mutate(tall_y = 1e6,
         low_y = 0)

# window plot df
inflow_vol_AMS_window <- inflow_vol_AMS %>% 
  bind_rows(tibble(
    Year = 1910,
    Source = "USGS La Junta - Extension",
    tall_y = 1e6,
    low_y = 0)) %>% 
  arrange(Year)


# Plot 1: Colored dots depending on "Source"
ams_col_points <- ggplot(inflow_vol_AMS, aes(x = Year, y = Q_2day, color = Source)) +
  geom_point(alpha = 0.85, size = 2) +
  scale_color_manual(breaks = c("USGS La Junta - Extension",
                               "USGS Streamflow",
                               "Reservoir Inflow Data"),
                    values = c("USGS La Junta - Extension" = "#F5C710",
                               "USGS Streamflow" = "#2297E6",
                               "Reservoir Inflow Data" = "#61D04F")) +
  scale_y_continuous(breaks = seq(0,100000,10000),minor_breaks = seq(0,100000,5000),labels = scales::comma) + 
  scale_x_continuous(breaks = seq(1900,2030,10),minor_breaks = seq(1900,2030,2)) + 
  coord_cartesian(ylim = c(0,90000), xlim = c(1910,2025))+
  labs(x = "Water Year", y = "2-Day Reservoir Inflow (cfs)") +
  theme_bw() +
  theme(legend.title = element_blank(),legend.position = "bottom")

# Plot 2: Colored areas depending on "Source"
ams_col_window <- ggplot(inflow_vol_AMS_window) +
  geom_ribbon(aes(x = Year, ymin = low_y, ymax = tall_y, fill = Source), alpha = 0.375) +
  geom_point(aes(x = Year, y = Q_2day), alpha = 0.9, size = 1.5) +
  labs(x = "Water Year", y = "2-Day Reservoir Inflow (cfs)") +
  scale_fill_manual(values = c("USGS La Junta - Extension" = "#F5C710",
                                "USGS Streamflow" = "#2297E6",
                                "Reservoir Inflow Data" = "#61D04F"),
                    breaks = c("USGS La Junta - Extension",
                               "USGS Streamflow",
                               "Reservoir Inflow Data")) +
  scale_y_continuous(breaks = seq(0,100000,10000),minor_breaks = seq(0,100000,5000),labels = scales::comma) + 
  scale_x_continuous(breaks = seq(1900,2030,10),minor_breaks = seq(1900,2030,2)) + 
  coord_cartesian(ylim = c(0,90000), xlim = c(1910,2025))+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.75,0.85))

# Save
ggsave(paste0(out_dir,"1_2DayAMS_Source.png"),
       plot = ams_col_points, height = 5, width = 8, dpi = 500)

ggsave(paste0(out_dir,"2_2DayAMS_Source.png"),
       plot = ams_col_window, height = 5, width = 8, dpi = 500)
