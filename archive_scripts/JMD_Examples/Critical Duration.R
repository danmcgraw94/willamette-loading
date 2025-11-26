# JMD Critical Duration
# DQC 10/31/2025

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(gt)
library(lubridate)
library(scales)
library(zoo)
library(patchwork)
library(fs)
library(ggsci)
source("D:/R/theme_USACE.r")

theme_set(theme_bw())
plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"

# File Path --------------------------------------------------------------------
file_dir <- dir_ls("D:/0.RMC/JohnMartin/",glob = "*RFA_Hydrographs",recurse = T)
csv_files <- dir_ls(file_dir,glob = "*.csv") 
# dir_tree(file_dir)

# RFA Hydrographs --------------------------------------------------------------
# Import RFA Inflow hydrographs
rfa_hydros <- dir_ls(file_dir, glob = "*RFA_Hydrographs.csv") %>% read_csv() %>% 
  mutate(Hour = Ord - 1,.before = everything())

# Import June 1965
june1965 <- dir_ls(file_dir, glob = "*June*.csv") %>% read_csv()
june1965 <- june1965 %>% rename(Q_June1965 = Flow_cfs)

# Convert RFA Hydrographs to 0.25hr timestep -----------------------------------
rfa_hydros_15min <- tibble(Hour = seq(0,max(rfa_hydros$Hour),0.25))

# Join with hourly flow
rfa_hydros_15min <- rfa_hydros_15min %>% 
  left_join(rfa_hydros %>% select(-Ord),join_by(Hour))

# NA Approx (or Spline if it's not right)
rfa_hydros_15min <- rfa_hydros_15min %>%
  mutate(across(c(Q_PMF, Q_SDF, Q_May1955, Q_June1965HMS, Q_May1999,Q_June1921),
                ~na.approx(., x = Hour, na.rm = FALSE)))

# Join June1965
rfa_hydros_15min <- rfa_hydros_15min %>%
  left_join(june1965, join_by(Hour))

# Pivot longer to check/compare ------------------------------------------------
rfa_hydros_15min_long <- rfa_hydros_15min %>%
  pivot_longer(cols = starts_with("Q_"),
               names_to = "Hydro",
               values_to = "Flow_cfs",
               names_prefix = "Q_")

rfa_hydros_1hr_long <- rfa_hydros %>%
  pivot_longer(cols = starts_with("Q_"),
               names_to = "Hydro",
               values_to = "Flow_cfs",
               names_prefix = "Q_")

ggplot() +
  geom_point(data = rfa_hydros_15min_long, aes(x = Hour, y = Flow_cfs),
    color = "#1F77B4", size = 1) +
  geom_line(data = rfa_hydros_1hr_long, aes(x = Hour, y = Flow_cfs),
            color = "forestgreen", linewidth = 0.7) +
  facet_wrap(~Hydro, ncol = 2, scales = "free_y") +
  labs(title = "RFA Hydrographs by Event",
       x = "Hour",
       y = "Flow (cfs)") +
  theme_bw() +
  theme(legend.position = "none")

# Looks good 

# Nicer Event Names ------------------------------------------------------------
rfa_hydros_15min_long <- rfa_hydros_15min_long %>% 
  mutate(Event = case_when(
    Hydro == "June1921" ~ "June 1921",    
    Hydro == "June1965" ~ "June 1965",
    Hydro == "June1965HMS" ~ "June 1965-HMS",
    Hydro == "May1955" ~ "May 1955",
    Hydro == "May1999" ~ "May 1999",
    Hydro == "PMF" ~ "PMF",
    Hydro == "SDF" ~ "SDF"
  ))

# Critical Duration - Peak Flow Centered ---------------------------------------
colMax <- function(data) sapply(data, max, na.rm = TRUE)
maxQ <- colMax(rfa_hydros_15min)

# Find Peak Flow and Time ---
peak_Q_sum <- rfa_hydros_15min_long %>% 
  group_by(Event) %>%
  summarize(Max_Flow_cfs = max(Flow_cfs, na.rm = TRUE),
            Hour_at_Peak = Hour[which.max(Flow_cfs)]) %>% ungroup()

# Scale Flow to Peak Flow ---
rfa_hydros_15min_long <- rfa_hydros_15min_long %>%
  left_join(peak_Q_sum, join_by(Event)) %>% 
  mutate(Flow_Scaled = Flow_cfs/Max_Flow_cfs)


ggplot(data = rfa_hydros_15min_long, aes(x = Hour, y = Flow_Scaled, color = Event)) +
  geom_line(linewidth = 0.5) +
  labs(title = "RFA Hydrographs - All Events",
       x = "Hour",
       y = "Flow (cfs)") +
  scale_color_aaas() +
  theme_bw()

# Center at Peak -----
rfa_hydros_centered <- rfa_hydros_15min_long %>%
  mutate(Hour_Centered = Hour - Hour_at_Peak) %>%
  select(Event, Hour_Centered, Flow_cfs, Flow_Scaled, Hour_Original = Hour)

# Take Average at each time step -----
rfa_hydros_avg <- rfa_hydros_centered %>%
  group_by(Hour_Centered) %>%
  summarize(
    Flow_cfs_avg = mean(Flow_cfs, na.rm = FALSE),
    Flow_cfs_scaled_avg = mean(Flow_Scaled, na.rm = FALSE),
    n_events = n(),
    n_non_na = sum(!is.na(Flow_cfs))
  ) %>%
  ungroup()

# Overlaid plot -----------
event_colors <- c("June 1921" = "#4E79A7",
                  "June 1965" = "#F28E2B",
                  "June 1965-HMS" = "#E15759",
                  "May 1955" = "#76B7B2",
                  "May 1999" = "#59A14F",
                  "PMF" = "#B07AA1",
                  "SDF" = "#9C755F")

rfa_hydro_center_overlay <- 
  ggplot() +
  geom_line(data = rfa_hydros_centered, 
            aes(x = Hour_Centered, y = Flow_Scaled, group = Event,linetype =  Event),
            color = "grey50",alpha = 0.45, linewidth = 0.75) +
  # geom_line(data = rfa_hydros_avg, 
  #           aes(x = Hour_Centered, y = Flow_cfs_scaled_avg), 
  #           color = "#1F77B4", linewidth = 1.2) +
  geom_line(data = rfa_hydros_avg,
            aes(x = Hour_Centered, y = Flow_cfs_scaled_avg, color = "Average"), linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = seq(-96,120,12))+
  coord_cartesian(xlim = c(-48,120))+
  scale_color_manual(values = c("Average" = "#B40F20"))+
  labs(x = "Hours from Peak Inflow", y = "Fraction of Peak Inflow (cfs)", color = NULL,linetype = NULL) + 
  theme_bw() +
  theme(legend.position = "inside",legend.position.inside = c(0.80,0.70))+
  geom_segment(aes(x = -12, y = 0.25, xend = 34, yend = 0.25),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 12, y = 0.3, label= "2-Day Duration",size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

ggsave(paste0(plot_dir,"/CritDur/RFA_Hydros_Overlay.png"),
       plot = rfa_hydro_center_overlay, height = 6, width = 9, dpi = 500)

# Or faceted -----------------
rfa_hydro_center_facet <- ggplot(rfa_hydros_centered, aes(x = Hour_Centered, y = Flow_cfs)) +
  geom_line(aes(color = Event), linewidth = 0.8) +
  scale_color_manual(values = event_colors)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20") +
  scale_x_continuous(breaks = seq(-96,120,24))+
  scale_y_continuous(labels = scales::comma)+
  coord_cartesian(xlim = c(-48,120))+
  facet_wrap(~Event, scales = "free_y", ncol = 2) +
  labs(
    #    title = "RFA Hydrographs - Centered at Peak",
    x = "Hours from Peak Inflow",
    y = "Flow (cfs)") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(paste0(plot_dir,"/CritDur/RFA_Hydros_Facet.png"),
       plot = rfa_hydro_center_facet, height = 6, width = 9, dpi = 500)

# Patchwork
rfa_hydro_stack <- rfa_hydro_center_overlay/rfa_hydro_center_facet

rfa_hydro_side <- rfa_hydro_center_overlay+rfa_hydro_center_facet

ggsave(paste0(plot_dir,"/CritDur/RFA_Hydros_stack.png"),
       plot = rfa_hydro_stack, height = 7, width = 10, dpi = 500)

ggsave(paste0(plot_dir,"/CritDur/RFA_Hydros_side.png"),
       plot = rfa_hydro_side, height = 7, width = 11, dpi = 500)

# Fraction of 2-day Volume -----------------------------------------------------
# while I'm here
# 2 days to minutes, k
twodays <- 2*24*4

rfa_hydros_2day_sum <- rfa_hydros_centered %>%
  group_by(Event) %>%
  mutate(Inflow_2Day = rollmean(Flow_cfs, k = twodays,fill = NA, align = "right")) %>% 
  summarize(Max_2day_cfs = max(Inflow_2Day, na.rm = TRUE)) %>% 
  ungroup()

# Join and apply Fraction
rfa_hydros_centered <- rfa_hydros_centered %>%
  left_join(rfa_hydros_2day_sum, join_by(Event)) %>% 
  mutate(Fract_2day_cfs = Flow_cfs/Max_2day_cfs)

rfa_hydro_fract2day <- ggplot() +
  geom_line(data = rfa_hydros_centered, 
            aes(x = Hour_Centered, y = Fract_2day_cfs, group = Event,color =  Event),alpha = 0.75, linewidth = 0.85) +
  facet_wrap(~Event, scales = "fixed", ncol = 2) +
  #scale_x_continuous(breaks = seq(0,240,24),minor_breaks = seq(0,240,12)) +
  #coord_cartesian(xlim = c(0,192)) +
  scale_x_continuous(breaks = seq(-96,120,24),labels = (seq(-96,120,24) + 48))+
  coord_cartesian(xlim = c(-48,120))+
  scale_color_manual(values = event_colors) +
  labs(x = "Hours", y = "Fraction of Max 2-Day Volume (cfs)", color = NULL,linetype = NULL) + 
  theme_bw() +
  theme(legend.position = "none")
  
ggsave(paste0(plot_dir,"/CritDur/RFA_Hydros_2dayFract.png"),
       plot = rfa_hydro_fract2day, height = 5, width = 8, dpi = 500)
