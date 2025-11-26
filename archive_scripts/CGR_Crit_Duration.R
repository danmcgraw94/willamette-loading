# Top --------------------------------------------------------------------------
# Cougar Critical Duration 
# Willamette Loading Report Figures
today = format(Sys.Date(),"%d-%b-%Y")

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
theme_set(theme_bw()) # I dislike the default/grey ggplot theme 

# Read Data --------------------------------------------------------------------
csvfile <- "D:/0.RMC/Willamette/2025-Report/data/Crit_Duration/RFA_Hydrographs.csv"
cgr_hydros <- read.csv(csvfile)

# Obtain uniform timestep ------------------------------------------------------
cgr_hydros_scratch <- cgr_hydros %>% 
  mutate(DT = mdy_hms(paste0(Date," ",Time),truncated = 3),.before = Date) %>% 
  group_by(Hydro) %>% 
  mutate(TimeDiff = difftime(DT,lag(DT,n = 1),units = "hours"),
         Timestep = as.numeric(TimeDiff)) %>% 
  mutate(Timestep = ifelse(is.na(TimeDiff),0,Timestep),
         Time_hrs = cumsum(Timestep),
         Hourly_Avg_Flow = ifelse(Hydro == "Dec-1964" | Hydro == "Jan-1996",zoo::rollmean(Flow,k = 4,fill = NA),Flow), # specific error handling bc I was lazy
         Time_hrs_frompeak = Time_hrs - Time_hrs[which.max(Flow)]) %>% 
  ungroup()

cgr_hydros <- cgr_hydros_scratch %>% 
  dplyr::select(c(Time_hrs,Time_hrs_frompeak,DT,Flow,Hourly_Avg_Flow,Hydro))

# Pivot to a wide DF to take the avg -------------------------------------------
cgr_hydros_wide <- cgr_hydros %>% 
  filter(Time_hrs_frompeak %% 1 == 0) %>%          # keep whole-hour values only
  select(Time_hrs_frompeak, Hydro, Flow) %>%
  mutate(Hydro = paste0("Flow_", Hydro)) %>% 
  pivot_wider(names_from = Hydro,values_from = Flow) %>% 
  arrange(Time_hrs_frompeak) %>% 
  mutate(Avg_Flow = rowMeans(across(2:5), na.rm = TRUE)) %>% 
  filter()

# Take Average -----------------------------------------------------------------
avg_hydros <- cgr_hydros_wide %>% 
  select(c(Time_hrs_frompeak,Avg_Flow)) %>% 
  rename(Flow = Avg_Flow) %>% 
  mutate(Hydro = "Avg Flow")

# Bind Row for plotting --------------------------------------------------------
cgr_critdur <- bind_rows(cgr_hydros,avg_hydros)
# Plot -------------------------------------------------------------------------
plotdir <- "D:/0.RMC/Willamette/2025-Report/Figures/Crit_Dur/"

# Set Manual Colors
hydro_colors <- c("Dec-1964" = "#E41A1C", "Dec-1999" = "#377EB8", "Jan-1965" = "#4DAF4A", "Jan-1996" = "#984EA3", "Avg Flow" = "black")
hydro_lines <- c("Dec-1964" = "solid", "Dec-1999" = "solid", "Jan-1965" = "solid", "Jan-1996" = "solid", "Avg Flow" = "dashed")
hydro_sizes <- c("Dec-1964" = 0.5, "Dec-1999" = 0.5, "Jan-1965" = 0.5, "Jan-1996" = 0.5, "Avg Flow" = 1)

colors <- setNames(rainbow(length(df)), GageId[-length(df)])
colors["Average"] <- "black"

ggplot(cgr_critdur, aes(x = Time_hrs_frompeak, y = Flow, group = Hydro)) +
  geom_line(aes(color = Hydro, linetype = Hydro, size = Hydro)) +
  scale_color_manual(values = hydro_colors) +
  scale_linetype_manual(values = hydro_lines) +
  scale_size_manual(values = hydro_sizes) + 
  scale_x_continuous(breaks = seq(-120,168,24),minor_breaks = seq(-120,168,6),limits = c(-48,144)) + 
  scale_y_continuous(breaks = seq(0,50000,10000),minor_breaks = seq(0,50000,1000),labels = scales::label_comma(), limits = c(0,40000)) +
  labs(x = "Hours from Peak Inflow", y = "Inflow (cfs)") + 
  theme(legend.position = "inside", legend.position.inside = c(.85,.5)) +
  annotate(geom="label",x=0, y=6800,label="3-Day Duration",size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) + 
  geom_segment(x = -30, y = 7100, xend = 42, yend = 7100,
               arrow = arrow(length = unit(0.02, "npc"), ends = "both"))


ggsave(paste0(plotdir,"Crit_Duration.png"),width = 7, height = 5,dpi = 300)

# Inflow Hydrograpgs as fraction of max 3-day volume ---------------------------
# Calculate max 3-day inflow vol
cgr_hydros <- cgr_hydros %>% 
  group_by(Hydro) %>% 
  mutate(Inflow_3day = ifelse(Hydro == "Dec-1964" | Hydro == "Jan-1996",
                              zoo::rollmean(Flow,k = 288,fill = NA),
                              zoo::rollmean(Flow,k = 72,fill = NA)),.before = Hydro) %>% 
  ungroup()

# Summary Table
cgr_3day_max <- cgr_hydros %>% 
  group_by(Hydro) %>% 
  summarise(Max_3day = max(Inflow_3day, na.rm = T))

# Add Fractional Column
cgr_hydros <- cgr_hydros %>% 
  group_by(Hydro) %>% 
  mutate(Inflow_3day = ifelse(Hydro == "Dec-1964" | Hydro == "Jan-1996",
                              zoo::rollmean(Flow,k = 288,fill = NA),
                              zoo::rollmean(Flow,k = 72,fill = NA)),.before = Hydro) %>% 
  mutate(Fract_3day_max = Flow/max(Inflow_3day, na.rm = T)) %>% 
  ungroup()

# Plot -------------------------------------------------------------------------
plotdir <- "D:/0.RMC/Willamette/2025-Report/Figures/RFA/"
hydro_colors <- c("Dec-1964" = "#E41A1C", "Dec-1999" = "#377EB8", "Jan-1965" = "#4DAF4A", "Jan-1996" = "#984EA3")

ggplot(cgr_hydros) +
  geom_line(aes(x = Time_hrs, y = Fract_3day_max, color = Hydro)) +
  scale_color_manual(values = hydro_colors) + 
  scale_x_continuous(breaks = seq(0,192,24),minor_breaks = seq(0,192,6),limits = c(0,168)) + 
  scale_y_continuous(breaks = seq(0,2,.5),minor_breaks = seq(0,2,0.1), limits = c(0,2)) +
  labs(x = "Time (hrs)", y = "Inflow (cfs)", color = NULL) + 
  theme(legend.position = "inside", legend.position.inside = c(.85,.75))

ggsave(paste0(plotdir,"Hydro_Shapes.png"),width = 7, height = 5,dpi = 300)
       
       