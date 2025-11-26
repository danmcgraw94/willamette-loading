# JMD Critical Duration - HMS & RAS Routing
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
pmf <- ("D:/0.RMC/JohnMartin/DQC/data/RFA_Hydrographs/PMF2024/PMF_RAS.csv") %>% read_csv()
pmf <- pmf %>% rename(Hour = Time_hrs)

rfa_hydros <- dir_ls(file_dir, glob = "*RFA_Hydrographs.csv") %>% read_csv()
hms_routing <- dir_ls(file_dir, glob = "*HMS_Routing.csv") %>% read_csv()
hms_routing <- hms_routing %>% 
  rename(Hour = Ord)

hms_inflow <- ggplot(data = hms_routing) +
  geom_line(aes(x = Hour, y = Inflow, color = Event), linewidth = 0.5) +
  scale_color_aaas() +
  scale_y_continuous(breaks = seq(0,80000,5000))+
  scale_x_continuous(breaks = seq(0,360,6))+
  theme_bw()

hms_stage <- ggplot(data = hms_routing, aes(x = Hour, y = Obs_Elevation, color = Event)) +
  geom_line(linewidth = 0.5) +
  geom_line(aes(x = Hour, y = Elevation, color = Event),linewidth = 0.5, linetype = "dashed")+
  scale_color_aaas() +
  scale_y_continuous(breaks = seq(3750,3900,10))+
  scale_x_continuous(breaks = seq(0,360,6))+
  theme_bw()

hms_inflow/hms_stage

# Stage Rate of Change/hour -------
hms_routing <- hms_routing %>% 
  group_by(Event) %>% 
  mutate(Stage_rateperhr = (Elevation - lag(Elevation,1))) %>% 
  ungroup()

# May 1955 ---------------------------------------------------------------------
may1955_inflow_hr <- 99
may1955_stagemax_hr <- 150
duration_1955 <- may1955_stagemax_hr - may1955_inflow_hr

may1955_inflow <- hms_routing %>% 
  filter(Event == "May 1955") %>% 
  ggplot() +
  geom_vline(xintercept = c(may1955_inflow_hr,may1955_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.7,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Inflow),color = "#1F77B4",linewidth = 1) +
  scale_y_continuous(breaks = seq(0,80000,20000),minor_breaks = seq(0,80000,5000),labels = scales::comma)+
  scale_x_continuous(breaks = seq(0,360,24))+
  theme_bw() +
  labs(x = "Time (hour)", y = "Inflow (cfs)")+
  geom_segment(aes(x = may1955_inflow_hr, y = 1e4, xend = may1955_stagemax_hr, yend = 1e4),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 123, y = 8e3, label= paste0(duration_1955,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

may1955_stage <- hms_routing %>% 
  filter(Event == "May 1955") %>% 
  ggplot() +
  geom_vline(xintercept = c(may1955_inflow_hr,may1955_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.5,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Elevation),color = "forestgreen",linewidth = 1) +
  scale_y_continuous(breaks = seq(3750,3900,10),minor_breaks = seq(3750,3900,5),labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,360,24))+
  theme_bw() + 
  labs(x = "Time (hour)", y = "Elevation (ft-NAVD88)")+
  geom_segment(aes(x = may1955_inflow_hr, y = 3795, xend = may1955_stagemax_hr, yend = 3795),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 123, y = 3793, label= paste0(duration_1955,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

may1955routing <- may1955_inflow/may1955_stage

ggsave(paste0(plot_dir,"/CritDur/May_1955_HMS.png"),
       plot = may1955routing, height = 5, width = 7, dpi = 500)


# May 1999 ---------------------------------------------------------------------
may1999_inflow_hr <- 110
may1999_stagemax_hr <- 162
duration_1999 <- may1999_stagemax_hr - may1999_inflow_hr

may1999_inflow <- hms_routing %>% 
  filter(Event == "May 1999") %>% 
  ggplot() +
  geom_vline(xintercept = c(may1999_inflow_hr,may1999_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.7,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Inflow),color = "#1F77B4",linewidth = 1) +
  scale_y_continuous(breaks = seq(0,80000,10000),minor_breaks = seq(0,80000,1000),labels = scales::comma)+
  scale_x_continuous(breaks = seq(0,360,24)) +
  theme_bw() +
  labs(x = "Time (hour)", y = "Inflow (cfs)") + 
  geom_segment(aes(x = may1999_inflow_hr, y = 1.2e4, xend = may1999_stagemax_hr, yend = 1.2e4),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 135, y = 1.15e4, label= paste0(duration_1999,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

may1999_stage <- hms_routing %>% 
  filter(Event == "May 1999") %>% 
  ggplot() +
  geom_vline(xintercept = c(may1999_inflow_hr,may1999_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.7,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Elevation),color = "forestgreen",linewidth = 1) +
  scale_y_continuous(breaks = seq(3750,3900,2),minor_breaks = seq(3750,3900,1),labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,360,24)) +
  theme_bw() + 
  labs(x = "Time (hour)", y = "Elevation (ft-NAVD88)") +
  geom_segment(aes(x = may1999_inflow_hr, y = 3851.5, xend = may1999_stagemax_hr, yend = 3851.5),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 135, y = 3851, label= paste0(duration_1999,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

may1999routing <- may1999_inflow/may1999_stage

ggsave(paste0(plot_dir,"/CritDur/May_1999_HMS.png"),
       plot = may1999routing, height = 5, width = 7, dpi = 500)

# June 1965 --------------------------------------------------------------------
june1965_inflow_hr <- 72
june1965_stagemax_hr <- 132
duration_1965 <- june1965_stagemax_hr - june1965_inflow_hr

june1965_inflow <- hms_routing %>% 
  filter(Event == "June 1965") %>% 
  ggplot() +
  geom_vline(xintercept = c(june1965_inflow_hr,june1965_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.7,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Inflow),color = "#1F77B4",linewidth = 1) +
  scale_y_continuous(breaks = seq(0,80000,10000),minor_breaks = seq(0,80000,2500),labels = scales::comma)+
  scale_x_continuous(breaks = seq(0,360,24)) +
  theme_bw() +
  labs(x = "Time (hour)", y = "Inflow (cfs)") + 
  geom_segment(aes(x = june1965_inflow_hr, y = 5e3, xend = june1965_stagemax_hr, yend = 5e3),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 100, y = 4e3, label= paste0(duration_1965,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

june1965_stage <- hms_routing %>% 
  filter(Event == "June 1965") %>% 
  ggplot() +
  geom_vline(xintercept = c(june1965_inflow_hr,june1965_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.7,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Elevation),color = "forestgreen",linewidth = 1) +
  scale_y_continuous(breaks = seq(3750,3900,20),minor_breaks = seq(3750,3900,5),labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,360,24)) +
  theme_bw() + 
  labs(x = "Time (hour)", y = "Elevation (ft-NAVD88)") +
  geom_segment(aes(x = june1965_inflow_hr, y = 3797, xend = june1965_stagemax_hr, yend = 3797),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 100, y = 3796, label= paste0(duration_1965,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

june1965routing <- june1965_inflow/june1965_stage

ggsave(paste0(plot_dir,"/CritDur/June_1965_HMS.png"),
       plot = june1965routing, height = 5, width = 7, dpi = 500)


# PMF --------------------------------------------------------------------
pmf_inflow_hr <- 30
pmf_stagemax_hr <- 50
duration_pmf <- pmf_stagemax_hr - pmf_inflow_hr

pmf_inflow <- 
  ggplot(data = pmf) +
  geom_vline(xintercept = c(pmf_inflow_hr,pmf_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.7,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Discharge_cfs,color = "Discharge"),linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(x = Hour, y = Inflow_cfs,color = "Inflow"),linewidth = 1) +
  scale_y_continuous(breaks = seq(0,3e6,5e5),minor_breaks = seq(0,3e6,1e5),labels = scales::comma)+
  scale_x_continuous(breaks = seq(0,360,24)) +
  scale_color_manual(values = c("Inflow" = "#1F77B4", "Discharge" = "#B40F20"))+
  theme_bw() +
  labs(x = "Time (hour)", y = "Inflow (cfs)", color = NULL) + 
  theme(legend.position = "inside",legend.position.inside = c(0.75,0.75))+
  geom_segment(aes(x = pmf_inflow_hr, y = 4.5e5, xend = pmf_stagemax_hr, yend = 4.5e5),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 40, y = 3.8e5, label= paste0(duration_pmf,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

pmf_stage <- 
  ggplot(data = pmf) +
  geom_vline(xintercept = c(pmf_inflow_hr,pmf_stagemax_hr),linetype = "dotted",color = "grey30",alpha = 0.7,linewidth = 0.75) +
  geom_line(aes(x = Hour, y = Stage_ft),color = "forestgreen",linewidth = 1) +
  scale_y_continuous(breaks = seq(3750,3900,5),minor_breaks = seq(3750,3900,1),labels = scales::comma)+
  scale_x_continuous(breaks = seq(0,360,24)) +
  theme_bw() + 
  labs(x = "Time (hour)", y = "Elevation (ft-NAVD88)") +
  geom_segment(aes(x = pmf_inflow_hr, y = 3880, xend = pmf_stagemax_hr, yend = 3880),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 40, y = 3879, label= paste0(duration_pmf,"-hours"),size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) 

pmfrouting <- pmf_inflow/pmf_stage

ggsave(paste0(plot_dir,"/CritDur/PMF_RAS.png"),
       plot = pmfrouting, height = 5, width = 7, dpi = 500)
