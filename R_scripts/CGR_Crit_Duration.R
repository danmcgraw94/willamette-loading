# Cougar Critical Duration -----------------------------------------------------
# Repaired Data and script 11/19/2025
# Willamette Loading Report Figures
today = format(Sys.Date(),"%d-%b-%Y")

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo)

# Theme and plot directory
theme_set(theme_bw())
plotdir <- "/outputs/Figures/Critical_Duration/"

# Read Data --------------------------------------------------------------------
dec1964 <- dir_ls("data/Cougar/",glob = "*Dec-1964.csv*",recurse = T) %>% read_csv() %>% mutate(DT = mdy(Date) + hms(Time),.before = everything())
jan1995 <- dir_ls("data/Cougar/",glob = "*Jan-1995.csv*",recurse = T) %>% read_csv() %>% mutate(DT = mdy(Date) + hms(Time),.before = everything())
jan1996 <- dir_ls("data/Cougar/",glob = "*Jan-1996.csv*",recurse = T) %>% read_csv() %>% mutate(DT = mdy(Date) + hms(Time),.before = everything())
dec1999 <- dir_ls("data/Cougar/",glob = "*Dec-1999.csv*",recurse = T) %>% read_csv() %>% mutate(DT = mdy(Date) + hms(Time),.before = everything())

# Create Uniform Timesteps (0.25 hour) --------------------------------------------
# Determine Longest Hydrograph Duration in Days
hydro_list <- list(dec1964 = dec1964,
                   jan1995 = jan1995,
                   jan1996 = jan1996,
                   dec1999 = dec1999)

max_durations <- map_df(hydro_list, function(df) {
  tibble(Max_Duration_hrs = 
           as.numeric(rev(df$DT)[1] - (df$DT)[1],units = "hours"))
  }, .id = "Hydro")

# Create Uniform Timeseries 
cgr_hydros <- tibble(Timestep_hr = seq(0,max(max_durations$Max_Duration_hrs),by = 0.25))

# Add Timestep field to hydrographs
add_timestep <- function(Ord,DT){
  difftime = as.numeric(DT - lag(DT,n=1),units = "hours")
  step_factor = mean(difftime,na.rm = T)
  timestep_hr = (Ord - 1)*step_factor
  return(timestep_hr)
}

# This will only need to be done once, reread csvs if bugged
dec1964 <- dec1964 %>% mutate(Timestep_hr = add_timestep(Ord,DT),.before = everything()) %>% 
  rename("Q_Dec1964" = Flow)
jan1995 <- jan1995 %>% mutate(Timestep_hr = add_timestep(Ord,DT),.before = everything()) %>% 
  rename("Q_Jan1995" = Flow)
jan1996 <- jan1996 %>% mutate(Timestep_hr = add_timestep(Ord,DT),.before = everything()) %>% 
  rename("Q_Jan1996" = Flow)
dec1999 <- dec1999 %>% mutate(Timestep_hr = add_timestep(Ord,DT),.before = everything()) %>% 
  rename("Q_Dec1999" = Flow)

# Join Hydrographs to uniform timeseries -----
cgr_hydros <- cgr_hydros %>%
  left_join(dec1964 %>% select(Timestep_hr,Q_Dec1964), by = "Timestep_hr") %>%
  left_join(jan1995 %>% select(Timestep_hr,Q_Jan1995), by = "Timestep_hr") %>%
  left_join(jan1996 %>% select(Timestep_hr,Q_Jan1996), by = "Timestep_hr") %>%
  left_join(dec1999 %>% select(Timestep_hr,Q_Dec1999), by = "Timestep_hr")

# Col Max
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMax(cgr_hydros %>% select(-Timestep_hr))

# Pivot Longer -----------------------------------------------------------------
cgr_hydros_long <- cgr_hydros %>%
  pivot_longer(cols = starts_with("Q_"),
               names_to = "Hydro",
               values_to = "Flow_cfs",
               names_prefix = "Q_")

# NA Approx
cgr_hydros_long <- cgr_hydros_long %>%
  group_by(Hydro) %>% 
  mutate(Flow_cfs = na.approx(Flow_cfs, x = Timestep_hr, na.rm = FALSE)) %>% 
  ungroup()

# Nicer Hydro Names ---------
cgr_hydros_long <- cgr_hydros_long %>% 
  mutate(Event = case_when(
    Hydro == "Dec1964" ~ "Dec 1964",    
    Hydro == "Jan1995" ~ "Jan 1995",
    Hydro == "Jan1996" ~ "Jan 1996",
    Hydro == "Dec1999" ~ "Dec 1999"))

# Critical Duration - Peak Flow & Scaled Peak Flows Centered -------------------
# Find Peak Flow, Time, and max 3-day volume
three_days <- 3*24*4

peak_Q_sum <- cgr_hydros_long %>% 
  group_by(Event) %>%
  mutate(Inflow_3Day = rollmean(Flow_cfs, k = three_days,fill = NA, align = "right")) %>% 
  summarize(Max_Flow_cfs = max(Flow_cfs, na.rm = TRUE),
            Max_3day_cfs = max(Inflow_3Day, na.rm = TRUE),
            Hour_at_Peak = Timestep_hr[which.max(Flow_cfs)]) %>%
  ungroup()

# Scale Flow to Peak Flow & 3-day ---
cgr_hydros_long <- cgr_hydros_long %>%
  left_join(peak_Q_sum, join_by(Event)) %>% 
  mutate(Flow_Scaled = Flow_cfs/Max_Flow_cfs,
         Flow_Scaled_3day = Flow_cfs/Max_3day_cfs) %>% 
  select(-c(Max_Flow_cfs,Max_3day_cfs))

# Center at Peak ---
cgr_hydros_centered <- cgr_hydros_long %>%
  mutate(Time_Centered = Timestep_hr - Hour_at_Peak) %>%
  select(Event, Time_Centered, Time_Original = Timestep_hr, Flow_cfs, Flow_Scaled, Flow_Scaled_3day)

# Take Average at each time step ---
cgr_hydros_avg <- cgr_hydros_centered %>%
  group_by(Time_Centered) %>%
  summarize(
    Flow_cfs_avg = mean(Flow_cfs, na.rm = FALSE),
    Flow_Scaled_avg = mean(Flow_Scaled, na.rm = FALSE),
    Flow_Scaled_3day_avg = mean(Flow_Scaled_3day, na.rm = FALSE),
    n_events = n(),
    n_non_na = sum(!is.na(Flow_cfs))
  ) %>%
  ungroup()

# Figure Generation ------------------------------------------------------------
# Overlaid plot -----------
event_colors <- c("Dec 1964" = "#4E79A7",
                  "Jan 1995" = "#F28E2B",
                  "Jan 1996" = "#E15759",
                  "Dec 1999" = "#59A14F")

min_hr_center <- floor(min(cgr_hydros_avg$Time_Centered)/24)*24
max_hr_center <- ceiling(max(cgr_hydros_avg$Time_Centered)/24)*24

cgr_hydro_center_overlay <- 
  ggplot() +
  geom_line(data = cgr_hydros_centered, 
            aes(x = Time_Centered, y = Flow_Scaled, group = Event,linetype =  Event),
            color = "grey50",alpha = 0.45, linewidth = 0.75) +
  geom_line(data = cgr_hydros_avg,
            aes(x = Time_Centered, y = Flow_Scaled_avg, color = "Average"), linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = seq(min_hr_center,max_hr_center,24),minor_breaks = seq(min_hr_center,max_hr_center,6)) +
  coord_cartesian(xlim = c(-48,126)) +
  scale_color_manual(values = c("Average" = "maroon")) +
  labs(x = "Hours from Peak Inflow", y = "Fraction of Peak Inflow (cfs)", color = NULL,linetype = NULL) + 
  theme_bw() +
  theme(legend.position = "inside",legend.justification.inside = c(0.80,0.70)) +
  geom_segment(aes(x = -21, y = 0.29, xend = 51, yend = 0.29),
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  annotate(geom="label",x = 15, y = 0.27, label= "3-Day Duration",size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1)

ggsave(paste0(getwd(),plotdir,"Critical_Duration_Overlay.png"), cgr_hydro_center_overlay, height = 6, width = 9, dpi = 500)

# Faceted Plot ------
cgr_hydro_center_facet <- ggplot(cgr_hydros_centered, aes(x = Time_Centered, y = Flow_cfs)) +
  geom_line(aes(color = Event), linewidth = 0.8) +
  #scale_color_manual(values = event_colors)+
  scale_color_aaas()+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min_hr_center,max_hr_center,24),minor_breaks = seq(min_hr_center,max_hr_center,6)) +
  coord_cartesian(xlim = c(-48,126)) +
  facet_wrap(~Event, scales = "free_y", ncol = 2) +
  labs(
    #    title = "RFA Hydrographs - Centered at Peak",
    x = "Hours from Peak Inflow",
    y = "Flow (cfs)") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(paste0(getwd(),plotdir,"Critical_Duration_Facet.png"), cgr_hydro_center_facet, height = 6, width = 9, dpi = 500)

# Patchwork Combined -------
cgr_hydro_stack <- cgr_hydro_center_overlay / cgr_hydro_center_facet

cgr_hydro_side <- cgr_hydro_center_overlay + cgr_hydro_center_facet

ggsave(paste0(getwd(),plotdir,"Critical_Duration_Stack.png"),
       plot = cgr_hydro_stack, height = 7, width = 10, dpi = 500)

ggsave(paste0(getwd(),plotdir,"Critical_Duration_Side.png"),
       plot = cgr_hydro_side, height = 7, width = 11, dpi = 500)

# Fraction of 3-day Volume -----------------------------------------------------
# Overlay ---
cgr_hydro_center_overlay_fract3day <- 
  ggplot() +
  geom_line(data = cgr_hydros_centered, 
            aes(x = Time_Original, y = Flow_Scaled_3day, group = Event,color =  Event),
            alpha = 0.7, linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0,max_hr_center+2,24),minor_breaks = seq(0,max_hr_center+24,6)) +
  coord_cartesian(xlim = c(0,192)) +
  #scale_color_manual(values = event_colors) +
  scale_color_aaas()+
  labs(x = "Time (hours)", y = "Fraction of Max 3-Day Volume (cfs)", color = NULL,linetype = NULL) + 
  theme_bw() +
  theme(legend.position = "inside",legend.justification.inside = c(0.80,0.70))

ggsave(paste0(getwd(),plotdir,"3-day fract/Critical_Duration_3DayFract_Overlay.png"), cgr_hydro_center_overlay_fract3day, height = 6, width = 9, dpi = 500)

# Facet ---
cgr_hydro_fract3day_facet <- ggplot() +
  geom_line(data = cgr_hydros_centered, 
            aes(x = Time_Centered, y = Flow_Scaled_3day, group = Event,color =  Event),alpha = 0.75, linewidth = 0.85) +
  facet_wrap(~Event, scales = "fixed", ncol = 2) +
  scale_x_continuous(breaks = seq(-96,120,24),labels = (seq(-96,120,24) + 48))+
  coord_cartesian(xlim = c(-48,120)) +
  #scale_color_manual(values = event_colors) +
  scale_color_aaas()+
  labs(x = "Hours", y = "Fraction of Max 2-Day Volume (cfs)", color = NULL,linetype = NULL) + 
  theme_bw() +
  theme(legend.position = "none")

ggsave(paste0(getwd(),plotdir,"3-day fract/CGR_Hydros_3DayFract_Facet.png"),
       plot = cgr_hydro_fract3day_facet, height = 6, width = 9, dpi = 500)

# Patchwork Combined -------
cgr_3day_stack <- cgr_hydro_center_overlay_fract3day / cgr_hydro_fract3day_facet

cgr_3day_side <- cgr_hydro_center_overlay_fract3day + cgr_hydro_fract3day_facet

ggsave(paste0(getwd(),plotdir,"3-day fract/Critical_Duration_Stack.png"),
       plot = cgr_3day_stack, height = 7, width = 10, dpi = 500)

ggsave(paste0(getwd(),plotdir,"3-day fract/Critical_Duration_Side.png"),
       plot = cgr_3day_side, height = 7, width = 11, dpi = 500)
