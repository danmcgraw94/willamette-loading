# Cougar Stage and Flood Seasonallity  -----------------------------------------
# RMC Flood Hazards - Dan McGraw

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, memoise,paletteer)

theme_set(theme_bw())
plotdir <- "/outputs/Figures/Stage_Duration/"

# Stage Duration Function ------------------------------------------------------
stage_duration_funct <- function(stage_timeseries){
  Rank <- rank(stage_timeseries)
  Prob <- Rank/(length(Rank) + 1)
  return(Prob)
}

# Datum Function ---------------------------------------------------------------
ngvd29_to_navd88 <- function(ngvd29){
  navd88 = ngvd29 + 4.38
  return(navd88)
}

# Stage Duration Data ----------------------------------------------------------
stage_duration <- dir_ls("data/Cougar/",glob = "*Stage_Duration.csv*",recurse = T) %>% read_csv()

stage_duration_long <- stage_duration %>% 
  pivot_longer(cols = -Probability, names_to = "Month", values_to = "Elev")

# Use month.name base vector
stage_duration_long <- stage_duration_long %>% 
  mutate(Month = factor(Month, levels = month.name))

# Duration Curve -----------------------
stage_dur <- ggplot(stage_duration_long, aes(x = Probability, y = Elev))+
  geom_line(aes(color = Month),linewidth = 0.85) +
  scale_colour_paletteer_d("ggthemes::Classic_Cyclic") +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Percen of Time Exceeded",
       y = "Elevation (ft-NAVD88)")

# Save
ggsave(paste0(getwd(),plotdir,"Stage_Duration.png"), stage_dur, height = 5, width = 7, dpi=300)

# Read POR --------------------------------------------------------------------
por_daily <- dir_ls("data/",glob = "*CGR_Daily_Stage.csv*",recurse = T) %>%
  read_csv()

por_hourly <- dir_ls("data/",glob = "*CGR_Hourly_Stage.csv*",recurse = T) %>%
  read_csv() %>% select(-Ord)

# Convert to NAV88, DT, and Timestep -----------
por_daily <- por_daily %>% 
  mutate(Stage_navd88 = ngvd29_to_navd88(Stage_ngvd29)) %>% 
  mutate(DT = dmy_hms(Date_Time,truncated = 3),
         Days = seq(1:n()),
         WY = ifelse(month(DT) > 9, year(DT) + 1, year(DT)),
         Month =  month(DT,label = T, abbr = F),.before = everything())

por_hourly <- por_hourly %>% 
  mutate(Stage_navd88 = ngvd29_to_navd88(Stage_ngvd29)) %>% 
  mutate(DT = ymd_hms(paste0(Date," ",Time),truncated = 3),
         Hours = seq(1:n()),
         WY = ifelse(month(DT) > 9, year(DT) + 1, year(DT)),
         Month = month(DT,label = T, abbr = F),.before = everything())

# All Season Stage-Duration ----------------------------------------------------
probs <- c(0.999, 0.998, 0.997, 0.996, 0.995, 0.994, 0.993, 0.992, 0.991, 0.990, 
           0.980, 0.970, 0.960, 0.950, 0.940, 0.930, 0.920, 0.910, 0.900,
           0.800, 0.700, 0.600, 0.500, 0.400, 0.300, 0.200, 0.100, 0.090, 
           0.080, 0.070, 0.060, 0.050, 0.040, 0.030, 0.020, 0.010,0.009, 
           0.008, 0.007, 0.006, 0.005, 0.004, 0.003, 0.002, 0.001)

por_daily <- por_daily %>%
  filter(!is.nan(Stage_navd88)) %>% 
  filter(!is.na(Stage_navd88)) %>% 
  mutate(All_szn = stage_duration_funct(Stage_navd88))

por_hourly <- por_hourly %>%
  filter(!is.nan(Stage_navd88)) %>% 
  filter(!is.na(Stage_navd88)) %>% 
  mutate(All_szn_Probability = stage_duration_funct(Stage_navd88))

# By Month -------------
por_daily_duration <- por_daily %>%
  filter(!is.nan(Stage_navd88)) %>%
  filter(!is.na(Stage_navd88)) %>%
  group_by(Month) %>% 
  summarise(Probability = probs,
            Exceed = 1-Probability,
            Stage_navd88 = quantile(Stage_navd88,prob = probs)) %>% 
  ungroup() %>% 
  mutate(Month = factor(Month, levels = month.name))
 
por_hourly_duration <- por_hourly %>%
  filter(!is.nan(Stage_navd88)) %>% 
  filter(!is.na(Stage_navd88)) %>% 
  group_by(Month) %>% 
  summarise(Probability = probs,
            Exceed = 1-Probability,
            Stage_navd88 = quantile(Stage_navd88,prob = probs)) %>% 
  ungroup() %>% 
  mutate(Month = factor(Month, levels = month.name))


