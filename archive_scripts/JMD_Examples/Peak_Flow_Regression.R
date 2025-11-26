# Peak Flow Regression Update
# 10/23/2025
# Rewriting Code to fill gaps & correct artifacts from .rdata files

# Libraries --------------------------------------------------------------------
library(dataRetrieval)
library(tidyverse)
library(gt)
library(lubridate)
library(scales)
library(zoo)
library(patchwork)
source("D:/R/theme_USACE.r")
theme_set(theme_bw())

# DQC Data Export Directory ----------------------------------------------------
dqc_data_export_dir <- "D:/0.RMC/JohnMartin/DQC/data/Peak_regression_exports/"

# USGS data --------------------------------------------------------------------
# Arkansas & Purgatoire
site_numbers <- c("07124000", "07128500")

# Parameter Code - # Discharge in cubic feet per second (cfs
parameter_cd <- "00060"

# Start Date should encompass all available data
start_date <- "1912-10-01"
end_date <- Sys.Date()

# Site Info and Map using new USGS functions
# Site Info and Map ------------------------------------------------------------
sites_info <- read_waterdata_monitoring_location(monitoring_location_id = c("USGS-07124000","USGS-07128500"))

# Daily Discharge Data ---------------------------------------------------------
daily_data_long <- site_numbers %>%
  lapply(function(site) {
    readNWISdv(
      siteNumbers = site,
      parameterCd = parameter_cd,
      startDate = start_date,
      endDate = end_date
    ) %>%
      renameNWISColumns() %>%
      mutate(site_no = site)
  }) %>%
  bind_rows()

# Add Columns for River Name
daily_data_long <- daily_data_long %>% 
  mutate(River_Name = ifelse(site_no == "07124000","Arkansas","Purgatoire"))

# Pivot Wider to line up flow on same date
daily_data_wide <- daily_data_long %>%
  dplyr::select(agency_cd, Date, River_Name, Flow) %>%
  pivot_wider(
    names_from = River_Name,
    values_from = Flow,
    names_glue = "{River_Name}_Daily"
  )

# Create timeseries for entire record. Include timestamps with missing data
# Start with earliest recorded value
first_date <- min(daily_data_long$Date)
full_dates <- tibble(Date = seq.Date(from = as.Date(first_date), to = Sys.Date(), by = "day"))

# Join the daily data from both sources by date so they are aligned
daily_usgs <- full_dates %>%
  left_join(daily_data_wide, by = "Date") %>%
  dplyr::select(Date, Arkansas_Daily, Purgatoire_Daily) %>%
  mutate(DT = as.POSIXct(Date),.before = everything(),
         WY = ifelse(month(DT) > 9, year(DT)+1, year(DT))) %>%
  dplyr::select(-Date)

# Subset the daily data by location
ark_daily <- daily_usgs %>% dplyr::select(DT, Arkansas_Daily)
purg_daily <- daily_usgs %>% dplyr::select(DT, Purgatoire_Daily)

# Remove daily data wide/long (this can be recreated if needed)
# tryCatch(expr = {rm(daily_data_wide)},
#          error = function(e) {print (e)},
#          warning = function(w) {print (w)},
#          finally = {"daily_data_long gone"})
# 
# tryCatch(expr = {rm(daily_data_long)},
#          error = function(e) {print (e)},
#          warning = function(w) {print (w)},
#          finally = {"daily_data_long gone"})

# Keep memory low 
gc()

# Peak Discharge Data ----------------------------------------------------------
peak_data_long <- site_numbers %>%
  lapply(function(site) {
    readNWISpeak(
      siteNumbers = site,
      startDate = start_date,
      endDate = end_date
    ) %>%
      renameNWISColumns() %>%
      mutate(site_no = site)
  }) %>%
  bind_rows()

# Add Columns for River Name & Format Datetime
peak_data_long <- peak_data_long %>%
  mutate(River_Name = ifelse(site_no == "07124000", "Arkansas", "Purgatoire"),
         DT = as.POSIXct(peak_dt)) %>%
  dplyr::select(DT, River_Name, Peak_Flow = peak_va)

# Subset the peak data by location
ark_peaks <- peak_data_long %>%
  filter(River_Name == "Arkansas") %>%
  mutate(WY = ifelse(month(DT) > 9, year(DT)+1, year(DT)),.before = everything()) %>%
  rename(Ark_DT = DT,
         Ark_Peak = Peak_Flow) %>%
  dplyr::select(-River_Name)

purg_peaks <- peak_data_long %>%
  filter(River_Name == "Purgatoire") %>%
  mutate(WY = ifelse(month(DT) > 9, year(DT)+1, year(DT)),.before = everything()) %>%
  rename(Purg_DT = DT,
         Purg_Peak = Peak_Flow)

# Join the sites by Water Year
USGS_peaks_raw <- left_join(ark_peaks,purg_peaks, by = "WY")

# Calculate Days between Peak DT
USGS_peaks_raw <- USGS_peaks_raw %>%
  mutate(Days_Apart = as.numeric(difftime(USGS_peaks_raw$Ark_DT, USGS_peaks_raw$Purg_DT, units = "days")))

# Determine which are within 1 day of each other
USGS_peaks <- USGS_peaks_raw %>%
  mutate(Same_Event = abs(Days_Apart)<= 1)

# Find Daily Flows on the Peak Dates 
USGS_peaks <- USGS_peaks %>% left_join(ark_daily, by = c("Ark_DT" = "DT"))
USGS_peaks <- USGS_peaks  %>% left_join(purg_daily, by = c("Purg_DT" = "DT"))

# Remove peak_data_long and USGS_peaks_raw (this can be recreated if needed)
# tryCatch(expr = {rm(peak_data_long)},
#          error = function(e) {print (e)},
#          warning = function(w) {print (w)},
#          finally = {"peak_data_long gone"})

# Keep memory low 
gc()

# Define Coincident Peaks ------------------------------------------------------
# Include daily data
USGS_peaks_coincident <- USGS_peaks %>%
  filter(Same_Event) %>%
  mutate(Combined_Peak = Ark_Peak + Purg_Peak,
         Combined_daily = Arkansas_Daily + Purgatoire_Daily)

# Export Coincident Peaks
write_csv(USGS_peaks_coincident,paste0(dqc_data_export_dir,"USGS_Coincident_Peaks.csv"))

# Instantaneous Discharge Data -------------------------------------------------
ark_instant_raw <- read.csv("D:/0.RMC/JohnMartin/DQC/data/USGS_Instantaneous/Arkansas_Las_Animas_15min.csv",header = T)
purg_instant_raw <- read.csv("D:/0.RMC/JohnMartin/DQC/data/USGS_Instantaneous/Purg_Las_Animas_15min.csv",header = T)

# Format Datetimes & Extract Water Year
ark_instant <- ark_instant_raw %>% 
  mutate(DT_MTN = mdy_hm(datetime),.before = everything()) %>% 
  mutate(DT_UTC = DT_MTN - hours(6),.before = everything()) %>% 
  mutate(DATE_UTC = floor_date(DT_UTC, "day"),.before = everything()) %>% 
  mutate(WY = ifelse(month(DT_UTC) > 9, year(DT_UTC)+1, year(DT_UTC)),.before = everything()) %>% 
  dplyr::select(-c(datetime,tz_cd,QA_Code,agency_cd,site_no))

purg_instant <- purg_instant_raw %>% 
  mutate(DT_MTN = mdy_hm(datetime),.before = everything()) %>% 
  mutate(DT_UTC = DT_MTN - hours(6),.before = everything()) %>% 
  mutate(DATE_UTC = floor_date(DT_UTC, "day"),.before = everything()) %>% 
  mutate(WY = ifelse(month(DT_UTC) > 9, year(DT_UTC)+1, year(DT_UTC)),.before = everything()) %>% 
  dplyr::select(-c(datetime,tz_cd,QA_Code,agency_cd,site_no))

# Join the time series and add the instantaneous flow to estimate combined flow
combined_instant <- left_join(ark_instant, dplyr::select(purg_instant, c(DT_UTC,Purg_instant)), by = "DT_UTC")

# Estimate combined instantaneous
combined_instant <- combined_instant %>% 
  mutate(USGS_combined_instant = Ark_instant + Purg_instant)

# # Query the max daily -                                    <<<<  Come back to this: not sure if this is needed
# combined_instant_max_daily <- combined_instant %>%
#   group_by(DATE_UTC) %>%
#   slice_max(USGS_combined_instant, n=1, with_ties = F) %>%
#   ungroup()

# Find AMS of each gage
ark_instant_AMS <- ark_instant %>%
  group_by(WY) %>%
  slice_max(Ark_instant, n=1, with_ties = F)

purg_instant_AMS <- purg_instant %>%
  group_by(WY) %>%
  slice_max(Purg_instant, n=1, with_ties = F)

# Find AMS of combined time series
USGS_combined_instant_AMS <- combined_instant %>%
  group_by(WY) %>%
  slice_max(USGS_combined_instant, n=1, with_ties = F)

# Join to Coincident peaks data as a check
# USGS_peaks_coincident_check <- USGS_peaks_coincident %>%
#   left_join(dplyr::select(USGS_combined_instant_AMS,c(WY,DATE_UTC,Ark_instant,Purg_instant,USGS_combined_instant)),
#             by = c("Ark_DT" = "DATE_UTC")) %>%
#   dplyr::select(c("WY.x","Ark_DT","Ark_Peak", "Purg_Peak","Days_Apart","Combined_Peak","Ark_instant","Purg_instant","USGS_combined_instant" )) %>% 
#   rename(DT = Ark_DT, WY = WY.x) 

# Export Combined Instantaneous Peaks
write_csv(USGS_peaks_coincident_check,paste0(dqc_data_export_dir,"USGS_Coincident_Peaks_check.csv"))

# Remove combined_instant and ark_instant,purg_instant (this can be recreated if needed)
# tryCatch(expr = {rm(combined_instant)},
#          error = function(e) {print (e)},
#          warning = function(w) {print (w)},
#          finally = {"combined instant gone"})

# tryCatch(expr = {rm(ark_instant)},
#          error = function(e) {print (e)},
#          warning = function(w) {print (w)},
#          finally = {"ark_instant gone"})
# 
# tryCatch(expr = {rm(purg_instant)},
#          error = function(e) {print (e)},
#          warning = function(w) {print (w)},
#          finally = {"purg_instant gone"})

# Keep memory low 
gc()

# Water Control Manual Peaks & know peak/daily from report ---------------------
peak_vol <- tibble(
  WY = c(1921,1927,1929,1936,1942,1949,1954,1955,1965,1999),
  PeakQ = c(187000, 22000,45000,36300,75000,48500,38900,88000,163000,38100),
  PtV_1day = c(2.14,2.29,1.81,2.52,2.25,3.39,2.24,1.22, 1.97,1.44),
  PtV_2day = c(2.52,2.47,1.88,2.54,2.30,3.81,3.13,1.52,2.76,1.51),
  PtV_3day = c(3.09,2.67,2.47,3.09,2.28,4.95,4.40,1.97,3.58,1.75),
  PtV_4day = c(3.57,2.82,3.01,3.74,2.45,5.85,5.16,2.42,4.21,1.99),
  PtV_5day = c(5.01,2.85,4.60,5.53,3.09,8.46,8.51,3.89,6.50,2.84))

# Make it long for Peak to Daily conversion
peak_vol_long <- peak_vol %>%
  pivot_longer(
    cols = starts_with("PtV_"),
    names_to = "Duration",
    values_to = "PtV") %>% 
  mutate(Inflow_Vol = PeakQ/PtV)

# Filter to just peak & 1 day
WCM_peaks <- peak_vol_long %>% 
  filter(Duration == "PtV_1day") %>% 
  rename(DailyQ = Inflow_Vol) %>% 
  mutate(Source = "WCM Peaks") %>% 
  mutate(DT = NA, .after = WY) %>%
  dplyr::select(WY, DT, DailyQ, PeakQ, Source)

# Export WCM Peaks
write_csv(WCM_peaks,paste0(dqc_data_export_dir,"WCM_Peaks.csv"))

# Create Combined Peak & Daily Flow Dataset ------------------------------------
# Save state DFs
USGS_peaks_coincident0 <- USGS_peaks_coincident
USGS_combined_instant_AMS0 <- USGS_combined_instant_AMS

# Format Data for bind rows
USGS_peaks_coincident_AMS <- USGS_peaks_coincident %>% 
  dplyr::select(WY,Ark_DT,Combined_daily,Combined_Peak) %>% 
  rename(DT = Ark_DT,DailyQ = Combined_daily, PeakQ = Combined_Peak) %>% 
  mutate(Source = "USGS Coincident Peaks")

# Format Data for bind rows
USGS_combined_instant_AMS <- USGS_combined_instant_AMS %>% 
  dplyr::select(WY,DATE_UTC, USGS_combined_instant) %>% 
  rename(DT = DATE_UTC,PeakQ = USGS_combined_instant) %>% 
  filter(WY != 1988) %>% 
  mutate(DailyQ = NA,
         Source = "USGS Combined Instantaneous")

# Bind USGS Data and remove smaller value when duplicate WYs (typically the instant value)
USGS_only <- bind_rows(USGS_peaks_coincident_AMS,USGS_combined_instant_AMS) %>%
  arrange(WY, desc(PeakQ)) %>%
  distinct(WY, .keep_all = TRUE)

# Export
write_csv(USGS_only,paste0(dqc_data_export_dir,"USGS_data_Only.csv"))

# Merge the WCM Peaks data. Keep the WCM Peak whenever there is a duplicate WY --
USGS_WCM <- bind_rows(USGS_only, WCM_peaks) %>%
  arrange(WY, desc(Source == "WCM Peaks")) %>%  # WCM Peaks always first
  distinct(WY, .keep_all = TRUE)

# Use USGS Daily for unknown daily flows ------------------------------------------
# Calculate Combined Daily USGS
daily_usgs <- daily_usgs %>% 
  mutate(USGS_Combined_Daily = replace_na(Arkansas_Daily, 0) + replace_na(Purgatoire_Daily, 0))

# Join to estimate missing daily USGS 
USGS_WCM <- USGS_WCM %>% left_join(dplyr::select(daily_usgs,c(DT,USGS_Combined_Daily)), by = "DT") %>% 
  mutate(DailyQ = ifelse(is.na(DailyQ),USGS_Combined_Daily,DailyQ)) %>% 
  dplyr::select(-USGS_Combined_Daily)

save(USGS_WCM,file = "D:/0.RMC/JohnMartin/DQC/R/rdata/usgs_wcm.Rdata")

# Export USGS WCM combined (final) data set ------------------------------------
write_csv(USGS_WCM,paste0(dqc_data_export_dir,"USGS_WCM.csv"))

# John Martin Hourly Data ------------------------------------------------------
JMD_Hourly <- read_csv("D:/0.RMC/JohnMartin/DQC/data/JMD/Hourly_Inflow.csv")

# 6 Hour Smoothing
JMD_Hourly <- JMD_Hourly %>%
  mutate(Inflow_Smooth_CFS_6hr = rollmean(Inflow_Smooth_CFS,k = 6,fill = NA, align = "right",na.rm=TRUE),
         DT = dmy_hm(gsub(",","",DT)))

# JMD Hourly AMS
JMD_Hourly_AMS <- JMD_Hourly %>%
  filter(Inflow_Smooth_CFS_6hr < 35000) %>% 
  mutate(WY = ifelse(month(DT) > 9, year(DT)+1, year(DT))) %>%
  group_by(WY) %>%
  slice_max(Inflow_Smooth_CFS_6hr, n=1, with_ties = F) %>% 
  select(WY,DT, Inflow_Smooth_CFS_6hr) %>% 
  ungroup() %>% 
  rename(JMD_Inflow = Inflow_Smooth_CFS_6hr)

# John Martin Daily Data -------------------------------------------------------
# Daily Inflow Data back to 1912
JMD_Daily <- read_csv("D:/0.RMC/JohnMartin/DQC/data/JMD/JMD_POR_Daily_alldata.csv")

# Format cols
JMD_Daily <- JMD_Daily %>%
  mutate(DT = ymd(Date),.before = everything()) %>%
  select(DT,Daily_Inflow) %>% 
  rename(JMD_Daily = Daily_Inflow)

# JMD_Daily %>% filter(year(DT) == 1936) %>% slice_max(JMD_Daily, n = 5, with_ties = TRUE)

# Compare JMD Daily and USGS Daily
USGS_JMD_Daily <- daily_usgs %>% left_join(select(JMD_Daily,c(DT,JMD_Daily)), by = "DT") %>% 
  mutate(USGS_Combined_Daily = replace_na(Arkansas_Daily, 0) + replace_na(Purgatoire_Daily, 0))

# JMD 1-day AMS 
jmd_AMS_1day <- JMD_Daily %>%
  mutate(WY = ifelse(month(DT) > 9, year(DT)+1, year(DT)),.before = everything()) %>%
  group_by(WY) %>%
  slice_max(JMD_Daily, n=1, with_ties = F)

# BUILD REGRESSION -------------------------------------------------------------
# Build Regression #############################################################
USGS_WCM_savestate <- USGS_WCM
USGS_WCM$DailyQ[USGS_WCM$WY == 2000] <- 1100 # Obtained from JMD inflow. I believe this point is bugged

peak_source_colors <- c("WCM Peaks" = "#D55E00",
                        "USGS Coincident Peaks" = "#228833", 
                        "USGS Combined Instantaneous" = "#CCBB44")

ggplot(data = USGS_WCM,aes(x = DailyQ, y = PeakQ))  +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",color = "red3",alpha = 0.5)+
  geom_point(aes(color = Source),alpha = 0.85) + 
  scale_x_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(),labels = scales::comma)+
  scale_y_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(),labels = scales::comma)+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,color = "grey20") +
  #coord_cartesian(xlim = c(7E2,3E5),ylim = c(7E2,3E5)) +
  labs(x = "Daily Avg Inflow (cfs)", y = "Peak Flow (cfs)")+
  scale_color_manual(values = peak_source_colors) +
  theme(legend.position = "inside",legend.position.inside = c(.75,.25))


# Regression Models ------------------------------------------------------------
model_linear <- lm(PeakQ ~ DailyQ, data = USGS_WCM)
model_log <- lm(log(PeakQ) ~ log(DailyQ), data = USGS_WCM)
model_gam <- mgcv::gam(log(PeakQ) ~ s(log(DailyQ)), data = USGS_WCM, method = "REML")
model_poly <- lm(PeakQ ~ poly(DailyQ, 2, raw = TRUE), data = USGS_WCM)
model_poly_log <- lm(log(PeakQ) ~ poly(log(DailyQ), 2, raw = TRUE), data = USGS_WCM)

# Save Models & rdata to this point
save.image(file = "D:/0.RMC/JohnMartin/DQC/R/peak_regression_models.RData")

# QUALITY METRICS --------------------------------------------------------------

# USGS Peaks Coincident --------------------------------------------------------
USGS_peaks_coincident <- USGS_peaks_coincident %>% 
  rowwise() %>% 
  mutate(lower_peak = min(Ark_Peak,Purg_Peak))

# USGS Instant - JMD Hourly Correlation ----------------------------------------
USGS_overlap_period <- combined_instant %>%
  filter(year(DT_UTC) %in% c(1998:2018))

JMD_Hourly_overlap_period <- JMD_Hourly %>% 
  filter(year(DT) %in% c(1998:2018))

JMD_USGS_overlap <- left_join(USGS_overlap_period,
                              select(JMD_Hourly_overlap_period, c(DT,Inflow_Smooth_CFS_6hr)),
                              by = join_by(DT_MTN == DT))

overlap_complete <- JMD_USGS_overlap %>%
  filter(!is.na(Inflow_Smooth_CFS_6hr)) %>% 
  filter(!is.na(USGS_combined_instant)) %>% 
  mutate(test_ratio = Inflow_Smooth_CFS_6hr/USGS_combined_instant) %>% 
  filter(between(test_ratio,-50,50))

overlap_complete <- overlap_complete %>%
  mutate(residual = Inflow_Smooth_CFS_6hr - USGS_combined_instant,
         abs_residual = abs(residual),
         pct_diff = (residual / USGS_combined_instant) * 100,
         abs_pct_diff = abs(pct_diff))

# Linear model
lm_fit <- lm(Inflow_Smooth_CFS_6hr ~ USGS_combined_instant, data = overlap_complete)
lm_summary <- summary(lm_fit)

# RMSE
rmse <- sqrt(mean((overlap_complete$Inflow_Smooth_CFS_6hr - 
                     overlap_complete$USGS_combined_instant)^2))

# Mean absolute error
mae <- mean(abs(overlap_complete$Inflow_Smooth_CFS_6hr - 
                  overlap_complete$USGS_combined_instant))

# Correlation
cor_pearson <- cor(overlap_complete$USGS_combined_instant, 
                   overlap_complete$Inflow_Smooth_CFS_6hr,
                   method = "pearson")

cor_spearman <- cor(overlap_complete$USGS_combined_instant,
                    overlap_complete$Inflow_Smooth_CFS_6hr,
                    method = "spearman")

# Show percentage difference between low and higher flows
quantile_analysis <- overlap_complete %>%
  mutate(Flow_Percentile = case_when(
    USGS_combined_instant <= quantile(USGS_combined_instant, 0.25) ~ "0-25th",
    USGS_combined_instant <= quantile(USGS_combined_instant, 0.50) ~ "26-50th",
    USGS_combined_instant <= quantile(USGS_combined_instant, 0.75) ~ "51-75th",
    USGS_combined_instant <= quantile(USGS_combined_instant, 0.90) ~ "76-90th",
    TRUE ~ "91-100th"
  )) %>%
  group_by(Flow_Percentile) %>%
  summarise(
    n = n(),
    Mean_USGS = round(mean(USGS_combined_instant), 0),
    Mean_JMD = round(mean(Inflow_Smooth_CFS_6hr), 0),
    Mean_Abs_Error = round(mean(abs_residual), 0),
    Mean_Abs_Pct = round(mean(abs_pct_diff), 1),
    Median_Abs_Pct = round(median(abs_pct_diff), 1),
    .groups = "drop"
  ) %>% 
  mutate(Upper_Flow = c(scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.25)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.5)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.75)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.9)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 1.0))),
         Lower_Flow = c(scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.01)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.26)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.51)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.76)),
                        scales::comma(quantile(overlap_complete$USGS_combined_instant, 0.91))),
         Flow_Range = paste0(Lower_Flow," - ",Upper_Flow),.before = n) %>% 
  select(-c(Upper_Flow,Lower_Flow))

# Exclude very low flows
pct_diff_gt100 <- overlap_complete %>%  filter(USGS_combined_instant > 100) %>%  pull(abs_pct_diff) %>%  mean()

pct_diff_gt500 <- overlap_complete %>%  filter(USGS_combined_instant > 500) %>%  pull(abs_pct_diff) %>%  mean()

pct_diff_gt1000 <- overlap_complete %>%  filter(USGS_combined_instant > 1000) %>%  pull(abs_pct_diff) %>%  mean()

pct_diff_gt10000 <- overlap_complete %>%  filter(USGS_combined_instant > 10000) %>%  pull(abs_pct_diff) %>%  mean()

# Create summary table
summary_table <- tibble(
  Metric = c(
    "Pearson Correlation (r) - All Data", 
    "R-squared - All Data",
    "RMSE", 
    "MAE",
    "Relative RMSE",
    "Mean Abs % Diff (flows > 500 cfs)",
    "Mean Abs % Diff (flows > 10,000 cfs)",
    "Slope (JMD ~ USGS)",
    "Intercept",
    "Sample Size"
  ),
  Value = c(
    round(cor_pearson, 4),
    round(cor_pearson^2, 4),
    paste0(round(rmse, 0), " cfs"),
    paste0(round(mae, 0), " cfs"),
    paste0(round(rmse / mean(overlap_complete$USGS_combined_instant) * 100, 1), "%"),
    paste0(round(pct_diff_gt500, 1), "%"),
    paste0(round(pct_diff_gt10000, 1), "%"),
    round(coef(lm_fit)[2], 4),
    paste0(round(coef(lm_fit)[1], 0), " cfs"),
    scales::comma(nrow(overlap_complete))
  )
)

# Lag Sensitivity --------------------------------------------------------------
# Test lags from 0 to 12 hours (in 30 minute increments)
lag_minutes <- seq(-180, 720, by = 30)  # -3 to +6 hours in 30 min increments

lag_correlations <- map_dfr(lag_minutes, function(lag) {
  
  # Shift USGS time by lag (in minutes)
  USGS_lagged <- overlap_complete %>%
    mutate(DT_MTN_lagged = DT_MTN + minutes(lag))
  
  # Join with lagged time
  overlap_lagged <- left_join(
    USGS_lagged,
    select(overlap_complete, c(DT, Inflow_Smooth_CFS_6hr)),
    by = join_by(DT_MTN_lagged == DT))
  
  # Calculate correlation
  if(nrow(overlap_lagged) > 1000) {
    cor_value <- cor(overlap_lagged$USGS_combined_instant, 
                     overlap_lagged$Inflow_Smooth_CFS_6hr,
                     method = "pearson")
    
    rmse_value <- sqrt(mean((overlap_lagged$Inflow_Smooth_CFS_6hr - 
                               overlap_lagged$USGS_combined_instant)^2))
  } else {
    cor_value <- NA
    rmse_value <- NA
  }
  
  tibble(
    Lag_Minutes = lag,
    Lag_Hours = lag / 60,
    Correlation = cor_value,
    R_squared = cor_value^2,
    RMSE = rmse_value,
    n = nrow(overlap_lagged)
  )
})

# Find optimal lag
optimal_lag <- lag_correlations %>%
  filter(!is.na(R_squared)) %>%
  slice_max(R_squared, n = 1)

# Statistical Comparison of Peak Flow Sources ----------------------------------
# Identify which records come from which source
peak_verification <- USGS_WCM %>%
  mutate(
    Year = WY,
    Date = as.Date(DT)) %>%
  select(Year, Date, PeakQ, DailyQ, Source)

# Summary table of data sources
source_summary <- peak_verification %>%
  group_by(Source) %>%
  summarise(Count = n(),
            Year_Range = paste(min(Year), "-", max(Year)),
            Avg_PeakQ = round(mean(PeakQ), 0),
            Avg_DailyQ = round(mean(DailyQ), 0))

wcm_years <- USGS_WCM %>% 
  filter(Source == "WCM") %>%
  pull(WY)

# Calculate ratio of Peak to Daily flow by source
ratio_analysis <- peak_verification %>%
  mutate(Peak_Daily_Ratio = PeakQ / DailyQ) %>%
  group_by(Source) %>%
  summarise(
    Count = n(),
    Mean_Ratio = round(mean(Peak_Daily_Ratio), 2),
    Median_Ratio = round(median(Peak_Daily_Ratio), 2),
    SD_Ratio = round(sd(Peak_Daily_Ratio), 2),
    Min_Ratio = round(min(Peak_Daily_Ratio), 2),
    Max_Ratio = round(max(Peak_Daily_Ratio), 2)
  )

ratio_data <- peak_verification %>%
  mutate(Peak_Daily_Ratio = PeakQ / DailyQ)

# ANOVA Test
anova_result <- aov(Peak_Daily_Ratio ~ Source, data = ratio_data)

# Create a summary table for your report
verification_table <- peak_verification %>%
  group_by(Source) %>%
  summarise(
    Number_of_Events = n(),
    Year_Range = paste(min(Year), "-", max(Year)),
    Peak_Flow_Range_cfs = paste(scales::comma(min(PeakQ)), "-", scales::comma(max(PeakQ))),
    mean_peak_to_daily_ratio = round(mean(PeakQ / DailyQ), 2)
  )

# Pairwise comparisons
pairwise_test <- TukeyHSD(anova_result)

# Detailed table
detailed_comparison <- ratio_data %>%
  group_by(Source) %>%
  summarise(
    N = n(),
    Mean_Ratio = round(mean(Peak_Daily_Ratio), 3),
    Median_Ratio = round(median(Peak_Daily_Ratio), 3),
    SD_Ratio = round(sd(Peak_Daily_Ratio), 3),
    Min_Ratio = round(min(Peak_Daily_Ratio), 3),
    Max_Ratio = round(max(Peak_Daily_Ratio), 3)
  ) %>%
  arrange(Mean_Ratio)

anova_table <- broom::tidy(anova_result) %>%
  mutate(
    term = case_when(
      term == "Source_Category" ~ "Between Sources",
      term == "Residuals" ~ "Within Sources (Error)",
      TRUE ~ term
    )
  )

# Extract Tukey results and clean encoding
tukey_table <- broom::tidy(pairwise_test) %>%
  mutate(
    # Clean up comparison names - avoid special characters
    contrast = case_when(
      grepl("Matching Peaks.*Instantaneous", contrast) ~ 
        "Matching Peaks vs Instantaneous Combined",
      grepl("WCM.*Instantaneous", contrast) ~ 
        "WCM vs Instantaneous Combined",
      grepl("WCM.*Matching", contrast) ~ 
        "WCM vs Matching Peaks",
      TRUE ~ as.character(contrast)
    ),
    # Add significance indicator
    Significance = case_when(
      adj.p.value < 0.001 ~ "***",
      adj.p.value < 0.01 ~ "**",
      adj.p.value < 0.05 ~ "*",
      adj.p.value < 0.1 ~ ".",
      TRUE ~ "ns"
    )
  ) %>%
  select(contrast, estimate, adj.p.value, Significance)

# Prior Empirical CDF ----------------------------------------------------------
weibull_peak <- USGS_WCM %>%
  arrange(PeakQ) %>%
  mutate(
    rank = row_number(),
    weibull_prob = rank / (n() + 1),
    exceedance_prob = 1 - weibull_prob
  )

# MODEL METRICS ----------------------------------------------------------------
# Function to calculate Rsq manually
calc_r2 <- function(observed, predicted) {
  ss_res <- sum((observed - predicted)^2)
  ss_tot <- sum((observed - mean(observed))^2)
  return(1 - ss_res / ss_tot)
}

# --- LINEAR MODEL ---
linear_r2_real <- summary(model_linear)$r.squared
linear_pred_real <- predict(model_linear)
linear_r2_log <- calc_r2(log(USGS_WCM$PeakQ), log(linear_pred_real))

# --- LOG-LINEAR MODEL ---
log_r2_log <- summary(model_log)$r.squared
log_pred_log <- predict(model_log)
log_pred_real <- exp(log_pred_log)
log_r2_real <- calc_r2(USGS_WCM$PeakQ, log_pred_real)

# --- GAM MODEL ---
y_log <- log(USGS_WCM$PeakQ)
gam_pred_log <- predict(model_gam)
gam_r2_log <- calc_r2(y_log, gam_pred_log)
gam_pred_real <- exp(gam_pred_log)
gam_r2_real <- calc_r2(USGS_WCM$PeakQ, gam_pred_real)

# --- POLYNOMIAL MODEL ---
poly_r2_real <- summary(model_poly)$r.squared
poly_pred_real <- predict(model_poly)
poly_r2_log <- calc_r2(log(USGS_WCM$PeakQ), log(poly_pred_real))

# --- LOG-POLYNOMIAL MODEL ---
poly_log_r2_log <- summary(model_poly_log)$r.squared
poly_log_pred_log <- predict(model_poly_log)
poly_log_pred_real <- exp(poly_log_pred_log)
poly_log_r2_real <- calc_r2(USGS_WCM$PeakQ, poly_log_pred_real)

# Create comprehensive R_squared table
r2_comparison <- tibble(
  Model = c("Linear", "Log-Linear", "Log-GAM", "Polynomial", "Log-Polynomial"),
  Fit_Space = c("Real", "Log", "Log", "Real", "Log"),
  Rsq_Log_Space = round(c(linear_r2_log, log_r2_log, gam_r2_log, 
                          poly_r2_log, poly_log_r2_log), 4),
  Rsq_Real_Space = round(c(linear_r2_real, log_r2_real, gam_r2_real, 
                           poly_r2_real, poly_log_r2_real), 4)
)

# Calculate RMSE in log space
rmse_log <- tibble(
  Model = c("Linear", "Log-Linear", "Log-GAM", "Polynomial", "Log-Polynomial"),
  RMSE_Log_Space = c(
    sqrt(mean((log(USGS_WCM$PeakQ) - log(linear_pred_real))^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - log_pred_log)^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - gam_pred_log)^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - log(poly_pred_real))^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - poly_log_pred_log)^2))
  )
)

# Calculate RMSE in real space (what you already had)
rmse_real <- tibble(
  Model = c("Linear", "Log-Linear", "Log-GAM", "Polynomial", "Log-Polynomial"),
  RMSE_Real_Space = c(
    sqrt(mean((USGS_WCM$PeakQ - linear_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - log_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - gam_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - poly_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - poly_log_pred_real)^2))
  )
)

rmse_comparison <- left_join(rmse_log, rmse_real, by = "Model") %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# Comprehensive Metrics
aics <- AIC(model_linear, model_log, model_gam, model_poly, model_poly_log)
bics <- BIC(model_linear, model_log, model_gam, model_poly, model_poly_log)

comprehensive_metrics <- tibble(
  Model = c("Linear", "Log-Linear", "Log-GAM", "Polynomial", "Log-Polynomial"),
  fit_space = c("Real", "Log", "Log", "Real", "Log"),
  Rsq_fit_Space = round(c(linear_r2_real, log_r2_log, gam_r2_log, 
                          poly_r2_real, poly_log_r2_log), 4),
  Rsq_Real_Space = round(c(linear_r2_real, log_r2_real, gam_r2_real, 
                           poly_r2_real, poly_log_r2_real), 4),
  RMSE_Real_Space = round(c(
    sqrt(mean((USGS_WCM$PeakQ - linear_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - log_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - gam_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - poly_pred_real)^2)),
    sqrt(mean((USGS_WCM$PeakQ - poly_log_pred_real)^2))
  ), 0),
  RMSE_Log_Space = round(c(
    sqrt(mean((log(USGS_WCM$PeakQ) - log(linear_pred_real))^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - log_pred_log)^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - gam_pred_log)^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - log(poly_pred_real))^2)),
    sqrt(mean((log(USGS_WCM$PeakQ) - poly_log_pred_log)^2))
  ), 3),
  AIC = round(aics$AIC, 1),
  BIC = round(bics$BIC, 1)) %>%
  arrange(AIC)

# APPLY LOG-LINEAR REGRESSION TO JMD 1 DAY -------------------------------------
jmd_AMS_1day <- jmd_AMS_1day %>% rename(DailyQ = JMD_Daily)
jmd_AMS_1day$PeakQ_pred <- exp(predict(model_log, newdata = jmd_AMS_1day))

# RESUME HERE 

# Join with Observed Peaks
jmd_peak_AMS <- jmd_AMS_1day %>% left_join(select(USGS_WCM,c(WY,PeakQ,DailyQ,Source)),by = "WY")

# Select observations where available
jmd_peak_AMS <- jmd_peak_AMS %>% 
  mutate(Source = ifelse(is.na(Source),"Peak Regression",Source)) %>% 
  mutate(PeakQ = ifelse(is.na(PeakQ),PeakQ_pred,PeakQ)) %>% 
  mutate(DailyQ_final = ifelse(is.na(DailyQ.y),DailyQ.x,DailyQ.y),.before = Source) %>% 
  select(-c(DailyQ.x,DailyQ.y))

# Remove the DailyQ Artifacts
jmd_peak_AMS <- jmd_peak_AMS %>% 
  rename(DailyQ = DailyQ_final)

# Remove Unneeded Preds
jmd_peak_AMS <- jmd_peak_AMS %>% 
  mutate(Source_Primary = ifelse(Source == "Peak Regression","Peak Regression","Observed Peak Flow"))

# Summary Table
summary_by_type <- jmd_peak_AMS %>%
  group_by(Source) %>%
  summarise(
    Count = n(),
    Mean_Peak = round(mean(PeakQ), 0),
    Max_Peak = round(max(PeakQ), 0),
    Year_Range = paste(min(WY), "-", max(WY))
  )
