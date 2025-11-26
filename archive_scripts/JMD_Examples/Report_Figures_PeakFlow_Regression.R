# ============================================================================
# Word Document Figures
# ============================================================================
library(tidyverse)
library(gt)
# 

# load("D:/0.RMC/JohnMartin/DQC/R/Post_RMD_figures.RData")
out_dir <- "D:/0.RMC/JohnMartin/DQC/Figures/Peak Regression/Report_Figures/"

# ============================================================================
# Available Record =====
# ============================================================================
# Colors
source_colors <- c(
  "John Martin Dam" = "#D55E00",
  "USGS - Arkansas River at Las Animas" = "#4477AA", 
  "USGS - Purgatoire River at Las Animas" = "#66CCEE")

data_with_gaps <- data_with_gaps %>% 
  mutate(Start_WYi = Start_WY -.15,
         End_WYi = End_WY + .15)

data_with_gaps$Start_WY[data_with_gaps$Interval == "WCM Peaks"] <- data_with_gaps$Start_WYi[data_with_gaps$Interval == "WCM Peaks"]
data_with_gaps$End_WY[data_with_gaps$Interval == "WCM Peaks"] <- data_with_gaps$End_WYi[data_with_gaps$Interval == "WCM Peaks"]


p_timeline_faceted_gaps <- ggplot(data_with_gaps) +
  theme_bw() +
  # Draw segments (showing gaps)
  geom_segment(aes(x = Start_WY, xend = End_WY, 
                   y = reorder(Label, axis_val), yend = reorder(Label, axis_val),
                   color = Source),
               linewidth = 4, lineend = "round", alpha = 0.8) +
  # Add total year counts on the right
  geom_text(data = total_years,
            aes(x = Last_End + 1.5, y = Interval, 
                label = paste0(Total_Years, " yrs"),
                color = Source),
            hjust = 0, size = 3, fontface = "bold") +
  # Facet by source
  facet_wrap(~Source, ncol = 1, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(1900, 2030, by = 10),
    minor_breaks = seq(1900, 2030, by = 1),
    limits = c(1910, 2030)
  ) +
  scale_color_manual(values = source_colors) +
  labs(
    x = "Water Year",
    y = ""
  ) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "gray92", linewidth = 0.3),
    strip.text = element_text(face = "bold", hjust = 0, size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    axis.text.y = element_text(size = 10)
  )

p_timeline_faceted_gaps

ggsave(paste0(out_dir,"2_data_timelines.png"),
       plot = p_timeline_faceted_gaps, height = 6, width = 10, dpi = 500)

# ============================================================================
# Coincident "Matching" peaks =====
# ============================================================================
USGS_peaks_coincident <- USGS_peaks_coincident %>% 
  rowwise() %>% 
  mutate(lower_peak = min(Ark_Peak,Purg_Peak))

# Create the coincident peaks plot
p_coincident <- USGS_peaks %>% 
  ggplot() +
  theme_bw() +
  # Optional: Connect matching peaks with lines to show they're paired
  geom_segment(data = USGS_peaks_coincident,
               aes(x = Ark_DT, xend = Purg_DT, 
                   y = lower_peak, yend = Combined_Peak),
               linetype = "dashed", color = "gray50") +
  # Arkansas peaks
  geom_point(aes(x = Ark_DT, y = Ark_Peak, color = "Arkansas River Peak"), 
             size = 2.5, alpha = 0.7) +
  # Purgatoire peaks
  geom_point(aes(x = Purg_DT, y = Purg_Peak, color = "Purgatoire River Peak"), 
             size = 2.5, alpha = 0.7) +
  # Matching peaks (combined)
  geom_point(data = USGS_peaks_coincident, 
             aes(x = Ark_DT, y = Ark_Peak + Purg_Peak, color = "Coincident Peak"),
             size = 3.5, alpha = 0.8, shape = 17) +
  scale_y_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(),labels = scales::comma) +
  scale_x_datetime(date_breaks = "10 years", date_minor_breaks = "2 years",date_labels = "%Y") +
  coord_cartesian(xlim = c(as.POSIXct("1940-01-01"), as.POSIXct("2025-01-01"))) +
  scale_color_manual(
    values = c("Arkansas River Peak" = "#4477AA",
               "Purgatoire River Peak" = "#66CCEE",
               "Coincident Peak" = "#228833"),
    breaks = c("Arkansas River Peak","Purgatoire River Peak","Coincident Peak")) +
  labs(
#    subtitle = "Peak flows within one day are combined to estimate total inflow",
    x = "Date",
    y = "Peak Flow (cfs, log scale)",
    color = "Data Source"
  ) +
  theme(legend.position = "bottom")

print(p_coincident)

ggsave(paste0(out_dir,"3_coincident_peaks.png"),
       plot = p_coincident, height = 6, width = 10, dpi = 500)


# ============================================================================
# Combined Instantaneous =====
# ============================================================================

event_DT <- USGS_combined_instant_AMS$DT[7]

# Filter to event window (5 days before and after)
event_window <- combined_instant %>% 
  filter(between(DT_MTN, event_DT - days(5), event_DT + days(5))) %>% 
  mutate(Days = as.numeric(difftime(DT_MTN, event_DT - days(5), units = "days")))

# Identify the peaks for combined flow
combined_peak <- event_window %>% 
  filter(USGS_combined_instant == max(USGS_combined_instant, na.rm = TRUE)) %>% 
  slice(1)

# Create the comparison plot
p_real_event <- ggplot(event_window) +
  theme_bw() +
  # Individual tributaries
  geom_line(aes(x = Days, y = Ark_instant, color = "Arkansas River"), 
            linewidth = 0.75) +
  geom_line(aes(x = Days, y = Purg_instant, color = "Purgatoire River"), 
            linewidth = 0.75) +
  # Combined flow
  geom_line(aes(x = Days, y = USGS_combined_instant, color = "Combined Inflow"), 
            linewidth = 1.25) +
  
  # Mark combined peak
  geom_point(data = combined_peak, aes(x = Days, y = USGS_combined_instant), 
             size = 4, color = "#CCBB44") +
  
  # Vertical line at combined peak
  geom_segment(data = combined_peak,
               aes(x = Days, xend = Days, y = 0, yend = USGS_combined_instant),
               linetype = "dashed", color = "#CCBB44", linewidth = 1, alpha = 0.7) +
  
  # Annotation for true combined peak
  annotate("text",
           x = combined_peak$Days-1.25,
           y = combined_peak$USGS_combined_instant - max(event_window$USGS_combined_instant, na.rm = TRUE) * 0.05,
           label = paste0("Combined Peak = ",scales::comma(round(combined_peak$USGS_combined_instant, 0)), " cfs", "\n",
                          format(combined_peak$DT_MTN, "%m-%d-%Y %H:%M"), "\n"),
           size = 3.5, color = "#CCBB44", fontface = "bold") +
  scale_color_manual(
    values = c("Arkansas River" = "#4477AA",
               "Purgatoire River" = "#66CCEE",
               "Combined Inflow" = "#CCBB44"),
    breaks = c("Arkansas River","Purgatoire River","Combined Inflow")) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Days",
    y = "Flow (cfs)",
    color = "Data Source"
  ) +
  theme(legend.position = "bottom")

p_real_event

ggsave(paste0(out_dir,"4_combined_peak_1995.png"),
       plot = p_real_event, height = 6, width = 10, dpi = 500)

rm(p_real_event)

# ============================================================================
# JMD 6-hour smoothing =====
# ============================================================================
event_DT <- JMD_Hourly_AMS$DT[2]
peak_event <- JMD_Hourly %>%
  filter(between(DT, event_DT - days(5), event_DT + days(5)))

p_peak <- peak_event %>%
  ggplot(aes(x = DT)) +
  theme_bw() +
  geom_hline(yintercept = 0,linetype = "dashed",linewidth = 0.5)+
  geom_line(aes(y = Inflow_Smooth_CFS, color = "Hourly Calculated"), 
            alpha = 0.5, linewidth = 0.5) +
  geom_line(aes(y = Inflow_Smooth_CFS_6hr, color = "6-Hour Smoothed"), 
            linewidth = 1) +
  # Mark the annual maximum from smoothed data
  geom_point(data = peak_event %>% 
               filter(Inflow_Smooth_CFS_6hr == max(Inflow_Smooth_CFS_6hr, na.rm = TRUE)),
             aes(x = DT, y = Inflow_Smooth_CFS_6hr),
             size = 4, color = "#D55E00", shape = 17) +
  geom_text(data = peak_event %>% 
              filter(Inflow_Smooth_CFS_6hr == max(Inflow_Smooth_CFS_6hr, na.rm = TRUE)),
            aes(x = DT, y = Inflow_Smooth_CFS_6hr,
                label = paste0("Peak: ", scales::comma(round(Inflow_Smooth_CFS_6hr, 0)), " cfs")),
            vjust = 0, hjust = -.25, size = 4, fontface = "bold", color = "#D55E00") +
  scale_color_manual(
    values = c(
      "Hourly Calculated" = "gray60",
      "6-Hour Smoothed" = "#D55E00"
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = paste0("Annual Maximum Identification: WY ", year(peak_event$DT[1])),
    x = "Date",
    y = "Inflow (cfs)",
    color = ""
  ) +
  theme(legend.position = "bottom")

# Difference CFS HIST
diff_data <- JMD_Hourly %>%
  filter(!is.na(Inflow_Smooth_CFS) & !is.na(Inflow_Smooth_CFS_6hr)) %>%
  mutate(
    Difference = Inflow_Smooth_CFS - Inflow_Smooth_CFS_6hr,
    Pct_Difference = (Difference / Inflow_Smooth_CFS) * 100
  )

p_diff <- ggplot(diff_data, aes(x = Difference)) +
  theme_bw() +
  geom_histogram(binwidth = 50, fill = "#D55E00", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(diff_data$Difference, na.rm = TRUE), 
             color = "blue", linewidth = 0.25,linetype = "dashed") +
  scale_x_continuous(breaks = seq(-5000,5000,250),
                     minor_breaks = seq(-5000,5000,50),
                     labels = scales::comma(seq(-5000,5000,250)),
                     limits = c(-1000,1000)) +
  scale_y_continuous(breaks = seq(0,200000,10000),
                     minor_breaks = seq(0,200000,2500),
                     labels = scales::comma,
                     limits = c(0,70000))+
  labs(
    title = "Distribution of Differences: Hourly - Smoothed",
    subtitle = "Binwidth = 50 cfs",
    x = "Difference (cfs)",
    y = "Count"
  )

jmd_peak_diff <- p_peak/p_diff
ggsave(paste0(out_dir,"5_JMD_smoothing.png"),
       plot = jmd_peak_diff, height = 8, width = 10, dpi = 500)

rm(jmd_peak_diff)
# ============================================================================
# JMD USGS Correlation =====
# ============================================================================

p_scatter <- ggplot(overlap_complete, 
                    aes(x = USGS_combined_instant, y = Inflow_Smooth_CFS_6hr)) +
  theme_bw() +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "red", linewidth = 1, alpha = 0.7) +
  geom_point(alpha = 0.3, size = 1.5, color = "#4477AA") +
  geom_smooth(method = "lm", se = F, color = "#D55E00", linewidth = 1.2) +
  # Annotations
  annotate("text", x = 1, y = 28000,
           label = paste0("R\u00B2 = ", round(cor_pearson^2, 3), "\n",
                          "RMSE = ", round(rmse, 0), " cfs\n",
                          "n = ", scales::comma(nrow(overlap_complete))),
           hjust = 0, vjust = 1, size = 4, fontface = "bold") +
  annotate("text", x = Inf, y = -Inf,
           label = "1:1 line (red dashed)",
           hjust = 1.1, vjust = -0.5, size = 3, color = "red", fontface = "italic") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  coord_fixed(ratio = 1) +
  labs(
    title = "JMD & USGS Inflow Correlation",
    #subtitle = "Demonstrating USGS upstream gages as reliable proxy for JMD inflow (1998-2018)",
    x = "USGS Combined Instantaneous Flow (cfs)",
    y = "JMD Smoothed 6-Hour Inflow (cfs)"
  )

# Distribution Comparison
overlap_long <- overlap_complete %>%
  select(DT_MTN, USGS_combined_instant, Inflow_Smooth_CFS_6hr) %>%
  pivot_longer(cols = c(USGS_combined_instant, Inflow_Smooth_CFS_6hr),
               names_to = "Source",
               values_to = "Inflow") %>%
  mutate(Source = recode(Source,
                         "USGS_combined_instant" = "USGS Combined",
                         "Inflow_Smooth_CFS_6hr" = "JMD 6-Hour"))

# Time Series
sample_size <- min(5000, nrow(overlap_complete))
overlap_sample <- overlap_complete %>%
  sample_n(sample_size) %>%
  arrange(DT_MTN)

p_timeseries <- ggplot(overlap_sample) +
  theme_bw() +
  geom_line(aes(x = DT_MTN, y = USGS_combined_instant, color = "USGS Combined"),
            alpha = 0.6, linewidth = 0.5) +
  geom_line(aes(x = DT_MTN, y = Inflow_Smooth_CFS_6hr, color = "JMD 6-Hour"),
            alpha = 0.6, linewidth = 0.5) +
  scale_color_manual(
    values = c(
      "USGS Combined" = "#4477AA",
      "JMD 6-Hour" = "#D55E00"
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Time Series Comparison: JMD vs USGS Inflow",
    subtitle = paste0("R\u00B2 = ", round(cor_pearson^2, 3)),
    x = "Date",
    y = "Inflow (cfs)",
    color = "Source"
  ) +
  theme(legend.position = "none")

p_distributions <- ggplot(overlap_long, aes(x = Inflow, fill = Source)) +
  theme_bw() +
  geom_density(alpha = 0.5, linewidth = 0.75) +
  scale_fill_manual(
    values = c(
      "USGS Combined Instantaneous" = "#4477AA",
      "JMD 6-Hour" = "#D55E00"
    )
  ) +
  scale_x_log10(breaks = breaks_log(base = 10,n = 7), minor_breaks = minor_breaks_log(detail = 1),labels = scales::comma) +
  scale_y_continuous(breaks = seq(0,1.4,0.2)) +
  coord_cartesian(xlim = c(.1,500000),ylim = c(0,1.5))+
  labs(
    title = "Flow Distribution Comparison",
    x = "Inflow (cfs, log scale)",
    y = "Relative Frequency Density",
    fill = "Source"
  ) +
  theme(legend.position = "bottom")

combined_correlation <- p_scatter+(p_timeseries/p_distributions)

combined_correlation

ggsave(paste0(out_dir,"6_JMD_USGS_Correlation.png"),
       plot = combined_correlation, height = 6, width = 10, dpi = 500)

ggsave(paste0(out_dir,"6a_JMD_USGS_Correlation.png"),
       plot = p_scatter, height = 6, width = 10, dpi = 500)

ggsave(paste0(out_dir,"6b_JMD_USGS_Correlation.png"),
       plot = p_distributions, height = 6, width = 10, dpi = 500)

ggsave(paste0(out_dir,"6c_JMD_USGS_Correlation.png"),
       plot = p_timeseries + theme(legend.position = "bottom"), height = 6, width = 10, dpi = 500)

combined_timeseries <- (p_timeseries/p_distributions)
ggsave(paste0(out_dir,"6d_JMD_USGS_Correlation.png"),
       plot = combined_timeseries, height = 6, width = 10, dpi = 500)

# ============================================================================
# Travel Time Assumption =====
# ============================================================================

p_lag <- ggplot(lag_correlations, aes(x = Lag_Hours, y = R_squared)) +
  theme_bw() +
  geom_line(linewidth = 1.2, color = "#4477AA") +
  geom_point(size = 2, color = "#4477AA", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", linewidth = 1) +
  geom_point(data = lag_correlations %>% filter(Lag_Minutes == 0),
             aes(x = Lag_Hours, y = R_squared),
             size = 3.5, color = "red", shape = 17) +
  geom_point(data = lag_correlations %>% filter(Lag_Hours == optimal_lag$Lag_Hours),
             aes(x = Lag_Hours, y = R_squared),
             size = 3.5, color = "grey30", shape = 17) +
  geom_hline(yintercept = lag_correlations %>% filter(Lag_Minutes == 0) %>% pull(R_squared), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  # annotate("rect", xmin = -0.1, xmax = 0.1, ymin = -Inf, ymax = Inf,
  #          fill = "red", alpha = 0.05) +
  annotate("text", x = 0.4, y = 0.84,
           label = paste0("Zero Lag\nR\u00B2 = 0.86"),
           hjust = 0, vjust = 1, color = "red", size = 3.5, fontface = "bold") +
  annotate("text", x = optimal_lag$Lag_Hours-1, y = optimal_lag$R_squared,
           label = paste0("Optimal Lag: ", optimal_lag$Lag_Hours, " hr\n R\u00B2 = 0.92\nΔR\u00B2 = +0.06"),
           hjust = 0, vjust = 1.25, color = "gray30", size = 3, fontface = "italic") +
  scale_x_continuous(breaks = seq(-3, 20, by = 1)) +
  scale_y_continuous(limits = c(0.81, 0.95), breaks = seq(0.80, 0.98, by = 0.01)) +
  labs(
#    title = "Lag Sensitivity Analysis",
#    subtitle = "Lag time applied to John Martin Dam Inflow",
    x = "Time Lag (hours)",
    y = "Coefficient of Determination (R\u00B2)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 12))

p_lag
ggsave(paste0(out_dir,"7_Lag_Time.png"),
       plot = p_lag, height = 5, width = 7, dpi = 500)

# ============================================================================
# ECDF by Source =====
# ============================================================================
ams_source_colors <- c("WCM Peaks" = "#D55E00", "USGS Combined Instantaneous" = "#CCBB44", "USGS Coincident Peaks" = "#228833")

# Empirical CDF
weibull_peak <- USGS_WCM %>%
  arrange(PeakQ) %>%
  mutate(
    rank = row_number(),
    weibull_prob = rank / (n() + 1),
    exceedance_prob = 1 - weibull_prob
  )

p_peak_weibull <- ggplot(weibull_peak, aes(x = exceedance_prob, y = PeakQ, color = Source)) +
  theme_bw() +
  geom_point(size = 1.5, alpha = 0.6) +
  scale_x_continuous(trans = trans_new("reverse_log", transform = function(x) -log10(x),inverse = function(x) 10^(-x)), 
                     breaks = scales::log_breaks(n = 10, base =10),
                     minor_breaks = scales::minor_breaks_log(),
                     labels = label_number(accuracy = 0.001),
                     limits = c(1,0.01)) + 
  scale_y_log10(breaks = scales::log_breaks(),
                minor_breaks = scales::minor_breaks_log(),
                labels = scales::comma) +
  scale_color_manual(values = ams_source_colors) +
  labs(title = "Peak Flow Empirical CDF",
       x = "Annual Exceedance Probability (AEP)",
       y = "Peak Flow (cfs)",
       color = "Source"
  ) +
  theme(legend.position = "none")

p_peak_weibull

ggsave(paste0(out_dir,"8_ECDF_Peak_prior.png"),
       plot = p_peak_weibull, height = 6, width = 8, dpi = 500)

# ============================================================================
# Scatter and Boxplot =====
# ============================================================================
peak_verification <- USGS_WCM

peak_daily_scatter <- ggplot(peak_verification, aes(x = DailyQ, y = PeakQ, color = Source, shape = Source)) +
  theme_bw() +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_log10(breaks = breaks_log(base = 10), minor_breaks = minor_breaks_log(detail = 1),labels = scales::comma) +
  scale_y_log10(breaks = breaks_log(base = 10), minor_breaks = minor_breaks_log(detail = 1),labels = scales::comma) +
  coord_cartesian(xlim = c(500,100000),ylim = c(1000,200000))+
  scale_color_manual(values = c(
    "WCM Peaks" = "#D55E00",
    "USGS Coincident Peaks" = "#228833",
    "USGS Combined Instantaneous" = "#CCBB44"
  )) +
  scale_shape_manual(values = c(
    "WCM Peaks" = 17,  # triangle
    "USGS Coincident Peaks" = 16,  # circle
    "USGS Combined Instantaneous" = 15  # square
  )) +
  labs(
    title = "Peak Flow vs 1-Day Flow by Data Source",
    x = "Daily Flow (cfs)",
    y = "Peak Flow (cfs)",
    color = "Data Source",
    shape = "Data Source"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  theme(legend.position = "bottom")

peak_daily_box <- ggplot(peak_verification %>% mutate(Peak_Daily_Ratio = PeakQ / DailyQ),
                         aes(x = Source, y = Peak_Daily_Ratio, fill = Source)) +
  theme_bw() +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c(
    "WCM Peaks" = "#D55E00",
    "USGS Coincident Peaks" = "#228833",
    "USGS Combined Instantaneous" = "#CCBB44"
  )) +
  labs(
    title = "Peak to 1-Day Ratio by Data Source",
    x = "Data Source",
    y = "Peak to 1-Day Ratio"
  ) +
  theme(legend.position = "none")+
  guides(x = guide_axis(n.dodge = 2))+
  scale_y_continuous(breaks = seq(0,10,1))

peak_1day_combined <- peak_daily_scatter + peak_daily_box

peak_1day_combined

ggsave(paste0(out_dir,"9_peak_scatter_boxplot.png"),
       plot = peak_1day_combined, height = 6, width = 10, dpi = 500)

# ============================================================================
# All Regression Models =====
# ============================================================================
plot_model_data <- USGS_WCM %>%
  mutate(
    pred_linear = linear_pred_real,
    pred_log = log_pred_real,
    pred_gam = gam_pred_real,
    pred_poly = poly_pred_real,
    pred_poly_log = poly_log_pred_real
  )

# Wes colors
model_colors <- c("Linear" = "#E2D200","Log-Linear" = "#46ACC8","Log-GAM" = "#B40F20","Log-Polynomial" = "#DD8D29", "Polynomial" = "#446455")

models_plot <- ggplot(plot_model_data, aes(x = DailyQ, y = PeakQ)) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(y = pred_linear, color = "Linear"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = pred_log, color = "Log-Linear"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = pred_gam, color = "Log-GAM"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = pred_poly_log, color = "Log-Polynomial"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = pred_poly, color = "Polynomial"), linewidth = 0.8, alpha = 0.7) +
  scale_x_log10(labels = scales::comma, breaks = scales::log_breaks(n = 5, base = 10),minor_breaks = scales::minor_breaks_log()) +
  scale_y_log10(labels = scales::comma, breaks = scales::log_breaks(n = 5, base = 10),minor_breaks = scales::minor_breaks_log()) +
  scale_color_manual(values = model_colors) +
  # scale_color_manual(values = c(
  #   "Linear" = "#E15759",
  #   "Log-Linear" = "#59A14F",
  #   "Log-GAM" = "#4E79A7",
  #   "Log-Poly" = "#B07AA1"
  # )) +
  labs(
    title = "Peak Flow Regression Models",
    x = "Max Daily Flow (cfs)",
    y = "Peak Flow (cfs)",
    color = "Model"
  ) +
  theme(legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = "white", color = "black"))

models_plot

ggsave(paste0(out_dir,"10a_all_models.png"),
       plot = models_plot, height = 6, width = 10, dpi = 500)

# Reshape data to long format
plot_model_long <- plot_model_data %>%
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "model",
    values_to = "prediction"
  ) %>%
  mutate(
    model = case_when(
      model == "pred_linear" ~ "Linear",
      model == "pred_log" ~ "Log-Linear",
      model == "pred_gam" ~ "Log-GAM",
      model == "pred_poly_log" ~ "Log-Polynomial",
      model == "pred_poly" ~ "Polynomial"
    )
  )

# Wes colors
model_colors <- c("Linear" = "#E2D200","Log-Linear" = "#46ACC8","Log-GAM" = "#B40F20","Log-Polynomial" = "#DD8D29", "Polynomial" = "#446455")

# Faceted plot
models_plot_faceted <- ggplot(plot_model_long, aes(x = DailyQ, y = PeakQ)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(aes(y = prediction, color = model), linewidth = 0.8, alpha = 0.7) +
  scale_color_manual(values = model_colors) +
  scale_x_log10(labels = comma,
                breaks = scales::log_breaks(n = 5, base = 10),
                minor_breaks = scales::minor_breaks_log()) +
  scale_y_log10(labels = comma,
                breaks = scales::log_breaks(n = 5, base = 10),
                minor_breaks = scales::minor_breaks_log()) +
  facet_wrap(~ model, scales = "fixed") +
  labs(
    #title = "Peak Flow Regression Models",
    x = "Daily Flow (cfs)",
    y = "Peak Flow (cfs)",
    color = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text = element_text(face = "bold")
  )

models_plot_faceted

ggsave(paste0(out_dir,"10b_all_models_facet.png"),
       plot = models_plot_faceted, height = 6, width = 10, dpi = 500)

# ============================================================================
# Selected Log Regression Models =====
# ============================================================================
# Common theme elements
common_theme <- theme_bw() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10))

# Define log breaks for consistent axes
log_brks_y <- c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
log_minor_brks_y <- c(seq(1000, 10000, 1000), seq(20000, 100000, 10000), 
                      seq(200000, 300000, 100000))

# ============
# Create prediction sequences

# Create a sequence of Daily flows for smooth prediction curves
daily_seq <- tibble(
  DailyQ = seq(min(USGS_WCM$DailyQ), max(USGS_WCM$DailyQ), length.out = 200)
)

# =============
# LOG-LINEAR MODEL
# Get predictions in log space with intervals
log_linear_pred <- predict(model_log, newdata = daily_seq, 
                           interval = "prediction", level = 0.95)
log_linear_conf <- predict(model_log, newdata = daily_seq, 
                           interval = "confidence", level = 0.95)

# Back-transform to real space
log_linear_intervals <- daily_seq %>%
  mutate(
    fit = exp(log_linear_pred[, "fit"]),
    pred_lwr = exp(log_linear_pred[, "lwr"]),
    pred_upr = exp(log_linear_pred[, "upr"]),
    conf_lwr = exp(log_linear_conf[, "lwr"]),
    conf_upr = exp(log_linear_conf[, "upr"])
  )

# =============
# LOG-GAM MODEL
# GAM predictions with standard errors
log_gam_pred <- predict(model_gam, newdata = daily_seq, se.fit = TRUE)

# Calculate prediction and confidence intervals manually for GAM
gam_sigma <- sqrt(sum(residuals(model_gam)^2) / model_gam$df.residual)

# Confidence interval (for the mean)
log_gam_conf_lwr <- log_gam_pred$fit - qnorm(0.975) * log_gam_pred$se.fit
log_gam_conf_upr <- log_gam_pred$fit + qnorm(0.975) * log_gam_pred$se.fit

# Prediction interval (for new observations)
log_gam_pred_lwr <- log_gam_pred$fit - qnorm(0.975) * sqrt(log_gam_pred$se.fit^2 + gam_sigma^2)
log_gam_pred_upr <- log_gam_pred$fit + qnorm(0.975) * sqrt(log_gam_pred$se.fit^2 + gam_sigma^2)

# Back-transform to real space
log_gam_intervals <- daily_seq %>%
  mutate(
    fit = exp(log_gam_pred$fit),
    pred_lwr = exp(log_gam_pred_lwr),
    pred_upr = exp(log_gam_pred_upr),
    conf_lwr = exp(log_gam_conf_lwr),
    conf_upr = exp(log_gam_conf_upr)
  )

# ================
# POLYNOMIAL MODEL
# Get predictions in log space with intervals
log_poly_pred <- predict(model_poly_log, newdata = daily_seq, 
                         interval = "prediction", level = 0.95)
log_poly_conf <- predict(model_poly_log, newdata = daily_seq, 
                         interval = "confidence", level = 0.95)

# Back-transform to real space
log_poly_intervals <- daily_seq %>%
  mutate(
    fit = exp(log_poly_pred[, "fit"]),
    pred_lwr = exp(log_poly_pred[, "lwr"]),
    pred_upr = exp(log_poly_pred[, "upr"]),
    conf_lwr = exp(log_poly_conf[, "lwr"]),
    conf_upr = exp(log_poly_conf[, "upr"])
  )

# ==============
# CREATE PLOTS

model_colors <- c("Linear" = "#E2D200","Log-Linear" = "#46ACC8","Log-GAM" = "#B40F20","Log-Poly" = "#DD8D29")

# Plot 1: Log-Linear
p1 <- ggplot() +
  common_theme +
  # Prediction interval (wider)
  geom_ribbon(data = log_linear_intervals, 
              aes(x = DailyQ, ymin = pred_lwr, ymax = pred_upr),
              fill = "#46ACC8", alpha = 0.15) +
  # Confidence interval (narrower)
  geom_ribbon(data = log_linear_intervals,
              aes(x = DailyQ, ymin = conf_lwr, ymax = conf_upr),
              fill = "#46ACC8", alpha = 0.3) +
  # Fitted line
  geom_line(data = log_linear_intervals, aes(x = DailyQ, y = fit),
            color = "#46ACC8", linewidth = 1) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  # Data points
  geom_point(data = USGS_WCM, aes(x = DailyQ, y = PeakQ),
             size = 1.5, alpha = 0.7, shape = 16) +
  scale_x_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(), 
                labels = scales::comma) +
  scale_y_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(), 
                labels = scales::comma) +
  coord_cartesian(xlim = c(700, 100000), ylim = c(700, 400000)) +
  labs(
    title = "Log-Linear Model",
    #subtitle = "95% confidence and prediction intervals",
    x = "Daily Inflow (cfs)",
    y = "Peak Inflow (cfs)")

# Plot 2: Log-GAM
p2 <- ggplot() +
  common_theme +
  # Prediction interval
  geom_ribbon(data = log_gam_intervals,
              aes(x = DailyQ, ymin = pred_lwr, ymax = pred_upr),
              fill = "#B40F20", alpha = 0.15) +
  # Confidence interval
  geom_ribbon(data = log_gam_intervals,
              aes(x = DailyQ, ymin = conf_lwr, ymax = conf_upr),
              fill = "#B40F20", alpha = 0.3) +
  # Fitted line
  geom_line(data = log_gam_intervals, aes(x = DailyQ, y = fit),
            color = "#B40F20", linewidth = 1) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  # Data points
  geom_point(data = USGS_WCM, aes(x = DailyQ, y = PeakQ),
             size = 1.5, alpha = 0.7, shape = 16) +
  scale_x_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(), 
                labels = scales::comma) +
  scale_y_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(), 
                labels = scales::comma) +
  coord_cartesian(xlim = c(700, 100000), ylim = c(700, 400000)) +
  labs(
    title = "Log-GAM Model",
    #subtitle = "95% confidence and prediction intervals",
    x = "Daily Inflow (cfs)",
    y = ""
  )+
  theme(axis.text.y = element_blank())

# Plot 3: Polynomial
p3 <- ggplot() +
  common_theme +
  # Prediction interval
  geom_ribbon(data = log_poly_intervals,
              aes(x = DailyQ, ymin = pred_lwr, ymax = pred_upr),
              fill = "#DD8D29", alpha = 0.15) +
  # Confidence interval
  geom_ribbon(data = log_poly_intervals,
              aes(x = DailyQ, ymin = conf_lwr, ymax = conf_upr),
              fill = "#DD8D29", alpha = 0.3) +
  # Fitted line
  geom_line(data = log_poly_intervals, aes(x = DailyQ, y = fit),
            color = "#DD8D29", linewidth = 1) +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  # Data points
  geom_point(data = USGS_WCM, aes(x = DailyQ, y = PeakQ),
             size = 1.5, alpha = 0.7, shape = 16) +
  scale_x_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(), 
                labels = scales::comma) +
  scale_y_log10(breaks = scales::log_breaks(), minor_breaks = scales::minor_breaks_log(), 
                labels = scales::comma) +
  coord_cartesian(xlim = c(700, 100000), ylim = c(700, 400000)) +
  labs(
    title = "Polynomial Model",
    #subtitle = "95% confidence and prediction intervals",
    x = "Daily Inflow (cfs)",
    y = ""
  )+
  theme(axis.text.y = element_blank())

# COMBINE WITH PATCHWORK
# Create combined plot
combined_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Prediction and Confidence Intervals for Selected Models",
    subtitle = "Darker shading = 95% confidence interval; Lighter shading = 95% prediction interval",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10)
    )
  )

print(combined_plot)

ggsave(paste0(out_dir,"10b_log_models.png"),
       plot = combined_plot, height = 6, width = 10, dpi = 500)

ggsave(paste0(out_dir,"10c_loglinear_model.png"),
       plot = p1, height = 4, width = 6, dpi = 500)

# ============================================================================
# Peak Flow AMS =====
# ============================================================================
peak_source_colors <- c("WCM Peaks" = "#D55E00",
                        "USGS Coincident Peaks" = "#228833", 
                        "USGS Combined Instantaneous" = "#CCBB44",
                        "Peak Regression" = "#4477AA")

# Timeline plot showing data types
p_timeline <- ggplot(jmd_peak_AMS, aes(x = WY, y = PeakQ)) +
#  geom_line(aes(group = Source_Primary),alpha = 0.3)+
  theme_bw() +
  geom_point(aes(color = Source),size = 2, alpha = 0.7) +
  #scale_y_log10(breaks = scales::log_breaks(),minor_breaks = scales::minor_breaks_log(),labels = scales::comma) +
  scale_y_continuous(breaks = seq(0,200000,50000),minor_breaks = seq(0,200000,10000),labels = scales::comma)+
  scale_x_continuous(breaks = seq(1900,2040,10),minor_breaks = seq(1900,2040,5))+
  scale_color_manual(values = peak_source_colors) +
  labs(
#    title = "Complete Peak Flow Annual Maximum Series (1913-2024)",
#    subtitle = "Regression used to fill historical gaps",
    x = "Water Year",
    y = "Peak Flow (cfs)",
    color = "Source"
  ) +
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(1910,2025), ylim = c(0,200000))

p_timeline

ggsave(paste0(out_dir,"11_Complete_AMS.png"),
       plot = p_timeline, height = 6, width = 10, dpi = 500)

# ============================================================================
# Final ECDF =====
# ============================================================================
weibull_pp <- function(x) {
  n <- length(x)
  i <- rank(x)
  return(i / (n+1))
}

jmd_peak_AMS$pp <- weibull_pp(jmd_peak_AMS$PeakQ)

# AEP to Z-score funciton
# makes plotting easy or whatever
AEPtoZ <- function(AEP) { 
  Z = qnorm(AEP, mean = 0, sd = 1, lower.tail = TRUE)
  return(Z) 
}

jmd_peak_AMS <- jmd_peak_AMS %>%
  mutate(AEP = 1-pp) %>%
  mutate(Z = AEPtoZ(pp))

peak_source_colors <- c("WCM Peaks" = "#D55E00",
                        "USGS Coincident Peaks" = "#228833", 
                        "USGS Combined Instantaneous" = "#CCBB44",
                        "Peak Regression" = "#4477AA")

# SEE BELOW
# p_peak_weibull_final

# ggsave(paste0(out_dir,"11a_ECDF.png"),
#        plot = p_peak_weibull_final, height = 6, width = 10, dpi = 500)

# ============================================================================
# Peak to daily ratio comparison =====
# ============================================================================
peak_source_colors <- c("WCM Peak" = "#D55E00",
                        "USGS Coincident Peaks" = "#228833", 
                        "USGS Combined Instantaneous" = "#CCBB44",
                        "Peak Regression" = "#4477AA")

p_comparison <- ggplot(jmd_peak_AMS %>% mutate(Peak_Daily_Ratio = PeakQ / DailyQ),
                       aes(x = Source, y = Peak_Daily_Ratio, fill = Source)) +
  theme_bw() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = peak_source_colors) +
  scale_y_continuous(breaks = seq(0,10,1),minor_breaks = seq(0,10,.25))+
  coord_cartesian(ylim = c(1,9))+
  labs(
    title = "Peak to 1-Day Ratio by Data Source",
    x = "Data Source",
    y = "Peak to 1-Day Flow Ratio") +
  theme(legend.position = "none")

p_comparison

ggsave(paste0(out_dir,"12_Boxplot_comparison.png"),
       plot = p_comparison, height = 6, width = 10, dpi = 500)

# ============================================================================
# Fitted AMS to Source AMS - ECDF KS comparison =====
# ============================================================================
weibull_peak_prior

# Empirical CDF
# Weibull Plotting Position
weibull_pp <- function(x) {
  n <- length(x)
  i <- rank(x)
  return(i / (n+1))
}

jmd_peak_AMS$pp <- weibull_pp(jmd_peak_AMS$PeakQ)

# AEP to Z-score funciton
# makes plotting easy or whatever
AEPtoZ <- function(AEP) { 
  Z = qnorm(AEP, mean = 0, sd = 1, lower.tail = TRUE)
  return(Z) 
}

jmd_peak_AMS <- jmd_peak_AMS %>%
  mutate(AEP = 1-pp) %>%
  mutate(Z = AEPtoZ(pp))

peak_source_colors <- c("WCM Peaks" = "#D55E00",
                        "USGS Coincident Peaks" = "#228833", 
                        "USGS Combined Instantaneous" = "#CCBB44",
                        "Peak Regression" = "#4477AA")

p_peak_weibull_final <- ggplot(jmd_peak_AMS, aes(x = AEP, y = PeakQ, color = Source)) +
  theme_bw() +
  geom_point(size = 2, alpha = 0.6) +
  scale_x_continuous(trans = scales::trans_new("reverse_log", transform = function(x) -log10(x),inverse = function(x) 10^(-x)),
                     breaks = scales::log_breaks(n = 10, base =10),
                     minor_breaks = scales::minor_breaks_log(),
                     labels = scales::label_number(accuracy = 0.001),
                     limits = c(1,0.01)) +
  scale_y_log10(breaks = scales::log_breaks(),
                minor_breaks = scales::minor_breaks_log(),
                labels = scales::comma) +
  scale_color_manual(values = peak_source_colors) +
  labs(
    #    title = "Peak Flow Empirical CDF",
    x = "Annual Exceedance Probability (AEP)",
    y = "Peak Flow (cfs)",
    color = "Source"
  ) +
  theme(legend.position = "inside",legend.position.inside = c(.8,.15))

p_peak_weibull_final

ggsave(paste0(out_dir,"13_Final_AMS_ECDF.png"),
       plot = p_peak_weibull_final, height = 6, width = 10, dpi = 500)

# Side by Side
p_timeline_no_legend <- p_timeline + theme(legend.position = "none")
ams_side_by_side <- p_timeline_no_legend + 
  (p_peak_weibull_final + theme(legend.position.inside = c(0.6,.15)) + 
     theme(legend.background = element_rect(fill = "transparent")))

ggsave(paste0(out_dir,"13a_Final_AMS_ECDF_comb.png"),
       plot = ams_side_by_side, height = 6, width = 10, dpi = 500)

# KS TEST ------
ks_test <- ks.test(jmd_peak_AMS$PeakQ,weibull_peak_prior$PeakQ)

comparison_colors <- c(
  "Regress.Augmented Record (1913-2024, n=112)" = "#009E73",  # Professional blue
  "Observed Peaks Only (1921-2024, n=46)" = "#E69F00"  # Professional orange
)

ecdf_comparison <- ggplot() +
  theme_bw() +
  # Extended record with regression
  geom_line(data = jmd_peak_AMS, 
            aes(x = AEP, y = PeakQ, 
                color = "Regress.Augmented Record (1913-2024, n=112)"), 
            linewidth = 1.2, alpha = 0.8) +
  # Observed peaks only
  geom_line(data = weibull_peak_prior, 
            aes(x = exceedance_prob, y = PeakQ, 
                color = "Observed Peaks Only (1921-2024, n=46)"), 
            linewidth = 1.2, alpha = 0.8) +
  scale_x_continuous(
    trans = trans_new("reverse_log", 
                      transform = function(x) -log10(x),
                      inverse = function(x) 10^(-x)), 
    breaks = c(1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01),
    minor_breaks = scales::minor_breaks_log(),
    labels = scales::label_number(accuracy = 0.001),
    limits = c(1, 0.01)
  ) + 
  scale_y_log10(
    breaks = scales::log_breaks(),
    minor_breaks = scales::minor_breaks_log(),
    labels = scales::comma
  ) +
  scale_color_manual(values = comparison_colors) +
  labs(
    x = "Annual Exceedance Probability",
    y = "Peak Flow (cfs)",
    color = "Dataset"
  ) +
  theme(
    legend.position = c(0.72, 0.15),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(face = "bold"))

ecdf_comparison

ggsave(paste0(out_dir,"14_KS_Test_ECDF.png"),
       plot = ecdf_comparison, height = 6, width = 8, dpi = 500)

# ============================================================================
# Model Fit (Predicted vs Observed) =====
# ============================================================================
# grab the eval df from the RMD
pred_v_obs_df <- jmd_peak_AMS %>% filter(Source_Primary != "Peak Regression")

pred_v_obs <- ggplot(pred_v_obs_df, aes(x = PeakQ, y = PeakQ_pred)) +
  geom_point(color = "#4477AA", alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_log10(breaks = scales::log_breaks(n = 5, base =10),
                minor_breaks = scales::minor_breaks_log(),
                labels = scales::comma) +
  scale_y_log10(breaks = scales::log_breaks(n = 5, base =10),
                minor_breaks = scales::minor_breaks_log(),labels = scales::comma) +
  coord_cartesian(ylim = c(100,150000))+
  labs(
    title = "Observed vs. Predicted Peak Flows",
    x = "Observed Peak Flow (cfs)",
    y = "Predicted Peak Flow (cfs)")

pred_v_obs

# ggsave(paste0(out_dir,"15_pred_v_obs.png"),
#        plot = pred_v_obs, height = 6, width = 8, dpi = 500)

# ============================================================================
# Model Residuals =====
# ============================================================================
# Residuals
pred_v_obs_df <- pred_v_obs_df %>%
  mutate(residual = PeakQ - PeakQ_pred)

pred_residuals <- ggplot(pred_v_obs_df, aes(x = PeakQ, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(color = "#4477AA", alpha = 0.7, size = 2) +
  scale_x_log10(breaks = scales::log_breaks(n = 7, base =10),
                minor_breaks = scales::minor_breaks_log(),
                labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Residuals of Predicted Peak Flows",
    x = "Observed Peak Flow (cfs)",
    y = "Residual (Observed - Predicted, cfs)"
  ) + 
  coord_cartesian(ylim = c(-50000,30000))

pred_residuals

# ggsave(paste0(out_dir,"16_pred_residuals.png"),
#        plot = pred_residuals, height = 6, width = 8, dpi = 500)

combined_pred_v_obs <- pred_v_obs + pred_residuals

ggsave(paste0(out_dir,"17_pred_residual_combined.png"),
       plot = combined_pred_v_obs, height = 6, width = 10, dpi = 500)

# ============================================================================
# NEXT? =====
# ============================================================================

