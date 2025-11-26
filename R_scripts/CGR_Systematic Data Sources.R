# Cougar Peak to Volume Transformation -----------------------------------------
# Vida Gage - Peak to 3-day
# Vida 3-day to Cougar 3-day

# RMC Flood Hazards - Dan McGraw
today = format(Sys.Date(),"%d-%b-%Y")

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, memoise)

theme_set(theme_bw())

# Plot dir ----
plotdir <- "/outputs/Figures/Systematic Inflow/"

# Load Data --------------------------------------------------------------------
cgr_winter_ams <- dir_ls("data/Cougar/",glob = "*Cougar_3day.csv*",recurse = T) %>% read_csv()

# AMS Timeseries ----------------
ams_col_points <- ggplot(cgr_winter_ams, aes(x = WY, y = Flow, color = Source)) +
  geom_point(alpha = 0.85, size = 2) +
  scale_color_manual(breaks = c("2022 Unregulated Study",
                                "CWMS"),
                     values = c("2022 Unregulated Study" = "#4E79A7",
                                "CWMS" = "#F28E2B")) +
  scale_y_continuous(breaks = seq(0,30000,5000),minor_breaks = seq(0,30000,1000),labels = scales::comma) + 
  scale_x_continuous(breaks = seq(1900,2030,10),minor_breaks = seq(1900,2030,2)) + 
  coord_cartesian(ylim = c(0,25000), xlim = c(1925,2025),expand = F) +
  labs(x = "Water Year", y = "3-Day Reservoir Inflow (cfs)") +
  theme(legend.title = element_blank(),legend.position = "bottom")

ggsave(paste0(getwd(),plotdir,"AMS_Timeseries.png"), ams_col_points, height = 5, width = 7, dpi=300)

# AMS ECDF ----------------------
weibull_pp <- function(x) {
  n <- length(x)
  i <- rank(x)
  return(i / (n+1))
}

cgr_winter_ams <- cgr_winter_ams %>% 
  mutate(pp = 1 - weibull_pp(Flow))

ams_ecdf <- ggplot(cgr_winter_ams, aes(x = pp, y = Flow, color = Source)) +
  geom_point(alpha = 0.85, size = 1.5) +
  scale_color_manual(breaks = c("2022 Unregulated Study",
                                "CWMS"),
                     values = c("2022 Unregulated Study" = "#4E79A7",
                                "CWMS" = "#F28E2B")) +
  scale_x_continuous(trans = scales::trans_new("reverse_log", transform = function(x) -log10(x),inverse = function(x) 10^(-x)),
                     breaks = scales::log_breaks(n = 10, base = 10),
                     minor_breaks = scales::minor_breaks_log(),
                     labels = scales::label_number(accuracy = 0.001),
                     limits = c(1,0.01)) +
  scale_y_log10(breaks = scales::log_breaks(n = 6),
                minor_breaks = scales::minor_breaks_log(),
                labels = scales::comma) +
  coord_cartesian(ylim = c(500,25000),expand = F)+
  labs(x = "Empirical AEP", y = "3-Day Reservoir Inflow (cfs)") +
  theme(legend.title = element_blank(),legend.position = "bottom")

ggsave(paste0(getwd(),plotdir,"AMS_ECDF.png"), ams_ecdf, height = 5, width = 7, dpi=300)

# Patchworkd
combined_AMS <- ams_col_points + ams_ecdf
ggsave(paste0(getwd(),plotdir,"AMS_TS_ECDF.png"), combined_AMS, height = 7, width = 10, dpi=300)
