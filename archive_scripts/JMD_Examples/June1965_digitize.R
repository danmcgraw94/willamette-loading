# June 1965 Hydrograph Digitize & Cleaning

# ---- Package Management ----
pacman::p_load(
  # Core
  tidyverse,
  zoo,
  patchwork,                      # Combine plots
  ggsci,                          # Scientific color palettes (optional)
  ggtext,                         # Rich text in plots
  plotly,                         # Interactive plots
  # Performance
  future, furrr,                  # Parallel processing
  memoise                         # Caching
)

theme_set(theme_bw())

data_dir <- "D:/0.RMC/JohnMartin/DQC/data/RFA_Hydrographs/June1965_digitized/WPDigitizer/"
out_dir <- "D:/0.RMC/JohnMartin/DQC/data/RFA_Hydrographs/June1965_digitized/"
# Read Raw digitized data ------------------------------------------------------
raw_data <- read_csv(paste0(data_dir,"Default Dataset.csv"))

# Adjust the y-axis break ------------------------------------------------------
raw_data <- raw_data %>% 
  mutate(Flow_Kcfs_corrected = case_when(
    Notes == "100k = 20k: subtract 80k" ~ Flow_Kcfs - 80,
    TRUE ~ Flow_Kcfs
  ))

slice_max(raw_data,Flow_Kcfs_corrected,n=5)

# Adjust x-axis break ----------------------------------------------------------
# Remove duplicate timesteps
raw_data <- raw_data %>% distinct(Hour, .keep_all=TRUE)

# Used negative numbers & notes to flag the break
end_window1_idx <- (which(raw_data$Notes %in% "x and y break") %>% first()) - 1
start_window2_idx <- (which(raw_data$Notes %in% "x and y break") %>% last()) + 1

# The start of the break should be the end of the first plot window
end_window1_hr <- raw_data$Hour[end_window1_idx]
end_window2_hr <- raw_data$Hour[start_window2_idx]
delta_hrs <- end_window2_hr - end_window1_hr - (96 - end_window1_hr) # sets the start of break to ~96 hours

raw_data <- raw_data %>% 
  mutate(Hour_corrected = ifelse(Hour > end_window1_hr,
                                 Hour - delta_hrs,
                                 Hour))

# Convert to Correct units (kcfs to cfs) ---------------------------------------
raw_data <- raw_data %>% 
  mutate(Flow_cfs = Flow_Kcfs_corrected*1000)

# Extract as June 1965 --------------------------------------------------------
june1965 <- raw_data %>%
  filter(Notes != "x and y break" | is.na(Notes)) %>% 
  select(c(Hour_corrected, Flow_cfs)) %>% 
  rename(Hour = Hour_corrected)

june1965$Hour[1] <- 0
june1965$Flow_cfs[1] <- 0
june1965$Flow_cfs[which.max(june1965$Flow_cfs)] <- 163000

# Plot Digitized Hydro ---------------------------------------------------------
plot_dir <- "D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"

#colors <- pal_observable("observable10", alpha = 0.75)(8)
#palette.colors("Classic Tableau",n=8)
# "#1F77B4"

digitized_raw <- ggplot() + 
  geom_line(data = june1965, aes(x = Hour, y = Flow_cfs),color = "#1F77B4",alpha = 0.8,linewidth = 0.5)+
  scale_x_continuous(breaks = seq(0,168,24), minor_breaks = seq(0,168,6))+
  scale_y_continuous(breaks = seq(0,200000,20000), minor_breaks = seq(0,200000,5000),labels = scales::comma) + 
  coord_cartesian(xlim = c(4,144), ylim = c(0,160000))+
  labs(x = "Hour", y = "Inflow (cfs)")

digit_w_title <- digitized_raw + labs(title = "June 17th 1965 Inflow Hydrograph") + labs(subtitle = "Obtained from USACE Flood Report (Flood of June 1965)")

ggsave(paste0(plot_dir,"/June1965/June1965_digitized.png"), digitized_raw, height = 4, width = 6, dpi = 500)
ggsave(paste0(plot_dir,"/June1965/June1965_digitized_title.png"), digit_w_title, height = 4, width = 6, dpi = 500)

ggsave(paste0(out_dir,"June1965_digitized.png"), digitized_raw, height = 4, width = 6, dpi = 500)
ggsave(paste0(out_dir,"June1965_digitized_title.png"), digit_w_title, height = 4, width = 6, dpi = 500)

# Convert to clean timesteps ---------------------------------------------------
# Create clean timsteps
step <- 0.25 # hours
june_1965_clean_times <- tibble(
  Hour = seq(floor(min(june1965$Hour)),
             floor(max(june1965$Hour)),
             step),
  Flow_cfs = NA)

# Append to the existing data
june1965_clean <- june1965 %>%
  add_row(june_1965_clean_times) %>% 
  arrange(Hour)

# ID the digitize points
june1965_clean <- june1965_clean %>% 
  mutate(Digit_pt = ifelse(is.na(Flow_cfs),F,T))

# Interpolate between digitize points
june1965_clean <- june1965_clean %>% 
  mutate(
    Flow_cfs_spline = spline(x = Hour[!is.na(Flow_cfs)], 
                             y = Flow_cfs[!is.na(Flow_cfs)], 
                             xout = Hour, 
                             method = "natural")$y,
    
    Flow_cfs_NAapprox =  na.approx(Flow_cfs, x = Hour, na.rm = FALSE))

# Compare
june1965_test <- june1965_clean %>% filter(Digit_pt == F)

ggplot() +
  geom_point(data = june1965_test, aes(Hour, Flow_cfs_spline, color = "Zoo"), shape = 1,alpha = 0.5) +
  geom_point(data = june1965_test, aes(Hour, Flow_cfs_NAapprox, color = "Spline"), shape = 2,alpha = 0.5) +
  geom_line(data = june1965, aes(x = Hour, y = Flow_cfs),color = "#1F77B4",alpha = 0.8,linewidth = 0.5)+
  scale_color_manual(values = c("Spline" = "red3", "Zoo" = "darkgreen")) +
  scale_x_continuous(breaks = seq(0,168,24), minor_breaks = seq(0,168,6))+
  scale_y_continuous(breaks = seq(0,200000,20000), minor_breaks = seq(0,200000,5000),labels = scales::comma) + 
  coord_cartesian(xlim = c(4,144), ylim = c(0,160000))+
  labs(title = "Comparison of Interpolation Methods",x = "Hour", y = "Inflow (cfs)",color = "Method") +
  theme_bw()

# Use Spline -------------------------------------------------------------------
june1965_digitized_15min <- june1965_clean %>% 
  filter(Digit_pt == F) %>% 
  select(Hour,Flow_cfs_NAapprox) %>% 
  rename(Flow_cfs = Flow_cfs_NAapprox)

write_csv(june1965_digitized_15min, file = paste(out_dir,"June1965_15min.csv"))
