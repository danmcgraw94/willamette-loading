# Willamette Basin Climate Normals
# 1991 - 2020 Mean Temp and Precip

# 7/14/2025
rm(list = ls(all.names = TRUE))

# Libraries
library(tidyverse)
library(terra)
library(furrr)
library(fs)
library(progressr)
library(lubridate)
handlers(global = TRUE)  # Progress bar

# Theme set
theme_set(theme_bw())
source("D:/R/theme_USACE.r")
today = format(Sys.Date(),"%d-%b-%Y")
plot_height = 5
plot_width = 7

# Function ####################################################################
# Compute clipped raster and summary stats
process_raster <- function(file_path, shapefile_path, var_name, month_label, out_dir) {
  shape <- terra::vect(shapefile_path)
  
  rast_clipped <- rast(file_path) %>%
    crop(shape) %>%
    mask(shape)
  
  values <- values(rast_clipped, na.rm = TRUE)
  
  stats <- tibble(
    variable = var_name,
    month = month_label,
    min = min(values),
    max = max(values),
    mean = mean(values),
    sd = sd(values),
    median = median(values),
    q25 = quantile(values, probs = 0.25),
    q75 = quantile(values, probs = 0.75)
  )
  
  out_name <- file.path(out_dir, paste0(var_name, "_", month_label, "_clipped.tif"))
  writeRaster(rast_clipped, out_name, overwrite = TRUE)
  
  return(stats)
}

# Load Willamette Basin shapefile ##############################################
willam_huc <- dir_ls("data/",glob = "*Willamette_HUC4.shp",recurse = T)
willam_shape <- vect(willam_huc)

# Ensure output dirs for clipped rasters  ######################################
prism_dir <- file.path(getwd(),"data","GIS","PRISM")
prism_clip <- file.path(getwd(),"data","GIS","PRISM","clipped")

dir.create(prism_dir)
dir.create(prism_clip)

dir.create(file.path(prism_clip, "tmean"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(prism_clip, "ppt"), recursive = TRUE, showWarnings = FALSE)

# CSV Export
csv_export_dir <- file.path(getwd(),"data","Climate","Normals")
dir.create(csv_export_dir, showWarnings = FALSE)

# Load  Climate Normal Data  ###################################################
ppt_dir <- dir_ls("data/",glob = "*ppt_30yr_norm",recurse = T)
ppt_files <- list.files(path = ppt_dir, pattern="\\.bil$", all.files=TRUE,full.names=TRUE)

# Month Strings - both separately incase they are out of order
ppt_months <- str_extract(ppt_files, "_(\\d{2}|annual)_bil(?=\\.bil$)") %>% 
  str_replace_all("_bil", "") %>%
  str_replace_all("^_", "")

# Create data frames of paths and labels  ######################################
ppt_df <- tibble(
  file = ppt_files,
  month = ppt_months,
  var = "ppt",
  out_dir = file.path(output_dir, "ppt"))

raster_jobs <- bind_rows(ppt_df)

# Set up parallell processing plan #############################################
# Set up parallel plan
plan(multisession)  # or multicore on Linux/macOS

# Process all rasters in parallel
summary_stats <- with_progress({
  furrr::future_pmap_dfr(
    .l = list(
      file_path = raster_jobs$file,
      shapefile_path = willam,  # pass shapefile path, not object
      var_name = raster_jobs$var,
      month_label = raster_jobs$month,
      out_dir = raster_jobs$out_dir
    ),
    .f = process_raster,
    .options = furrr::furrr_options(seed = TRUE, scheduling = 1)
  )
})

# Write summary CSV
write_csv(summary_stats, file.path(csv_export_dir, "climate_normals_summary_metric.csv"))

summary_stats <- read.csv(paste0(csv_export_dir, "/climate_normals_summary_metric.csv"))

# Convert to English
ppt_IN <- summary_stats %>% 
  filter(variable == "ppt")

ppt_IN <- ppt_IN %>% 
  mutate(across(c(min, max, mean, sd, median, q25, q75), ~.x / 25.4, .names = "{col}_IN")) %>% 
  select(c(variable,month,ends_with("_In")))

write_csv(ppt_IN, file.path(csv_export_dir, "climate_normals_summary_INCHES.csv"))

# Create figures ###############################################################
wy_order <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08", "09")
wy_months <- as.character(c(lubridate::month(as.numeric(wy_order), label = TRUE, abbr = TRUE)))

# filter out annual
ppt_monthly <- ppt_IN %>%
  filter(month != "annual") %>%
  mutate(month = factor(month, levels = wy_order, ordered = TRUE))

# Bar Chart of monthly means
ggplot(ppt_monthly, aes(x = month, y = mean_IN)) +
  geom_col(fill = "lightblue",color = "black") +
  labs(
    x = "Month",
    y = "Mean Precipitation (IN)") +
  scale_x_discrete(labels = wy_months) +
  scale_y_continuous(breaks = seq(0,15,1), minor_breaks = seq(0,15,.25)) + 
  coord_cartesian(ylim = c(0,12))
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Monthly_Precip.png",height = plot_height, width = plot_width, dpi =300)

# Box plot of monthly means
ggplot(ppt_monthly, aes(x = month)) +
  # Whiskers (min to Q1 and Q3 to max)
  geom_segment(aes(y = min_IN, yend = q25_IN, xend = month), linewidth = 0.5) +
  geom_segment(aes(y = q75_IN, yend = max_IN, xend = month), linewidth = 0.5) +

  # Box (Q1 to Q3 with median line)
  geom_crossbar(aes(ymin = q25_IN, y = median_IN, ymax = q75_IN),
                fill = "lightblue", color = "black", width = 0.6, fatten = 1) +
  
  # Optionally, add points for min/max
  # geom_point(aes(y = min_IN), shape = 3, size = 2) +
  # geom_point(aes(y = max_IN), shape = 3, size = 2) +
  labs(x = "Month",
    y = "Precipitation (IN)") +
  scale_x_discrete(labels = wy_months) +
  scale_y_continuous(breaks = seq(0,45,5), minor_breaks = seq(0,45,1)) + 
  coord_cartesian(ylim = c(0,35))
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Monthly_Precip_Distribution.png",height = plot_height, width = plot_width, dpi =300)

# Plot of annual Precip #########################################################
ppt_dir <- dir_ls("data/",glob = "*clipped/ppt",recurse = T)
ppt_files <- list.files(path = ppt_dir, pattern="\\.tif$", all.files=TRUE,full.names=TRUE)

annual_precip <- rast(ppt_files[14])

annual_precip_df <- as.data.frame(annual_precip, xy = TRUE) %>%
  na.omit()
head(annual_precip_df)

annual_precip_df <- annual_precip_df %>% 
  rename(Precip_mm = PRISM_ppt_30yr_normal_800mM4_annual_bil) %>% 
  mutate(Precip_in = Precip_mm/25.4)

ggplot(annual_precip_df, aes(x = x, y = y, fill = Precip_in)) +
  geom_raster() +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdYlGn")),
                       name = "Annual Precipitation (in)") +
  coord_equal() +
  #labs(title = "Continuous Terrain (ft)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.title = element_text(size = 8))
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Annual_Precip.png",height = plot_height, width = plot_width, dpi = 300)


max(annual_precip_df$Precip_in)
min(annual_precip_df$Precip_in)
mean(annual_precip_df$Precip_in)
