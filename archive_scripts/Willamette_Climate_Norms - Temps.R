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
tmean_dir <- dir_ls("data/",glob = "*tmean_30yr_norm",recurse = T)
tmean_files <- list.files(path = tmean_dir, pattern="\\.bil$", all.files=TRUE,full.names=TRUE)

ppt_dir <- dir_ls("data/",glob = "*ppt_30yr_norm",recurse = T)
ppt_files <- list.files(path = ppt_dir, pattern="\\.bil$", all.files=TRUE,full.names=TRUE)

# Month Strings - both separately incase they are out of order
tmean_months <- str_extract(tmean_files, "_(\\d{2}|annual)_bil(?=\\.bil$)") %>% 
  str_replace_all("_bil", "") %>%
  str_replace_all("^_", "")

ppt_months <- str_extract(ppt_files, "_(\\d{2}|annual)_bil(?=\\.bil$)") %>% 
  str_replace_all("_bil", "") %>%
  str_replace_all("^_", "")

# Create data frames of paths and labels  ######################################
tmean_df <- tibble(
  file = tmean_files,
  month = tmean_months,
  var = "tmean",
  out_dir = file.path(output_dir, "tmean"))

ppt_df <- tibble(
  file = ppt_files,
  month = ppt_months,
  var = "ppt",
  out_dir = file.path(output_dir, "ppt"))

raster_jobs <- bind_rows(tmean_df, ppt_df)

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

# Convert to English
tmean_F <- summary_stats %>% 
  filter(variable == "tmean")

tmean_F <- tmean_F %>% 
  mutate(across(c(min, max, mean, sd, median, q25, q75), ~(.x * (9/5)) + 32, .names = "{col}_F")) %>% 
  select(c(variable,month,ends_with("_F")))
  
ppt_IN <- summary_stats %>% 
  filter(variable == "ppt")

ppt_IN <- ppt_IN %>% 
  mutate(across(c(min, max, mean, sd, median, q25, q75), ~.x / 25.4, .names = "{col}_IN")) %>% 
  select(c(variable,month,ends_with("_In")))

# Create figures ###############################################################
wy_order <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08", "09")
wy_months <- as.character(c(lubridate::month(as.numeric(wy_order), label = TRUE, abbr = TRUE)))

# filter out annual
ppt_monthly <- ppt_IN %>%
  filter(month != "annual") %>%
  mutate(month = factor(month, levels = wy_order, ordered = TRUE))

temp_monthly <- tmean_F %>%
  filter(month != "annual") %>%
  mutate(month = factor(month, levels = wy_order, ordered = TRUE))

# Facet Plot monthly average temps #############################################
month_names <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
order_idx <- c(10:12, 1:9)

# Read files
temp_dir <- dir_ls("data/",glob = "*clipped/tmean",recurse = T)
temp_files <- list.files(path = temp_dir, pattern="\\.tif$", all.files=TRUE, full.names=TRUE)
temp_monthly_files <- temp_files[!grepl("annual", temp_files)]
temp_monthly_files_ordered <- temp_files[order_idx]

# Read all rasters using terra
monthly_temp_rasters <- lapply(temp_monthly_files_ordered, rast)

# Convert each raster to a data.frame with a month column
monthly_temp_raster_dfs <- mapply(function(r, name) {
  df <- as.data.frame(r, xy = TRUE)
  colnames(df)[3] <- "value"
  df$month <- name
  return(df)
}, monthly_temp_rasters, month_names, SIMPLIFY = FALSE)

# Combine all into one data.frame
monthly_temp_plotdf <- bind_rows(monthly_temp_raster_dfs)
monthly_temp_plotdf <- monthly_temp_plotdf %>%
  mutate(Deg_F = (value*(9/5)) + 32) %>% 
  mutate(plot_month = factor(month,levels = month_names))

# Color distiller
ggplot(monthly_temp_plotdf, aes(x = x, y = y, fill = Deg_F)) +
  geom_raster() +
  scale_fill_distiller(n = 6,palette = "RdYlBu",name = "Temp (°F)") +
  coord_equal() +
  facet_wrap(~ plot_month, ncol = 4) +
  theme_minimal() +
 # labs(title = "Monthly Mean Temperature", x = NULL, y = NULL) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))
# update save lines using fs
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Monthly_Temps.png",height = plot_height, width = plot_width, dpi = 300)

# Bin temps
breaks <- seq(20,70,length.out = 11)
temp_labels <- as.character(breaks[2:length(breaks)])

monthly_temp_plotdf <- monthly_temp_plotdf %>%
  mutate(temp_bin = cut(Deg_F, breaks = breaks, include.lowest = TRUE))

ggplot(monthly_temp_plotdf, aes(x = x, y = y, fill = temp_bin)) +
  geom_raster() +
  scale_fill_brewer(palette = "RdYlBu", name = "Temp (°F)",direction = -1,labels = temp_labels, na.translate = FALSE) +
  coord_equal() +
  facet_wrap(~ plot_month, ncol = 4) +
  theme_minimal() +
  # labs(title = "Monthly Mean Temperature", x = NULL, y = NULL) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Monthly_Temps_discrete.png",height = plot_height, width = plot_width, dpi = 300)

# Reclassify Terrain into Elevation Bands and then define elevation-temp relations ############################
# Read raster
raster_file <- dir_ls("data/",glob = "*USGS_30m_FT.tif",recurse = T)
terrain_ft <- rast(raster_file)
terrain_ft_clean <- clamp(terrain_ft, lower = 0)

terrain_ft_clean_df <- as.data.frame(terrain_ft_clean, xy = TRUE) %>%
  na.omit()
head(terrain_ft_clean_df)
terrain_ft_clean_df <- terrain_ft_clean_df %>% 
  rename(Elevation_ft = Band_1)

# Define elevation bands: lower bound, upper bound, new value
elev_classes <- tibble(low_elev = seq(0,10000,1000),
       upper_elev = seq(1000,11000,1000),
       mid_elev = (upper_elev + low_elev)/2)

elev_mat <- elev_classes %>%
  select(low_elev, upper_elev, mid_elev) %>%
  as.matrix()

terrain_reclass <- terra::classify(terrain_ft_clean, elev_classes,others = NA)

# Reclassed Terrain as DF
terrain_reclass_df <- as.data.frame(terrain_reclass, xy = TRUE) %>%
  na.omit()

head(terrain_reclass_df)
terrain_reclass_df <- terrain_reclass_df %>% 
  rename(Elevation_Band_Midpoint = Band_1)
unique(terrain_reclass_df$Elevation_Band_Midpoint)

# Polygon fo elevation Zones
terrain_bands_1k <- as.polygons(terrain_reclass,aggregate = T, values = T, na.rm = T)
# terrain_sf <- sf::st_as_sf(terrain_bands_2k)

# Load 12 monthly rasters
monthly_temp_rasters <- lapply(temp_monthly_files_ordered, rast)

# Storage
all_temp_values <- list()

# For each raster:
for (i in 1:length(monthly_temp_rasters)) {
  t_rast <- monthly_temp_rasters[[i]]
  
  # Stack zones and temp
  mon <- month_names[i]
  zonal_max <- unlist(unname(zonal(t_rast,terrain_bands_1k,"max", na.rm=TRUE)))
  zonal_min <- unlist(unname(zonal(t_rast,terrain_bands_1k,"min", na.rm=TRUE)))
  zonal_avg <- unlist(unname(zonal(t_rast,terrain_bands_1k,"mean", na.rm=TRUE)))
  
  # Convert to data.frame
  month_zonal_temps <- tibble(
    Month = mon,
    Elev_Band_Lower = elev_classes$low_elev,
    Elev_Band_Upper = elev_classes$upper_elev,
    Elev_Band_Mid = elev_classes$mid_elev,
    Max_Mon_Avg = (zonal_max*(9/5) + 32),
    Min_Mon_Avg = (zonal_min*(9/5) + 32),
    Avg_Mon_Avg = (zonal_avg*(9/5) + 32))
  
  # Store
  all_temp_values[[i]] <- month_zonal_temps
}

# Combine all
temperature_df <- bind_rows(all_temp_values)
elevation_bands <- unique(temperature_df$Elev_Band_Mid)

# Map Month name to number
temperature_df <- temperature_df %>%
  mutate(WY_month_num = recode(Month,
                               Jan = 1, Feb = 2, Mar = 3, Apr = 4,
                               May = 5, Jun = 6, Jul = 7, Aug = 8,
                               Sep = 9, Oct = 10, Nov = 11, Dec = 12
  ))

# Reorder factor with Water Year order: Oct → Sep
wy_labels <- month.abb[c(10:12, 1:9)]

temperature_df <- temperature_df %>%
  mutate(WY_month = factor(WY_month_num,
                           levels = c(10:12, 1:9),
                           labels = wy_labels,
                           ordered = TRUE))

# Plot All Bands
for (i in 1:length(elevation_bands)){
  elev_band <- elevation_bands[i]
  elev_band_name <- paste0("Elevation Band ",
                           scales::comma(temperature_df$Elev_Band_Lower[i]),"-",
                           scales::comma(temperature_df$Elev_Band_Upper[i]), " ft-NAVD88")
  
  temperature_df %>% 
    filter(Elev_Band_Mid == elev_band) %>% 
    ggplot() +
    geom_hline(yintercept = 32,color = "blue2", linetype = "dashed") +
    geom_line(aes(x = WY_month, y = Avg_Mon_Avg, group = 1),linewidth = 1) +
    geom_ribbon(aes(x = WY_month, ymin = Min_Mon_Avg, ymax = Max_Mon_Avg, group = 1), fill = "gray60", alpha = 0.15) +
    labs(subtitle = elev_band_name, y = "Avg Temp (°F) within Band", x = "Month") +
    theme(plot.subtitle = element_text(hjust = 0.5))+
    scale_x_discrete(expand = c(0,0.1)) +
    scale_y_continuous(breaks = seq(0,80,10),minor_breaks = seq(0,80,2)) +
    coord_cartesian(ylim = c(0,80))
#  ggsave(paste0("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Temp-ElevBands/Band_mid_",elev_band,"_ft.png"),height = plot_height, width = plot_width, dpi = 300)
    
}

# Facets
selected_bands <- c(2500, 3500, 4500, 5500)

temperature_df <- temperature_df %>%
  mutate(
    elev_label = paste0(
      scales::comma(Elev_Band_Lower), "–", scales::comma(Elev_Band_Upper), " ft"
    )
  )

temperature_df %>%
  filter(Elev_Band_Mid %in% selected_bands) %>%
  ggplot(aes(x = WY_month)) +
  geom_hline(yintercept = 32, color = "blue2", linetype = "dashed") +
  geom_ribbon(aes(ymin = Min_Mon_Avg, ymax = Max_Mon_Avg, group = elev_label),
              alpha = 0.15, fill = "gray60") +
  geom_line(aes(y = Avg_Mon_Avg, group = elev_label), linewidth = 1, color = "black") +
  facet_wrap(~ elev_label, ncol = 2) +
  scale_x_discrete(expand = c(0, .2)) +
  scale_y_continuous(breaks = seq(0, 80, 10), minor_breaks = seq(0, 80, 2)) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(x = "Month", y = "Avg Temp (°F)") +
  theme_minimal(base_size = 8)
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Temp-ElevBands/Bands_2500-5500ft.png",height = plot_height, width = plot_width, dpi = 300)

# plot terrains ################################################################
library(patchwork)

p1 <- ggplot(terrain_ft_clean_df, aes(x = x, y = y, fill = Elevation_ft)) +
  geom_raster() +
  scale_fill_gradientn(colours = terrain.colors(10),
                       name = "Elevation (ft)") +
  coord_equal() +
  labs(title = "Continuous Terrain (ft)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8))+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.75, "cm"))

# Use Polygon of reclassified layers
terrain_bands_1k_sf <- sf::st_as_sf(terrain_bands_1k)

custom_colors <- setNames(
  terrain.colors(length(unique(terrain_bands_1k_sf$Band_1))),
  levels(terrain_bands_1k_sf$Band_1))

p2 <- ggplot(terrain_bands_1k_sf, aes(fill = factor(Band_1))) +
  #geom_sf(color = "black", size = 0.001) +
  geom_sf(color = NA) +
  scale_fill_manual(values = custom_colors,
                    name = "Elev. Band (ft)") +
  labs(title = "Reclassified Elevation Bands (1k-ft)") +
  coord_sf() +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(labels = scales::label_number()) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 8))+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"))
    # plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

# # Ensure p2's legend is removed
# p2_clean <- p2 + theme(legend.position = "none")
# 
# # Move p1's legend to bottom
# p1_bottom_legend <- p1 + theme(legend.position = "bottom")

# Combine, keeping separate guides
# combined_plot <- p1_bottom_legend + p2_clean +
#   plot_layout(ncol = 2, guides = "keep")

combined_plot <- p1 + p2 #+  plot_layout(ncol = 2, guides = "keep")

#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Both_Terrain_Figs_R.png",combined_plot,height = plot_height, width = plot_width, dpi = 300)
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Terrain_Continuous_R.png",p1,height = plot_height, width = plot_width, dpi = 300)
#ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Climatology/Terrain_Classification_R.png",p2,height = plot_height, width = plot_width, dpi = 300)

