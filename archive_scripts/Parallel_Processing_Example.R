library(terra)
library(future)    # For parallel processing backend
library(furrr)     # For parallel mapping functions (like purrr::map but parallel)

# --- 0. Define your terrain raster (template for resampling) ---
# This needs to be loaded outside the parallel loop as it's the target geometry
terrain_30m_ft <- rast("D:/0.RMC/Willamette/2025-Report/data/GIS/Willamette/Lidar_30m/USGS_30m_FT.tif")

# --- 1. Define your list of temperature file paths ---
# Assuming temp_files is already defined and contains paths to all 13 monthly/annual temp TIFFs
# Example (replace with your actual temp_files variable):
# temp_files <- c(
#   "D:/0.RMC/Willamette/2025-Report/data/GIS/Willamette/path_to_your_jan_temp.tif",
#   "D:/0.RMC/Willamette/2025-Report/data/GIS/Willamette/path_to_your_feb_temp.tif",
#   # ... add all 13 paths here ...
#   "D:/0.RMC/Willamette/2025-Report/data/GIS/Willamette/path_to_your_dec_temp.tif",
#   "D:/0.RMC/Willamette/2025-Report/data/GIS/Willamette/path_to_your_annual_temp.tif"
# )

# --- 2. Set up parallel processing ---

# Determine how many cores to use.
# It's generally good practice to leave one core free for system operations.
# You can adjust this based on your CPU.
num_cores <- parallel::detectCores() - 1
if (num_cores < 1) num_cores <- 1 # Ensure at least 1 core

# Set up the parallel plan. 'multisession' is good for local parallelization.
# 'multicore' can be faster on Linux/macOS, but 'multisession' is safer across OS.
plan(multisession, workers = num_cores)
message(paste("Using", num_cores, "cores for parallel processing."))

# --- 3. Define a function to load and resample a single raster ---
# This function will be applied to each file path in parallel.
# We pass 'terrain_template' as an argument to ensure it's available to each worker.
process_temp_raster <- function(file_path, terrain_template) {
  # Load the temperature raster
  temp_raster <- rast(file_path)
  
  # Harmonize CRS if necessary (though you confirmed they match, it's good practice)
  if (!same.crs(temp_raster, terrain_template)) {
    message(paste("Reprojecting", basename(file_path), "to match template CRS."))
    temp_raster <- project(temp_raster, terrain_template)
  }
  
  # Resample to match the terrain raster's geometry
  # 'bilinear' is suitable for continuous temperature data
  temp_resampled <- resample(temp_raster, terrain_template, method="bilinear")
  
  return(temp_resampled)
}

# --- 4. Process all temperature files in parallel ---
# future_map applies the 'process_temp_raster' function to each file in 'temp_files'
# and distributes the work across the parallel workers.
# The 'terrain_template = terrain_30m_ft' argument ensures the template raster
# is passed to each worker.
list_of_resampled_rasters <- future_map(temp_files, process_temp_raster, terrain_template = terrain_30m_ft)

# --- 5. Name the resampled rasters for easier access ---
# You can use the original file names or create more descriptive names
# For example, extract month names from file paths if they are consistent,
# or use a predefined list of names.
# Assuming temp_files contains names like "temp_jan.tif", "temp_feb.tif", etc.
# We'll use a simple sequential naming for this example.
names(list_of_resampled_rasters) <- paste0("temp_resampled_", 1:length(list_of_resampled_rasters))

# You can now access individual resampled rasters like:
# temp_resampled_1 (for January)
# temp_resampled_2 (for February)
# ...
# temp_resampled_13 (for Annual)

# Example: Accessing the January resampled raster
# temp_jan_resampled <- list_of_resampled_rasters$temp_resampled_1
# print(temp_jan_resampled)

# --- 6. (Optional) Combine all resampled rasters into a single SpatRaster stack ---
# This can be useful for further analysis or visualization as a multi-band raster.
# Ensure all individual rasters in the list have identical geometry before stacking.
# The 'resample' step above should guarantee this.
final_temp_stack <- c(list_of_resampled_rasters)

# You can rename layers in the stack for clarity
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Annual")
names(final_temp_stack) <- paste0("Temp_", month_names)

cat("\n--- Final Stack of Resampled Temperature Rasters ---\n")
print(final_temp_stack)

# --- 7. (Optional) Extract all data to a single DataFrame ---
# If you want a single DataFrame with Elevation and all monthly temperatures
# This will be a wide DataFrame.
df_all_data <- as.data.frame(c(terrain_30m_ft, final_temp_stack), xy=TRUE)

# Filter out rows where elevation is NA (assuming terrain is your definitive extent)
df_final_all_months <- df_all_data %>%
  filter(!is.na(Elevation_ft)) # Assuming 'Elevation_ft' is the name of terrain_30m_ft in the stack

# Also remove NAs from temperature columns if any resulted from resampling outside original extent
# (though the resample to terrain_30m_ft should mostly align this)
# You might want to decide how to handle NAs if some months have missing data in areas
# where terrain exists. For now, we'll keep them if terrain is present.

cat("\n--- First few rows of the final DataFrame with all months ---\n")
print(head(df_final_all_months))

# --- 8. Shut down parallel workers ---
plan(sequential) # Always a good idea to revert to sequential plan when done
message("Parallel processing complete. Workers shut down.")