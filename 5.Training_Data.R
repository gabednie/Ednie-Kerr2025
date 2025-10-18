library(sf)
library(dplyr)
library(tools)
library(terra)

# File paths
raster_folder <- "E:/PhD/Rasters/PhD Rasters/Corrected Temp"
north_buffer_folder <- "E:/PhD/iButton/Shapefiles/North"
south_buffer_folder <- "E:/PhD/iButton/Shapefiles/South"
csv_path <- "E:/PhD/Data Files/TempModel.csv"
output_csv <- "E:/PhD/Data Files/TempModel_Results.csv"

# Read TempModel CSV
temp_data <- read.csv(csv_path, stringsAsFactors = FALSE)

#-----------------------------#
# GROUND TEMPERATURE 
#-----------------------------#

# Function to extract mean from raster for each buffer
extract_mean_temp <- function(buffer_file, rasters_list, section, temp_data) {
  buffer_name <- file_path_sans_ext(basename(buffer_file))
  buffer <- st_read(buffer_file, quiet = TRUE)
  
  # Convert buffer (sf) to SpatVector (terra)
  buffer_spat <- vect(buffer)
  
  # Get relevant rows in the TempModel CSV
  sub_data <- temp_data %>%
    filter(Section == section, Origin == buffer_name)
  
  if (nrow(sub_data) == 0) return(temp_data)
  
  for (i in 1:nrow(sub_data)) {
    date <- as.Date(sub_data$Date[i])
    month <- as.numeric(format(date, "%m"))
    day <- as.numeric(format(date, "%d"))
    
    # Construct expected raster prefix
    raster_prefix <- if (section == "North") "TE_N" else "TE_S"
    
    # Match the raster by month and day
    matched_raster_name <- rasters_list[grepl(
      paste0("^", raster_prefix, "_", month, "_", day, "\\.tif$"), 
      rasters_list
    )]
    
    if (length(matched_raster_name) == 1) {
      raster_path <- file.path(raster_folder, matched_raster_name)
      
      # Read raster as SpatRaster (terra)
      r <- rast(raster_path)
      
      # Check if projections match and transform if necessary
      if (!st_crs(buffer) == st_crs(r)) {
        buffer_spat <- terra::project(buffer_spat, crs = st_crs(r))
      }
      
      # Ensure overlap before extraction
      if (st_intersects(buffer, st_as_sfc(st_bbox(r)), sparse = FALSE)[1]) {
        # Extract mean value under buffer
        extracted_values <- terra::extract(r, buffer_spat)
        
        # Check if any values were extracted (non-NA)
        if (!all(is.na(extracted_values[, 2]))) {
          mean_value <- mean(extracted_values[, 2], na.rm = TRUE)
          
          # Update the 'Ground' column in TempModel CSV
          temp_data$Ground[temp_data$Section == section &
                             temp_data$Origin == buffer_name &
                             temp_data$Date == as.character(date)] <- mean_value
        }
      }
    }
  }
  
  return(temp_data)
}

# List rasters
raster_files <- list.files(raster_folder, pattern = "^TE_[NS]_.*\\.tif$", full.names = FALSE)

# Process North
north_buffers <- list.files(north_buffer_folder, pattern = "\\.shp$", full.names = TRUE)
for (buffer in north_buffers) {
  temp_data <- extract_mean_temp(buffer, raster_files, "North", temp_data)
}

# Process South
south_buffers <- list.files(south_buffer_folder, pattern = "\\.shp$", full.names = TRUE)
for (buffer in south_buffers) {
  temp_data <- extract_mean_temp(buffer, raster_files, "South", temp_data)
}

# Save updated CSV
write.csv(temp_data, output_csv, row.names = FALSE)
cat("✅ TempModel_Results.csv saved.\n")

