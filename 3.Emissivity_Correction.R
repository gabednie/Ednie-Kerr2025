# Load necessary libraries
library(terra)
library(stringr)
library(dplyr)

#-----------------------------------------------------------------------#
# Resample and Clip all emissivity rasters to match temperature rasters
#-----------------------------------------------------------------------#

# Define the paths to the folders containing the rasters
temp_path <- "E:/PhD/Rasters/PhD Rasters/Thermal"
emissivity_path <- "E:/PhD/Rasters/PhD Rasters/Emissivity"
output_path <- "E:/PhD/Rasters/PhD Rasters/Clipped Emissivity"

# Get a list of temperature and emissivity raster files
temp_files <- list.files(temp_path, pattern = "\\.tif$", full.names = TRUE)
emissivity_files <- list.files(emissivity_path, pattern = "\\.tif$", full.names = TRUE)

# Function to extract the numerical sequence from the filename
# Note that the naming convention used is "file type_surveylocation_month_day.tif"
extract_sequence <- function(filename) {
  # Adjust the regex pattern to match your file naming convention
  #match <- str_match(basename(filename), "_([0-9_]+)\\.tif$")
  rast_name <- strsplit(filename, split = "_")[[1]][-1]
  rast_name <- paste(rast_name, collapse="_")
  match <- sub('\\.tif$', '', rast_name)
  return(match)
}

# Create a named list of temperature and emissivity files by their sequence
temp_list <- setNames(temp_files, sapply(temp_files, extract_sequence))
emissivity_list <- setNames(emissivity_files, sapply(emissivity_files, extract_sequence))

# Loop through the sequences and perform the operations
for (sequence in intersect(names(temp_list), names(emissivity_list))) {
  temp_file <- temp_list[[sequence]]
  emissivity_file <- emissivity_list[[sequence]]
  
  # Load the rasters
  temp_raster <- rast(temp_file)
  emissivity_raster <- rast(emissivity_file)
  
  # Resample emissivity raster to match temperature raster
  emissivity_resampled <- resample(emissivity_raster, temp_raster, method = 'bilinear')
  
  # Remove areas of emissivity raster that don't overlap with temp raster
  temp_binary <- temp_raster
  temp_binary[is.na(temp_raster)] <- 0
  temp_binary[temp_raster > 0] <- 1
  
  c_emissivity <- temp_binary * emissivity_resampled
  c_emissivity[c_emissivity < 0.7] <- NA
  
  # Define the output filename and path
  output_filename <- paste0("CE_", sequence, ".tif")
  output_filepath <- file.path(output_path, output_filename)
  
  # Save the resulting clipped emissivity raster
  writeRaster(c_emissivity, output_filepath, overwrite=TRUE)
  
  # Print a message indicating the file has been processed
  cat("Processed:", sequence, "\n")
}

#---------------------------------#
# MAIN LOOP EMISSIVITY CORRECTION
#---------------------------------#

# Define paths to the folders containing the rasters
temp_path <- "E:/PhD/Rasters/PhD Rasters/Thermal"
emissivity_path <- "E:/PhD/Rasters/PhD Rasters/Clipped Emissivity"
output_path <- "E:/PhD/Rasters/PhD Rasters/Corrected Temp"

# Get a list of temperature and emissivity raster files
temp_files <- list.files(temp_path, pattern = "\\.tif$", full.names = TRUE)
emissivity_files <- list.files(emissivity_path, pattern = "\\.tif$", full.names = TRUE)

# Function to extract the numerical sequence from the filename
extract_sequence <- function(filename) {
  # Adjust the regex pattern to match your file naming convention
  rast_name <- strsplit(filename, split = "_")[[1]][-1]
  rast_name <- paste(rast_name, collapse="_")
  match <- sub('\\.tif$', '', rast_name)
  return(match)
}

# Create a named list of temperature and emissivity files by their sequence
temp_list <- setNames(temp_files, sapply(temp_files, extract_sequence))
emissivity_list <- setNames(emissivity_files, sapply(emissivity_files, extract_sequence))

# Define the function to perform emissivity correction
correct_temperature <- function(temperature, c_emissivity) {
  # Convert temperature from Celsius to Kelvin
  temp_k <- temperature + 273.15
  
  # Planck's Law constants
  h <- 6.626e-34 # Planck's constant (Joule*Seconds)
  speed <- 300000000 # Speed of light (meters/second)
  k <- 1.38e-23 # Boltzmann's constant (Joules/Kelvin)
  w <- 0.00011 # Wavelength in meters
  
  # Function to calculate radiance
  radiance <- function(x) {
    (2 * h * speed^2 / w^5) * (1 / (exp((h * speed) / (w * k * x)) - 1))
  }
  
  # Calculate observed radiance
  radiance_raster <- app(temp_k, radiance)
  
  # Perform emissivity correction
  radiance_by_emissivity <- radiance_raster / c_emissivity
  
  # Function to calculate the actual temperature
  correct_temp_k <- function(y) {
    (h * speed) / (log((2 * h * speed^2 / (w^5 * y)) + 1) * (w * k))
  }
  
  # Calculate corrected temperature in Kelvin
  corrected_temp_k <- app(radiance_by_emissivity, correct_temp_k)
  
  # Handle NA and infinite values
  corrected_temp_k <- clamp(corrected_temp_k, lower = -Inf, upper = Inf, values = TRUE)
  corrected_temp_k[is.infinite(corrected_temp_k)] <- NA
  
  # Convert corrected temperature back to Celsius
  corrected_temp_c <- corrected_temp_k - 273.15
  return(corrected_temp_c)
}

# Loop through the sequences and perform the operations
for (sequence in intersect(names(temp_list), names(emissivity_list))) {
  temp_file <- temp_list[[sequence]]
  emissivity_file <- emissivity_list[[sequence]]
  
  # Load the rasters
  temp_raster <- rast(temp_file)
  emissivity_raster <- rast(emissivity_file)
  
  # Apply the correction function to the raster data
  corrected_temperature_raster <- correct_temperature(temp_raster, emissivity_raster)
  
  # Define the output filename and path
  output_filename <- paste0("TE_", sequence, ".tif")
  output_filepath <- file.path(output_path, output_filename)
  
  # Save the corrected temperature raster
  writeRaster(corrected_temperature_raster, output_filepath, overwrite=TRUE)
  
  # Print a message indicating the file has been processed
  cat("Processed:", sequence, "\n")
}
