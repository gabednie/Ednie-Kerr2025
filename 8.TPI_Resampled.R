library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(plyr)
library(raster)
library(rgdal)
library(terra)

# Load species-specific thermal limits from CSV (with precomputed codes)
species_limits <- read.csv("E:/PhD/Data Files/Thermal Limits/SpeciesLimits_Thermal_Monthly_Cleaned_WithCode.csv", 
                           stringsAsFactors = FALSE)

# Rename columns
names(species_limits) <- c("species", "min5", "max5", "code")

species_list <- setNames(lapply(1:nrow(species_limits), function(i) {
  min5 <- species_limits$min5[i]
  max5 <- species_limits$max5[i]
  function(x) (x + min5) / (max5 + min5)
}), species_limits$species)

# Use precomputed species codes
species_codes <- setNames(species_limits$code, species_limits$species)

# Directories
in_dir <- "E:/PhD/Rasters/PhD Rasters/Air Temperature" # naming convention is "AT_section_month_day.tif"
out_dir <- "E:/PhD/Rasters/PhD Rasters/TPI Linear Simple"

# Create output directories
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
site_dir <- file.path(out_dir, "SITE")
if (!dir.exists(site_dir)) dir.create(site_dir)
species_dir <- file.path(out_dir, "SPECIES")
if (!dir.exists(species_dir)) dir.create(species_dir)
for (code in unique(species_codes)) {
  dir.create(file.path(species_dir, code), showWarnings = FALSE)
}

# List rasters
rasters <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
message(length(rasters), " rasters found.")

site_date_map <- list()

for (r in rasters) {
  base_name <- tools::file_path_sans_ext(basename(r))  # e.g., AT_S_7_2
  site_part <- sub("^AT_", "", base_name)            # e.g., S_7_2
  rast_in <- rast(r)
  
  for (species in names(species_list)) {
    code <- species_codes[[species]]
    fun <- species_list[[species]]
    message("Applying transformation for species: ", species, " [", code, "]")
    rast_out <- app(rast_in, fun)
    out_name <- paste0(code, "_", site_part, ".tif")
    out_path <- file.path(species_dir, code, out_name)
    writeRaster(rast_out, out_path, overwrite = TRUE)
    message("Saved: ", out_path)
    
   
  }
}

# Reorganize into SITE folders
# Loop through the site_date_map and group rasters into folders by site and date
library(tools)

# Define the path to the SPECIES folder
species_dir <- "E:/PhD/Rasters/PhD Rasters/TPI Linear Simple/SPECIES"

# List all .tif files recursively
tif_files <- list.files(species_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

# Extract base names and derive Site_Month_Day keys
base_names <- file_path_sans_ext(basename(tif_files))
site_md_keys <- sapply(base_names, function(x) {
  parts <- unlist(strsplit(x, "_"))
  if (length(parts) >= 4) {
    paste(parts[2], parts[3], parts[4], sep = "_")  # site_month_day
  } else {
    NA
  }
})

# Clean and sort unique keys
unique_keys <- sort(unique(na.omit(site_md_keys)))

# Print preview
cat("📌 Unique Site_Month_Day combinations found:\n")
print(unique_keys)
cat("Total unique combinations:", length(unique_keys), "\n")

library(fs)

# Output directory for SITE organization
site_dir <- "E:/PhD/Rasters/PhD Rasters/TPI Linear Simple/SITE"

# Loop through each unique site_month_day key
for (key in unique_keys) {
  # Split key into components
  parts <- unlist(strsplit(key, "_"))
  site <- parts[1]
  month <- parts[2]
  day <- parts[3]
  
  # Create site identifier and date string
  site_id <- site
  date_str <- paste0(month, "_", day)
  
  # Create a subdirectory under SITE
  subfolder <- paste0(site_id, "_", date_str)
  folder_path <- file.path(site_dir, subfolder)
  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)
  
  # Find all matching rasters
  matching_rasters <- tif_files[grepl(paste0("_", site_id, "_", month, "_", day, "\\.tif$"), tif_files)]
  
  # Copy each raster to the correct folder
  for (f in matching_rasters) {
    file.copy(from = f, to = folder_path, overwrite = TRUE)
  }
  
  message("📂 Copied ", length(matching_rasters), " files to ", folder_path)
}


