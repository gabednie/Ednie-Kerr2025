##### CALCULATE FOLIAGE HEIGHT DIVERSITY ####################
# AKA SHANNON-WEINER INDEX WITH CANOPY HEIGHT
# AKA HETEROGENEITY INDEX

# Load required libraries
library(terra)

# Define paths to DTM, DSM, and output folders
# Note that all rasters follow the naming convention 
# "rastertype_surveylocation_month_day.tif" for the basename
dtm_folder <- "E:/PhD/Rasters/PhD Rasters/DTM" #raster name example MV_N_6_27_dtm.tif"
dsm_folder <- "E:/PhD/Rasters/PhD Rasters/DSM" #raster name example MV_N_6_27_dsm.tif"
output_folder <- "E:/PhD/Rasters/PhD Rasters/CHM" #raster name example CHM_N_6_27.tif"

# Get list of DTM and DSM files
dtm_files <- list.files(dtm_folder, pattern = "_dtm.tif$", full.names = TRUE)
dsm_files <- list.files(dsm_folder, pattern = "_dsm.tif$", full.names = TRUE)

# Loop through the DTM files
for (dtm_file in dtm_files) {
  # Extract the base name (without _dtm or _dsm) for matching
  base_name <- sub("_dtm.tif$", "", basename(dtm_file))
  
  # Find the corresponding DSM file
  dsm_file <- file.path(dsm_folder, paste0(base_name, "_dsm.tif"))
  
  # Check if the DSM file exists
  if (file.exists(dsm_file)) {
    # Load the DTM and DSM rasters
    dtm_raster <- rast(dtm_file)
    dsm_raster <- rast(dsm_file)
    
    # Check if the CRS of both rasters match
    if (!compareGeom(dtm_raster, dsm_raster, stopOnError = FALSE)) {
      # If they don't match, project DSM to DTM's CRS
      dsm_raster <- project(dsm_raster, crs(dtm_raster))
    }
    
    # Align the DSM raster to DTM (matching resolution and extent)
    dsm_aligned <- resample(dsm_raster, dtm_raster)
    
    # Clip both rasters to their overlapping extent
    overlap_extent <- intersect(ext(dtm_raster), ext(dsm_aligned))
    
    # Crop the rasters to the overlapping area
    dtm_cropped <- crop(dtm_raster, overlap_extent)
    dsm_cropped <- crop(dsm_aligned, overlap_extent)
    
    # Subtract DTM from DSM to create CHM
    chm_raster <- dsm_cropped - dtm_cropped
    
    # Modify the base name to match the desired output format
    output_name <- sub("^MV_", "CHM_", base_name)
    
    # Construct the full output file path
    chm_file <- file.path(output_folder, paste0(output_name, ".tif"))
    
    # Save the CHM raster
    writeRaster(chm_raster, chm_file, overwrite = TRUE)
    
    cat("CHM raster created:", chm_file, "\n")
  } else {
    cat("No matching DSM file for", dtm_file, "\n")
  }
}


# CALCULATE FOLIAGE HEIGHT INDICES #################
# Load required libraries
library(terra)
library(vegan)

# Define paths to CHM folder and output CSV file
chm_folder <- "E:/PhD/Rasters/PhD Rasters/CHM"
output_csv <- "E:/PhD/Data Files/Heterogeneity.csv"

# Get list of CHM files
chm_files <- list.files(chm_folder, pattern = ".tif$", full.names = TRUE)

# Initialize an empty data frame to store results
results <- data.frame(Date = character(),
                      Location = character(),
                      Shannon = numeric(),
                      Simpson = numeric(),
                      stringsAsFactors = FALSE)

# Define reclassification matrix
reclass_df <- c(-Inf, 0.1, 1,
                0.1, 0.5, 2,
                0.5, 1, 3,
                1, 1.5, 4,
                1.5, 2, 5,
                2, 2.5, 6,
                2.5, 3, 7,
                3, 3.5, 8,
                3.5, 4, 9,
                4.5, 5, 10,
                5, 5.5, 11,
                5.5, 6, 12,
                6, 6.5, 13,
                6.5, 7, 14,
                7, 7.5, 15,
                7.5, 8, 16,
                8, 8.5, 17,
                8.5, 9, 18,
                9, 9.5, 19,
                9.5, 10, 20,
                10, 10.5, 21,
                10.5, 11, 22,
                11, 11.5, 23,
                11.5, 12, 24,
                12, 12.5, 25,
                12.5, 13, 26,
                13, 13.5, 27,
                13.5, 14, 28,
                14, 14.5, 29,
                14.5, 15, 30,
                15, Inf, 31)

reclass_m <- matrix(reclass_df, ncol=3, byrow=TRUE)

# Loop through the CHM files
for (chm_file in chm_files) {
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(chm_file))
  
  # Extract file name parts for Date and Location
  file_name_parts <- unlist(strsplit(sub("CHM_", "", file_name), "_"))
  
  # Location from the first part of the file name
  location <- file_name_parts[1]
  
  # Construct the Date in "YYYY-MM-DD" format
  year <- "2024"
  month <- sprintf("%02d", as.numeric(file_name_parts[2]))  # Add leading zero to month
  day <- sprintf("%02d", as.numeric(file_name_parts[3]))    # Add leading zero to day
  date <- paste(year, month, day, sep = "-")
  
  # Load the CHM raster
  chm_raster <- rast(chm_file)
  
  # Reclassify the raster using the provided reclassification matrix
  chm_reclassified <- classify(chm_raster, reclass_m)
  
  # Convert the reclassified raster to a dataframe while removing NA values
  chm_df <- as.data.frame(values(chm_reclassified), na.rm = TRUE)
  
  # Calculate Shannon and Simpson diversity indices
  canopy_classes <- table(chm_df[,1])
  shannon_index <- diversity(canopy_classes, index = "shannon")
  simpson_index <- diversity(canopy_classes, index = "simpson")
  
  # Append the results to the dataframe
  results <- rbind(results, data.frame(Date = date,
                                       Location = location,
                                       Shannon = shannon_index,
                                       Simpson = simpson_index))
}

# Write the results to a CSV file
write.csv(results, output_csv, row.names = FALSE)

cat("Diversity indices saved to", output_csv, "\n")
