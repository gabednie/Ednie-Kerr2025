# Air Temperature Raster Creation ##############################################
library(terra)
library(dplyr)
library(stringr)

#-----------------------------#
# Define folders and settings
#-----------------------------#
te_dir <- "E:/PhD/Rasters/PhD Rasters/Corrected Temp"
out_dir <- "E:/PhD/Rasters/PhD Rasters/Air Temperature/AT Linear Simple"

#-----------------------------#
# Load and fit the linear model
#-----------------------------#
model_df <- read.csv("E:/PhD/Data Files/TempModel_Results.csv", stringsAsFactors = FALSE)
model_data <- model_df[, c("Air", "Ground")]
model_data <- na.omit(model_data)

lm_model <- lm(Air ~ Ground, data = model_data)
summary(lm_model)

#-----------------------------#
# Helper: Extract ID info
#-----------------------------#
get_id <- function(fname) {
  str_match(fname, "([A-Z]+)_([0-9A-Z]+)_([0-9]+)_([0-9]+)\\.tif")[, 2:5]
}

#-----------------------------#
# Get unique section-date combos from TE files
#-----------------------------#
te_files <- list.files(te_dir, pattern = "\\.tif$", full.names = TRUE)
ids <- do.call(rbind, lapply(te_files, get_id))
colnames(ids) <- c("Type", "Section", "Month", "Day")
unique_ids <- unique(ids[, c("Section", "Month", "Day")])

#-----------------------------#
# Main processing loop
#-----------------------------#
for (i in 1:nrow(unique_ids)) {
  sec <- unique_ids[i, "Section"]
  mth <- unique_ids[i, "Month"]
  day <- unique_ids[i, "Day"]
  id_suffix <- paste(sec, mth, day, sep = "_")
  
  out_name <- paste0("AT_", id_suffix, ".tif")
  out_path <- file.path(out_dir, out_name)
  
  if (file.exists(out_path)) {
    message("⏩ Already exists, skipping: ", out_name)
    next
  }
  
  message("🔄 Processing section: ", sec, " | date: ", mth, "-", day)
  
  # File paths
  te_file <- file.path(te_dir, paste0("TE_", id_suffix, ".tif"))
  
  # Check required files exist
  if (!file.exists(te_file)) {
    message("⚠️ Missing required raster(s) for: ", id_suffix)
    next
  }
  
  # Load raster
  te_r <- rast(te_file)
  
  names(te_r) <- "Ground"
  predictors <- te_r
  
  # Identify valid cells (no NA in predictors)
  mask_r <- sum(is.na(predictors)) == 0
  valid_cells <- which(values(mask_r) == 1)
  
  if (length(valid_cells) == 0) {
    message("⚠️ No valid pixels for ", id_suffix)
    next
  }
  
  df_all <- as.data.frame(predictors, na.rm = FALSE)
  df <- df_all[valid_cells, , drop = FALSE]
  valid_rows <- complete.cases(df) & apply(df, 1, function(x) all(is.finite(x)))
  
  df <- df[valid_rows, , drop = FALSE]
  valid_cells <- valid_cells[valid_rows]
  
  if (nrow(df) == 0) {
    message("⚠️ No valid pixels with finite values for ", id_suffix)
    next
  }
  
  # Predict air temperature
  preds <- predict(lm_model, newdata = df)
  
  # Create output raster
  out_r <- te_r
  values(out_r) <- NA
  values(out_r)[valid_cells] <- preds
  
  # Save output
  writeRaster(out_r, filename = out_path, overwrite = TRUE, wopt = list(gdal = "COMPRESS=LZW"))
  
  message("✅ Saved air temperature raster: ", out_name)
}



#----------------------------------------------------------------------#
# Check max min values all rasters
#----------------------------------------------------------------------#
library(terra)
library(dplyr)
library(fs)

# Folder containing the rasters
raster_dir <- "E:/PhD/Rasters/PhD Rasters/Air Temperature/AT Linear Model" #pick one
raster_dir <- "E:/Master's/Rasters/NEW RASTERS/Air Temperature"
raster_dir <- "E:/PhD/Rasters/PhD Rasters/Corrected Temp"

# List all .tif files (excluding Forage and Matrix subfolders)
raster_files <- dir_ls(raster_dir, regexp = "\\.tif$", recurse = FALSE)

# Function to get min and max for a single raster
get_min_max <- function(file) {
  r <- rast(file)
  stats <- global(r, fun = c("min", "max"), na.rm = TRUE)
  data.frame(
    File = path_file(file),
    Min = stats$min,
    Max = stats$max
  )
}

# Apply to all raster files
min_max_df <- bind_rows(lapply(raster_files, get_min_max))

# View result
print(min_max_df)
