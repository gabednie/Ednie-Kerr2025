# Clean Thermal Limit Estimation
# Calculate climate and climatic positioning across continents 

#------------------------------------------------------------------------------#
#---------------------Yearly VERSION-------------------------------------------# #Works
#------------------------------------------------------------------------------#
# Load required libraries
library(tidyverse)
library(raster)
library(sp)
library(ncdf4)
library(rgdal)

# Define directories
dir_climdat <- "E:/PhD/Climate Grids/"
dir_speciesdata <- "E:/PhD/Data Files/Thermal Limits/"
outdir <- "E:/PhD/Data Files/Thermal Limits/"

# Define projections
proj_wsg84 <- "+proj=longlat +datum=WGS84 +no_defs"
proj_cea <- "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"

# Load CRU climate files
tmx_file <- list.files(dir_climdat, pattern = "tmx.*\\.nc$", full.names = TRUE)[1]
tmn_file <- list.files(dir_climdat, pattern = "tmn.*\\.nc$", full.names = TRUE)[1]

# Load full monthly stacks
climdat <- list(
  brick(tmx_file),  # monthly max temps
  brick(tmn_file)   # monthly min temps
)

# YEARLY AGGREGATION (using simple sequential index)
months_per_year <- 12
n_years <- nlayers(climdat[[1]]) / months_per_year
indices <- rep(1:n_years, each = months_per_year)

yearlymax_wsg <- stackApply(climdat[[1]], indices = indices, fun = max)
yearlymin_wsg <- stackApply(climdat[[2]], indices = indices, fun = min)

# Assign proper year names based on known CRU start year
start_year <- 1901
years <- start_year:(start_year + n_years - 1)
names(yearlymax_wsg) <- paste0("X", years)
names(yearlymin_wsg) <- paste0("X", years)

# Reproject to CEA
yearlymax <- projectRaster(yearlymax_wsg, crs = CRS(proj_cea))
yearlymin <- projectRaster(yearlymin_wsg, crs = CRS(proj_cea))

# Load species occurrence data
lepdata <- readRDS(paste0(dir_speciesdata, "lep2024.rds"))

# Preprocess: rename coordinates, check columns
if (!("species" %in% colnames(lepdata))) stop("species column missing.")
if (!("decimalLatitude" %in% colnames(lepdata) & "decimalLongitude" %in% colnames(lepdata))) stop("latitude/longitude columns missing.")

#lepdata <- lepdata %>% rename(latitude = decimalLatitude, longitude = decimalLongitude)
names(lepdata)[names(lepdata) == "decimalLatitude"] <- "latitude"
names(lepdata)[names(lepdata) == "decimalLongitude"] <- "longitude"

lepdata$year <- as.numeric(lepdata$year)

# Remove rows with NA year
lepdata <- lepdata %>% filter(!is.na(year))

# Filter to years matching climate data
valid_years <- 1901:2024
lepdata <- lepdata %>% filter(year %in% valid_years)

# Original thermal limits function (with CRS fix)
func_specieslimits <- function(lepdata, yearlymin, yearlymax){
  
  specieslist <- sort(unique(lepdata$species))
  
  lapply(specieslist, function(s){  #split data by species
    
    speciesdat <- lepdata %>% filter(species == s, year != 1900)
    
    yearlist <- sort(unique(speciesdat$year))
    
    year_maxes <- unlist(lapply(yearlist, function(yr){ #split data by yearXspecies
      
      lep_year <- speciesdat %>% filter(year== yr)
      
      lep_yearpts <- spTransform(SpatialPointsDataFrame(coords = dplyr::select(lep_year, longitude, latitude), 
                                                        data= lep_year, proj4string = CRS(proj_wsg84)), CRSobj = CRS(proj_cea))
      # extract the max
      maxes <- raster::extract(yearlymax[[paste0("X",yr)]], lep_yearpts)
      
      return(maxes)
    }))
    
    max5 <- mean(sort(year_maxes, decreasing = TRUE)[1:5])
    
    year_mins <- unlist(lapply(yearlist, function(yr){ #split data by yearXspecies
      
      lep_year <- speciesdat %>% filter(year== yr)
      
      lep_yearpts <- spTransform(SpatialPointsDataFrame(coords = dplyr::select(lep_year, longitude, latitude), 
                                                        data= lep_year, proj4string = CRS(proj_wsg84)), CRSobj = CRS(proj_cea))
      # extract the min
      mins <- raster::extract(yearlymin[[paste0("X",yr)]], lep_yearpts)
      
      return(mins)
    }))
    
    min5 <- mean(sort(year_mins, decreasing = FALSE)[1:5])
    
    return(c(s, as.numeric(min5), as.numeric(max5)))
  })
}

# Run extraction
specieslimits <- func_specieslimits(lepdata, yearlymin, yearlymax)
sink(paste0(outdir, "SpeciesLimits_Thermal_Yearly_All.txt")); print(specieslimits); sink()


#------------------------------------------------------------------------------#
#--------------------MONTHLY Version ------------------------------------------# # Makes NA
#------------------------------------------------------------------------------#
# Load required libraries
library(tidyverse)
library(raster)
library(sp)
library(ncdf4)
library(rgdal)

# Define directories
dir_climdat <- "E:/PhD/Climate Grids/"
dir_speciesdata <- "E:/PhD/Data Files/Thermal Limits/"
outdir <- "E:/PhD/Data Files/Thermal Limits/"

# Load CRU climate files

tmx_file <- list.files(dir_climdat, pattern = "tmx.*\\.nc$", full.names = TRUE)[1]
tmn_file <- list.files(dir_climdat, pattern = "tmn.*\\.nc$", full.names = TRUE)[1]

# --- Extract time labels from NetCDF metadata (critical step!) ---
nc <- nc_open(tmx_file)
time_vals <- ncvar_get(nc, "time")
time_units <- ncatt_get(nc, "time", "units")$value
nc_close(nc)

time_origin <- as.Date(sub("days since ", "", time_units))
layer_dates <- time_origin + time_vals
year_month_labels <- format(layer_dates, "%Y-%m")

# --- Load rasters using stack() with varname explicitly ---
tmx_stack <- stack(tmx_file, varname = "tmx")
tmn_stack <- stack(tmn_file, varname = "tmn")

# Apply proper names immediately after loading
names(tmx_stack) <- year_month_labels
names(tmn_stack) <- year_month_labels

cat("Climate data loaded. Date range in raster stack:\n")
print(head(names(tmx_stack)))
print(tail(names(tmx_stack)))

# --- Load species occurrence data ---
lepdata <- readRDS(paste0(dir_speciesdata, "lepdat_lyid.RDS"))

# Rename coordinate columns
lepdata$latitude <- lepdata$decimalLatitude
lepdata$longitude <- lepdata$decimalLongitude
lepdata$year <- as.numeric(lepdata$year)
lepdata$month <- as.numeric(lepdata$month)

# Filter to valid year and month
lepdata <- lepdata %>%
  filter(!is.na(year), !is.na(month), year >= 1901, month >= 1, month <= 12)

# Generate year-month keys to match raster layer names
lepdata$month_str <- sprintf("%02d", lepdata$month)
lepdata$ym_key <- paste0("X", lepdata$year, ".", sprintf("%02d", lepdata$month))

# SAFETY CHECK: verify year-month keys exist in raster stack
missing_keys <- setdiff(lepdata$ym_key, names(tmx_stack))
cat("Number of year-month keys not found in climate data:", length(missing_keys), "\n")
if(length(missing_keys) > 0){
  cat("Example missing keys:\n")
  print(head(missing_keys))
}

# Remove rows where no matching climate layer exists
lepdata <- lepdata %>% filter(ym_key %in% names(tmx_stack))
cat("Number of records after full filtering:", nrow(lepdata), "\n")

# --- Extraction function ---
func_specieslimits_monthly <- function(lepdata, tmin_stack, tmax_stack){
  specieslist <- sort(unique(lepdata$species))
  
  lapply(specieslist, function(s){
    speciesdat <- lepdata %>% filter(species == s)
    
    # Create spatial points (WGS84 projection)
    species_pts <- SpatialPointsDataFrame(
      coords = dplyr::select(speciesdat, longitude, latitude),
      data = speciesdat,
      proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")
    )
    
    # Extract temperatures with bilinear interpolation
    max_temps <- mapply(function(idx, key){
      if (key %in% names(tmax_stack)) {
        extract(tmax_stack[[key]], species_pts[idx, ], method = "bilinear")
      } else { NA }
    }, idx = seq_len(nrow(speciesdat)), key = speciesdat$ym_key)
    
    min_temps <- mapply(function(idx, key){
      if (key %in% names(tmin_stack)) {
        extract(tmin_stack[[key]], species_pts[idx, ], method = "bilinear")
      } else { NA }
    }, idx = seq_len(nrow(speciesdat)), key = speciesdat$ym_key)
    
    # Calculate 5 highest and 5 lowest
    max5 <- mean(sort(max_temps, decreasing = TRUE, na.last = NA)[1:5], na.rm = TRUE)
    min5 <- mean(sort(min_temps, decreasing = FALSE, na.last = NA)[1:5], na.rm = TRUE)
    
    cat("Species:", s, ": min5 =", round(min5,2), ", max5 =", round(max5,2), "\n")
    return(c(Species = s, Min5 = round(min5, 2), Max5 = round(max5, 2)))
  })
}

# --- Run extraction ---
specieslimits_monthly <- func_specieslimits_monthly(lepdata, tmn_stack, tmx_stack)

# --- Output results ---
outfile <- paste0(outdir, "SpeciesLimits_Thermal_Monthly_Bilinear_All.txt")
sink(outfile)
print(specieslimits_monthly)
sink()

cat("✅ Extraction complete. Results saved to:", outfile, "\n")
