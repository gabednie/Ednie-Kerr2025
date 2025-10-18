library(sf)

# Read CSV
csv_path <- "E:/PhD/Data Files/iButton_Location.csv"
locations <- read.csv(csv_path)

# Convert to sf object (assume coordinates are in WGS84 if not projected yet)
points_sf <- st_as_sf(locations, coords = c("Longitude", "Latitude"), crs = 4326)

# Check original CRS
cat("Original CRS:\n")
print(st_crs(points_sf))

# Define target CRS (WGS 84 / UTM zone 18N, EPSG:16018)
#target_crs <- 16018
target_crs <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"

# Transform to target CRS
points_proj <- st_transform(points_sf, crs = target_crs)

# Create 2m circular buffers
buffers <- st_buffer(points_proj, dist = 2)

# Create output paths for each UAV survey section
output_base <- "E:/PhD/iButton/Shapefiles"
north_path <- file.path(output_base, "North")
south_path <- file.path(output_base, "South")
dir.create(north_path, recursive = TRUE, showWarnings = FALSE)
dir.create(south_path, recursive = TRUE, showWarnings = FALSE)

# Write each shapefile
for (i in 1:nrow(buffers)) {
  name <- buffers$Origin[i]
  section <- buffers$Section[i]
  
  # Determine save path
  out_path <- ifelse(tolower(section) == "north", north_path, south_path)
  full_path <- file.path(out_path, paste0(name, ".shp"))
  
  # Write shapefile
  st_write(buffers[i, ], full_path, delete_layer = TRUE, quiet = TRUE)
}

cat("✅ All shapefiles created and projected to EPSG:16018\n")
