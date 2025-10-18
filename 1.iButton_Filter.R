# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)  # For time conversion
library(stringr)    # For string replacement

# Define directories
ground_dir <- "E:/PhD/iButton"  # Path to iButton csv data
when_file <- "E:/PhD/Data Files/When.csv"  # Path to "When.csv" describing the dates and times of each UAV survey

# Read "When" CSV
when_data <- read_csv(when_file)

# Filter the "When" data to only rows with the section of interest
when_data <- when_data %>% filter(Section == "South") #may not be necessary

# Convert StartTime and EndTime columns to 24-hour format
# First replace "a.m." and "p.m." with "AM" and "PM"
when_data <- when_data %>%
  mutate(StartTime = str_replace_all(StartTime, c("a\\.m\\." = "AM", "p\\.m\\." = "PM")),
         EndTime = str_replace_all(EndTime, c("a\\.m\\." = "AM", "p\\.m\\." = "PM")),
         StartTime = format(parse_date_time(StartTime, orders = "I:M:S p"), "%H:%M:%S"),
         EndTime = format(parse_date_time(EndTime, orders = "I:M:S p"), "%H:%M:%S"))

# Create a dataframe to store the results
results <- data.frame(Date = character(), Air = numeric(), Origin = character(), stringsAsFactors = FALSE)

# List all CSV files in "Ground" folder
ground_files <- list.files(path = ground_dir, pattern = "*.csv", full.names = TRUE)

# Iterate over each file in the "Ground" folder
for (file in ground_files) {
  
  # Read the current Ground file and skip the first 19 rows
  ground_data <- read_csv(file, skip = 19)
  
  # Rename the columns
  colnames(ground_data)[1] <- "Date"
  colnames(ground_data)[2] <- "Time"
  colnames(ground_data)[3] <- "Air"
  
  # Replace "a.m." and "p.m." with "AM" and "PM" in the "Time" column, then convert to 24-hour format
  ground_data <- ground_data %>%
    mutate(Time = str_replace_all(Time, c("a\\.m\\." = "AM", "p\\.m\\." = "PM")),
           Time = format(parse_date_time(Time, orders = "I:M:S p"), "%H:%M:%S"))
  
  # Remove "C," from every value in the "Air" column
  ground_data$Air <- as.numeric(gsub("C,", "", ground_data$Air))
  
  # Iterate over each row in the "When" CSV to get date, start time, and end time
  for (i in 1:nrow(when_data)) {
    current_date <- when_data$Date[i]
    start_time <- when_data$StartTime[i]
    end_time <- when_data$EndTime[i]
    
    # Filter the ground data by date and time range
    filtered_data <- ground_data %>%
      filter(Date == current_date & Time >= start_time & Time <= end_time)
    
    # If there are temperature values left after filtering
    if (nrow(filtered_data) > 0) {
      # Group the data by date and calculate the average temperature per date
      avg_data <- filtered_data %>%
        group_by(Date) %>%
        summarise(Air = mean(Air, na.rm = TRUE), .groups = 'drop')
      
      # Add the results to the results dataframe, including the origin file name without ".csv"
      results <- rbind(results, data.frame(Date = avg_data$Date, 
                                           Air = avg_data$Air, 
                                           Origin = gsub(".csv", "", basename(file)),  # Remove ".csv"
                                           stringsAsFactors = FALSE))
    }
  }
}

# Write the results to the "TempModel.csv" file
write_csv(results, "E:/PhD/Data Files/TempModel.csv")


##########################################################################################
## IN CASE NEED TO TRANSFORM COORDINATES FROM DEGREES MINUTES SECONDS TO DECIMAL DEGREE ##
#########################################################################################

library(dplyr)

# Read the CSV file
data <- read.csv("E:/PhD/Data Files/iButton_Location_DMS.csv")

# Function to convert DMS to Decimal Degrees and handle direction
dms_to_dd <- function(dms) {
  # Extract degrees, minutes, seconds, and direction using regex
  matches <- regmatches(dms, regexec("([0-9]+)°([0-9]+)'([0-9\\.]+)\"([NSEW])", dms))
  
  # Convert parts to numeric
  deg <- as.numeric(matches[[1]][2])
  min <- as.numeric(matches[[1]][3])
  sec <- as.numeric(matches[[1]][4])
  direction <- matches[[1]][5]
  
  # Convert to decimal degrees
  dd <- deg + (min / 60) + (sec / 3600)
  
  # Adjust for direction
  if (direction %in% c("S", "W")) {
    dd <- -dd
  }
  
  # Round to 8 decimal places
  dd <- round(dd, 8)
  
  return(dd)
}

# Apply the conversion function to Latitude and Longitude columns
data <- data %>%
  mutate(
    Latitude_DD = sapply(Latitude, dms_to_dd),
    Longitude_DD = sapply(Longitude, dms_to_dd)
  )

# View the updated data
print(data)

# Optionally save the result to a new CSV file
write.csv(data, "E:/PhD/Data Files/iButton_Location_DD.csv", row.names = FALSE)

