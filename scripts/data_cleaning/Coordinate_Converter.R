# Load required library
if (!requireNamespace("sp", quietly = TRUE)) {
  install.packages("sp")
}
library(sp)
library(readr)

# Function to convert SVY21 to WGS84
convert_svy21_to_wgs84 <- function(x, y) {
  # Define SVY21 CRS
  svy21_crs <- CRS("+proj=tmerc +lat_0=1.366666 +lon_0=103.833333 +k=1.0 +x_0=28001.642 +y_0=38744.572 +datum=WGS84 +units=m +no_defs")
  
  # Define WGS84 CRS
  wgs84_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Create SpatialPoints object
  points <- SpatialPoints(cbind(x, y), proj4string = svy21_crs)
  
  # Transform to WGS84
  transformed_points <- spTransform(points, wgs84_crs)
  
  # Extract latitude and longitude
  coords <- coordinates(transformed_points)
  return(data.frame(latitude = coords[, 2], longitude = coords[, 1]))
}

# Main script
process_csv <- function(input_csv, output_csv) {
  # Read the CSV file
  data <- read.csv(input_csv)
  
  # Check if 'x' and 'y' columns exist
  if (!all(c("x", "y") %in% colnames(data))) {
    stop("The input CSV file must contain 'x' and 'y' columns.")
  }
  
  # Convert coordinates
  converted_coords <- convert_svy21_to_wgs84(data$x, data$y)
  
  # Append new columns to the data
  data$latitude <- converted_coords$latitude
  data$longitude <- converted_coords$longitude
  
  # Write the updated data to a new CSV file
  write_csv(data, output_csv, na = "")
}

# Example usage
# Uncomment the following lines to test the script
input_csv <- "Resources/URA_PrivateTransactions2025-04-02.csv"  # Replace with your input file path
output_csv <- sub("\\.csv$", "_WGS84.csv", input_csv)  # Append _WGS84 to the input file name
process_csv(input_csv, output_csv)
