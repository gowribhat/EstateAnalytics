library(sf)
library(dplyr)
library(rvest)
library(stringr)

# Read the GeoJSON file
parks_data <- st_read("~/Documents/1. Academics/1. NUS/1. BBA/1. Y2S2/DBA3702/Assignments /Project/Parks.geojson")

# View the structure of the data
str(parks_data)

# Optionally, view the first few rows of the data
head(parks_data)
View(parks_data)

#Cleaning parks data
#remove objectid, x, y columns
parks_data <- parks_data[-c(1, 3, 4)] 

#change geometry to become separate columns (longitude & latitude)
parks_data <- parks_data %>%
  mutate(
    longitude = st_coordinates(geometry)[,1],
    latitude = st_coordinates(geometry)[,2])

parks_data<- parks_data %>%
  select(-geometry)
View(parks_data)

# Read the Gyms GeoJSON file
gyms_data <- st_read("~/Documents/1. Academics/1. NUS/1. BBA/1. Y2S2/DBA3702/Assignments /Project/GymsSGGEOJSON.geojson")
gyms_data <- gyms_data[-1]
gyms_data <- gyms_data %>%
  mutate(
    longitude = st_coordinates(geometry)[,1],
    latitude = st_coordinates(geometry)[,2],
    elevation = st_coordinates(geometry)[,3]  # The Z value (elevation)
  )
gyms_data <- gyms_data[-5]

# Function to extract data from HTML description
extract_data_from_html <- function(html_content) {
  # Parse the HTML content using rvest
  parsed_html <- read_html(html_content)
  
  # Extract all <td> (table data) elements from the HTML
  td_data <- html_nodes(parsed_html, "td")
  
  # Extract the text from each <td> tag
  extracted_values <- html_text(td_data)
  
  return(extracted_values)
}

# Apply the function to the 'Description' column of gyms_data
extracted_descriptions <- lapply(gyms_data$Description, extract_data_from_html)

# Check the structure of the extracted data
str(extracted_descriptions)

# Find the maximum number of elements in any row (to handle rows with varying lengths)
max_length <- max(sapply(extracted_descriptions, length))

# Fill shorter rows with NAs to ensure equal column length across all rows
extracted_df <- lapply(extracted_descriptions, function(x) c(x, rep(NA, max_length - length(x))))
extracted_df <- do.call(rbind, extracted_df)

# Convert the list into a data frame
extracted_df <- as.data.frame(extracted_df)

# Assign column names (adjust based on the number of attributes in your data)
colnames(extracted_df) <- paste0("Attribute", 1:ncol(extracted_df))

# View the extracted data
View(extracted_df)

gyms_desc <- extracted_df[-c(1,2,5,6,12,13)]
View(gyms_desc)

gyms_name <- gyms_desc[8]
View(gyms_name)
names(gyms_name)[1] <- "Name"

gyms_data <- gyms_data[-1]
gyms_data <- cbind(gyms_name, gyms_data)
View(gyms_data)
saveRDS(gyms_data, file = "/Users/tanwinnie/Documents/GitHub/Project/data/gyms_data.rds")
saveRDS(parks_data, file = "/Users/tanwinnie/Documents/GitHub/Project/data/parks_data.rds")