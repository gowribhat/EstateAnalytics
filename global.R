# Global settings, package loading, and data preprocessing
# Find Your New Home: Data-Driven Property Search Platform
# DBA3702 Team 3

# Load packages from centralized package management
source("scripts/load_packages.R")

# Explicitly load DT for DataTable functionality
library(DT)

# Define paths
resources_path <- "data/"

# Create a data registry to centralize all data specifications
data_registry <- list(
  # Geospatial files
  planning_areas = list(
    var_name = "planning_areas_data",
    file_path = "district_and_planning_area.geojson",
    read_func = "st_read"
  ),
  
  # RDS files
  hdb_resale = list(
    var_name = "hdb_data",
    file_path = "hdb.rds",
    read_func = "readRDS"
  ),
  ura_private = list(
    var_name = "ura_private_data", 
    file_path = "ura_private.rds",
    read_func = "readRDS"
  ),
  childcares = list(
    var_name = "childcares_data",
    file_path = "childcares.RDS",
    read_func = "readRDS"
  ),
  schools = list(
    var_name = "schools_data",
    file_path = "schools.RDS",
    read_func = "readRDS"
  ),
  household_income = list(
    var_name = "household_income_data",
    file_path = "household_income_data.rds",
    read_func = "readRDS"
  )
)

# Streamlined data loading function
loadData <- function(dataname) {
  if (!dataname %in% names(data_registry)) {
    stop(paste("Unknown dataset:", dataname))
  }
  
  data_spec <- data_registry[[dataname]]
  var_name <- data_spec$var_name
  
  # Check if data already exists in the global environment
  if (!exists(var_name, envir = .GlobalEnv)) {
    file_path <- paste0(resources_path, data_spec$file_path)
    
    # Use the appropriate function to read the data
    if (data_spec$read_func == "st_read") {
      data <- sf::st_read(file_path, quiet = TRUE)
    } else if (data_spec$read_func == "readRDS") {
      data <- readRDS(file_path)
    }
    
    # Assign to global environment for caching
    assign(var_name, data, envir = .GlobalEnv)
    return(data)
  } else {
    # Return existing data
    return(get(var_name, envir = .GlobalEnv))
  }
}

# Define color palettes for the app
property_price_palette <- colorNumeric(palette = "viridis", domain = NULL, na.color = "transparent")
density_palette <- colorNumeric(palette = "YlOrRd", domain = NULL, na.color = "transparent")
accessibility_palette <- colorNumeric(palette = "Blues", domain = NULL, na.color = "transparent")

# Singapore default map center coordinates
sg_lat <- 1.3521
sg_lng <- 103.8198
sg_zoom <- 11

# Define property types
property_types <- c("HDB", "Condominium")

# Source all module files
module_files <- list.files("R/modules", full.names = TRUE, pattern = "*.R")
sapply(module_files, source)