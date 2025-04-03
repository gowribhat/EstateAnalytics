# Global settings, package loading, and data preprocessing
# Find Your New Home: Data-Driven Property Search Platform
# DBA3702 Team 3

# Ensure all required libraries are loaded
library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(sf)
library(viridis)
library(htmltools)
library(RColorBrewer)
library(scales)

# Define paths
resources_path <- "data/"

# Load data function - will only load data when needed to optimize performance
loadData <- function(dataname) {
  switch(dataname,
         "hdb_resale" = {
           if (!exists("hdb_resale_data")) {
             hdb_data <- read.csv(paste0(resources_path, "HDB_Resale_Coordinates_region.csv"), 
                                  stringsAsFactors = FALSE)
             return(hdb_data)
           }
         },
         "ura_private" = {
           if (!exists("ura_data")) {
             ura_data <- read.csv(paste0(resources_path, "URA_PrivateTransactions2025-04-02_region.csv"), 
                                  stringsAsFactors = FALSE)
             return(ura_data)
           }
         },
         "mrt_stations" = {
           if (!exists("mrt_stations_data")) {
             mrt_data <- st_read(paste0(resources_path, "LTAMRTStationExitGEOJSON.geojson"))
             return(mrt_data)
           }
         },
         "planning_areas" = {
           if (!exists("planning_areas_data")) {
             planning_areas <- st_read(paste0(resources_path, "district_and_planning_area.geojson"))
             return(planning_areas)
           }
         },
         "streets" = {
           if (!exists("streets_data")) {
             streets <- st_read(paste0(resources_path, "StreetandPlaces.geojson"))
             return(streets)
           }
         }
  )
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
property_types <- c("HDB", "Condominium", "Landed", "All")

# Source all module files
module_files <- list.files("R/modules", full.names = TRUE, pattern = "*.R")
sapply(module_files, source)