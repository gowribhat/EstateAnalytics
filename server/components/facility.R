# Building Details Logic (Right Overlay)
# This script manages the logic for displaying nearby facilities in the right overlay.
# It includes reactive datasets, UI rendering, and visualizations for selected buildings.
# Key components:
# - Nearby facilities: Displays detailed information about facilities near the selected building.
# - Visualizations: Generates plots and tables for building-specific data.

source("C:/Users/User/R-4.4.3/Project/temp.R")
resource_path <- here("data")

childcare <- readRDS(paste0(resource_path, "/childcares.rds"))
gym <- readRDS(paste0(resource_path, "/gyms_data.rds"))
mrt <- readRDS(paste0(resource_path, "/LRT_MRT.rds"))
park <- readRDS(paste0(resource_path, "/parks_data.rds"))
sch <- readRDS(paste0(resource_path, "/schools.rds"))
mart <- readRDS(paste0(resource_path, "/Supermarkets.rds"))

hdb <- readRDS(paste0(resource_path, "/hdb.rds"))
priv <- readRDS(paste0(resource_path, "/ura_private.rds"))

# Distance calculation utility
distances <- function(x, y) {
  distVincentySphere(c(x$longitude, x$latitude), c(y$longitude, y$latitude))
}

# Find nearest facilities
get_nearest <- function(a, b) {
  dists <- sapply(1:nrow(b), function(i) {
    distances(a, b[i, ])
  })
  nearest_indices <- order(dists)[1]
  clean <- b[nearest_indices, ]
  clean$distance <- dists[nearest_indices]
  return(clean)
}

# Reactive data for selected building and its facilities
facilities <- reactive({
  building <- selected_building()
  property_type <- selected_property_type()
  
  if (is.null(building)) {
    return(NULL)  # No building selected
  }
  # Filter building data based on property type
  if (property_type == "HDB") {
    data <- filtered_hdb_data()
    req(data)
    building_data <- data %>%
      filter(
        block == building$block,
        street_name == building$street_name
      )
  } else {
    data <- filtered_ura_data()
    req(data)
    building_data <- data %>%
      filter(
        project == building$project,
        street == building$street
      )
  }
  if ("Childcare" %in% selected) {
    building_data$childcare <- get_nearest(building, childcare)$distance
  }
  if ("Gym" %in% selected) {
    building_data$gym <- get_nearest(building, gym)$distance
  }
  if ("MRT" %in% selected) {
    building_data$mrt <- get_nearest(building, mrt)$distance
  }
  if ("Park" %in% selected) {
    building_data$park <- get_nearest(building, park)$distance
  }
  if ("School" %in% selected) {
    building_data$sch <- get_nearest(building, sch)$distance
  }
  if ("Supermarket" %in% selected) {
    building_data$mart <- get_nearest(building, mart)$distance
  }
  return(building_data)
})

# Calculate dynamic weights based on user-selected facilities
calculate_weights <- function(selected_facilities) {
  n <- length(selected_facilities)
  if (n == 0) return(NULL)
  
  total_weight <- 0.5 * n * (n + 1)  # Total weight sum
  weights <- rev(seq_len(n)) / total_weight  # Descending weights
  return(data.frame(
    facility = selected_facilities,
    weight = weights
  ))
}

# Reactive ranking and weight calculation based on user selection
facility_ranking <- reactive({
  req(input$selected_facilities)  # User-selected facilities
  selected_facilities <- input$selected_facilities
  
  # Calculate weights dynamically
  calculate_weights(selected_facilities)
})

# Calculate total score based on selected facilities and weights
# Generate nearest facility data based on user selection
