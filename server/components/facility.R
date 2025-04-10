# Building Details Logic (Right Overlay)
# This script manages the logic for displaying nearby facilities in the right overlay.
# It includes reactive datasets, UI rendering, and visualizations for selected buildings.
# Key components:
# - Nearby facilities: Displays detailed information about facilities near the selected building.
# - Visualizations: Generates plots and tables for building-specific data.

resource_path <- here("data")

childcare <- readRDS(paste0(resource_path, "/childcares.rds"))
gym <- readRDS(paste0(resource_path, "/gyms_data.rds"))
mrt <- readRDS(paste0(resource_path, "/LRT_MRT.rds"))
park <- readRDS(paste0(resource_path, "/parks_data.rds"))
sch <- readRDS(paste0(resource_path, "/schools.rds"))
mart <- readRDS(paste0(resource_path, "/Supermarkets.rds"))

hdb <- readRDS(paste0(resource_path, "/hdb.rds"))
priv <- readRDS(paste0(resource_path, "/ura_private.rds"))

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
  
  return(building_data)
})

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
score <- reactive({
  building_data <- facilities()
  facility_data <- nearby_facilities()  # Nearest facility data
  ranking <- facility_ranking()  # User ranking and weights
  
  if (is.null(building_data) || is.null(facility_data) || is.null(ranking)) {
    return(0)
  }
  
  # Normalize distances and calculate weighted score
  norm <- function(x) { max(100, min(1600, x)) }
  total_score <- 0
  
  for (i in seq_len(nrow(ranking))) {
    facility <- ranking$facility[i]
    weight <- ranking$weight[i]
    
    if (!is.null(facility_data[[tolower(facility)]])) {
      distance <- facility_data[[tolower(facility)]]$distance
      norm_dist <- norm(distance)
      total_score <- total_score + (1600 - norm_dist) / 1500 * weight
    }
  }
  
  return(total_score)
})

# Generate nearest facility data based on user selection
server <- function(input, output,session){
  nearby_facilities <- reactive({
    selected <- input$selected_facilities
    building <- facilities()
    
    if (is.null(building)) return(NULL)
    
    facility_data <- list()
    if ("Childcare" %in% selected) {
      facility_data$childcare <- get_nearest(building, childcare)
    }
    if ("Gym" %in% selected) {
      facility_data$gym <- get_nearest(building, gym)
    }
    if ("MRT" %in% selected) {
      facility_data$mrt <- get_nearest(building, mrt)
    }
    if ("Park" %in% selected) {
      facility_data$park <- get_nearest(building, park)
    }
    if ("School" %in% selected) {
      facility_data$sch <- get_nearest(building, sch)
    }
    if ("Supermarket" %in% selected) {
      facility_data$mart <- get_nearest(building, mart)
    }
    return(facility_data)
  })
}