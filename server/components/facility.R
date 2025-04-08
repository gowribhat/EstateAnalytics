# Building Details Logic (Right Overlay)
# This script manages the logic for displaying nearby facilities in the right overlay.
# It includes reactive datasets, UI rendering, and visualizations for selected buildings.
# Key components:
# - Nearby facilities: Displays detailed information about facilities near the selected building.
# - Visualizations: Generates plots and tables for building-specific data.

library(geosphere)

resource_path <- "../../data/"
childcare <- readRDS(paste0(resource_path,"childcares.rds"))
gym <- readRDS(paste0(resource_path,"gyms_data.rds"))
mrt <- readRDS(paste0(resource_path,"LRT_MRT.rds"))
park <- readRDS(paste0(resource_path,"parks_data.rds"))
sch <- readRDS(paste0(resource_path,"schools.rds"))
mart <- readRDS(paste0(resource_path,"Supermarkets.rds"))

hdb <- readRDS(paste0(resource_path,"hdb.rds"))
priv <- readRDS(paste0(resource_path,"ura_private.rds"))

facilities <- reactive({
  building <- selected_building()
  property_type <- selected_property_type()
  
  # If no building is selected, return NULL
  if (is.null(building)) {
    return(NULL)
  }
  
  # Get appropriate dataset
  if (property_type == "HDB") {
    data <- filtered_hdb_data() # Assumes filtered_hdb_data is defined elsewhere
    req(data)
    
    # Extract block and street name from selected building
    # Filter data for the specific building
    building_data <- data %>%
      filter(
        block == building$block,
        street_name == building$street_name
      ) 
    
  } else {
    data <- filtered_ura_data() # Assumes filtered_ura_data is defined elsewhere
    req(data)
    
    # Filter data for the specific building/project
    building_data <- data %>%
      filter(
        project == building$project,
        street == building$street
      ) 
  }
  
  return(building_data)
})

distances <- function(x,y) {
  distVincentySphere(c(x$longitude, x$latitude), c(y$longitude, y$latitude))
}
get_nearest <- function(a, b, n = 1) {
  # Calculate distances from the HDB location to all childcare centers
  dists <- sapply(1:nrow(b), function(i) {
    distances(a, b[i, ])
  })
  
  # Get the indices of the 5 nearest childcare centers
  nearest_indices <- order(dists)[1:n]
  clean <- b[nearest_indices, ]
  clean$distance <- dists[nearest_indices]
  # Return the rows of the nearest childcare centers
  return(clean)
}

output$property_details <- renderUI({
  building <- selected_building()
  building_data <- building_transactions()
  
  # If no building is selected or no data is available, show a prompt message (without the button)
  if (is.null(building) || is.null(building_data) || nrow(building_data) == 0) {
    return(HTML("<p>Click on a property marker to see details.</p>"))
  }
  
  nearest_childcare <- get_nearest(building_data, childcare)
  nearest_gym <- get_nearest(building_data, gym)
  nearest_mrt <- get_nearest(building_data, mrt)
  nearest_park <- get_nearest(building_data,park)
  nearest_sch <- get_nearest(building_data,sch)
  nearest_mart <- get_nearest(building_data,mart)
  weight <- c(15,10,25,15,20,15)
  norm <- function(x){max(100,min(1600,x))}
  norm_dist <- sapply(c(nearest_childcare$distance, nearest_gym$distance, nearest_mrt$distance,
                        nearest_park$distance,nearest_sch$distance,nearest_mart$distance),norm)
  score <- (1600-norm_dist)/1500
    
  # Include the Past Transactions button only when a building is selected
  HTML(paste0(
    "<div style='font-size: 18px; font-weight: bold;'>", building$block, " ", building$street_name, "</div>",
    "<div style='margin-top: 10px;'>",
    "<div><strong>Nearest Childcare Centre:</strong>", nearest_childcare$distance,  " m</div>",
    "<div><strong>Nearest Gym:</strong>", nearest_gym$distance, " m</div>",
    "<div><strong>Nearest LRT/MRT station:</strong> ", nearest_mrt$distance, " m</div>",
    "<div><strong>Nearest Park:</strong> ", nearest_park$distance, " m</div>",
    "<div><strong>Nearest School:</strong> ", nearest_sch$distance, " m</div>",
    "<div><strong>Nearest Supermarket:</strong> ", nearest_mart$distance, " m</div>",
    "<div><strong>Total score based on proximity of facilities:</strong> ", sum(weight*score), " %</div>"
    "<div style='margin-top: 15px;'>",
    # Ensure the button ID matches the observer in overlay_logic.R
    "<button id='toggle_transactions_overlay' type='button' class='btn btn-primary btn-block action-button'>Past Transactions</button>",
    "</div>",
    "</div>"
    ))
  }
})