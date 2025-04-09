# Building Details Logic (Right Overlay)
# This script manages the logic for displaying nearby facilities in the right overlay.
# It includes reactive datasets, UI rendering, and visualizations for selected buildings.
# Key components:
# - Nearby facilities: Displays detailed information about facilities near the selected building.
# - Visualizations: Generates plots and tables for building-specific data.

resource_path <- "../../data/"
childcare <- readRDS(paste0(resource_path,"childcares.rds"))
gym <- readRDS(paste0(resource_path,"gyms_data.rds"))
mrt <- readRDS(paste0(resource_path,"LRT_MRT.rds"))
park <- readRDS(paste0(resource_path,"parks_data.rds"))
sch <- readRDS(paste0(resource_path,"schools.rds"))
mart <- readRDS(paste0(resource_path,"Supermarkets.rds"))

hdb <- readRDS(paste0(resource_path,"hdb.rds"))
priv <- readRDS(paste0(resource_path,"ura_private.rds"))

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
data <- c(building_data,norm_dist,score)