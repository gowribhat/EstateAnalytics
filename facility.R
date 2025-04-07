library(geosphere)

resource_path <- "data/"
childcare <- readRDS(paste0(resource_path,"childcares.rds"))
gym <- readRDS(paste0(resource_path,"gyms_data.rds"))
mrt <- readRDS(paste0(resource_path,"LRT_MRT.rds"))
park <- readRDS(paste0(resource_path,"parks_data.rds"))
sch <- readRDS(paste0(resource_path,"schools.rds"))
mart <- readRDS(paste0(resource_path,"Supermarkets.rds"))

hdb <- readRDS(paste0(resource_path,"hdb.rds"))
priv <- readRDS(paste0(resource_path,"ura_private.rds"))

distances <- apply(as.numeric(hdb[, c("latitude", "longitude")]), 1, function(housing_point) {
  apply(as.numeric(childcare[, c("latitude", "longitude")]), 1, function(childcare_point) {
    # Calculate the distance using distVincentySphere
    distVincentySphere(housing_point, childcare_point)
  })
})

distances <- function(x,y) {
  distVincentySphere(c(x$longitude, x$latitude), c(y$longitude, y$latitude))
}
get_nearest <- function(hdb, childcare, num_nearest = 5) {
  # Calculate distances from the HDB location to all childcare centers
  dists <- sapply(1:nrow(childcare), function(i) {
    distances(hdb, childcare[i, ])
  })
  
  # Get the indices of the 5 nearest childcare centers
  nearest_indices <- order(dists)[1:num_nearest]
  
  # Return the rows of the nearest childcare centers
  return(childcare[nearest_indices, ])
}
nearest_childcare <- lapply(1:nrow(hdb), function(i) {
  get_nearest(hdb[i, ], childcare)
})