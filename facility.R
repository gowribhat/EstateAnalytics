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

distances <- function(x, y) {
  lat_diff <- outer(x$latitude, y$latitude, FUN = "-")
  lon_diff <- outer(x$longitude, y$longitude, FUN = "-")
  d <- sqrt(lat_diff^2 + lon_diff^2)
  return(111.1 * d)  # Distance in kilometers
}
get_nearest <- function(i, hdb, childcare, num_nearest = 5) {
  # Calculate the distances for this HDB to all childcare centers
  dists <- sapply(1:nrow(childcare), function(j) {
    distances(hdb[i,], childcare[j,])
  })
  
  # Get the indices of the 5 nearest childcare centers
  nearest_indices <- order(dists)[1:num_nearest]
  
  # Return the nearest childcare center ids
  return(childcare[nearest_indices,])
}
nearest_childcare <- lapply(1:nrow(hdb), get_nearest, 
                            hdb = hdb, childcare = childcare)