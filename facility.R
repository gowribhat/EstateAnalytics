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

distances <- function(x,y) {
  distVincentySphere(c(x$longitude, x$latitude), c(y$longitude, y$latitude))
}
get_nearest <- function(a, b, n = 5) {
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
nearest_childcare <- get_nearest(hdb[2025, ], childcare)
nearest_childcare <- nearest_childcare[,c("centre_address","centre_name","distance")]

nearest_gym <- get_nearest(hdb[2025, ], gym)
nearest_mrt <- get_nearest(hdb[2025, ], mrt)