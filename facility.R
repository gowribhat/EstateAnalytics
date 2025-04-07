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
nearest_childcare <- get_nearest(hdb[2025, ], childcare)
nearest_childcare <- nearest_childcare[,c("centre_address","centre_name","distance")]

nearest_gym <- get_nearest(hdb[2025, ], gym)
nearest_mrt <- get_nearest(hdb[2025, ], mrt)
nearest_park <- get_nearest(hdb[2025,],park)
nearest_sch <- get_nearest(hdb[2025,],sch)
nearest_sch <- nearest_sch[,c("address","school_name","distance")]
nearest_mart <- get_nearest(hdb[2025,],mart)
nearest_mart <- nearest_mart[,c("address","name","distance")]

weight <- c(0.15,0.1,0.15,0.25,0.15,0.2)
score <- 400/c(nearest_childcare$distance, nearest_gym$distance, nearest_mrt$distance,
           nearest_park$distance,nearest_sch$distance,nearest_mart$distance)
sum(weight*score)