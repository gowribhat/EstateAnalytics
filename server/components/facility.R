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
      ) %>%
      arrange(desc(month))
    
  } else {
    data <- filtered_ura_data() # Assumes filtered_ura_data is defined elsewhere
    req(data)
    
    # Filter data for the specific building/project
    building_data <- data %>%
      filter(
        project == building$project,
        street == building$street
      ) %>%
      arrange(desc(contractDate)
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

# For HDB
nearest_childcare <- get_nearest(hdb[5639, ], childcare)
nearest_childcare <- nearest_childcare[,c("centre_address","centre_name","distance")]

nearest_gym <- get_nearest(hdb[5639, ], gym)
nearest_gym <- nearest_gym[,c("Name","distance")]
nearest_mrt <- get_nearest(hdb[5639, ], mrt)
nearest_park <- get_nearest(hdb[5639,],park)
nearest_park <- nearest_park[,c("NAME","distance")]
nearest_sch <- get_nearest(hdb[5639,],sch)
nearest_sch <- nearest_sch[,c("address","school_name","distance")]
nearest_mart <- get_nearest(hdb[5639,],mart)
nearest_mart <- nearest_mart[,c("address","name","distance")]

weight <- c(15,10,25,15,20,15)
norm <- function(x){max(100,min(1600,x))}
norm_dist <- sapply(c(nearest_childcare$distance, nearest_gym$distance, nearest_mrt$distance,
               nearest_park$distance,nearest_sch$distance,nearest_mart$distance),norm)
score <- (1600-norm_dist)/1500
sum(weight*score)
hdb[5639,]

# For private
Nearest_childcare <- get_nearest(priv[87299,], childcare)
Nearest_childcare <- Nearest_childcare[,c("centre_address","centre_name","distance")]

Nearest_gym <- get_nearest(priv[87299,], gym)
Nearest_gym <- Nearest_gym[,c("Name","distance")]
Nearest_mrt <- get_nearest(priv[87299,], mrt)
Nearest_park <- get_nearest(priv[87299,],park)
Nearest_park <- Nearest_park[,c("NAME","distance")]
Nearest_sch <- get_nearest(priv[87299,],sch)
Nearest_sch <- Nearest_sch[,c("address","school_name","distance")]
Nearest_mart <- get_nearest(priv[87299,],mart)
Nearest_mart <- Nearest_mart[,c("address","name","distance")]

norm_dist <- sapply(c(Nearest_childcare$distance, Nearest_gym$distance, Nearest_mrt$distance,
                      Nearest_park$distance,Nearest_sch$distance,Nearest_mart$distance),norm)
score <- (1600-norm_dist)/1500
sum(weight*score)