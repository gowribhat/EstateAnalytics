resource_path <- "data/"
childcare <- readRDS(paste0(resource_path,"childcares.rds"))
gym <- readRDS(paste0(resource_path,"gyms_data.rds"))
mrt <- readRDS(paste0(resource_path,"LRT_MRT.rds"))
park <- readRDS(paste0(resource_path,"parks_data.rds"))
sch <- readRDS(paste0(resource_path,"schools.rds"))
mrt <- readRDS(paste0(resource_path,"LRT_MRT.rds"))

hdb <- readRDS(paste0(resource_path,"hdb.rds"))
priv <- readRDS(paste0(resource_path,"ura_private.rds"))

distance <- function(x,y){
  d <- sqrt((x$longitude-y$longitude)^2+(x$latitude-y$latitude)^2)
  return(111.1*d)
}
