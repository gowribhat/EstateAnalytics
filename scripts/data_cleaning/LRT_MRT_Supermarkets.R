library(jsonlite)
library(sf)
library(ggmap)
library(leaflet)
library(dplyr)
library(tidyr)

setwd("../R-4.4.3")
mrt <- fromJSON("LTAMRTStationExitGEOJSON.geojson")
mrt <- as.data.frame(mrt)
mrt$lon <- unlist(mrt$features.geometry[[2]])[c(T,F,F)]
mrt$lat <- unlist(mrt$features.geometry[[2]])[c(F,T,F)]

mrt$info <- gsub("<[^>]+>", "", mrt$features.properties[[2]])
mrt$info <- gsub("Attributes STATION_NA ","", mrt$info)
mrt$info <- gsub("EXIT_CODE ","", mrt$info)
mrt$info <- strsplit(mrt$info,"  ")
mrt$name <- unlist(mrt$info)[c(T,F,F,F)]
mrt$exit <- unlist(mrt$info)[c(F,T,F,F)]
mrt$exit <- gsub("Exit ","",mrt$exit)

mrt <- select(mrt,features.geometry,longitude=lon,latitude=lat,name,exit)
saveRDS(mrt, "LRT_MRT.rds")

mart <- fromJSON("SupermarketsGEOJSON.geojson")
mart <- as.data.frame(mart)
mart$lon <- unlist(mart$features.geometry[[2]])[c(T,F,F)]
mart$lat <- unlist(mart$features.geometry[[2]])[c(F,T,F)]

mart$info <- gsub("<[^>]+>", "", mart$features.properties[[2]])
mart$info <- gsub("Attributes LIC_NAME ","", mart$info)
mart$info <- gsub("BLK_HOUSE ","", mart$info)
mart$info <- gsub("STR_NAME ","", mart$info)
mart$info <- strsplit(mart$info,"  ")
mart$name <- unlist(mart$info)[c(T,F,F,F,F,F,F,F)]
mart$add <- unlist(mart$info)[c(F,T,F,F,F,F,F,F)]
mart$road <- unlist(mart$info)[c(F,F,T,F,F,F,F,F)]
mart <- unite(mart,address,add:road,sep=" ")

mart <- select(mart,features.geometry,longitude=lon,latitude=lat,name,address)
saveRDS(mart, "Supermarkets.rds")