library(dotenv)
library(dplyr)
library(stringr)
library(lubridate)
library(ggmap)

dotenv::load_dot_env()
ggmap::register_google(key = Sys.getenv("GOOGLE_API_KEY")) ## Create a .env file in the root directory and add your Google API key as GOOGLE_API_KEY=your_key_here

if (!dir.exists("./data/clean")) {
  dir.create("./data/clean", recursive = TRUE, showWarnings = FALSE)
  cat("Created missing directory: ./data/cleaned\n")
}

##### Helper Functions

get_lat_long <- function(postal_code) {
  if (is.na(postal_code) | postal_code == "") {
    return(c(NA, NA))
  }
  
  result <- tryCatch(
    geocode(paste(postal_code, "Singapore"), source = "google"),
    error = function(e) return(data.frame(lon = NA, lat = NA))
  )
  
  return(c(result$lon, result$lat))
}

clean_postal_codes <- function(df, column) {
  df %>%
    mutate(!!column := case_when(
      str_detect(!!sym(column), "^[0-9]+$") & nchar(!!sym(column)) < 6 ~ str_pad(!!sym(column), 6, pad = "0"),
      nchar(!!sym(column)) == 6 & str_detect(!!sym(column), "^[0-9]+$") ~ !!sym(column),
      TRUE ~ NA_character_
    ))
}

clean_character_cols <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ str_trim(.))) %>%
    mutate(across(where(is.character), ~ if_else(. %in% c("", "na", "NA", "Na"), NA_character_, .)))
}

#### Cleaner Functions

clean_schools <- function() {
  schools_data <- read.csv("./data/raw/Generalinformationofschools.csv", stringsAsFactors = FALSE)
  
  # Remove unnecessary columns
  schools_data <- schools_data %>%
    select(school_name, address, postal_code) # Retain only the required columns
  
  # Clean char values
  schools_data <- schools_data %>%
    clean_character_cols() %>%
    mutate(postal_code = as.character(postal_code)) %>%
    clean_postal_codes("postal_code")
  
  # Get Latitude & Longitude
  coords <- t(sapply(schools_data$postal_code, get_lat_long))
  schools_data$longitude <- coords[, 1]
  schools_data$latitude <- coords[, 2]
  
  # Rename school_name to name
  schools_data <- schools_data %>%
    rename(name = school_name) %>%
    select(name, address, postal_code, latitude, longitude)
  
  saveRDS(schools_data, "./data/clean/schools.RDS")
  schools_data
}

clean_childcares <- function() {
  childcares_data <- read.csv("./data/raw/ListingofCentres.csv", stringsAsFactors = FALSE)
  
  # Remove unnecessary columns that do not add value
  childcares_data <- childcares_data %>%
    select(-c(remarks, government_subsidy, extended_operating_hours))
  
  # Clean Character columns
  childcares_data <- childcares_data %>%
    clean_character_cols() %>%
    mutate(postal_code = as.character(postal_code)) %>%
    clean_postal_codes("postal_code") %>%
    filter(!is.na(centre_name))
  
  # Get Latitude & Longitude
  coords <- t(sapply(childcares_data$postal_code, get_lat_long))
  childcares_data$longitude <- coords[, 1]
  childcares_data$latitude <- coords[, 2]
  
  # Rename columns
  childcares_data <- childcares_data %>%
    rename(name = centre_name, address = centre_address) %>%
    select(name, address, postal_code, latitude, longitude)
  
  saveRDS(childcares_data, "./data/clean/childcares.RDS")
  childcares_data
}


clean_schools()
clean_childcares()