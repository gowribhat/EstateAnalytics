library(dotenv)
library(dplyr)
library(stringr)
library(lubridate)
library(ggmap)

dotenv::load_dot_env()
ggmap::register_google(key = Sys.getenv("GOOGLE_API_KEY"))

if (!dir.exists("./data/cleaned")) {
  dir.create("./data/cleaned", recursive = TRUE, showWarnings = FALSE)
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

clean_phone_numbers <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ gsub("^\\+65|^\\(65\\)", "", .)))
}

convert_to_logical <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ ifelse(. == "Yes", TRUE, FALSE)))
}

#### Cleaner Functions

clean_schools <- function() {
  schools_data <- read.csv("./data/raw/Generalinformationofschools.csv", stringsAsFactors = FALSE)
  
  # Remove unnecessary columns
  schools_data <- schools_data %>%
    select(-c(principal_name, first_vp_name, second_vp_name, third_vp_name, 
              fourth_vp_name, fifth_vp_name, sixth_vp_name, fax_no, fax_no_2))
  
  # Clean char values
  schools_data <- schools_data %>%
    clean_character_cols() %>%
    clean_phone_numbers(c("telephone_no", "telephone_no_2")) %>%
    mutate(postal_code = as.character(postal_code)) %>%
    clean_postal_codes("postal_code")
  
  # Convert relevant columns to factors and logical
  factor_columns <- c("dgp_code", "zone_code", "type_code", "nature_code", "session_code", "mainlevel_code")
  schools_data <- schools_data %>%
    mutate(across(all_of(factor_columns), as.factor)) %>%
    rename(mt_chinese = mothertongue1_code, mt_malay = mothertongue2_code, mt_tamil = mothertongue3_code) %>%
    mutate(across(c("mt_chinese", "mt_malay", "mt_tamil"), ~ . %in% c("Chinese", "Malay", "Tamil"))) %>%
    convert_to_logical(c("sap_ind", "autonomous_ind", "gifted_ind", "ip_ind"))
  
  # Get Latitude & Longitude
  coords <- t(sapply(schools_data$postal_code, get_lat_long))
  schools_data$longitude <- coords[, 1]
  schools_data$latitude <- coords[, 2]
  
  saveRDS(schools_data, "./data/cleaned/cleaned_schools.RDS")
  
  return(schools_data)
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
    clean_phone_numbers("centre_contact_no") %>%
    mutate(centre_contact_no = if_else(centre_contact_no == "0", "Unknown", centre_contact_no)) %>%
    mutate(centre_email_address = if_else(
      !str_detect(centre_email_address, "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"),
      "Unknown", 
      centre_email_address
    )) %>%
    filter(!is.na(centre_name))
  
  # Extract second language data for Chinese, Malay, and Tamil
  childcares_data <- childcares_data %>%
    mutate(
      second_language_chinese = str_detect(second_languages_offered, "(?i)Chinese"),
      second_language_malay = str_detect(second_languages_offered, "(?i)Malay"),
      second_language_tamil = str_detect(second_languages_offered, "(?i)Tamil")
    )
  
  # Clean erroneous values in Saturday column
  childcares_data <- childcares_data %>%
    mutate(saturday = if_else(saturday %in% c("00:00-00:00", "00:00-00:01"), NA_character_, saturday))
  
  # Convert relevant columns to factors
  factor_columns <- c("tp_code", "organisation_code", "organisation_description", 
                      "service_model", "scheme_type", "food_offered", 
                      "second_languages_offered", "weekday_full_day", "saturday")
  
  childcares_data <- childcares_data %>%
    mutate(food_offered = if_else(is.na(food_offered), "Unknown", as.character(food_offered))) %>%
    mutate(across(all_of(factor_columns), as.factor))
  
  # Set vacancy columns as ordered factors
  vacancy_columns <- grep("vacancy", names(childcares_data), value = TRUE)
  childcares_data <- childcares_data %>%
    mutate(across(all_of(vacancy_columns), ~ factor(., levels = c("Not Applicable", "Limited", "Available", "Full"), ordered = TRUE)))
  
  # Set logicals and date
  childcares_data <- childcares_data %>%
    rename(gst_registration = gst_regisration) %>%
    convert_to_logical(c("spark_certified", "provision_of_transport", "gst_registration")) %>%
    mutate(last_updated = dmy(last_updated))
  
  # Get Latitude & Longitude
  coords <- t(sapply(childcares_data$postal_code, get_lat_long))
  childcares_data$longitude <- coords[, 1]
  childcares_data$latitude <- coords[, 2]
  
  saveRDS(childcares_data, "./data/cleaned/cleaned_childcares.RDS")
  
  return(childcares_data)
}

clean_datasets <- function() {
  clean_schools()
  clean_childcares()
}

clean_datasets()
