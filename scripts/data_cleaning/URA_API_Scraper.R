# Load required libraries
library(httr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(dplyr)
library(future)
library(future.apply)
library(readr)
library(sp) # Added for coordinate conversion

message("Starting URA data scraping and coordinate update process...")

# --- URA API Scraping ---
accesskey <- Sys.getenv("URA_ACCESSKEY") # Consider using environment variables for sensitive keys
output_filepath <- paste0("Resources/URA_PrivateTransactions", Sys.Date(), ".csv") # Define final output path with today's date

# ... existing URA token retrieval code ...
res = GET("https://eservice.ura.gov.sg/uraDataService/insertNewToken/v1", add_headers('AccessKey' = accesskey))
token = fromJSON(rawToChar(res$content))
token <- token$Result

# Initialize an empty data frame
merged_URAJSON <- data.frame()

message("Fetching URA transaction data...")
for (x in 1:4) {
  # ... existing URA data fetching loop ...
  url <- "https://eservice.ura.gov.sg/uraDataService/invokeUraDS/v1?service=PMI_Resi_Transaction&batch="
  URAres <- GET(paste0(url, x), add_headers('AccessKey' = accesskey, 'Token' = token))
  URAdata <- fromJSON(iconv(rawToChar(URAres$content), from = "ISO-8859-1", to = "UTF-8", sub = "byte"))
  # Check if Result is not NULL and has data
  if (!is.null(URAdata$Result) && length(URAdata$Result) > 0) {
    URAdata <- URAdata$Result
    merged_URAJSON <- rbind(merged_URAJSON, URAdata)
    message(paste("Batch", x, "fetched successfully."))
  } else {
    # Optionally print a message or handle the case where Result is empty/NULL
    message(paste("Batch", x, "returned no results or an error."))
  }
}
message("Finished fetching URA data.")

# --- URA Data Processing ---
message("Processing fetched URA data...")
# Flatten the nested transaction data
if (nrow(merged_URAJSON) > 0 && "transaction" %in% names(merged_URAJSON)) {
  merged_URAJSON <- tidyr::unnest(merged_URAJSON, cols = c(transaction))

  # Convert columns to appropriate data types
  # ... existing type conversion code ...
  # Ensure x and y are treated correctly even if initially character "NA"
  merged_URAJSON$x <- suppressWarnings(as.numeric(merged_URAJSON$x))
  merged_URAJSON$y <- suppressWarnings(as.numeric(merged_URAJSON$y))
  merged_URAJSON$area <- as.numeric(merged_URAJSON$area)
  merged_URAJSON$price <- as.numeric(merged_URAJSON$price)
  if ("nettPrice" %in% names(merged_URAJSON)) {
    merged_URAJSON$nettPrice <- suppressWarnings(as.numeric(merged_URAJSON$nettPrice))
  }
  merged_URAJSON$district <- suppressWarnings(as.integer(merged_URAJSON$district))
  merged_URAJSON$noOfUnits <- suppressWarnings(as.integer(merged_URAJSON$noOfUnits))

  # Date conversion
  merged_URAJSON$contractDate <- as.Date(sapply(merged_URAJSON$contractDate, function(date_str) {
    if (is.na(date_str) || date_str == "") return(NA)
    month <- as.integer(substr(date_str, 1, 2))
    year <- as.integer(substr(date_str, 3, 4))
    full_year <- ifelse(year < 50, 2000 + year, 1900 + year) # Assuming 20xx for years less than 50
    tryCatch({
      paste0(full_year, "-", sprintf("%02d", month), "-01")
    }, error = function(e) NA) # Handle potential errors in date construction
  }), format="%Y-%m-%d")

  # Factor conversions
  # ... existing factor conversion code ...
  merged_URAJSON$marketSegment <- factor(merged_URAJSON$marketSegment,
                                        levels = c("CCR", "RCR", "OCR"))

  merged_URAJSON$propertyType <- factor(merged_URAJSON$propertyType,
                                       levels = c("Strata Detached", "Strata Semidetached",
                                                 "Strata Terrace", "Detached",
                                                 "Semi-detached", "Terrace",
                                                 "Apartment", "Condominium",
                                                 "Executive Condominium"))

  merged_URAJSON$typeOfArea <- factor(merged_URAJSON$typeOfArea,
                                     levels = c("Strata", "Land", "Unknown"))

  merged_URAJSON$typeOfSale <- factor(merged_URAJSON$typeOfSale,
                                     levels = c(1, 2, 3),
                                     labels = c("New Sale", "Sub Sale", "Resale"))

  # Reorder columns
  # ... existing column reordering code ...
  column_order <- c(
    "project",
    "street",
    "marketSegment",
    "x",
    "y",
    "latitude", # Added
    "longitude", # Added
    "contractDate",
    "typeOfSale",
    "area",
    "price",
    "nettPrice",
    "propertyType",
    "typeOfArea",
    "tenure",
    "floorRange",
    "district",
    "noOfUnits"
  )

  # Check which columns exist and keep only those in the correct order
  available_columns <- column_order[column_order %in% names(merged_URAJSON)]
  merged_URAJSON <- merged_URAJSON[, available_columns]

  message("Finished processing URA data.")
  message(paste("Initial data has", nrow(merged_URAJSON), "rows."))

} else {
  stop("URA data fetching failed or returned empty results. Cannot proceed.")
}

# --- Coordinate Update Section ---
message("Starting coordinate update process...")

# Configure parallel processing
plan(multisession) # Use multisession backend for parallel processing

# Function to retrieve OneMap API token dynamically using R
get_auth_token <- function() {
    # ... existing get_auth_token function ...
    url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
    payload <- list(
        # Retrieve from environment variables
        email = Sys.getenv("ONEMAP_EMAIL"),
        password = Sys.getenv("ONEMAP_EMAIL_PASSWORD")
    )
    response <- httr::POST(url, body = payload, encode = "json")
    if (httr::status_code(response) != 200) {
        stop("Failed to retrieve auth token. Check your ONEMAP_EMAIL and ONEMAP_PASSWORD environment variables.")
    }
    content <- httr::content(response, as = "parsed", type = "application/json")
    return(content$access_token)
}

# Create a function to get SVY21 coordinates for a project and street
get_svy21_coordinates <- function(project, street, auth_token) { # Pass auth_token explicitly
    # ... existing get_svy21_coordinates function ...
    # For URA data, we'll use the project and street name together as our search term
    search_val <- paste(project, street, sep = " ")

    # First get the address from OneMap
    api_url <- paste0("https://www.onemap.gov.sg/api/common/elastic/search?searchVal=",
                      URLencode(search_val),
                      "&returnGeom=Y&getAddrDetails=Y&pageNum=1")

    response <- tryCatch({
        httr::GET(api_url, add_headers(Authorization = paste("Bearer", auth_token))) # Use passed token
    }, error = function(e) {
        message(paste("Error fetching coordinates for:", search_val, "-", e$message))
        return(NULL)
    })

    if (is.null(response) || httr::http_status(response)$category != "Success") {
        message(paste("Failed API request for:", search_val, "(Status:", httr::status_code(response), ")"))
        return(list(x = NA, y = NA))
    }

    resp_content <- httr::content(response, "text", encoding = "UTF-8")
    tryCatch({
        content <- jsonlite::fromJSON(resp_content)
        if (is.null(content$results) || length(content$results) == 0) {
            # message(paste("No results found for:", search_val)) # Reduce verbosity
            return(list(x = NA, y = NA))
        }

        result <- content$results[1,]
        x <- result$X
        y <- result$Y

        # OneMap directly provides SVY21 coordinates as X and Y
        if (!is.null(x) && !is.null(y) && x != "" && y != "" && x != "NIL" && y != "NIL" && !is.na(suppressWarnings(as.numeric(x))) && !is.na(suppressWarnings(as.numeric(y)))) {
             # Check if x and y are non-empty, not "NIL", and potentially numeric
             x_num <- suppressWarnings(as.numeric(x))
             y_num <- suppressWarnings(as.numeric(y))
             if (!is.na(x_num) && !is.na(y_num) && x_num != 0 && y_num != 0) {
                return(list(x = x_num, y = y_num))
             } else {
                # message(paste("Non-numeric or zero coordinates returned for:", search_val)) # Reduce verbosity
                return(list(x = NA, y = NA))
             }
        } else {
            # message(paste("Invalid or missing coordinates returned for:", search_val)) # Reduce verbosity
            return(list(x = NA, y = NA))
        }
    }, error = function(e) {
        message(paste("Error processing results for:", search_val, "-", e$message))
        return(list(x = NA, y = NA))
    })
}

# Retrieve the auth token once at the beginning
message("Retrieving OneMap API token...")
onemap_auth_token <- tryCatch({ # Use a different variable name to avoid conflict
    get_auth_token()
}, error = function(e) {
    message("Error retrieving auth token. Make sure you've set the ONEMAP_EMAIL and ONEMAP_PASSWORD environment variables.")
    stop(e)
})
message("OneMap API token retrieved successfully!")

# Identify rows with missing coordinates (NA or 0, as 0 is invalid for SVY21)
# Ensure x and y are numeric first
merged_URAJSON <- merged_URAJSON %>%
    mutate(
        x = suppressWarnings(as.numeric(x)),
        y = suppressWarnings(as.numeric(y))
    )

missing_coords_rows_indices <- which(is.na(merged_URAJSON$x) | is.na(merged_URAJSON$y) | merged_URAJSON$x == 0 | merged_URAJSON$y == 0)
original_missing_count <- length(missing_coords_rows_indices)

message(paste("Found", original_missing_count, "rows with missing or invalid (0) coordinates."))

if (original_missing_count > 0) {
    missing_coords_data <- merged_URAJSON[missing_coords_rows_indices, ]

    # Get unique project/street combinations from rows with missing coordinates
    unique_combinations <- missing_coords_data %>%
        filter(!is.na(project) & project != "", !is.na(street) & street != "") %>% # Exclude rows where project/street is missing
        distinct(project, street)

    message(paste("Processing", nrow(unique_combinations), "unique project/street combinations for coordinate lookup..."))

    # Fetch coordinates for unique combinations using future_lapply for parallelism
    results_list <- future_lapply(1:nrow(unique_combinations), function(i) {
        combo <- unique_combinations[i, ]
        # Pass the retrieved token to the function
        coords <- get_svy21_coordinates(combo$project, combo$street, onemap_auth_token)
        # Return the combination along with the coordinates
        return(list(project = combo$project, street = combo$street, x_new = coords$x, y_new = coords$y))
    }, future.seed = TRUE) # Add future.seed for reproducibility

    # Combine results into a dataframe (lookup table)
    coords_lookup <- bind_rows(results_list) %>%
        filter(!is.na(x_new) & !is.na(y_new)) # Keep only successful lookups

    message(paste("Fetched coordinates for", nrow(coords_lookup), "unique combinations."))

    # Join the fetched coordinates back to the main dataframe
    # Use a temporary dataframe for joining to avoid modifying the original during the join
    merged_URAJSON_temp <- merged_URAJSON %>%
        left_join(coords_lookup, by = c("project", "street")) %>%
        mutate(
            # Use coalesce to fill NA/0 values in original x/y with new values if available
            x = ifelse((is.na(x) | x == 0) & !is.na(x_new), x_new, x),
            y = ifelse((is.na(y) | y == 0) & !is.na(y_new), y_new, y)
        ) %>%
        select(-x_new, -y_new) # Remove temporary columns

    # Overwrite the original dataframe
    merged_URAJSON <- merged_URAJSON_temp
    rm(merged_URAJSON_temp) # Clean up temporary dataframe

    message("Merged fetched coordinates back into the main dataset.")

} else {
    message("No missing coordinates found to update.")
}

# --- Coordinate Conversion Section ---
message("Converting SVY21 coordinates to WGS84 (Latitude/Longitude)...")

# Function to convert SVY21 to WGS84
convert_svy21_to_wgs84 <- function(x_coords, y_coords) {
  # Ensure input vectors are numeric and have the same length
  if (!is.numeric(x_coords) || !is.numeric(y_coords) || length(x_coords) != length(y_coords)) {
    stop("Input x and y coordinates must be numeric vectors of the same length.")
  }

  # Create a dataframe of valid coordinates to convert
  valid_indices <- !is.na(x_coords) & !is.na(y_coords) & x_coords != 0 & y_coords != 0
  coords_to_convert <- data.frame(x = x_coords[valid_indices], y = y_coords[valid_indices])

  # Initialize result dataframe with NAs
  result_coords <- data.frame(latitude = rep(NA_real_, length(x_coords)),
                              longitude = rep(NA_real_, length(x_coords)))

  if (nrow(coords_to_convert) > 0) {
    # Define SVY21 CRS
    svy21_crs <- CRS("+proj=tmerc +lat_0=1.366666 +lon_0=103.833333 +k=1.0 +x_0=28001.642 +y_0=38744.572 +datum=WGS84 +units=m +no_defs")
    # Define WGS84 CRS
    wgs84_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

    # Create SpatialPoints object
    points <- SpatialPoints(coords_to_convert, proj4string = svy21_crs)

    # Transform to WGS84
    transformed_points <- spTransform(points, wgs84_crs)

    # Extract latitude and longitude
    wgs84_coords <- coordinates(transformed_points)

    # Place converted coordinates back into the result dataframe at the correct indices
    result_coords$latitude[valid_indices] <- wgs84_coords[, 2]
    result_coords$longitude[valid_indices] <- wgs84_coords[, 1]
  }

  return(result_coords)
}

# Ensure x and y are numeric before conversion
merged_URAJSON <- merged_URAJSON %>%
    mutate(
        x = suppressWarnings(as.numeric(x)),
        y = suppressWarnings(as.numeric(y))
    )

# Perform conversion only if x and y columns exist
if ("x" %in% names(merged_URAJSON) && "y" %in% names(merged_URAJSON)) {
    converted_coords <- convert_svy21_to_wgs84(merged_URAJSON$x, merged_URAJSON$y)
    merged_URAJSON$latitude <- converted_coords$latitude
    merged_URAJSON$longitude <- converted_coords$longitude
    message("Coordinate conversion completed.")
} else {
    message("Skipping coordinate conversion as 'x' or 'y' columns are missing.")
    merged_URAJSON$latitude <- NA_real_
    merged_URAJSON$longitude <- NA_real_
}


# --- Final Output ---
# Ensure column order is maintained
final_available_columns <- column_order[column_order %in% names(merged_URAJSON)]
merged_URAJSON <- merged_URAJSON[, final_available_columns]

# Write the updated data to a new CSV file
message(paste("Writing updated data to:", output_filepath))

# Ensure consistent data types before writing
# ... existing data type conversion before writing ...
# Handle NA in numeric/lat/lon for CSV
merged_URAJSON <- merged_URAJSON %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(across(c(where(is.numeric), latitude, longitude), ~ ifelse(is.na(.), "", as.character(.)))) %>%
    mutate(across(where(is.Date), ~ format(., "%Y-%m-%d")))

# Use write_csv from readr
write_csv(merged_URAJSON, output_filepath, na = "")

message(paste("Successfully updated coordinates, converted to WGS84, and saved to", output_filepath))

# --- Summary ---
final_missing_x <- sum(is.na(merged_URAJSON$x) | merged_URAJSON$x == 0 | merged_URAJSON$x == "")
final_missing_y <- sum(is.na(merged_URAJSON$y) | merged_URAJSON$y == 0 | merged_URAJSON$y == "")
final_missing_latlon <- sum(is.na(merged_URAJSON$latitude) | is.na(merged_URAJSON$longitude))
coords_found <- original_missing_count - final_missing_x # Approx. based on x

message("Summary:")
message(paste("Initial data rows:", nrow(merged_URAJSON))) # Added total rows for context
message(paste("Original missing/invalid SVY21 coordinates:", original_missing_count))
message(paste("Remaining missing/invalid x coordinates:", final_missing_x))
message(paste("Remaining missing/invalid y coordinates:", final_missing_y))
message(paste("SVY21 Coordinates found/updated for approx.", coords_found, "rows"))
message(paste("Rows with missing WGS84 Latitude/Longitude:", final_missing_latlon)) # Added Lat/Lon summary

# Reset to sequential processing
plan(sequential)

message("Process completed!")

# Remove intermediate objects if desired
# rm(list = c("res", "token", "URAres", "URAdata", "results_list", "coords_lookup", "missing_coords_data", "unique_combinations", "converted_coords"))
