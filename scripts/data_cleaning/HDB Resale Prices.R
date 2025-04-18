# HDB Resale Prices retrieved from HDB data.gov on Mar 29, 2025. We cannot guarantee data accuracy for data added beyodn this retrieval date.
hdb_resale_prices <- read.csv("./data/raw/HDB Resale.csv")

# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(future)
library(future.apply)

# Configure parallel processing
plan(multisession) # Use multisession backend for parallel processing

# Function to retrieve OneMap API token dynamically using R
get_auth_token <- function() {
    url <- "https://www.onemap.gov.sg/api/auth/post/getToken"
    payload <- list(
        # Retrieve from environment variables
        email = Sys.getenv("ONEMAP_EMAIL"),
        password = Sys.getenv("ONEMAP_EMAIL_PASSWORD")
    )
    response <- httr::POST(url, body = payload, encode = "json")
    if (httr::status_code(response) != 200) {
        stop("Failed to retrieve auth token")
    }
    content <- httr::content(response, as = "parsed", type = "application/json")
    return(content$access_token)
}

# Retrieve the auth token once at the beginning
auth_token <- get_auth_token()

# Create a function to get coordinates for a given block and street name
get_coordinates <- function(block, street_name) {
    search_val <- paste(block, street_name, sep = " ")
    api_url <- paste0("https://www.onemap.gov.sg/api/common/elastic/search?searchVal=", 
                      URLencode(search_val), 
                      "&returnGeom=Y&getAddrDetails=Y&pageNum=1")
    response <- tryCatch({
        httr::GET(api_url, add_headers(Authorization = paste("Bearer", auth_token)))
    }, error = function(e) {
        return(NULL)
    })
    if (is.null(response) || httr::http_status(response)$category != "Success") {
        return(list(latitude = NA, longitude = NA, postal = NA))
    }
    resp_content <- httr::content(response, "text", encoding = "UTF-8")
    tryCatch({
        content <- jsonlite::fromJSON(resp_content)
        if (is.null(content$results) || length(content$results) == 0) {
            return(list(latitude = NA, longitude = NA, postal = NA))
        }
        result <- if (is.data.frame(content$results)) {
            content$results[1,]
        } else if (is.list(content$results) && length(content$results) > 0) {
            content$results[[1]]
        } else {
            return(list(latitude = NA, longitude = NA, postal = NA))
        }
        lat <- if (!is.null(result$LATITUDE)) as.numeric(result$LATITUDE) else NA
        lng <- if (!is.null(result$LONGITUDE)) as.numeric(result$LONGITUDE) else NA
        pst <- if (!is.null(result$POSTAL)) result$POSTAL else NA
        
        if (!is.na(lat) && !is.na(lng) && lat != 0 && lng != 0) {
            return(list(latitude = lat, longitude = lng, postal = pst))
        }
        return(list(latitude = NA, longitude = NA, postal = NA))
    }, error = function(e) {
        return(list(latitude = NA, longitude = NA, postal = NA))
    })
}

# Create a unique address identifier
hdb_resale_prices <- hdb_resale_prices %>%
  mutate(address = paste(block, street_name, sep = " "))

# Get unique addresses
unique_addresses <- hdb_resale_prices %>%
  distinct(block, street_name, address)

# Use future_lapply for parallel processing
# We pass block and street_name as arguments to get_coordinates
# The progress bar can be enabled with progress = TRUE if the future.apply version supports it
# Note: Error handling within future_lapply might require adjustments depending on specific needs.
# The current get_coordinates function returns NA on errors, which is handled below.
results_list <- future_lapply(seq_len(nrow(unique_addresses)), function(i) {
  get_coordinates(unique_addresses$block[i], unique_addresses$street_name[i])
}, future.seed = TRUE) # future.seed = TRUE for reproducibility if needed

# Combine results into a dataframe
coords_df <- bind_rows(results_list)
coords_df$address <- unique_addresses$address # Add the address identifier back

# Join coordinates back to the original dataframe
hdb_resale_prices <- hdb_resale_prices %>%
  left_join(coords_df %>% select(address, latitude, longitude, postal), by = "address")

# Remove the temporary address column if not needed
hdb_resale_prices <- hdb_resale_prices %>% select(-address)

# Rename 'postal' column to 'postal_code' to match original intent (optional)
hdb_resale_prices <- hdb_resale_prices %>% rename(postal_code = postal)

# Save the final dataframe
# Consider saving to a new file to avoid overwriting potentially useful intermediate files
output_filename <- "./data/clean/HDB_Resale_Coordinates.csv"
write.csv(hdb_resale_prices, output_filename, row.names = FALSE, quote = FALSE)

# Optional: Clean up parallel workers
plan(sequential) # Reset to default sequential processing