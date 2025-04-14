# --- 1. Load Required Libraries ---
# install.packages("sf") # Run this line once if you don't have the 'sf' package
library(sf)
library(tools) # For file path manipulation


# --- 2. Configuration ---
# Use relative paths for portability
csv_file_path <- "./data/raw/HDB_Resale_Coordinates.csv"
geojson_file_path <- "./data/clean/district_and_planning_area.geojson"
# --- End Configuration ---

# --- 3. Input Validation ---
if (!file.exists(csv_file_path)) {
  stop("Error: Input CSV file not found at: ", csv_file_path)
}
if (!file.exists(geojson_file_path)) {
  stop("Error: GeoJSON file not found at: ", geojson_file_path)
}

cat("Starting reverse geocoding process...\n")
cat("Input CSV:", csv_file_path, "\n")
cat("GeoJSON:", geojson_file_path, "\n")

# --- 4. Read Input Data ---
# Read the CSV file
cat("Reading CSV file...\n")
tryCatch({
  input_data <- read.csv(csv_file_path, stringsAsFactors = FALSE, check.names = FALSE)
}, error = function(e) {
  stop("Error reading CSV file: ", e$message)
})

# Check if required columns exist
if (!all(c("latitude", "longitude") %in% names(input_data))) {
  stop("Error: CSV file must contain 'latitude' and 'longitude' columns.")
}

# Read the GeoJSON file containing planning area polygons
cat("Reading GeoJSON file...\n")
tryCatch({
  # suppress messages about GDAL driver detection etc.
  planning_areas_sf <- suppressMessages(st_read(geojson_file_path, quiet = TRUE))
}, error = function(e) {
  stop("Error reading GeoJSON file: ", e$message)
})

# Check if 'planning_area' property exists in GeoJSON
# (Handle potential case variations or slightly different names)
pa_col_name <- NULL
if ("planning_area" %in% names(planning_areas_sf)) {
    pa_col_name <- "planning_area"
} else {
    prop_names <- names(planning_areas_sf)
    # Try case-insensitive match
    match_idx <- match(tolower("planning_area"), tolower(prop_names))
     if (!is.na(match_idx)) {
        pa_col_name <- prop_names[match_idx]
        warning(paste0("Exact 'planning_area' property not found in GeoJSON. Using '", pa_col_name, "' instead."))
        # Rename the column for consistency later if needed, but better to use the variable
        # names(planning_areas_sf)[match_idx] <- "planning_area" # Avoid direct rename if using variable
    } else {
         stop("Error: GeoJSON file does not contain a 'planning_area' (or similar) property in its features.")
    }
}


# --- 4b. Check and Repair GeoJSON Geometry Validity ---
cat("Checking GeoJSON geometry validity...\n")
# Suppress warnings during validity check as we will handle invalid cases
validity_check <- suppressWarnings(st_is_valid(planning_areas_sf))
invalid_count <- sum(!validity_check, na.rm = TRUE) # Count FALSE values, ignore NA geometries if any

if (invalid_count > 0) {
    cat(sprintf("Warning: Found %d invalid geometries in the GeoJSON file.\n", invalid_count))
    cat("Attempting to repair geometries using st_make_valid()...\n")
    # Use st_make_valid to fix issues like degenerate edges
    planning_areas_sf <- st_make_valid(planning_areas_sf)

    # Re-check after repair (optional but informative)
    validity_check_after_repair <- suppressWarnings(st_is_valid(planning_areas_sf))
    invalid_count_after_repair <- sum(!validity_check_after_repair, na.rm = TRUE)
    if (invalid_count_after_repair > 0) {
         cat(sprintf("Warning: %d geometries remain invalid after repair attempt. Proceeding, but results might be affected for these areas.\n", invalid_count_after_repair))
    } else {
         cat("Geometries successfully repaired.\n")
    }
} else {
    cat("GeoJSON geometries appear valid.\n")
}

# --- 5. Prepare Spatial Data ---
cat("Preparing spatial data...\n")

# Convert the input data frame to an sf object (points)
# Assuming input coordinates are WGS84 (EPSG:4326)
tryCatch({
    # Explicitly handle potential NA/non-numeric values in coordinates before conversion
    input_data$longitude <- as.numeric(input_data$longitude)
    input_data$latitude <- as.numeric(input_data$latitude)
    valid_coords <- !is.na(input_data$longitude) & !is.na(input_data$latitude)
    if(sum(!valid_coords) > 0) {
        warning(paste("Removed", sum(!valid_coords), "rows with missing or non-numeric latitude/longitude values."))
        input_data_clean <- input_data[valid_coords, ]
    } else {
        input_data_clean <- input_data
    }

    if(nrow(input_data_clean) == 0) {
        stop("No valid coordinates found in the input CSV after cleaning.")
    }

    points_sf <- st_as_sf(input_data_clean, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) # Keep original columns

}, error = function(e) {
    stop("Error converting CSV data to spatial points: ", e$message,
         "\nPlease check 'latitude' and 'longitude' columns for valid numeric data.")
})


# Get the Coordinate Reference System (CRS) of the planning areas
polygons_crs <- st_crs(planning_areas_sf)
cat("GeoJSON CRS:", polygons_crs$input, "\n") # Note: This shows the *input string*, not necessarily the full definition

# Ensure both layers use the same CRS. Transform points CRS to match polygons CRS.
# This is crucial for accurate spatial joins.
if (st_crs(points_sf) != polygons_crs) {
  cat("Transforming points CRS to match GeoJSON CRS...\n")
  # Ensure target CRS is valid before transforming
  if(is.na(polygons_crs)) {
      stop("Cannot transform points: The CRS of the GeoJSON polygon layer is missing or invalid.")
  }
  tryCatch({
      points_sf <- st_transform(points_sf, crs = polygons_crs)
  }, error = function(e) {
      stop("Error transforming points CRS: ", e$message, "\nCheck if the GeoJSON CRS is correctly defined.")
  })

}

# --- 6. Perform Spatial Join (Reverse Geocoding) ---
cat("Performing spatial join (finding points within polygons)...\n")

# --- !!! ADD THIS LINE !!! ---
# Disable S2 geometry engine as it can be strict with minor self-intersections
# or validity issues even after st_make_valid. Use GEOS instead.
cat("Temporarily disabling S2 geometry library for join operation...\n")
sf::sf_use_s2(FALSE)
# --- End of Added Line ---

# Use st_join with st_intersects predicate.
# `left = TRUE` keeps all original points, assigning NA if a point doesn't fall into any polygon.
# Select only the dynamically identified 'planning_area' column from the polygons to join.
# Make sure the planning area column name variable `pa_col_name` is used
tryCatch({
    # Select only the planning area column identified earlier
    planning_areas_subset <- planning_areas_sf[, pa_col_name, drop = FALSE] # Use drop=FALSE to keep sf class even with one col

    # Perform the join
    joined_data_sf <- st_join(points_sf, planning_areas_subset, join = st_intersects, left = TRUE)

}, error = function(e) {
    # If the error persists, suggest trying without S2 (already done above, but keep msg)
    if(grepl("Loop|Edge|degenerate|valid", e$message, ignore.case = TRUE)) {
         message("\nSpatial join failed again, even after disabling S2.")
         message("This suggests the remaining geometry invalidity in the GeoJSON is significant.")
         message("Consider using GIS software (like QGIS) to manually inspect and repair the geometry identified as invalid after st_make_valid, or remove it if acceptable.")
         message("Alternatively, try adding 'planning_areas_sf <- st_buffer(planning_areas_sf, dist = 0)' after 'st_make_valid' as another repair attempt.\n")
    }
    # Re-enable S2 potentially, although for a single script run it might not matter
    # sf::sf_use_s2(TRUE)
    stop("Error during spatial join: ", e$message)
})
# Optionally re-enable S2 if you perform other spherical operations later in the script
# sf::sf_use_s2(TRUE)


# --- 7. Finalize Data ---
cat("Finalizing data...\n")

# Rename the joined planning area column to exactly 'planning_area' if it wasn't already
# This ensures consistent output column naming
if (pa_col_name != "planning_area" && pa_col_name %in% names(joined_data_sf)) {
    names(joined_data_sf)[names(joined_data_sf) == pa_col_name] <- "planning_area"
}


# Convert back to a regular data frame (remove sf geometry column)
# The 'planning_area' column is now appended
output_data <- st_drop_geometry(joined_data_sf)

# Check how many points were successfully matched
# Need to handle the case where the planning_area column might not exist if join failed catastrophically
# (though the tryCatch should prevent reaching here in that case)
if ("planning_area" %in% names(output_data)) {
    matched_count <- sum(!is.na(output_data$planning_area))
    total_count <- nrow(output_data)
    cat(sprintf("Matched %d out of %d points to planning areas.\n", matched_count, total_count))
    if(matched_count < total_count) {
        unmatched_count = total_count - matched_count
        # Be more specific about why points might be unmatched
        cat(sprintf("Note: %d points did not fall within any valid planning area polygon or fell within the polygon that could not be fully repaired.\n", unmatched_count))
    }
} else {
    warning("The 'planning_area' column was not found in the final result. The spatial join might have failed.")
    total_count <- nrow(output_data)
    cat(sprintf("Processed %d points, but planning area information could not be added.\n", total_count))
}

# --- 8. Construct Output Filename and Save ---
# (Rest of the script remains the same)
# Get directory and base name of the input CSV
input_dir <- dirname(csv_file_path)
input_base_noext <- file_path_sans_ext(basename(csv_file_path))

# Create the output filename
output_filename <- paste0(input_base_noext, "_region.csv")
output_file_path <- file.path(input_dir, output_filename)

cat("Writing output file to:", output_file_path, "\n")

# Write the results to a new CSV file
tryCatch({
  write.csv(output_data, output_file_path, row.names = FALSE, na = "", quote = FALSE) # Represent NA as empty string in CSV
}, error = function(e) {
  stop("Error writing output CSV file: ", e$message)
})

cat("Process completed successfully!\n")