# Map Rendering and Interaction Logic
# This script handles the rendering and interaction logic for the map component.
# It includes base map rendering, reverse geocoding, marker display, and spatial filtering.
# Key components:
# - Base map rendering: Initializes the map with default settings and tiles.
# - Reverse geocoding: Determines the current planning area based on the map center.
# - Marker display: Dynamically updates markers based on zoom level and filters.
# - Spatial filtering: Filters data based on map bounds and user-selected criteria.

# --- Base Map Rendering ---
output$property_map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>% # Add default OpenStreetMap map tiles
    setView(lng = sg_lng, lat = sg_lat, zoom = sg_zoom) %>% # Use coordinates/zoom from global.R
    setMaxBounds(
      lng1 = 103.5, lat1 = 1.1,  # Southwest corner of the bounding box
      lng2 = 104.2, lat2 = 1.6   # Northeast corner of the bounding box
    )
})

# --- Reverse Geocoding for Map Center ---
# Debounce the map center input to avoid rapid updates during panning
map_center_debounced <- reactive({
  input$property_map_center
}) %>% debounce(500) # Update 500ms after user stops moving map

current_planning_area <- reactive({
  center <- map_center_debounced()
  pa_sf <- planning_areas_data()

  # Ensure data is loaded and center coordinates are available
  req(center, pa_sf)

  # Create an sf point for the map center (Leaflet uses WGS84 - EPSG:4326)
  center_point <- st_sfc(st_point(c(center$lng, center$lat)), crs = 4326)

  # Transform point CRS to match planning area CRS if necessary
  pa_crs <- st_crs(pa_sf)
  if (st_crs(center_point) != pa_crs) {
    center_point <- st_transform(center_point, crs = pa_crs)
  }

  # Check and repair geometry validity
  if (any(!st_is_valid(pa_sf))) {
    pa_sf <- st_make_valid(pa_sf)
  }

  # Disable S2 temporarily for spatial intersection
  sf_use_s2(FALSE)
  intersection <- st_intersects(center_point, pa_sf, sparse = FALSE)
  sf_use_s2(TRUE)

  # Find the index of the intersecting polygon
  intersecting_index <- which(intersection, arr.ind = TRUE)[, "col"]

  if (length(intersecting_index) > 0) {
    # Dynamically identify the planning area column
    pa_col_name <- NULL
    if ("planning_area" %in% names(pa_sf)) {
      pa_col_name <- "planning_area"
    } else {
      match_idx <- match(tolower("planning_area"), tolower(names(pa_sf)))
      if (!is.na(match_idx)) {
        pa_col_name <- names(pa_sf)[match_idx]
      }
    }

    if (!is.null(pa_col_name)) {
      area_name <- pa_sf[[pa_col_name]][intersecting_index[1]]
      return(toupper(area_name))
    } else {
      return("Planning Area Column Not Found")
    }
  } else {
    return("Outside Planning Area")
  }
})

# --- Marker Display Logic ---

# Get current zoom level
observeEvent(input$property_map_zoom, {
  current_zoom(input$property_map_zoom)
})

# Determine if zoom is sufficient for showing individual property markers
should_show_markers <- reactive({
  zoom <- current_zoom()
  # Progressive loading strategy based on zoom level
  return(zoom >= 12)
})

# Function to determine how many markers to show based on zoom level
markers_to_show <- reactive({
  zoom <- current_zoom()
  # Progressive loading strategy - more markers at higher zoom levels
  if(zoom >= 16) return(3000)      # Very detailed view
  else if(zoom >= 14) return(1500) # Detailed view
  else if(zoom >= 13) return(1000) # Medium view
  else return(500)                 # Overview
})

# Reactive to get visible data with spatial filtering - supports both HDB and private properties
visible_filtered_data <- reactive({
  # Check selected property type
  property_type <- selected_property_type()

  # Get appropriate dataset based on property type
  data <- if(property_type == "HDB") {
    filtered_hdb_data()
  } else {
    filtered_ura_data()
  }

  req(data)
  show_markers <- should_show_markers()

  # Exit early if zoom level is too low or no data
  if (!show_markers || nrow(data) == 0) {
    return(NULL)
  }

  # Get current map bounds if available for spatial filtering
  bounds <- input$property_map_bounds

  # Only apply spatial filtering if bounds are available
  if (!is.null(bounds)) {
    data <- data %>%
      filter(
        longitude >= bounds$west,
        longitude <= bounds$east,
        latitude >= bounds$south,
        latitude <= bounds$north
      )
  }

  # Filter for recent transactions (within 5 years from current date)
  current_date <- as.Date("2025-04-03")  # Current date from context
  five_years_ago <- current_date - (5 * 365)

  # Date column has different names in HDB vs URA
  if(property_type == "HDB") {
    data <- data %>%
      filter(as.Date(month) >= five_years_ago) %>%
      arrange(desc(month))
  } else {
    data <- data %>%
      filter(as.Date(contractDate) >= five_years_ago) %>%
      arrange(desc(contractDate))
  }

  # Limit markers based on zoom level for performance
  limit <- markers_to_show()
  if(nrow(data) > limit) {
    data <- head(data, limit)
  }

  return(data)
})

# Update markers based on filters and zoom level
observe({
  # Get filtered data based on current view
  data <- visible_filtered_data()
  property_type <- selected_property_type()

  # Clear any existing markers and legends
  leafletProxy("property_map") %>%
    clearMarkers() %>%
    clearMarkerClusters() %>%
    clearControls() # Clear all legends and other controls

  # Exit early if no data to show
  if (is.null(data) || nrow(data) == 0) {
    # Clear the legend explicitly if no data
    output$price_legend <- renderUI({ NULL })
    return()
  }

  # Create price palette based on property type
  if(property_type == "HDB") {
    price_palette <- colorNumeric(
      palette = "viridis",
      domain = data$resale_price,
      reverse = FALSE
    )
    price_col <- data$resale_price
  } else {
    price_palette <- colorNumeric(
      palette = "viridis",
      domain = data$price,
      reverse = FALSE
    )
    price_col <- data$price
  }

  # Update map with markers or clusters
  map_proxy <- leafletProxy("property_map")

  # Create popup content and building IDs based on property type
  if(property_type == "HDB") {
    popup_content <- paste0(
      "<strong>", data$block, " ", data$street_name, "</strong><br>",
      "Price: $", format(data$resale_price, big.mark = ","), "<br>",
      "Date: ", data$month, "<br>",
      "Floor: ", data$storey_range, "<br>",
      "Flat Type: ", data$flat_type, "<br>",
      "Area: ", data$floor_area_sqm, " sqm<br>",
      "Built: ", data$lease_commence_date
    )

    # Add unique building identifiers for click events - keep consistent format
    data$building_id <- paste(data$block, data$street_name)
  } else {
    popup_content <- paste0(
      "<strong>", data$project, " - ", data$street, "</strong><br>",
      "Price: $", format(data$price, big.mark = ","), "<br>",
      "Date: ", data$contractDate, "<br>",
      "Floor: ", data$floorRange, "<br>",
      "Type: ", data$propertyType, "<br>",
      "Area: ", data$area, " sqm<br>",
      "Tenure: ", data$tenure
    )

    # Important: Use consistent format for building_id with a hyphen and space
    # This must exactly match what we parse in the click handler
    data$building_id <- paste0(data$project, " - ", data$street)
  }

  # Create marker click event JavaScript
  # This will store marker info in a Shiny input value
  map_proxy %>% addCircleMarkers(
    data = data,
    lng = ~longitude,
    lat = ~latitude,
    radius = 4,
    stroke = FALSE,
    fillOpacity = 0.7,
    fillColor = ~price_palette(price_col),
    popup = popup_content,
    layerId = ~building_id,
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      zoomToBoundsOnClick = TRUE,
      maxClusterRadius = 50,
      disableClusteringAtZoom = 15
    )
  )

  # We'll create a legend in the left overlay instead of on the map
  if(nrow(data) > 0) {
    output$price_legend <- renderUI({
      min_price <- if(property_type == "HDB") min(data$resale_price) else min(data$price)
      max_price <- if(property_type == "HDB") max(data$resale_price) else max(data$price)

      tags$div(
        style = "width: 100%; padding: 8px 0;",
        tags$h5("Property Price Legend", style = "margin-top: 0; margin-bottom: 8px; font-size: 14px;"),
        tags$div(
          style = "display: flex; flex-direction: row; align-items: center;",
          # Create a gradient bar for the legend
          tags$div(
            style = "flex-grow: 1; height: 15px; background: linear-gradient(to right, #440154, #414487, #2a788e, #22a884, #7ad151, #fde725); border-radius: 3px;"
          )
        ),
        tags$div(
          style = "display: flex; justify-content: space-between; margin-top: 2px; font-size: 12px;",
          tags$span(paste0("$", format(min_price, big.mark = ",")), style = "font-weight: bold;"),
          tags$span(paste0("$", format(max_price, big.mark = ",")), style = "font-weight: bold;")
        )
      )
    })
  } else {
      # Clear the legend if no data
      output$price_legend <- renderUI({ NULL })
  }
})

# Marker click observer to update the selected building
observeEvent(input$property_map_marker_click, {
  click <- input$property_map_marker_click

  # Debug: Print the clicked ID to console
  if (!is.null(click) && !is.null(click$id)) {
    print(paste("Marker clicked with ID:", click$id))
  }

  # Reset selection when no click or no id
  if (is.null(click) || is.null(click$id)) {
    selected_building(NULL)
    return()
  }

  property_type <- selected_property_type()

  # Get appropriate dataset
  data <- if(property_type == "HDB") {
    filtered_hdb_data()
  } else {
    filtered_ura_data()
  }

  # Parse the building_id to identify which building was clicked
  if(property_type == "HDB") {
    # For HDB, the ID is in format "block street_name"
    clicked_id <- click$id
    print(paste("Looking for HDB with ID:", clicked_id))

    # First try exact match on the full ID
    matches <- data %>%
      filter(paste(block, street_name) == clicked_id) %>%
      head(1)

    if(nrow(matches) == 0) {
      # If no direct match, try parsing the ID
      parts <- strsplit(clicked_id, " ")[[1]]
      if(length(parts) >= 2) {
        block <- parts[1]
        street_name <- paste(parts[-1], collapse = " ")

        # Debug: Print the extracted block and street
        print(paste("Looking for HDB block:", block, "on street:", street_name))

        # Find the first matching transaction
        matches <- data %>%
          filter(
            block == block,
            street_name == street_name
          ) %>%
          head(1)
      }
    }

    if(nrow(matches) > 0) {
      selected_building(matches)
      print(paste("Selected HDB building:", matches$block, matches$street_name))
    } else {
      print("No matching HDB building found in filtered data")

      # Last resort - try to match by coordinates
      if(!is.null(click$lat) && !is.null(click$lng)) {
        # Find the closest point in the data
        data$dist <- sqrt((data$latitude - click$lat)^2 + (data$longitude - click$lng)^2)
        closest_match <- data %>%
          arrange(dist) %>%
          head(1)

        if(nrow(closest_match) > 0) {
          selected_building(closest_match)
          print(paste("Selected closest HDB by coordinates:", closest_match$block, closest_match$street_name))
        }
      }
    }
  } else {
    # For private properties, the ID is more complex
    clicked_id <- click$id
    print(paste("Looking for private property with ID:", clicked_id))

    # Try direct match on project and street first
    matches <- NULL

    # Extract project and street from the clicked ID
    parts <- strsplit(clicked_id, " - ")[[1]]
    if(length(parts) >= 2) {
      project_name <- parts[1]
      street_name <- parts[2]

      print(paste("Extracted project:", project_name, "and street:", street_name))

      # Try to find by exact project and street match
      matches <- data %>%
        filter(
          project == project_name,
          street == street_name
        ) %>%
        head(1)
    } else {
      # If the separator isn't " - ", try alternative pattern matching
      # First, search using string contains for project name
      similar_projects <- data %>%
        filter(grepl(clicked_id, project, fixed = TRUE) |
                 grepl(project, clicked_id, fixed = TRUE)) %>%
        head(1)

      if(nrow(similar_projects) > 0) {
        matches <- similar_projects
      } else {
        # Last resort - try to match by coordinates
        if(!is.null(click$lat) && !is.null(click$lng)) {
          # Find the closest point in the data
          data$dist <- sqrt((data$latitude - click$lat)^2 + (data$longitude - click$lng)^2)
          matches <- data %>%
            arrange(dist) %>%
            head(1)
        }
      }
    }

    if(!is.null(matches) && nrow(matches) > 0) {
      selected_building(matches)
      print(paste("Selected private property:", matches$project, "-", matches$street))
    } else {
      print("No matching private property found in filtered data")
    }
  }
})
