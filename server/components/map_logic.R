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
  # Calculate responsive zoom level based on screen width
  # This function is called once when the app initializes
  # We'll set both the initial zoom and minimum zoom to the same value
  
  # Default zoom level based on typical screen sizes, will be updated by JS later
  initialZoom <- 11  # Default value
  
  # Create the map with minZoom set to the same as initialZoom
  leaflet(options = leafletOptions(
    zoomControl = FALSE,
    minZoom = initialZoom,  # Setting minimum zoom equal to initial zoom
    zoomSnap = 0.5,         # Allow finer zoom levels (0.5 increments)
    zoomDelta = 0.5,        # Zoom changes in 0.5 increments
    attributionControl = FALSE
  )) %>%
    addTiles() %>%
    setView(lng = sg_lng, lat = sg_lat, zoom = initialZoom) %>%
    setMaxBounds(
      lng1 = 103.5, lat1 = 1.1,  # Southwest corner of the bounding box
      lng2 = 104.2, lat2 = 1.6   # Northeast corner of the bounding box
    )
})

# Send screenDimensions request immediately after session starts
observe({
  # This ensures the client sends screen dimensions as early as possible
  session$sendCustomMessage(type = "getScreenDimensions", message = list())
}, priority = 1000) # High priority to run early

# Observer to update map zoom based on screen dimensions
observeEvent(input$screenDimensions, {
  dims <- input$screenDimensions
  
  # Calculate responsive zoom level based on screen width
  responsive_zoom <- if (dims$width < 768) {
    10
  } else if (dims$width < 1200) {
    10.5
  } else if (dims$width < 1600) {
    11
  } else {
    11.5
  }
  
  # Update map with the responsive zoom levels
  # We'll recreate the map completely with the new minZoom setting
  leafletProxy("property_map") %>%
    setView(lng = sg_lng, lat = sg_lat, zoom = responsive_zoom)
  
  # Send the new minZoom value to JavaScript to enforce it client-side
  session$sendCustomMessage(type = "setMinZoom", message = list(zoom = responsive_zoom))
  
  # Update global zoom variable for reference
  sg_zoom <<- responsive_zoom
})

# --- Reverse Geocoding for Map Center ---
# Debounce the map center input to avoid rapid updates during panning
map_center_debounced <- reactive({
  input$property_map_center
}) %>% debounce(400) # Update 400ms after user stops moving map

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
  # More restrictive - only show individual markers at higher zoom levels
  return(zoom >= 13) # Increased from 12 to 13
})

# Function to determine visualization mode based on zoom level
map_visualization_mode <- reactive({
  zoom <- current_zoom()
  if(zoom >= 13) {
    return("markers") # Show individual markers when zoomed in
  } else if(zoom >= 10) {
    return("heatmap") # Show heatmap when at medium zoom
  } else {
    return("none")    # Show nothing when fully zoomed out
  }
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
  
  # Efficient spatial filtering - apply bounds filter first for better performance
  if (!is.null(bounds)) {
    # Expand bounds slightly to prevent edge artifacts
    bounds_expanded <- list(
      west = bounds$west - 0.005,
      east = bounds$east + 0.005, 
      south = bounds$south - 0.005,
      north = bounds$north + 0.005
    )
    
    data <- data %>%
      filter(
        longitude >= bounds_expanded$west,
        longitude <= bounds_expanded$east,
        latitude >= bounds_expanded$south,
        latitude <= bounds_expanded$north
      )
  }
  
  # Optional early exit if no data in view
  if (nrow(data) == 0) {
    return(NULL)
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
  
  # Implement spatial pre-clustering to reduce markers
  zoom <- current_zoom()
  grid_size <- if(zoom >= 16) 0.0005 else if(zoom >= 14) 0.001 else if(zoom >= 13) 0.002 else 0.004
  
  # Group nearby points using grid-based clustering
  data <- data %>%
    mutate(
      grid_lon = floor(longitude / grid_size) * grid_size,
      grid_lat = floor(latitude / grid_size) * grid_size
    )
  
  # If there are too many points, use representative points from each grid cell
  if (nrow(data) > markers_to_show()) {
    # For each grid cell, keep the most recent transaction
    data <- data %>%
      group_by(grid_lon, grid_lat) %>%
      slice_head(n = 1) %>%
      ungroup()
  }
  
  # Still limit total markers for extremely dense areas
  limit <- markers_to_show()
  if (nrow(data) > limit) {
    data <- head(data, limit)
  }
  
  return(data)
})

# Update markers based on filters and zoom level
observe({
  # Get data based on current view and determine visualization mode
  property_type <- selected_property_type()
  vis_mode <- map_visualization_mode()
  
  # Clear any existing markers and legends
  map_proxy <- leafletProxy("property_map") %>%
    clearMarkers() %>%
    clearMarkerClusters() %>%
    clearHeatmap() %>%
    clearControls() # Clear all legends and other controls
  
  # Exit early if we should show nothing (very zoomed out)
  if (vis_mode == "none") {
    output$price_legend <- renderUI({ NULL })
    return()
  }
  
  # Get appropriate data based on visualization mode
  if (vis_mode == "markers") {
    # Use the filtered and pre-clustered data for markers
    data <- visible_filtered_data()
  } else if (vis_mode == "heatmap") {
    # For heatmap, we can use more data points since they're aggregated
    data <- if(property_type == "HDB") {
      filtered_hdb_data()
    } else {
      filtered_ura_data()
    }
    
    # Apply basic spatial filtering for the heatmap
    bounds <- input$property_map_bounds
    if (!is.null(bounds) && !is.null(data) && nrow(data) > 0) {
      data <- data %>%
        filter(
          longitude >= bounds$west,
          longitude <= bounds$east,
          latitude >= bounds$south,
          latitude <= bounds$north
        )
      
      # Random sampling for very large datasets to improve performance
      if (nrow(data) > 5000) {
        data <- data[sample(nrow(data), 5000), ]
      }
    }
  }
  
  # Exit if no data to show
  if (is.null(data) || nrow(data) == 0) {
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
  
  # Apply the appropriate visualization based on mode
  if (vis_mode == "heatmap") {
    # Add heatmap layer
    intensity_col <- if(property_type == "HDB") data$resale_price else data$price
    
    map_proxy %>% addHeatmap(
      data = data,
      lng = ~longitude,
      lat = ~latitude,
      intensity = intensity_col,
      blur = 20,
      max = 0.5, 
      radius = 15
    )
  } else if (vis_mode == "markers") {
    # Create popup content and building IDs based on property type
    if(property_type == "HDB") {
      popup_content <- paste0(
        "<strong>", data$block, " ", data$street_name, "</strong><br>",
        "Price: $", format(data$resale_price, big.mark = ","), "<br>",
        "Date: ", data$month
      )
      data$building_id <- paste(data$block, data$street_name)
    } else {
      popup_content <- paste0(
        "<strong>", data$project, " - ", data$street, "</strong><br>",
        "Price: $", format(data$price, big.mark = ","), "<br>",
        "Date: ", data$contractDate
      )
      data$building_id <- paste0(data$project, " - ", data$street)
    }
    
    # Use a different marker rendering approach based on the number of points
    if(nrow(data) > 500) {
      # For large datasets, use more aggressive clustering and simpler markers
      map_proxy %>% addCircleMarkers(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        radius = 7,
        stroke = FALSE,
        fillOpacity = 0.8,
        fillColor = ~price_palette(price_col),
        layerId = ~building_id, # Keep layerId for click events
        # Skip popup content for large datasets - will show on click instead
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = FALSE,
          zoomToBoundsOnClick = TRUE,
          maxClusterRadius = 80,
          disableClusteringAtZoom = 16
        )
      )
    } else {
      # For smaller datasets, use normal markers with popups
      map_proxy %>% addCircleMarkers(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
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
    }
  }
  
  # Show price legend for any visualization type
  if(nrow(data) > 0) {
    # Only use property data for min/max price calculation
    min_price <- if(property_type == "HDB") min(data$resale_price, na.rm = TRUE) else min(data$price, na.rm = TRUE)
    max_price <- if(property_type == "HDB") max(data$resale_price, na.rm = TRUE) else max(data$price, na.rm = TRUE)
    output$price_legend <- renderUI({
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
  
  # Only show facility markers when we're zoomed in enough to see individual points
  # (vis_mode == "markers" AND zoom is high enough to disable clustering)
  zoom <- current_zoom()
  if (vis_mode == "markers" && zoom >= 15) {
    facilities <- filtered_facilities()
    for (facility_type in names(facilities)) {
      data <- facilities[[facility_type]]
      if (!"name" %in% colnames(data)) next
      data <- data[!is.na(data$name) & data$name != "", ]
      if (nrow(data) == 0) next
      map_proxy <- map_proxy %>%
        addMarkers(
          data = data,
          lng = ~longitude,
          lat = ~latitude,
          icon = facility_icons[[facility_type]],
          group = "facilities",
          popup = ~paste(
            "<strong>", tools::toTitleCase(name), "</strong><br>",
            if (facility_type == "mrt" && "exit" %in% colnames(data)) {
              paste0("<span style='font-size: smaller;'>Exit: <strong>", exit, "</strong></span><br>")
            } else {
              ""
            },
            "<span style='font-size: x-small;'>", tools::toTitleCase(facility_type), "</span>"
          )
        )
    }
  }
})

# Marker click observer to update the selected building
observeEvent(input$property_map_marker_click, {
  click <- input$property_map_marker_click
  
  # Reset selection when no click or no id
  if (is.null(click) || is.null(click$id)) {
    selected_building(NULL)
    # Hide the right overlay when clicking outside of markers
    session$sendCustomMessage("hideRightOverlay", list())
    # Hide the right overlay when clicking outside of markers
    session$sendCustomMessage("hideRightOverlay", list())
    return()
  }
  
  property_type <- selected_property_type()
  
  # Get appropriate dataset
  data <- if(property_type == "HDB") {
    filtered_hdb_data()
  } else {
    filtered_ura_data()
  }
  
  building_found <- FALSE  # Flag to track if we found a matching building
  
  # Parse the building_id to identify which building was clicked
  if(property_type == "HDB") {
    # For HDB, the ID is in format "block street_name"
    clicked_id <- click$id
    
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
      building_found <- TRUE
      building_found <- TRUE
    } else {
      # Last resort - try to match by coordinates
      if(!is.null(click$lat) && !is.null(click$lng)) {
        # Find the closest point in the data
        data$dist <- sqrt((data$latitude - click$lat)^2 + (data$longitude - click$lng)^2)
        closest_match <- data %>%
          arrange(dist) %>%
          head(1)
        
        if(nrow(closest_match) > 0) {
          selected_building(closest_match)
          building_found <- TRUE
          building_found <- TRUE
        }
      }
    }
  } else {
    # For private properties, the ID is more complex
    clicked_id <- click$id
    
    # Try direct match on project and street first
    matches <- NULL
    
    # Extract project and street from the clicked ID
    parts <- strsplit(clicked_id, " - ")[[1]]
    if(length(parts) >= 2) {
      project_name <- parts[1]
      street_name <- parts[2]
      
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
      building_found <- TRUE
      building_found <- TRUE
    }
  }
  
  # Show right overlay only if a building was found
  if(building_found) {
    session$sendCustomMessage("showRightOverlay", list())
  } else {
    session$sendCustomMessage("hideRightOverlay", list())
  }
  
  # Show right overlay only if a building was found
  if(building_found) {
    session$sendCustomMessage("showRightOverlay", list())
  } else {
    session$sendCustomMessage("hideRightOverlay", list())
  }
})

# Update the visualization mode text based on current view mode
output$visualization_mode_text <- renderText({
  mode <- map_visualization_mode()
  zoom <- current_zoom()
  
  if(mode == "markers") {
    paste("Viewing Property Markers (Zoom:", zoom, ")")
  } else if(mode == "heatmap") {
    paste("Price Heatmap View (Zoom:", zoom, ") - Zoom in for markers")
  } else {
    paste("Overview Mode (Zoom:", zoom, ") - Zoom in to see properties")
  }
})

# Facility icons using updated URLs
facility_icons <- iconList(
  gym = makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/7984/7984880.png", 
    iconWidth = 25, iconHeight = 25
  ),
  childcare = makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/17012/17012962.png", 
    iconWidth = 25, iconHeight = 25
  ),
  park = makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/7057/7057859.png", 
    iconWidth = 25, iconHeight = 25
  ),
  supermarket = makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/2331/2331970.png", 
    iconWidth = 25, iconHeight = 25
  ),
  school = makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/2602/2602414.png", 
    iconWidth = 25, iconHeight = 25
  ),
  mrt = makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/821/821354.png", 
    iconWidth = 25, iconHeight = 25
  )
)

# Reactive to filter facilities based on user selection
filtered_facilities <- reactive({
  selected <- user_selection()
  req(selected)
  
  facilities <- list()
  if ("Gym" %in% selected) facilities$gym <- gym()
  if ("Childcare Centre" %in% selected) facilities$childcare <- childcare()
  if ("Park" %in% selected) facilities$park <- park()
  if ("Supermarket" %in% selected) facilities$supermarket <- mart()
  if ("School" %in% selected) facilities$school <- sch()
  if ("LRT/MRT" %in% selected) facilities$mrt <- mrt()
  
  facilities
})