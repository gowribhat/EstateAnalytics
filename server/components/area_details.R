# UI Update Logic (Left Overlay)
# This script handles the logic for updating UI elements dynamically based on user interactions and data changes.
# It includes rendering income statistics, price density plots, and other visualizations for the left overlay.
# Key components:
# - Income statistics: Displays household income distribution for the selected planning area.
# - Price density plot: Visualizes the price distribution of visible transactions on the map.
# - Reactive datasets: Filters and processes data for visualizations based on user inputs and map interactions.

# --- Update UI Elements ---
# Update the region name text
output$current_region_name <- renderText({
  current_planning_area() # Assumes current_planning_area is defined in map_logic.R
})

# Income statistics for the current region
output$income_stats <- NULL

# Reactive to check income data availability and retrieve necessary data
income_data_status <- reactive({
  area_name <- current_planning_area()
  income_data <- household_income_data()

  # Default status
  status <- list(available = FALSE, area_name = area_name, total_households = NA, region_data = NULL)

  # Check prerequisites
  if (is.null(income_data) || is.null(area_name) || area_name == "Outside Planning Area") {
    # If no area selected or outside, explicitly set area_name for message
    if(is.null(area_name)) status$area_name <- "Selected Area"
    return(status)
  }

  # Find data for the region
  region_data_filtered <- income_data %>%
    filter(toupper(Number) == toupper(area_name))

  if (nrow(region_data_filtered) > 0) {
    total_households_val <- region_data_filtered$Total
    if (!is.na(total_households_val) && total_households_val > 0) {
      status$available <- TRUE
      status$total_households <- total_households_val
      status$region_data <- region_data_filtered # Pass data along
    }
  }
  # Ensure area_name is always populated in the status for the message
  status$area_name <- area_name
  return(status)
})

# UI Output for Income section (conditional rendering)
output$income_display_ui <- renderUI({
  status <- income_data_status()
  req(status) # Ensure status is calculated

  if (status$available) {
    # If data is available, render the plotly output container WITH an overlay
    div(
      style = "position: relative; margin-bottom: 15px;", # Container needs relative positioning
      plotlyOutput("income_plot_output", height = "200px"), # Adjust height as needed
      # Add a transparent overlay div to block interactions
      div(style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 10; background-color: rgba(0,0,0,0);")
    )
  } else {
    # If data is not available, render the message
    div(
      style = "padding: 15px; margin-bottom: 15px; text-align: center; background-color: #f8f9fa; border-radius: 5px; color: #555;",
      h5(paste0('Household Income'), style="margin-bottom: 5px; font-size: 13px; font-weight: bold;"),
      # Use the area_name from the status, which handles NULL/Outside cases
      p(paste("Income data not available for", status$area_name))
    )
  }
})


# Render the plotly income visualization - ONLY if data is available
output$income_plot_output <- renderPlotly({
  # This plot only renders if income_display_ui decided to show its container
  status <- income_data_status()
  # Use req() to ensure all necessary reactive values derived *within* this reactive context are valid
  req(status$available, status$region_data, status$total_households, status$area_name)

  # Retrieve data from status
  region_data <- status$region_data
  total_households <- status$total_households
  area_name <- status$area_name # Use area_name from status for consistency

  # Proceed with calculations and plotting (existing logic)
  no_income <- region_data$NoEmployedPerson
  no_income_percent <- round(no_income / total_households * 100, 1)

  low_income <- sum(region_data$Below_1_000, region_data$X1_000_1_999, region_data$X2_000_2_999)
  low_income_percent <- round(low_income / total_households * 100, 1)

  mid_income <- sum(region_data$X3_000_3_999, region_data$X4_000_4_999, region_data$X5_000_5_999,
                   region_data$X6_000_6_999, region_data$X7_000_7_999, region_data$X8_000_8_999)
  mid_income_percent <- round(mid_income / total_households * 100, 1)

  high_income <- sum(region_data$X9_000_9_999, region_data$X10_000_10_999, region_data$X11_000_11_999,
                    region_data$X12_000_12_999, region_data$X13_000_13_999, region_data$X14_000_14_999,
                    region_data$X15_000_17_499, region_data$X17_500_19_999)
  high_income_percent <- round(high_income / total_households * 100, 1)

  affluent <- region_data$X20_000andOver
  affluent_percent <- round(affluent / total_households * 100, 1)

  # Create data for plotly horizontal stacked bar chart
  legend_labels <- c("No Income", "<$3K", "$3-9K", "$9-20K", ">$20K")
  hover_labels <- c("No Income", "Low Income (<$3K)", "Mid Income ($3-9K)",
                    "High Income ($9-20K)", "Affluent (>$20K)")
  values <- c(no_income_percent, low_income_percent, mid_income_percent,
             high_income_percent, affluent_percent)

  # Ensure sum is close to 100, adjust largest category if needed due to rounding
  # Check if sum is significantly off (e.g., more than 0.1 difference)
  if (abs(sum(values) - 100) > 0.1 && sum(values) > 0) {
      values <- values / sum(values) * 100
  }
  values <- round(values, 1) # Re-round after potential normalization

  colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")

  plot_data <- data.frame(
    y = "Income Distribution",
    x = values,
    income_type = factor(legend_labels, levels = legend_labels),
    hover_label = hover_labels,
    stringsAsFactors = FALSE
  )

  # Create horizontal stacked bar chart using plotly
  plot_ly() %>%
    add_bars(
      data = plot_data,
      x = ~x,
      y = ~y,
      color = ~income_type,
      colors = colors,
      hoverinfo = "text",
      hovertext = ~paste0(hover_label, ": ", x, "%"),
      orientation = 'h',
      text = ~paste0(x, "%"),
      textposition = 'inside',
      insidetextanchor = 'middle',
      textfont = list(color = 'white', size=10) # Make inside text white and smaller
    ) %>%
    layout(
      title = list(
        text = paste0('<b>Household Income</b><br>',
                     '<span style="font-size: 11px;">Total: ',
                     format(total_households, big.mark=","), ' households</span>'),
        font = list(size = 13)
      ),
      barmode = 'stack',
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        y = -0.2, # Adjust legend position further down if needed
        x = 0.5,
        xanchor = 'center',
        yanchor = 'top',
        font = list(size = 10)
      ),
      xaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE, showgrid = FALSE, range = c(0, 100)), # Ensure x-axis goes 0-100
      yaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE, showgrid = FALSE),
      margin = list(l = 10, r = 10, t = 50, b = 40) # Increase bottom margin for legend
    )
})

# Reactive dataset for the area summary plot
visible_transactions <- reactive({
  # Determine property type and get corresponding filtered data
  property_type <- selected_property_type()
  data <- if(property_type == "HDB") {
    filtered_hdb_data()
  } else {
    filtered_ura_data()
  }
  req(data)

  # Get current map bounds if available
  bounds <- input$property_map_bounds

  # Filter by map bounds if we have them
  if (!is.null(bounds)) {
    data <- data %>%
      filter(
        longitude >= bounds$west,
        longitude <= bounds$east,
        latitude >= bounds$south,
        latitude <= bounds$north
      )
  }

  return(data)
})


# Price density plot for the currently visible area
output$summary_plot <- renderPlot({
  data <- visible_transactions()
  req(data)
  req(nrow(data) > 0)

  # Get current planning area and zoom level
  area_name <- current_planning_area()
  zoom_level <- current_zoom()
  zoom_threshold <- 15 # Define the zoom level threshold

  # Determine plot title based on zoom level
  plot_title <- if (zoom_level >= zoom_threshold && area_name != "Outside Planning Area") {
    paste0("Price Distribution in\n", area_name)
  } else {
    "Price Distribution of Area"
  }

  # Determine price column based on property type
  property_type <- selected_property_type()
  price_col_name <- if(property_type == "HDB") "resale_price" else "price"

  # Create a count-based histogram with properly aligned density curve
  # Calculate bin width based on data range and bin count
  bin_count <- 15
  bin_width <- (max(data[[price_col_name]]) - min(data[[price_col_name]])) / bin_count
  
  ggplot(data, aes_string(x = price_col_name)) +
    geom_histogram(fill = "#4676a9", alpha = 0.5, bins = bin_count) +
    # Scale density to histogram counts properly
    stat_density(geom = "line", 
                aes_string(y = paste0("after_stat(density) * ", nrow(data), " * ", bin_width)), 
                color = "#003366", size = 1.2) +
    geom_vline(aes(xintercept = median(data[[price_col_name]])),
              color = "#ff5555", linetype = "dashed", size = 1) +
    labs(
      title = plot_title, # Use the conditional title
      subtitle = paste0("Median: $", format(median(data[[price_col_name]]), big.mark = ",")),
      x = "Price (SGD)",
      y = "Count"
    ) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
})

# Move planning_areas_sf load to a reactive value at the top level
planning_areas_data <- reactive({
  tryCatch({
    pa_data <- st_read("data/clean/district_and_planning_area.geojson", quiet = TRUE)
    
    # Ensure geometry is valid upon loading
    if (!inherits(pa_data, "sf")) {
        stop("Loaded planning area data is not an sf object.")
    }
    if (any(!st_is_valid(pa_data))) {
      print("Fixing invalid geometries in planning areas upon load...")
      pa_data <- st_make_valid(pa_data)
    }
    
    print("Planning areas columns:")
    print(names(pa_data))
    return(pa_data)
  }, error = function(e) {
    warning("Error loading or validating planning areas: ", e$message)
    NULL
  })
})

# Function to get planning area for facilities using reverse geocoding
get_facilities_in_area <- function(facility_data, planning_areas_sf, area_name) {
  # Debug prints (keep for now)
  print(paste("Processing facilities for area:", area_name))
  
  # Ensure required inputs are valid sf objects and area_name is not NULL/empty
  req(facility_data, inherits(planning_areas_sf, "sf"), !is.null(area_name), nzchar(area_name), area_name != "Outside Planning Area")
  
  # Ensure facility data has coordinates and remove rows with missing ones
  if (!all(c("longitude", "latitude") %in% names(facility_data))) {
      stop("Facility data must contain 'longitude' and 'latitude' columns.")
  }
  facility_data <- facility_data %>% filter(!is.na(longitude) & !is.na(latitude))
  print(paste("Number of facilities with valid coordinates:", nrow(facility_data)))
  if (nrow(facility_data) == 0) {
      print("No facilities with valid coordinates.")
      return(0)
  }

  # Create spatial points for facilities (use WGS84 - EPSG:4326)
  # Use remove=FALSE to keep original coordinate columns if needed later
  facilities_sf <- st_as_sf(facility_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) 
  
  # Find planning area column dynamically (case-insensitive search)
  pa_col_candidates <- grep("planning.*area|pln.*area", names(planning_areas_sf), ignore.case = TRUE, value = TRUE)
  if (length(pa_col_candidates) > 0) {
      pa_col <- pa_col_candidates[1] # Take the first match
  } else if ("planning_area" %in% names(planning_areas_sf)) {
      pa_col <- "planning_area" # Default fallback
  } else {
      warning("Could not automatically determine the planning area column name in the planning areas dataset.")
      # Attempt to find a likely candidate or stop
      potential_cols <- names(planning_areas_sf)[!sapply(planning_areas_sf, inherits, "sf")] # Exclude geometry
      if (length(potential_cols) > 0) {
          pa_col <- potential_cols[1] # Guess the first non-geometry column
          warning(paste("Assuming planning area column is:", pa_col))
      } else {
          stop("No suitable planning area column found.")
      }
  }
  print(paste("Using planning area column:", pa_col))
  
  # Ensure the planning area column exists in the sf object
  if (!pa_col %in% names(planning_areas_sf)) {
      stop(paste("Planning area column '", pa_col, "' not found in planning_areas_sf."))
  }

  # Filter the target planning area polygon using case insensitive comparison
  # Use .data pronoun for non-standard evaluation safety
  current_area <- planning_areas_sf %>% 
      filter(toupper(.data[[pa_col]]) == toupper(area_name))
      
  if (nrow(current_area) == 0) {
    print(paste("No matching planning area polygon found for:", area_name))
    # Optional: Print available areas for debugging
    # print(paste("Available areas:", paste(unique(planning_areas_sf[[pa_col]]), collapse=", ")))
    return(0)
  }
  # Ensure we only have one polygon for the target area
  if (nrow(current_area) > 1) {
      warning(paste("Multiple polygons found for planning area:", area_name, ". Using the first one."))
      current_area <- head(current_area, 1)
  }
  
  # Ensure CRS matches between facilities and the target area polygon
  target_crs <- st_crs(current_area)
  if (st_crs(facilities_sf) != target_crs) {
    print(paste("Transforming facilities CRS from", st_crs(facilities_sf)$input, "to", target_crs$input))
    facilities_sf <- st_transform(facilities_sf, crs = target_crs)
  }
  
  # Perform spatial intersection (points in polygon)
  # Disable S2 temporarily for consistency with map_logic.R's reverse geocoding
  sf_use_s2(FALSE)
  count <- 0 # Initialize count
  tryCatch({
      # Use the filtered 'current_area' polygon for intersection
      # st_intersects returns a list. Each element corresponds to a facility.
      # The content of each element is the index(es) of the planning area polygon(s) it intersects.
      # Since current_area has only one polygon, we expect intersecting facilities to have list elements like c(1L).
      # Non-intersecting facilities will have list elements like integer(0).
      intersection_list <- st_intersects(facilities_sf, current_area)

      # Correctly count how many elements in the list have length > 0 (meaning they intersected the polygon)
      count <- sum(sapply(intersection_list, length) > 0)

  }, error = function(e) {
      warning(paste("Error during spatial intersection for", area_name, ":", e$message))
      count <- 0 # Return 0 on error
  }, finally = {
      sf_use_s2(TRUE) # Ensure S2 is re-enabled
  })

  print(paste("Found", count, "facilities intersecting with area:", area_name))
  return(count)
}

# Add facility summary count to the UI
output$facility_summary <- renderUI({
  # Get current planning area
  area_name <- current_planning_area()
  
  # Get planning areas data (already validated in the reactive)
  planning_areas_sf <- planning_areas_data()
  
  # Enhanced debugging - print more detailed information
  print(paste("Facility Summary UI - Current area:", area_name))
  print(paste("Facility Summary UI - Planning areas loaded:", !is.null(planning_areas_sf)))
  
  # Exit if no area selected or no planning areas data
  if (is.null(area_name) || !nzchar(area_name) || area_name == "Outside Planning Area" || is.null(planning_areas_sf)) {
    return(div(
      style = "margin: 15px 0; padding: 15px; background-color: #f8f9fa; border-radius: 5px; text-align: center; color: #6c757d;",
      "Select a planning area on the map to see facility counts." # Updated message
    ))
  }
  
  # Validate planning areas data type (redundant check, but safe)
  if (!inherits(planning_areas_sf, "sf")) {
    warning("Facility Summary UI: Planning areas data is not an sf object.")
    return(div(
      style = "margin: 15px 0; padding: 15px; background-color: #fff3cd; border-radius: 5px; color: #856404;",
      "Error: Invalid planning areas data format."
    ))
  }
  
  # Load all required facility data reactives
  # Use req() to ensure they are loaded before proceeding
  childcare_data <- childcare()
  gym_data <- gym()
  mrt_data <- mrt()
  park_data <- park()
  school_data <- sch()
  mart_data <- mart()
  req(childcare_data, gym_data, mrt_data, park_data, school_data, mart_data)
  
  # Get facility counts with error handling for the overall process
  tryCatch({
    # Get counts using the refined function
    childcare_count <- get_facilities_in_area(childcare_data, planning_areas_sf, area_name)
    gym_count <- get_facilities_in_area(gym_data, planning_areas_sf, area_name)
    mrt_count <- get_facilities_in_area(mrt_data, planning_areas_sf, area_name)
    park_count <- get_facilities_in_area(park_data, planning_areas_sf, area_name)
    school_count <- get_facilities_in_area(school_data, planning_areas_sf, area_name)
    supermarket_count <- get_facilities_in_area(mart_data, planning_areas_sf, area_name)

    # Helper function to create styled facility item (unchanged)
    create_facility_item <- function(icon, label, count) {
      div(
        style = "display: flex; align-items: center; justify-content: center; background-color: #ffffff; border: 1px solid #e9ecef; border-radius: 6px; padding: 5px 8px; text-align: center;", # Adjusted padding
        span(style = "font-size: 1.3em; margin-right: 5px;", icon), # Slightly smaller icon, added right margin
        # Removed the label span
        span(style = "font-weight: bold; font-size: 0.9em; color: #007bff;", count) # Slightly smaller count size
      )
    }

    # Render the UI block (unchanged)
    div(
      style = "margin: 15px 0; padding: 10px; background-color: #f8f9fa; border-radius: 8px;",
      h5("Facilities in Area", style = "margin-top: 0; margin-bottom: 10px; font-size: 1.0em; font-weight: 600; color: #343a40; text-align: center;"),
      # Use CSS Grid for layout
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px;", # Use CSS Grid with 3 columns
        # Create items using the helper function (label is now ignored)
        create_facility_item("🏫", "Schools", school_count),
        create_facility_item("👶", "Childcare", childcare_count),
        create_facility_item("🚉", "MRT/LRT", mrt_count),
        create_facility_item("🏋️", "Gyms", gym_count),
        create_facility_item("🌳", "Parks", park_count),
        create_facility_item("🛒", "Markets", supermarket_count)
      )
    )
  }, error = function(e) {
    # Catch errors specifically from the counting process within the UI render
    print(paste("Error counting facilities within UI render:", e$message))
    # Return error message if something goes wrong
    div(
      style = "margin: 15px 0; padding: 15px; background-color: #fff3cd; border-radius: 5px; color: #856404;",
      "Unable to load facility counts for the selected area."
    )
  })
})

# Update the left overlay UI with optimized graph sizes
output$left_overlay <- renderUI({
  div(
    style = "padding: 10px;",
    # Area Summary Header
    h4("Area Summary"),
    h5(textOutput("current_region_name", inline = TRUE)),
    
    # Main scrollable content
    div(
      style = "height: calc(100% - 70px); overflow-y: auto; padding-right: 5px;",
      
      # Price Distribution Plot - full size
      div(
        style = "margin-bottom: 25px;",
        plotOutput("summary_plot", height = "400px")
      ),
      # Spacer between plots
      div(style = "height: 18px;"),
      # Income Distribution Plot - full size
      div(
        style = "margin-bottom: 25px;",
        plotlyOutput("income_plot_output", height = "350px")
      ),
      
      # Facility Summary - now more compact
      uiOutput("facility_summary")
    ),
    
    # Price legend at bottom
    div(
      style = "position: absolute; bottom: 10px; left: 15px; right: 15px; height: 60px;",
      htmlOutput("price_legend")
    )
  )
})
