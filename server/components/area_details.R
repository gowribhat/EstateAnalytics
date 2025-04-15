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

# Render the plotly income visualization - changed from donut chart to horizontal stacked bar
output$income_donut <- renderPlotly({
  # Get the current planning area
  area_name <- current_planning_area()

  # Get income data
  income_data <- household_income_data()

  # Exit early if we don't have proper data
  req(income_data, area_name, area_name != "Outside Planning Area")

  # Find income data for current region
  region_data <- income_data %>%
    filter(toupper(Number) == toupper(area_name))

  req(nrow(region_data) > 0)

  # Calculate income metrics
  total_households <- region_data$Total

  # Calculate households in different income brackets
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
  # Keep full labels for hover text
  hover_labels <- c("No Income", "Low Income (<$3K)", "Mid Income ($3-9K)",
                    "High Income ($9-20K)", "Affluent (>$20K)")
  values <- c(no_income_percent, low_income_percent, mid_income_percent,
             high_income_percent, affluent_percent)
  colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")

  # Create a data frame for plot_ly with a single row
  plot_data <- data.frame(
    y = "Income Distribution",
    x = values,
    income_type = factor(legend_labels, levels = legend_labels), # Ensure factor levels are ordered
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
      insidetextanchor = 'middle'
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
        y = -0.1,           # Moved closer to the bar
        x = 0.5,
        xanchor = 'center',
        yanchor = 'top',
        font = list(size = 9),  # Smaller font for legend
        itemsizing = 'constant', # Makes legend items consistent size
        itemwidth = 30,         # Narrower legend items
        traceorder = "normal"
      ),
      yaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE),
      xaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, range = c(0, 100)),
      margin = list(t = 50, b = 30, l = 10, r = 10) # Reduced margins
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
    paste0("Price Distribution in ", area_name)
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
    print("Planning areas columns:")
    print(names(pa_data))
    return(pa_data)
  }, error = function(e) {
    warning("Error loading planning areas: ", e$message)
    NULL
  })
})

# Function to get planning area for facilities using reverse geocoding
get_facilities_in_area <- function(facility_data, planning_areas_sf, area_name) {
  # Debug prints
  print(paste("Processing facilities for area:", area_name))
  print(paste("Number of facilities:", nrow(facility_data)))
  print(paste("Planning areas columns:", paste(names(planning_areas_sf), collapse=", ")))
  
  req(facility_data, planning_areas_sf, area_name)
  
  # Create spatial points for facilities
  facilities_sf <- st_as_sf(facility_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Ensure CRS matches
  if (st_crs(facilities_sf) != st_crs(planning_areas_sf)) {
    facilities_sf <- st_transform(facilities_sf, crs = st_crs(planning_areas_sf))
  }
  
  # Find planning area column dynamically
  pa_col <- names(planning_areas_sf)[grep("planning.*area|pln.*area", names(planning_areas_sf), ignore.case = TRUE)]
  if (length(pa_col) == 0) pa_col <- "planning_area"
  print(paste("Using planning area column:", pa_col))
  
  # Find facilities in the current planning area (case insensitive)
  current_area <- planning_areas_sf[toupper(planning_areas_sf[[pa_col]]) == toupper(area_name), ]
  if (nrow(current_area) == 0) {
    print(paste("No matching area found for:", area_name))
    print(paste("Available areas:", paste(unique(planning_areas_sf[[pa_col]]), collapse=", ")))
    return(0)
  }
  
  # Temporary disable S2 for consistent results
  sf_use_s2(FALSE)
  facilities_in_area <- st_intersects(facilities_sf, current_area, sparse = FALSE)
  sf_use_s2(TRUE)
  
  count <- sum(facilities_in_area)
  print(paste("Found facilities in area:", count))
  return(count)
}

# Add facility summary count to the UI
output$facility_summary <- renderUI({
  # Get current planning area
  area_name <- current_planning_area()
  
  # Get planning areas data
  planning_areas_sf <- planning_areas_data()
  
  # Enhanced debugging - print more detailed information
  print(paste("Current area:", area_name))
  print(paste("Planning areas loaded:", !is.null(planning_areas_sf)))
  if (!is.null(planning_areas_sf)) {
    print(paste("Number of planning areas:", nrow(planning_areas_sf)))
    print(paste("Planning area column names:", paste(names(planning_areas_sf), collapse=", ")))
  }
  
  # Exit if no area selected or no planning areas data
  if (is.null(area_name) || area_name == "Outside Planning Area" || is.null(planning_areas_sf)) {
    return(div(
      style = "margin: 15px 0; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
      "No facility data available for this area"
    ))
  }
  
  # Validate planning areas data
  if (!inherits(planning_areas_sf, "sf")) {
    print("Warning: planning_areas_sf is not an sf object")
    return(div(
      style = "margin: 15px 0; padding: 15px; background-color: #fff3cd; border-radius: 5px;",
      "Error loading planning areas data"
    ))
  }
  
  # Ensure geometry is valid
  if (any(!st_is_valid(planning_areas_sf))) {
    print("Fixing invalid geometries in planning areas...")
    planning_areas_sf <- st_make_valid(planning_areas_sf)
  }
  
  # Get facility counts with error handling
  tryCatch({
    # Load facility data first to ensure it's available
    childcare_data <- childcare()
    gym_data <- gym()
    mrt_data <- mrt()
    park_data <- park()
    school_data <- sch()
    mart_data <- mart()
    
    # Verify facility data is loaded
    req(childcare_data, gym_data, mrt_data, park_data, school_data, mart_data)
    
    # Get counts
    childcare_count <- get_facilities_in_area(childcare_data, planning_areas_sf, area_name)
    gym_count <- get_facilities_in_area(gym_data, planning_areas_sf, area_name)
    mrt_count <- get_facilities_in_area(mrt_data, planning_areas_sf, area_name)
    park_count <- get_facilities_in_area(park_data, planning_areas_sf, area_name)
    school_count <- get_facilities_in_area(school_data, planning_areas_sf, area_name)
    supermarket_count <- get_facilities_in_area(mart_data, planning_areas_sf, area_name)
    
    div(
      style = "margin: 15px 0; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
      h4("Facilities in Area", style = "margin-top: 0; margin-bottom: 15px; font-size: 16px; color: #2c3e50;"),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; font-size: 14px;",
        div(style = "padding: 8px; background: #fff; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
            HTML(sprintf("<strong>🏫 Schools:</strong> %d", school_count))),
        div(style = "padding: 8px; background: #fff; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
            HTML(sprintf("<strong>👶 Childcare:</strong> %d", childcare_count))),
        div(style = "padding: 8px; background: #fff; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
            HTML(sprintf("<strong>🚉 MRT/LRT:</strong> %d", mrt_count))),
        div(style = "padding: 8px; background: #fff; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
            HTML(sprintf("<strong>🏋️ Gyms:</strong> %d", gym_count))),
        div(style = "padding: 8px; background: #fff; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
            HTML(sprintf("<strong>🌳 Parks:</strong> %d", park_count))),
        div(style = "padding: 8px; background: #fff; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
            HTML(sprintf("<strong>🛒 Supermarkets:</strong> %d", supermarket_count)))
      )
    )
  }, error = function(e) {
    print(paste("Error counting facilities:", e$message))
    # Return error message if something goes wrong
    div(
      style = "margin: 15px 0; padding: 15px; background-color: #fff3cd; border-radius: 5px; color: #856404;",
      "Unable to load facility data"
    )
  })
})

# Update the left overlay UI to restore original graph sizes
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
      
      # Income Distribution Plot - full size
      div(
        style = "margin-bottom: 25px;",
        plotlyOutput("income_donut", height = "350px")
      ),
      
      # Facility Summary
      uiOutput("facility_summary")
    ),
    
    # Price legend at bottom
    div(
      style = "position: absolute; bottom: 10px; left: 15px; right: 15px; height: 60px;",
      htmlOutput("price_legend")
    )
  )
})
