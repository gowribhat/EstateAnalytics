# Find Your New Home: Data-Driven Property Search Platform
# Server File
# DBA3702 Team 3

server <- function(input, output, session) {

  # --- Overlay Visibility Handling ---
  # This observes the input value set by our JavaScript to track overlay visibility
  observeEvent(input$overlays_visible, {
    # You can add additional logic here if needed based on overlay visibility
    # For example, adjusting the map size or other UI elements
  }, ignoreInit = TRUE)
  
  # --- Transactions Overlay Handler ---
  # Toggle transactions overlay when button is clicked
  observeEvent(input$toggle_transactions_overlay, {
    # Toggle the transaction overlay visibility status
    transactions_overlay_visible(!transactions_overlay_visible())
    
    # Use JavaScript to actually show/hide the transactions overlay
    if(transactions_overlay_visible()) {
      # Show the overlay
      session$sendCustomMessage("showTransactionsOverlay", list())
    } else {
      # Hide the overlay
      session$sendCustomMessage("hideTransactionsOverlay", list())
    }
  })
  
  # Handle force refresh requests from JavaScript
  observeEvent(input$force_dt_refresh, {
    # Only force refresh if overlay is visible
    if(transactions_overlay_visible()) {
      # Just triggering this reactive will cause the table to redraw
      building_data <- building_transactions()
      
      # Explicitly send a refresh command to the JavaScript
      session$sendCustomMessage("refreshDataTable", list(tableId = "building_transactions"))
    }
  }, ignoreInit = TRUE)

  # --- Selected Building Tracking ---
  # Reactive value to store selected building information
  selected_building <- reactiveVal(NULL)
  
  # Reactive value to track whether transactions overlay is visible
  transactions_overlay_visible <- reactiveVal(FALSE)

  # --- Load Data ---
  # Load planning areas data reactively or on startup
  # Using reactiveVal for potential future dynamic loading/updates
  planning_areas_data <- reactiveVal(NULL)
  observe({
    planning_areas_data(loadData("planning_areas"))
  })
  
  # Load HDB resale data 
  hdb_data <- reactiveVal(NULL)
  observe({
    # Load RDS file for better performance
    tryCatch({
      data <- readRDS(paste0(resources_path, "hdb.rds"))
      hdb_data(data)
    }, error = function(e) {
      # Fallback to CSV if RDS not available
      data <- loadData("hdb_resale")
      hdb_data(data)
    })
  })
  
  # Load household income data
  household_income_data <- reactiveVal(NULL)
  observe({
    tryCatch({
      data <- readRDS(paste0(resources_path, "household_income_data.rds"))
      household_income_data(data)
    }, error = function(e) {
      # Log error but continue app execution
      message("Error loading household income data: ", e$message)
    })
  })
  
  # Load URA private transaction data
  ura_data <- reactiveVal(NULL)
  observe({
    # Load RDS file for better performance
    tryCatch({
      data <- readRDS(paste0(resources_path, "ura_private.rds"))
      ura_data(data)
    }, error = function(e) {
      # Fallback to CSV if RDS not available
      data <- loadData("ura_private")
      ura_data(data)
    })
  })
  
  # Property type selection (HDB or Private)
  selected_property_type <- reactiveVal("HDB")
  
  observeEvent(input$ok_house_type, {
    selected_property_type(input$modal_house_type)
    # Reset selected building when house type changes to prevent errors
    selected_building(NULL)
  })
  
  # Define filter reactive values
  budget_range <- reactiveVal(c(100000, 10000000))
  floor_range <- reactiveVal(c(1, 50))
  area_range <- reactiveVal(c(0, 1000))
  
  # Update filter reactive values when modals are confirmed
  observeEvent(input$ok_budget, {
    budget_range(input$modal_budget)
  })
  
  observeEvent(input$ok_floor_height, {
    floor_range(input$modal_floor_height) 
  })
  
  observeEvent(input$ok_area, {
    area_range(input$modal_area)
  })

  # Filtered HDB data based on selected filters
  filtered_hdb_data <- reactive({
    data <- hdb_data()
    req(data)
    
    # Extract floor level from storey_range (e.g., "07 TO 09")
    data$floor_low <- data$storey_range_min
    data$floor_high <- data$storey_range_max
    
    # Get current filter values
    budget_min <- budget_range()[1]
    budget_max <- budget_range()[2]
    floor_min <- floor_range()[1]
    floor_high <- floor_range()[2]
    area_min <- area_range()[1]
    area_max <- area_range()[2]
    
    # Apply filters
    filtered <- data %>%
      filter(
        resale_price >= budget_min,
        resale_price <= budget_max,
        floor_low >= floor_min,
        floor_high <= floor_high,
        floor_area_sqm >= area_min,
        floor_area_sqm <= area_max
      )
    
    return(filtered)
  })
  
  # Filtered URA private property data based on selected filters
  filtered_ura_data <- reactive({
    data <- ura_data()
    req(data)
    
    # Get current filter values
    budget_min <- budget_range()[1]
    budget_max <- budget_range()[2]
    floor_min <- floor_range()[1]
    floor_high <- floor_range()[2]
    area_min <- area_range()[1]
    area_max <- area_range()[2]
    
    # Apply filters - using URA column names
    filtered <- data %>%
      filter(
        price >= budget_min,
        price <= budget_max,
        floorRange_min >= floor_min,
        floorRange_max <= floor_high,
        area >= area_min,
        area <= area_max
      )
    
    return(filtered)
  })

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

  # --- Update UI Elements ---
  # Update the region name text
  output$current_region_name <- renderText({
    current_planning_area()
  })

  # Income statistics for the current region
  output$income_stats <- renderUI({
    # Get the current planning area
    area_name <- current_planning_area()
    
    # Get income data
    income_data <- household_income_data()
    
    # If we don't have income data or a planning area, display a message
    if (is.null(income_data) || is.null(area_name) || area_name == "Outside Planning Area") {
      return(HTML("<p><em>Income data not available for this area.</em></p>"))
    }
    
    # Find income data for current region
    region_data <- income_data %>%
      filter(toupper(Number) == toupper(area_name))
    
    if (nrow(region_data) == 0) {
      return(HTML("<p><em>Income data not available for this area.</em></p>"))
    }
    
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
    
    # Create data for plotly donut chart
    labels <- c("No Income", "Low Income (<$3K)", "Mid Income ($3-9K)", 
               "High Income ($9-20K)", "Affluent (>$20K)")
    values <- c(no_income_percent, low_income_percent, mid_income_percent, 
               high_income_percent, affluent_percent)
    colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
    
    # Create donut chart using plotly
    donut_chart <- plot_ly(
      labels = labels,
      values = values,
      type = 'pie',
      hole = 0.6,
      marker = list(colors = colors),
      textinfo = 'label+percent',
      hoverinfo = 'label+value+percent',
      textposition = 'outside',
      insidetextorientation = 'radial'
    ) %>%
    layout(
      title = list(
        text = paste0('<b>Household Income Profile</b><br>',
                     '<span style="font-size: 12px;">Total Households: ', 
                     format(total_households, big.mark=","), '</span>'),
        font = list(size = 14)
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.2),
      margin = list(t = 80, b = 10, l = 10, r = 10)
    )
    
    # Return a div containing the plotly chart with appropriate sizing
    div(
      style = "height:300px; background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
      plotlyOutput(session$ns("income_donut"), height = "100%")
    )
  })
  
  # Render the plotly donut chart
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
    
    # Create data for plotly donut chart with simplified labels for the legend
    legend_labels <- c("No Income", "<$3K", "$3-9K", "$9-20K", ">$20K")
    # Keep full labels for hover text
    hover_labels <- c("No Income", "Low Income (<$3K)", "Mid Income ($3-9K)", 
                      "High Income ($9-20K)", "Affluent (>$20K)")
    values <- c(no_income_percent, low_income_percent, mid_income_percent, 
               high_income_percent, affluent_percent)
    colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
    
    # Create a data frame for plot_ly
    plot_data <- data.frame(
      legend_label = legend_labels,
      value = values,
      hover_label = hover_labels,
      stringsAsFactors = FALSE
    )
    
    # Create donut chart using plotly
    plot_ly(
      data = plot_data, # Pass the data frame
      labels = ~legend_label, # Use simplified labels for the legend from the data frame
      values = ~value,        # Use values from the data frame
      customdata = ~hover_label, # Store full labels for hover from the data frame
      type = 'pie',
      hole = 0.6,
      marker = list(colors = colors),
      textinfo = 'none', # Remove labels from slices
      hoverinfo = 'text', # Use custom hover text
      text = ~paste(hover_label, paste0(value, "%")), # Format hover text using columns
      insidetextorientation = 'radial' 
    ) %>%
    layout(
      title = list(
        text = paste0('<b>Household Income Profile</b><br>',
                     '<span style="font-size: 12px;">Total Households: ', 
                     format(total_households, big.mark=","), '</span>'),
        font = list(size = 14)
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h", # Horizontal legend
        y = -0.1,          # Position below the chart
        x = 0.5,           # Center the legend horizontally
        xanchor = 'center',
        yanchor = 'top'
      ),
      margin = list(t = 80, b = 40, l = 10, r = 10) # Adjust bottom margin for legend
    )
  })

  # Reactive dataset for the area summary plot
  visible_transactions <- reactive({
    data <- filtered_hdb_data()
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
    
    # Create a density plot
    ggplot(data, aes(x = resale_price)) +
      geom_density(fill = "#4676a9", alpha = 0.7) +
      geom_vline(aes(xintercept = median(resale_price)), 
                color = "#ff5555", linetype = "dashed", size = 1) +
      labs(
        title = plot_title, # Use the conditional title
        subtitle = paste0("Median: $", format(median(data$resale_price), big.mark = ",")),
        x = "Price (SGD)",
        y = "Density"
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
  
  # Right overlay: Building-specific data and visualization
  # Get transactions for specific building when selected
  building_transactions <- reactive({
    building <- selected_building()
    property_type <- selected_property_type()
    
    # If no building is selected, return NULL
    if (is.null(building)) {
      return(NULL)
    }
    
    # Get appropriate dataset
    if (property_type == "HDB") {
      data <- filtered_hdb_data()
      req(data)
      
      # Extract block and street name from selected building
      # Filter data for the specific building
      building_data <- data %>%
        filter(
          block == building$block,
          street_name == building$street_name
        ) %>%
        arrange(desc(month))
      
    } else {
      data <- filtered_ura_data()
      req(data)
      
      # Filter data for the specific building/project
      building_data <- data %>%
        filter(
          project == building$project,
          street == building$street
        ) %>%
        arrange(desc(contractDate)
      )
    }
    
    return(building_data)
  })
  
  # Building-specific details for the right overlay
  output$property_details <- renderUI({
    building <- selected_building()
    building_data <- building_transactions()
    
    # If no building is selected or no data is available, show a prompt message (without the button)
    if (is.null(building) || is.null(building_data) || nrow(building_data) == 0) {
      return(HTML("<p>Click on a property marker to see details.</p>"))
    }
    
    property_type <- selected_property_type()
    
    if (property_type == "HDB") {
      # Calculate stats
      median_price <- median(building_data$resale_price)
      recent_price <- building_data$resale_price[1]
      total_transactions <- nrow(building_data)
      year_built <- building_data$lease_commence_date[1]
      
      # Include the Past Transactions button only when a building is selected
      HTML(paste0(
        "<div style='font-size: 18px; font-weight: bold;'>", building$block, " ", building$street_name, "</div>",
        "<div style='margin-top: 10px;'>",
        "<div><strong>Latest Price:</strong> $", format(recent_price, big.mark = ","), " (", format(building_data$month[1], "%b %Y"), ")</div>",
        "<div><strong>Median Price:</strong> $", format(median_price, big.mark = ","), "</div>",
        "<div><strong>Transactions:</strong> ", total_transactions, " sales</div>",
        "<div><strong>Built:</strong> ", year_built, "</div>",
        "<div><strong>Flat Type:</strong> ", building_data$flat_type[1], "</div>",
        "<div><strong>Area Range:</strong> ", min(building_data$floor_area_sqm), " - ", max(building_data$floor_area_sqm), " sqm</div>",
        "<div style='margin-top: 15px;'>",
        "<button id='toggle_transactions_overlay' type='button' class='btn btn-primary btn-block action-button'>Past Transactions</button>",
        "</div>",
        "</div>"
      ))
    } else {
      # Calculate stats for private properties
      median_price <- median(building_data$price)
      recent_price <- building_data$price[1]
      total_transactions <- nrow(building_data)
      
      # Include the Past Transactions button only when a building is selected
      HTML(paste0(
        "<div style='font-size: 18px; font-weight: bold;'>", building$project, "</div>",
        "<div style='margin-top: 10px;'>",
        "<div><strong>Address:</strong> ", building$street, "</div>",
        "<div><strong>Latest Price:</strong> $", format(recent_price, big.mark = ","), " (", format(building_data$contractDate[1], "%b %Y"), ")</div>",
        "<div><strong>Median Price:</strong> $", format(median_price, big.mark = ","), "</div>",
        "<div><strong>Transactions:</strong> ", total_transactions, " sales</div>",
        "<div><strong>Property Type:</strong> ", building_data$propertyType[1], "</div>",
        "<div><strong>Area Range:</strong> ", min(building_data$area), " - ", max(building_data$area), " sqm</div>",
        "<div><strong>Tenure:</strong> ", building_data$tenure[1], "</div>",
        "<div style='margin-top: 15px;'>",
        "<button id='toggle_transactions_overlay' type='button' class='btn btn-primary btn-block action-button'>Past Transactions</button>",
        "</div>",
        "</div>"
      ))
    }
  })
  
  # Building-specific price density plot
  output$building_plot <- renderPlot({
    # Add tryCatch to gracefully handle errors
    tryCatch({
      # Only proceed if we have a selected building
      building <- selected_building()
      if(is.null(building)) {
        # Return a placeholder plot when no building is selected
        return(
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "Click on a marker to view price distribution", size = 5) + 
            theme_void() +
            theme(
              plot.background = element_rect(fill = "#f8f9fa", color = NA)
            )
        )
      }
      
      # Get building-specific transactions
      building_data <- building_transactions()
      if(is.null(building_data) || nrow(building_data) == 0) {
        # Return a placeholder plot when no transactions are found
        return(
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No transactions available for this building", size = 5) + 
            theme_void() +
            theme(
              plot.background = element_rect(fill = "#f8f9fa", color = NA)
            )
        )
      }
      
      property_type <- selected_property_type()
      
      # Plot title
      if (property_type == "HDB") {
        plot_title <- paste0(building$block, " ", building$street_name)
        price_col <- building_data$resale_price
      } else {
        plot_title <- paste0(building$project, " - ", building$street)
        price_col <- building_data$price
      }
      
      # Only show density plot if we have enough data points
      if (nrow(building_data) >= 3) {
        p <- ggplot(building_data, aes(x = price_col)) +
          geom_density(fill = "#4676a9", alpha = 0.7) +
          geom_vline(aes(xintercept = median(price_col)), 
                    color = "#ff5555", linetype = "dashed", size = 1) +
          labs(
            title = paste0("Price Distribution for ", plot_title),
            subtitle = paste0("Median: $", format(median(price_col), big.mark = ",")),
            x = "Price (SGD)",
            y = "Density"
          ) +
          scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          )
      } else {
        # For buildings with too few transactions, show a histogram or box plot
        p <- ggplot(building_data, aes(y = price_col)) +
          geom_boxplot(fill = "#4676a9", alpha = 0.7) +
          labs(
            title = paste0("Price Distribution for ", plot_title),
            subtitle = paste0("Median: $", format(median(price_col), big.mark = ",")),
            x = "",
            y = "Price (SGD)"
          ) +
          scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 10),
            panel.grid.minor = element_blank()
          )
      }
      
      return(p)
    }, error = function(e) {
      # Return a placeholder plot on error
      return(
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Select a property to view data", size = 5) + 
          theme_void() +
          theme(
            plot.background = element_rect(fill = "#f8f9fa", color = NA)
          )
      )
    })
  })
  
  # Building-specific transaction list for right overlay
  output$building_transactions <- renderDT({
    # Add tryCatch to gracefully handle errors
    tryCatch({
      # Get building-specific transactions
      building_data <- building_transactions()
      
      # Check if we have data to display
      if(is.null(building_data) || nrow(building_data) == 0) {
        # Return empty data frame with appropriate columns
        property_type <- selected_property_type()
        if(property_type == "HDB") {
          return(data.frame(
            Date = character(),
            Price = character(),
            Type = character(),
            Floor = character(),
            Area = character(),
            stringsAsFactors = FALSE
          ))
        } else {
          return(data.frame(
            Date = character(),
            Price = character(),
            Type = character(),
            Floor = character(),
            Area = character(),
            Tenure = character(),
            stringsAsFactors = FALSE
          ))
        }
      }
      
      property_type <- selected_property_type()
      
      if (property_type == "HDB") {
        # Format the data for the table
        result <- building_data %>%
          select(month, resale_price, flat_type, storey_range, floor_area_sqm) %>%
          rename(
            Date = month,
            Price = resale_price,
            Type = flat_type,
            Floor = storey_range,
            Area = floor_area_sqm
          )
        
        # Format price with commas
        result$Price <- paste0("$", format(result$Price, big.mark = ","))
        # Add "sqm" to area column
        result$Area <- paste(result$Area, "sqm")
        
      } else {
        # Format the data for the table for URA
        result <- building_data %>%
          select(contractDate, price, propertyType, floorRange, area, tenure) %>%
          rename(
            Date = contractDate,
            Price = price,
            Type = propertyType,
            Floor = floorRange,
            Area = area,
            Tenure = tenure
          )
        
        # Format price with commas
        result$Price <- paste0("$", format(result$Price, big.mark = ","))
        # Add "sqm" to area column
        result$Area <- paste(result$Area, "sqm")
      }
      
      return(result)
    }, error = function(e) {
      # On error, return an empty data frame with appropriate message
      return(data.frame(
        Message = "Click on a property marker to view transaction history",
        stringsAsFactors = FALSE
      ))
    })
  }, options = list(pageLength = 10, searching = TRUE, lengthChange = TRUE, scrollY = "calc(100% - 100px)", language = list(
    emptyTable = "No transaction history available for this property"
  )))

  # --- Modal Dialog Logic ---

  # Observe House Type button click
  observeEvent(input$filter_house_type, {
    showModal(modalDialog(
      title = "Select House Type",
      selectInput("modal_house_type", "Property Type:", choices = c("HDB", "Condominium"), selected = "Condominium"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_house_type", "OK")
      )
    ))
  })

  # Observe Budget button click
  observeEvent(input$filter_budget, {
    showModal(modalDialog(
      title = "Select Budget Range",
      sliderInput("modal_budget", "Budget (SGD):", min = 100000, max = 1000000000, value = c(500000, 1500000), step = 50000, pre = "$", sep = ","),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_budget", "OK")
      )
    ))
  })

  # Observe Area button click
  observeEvent(input$filter_area, {
    showModal(modalDialog(
      title = "Select Area Range",
      sliderInput("modal_area", "Area (sqm):", min = 0, max = 1000, value = c(80, 150), step = 10),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_area", "OK")
      )
    ))
  })

  # Observe Floor Height button click
  observeEvent(input$filter_floor_height, {
    showModal(modalDialog(
      title = "Select Floor Height Range",
      sliderInput("modal_floor_height", "Floor Level:", min = 0, max = 50, value = c(1, 20), step = 1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_floor_height", "OK")
      )
    ))
  })

  # Observe Facility button click
  observeEvent(input$filter_facility, {
    showModal(modalDialog(
      title = "Select Nearby Facilities (Priority)",
      # Using checkboxes as a simpler alternative to drag-and-drop
      checkboxGroupInput("modal_facility", "Select desired facilities:",
                         choices = c("Subway", "Hospital", "Supermarket", "Food Court"),
                         selected = c("Subway", "Supermarket")),
      # Add logic here later to handle priority if needed, or use a different input type
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_facility", "OK")
      )
    ))
  })

  # --- Update Button Labels on Modal OK ---

  observeEvent(input$ok_house_type, {
    updateActionButton(session, "filter_house_type", label = paste("Type:", input$modal_house_type))
    removeModal()
  })

  observeEvent(input$ok_budget, {
    # Format budget nicely (e.g., $500K - $1.5M)
    min_budget <- format(input$modal_budget[1], big.mark = ",", scientific = FALSE)
    max_budget <- format(input$modal_budget[2], big.mark = ",", scientific = FALSE)
    label_text <- paste0("Budget: $", min_budget, " - $", max_budget)
    # Abbreviate if too long? (Optional)
    # label_text <- paste0("Budget: $", round(input$modal_budget[1]/1e6,1),"M - $", round(input$modal_budget[2]/1e6,1),"M")
    updateActionButton(session, "filter_budget", label = label_text)
    removeModal()
  })

  observeEvent(input$ok_area, {
    label_text <- paste("Area:", input$modal_area[1], "-", input$modal_area[2], "sqm")
    updateActionButton(session, "filter_area", label = label_text)
    removeModal()
  })

  observeEvent(input$ok_floor_height, {
    label_text <- paste("Floor:", input$modal_floor_height[1], "-", input$modal_floor_height[2])
    updateActionButton(session, "filter_floor_height", label = label_text)
    removeModal()
  })

  observeEvent(input$ok_facility, {
    # Show number of facilities selected or list them if short
    num_selected <- length(input$modal_facility)
    label_text <- if (num_selected > 0) {
      paste("Facilities:", num_selected, "selected")
      # Alternative: paste("Facilities:", paste(input$modal_facility, collapse=", ")) # Could get long
    } else {
      "Facility" # Reset to default if none selected
    }
    updateActionButton(session, "filter_facility", label = label_text)
    removeModal()
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
  
  # --- Transactions Overlay Close Button ---
  observeEvent(input$close_transactions, {
    # Set reactive value to track state
    transactions_overlay_visible(FALSE)
    
    # Hide the overlay
    session$sendCustomMessage("hideTransactionsOverlay", list())
  })

  # --- Marker Display Logic ---
  
  # Get current zoom level
  current_zoom <- reactiveVal(sg_zoom) # Default zoom
  observeEvent(input$property_map_zoom, {
    current_zoom(input$property_map_zoom)
  })
  
  # Cache for marker data to avoid redundant processing
  marker_cache <- reactiveVal(NULL)
  
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
    }
  })
  
  # Debugging: Check if planning_areas_data is loaded
  observe({
    if (is.null(planning_areas_data())) {
      print("Planning areas data is not loaded.")
    } else {
      print("Planning areas data loaded successfully.")
    }
  })

  # Debugging: Check if map is rendering
  observe({
    if (is.null(input$property_map_center)) {
      print("Map center input is not available.")
    } else {
      print(paste("Map center coordinates:", input$property_map_center$lng, input$property_map_center$lat))
    }
  })

  # Debugging: Print column names of planning areas data
  observe({
    pa_sf <- planning_areas_data()
    if (!is.null(pa_sf)) {
      print("Column names in planning areas data:")
      print(names(pa_sf))
    }
  })

  # --- Debugging: Print spatial intersection result ---
  observe({
    center <- map_center_debounced()
    pa_sf <- planning_areas_data()

    if (!is.null(center) && !is.null(pa_sf)) {
      center_point <- st_sfc(st_point(c(center$lng, center$lat)), crs = 4326)
      pa_crs <- st_crs(pa_sf)
      if (st_crs(center_point) != pa_crs) {
        center_point <- st_transform(center_point, crs = pa_crs)
      }

      sf_use_s2(FALSE)
      intersection <- st_intersects(center_point, pa_sf, sparse = FALSE)
      sf_use_s2(TRUE)

      print("Spatial intersection result:")
      print(intersection)
    }
  })
}