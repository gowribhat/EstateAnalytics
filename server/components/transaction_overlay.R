# Building Analytics Dashboard
# This script manages the visibility and interactions of the analytics dashboard overlay.
# It includes logic for toggling the dashboard, handling user interactions, and generating visualizations.
# Key components:
# - Analytics dashboard: Displays interactive visualizations for selected buildings.
# - Overlay visibility: Tracks and updates the visibility state of overlays.
# - JavaScript integration: Sends custom messages to the frontend for overlay actions.

# --- Overlay Visibility Handling ---
# This observes the input value set by our JavaScript to track overlay visibility
observeEvent(input$overlays_visible, {
  # You can add additional logic here if needed based on overlay visibility
  # For example, adjusting the map size or other UI elements
}, ignoreInit = TRUE)

# Duplicate of building_transactions reactive from building_details.R
# This is needed because the original is in a different local environment
get_transaction_data <- reactive({
  building <- selected_building()
  property_type <- selected_property_type()

  # If no building is selected, return NULL
  if (is.null(building)) {
    return(NULL)
  }

  # Get appropriate dataset
  if (property_type == "HDB") {
    data <- filtered_hdb_data() # Assumes filtered_hdb_data is defined elsewhere
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
    data <- filtered_ura_data() # Assumes filtered_ura_data is defined elsewhere
    req(data)

    # Filter data for the specific building/project
    building_data <- data %>%
      filter(
        project == building$project,
        street == building$street
      ) %>%
      arrange(desc(contractDate))
  }

  return(building_data)
})

# --- Analytics Dashboard Overlay Handler ---
# Toggle analytics dashboard overlay when button is clicked
observeEvent(input$toggle_transactions_overlay, {
  # Toggle the dashboard overlay visibility status
  current_visibility <- !transactions_overlay_visible()
  transactions_overlay_visible(current_visibility)

  # Use JavaScript to actually show/hide the dashboard overlay
  if(current_visibility) {
    # Show the overlay
    session$sendCustomMessage("showTransactionsOverlay", list())
  } else {
    # Hide the overlay
    session$sendCustomMessage("hideTransactionsOverlay", list())
  }
})

# Handler for refreshing the analytics dashboard from JavaScript
observeEvent(input$refresh_analytics_dashboard, {
  # Only proceed if the dashboard is supposed to be visible
  req(transactions_overlay_visible())
  
  # Force reactivity by invalidating the context
  invalidateLater(10)
})

# Render the analytics dashboard content
output$analytics_dashboard <- renderUI({
  # First check if the overlay is visible - this prevents data processing when hidden
  req(transactions_overlay_visible())
  
  # Then check for a selected building
  req(selected_building())
  
  # Get building data
  building_data <- get_transaction_data()
  
  # Check if we have data to display
  req(building_data)
  req(nrow(building_data) > 0)
  
  # Format data based on property type
  property_type <- selected_property_type()
  
  # Determine column names based on property type
  if(property_type == "HDB") {
    date_col <- "month"
    price_col <- "resale_price"
    area_col <- "floor_area_sqm"
    floor_col <- "storey_range"
    type_col <- "flat_type"
  } else {
    date_col <- "contractDate"
    price_col <- "price"
    area_col <- "area"
    floor_col <- "floorRange"
    type_col <- "propertyType"
  }
  
  # --- Start: Added Sorting for Room Types ---
  # Get unique room types
  unique_types <- unique(building_data[[type_col]])

  # Sort room types using mixedsort (requires gtools package)
  # Ensure gtools is listed in packages.R and loaded in global.R
  if (requireNamespace("gtools", quietly = TRUE)) {
    sorted_types <- gtools::mixedsort(as.character(unique_types))
  } else {
    # Fallback to basic sort if gtools is not available
    warning("gtools package not found. Room types may not be sorted naturally.")
    sorted_types <- sort(as.character(unique_types))
  }
  # --- End: Added Sorting for Room Types ---

  # Create UI elements for the dashboard
  fluidRow(
    column(
      width = 4,
      div(
        style = "height: 100%; overflow-y: auto; overflow-x: hidden; max-height: 500px;", # Only vertical scrolling with fixed height
        wellPanel(
          style = "background-color: #f8f9fa;",
          h4("Analytics Controls"),
          # Visualization type selector
          radioButtons("viz_type", "Visualization Type:", 
                      choices = c("Price Trend" = "price_trend", 
                                "Price per SQM" = "price_per_sqm",
                                "Transaction Volume" = "transaction_volume"),
                      selected = "price_trend"),
          
          # Room type filter (if applicable)
          conditionalPanel(
            condition = "input.viz_type == 'price_trend' || input.viz_type == 'price_per_sqm'",
            selectInput("room_type_filter", "Filter by Room Type:",
                      # --- Start: Use sorted list for choices ---
                      choices = c("All Types" = "all", setNames(sorted_types, sorted_types)),
                      # --- End: Use sorted list for choices ---
                      selected = "all")
          ),
          
          # Time period filter
          conditionalPanel(
            condition = "input.viz_type == 'price_trend' || input.viz_type == 'price_per_sqm'",
            sliderInput("time_period", "Time Period:",
                      min = min(year(building_data[[date_col]])),
                      max = max(year(building_data[[date_col]])),
                      value = c(min(year(building_data[[date_col]])), 
                                max(year(building_data[[date_col]]))),
                      step = 1,
                      sep = "")  # Remove comma separator for year values
          ),
          
          # Floor range filter - sorted from low to high floors with all floors selected by default
          conditionalPanel(
            condition = "input.viz_type == 'price_per_sqm'",
            checkboxGroupInput("floor_range", "Floor Range:",
                            choices = sort_floor_ranges(unique(building_data[[floor_col]])),
                            selected = sort_floor_ranges(unique(building_data[[floor_col]])))
          )
        )
      )
    ),
    column(
      width = 8,
      div(
        style = "height: 400px; overflow: hidden;", # Prevent all scrolling with fixed height
        # Show different plots based on viz_type
        conditionalPanel(
          condition = "input.viz_type == 'price_trend'",
          plotOutput("price_trend_plot", height = "250px")
        ),
        conditionalPanel(
          condition = "input.viz_type == 'price_per_sqm'",
          plotOutput("price_per_sqm_plot", height = "250px")
        ),
        conditionalPanel(
          condition = "input.viz_type == 'transaction_volume'",
          plotOutput("transaction_volume_plot", height = "250px")
        )
      )
    )
  )
})

# Price Trend Plot
output$price_trend_plot <- renderPlot({
  # Get inputs
  req(input$viz_type == "price_trend")
  req(transactions_overlay_visible())
  
  building_data <- get_transaction_data()
  req(building_data)
  
  property_type <- selected_property_type()
  
  # Determine column names based on property type
  if(property_type == "HDB") {
    date_col <- "month"
    price_col <- "resale_price"
    type_col <- "flat_type"
  } else {
    date_col <- "contractDate"
    price_col <- "price"
    type_col <- "propertyType"
  }
  
  # Filter by room type if not "all"
  if(input$room_type_filter != "all") {
    building_data <- building_data %>%
      filter(!!sym(type_col) == input$room_type_filter)
  }
  
  # Filter by time period
  building_data <- building_data %>%
    filter(year(!!sym(date_col)) >= input$time_period[1],
           year(!!sym(date_col)) <= input$time_period[2])
  
  # Check if we have data after filtering
  if(nrow(building_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data for selected filters", size = 5) +
             theme_void())
  }
  
  # Convert date to proper format for plotting
  building_data <- building_data %>%
    mutate(plot_date = as.Date(!!sym(date_col)))
  
  # Create price trend plot
  ggplot(building_data, aes(x = plot_date, y = !!sym(price_col))) +
    geom_point(aes(color = !!sym(type_col)), alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    labs(
      title = "Price Trend Over Time",
      x = "Date",
      y = "Transaction Price (SGD)",
      color = "Unit Type"
    ) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
})

# Price Per SQM Plot
output$price_per_sqm_plot <- renderPlot({
  # Get inputs
  req(input$viz_type == "price_per_sqm")
  req(transactions_overlay_visible())
  
  building_data <- get_transaction_data()
  req(building_data)
  
  property_type <- selected_property_type()
  
  # Determine column names based on property type
  if(property_type == "HDB") {
    date_col <- "month"
    price_col <- "resale_price"
    area_col <- "floor_area_sqm"
    floor_col <- "storey_range"
    type_col <- "flat_type"
  } else {
    date_col <- "contractDate"
    price_col <- "price"
    area_col <- "area"
    floor_col <- "floorRange"
    type_col <- "propertyType"
  }
  
  # Filter by room type if not "all"
  if(input$room_type_filter != "all") {
    building_data <- building_data %>%
      filter(!!sym(type_col) == input$room_type_filter)
  }
  
  # Filter by time period
  building_data <- building_data %>%
    filter(year(!!sym(date_col)) >= input$time_period[1],
           year(!!sym(date_col)) <= input$time_period[2])
  
  # Filter by floor range if selected
  if(!is.null(input$floor_range) && length(input$floor_range) > 0) {
    building_data <- building_data %>%
      filter(!!sym(floor_col) %in% input$floor_range)
  }
  
  # Check if we have data after filtering
  if(nrow(building_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data for selected filters", size = 5) +
             theme_void())
  }
  
  # Create price per sqm column
  building_data <- building_data %>%
    mutate(price_per_sqm = !!sym(price_col) / !!sym(area_col),
           plot_date = as.Date(!!sym(date_col)))
  
  # Create price per sqm plot
  ggplot(building_data, aes(x = plot_date, y = price_per_sqm)) +
    geom_point(aes(color = !!sym(floor_col)), alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    labs(
      title = "Price per Square Meter Over Time",
      x = "Date",
      y = "Price per SQM (SGD)",
      color = "Floor Range"
    ) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
})

# Transaction Volume Plot with Market Insights
output$transaction_volume_plot <- renderPlot({
  # Get inputs
  req(input$viz_type == "transaction_volume")
  req(transactions_overlay_visible())
  
  building_data <- get_transaction_data()
  req(building_data)
  
  property_type <- selected_property_type()
  
  # Determine column names based on property type
  if(property_type == "HDB") {
    date_col <- "month"
    price_col <- "resale_price"
  } else {
    date_col <- "contractDate"
    price_col <- "price"
  }
  
  # Check if we have data initially
  req(nrow(building_data) > 0) # Use req for cleaner flow

  # --- Start: Modified Data Preparation for Volume Plot ---
  # Process data for transaction volume analysis
  volume_data_raw <- building_data %>%
    mutate(
      transaction_date = as.Date(!!sym(date_col)),
      transaction_year = year(transaction_date)
    ) %>%
    group_by(transaction_year) %>%
    summarize(
      volume = n(),
      avg_price = mean(!!sym(price_col), na.rm = TRUE), # Handle potential NAs in price
      .groups = "drop"
    ) %>%
    arrange(transaction_year)

  # Check if volume_data_raw is empty after summarization
  if(nrow(volume_data_raw) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5) +
             theme_void())
  }

  # Create a complete sequence of years
  min_year <- min(volume_data_raw$transaction_year)
  max_year <- max(volume_data_raw$transaction_year)
  all_years_df <- data.frame(transaction_year = seq(min_year, max_year, by = 1))

  # Join with summarized data and fill missing values
  volume_data <- all_years_df %>%
    left_join(volume_data_raw, by = "transaction_year") %>%
    mutate(
      volume = ifelse(is.na(volume), 0, volume),
      # Keep avg_price as NA for years with no data, handle downstream
      avg_price = ifelse(volume == 0, NA, avg_price)
    ) %>%
    arrange(transaction_year) %>%
    # Calculate year-to-year price changes, handling NAs
    mutate(
      # Calculate lagged price only where previous year exists and has price
      lagged_avg_price = lag(avg_price),
      price_change = ifelse(!is.na(avg_price) & !is.na(lagged_avg_price), avg_price - lagged_avg_price, NA),
      price_change_pct = ifelse(!is.na(price_change) & !is.na(lagged_avg_price) & lagged_avg_price != 0, price_change / lagged_avg_price * 100, NA),
      market_direction = case_when(
        is.na(price_change_pct) ~ "No Trend", # Assign for NA changes
        price_change_pct > 2 ~ "Rising",
        price_change_pct < -2 ~ "Falling",
        TRUE ~ "Stable"
      )
    )

  # Ensure market_direction has levels for consistent coloring
  volume_data$market_direction <- factor(volume_data$market_direction, levels = c("Rising", "Stable", "Falling", "No Trend"))
  # --- End: Modified Data Preparation for Volume Plot ---


  # --- Start: Modified Plotting Logic ---
  # Create a dual-axis plot for volume and price trends
  # Need to handle potential NAs in avg_price for the geom_line/geom_point
  # Filter out NA avg_price before plotting the line/points
  price_trend_data <- volume_data %>% filter(!is.na(avg_price))

  # Check if there's any price trend data left to plot
  has_price_trend <- nrow(price_trend_data) > 0

  # Base plot
  p <- ggplot(volume_data, aes(x = factor(transaction_year))) +
    # Volume bars
    geom_col(aes(y = volume, fill = market_direction), alpha = 0.8) +
    # Market direction color scale (add "No Trend")
    scale_fill_manual(values = c(
      "Rising" = "#1a9850",
      "Stable" = "#4575b4",
      "Falling" = "#d73027",
      "No Trend" = "#bdbdbd" # Grey for no trend/data
    ), drop = FALSE) # drop = FALSE keeps all levels in legend

  # Add price trend line only if data exists and is plottable
  if (has_price_trend && nrow(price_trend_data) >= 2) { # Need at least 2 points for a line
    # Calculate scaling factor based only on non-NA avg_price
    max_avg_price_non_na <- max(price_trend_data$avg_price, na.rm = TRUE)
    max_volume <- max(volume_data$volume, na.rm = TRUE)
    # Avoid division by zero or issues with zero max volume/price
    scaling_factor <- if (!is.na(max_avg_price_non_na) && max_avg_price_non_na > 0 && max_volume > 0) {
        max_volume * 0.8 / max_avg_price_non_na
    } else {
        1 # Default scaling factor if calculation is not possible
    }

    # --- Start: Dynamic Annotation Positioning ---
    # Get data for the last year to position annotation dynamically
    last_year_data <- volume_data %>% filter(transaction_year == max(transaction_year))
    last_year_volume <- last_year_data$volume
    # Get the scaled price for the last year (handle potential NA)
    last_year_scaled_price <- last_year_data$avg_price * scaling_factor
    last_year_scaled_price <- ifelse(is.na(last_year_scaled_price), 0, last_year_scaled_price)

    # Determine the y position for the annotation
    # Place it slightly above the max of the volume bar and the price point for the last year
    y_annotation_pos <- max(last_year_volume, last_year_scaled_price, na.rm = TRUE) + (max(volume_data$volume, na.rm = TRUE) * 0.05) # 5% buffer based on max overall volume
    # --- End: Dynamic Annotation Positioning ---

    p <- p +
      geom_line(data = price_trend_data, aes(y = avg_price * scaling_factor, group = 1),
                color = "#2C3E50", size = 1, linetype = "dashed", na.rm = TRUE) + # na.rm = TRUE for line
      geom_point(data = price_trend_data, aes(y = avg_price * scaling_factor),
                 color = "#2C3E50", size = 3, na.rm = TRUE) + # na.rm = TRUE for points
      # Add secondary axis only if price trend is plotted
      scale_y_continuous(
        name = "Number of Transactions",
        sec.axis = sec_axis(~ . / scaling_factor, name = "Average Price (SGD)", labels = scales::dollar_format(prefix = "$", suffix = "", big.mark = ","))
      ) +
      # Add annotation only if price trend is plotted
      annotate("text",
               x = length(volume_data$transaction_year), # Position at the last year
               # --- Start: Use calculated y position and vjust --- 
               y = y_annotation_pos, 
               label = "Avg Price Trend →",
               color = "#2C3E50",
               hjust = 1, # Right-align horizontally
               vjust = 0, # Align bottom of text to y_annotation_pos (places text above)
               # --- End: Use calculated y position and vjust ---
               fontface = "italic")

  } else {
    # If no price trend, just use the primary y-axis for volume
    p <- p + scale_y_continuous(name = "Number of Transactions")
  }

  # Add remaining plot elements
  p <- p +
    labs(
      title = "Transaction Volume & Market Trends",
      subtitle = if(has_price_trend && nrow(price_trend_data) >= 2) "Volume bars with avg price trend overlay (dashed line)" else "Volume bars",
      x = "Year",
      # y axis label is set by scale_y_continuous
      fill = "Market Direction"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5), # Keep angle 0
      panel.grid.minor = element_blank(),
      legend.position = "right",
      axis.title.y.right = element_text(color = "#2C3E50"), # Style secondary axis title
      axis.text.y.right = element_text(color = "#2C3E50")  # Style secondary axis text
    )
  # --- End: Modified Plotting Logic ---

  # Return the plot
  return(p)
})

# --- Dashboard Overlay Close Button ---
observeEvent(input$close_transactions, {
  # Set reactive value to track state
  transactions_overlay_visible(FALSE)

  # Hide the overlay via JavaScript
  session$sendCustomMessage("hideTransactionsOverlay", list())
})
