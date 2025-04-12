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
  
  # Create UI elements for the dashboard
  fluidRow(
    column(
      width = 4,
      div(
        style = "height: 100%; overflow-y: auto; overflow-x: hidden; max-height: 400px;", # Only vertical scrolling with fixed height
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
                      choices = c("All Types" = "all", 
                                  as.character(unique(building_data[[type_col]]))),
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
  
  # Check if we have data
  if(nrow(building_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5) +
             theme_void())
  }
  
  # Process data for transaction volume analysis
  volume_data <- building_data %>%
    mutate(
      transaction_date = as.Date(!!sym(date_col)),
      transaction_year = year(transaction_date)
    ) %>%
    group_by(transaction_year) %>%
    summarize(
      volume = n(),
      avg_price = mean(!!sym(price_col)),
      .groups = "drop"
    ) %>%
    arrange(transaction_year) %>%
    # Calculate year-to-year price changes
    mutate(
      price_change = c(0, diff(avg_price)),
      price_change_pct = c(0, diff(avg_price) / avg_price[-n()] * 100),
      market_direction = case_when(
        price_change_pct > 2 ~ "Rising",
        price_change_pct < -2 ~ "Falling",
        TRUE ~ "Stable"
      )
    )
  
  # Create a dual-axis plot for volume and price trends
  ggplot(volume_data, aes(x = factor(transaction_year))) +
    # Volume bars
    geom_col(aes(y = volume, fill = market_direction), alpha = 0.8) +
    # Price trend line (on secondary y-axis)
    geom_line(aes(y = avg_price / max(avg_price) * max(volume) * 0.8, 
                 group = 1), 
             color = "#2C3E50", size = 1, linetype = "dashed") +
    geom_point(aes(y = avg_price / max(avg_price) * max(volume) * 0.8), 
              color = "#2C3E50", size = 3) +
    # Market direction color scale
    scale_fill_manual(values = c(
      "Rising" = "#1a9850", 
      "Stable" = "#4575b4", 
      "Falling" = "#d73027"
    )) +
    # Labels
    labs(
      title = "Transaction Volume & Market Trends",
      subtitle = "Volume bars with price trend overlay (dashed line)",
      x = "Year",
      y = "Number of Transactions",
      fill = "Market Direction"
    ) +
    # Custom theme
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    ) +
    # Add annotations
    annotate("text", 
             x = length(volume_data$transaction_quarter), 
             y = max(volume_data$volume) * 0.95,
             label = "Price Trend →",
             color = "#2C3E50",
             hjust = 1,
             fontface = "italic")
})

# --- Dashboard Overlay Close Button ---
observeEvent(input$close_transactions, {
  # Set reactive value to track state
  transactions_overlay_visible(FALSE)

  # Hide the overlay via JavaScript
  session$sendCustomMessage("hideTransactionsOverlay", list())
})
