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

  # Return a div containing the plotly chart with appropriate sizing - reduced height for compression
  div(
    style = "height:220px; background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
    plotlyOutput(session$ns("income_donut"), height = "100%")
  )
})

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
    income_type = legend_labels,
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
