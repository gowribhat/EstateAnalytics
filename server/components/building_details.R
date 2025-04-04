# Building Details Logic (Right Overlay)

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
      # Ensure the button ID matches the observer in overlay_logic.R
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
      # Ensure the button ID matches the observer in overlay_logic.R
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

    # Plot title and price column
    if (property_type == "HDB") {
      plot_title <- paste0(building$block, " ", building$street_name)
      price_col_name <- "resale_price"
    } else {
      plot_title <- paste0(building$project, " - ", building$street)
      price_col_name <- "price"
    }
    price_col <- building_data[[price_col_name]]

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

# Building-specific transaction list for right overlay (transactions overlay)
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
