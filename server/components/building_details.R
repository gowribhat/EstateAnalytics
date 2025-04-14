# Building Details Logic (Right Overlay)
# This script manages the logic for displaying building-specific details and visualizations in the right overlay.
# It includes reactive datasets, UI rendering, and visualizations for selected buildings.
# Key components:
# - Building transactions: Retrieves transaction data for the selected building.
# - Property details: Displays detailed information about the selected building.
# - Visualizations: Generates plots and tables for building-specific data.

# Right overlay: Building-specific data and visualization
# Get transactions for specific building when selected
source("./server/components/facility.R")

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
      arrange(desc(contractDate)) # Fixed missing closing parenthesis
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
  facility_data <- reactive({
    facilities()
  })
  data <- building_data %>% 
    mutate(dist_to_childcare = facility_data()$childcare,
           dist_to_gym = facility_data()$gym,
           dist_to_mrt = facility_data()$mrt,
           dist_to_park = facility_data()$park,
           dist_to_sch = facility_data()$sch,
           dist_to_mart = facility_data()$mart,
           total = facility_data()$total_score)
  if (property_type == "HDB") {
    # Calculate stats
    median_price <- median(building_data$resale_price)
    recent_price <- building_data$resale_price[1]
    total_transactions <- nrow(building_data)
    year_built <- building_data$lease_commence_date[1]

    # Include the Building Analytics button only when a building is selected
    html <- HTML(paste0(
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
      "<button id='toggle_transactions_overlay' type='button' class='btn btn-primary btn-block action-button'>Building Analytics</button>",
      "</div>",
      "</div>"
    ))
  } else {
    # Calculate stats for private properties
    median_price <- median(building_data$price)
    recent_price <- building_data$price[1]
    total_transactions <- nrow(building_data)

    # Include the Past Transactions button only when a building is selected
    html <- HTML(paste0(
      "<div style='font-size: 18px; font-weight: bold;'>", building$project, "</div>",
      "<div style='margin-top: 10px;'>",
      "<div><strong>Address:</strong> ", building$street, "</div>",
      "<div><strong>Latest Price:</strong> $", format(recent_price, big.mark = ","), " (", format(building_data$contractDate[1], "%b %Y"), ")</div>",
      "<div><strong>Median Price:</strong> $", format(median_price, big.mark = ","), "</div>",
      "<div><strong>Transactions:</strong> ", total_transactions, " sales</div>",
      "<div><strong>Property Type:</strong> ", building_data$propertyType[1], "</div>",
      "<div><strong>Area Range:</strong> ", min(building_data$area), " - ", max(building_data$area), " sqm</div>",
      "<div><strong>Tenure:</strong> ", building_data$tenure[1], "</div>",
      "</div>",
      "</div>"
    ))
  }
  if(is.null(user_selection())){
    html <- HTML(paste0(html,
      "<div><strong>Nearest Childcare Centre: </strong> ", data$dist_to_childcare[1], " m away", "</div>",
      "<div><strong>Nearest Gym: </strong> ", data$dist_to_gym[1], " m away", "</div>",
      "<div><strong>Nearest LRT/MRT: </strong> ", data$dist_to_mrt[1], " m away", "</div>",
      "<div><strong>Nearest Park: </strong> ", data$dist_to_park[1], " m away", "</div>",
      "<div><strong>Nearest School: </strong> ", data$dist_to_sch[1], " m away", "</div>",
      "<div><strong>Nearest Supermarket: </strong> ", data$dist_to_mart[1], " m away", "</div>",
      "<div><strong>Total Proximity Score: </strong> ", "</div>",
      "<div style='font-size: 20px'>", data$total[1], "%","</div>",
      "<div style='margin-top: 15px;'>",
      "</div>",
      "</div>"
    ))
  } else {
    selected_facilities <- reactive({
      facility_ranking()
    })
    f <- c(facility_data()$childcare[1],facility_data()$gym[1],facility_data()$mrt[1],
           facility_data()$park[1],facility_data()$sch[1],facility_data()$mart[1])
    names(f) <- c("Childcare Centre", "Gym", "LRT/MRT", "Park", "School", "Supermarket")
    
    # Rearranges the vector of distances by user-selected priority
    f <- f[match(ranked_selection(),names(f))]
    n <- length(f)
    # Lists the distances to facilities in order specified by user
    for(i in 1:n){
      html <- HTML(paste0(html,
                          "<div><strong>","Nearest ", names(f)[i], ":</strong> ", f[i], " m away", "</div>"))
    }
    # Calculate dynamic weights based on user-selected facilities
    calculate_weights <- function(f) {
      total_weight <- n * (n + 1)/2  # Total weight sum
      weights <- (n:1) / total_weight*100 # Descending weights
      norm_dist <- sapply(f,normal)
      score <- (1600-norm_dist)/1500
      return(sum(weights*score))
    }
    data <- data %>% mutate(proximity_score=round(calculate_weights(f),1))
    html <- HTML(paste0(html, 
                   "<div><strong>Total Proximity Score: </strong> ", "</div>",
                   "<div style='font-size: 20px'>", data$proximity_score[1], "%", "</div>",
                   "<div style='margin-top: 15px;'>",
                   # Ensure the button ID matches the observer in overlay_logic.R
                   "<button id='toggle_transactions_overlay' type='button' class='btn btn-primary btn-block action-button'>Past Transactions</button>",
                   "</div>",
                   "</div>"))
    }
  html
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

output$facility_plot <- renderPlot({
  # Add tryCatch to gracefully handle errors
  tryCatch({
    # Check if a building is selected
    building <- selected_building()
    if (is.null(building)) {
      # Return a placeholder plot when no building is selected
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Click on a marker to view facility distances", size = 5) +
          theme_void() +
          theme(
            plot.background = element_rect(fill = "#f8f9fa", color = NA)
          )
      )
    }

    # Get facility data
    facility_data <- facilities()
    req(facility_data)  # Ensure facility data is available

    # Extract distances
    f <- c(
      facility_data$childcare[1],
      facility_data$gym[1],
      facility_data$mrt[1],
      facility_data$park[1],
      facility_data$sch[1],
      facility_data$mart[1]
    )
    names(f) <- c("Childcare Centre", "Gym", "LRT/MRT", "Park", "School", "Supermarket")

    # Create a data frame for ggplot
    facility_df <- data.frame(
      Facility = names(f),
      Distance = f
    )

    # Generate the bar plot using ggplot2
    ggplot(facility_df, aes(x = Distance, y = Facility)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(
        title = "Distance from Different Facilities",
        x = "Distance (m)",
        y = "Facilities"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
  }, error = function(e) {
    # Return a placeholder plot on error
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Error rendering facility plot", size = 5) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#f8f9fa", color = NA)
      )
  })
})

# Building-specific transaction list for right overlay (transactions overlay)
output$building_transactions <- renderUI({
  # Add tryCatch to gracefully handle errors
  tryCatch({
    # Explicitly require selected_building() *inside* the tryCatch
    req(selected_building())
    
    # Get building-specific transactions
    building_data <- building_transactions()
    
    # Simple success message
    div(
      class = "alert alert-info",
      style = "text-align: center; padding = 20px;",
      h4("Building Data Found", style = "margin-top: 0"),
      p(paste("Found", nrow(building_data), "transactions for this building"))
    )
  }, error = function(e) {
    # On error, return an error message
    div(
      class = "alert alert-danger",
      "Error loading transaction history. Please check logs."
    )
  })
})
# Building-specific proximity plot