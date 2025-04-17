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

# Reactive expression to prepare the ordered facility data for display
facility_display_data <- reactive({
  facility_data_val <- facilities()
  req(facility_data_val) # Ensure facility data is available

  f_distances <- c(
    `Childcare Centre` = facility_data_val$childcare[1],
    Gym = facility_data_val$gym[1],
    `LRT/MRT` = facility_data_val$mrt[1],
    Park = facility_data_val$park[1],
    School = facility_data_val$sch[1],
    Supermarket = facility_data_val$mart[1]
  )

  user_ranked <- user_selection() # Get user ranking
  
  if (is.null(user_ranked) || length(user_ranked) == 0) {
    # Default order (alphabetical or predefined)
    ordered_names <- sort(names(f_distances))
    f_ordered <- f_distances[ordered_names]
    ranks <- rep(NA, length(f_ordered)) # No ranks for default view
    is_ranked <- FALSE

  } else {
    # User ranked order
    # Filter f_distances to only include selected facilities
    f_distances_filtered <- f_distances[names(f_distances) %in% user_ranked]
    
    # Get the order based on ranked_selection (which should be ordered)
    ranked_fac_ordered <- ranked_selection() 
    # Ensure we only use facilities present in both ranked list and available data
    valid_ranked_fac <- intersect(ranked_fac_ordered, names(f_distances_filtered))
    
    req(length(valid_ranked_fac) > 0) # Need at least one valid facility

    f_ordered <- f_distances_filtered[valid_ranked_fac]
    # Use seq_along for safer rank generation
    ranks <- seq_along(f_ordered) 
    is_ranked <- TRUE
  }

  # Create the data frame
  df <- data.frame(
    Rank = ranks,
    Facility = names(f_ordered),
    Distance = as.numeric(f_ordered),
    stringsAsFactors = FALSE # Important!
  )
  
  # Set factor levels according to the display order
  df$Facility <- factor(df$Facility, levels = names(f_ordered)) 
  
  # Calculate score (keep existing logic, adapt if needed)
  score_info <- list(score = NA, type = "default")
  if (is_ranked) {
      n <- length(f_ordered)
      if (n > 0) {
          weights <- (n:1) / (n * (n + 1) / 2) * 100 # Descending weights
          # Ensure 'normal' function is available and handles potential NA distances
          norm_dist <- sapply(f_ordered, function(d) ifelse(is.na(d), NA, normal(d))) 
          score_contributions <- ifelse(is.na(norm_dist), 0, (1600 - norm_dist) / 1500) # Handle NA distances in score
          weighted_score <- sum(weights * score_contributions, na.rm = TRUE) # Use na.rm = TRUE
          score_info <- list(score = weighted_score, type = "ranked")
      }
  } else {
      # Use default total score if available
      if (!is.null(facility_data_val$total_score) && !is.na(facility_data_val$total_score[1])) {
         score_info <- list(score = facility_data_val$total_score[1], type = "default")
      }
  }

  return(list(data = df, score_info = score_info, is_ranked = is_ranked))
})


# Building-specific details for the right overlay
output$property_details <- renderUI({
  building <- selected_building()
  building_data <- building_transactions()

  # If no building is selected or no data is available, show a prompt message
  if (is.null(building) || is.null(building_data) || nrow(building_data) == 0) {
    return(
      div(class = "detail-placeholder",
          tags$i(class = "fa fa-info-circle"), # Example icon
          p("Click on a property marker to see details.")
      )
    )
  }

  property_type <- selected_property_type()

  # Basic Building Info
  building_name <- if (property_type == "HDB") paste(building$block, building$street_name) else building$project
  address <- if (property_type == "HDB") "" else paste("Address:", building$street) # Only show address for URA

  # Key Stats Calculation
  if (property_type == "HDB") {
    median_price <- median(building_data$resale_price)
    recent_price <- building_data$resale_price[1]
    latest_date <- format(building_data$month[1], "%b %Y")
    total_transactions <- nrow(building_data)
    year_built <- paste("Built:", building_data$lease_commence_date[1])
    flat_type <- paste("Flat Type:", building_data$flat_type[1])
    area_range_str <- paste("Area:", min(building_data$floor_area_sqm), "-", max(building_data$floor_area_sqm), "sqm")
    tenure_str <- "" # HDB doesn't have tenure in the same way
  } else {
    median_price <- median(building_data$price)
    recent_price <- building_data$price[1]
    latest_date <- format(building_data$contractDate[1], "%b %Y")
    total_transactions <- nrow(building_data)
    year_built <- paste("Property Type:", building_data$propertyType[1]) # Use Property Type instead of Built
    flat_type <- "" # URA doesn't have flat_type
    area_range_str <- paste("Area:", min(building_data$area), "-", max(building_data$area), "sqm")
    tenure_str <- paste("Tenure:", building_data$tenure[1])
  }

  # --- Facility Section ---
  facility_info <- facility_display_data() # Use the new reactive
  req(facility_info)
  
  facility_df <- facility_info$data
  score_info <- facility_info$score_info
  is_ranked <- facility_info$is_ranked

  proximity_score_display <- NULL

  if (nrow(facility_df) > 0) {
      # Generate score display (Keep this part)
      score_value <- round(score_info$score, 1)
      if (!is.na(score_value)) {
          score_class <- if(score_info$type == "ranked") "proximity-score ranked-score" else "proximity-score default-score"
          score_label <- if(score_info$type == "ranked") "Weighted Proximity Score:" else "Overall Proximity Score:"
          proximity_score_display <- div(class = score_class,
                                         tags$strong(score_label),
                                         span(paste0(score_value, "%"))
                                        )
      } else {
           proximity_score_display <- div(class = "proximity-score default-score", tags$em("Score not available"))
      }

  } else {
      # Handle case where no facilities are displayed (e.g., after filtering)
      proximity_score_display <- div(class = "proximity-score default-score", tags$em("Score not applicable / No facilities selected"))
  }


  # Construct the UI using tags
  tagList(
    div(class = "building-header",
        tags$h5(building_name),
        if (address != "") tags$p(class = "text-muted", address)
    ),
    div(class = "key-stats",
        div(class = "stat-item",
            tags$strong("Latest Price:"),
            span(paste0("$", format(recent_price, big.mark = ","), " (", latest_date, ")"))
        ),
        div(class = "stat-item",
            tags$strong("Median Price:"),
            span(paste0("$", format(median_price, big.mark = ",")))
        ),
        div(class = "stat-item",
            tags$strong("Transactions:"),
            span(paste(total_transactions, "sales"))
        )
    ),
    hr(), # Separator
    div(class = "building-attributes",
        if (year_built != "Built: NA" && year_built != "Property Type: NA") tags$p(year_built),
        if (flat_type != "Flat Type: NA" && flat_type != "") tags$p(flat_type),
        if (area_range_str != "Area: NA - NA sqm") tags$p(area_range_str),
        if (tenure_str != "Tenure: NA" && tenure_str != "") tags$p(tenure_str)
    ),
    hr(), # Separator
    # Updated Facility Section
    div(class = "facility-section",
        tags$h6("Nearby Facilities"),
        proximity_score_display, # Keep the score display
        # Embed the facility plot directly here - plot uses the same underlying data now
        # Increased height slightly for better visibility
        # Wrap plotOutput in divs for overlay
        div(
          style = "position: relative;", # Container for overlay
          plotOutput("facility_plot", height = "200px"),
          div(style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 10; background-color: rgba(0,0,0,0);") # Transparent overlay
        )
    ),
    div(style = "margin-top: 20px;", # Add space before button
        actionButton("toggle_transactions_overlay", "Building Analytics",
                     class = "btn btn-primary btn-block action-button")
    )
  )
})

# Building-specific facility distance plot (now embedded and enhanced)

# Building-specific facility distance plot (now embedded and enhanced)
output$facility_plot <- renderPlot({
  facility_info <- facility_display_data() # Use the reactive data
  req(facility_info)
  
  facility_df <- facility_info$data
  is_ranked <- facility_info$is_ranked
  # Allow plot rendering even if df is empty initially, show message instead
  # req(nrow(facility_df) > 0) 

  # Handle empty data case within the plot
  if (nrow(facility_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No facilities selected or available.", size = 4, color = "grey50") +
        theme_void() +
        theme(plot.background = element_rect(fill = "transparent", colour = NA))
    )
  }

  # Add rank to facility names if ranked
  if (is_ranked) {
    facility_df$DisplayFacility <- paste0("#", facility_df$Rank, " ", facility_df$Facility)
    # Update factor levels to include the rank for correct ordering
    # Ensure levels are set based on the *current* order in the dataframe
    facility_df$DisplayFacility <- factor(facility_df$DisplayFacility, levels = rev(facility_df$DisplayFacility))
  } else {
    facility_df$DisplayFacility <- facility_df$Facility
    # Update factor levels for default order, ensuring correct order
    facility_df$DisplayFacility <- factor(facility_df$DisplayFacility, levels = rev(levels(facility_df$Facility)))
  }

  # Generate the bar plot using ggplot2 - enhanced for direct display
  ggplot(facility_df, aes(x = Distance, y = DisplayFacility)) +
    geom_bar(stat = "identity", fill = "#6baed6", width = 0.7) + # Adjusted color and width
    # Add text labels for distance on the bars
    geom_text(aes(label = paste(round(Distance), "m")), 
              hjust = -0.15, # Position text slightly outside the bar end
              size = 3.2,   # Adjust text size
              color = "#444444") + # Slightly darker text
    # Explicitly set the y-axis order based on the factor levels
    scale_y_discrete(limits = levels(facility_df$DisplayFacility)) + 
    # Expand x-axis limits slightly to accommodate text labels
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.18))) + # Adjust expansion
    labs(title = NULL, x = "Distance (m)", y = NULL) + # Add x-axis label
    theme_minimal(base_size = 11) + 
    theme(
      axis.title.x = element_text(size = 9, margin = margin(t = 5), color = "grey30"), # Style x-axis title
      axis.text.x = element_text(size = 8, color = "grey30"), # Adjust x-axis text size
      axis.text.y = element_text(size = 9.5, hjust = 1, color = "grey20"), # Show y-axis text (facility names), adjust size/color
      axis.ticks.y = element_blank(),      # Hide y-axis ticks
      panel.grid.major.y = element_blank(), # Hide horizontal grid lines
      panel.grid.minor = element_blank(),   # Hide minor grid lines
      panel.grid.major.x = element_line(color = "grey90", size = 0.4), # Subtle vertical grid lines
      plot.margin = margin(t = 5, r = 15, b = 5, l = 5), # Adjust plot margins (more right margin for labels)
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA)
    )
}, bg = "transparent") # Set plot background to transparent

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