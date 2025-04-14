# Building Details Logic (Right Overlay)
# This script manages the logic for displaying nearby facilities in the right overlay.
# It includes reactive datasets, UI rendering, and visualizations for selected buildings.
# Key components:
# - Nearby facilities: Displays detailed information about facilities near the selected building.
# - Visualizations: Generates plots and tables for building-specific data.

# Distance calculation utility
distances <- function(x, y) {
  distVincentySphere(c(x$longitude, x$latitude), c(y$longitude, y$latitude))
}

# Find nearest facilities
get_nearest <- function(a, b) {
  dists <- sapply(1:nrow(b), function(i) {
    distances(a, b[i, ])
  })
  nearest_indices <- order(dists)[1]
  clean <- b[nearest_indices, ]
  clean$distance <- dists[nearest_indices]
  return(clean)
}
# Default weights for different facilities
weight <- c(15,10,25,15,15,20)
# Normalisation of distances
# Anything less than 100m treated as 100m (close enough)
# Anything more than 1600m treated as 1600m (too far)
normal <- function(x){max(100,min(1600,x))}

# Reactive data for selected building and its facilities
facilities <- reactive({
  building <- selected_building()
  property_type <- selected_property_type()
  
  if (is.null(building)) {
    return(NULL)  # No building selected
  }
  # Filter building data based on property type
  if (property_type == "HDB") {
    data <- filtered_hdb_data()
    req(data)
    building_data <- data %>%
      filter(
        block == building$block,
        street_name == building$street_name
      )
  } else {
    data <- filtered_ura_data()
    req(data)
    building_data <- data %>%
      filter(
        project == building$project,
        street == building$street
      )
  }
  building_data$childcare <- round(get_nearest(building, childcare())$distance)
  building_data$gym <- round(get_nearest(building, gym())$distance)
  building_data$mrt <- round(get_nearest(building, mrt())$distance)
  building_data$park <- round(get_nearest(building,park())$distance)
  building_data$sch <- round(get_nearest(building, sch())$distance)
  building_data$mart <- round(get_nearest(building, mart())$distance)
  
  norm_dist <- sapply(list(building_data$childcare, building_data$gym, building_data$mrt,
                           building_data$park,building_data$sch,building_data$mart),normal)
  
  score <- (1600-norm_dist)/1500
  building_data$total_score <- round(sum(weight*score),1)
  return(building_data)
})

# Reactive value to store the user's selected facilities
user_selection <- reactiveVal(NULL)
# Filtered facilities data based on selected filters
ranked_selection <- reactiveVal(NULL)
proximity_score <- reactiveVal(NULL)

# Reactive ranking and weight calculation based on user selection
server <- function(input, output, session) {
  # Observer for the filter facility button
  observeEvent(input$filter_facility, {
    showModal(modalDialog(
      title = "Select Nearby Facilities",
      # Checkbox input for selecting facilities
      checkboxGroupInput("selected_facilities", "Choose facilities:",
                         choices = c("Childcare Centre", "Gym", "LRT/MRT", "Park", "School", "Supermarket")),
      # Placeholder for dynamic priority UI
      uiOutput("priority_rank_ui"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_facility", "OK")
      )
    ))
  })
  
  # Render the dynamic rank list UI
  output$priority_rank_ui <- renderUI({
    req(input$selected_facilities)  # Ensure facilities have been selected
    if (length(input$selected_facilities) == 0) return(NULL)
    # Render a draggable rank list
    sortable::rank_list(
      text = "Prioritise selected facilities (drag to reorder):",
      labels = input$selected_facilities,
      input_id = "facility_priority"
    )
  })
  
  # Observer for confirming facility selection
  observeEvent(input$ok_facility, {
    # Store the selected facilities
    user_selection(input$selected_facilities)
    # Retrieve the ranked order from the rank list
    ranked_selection(input$facility_priority)
    
    # Display the ranked facilities in a modal dialog
    showModal(modalDialog(
      title = "Your Ranked Facilities",
      verbatimTextOutput("ranked_output"),  # Output for ranked facilities
      easyClose = TRUE
    ))
    removeModal()  # Close the selection modal
  })
  
  # Output for displaying ranked facilities
  output$ranked_output <- renderPrint({
    req(ranked_selection())
    ranked_selection()
  })
}