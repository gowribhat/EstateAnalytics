# Overlay Handling Logic
# This script manages the visibility and interactions of overlays in the application.
# It includes logic for toggling overlays, handling user interactions, and refreshing data tables.
# Key components:
# - Transactions overlay: Displays detailed transaction data for selected properties.
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

# --- Transactions Overlay Handler ---
# Toggle transactions overlay when button is clicked
observeEvent(input$toggle_transactions_overlay, {
  # Toggle the transaction overlay visibility status
  current_visibility <- !transactions_overlay_visible()
  transactions_overlay_visible(current_visibility)

  # Use JavaScript to actually show/hide the transactions overlay
  if(current_visibility) {
    # Show the overlay
    session$sendCustomMessage("showTransactionsOverlay", list())
  } else {
    # Hide the overlay
    session$sendCustomMessage("hideTransactionsOverlay", list())
  }
})

# Render the transaction data table
output$transactions_table <- renderDT({
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
  
  if(property_type == "HDB") {
    # Format HDB data
    result <- building_data %>%
      select(month, resale_price, flat_type, storey_range, floor_area_sqm) %>%
      rename(
        Date = month,
        Price = resale_price,
        Type = flat_type,
        Floor = storey_range,
        Area = floor_area_sqm
      )
  } else {
    # Format private property data
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
  }
  
  # Format price as currency
  result <- result %>%
    mutate(Price = paste0("$", format(Price, big.mark = ",")),
           Area = paste(Area, "sqm"))
  
  # Return the formatted data table with options
  datatable(
    result,
    options = list(
      pageLength = 10,
      searching = TRUE,
      lengthChange = TRUE,
      scrollY = "calc(100% - 100px)",
      dom = 'lftip',
      language = list(
        emptyTable = "No transaction history available for this property"
      ),
      initComplete = JS("
        function(settings, json) {
          // Force the DataTable to resize properly when shown
          setTimeout(function() {
            $(window).trigger('resize');
          }, 200);
        }
      ")
    ),
    rownames = FALSE,
    class = 'stripe hover cell-border display',
    style = 'bootstrap',
    fillContainer = TRUE
  )
})

# --- Transactions Overlay Close Button ---
observeEvent(input$close_transactions, {
  # Set reactive value to track state
  transactions_overlay_visible(FALSE)

  # Hide the overlay via JavaScript
  session$sendCustomMessage("hideTransactionsOverlay", list())
})
