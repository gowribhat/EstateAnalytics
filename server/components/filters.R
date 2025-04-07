# Data Filtering Logic
# This script defines the logic for filtering data based on user-selected criteria.
# It includes reactive values for filters and functions to filter HDB and private property data.
# Key components:
# - Reactive filters: Tracks user-selected values for property type, budget, area, and floor height.
# - Filtered datasets: Applies filters to HDB and private property datasets to generate subsets for visualization and analysis.

# Property type selection (HDB or Private)
observeEvent(input$ok_house_type, {
  selected_property_type(input$modal_house_type)
  # Reset selected building when house type changes to prevent errors
  selected_building(NULL)
})

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

observeEvent(input$filter_facility, {
  showModal(modalDialog(
    title = "Select Nearby Facilities",
    
    checkboxGroupInput("selected_facilities", "Choose facilities:",
                       choices = c("Childcare Centre", "Gym", "MRT", "Park", "School", "Supermarket")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_facilities", "Next")
    )
  ))
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
  floor_high <- floor_range()[2] # Note: This was floor_high, should likely be floor_range()[2]
  area_min <- area_range()[1]
  area_max <- area_range()[2]

  # Apply filters
  filtered <- data %>%
    filter(
      resale_price >= budget_min,
      resale_price <= budget_max,
      floor_low >= floor_min,
      floor_high <= floor_range()[2], # Corrected to use upper bound of floor_range
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
