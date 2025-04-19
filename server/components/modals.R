# Modal Dialog Logic
# This script defines the logic for modal dialogs used in the application.
# It includes modals for filtering data by property type, budget, area, floor height, and facilities.
# Key components:
# - Modal dialogs: Provides user-friendly interfaces for selecting filters.
# - Reactive updates: Updates reactive values based on user inputs in modals.
# - Button label updates: Dynamically updates button labels to reflect selected filter values.

# --- Modal Dialog Logic ---

# Observe House Type button click
observeEvent(input$filter_house_type, {
  showModal(modalDialog(
    title = "Select House Type",
    # Corrected choices based on usage elsewhere
    selectInput("modal_house_type", "Property Type:", choices = c("HDB", "Private"), selected = selected_property_type()),
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
    sliderInput("modal_budget", "Budget (SGD):", min = 100000, max = 10000000, value = budget_range(), step = 50000, pre = "$", sep = ",", width = "100%"), # Max adjusted based on reactiveVal default
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok_budget", "OK")
    )
  ))
})

# Observe Floor Size button click
observeEvent(input$filter_area, {
  showModal(modalDialog(
    title = "Select Floor Size Range",
    sliderInput("modal_area", "Floor Size (sqm):", min = 0, max = 1000, value = area_range(), step = 10, width = "100%"),
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
    sliderInput("modal_floor_height", "Floor Level:", min = 1, max = 50, value = floor_range(), step = 1, width = "100%"), # Min adjusted based on reactiveVal default
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok_floor_height", "OK")
    )
  ))
})

observeEvent(input$filter_facility, {
  showModal(modalDialog(
    title = "Select Nearby Facilities",
    # Checkbox input for selecting facilities
    checkboxGroupInput("selected_facilities", "Choose facilities:",
      choices = c("Childcare Centre", "Gym", "LRT/MRT", "Park", "School", "Supermarket"),
      selected = user_selection()),
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
  # Use previous ranking if available and matches selected facilities
  current_rank <- ranked_selection()
  # Only use previous ranking if it matches the current selection
  if (!is.null(current_rank) && setequal(current_rank, input$selected_facilities)) {
    labels_to_use <- current_rank
  } else {
    labels_to_use <- input$selected_facilities
  }
  sortable::rank_list(
    text = "Prioritise selected facilities (drag to reorder):",
    labels = labels_to_use,
    input_id = "facility_priority"
  )
})

# Observer for confirming facility selection
observeEvent(input$ok_facility, {
  # Store the selected facilities
  user_selection(input$selected_facilities)
  # Retrieve the ranked order from the rank list
  ranked_selection(input$facility_priority)
  print(user_selection())          # Debug: Check updated reactive value
  print(ranked_selection()) 
  # Display the ranked facilities in a modal dialog
  showModal(modalDialog(
    title = "Your Ranked Facilities",
    verbatimTextOutput("ranked_output"),  # Output for ranked facilities
    easyClose = TRUE
  ))
  removeModal()  # Close the selection modal
})
# Add logic here later to handle priority if needed, or use a different input type
# --- Update Button Labels on Modal OK ---

observeEvent(input$ok_house_type, {
  updateActionButton(session, "filter_house_type", label = paste("Type:", input$modal_house_type))
  removeModal()
  # Note: The reactive value selected_property_type is updated in filters.R
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
  # Note: The reactive value budget_range is updated in filters.R
})

observeEvent(input$ok_area, {
  label_text <- paste("Floor Size:", input$modal_area[1], "-", input$modal_area[2], "sqm")
  updateActionButton(session, "filter_area", label = label_text)
  removeModal()
  # Note: The reactive value area_range is updated in filters.R
})

observeEvent(input$ok_floor_height, {
  label_text <- paste("Floor:", input$modal_floor_height[1], "-", input$modal_floor_height[2])
  updateActionButton(session, "filter_floor_height", label = label_text)
  removeModal()
  # Note: The reactive value floor_range is updated in filters.R
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
  # Note: Facility selection currently doesn't update a reactive value for filtering
})