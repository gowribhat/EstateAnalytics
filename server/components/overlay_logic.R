# Overlay Handling Logic

# --- Overlay Visibility Handling ---
# This observes the input value set by our JavaScript to track overlay visibility
observeEvent(input$overlays_visible, {
  # You can add additional logic here if needed based on overlay visibility
  # For example, adjusting the map size or other UI elements
}, ignoreInit = TRUE)

# --- Transactions Overlay Handler ---
# Toggle transactions overlay when button is clicked
observeEvent(input$toggle_transactions_overlay, {
  # Toggle the transaction overlay visibility status
  transactions_overlay_visible(!transactions_overlay_visible())

  # Use JavaScript to actually show/hide the transactions overlay
  if(transactions_overlay_visible()) {
    # Show the overlay
    session$sendCustomMessage("showTransactionsOverlay", list())
  } else {
    # Hide the overlay
    session$sendCustomMessage("hideTransactionsOverlay", list())
  }
})

# Handle force refresh requests from JavaScript
observeEvent(input$force_dt_refresh, {
  # Only force refresh if overlay is visible
  if(transactions_overlay_visible()) {
    # Just triggering this reactive will cause the table to redraw
    building_data <- building_transactions() # Assumes building_transactions is defined elsewhere

    # Explicitly send a refresh command to the JavaScript
    session$sendCustomMessage("refreshDataTable", list(tableId = "building_transactions"))
  }
}, ignoreInit = TRUE)

# --- Transactions Overlay Close Button ---
observeEvent(input$close_transactions, {
  # Set reactive value to track state
  transactions_overlay_visible(FALSE)

  # Hide the overlay
  session$sendCustomMessage("hideTransactionsOverlay", list())
})
