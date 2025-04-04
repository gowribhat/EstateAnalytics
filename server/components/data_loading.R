# Data Loading Logic
# This script handles the loading of data required for the application.
# It includes logic for loading planning areas, HDB resale data, household income data, and private property transactions.
# Key components:
# - Reactive data loading: Loads data reactively to ensure it is available when needed.
# - Error handling: Provides fallback mechanisms and error logging for data loading issues.

# --- Load Data ---
# Load planning areas data
observe({
  planning_areas_data(loadData("planning_areas"))
})

# Load HDB resale data
observe({
  tryCatch({
    hdb_data(loadData("hdb_resale"))
  }, error = function(e) {
    message("Error loading HDB data: ", e$message)
  })
})

# Load household income data
observe({
  tryCatch({
    household_income_data(loadData("household_income"))
  }, error = function(e) {
    message("Error loading household income data: ", e$message)
  })
})

# Load URA private transaction data
observe({
  tryCatch({
    ura_data(loadData("ura_private"))
  }, error = function(e) {
    message("Error loading URA private data: ", e$message)
  })
})
