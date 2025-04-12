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

# Load Childcare Centre data
observe({
  tryCatch({
    childcare(loadData("childcares"))
  }, error = function(e) {
    message("Error loading Childcare Centre data: ", e$message)
  })
})

# Load Gym data
observe({
  tryCatch({
    gym(loadData("gyms_data"))
  }, error = function(e) {
    message("Error loading Gym data: ", e$message)
  })
})

# Load LRT/MRT data
observe({
  tryCatch({
    mrt(loadData("LRT_MRT"))
  }, error = function(e) {
    message("Error loading LRT/MRT data: ", e$message)
  })
})

# Load Park data
observe({
  tryCatch({
    park(loadData("parks_data"))
  }, error = function(e) {
    message("Error loading Park data: ", e$message)
  })
})

# Load School data
observe({
  tryCatch({
    sch(loadData("schools"))
  }, error = function(e) {
    message("Error loading School data: ", e$message)
  })
})

# Load Supermarket data
observe({
  tryCatch({
    mart(loadData("Supermarkets"))
  }, error = function(e) {
    message("Error loading Supermarket data: ", e$message)
  })
})