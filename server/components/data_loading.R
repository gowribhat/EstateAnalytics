# Data Loading Logic
# This script handles the loading of data required for the application.
# It includes logic for loading planning areas, HDB resale data, household income data, and private property transactions.
# Key components:
# - Reactive data loading: Loads data reactively to ensure it is available when needed.
# - Error handling: Provides fallback mechanisms and error logging for data loading issues.

# --- Load Data ---
# Load planning areas data reactively or on startup
observe({
  planning_areas_data(loadData("planning_areas"))
})

# Load HDB resale data
observe({
  # Load RDS file for better performance
  tryCatch({
    data <- readRDS(paste0(resources_path, "hdb.rds"))
    hdb_data(data)
  }, error = function(e) {
    # Fallback to CSV if RDS not available
    data <- loadData("hdb_resale")
    hdb_data(data)
  })
})

# Load household income data
observe({
  tryCatch({
    data <- readRDS(paste0(resources_path, "household_income_data.rds"))
    household_income_data(data)
  }, error = function(e) {
    # Log error but continue app execution
    message("Error loading household income data: ", e$message)
  })
})

# Load URA private transaction data
observe({
  # Load RDS file for better performance
  tryCatch({
    data <- readRDS(paste0(resources_path, "ura_private.rds"))
    ura_data(data)
  }, error = function(e) {
    # Fallback to CSV if RDS not available
    data <- loadData("ura_private")
    ura_data(data)
  })
})
