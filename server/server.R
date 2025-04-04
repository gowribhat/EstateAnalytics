# Main Server File

server <- function(input, output, session) {

  # --- Reactive Values ---
  # Central place for reactive values needed across components
  selected_building <- reactiveVal(NULL)
  transactions_overlay_visible <- reactiveVal(FALSE)
  planning_areas_data <- reactiveVal(NULL)
  hdb_data <- reactiveVal(NULL)
  household_income_data <- reactiveVal(NULL)
  ura_data <- reactiveVal(NULL)
  selected_property_type <- reactiveVal("HDB")
  budget_range <- reactiveVal(c(100000, 10000000))
  floor_range <- reactiveVal(c(1, 50))
  area_range <- reactiveVal(c(0, 1000))
  current_zoom <- reactiveVal(sg_zoom) # Default zoom from global.R
  marker_cache <- reactiveVal(NULL)
  
  # --- Source Component Logic ---
  # Source components in a specific order to ensure dependencies are properly loaded
  # First load map logic
  source("server/components/map_logic.R", local = TRUE)
  
  # Then load data
  source("server/components/data_loading.R", local = TRUE)
  
  # Then load filters which depend on the data
  source("server/components/filters.R", local = TRUE)
  
  # Then load the remaining components in alphabetical order
  remaining_components <- list.files("server/components", pattern = "\\.R$", full.names = TRUE)
  remaining_components <- remaining_components[!grepl("(map_logic\\.R|data_loading\\.R|filters\\.R)$", remaining_components)]
  lapply(remaining_components, function(file) source(file, local = TRUE))
  
  # Make sure the overlay is not shown at startup
  transactions_overlay_visible(FALSE)
}
