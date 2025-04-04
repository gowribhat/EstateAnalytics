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
  # Dynamically source all R scripts in the components folder
  component_files <- list.files("server/components", pattern = "\\.R$", full.names = TRUE)
  lapply(component_files, function(file) source(file, local = TRUE))

  # --- Debugging ---
  # Keep essential debugging observers if needed
  observe({
    if (is.null(planning_areas_data())) {
      print("Planning areas data is not loaded.")
    } else {
      print("Planning areas data loaded successfully.")
    }
  })
  
  # Make sure the overlay is not shown at startup
  transactions_overlay_visible(FALSE)
}
