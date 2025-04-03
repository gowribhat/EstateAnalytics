# Map Module
# This module handles the leaflet map display and interactions

mapModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "map-container",
        leafletOutput(ns("property_map"), width = "100%", height = "100vh"),
        absolutePanel(
          id = "map-controls",
          class = "panel panel-default",
          top = 10, right = 10, width = 300,
          draggable = TRUE,
          style = "z-index:1000; background-color: rgba(255, 255, 255, 0.9); padding: 15px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          h4("Map Controls"),
          
          # Property type selection
          selectInput(ns("property_type"), "Property Type:", 
                      choices = property_types, 
                      selected = "All"),
          
          # Price range slider
          sliderInput(ns("price_range"), "Price Range (SGD):", 
                      min = 0, max = 5000000, 
                      value = c(100000, 2000000), 
                      step = 10000),
          
          # Map overlays
          checkboxGroupInput(ns("map_overlays"), "Map Overlays:",
                            choices = list(
                              "Property Heatmap" = "heatmap",
                              "MRT Stations" = "mrt",
                              "Planning Areas" = "planning",
                              "Property Markers" = "markers"
                            ),
                            selected = c("markers", "mrt")),
          
          # Reset map view button
          actionButton(ns("reset_view"), "Reset Map View", 
                       class = "btn-primary btn-sm btn-block")
        )
    )
  )
}

mapModule <- function(input, output, session) {
  ns <- session$ns
  
  # Initialize reactive values
  map_data <- reactiveValues(
    hdb_data = NULL,
    ura_data = NULL,
    mrt_data = NULL,
    planning_areas = NULL,
    filtered_data = NULL
  )
  
  # Load data reactively when needed
  observe({
    map_data$hdb_data <- loadData("hdb_resale")
    map_data$ura_data <- loadData("ura_private")
    map_data$planning_areas <- loadData("planning_areas")
  })
  
  # Filter data based on user inputs
  observe({
    req(map_data$hdb_data)
    
    filtered <- map_data$hdb_data %>%
      filter(resale_price >= input$price_range[1] & 
               resale_price <= input$price_range[2])
    
    if (input$property_type != "All") {
      filtered <- filtered %>% 
        filter(flat_type == input$property_type)
    }
    
    map_data$filtered_data <- filtered
  })
  
  # Initialize the map
  output$property_map <- renderLeaflet({
    initializeLeafletMap()
  })
  
  # Update the map based on user inputs
  observe({
    req(map_data$filtered_data)
    
    map_proxy <- leafletProxy("property_map")
    
    # Clear existing layers
    map_proxy %>% 
      clearMarkers() %>%
      clearShapes() %>%
      clearHeatmap()
    
    # Add property markers if selected
    if ("markers" %in% input$map_overlays) {
      map_proxy <- addPropertyMarkers(map_proxy, map_data$filtered_data, property_price_palette)
    }
    
    # Add property heatmap if selected
    if ("heatmap" %in% input$map_overlays) {
      map_proxy <- addPropertyHeatmap(map_proxy, map_data$filtered_data)
    }
    
    # Add MRT stations if selected
    if ("mrt" %in% input$map_overlays && !is.null(map_data$mrt_data)) {
      map_proxy <- addMrtStations(map_proxy, map_data$mrt_data)
    }
    
    # Add planning areas if selected
    if ("planning" %in% input$map_overlays && !is.null(map_data$planning_areas)) {
      map_proxy <- addPlanningAreas(map_proxy, map_data$planning_areas)
    }
  })
  
  # Reset map view
  observeEvent(input$reset_view, {
    leafletProxy("property_map") %>%
      setView(lng = sg_lng, lat = sg_lat, zoom = sg_zoom)
  })
  
  # Return reactive values for other modules to use
  return(map_data)
}