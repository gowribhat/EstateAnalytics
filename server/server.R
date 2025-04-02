server <- function(input, output, session) {
  # ============================
  # DATA LOADING & PREPARATION
  # ============================
  dataset <- reactive({
    if (input$dataset == "Schools") {
      return(schools_data)
    } else {
      return(childcares_data)
    }
  })
  
  # Track the selected map type
  selected_map_type <- reactiveVal("pointmap")

  observeEvent(input$heatmap, {
    selected_map_type("heatmap")
  })

  observeEvent(input$pointmap, {
    selected_map_type("pointmap")
  })
  
  # ============================
  # INITIAL MAP RENDERING
  # ============================
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)  # Default center (Singapore)
  })
  
  # ============================
  # MAP TYPE SWITCHING
  # ============================
  
  output$map <- renderLeaflet({
    data <- dataset()
    
    if (input$dataset == "Schools") {
      name_col <- "school_name"
      color <- "blue"
    } else {
      name_col <- "centre_name"
      color <- "red"
    }
    
    # Base leaflet map
    map <- leaflet(data) %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)  # Center map on Singapore
    
    # Render selected map type
    if (selected_map_type() == "heatmap") {
      map <- map %>%
        addHeatmap(
          lng = ~longitude, lat = ~latitude,
          blur = 20, max = 0.05, radius = 15
        )
    } else if (selected_map_type() == "pointmap") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude, 
          popup = ~paste0("<b>Name:</b> ", get(name_col), 
                          "<br><b>Postal Code:</b> ", postal_code),
          radius = 5, color = color, fillOpacity = 0.5
        )
    }
    
    return(map)
  })
  
  # Placeholder for Cluster Map (Coming Soon)
  observeEvent(input$clustered, {
    showNotification("Clustered Map feature is coming soon!", type = "message")
  })
  
  # Placeholder for Filtering
  observeEvent(input$apply_filters, {
    showNotification("Filtering feature is coming soon!", type = "message")
  })
  
  # ============================
  # FUTURE ADDITIONS
  # ============================
  # TODO: Property Price Analysis
  #       - Choropleth maps for median prices
  #       - Time series trends visualization
  #
  # TODO: Lifestyle Match Scoring
  #       - Customizable scoring system based on user preferences
  #       - Radar/spider charts for comparing neighborhoods
  #
  # TODO: Transport & Accessibility Features
  #       - Walking distance calculations to nearby MRT/bus stops
  #       - Score-based system to rank areas by convenience
}
