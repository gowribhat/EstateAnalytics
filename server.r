server <- function(input, output, session) {
  # ============================
  # DATA LOADING & PREPARATION
  # ============================
  # TODO: Ensure datasets are properly loaded before use
  #       - Load data from `data/cleaned/`
  #       - Check for missing values and handle them appropriately
  dataset <- reactive({
    if (input$dataset == "Schools") {
      return(schools_data)
    } else {
      return(childcares_data)
    }
  })
  
  # Render table for selected dataset
  output$data_table <- renderDT({
    datatable(dataset(), options = list(pageLength = 10))
  })
  
  # Render summary of selected dataset
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  # Render map with dynamic markers
  output$map <- renderLeaflet({
    data <- dataset()  # Get selected dataset
    
    # Assign correct columns for name and postal code
    if (input$dataset == "Schools") {
      name_col <- "school_name"
      color <- "blue"  # Color for schools
    } else {
      name_col <- "centre_name"
      color <- "red"   # Color for childcare centers
    }
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%  # Center map on Singapore
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude, 
        popup = ~paste0("<b>Name:</b> ", get(name_col), 
                        "<br><b>Postal Code:</b> ", postal_code),
        radius = 5, color = color, fillOpacity = 0.5
      )
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
