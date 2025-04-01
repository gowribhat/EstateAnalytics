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
  
  # Render map (Placeholder)
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)  # Singapore center
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
