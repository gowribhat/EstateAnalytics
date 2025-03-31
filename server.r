server <- function(input, output, session) {
  # Load data based on user selection
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
}
