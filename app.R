source("global.R")
source("ui.R")
source("server/server.R")

ui <- fluidPage()
shinyApp(ui, server)
server <- function(input,output){
  output$plotMap <- renderLeaflet(
    {
      leaflet() %>% setView(lng = 103.9355, lat = 1.359087,
                            zoom = input$zoomlevel) %>% 
        addTiles() %>% 
        addMarkers(lat = 1.359087, lng = 103.9355,
                   popup = "My Old Home")
    }
  )
}

ui <- fluidPage(
  sliderInput(inputId = "zoomlevel",
              label = "Map Zooming Level",
              value = 11,
              min = 1,
              max = 20),
  leafletOutput(outputId = "plotMap")
)
shinyApp(ui = ui, server = server)

