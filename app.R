source("global.R")
source("ui.R")
source("server/server.R")

HDB <- readRDS("C:/Users/User/R-4.4.3/Project/data/hdb.rds")
priv <- readRDS("C:/Users/User/R-4.4.3/Project/data/ura_private.rds")

ui <- fluidPage()
shinyApp(ui, server)
server <- function(input,output){
  
  output$plotMap <- renderLeaflet(
    {
      leaflet() %>% setView(lng = 103.9355, lat = 1.359087,
                            zoom = input$zoomlevel) %>% 
        addTiles() %>% 
        addMarkers(lat = HDB$latitude[1], lng = HDB$longitude[1],
                   popup = paste(HDB$block[1],HDB$street_name[1]))
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

