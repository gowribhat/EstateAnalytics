source("global.R")
source("ui.R")
source("server/server.R")

area <- fromJSON("district_and_planning_area.geojson")
area <- as.data.frame(area)
HDB <- readRDS("C:/Users/User/R-4.4.3/Project/data/hdb.rds")
priv <- readRDS("C:/Users/User/R-4.4.3/Project/data/ura_private.rds")

ui <- fluidPage()
shinyApp(ui, server)
server <- function(input,output){
  
  output$plotMap <- renderLeaflet(
    {
      leaflet() %>% setView(lng = mean(HDB$longitude), lat = mean(HDB$latitude),
                            zoom = input$zoomlevel) %>% 
        addTiles() %>% 
        addMarkers(lat = HDB$latitude[1:1000], lng = HDB$longitude[1:1000],
                   popup = paste(HDB$block[1:1000],HDB$street_name[1:1000]))
    }
  )
}

ui <- fluidPage(
  sliderInput(inputId = "zoomlevel",
              label = "Map Zooming Level",
              value = 12,
              min = 1,
              max = 20),
  leafletOutput(outputId = "plotMap")
)
shinyApp(ui = ui, server = server)

