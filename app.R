source("global.R")
source("ui.R")
source("server/server.R")

# Loading data
area <- st_read("C:/Users/User/R-4.4.3/Project/data/district_and_planning_area.geojson")
HDB <- readRDS("C:/Users/User/R-4.4.3/Project/data/hdb.rds")

# UI definition
ui <- fluidPage(
  sliderInput(inputId = "zoomlevel",
              label = "Map Zooming Level",
              value = 11,
              min = 1,
              max = 20),
  leafletOutput(outputId = "plotMap")
)

# Server definition
server <- function(input, output) {

  # Reactive to summarize HDB data by planning_area
  HDB_clean <- reactive({
    HDB %>%
      group_by(planning_area) %>%
      summarise(price = mean(resale_price, na.rm = TRUE))
  })
  
  
  # Reactive to merge HDB_area with area
  HDB_merged <- reactive({
    merge(HDB_clean(), area, by.x = "planning_area", by.y = "planning_area")  # Ensure a left join
  })
  
  # Render the map with the merged data
  output$plotMap <- renderLeaflet({
    HDB_merged_data <- HDB_merged()
    
    # Create a color palette for the map
    pal <- colorNumeric(palette = "Reds", domain = HDB_merged_data$price)
    
    # Create the leaflet map
    leaflet(data = st_as_sf(HDB_merged_data)) %>%
      setView(lng=103.8, lat=1.35, zoom=input$zoomlevel) %>%
      addTiles() %>%
      addPolygons(
        weight = 2,
        stroke = TRUE,
        smoothFactor = 0.1,
        fillOpacity = 0.8,
        color = ~pal(price),
        popup = ~paste0(planning_area, ": $", comma(round(price,-3)))
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)