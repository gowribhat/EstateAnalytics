source("global.R")
source("ui.R")
source("server/server.R")

ui <- fluidPage()
shinyApp(ui, server)
server <- function(input,output){
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

