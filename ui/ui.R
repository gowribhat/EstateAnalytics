source("ui/components/navbar.R")
source("ui/components/sidebar.R")

ui <- fluidPage(
  tags$head(includeCSS("www/styles.css")),
  
  navbar_component,
  sidebar_component,
  
  # Map container
  div(id = "map-container",
      leafletOutput("map", width = "100%", height = "100%")
  )
)
