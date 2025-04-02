ui <- fluidPage(
  tags$head(includeCSS("www/styles.css")),
  
  # Top navigation bar for map type selection
  div(id = "top-bar",
      selectInput("dataset", "Choose Data:", choices = c("Schools", "Childcare"), selected = "Schools"),
      actionButton("heatmap", "Heatmap", class = "map-type-btn"),
      actionButton("pointmap", "Point Map", class = "map-type-btn"),
      actionButton("clustered", "Cluster Map", class = "map-type-btn")
  ),
  
  # Sidebar card for user preferences
  div(id = "sidebar-card",
      h4("User Preferences"),
      numericInput("budget", "Budget (SGD)", value = 800000, min = 200000, step = 50000),
      selectInput("house_type", "Type of House", choices = c("HDB", "Condo", "Landed")),
      sliderInput("distance_school", "Max Distance to School (km)", min = 0, max = 5, value = 2),
      sliderInput("distance_childcare", "Max Distance to Childcare (km)", min = 0, max = 5, value = 2),
      actionButton("apply_filters", "Apply Filters", class = "btn-primary")
  ),
  
  # Map container
  div(id = "map-container",
      leafletOutput("map", width = "100%", height = "100%")
  )
)
