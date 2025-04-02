navbar_component <- div(id = "navbar",
    selectInput("dataset", "Choose Data:", choices = c("Schools", "Childcare"), selected = "Schools"),
    actionButton("heatmap", "Heatmap", class = "map-type-btn"),
    actionButton("pointmap", "Point Map", class = "map-type-btn"),
    actionButton("clustered", "Cluster Map", class = "map-type-btn")
)
