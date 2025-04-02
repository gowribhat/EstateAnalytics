sidebar_component <- div(id = "sidebar-card",
    h4("User Preferences"),
    numericInput("budget", "Budget (SGD)", value = 800000, min = 200000, step = 50000),
    selectInput("house_type", "Type of House", choices = c("HDB", "Condo", "Landed")),
    sliderInput("distance_school", "Max Distance to School (km)", min = 0, max = 5, value = 2),
    sliderInput("distance_childcare", "Max Distance to Childcare (km)", min = 0, max = 5, value = 2),
    actionButton("apply_filters", "Apply Filters", class = "btn-primary")
)
