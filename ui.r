ui <- fluidPage(
  titlePanel("Find Your New Home - Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:", 
                  choices = c("Schools", "Childcare"), 
                  selected = "Schools"),
      actionButton("refresh", "Refresh Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table View", DTOutput("data_table")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Map View", leafletOutput("map"))
      )
    )
  )
)
