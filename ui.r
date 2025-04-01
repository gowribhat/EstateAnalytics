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
        # Data Table Tab - Displays the selected dataset
        # TODO: Add filtering/search functionality for users to refine search results
        # TODO: Allow exporting data to CSV for analysis
        tabPanel("Table View", 
                 DTOutput("data_table")),  
        
        # Summary Tab - Displays statistical summary of the dataset
        # TODO: Implement visual analytics (e.g., bar charts, histograms, box plots)
        # TODO: Provide insights into trends (e.g., top schools, price distribution)
        tabPanel("Summary", 
                 verbatimTextOutput("summary")),  
        
        # Map Visualization Tab - Displays locations of schools/childcare centers
        # TODO: Expand to include property price heatmaps, MRT connectivity, and amenities
        # TODO: Enable user-defined filters (e.g., show properties within 1km of MRT)
        tabPanel("Map View", 
                 leafletOutput("map"))  
      )
    )
  )
)
