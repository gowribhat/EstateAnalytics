# Dashboard Module
# This module handles the analytics dashboard and property information panels

dashboardModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             h3("Property Analytics Dashboard", class = "dashboard-title"),
             tabsetPanel(
               tabPanel("Price Analysis",
                        fluidRow(
                          column(6,
                                 box(
                                   title = "Median Prices by Area", 
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   width = NULL,
                                   plotlyOutput(ns("median_price_plot"), height = "300px")
                                 )
                          ),
                          column(6,
                                 box(
                                   title = "Price Trends Over Time", 
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   width = NULL,
                                   plotlyOutput(ns("price_trends_plot"), height = "300px")
                                 )
                          )
                        ),
                        fluidRow(
                          column(12,
                                 box(
                                   title = "Price Distribution by Property Type", 
                                   status = "info", 
                                   solidHeader = TRUE,
                                   width = NULL,
                                   plotlyOutput(ns("price_distribution_plot"), height = "300px")
                                 )
                          )
                        )
               ),
               tabPanel("Neighborhood Explorer",
                        fluidRow(
                          column(6,
                                 box(
                                   title = "Amenities Proximity Analysis", 
                                   status = "success", 
                                   solidHeader = TRUE,
                                   width = NULL,
                                   plotlyOutput(ns("amenities_plot"), height = "300px")
                                 )
                          ),
                          column(6,
                                 box(
                                   title = "Transportation Accessibility", 
                                   status = "success", 
                                   solidHeader = TRUE,
                                   width = NULL,
                                   plotlyOutput(ns("transportation_plot"), height = "300px")
                                 )
                          )
                        ),
                        fluidRow(
                          column(12,
                                 box(
                                   title = "Area Comparison", 
                                   status = "warning", 
                                   solidHeader = TRUE,
                                   width = NULL,
                                   plotlyOutput(ns("area_comparison_plot"), height = "300px")
                                 )
                          )
                        )
               ),
               tabPanel("Property Listings",
                        fluidRow(
                          column(12,
                                 box(
                                   title = "Property Search Results", 
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   width = NULL,
                                   DTOutput(ns("property_table")),
                                   downloadButton(ns("download_data"), "Download Data")
                                 )
                          )
                        )
               )
             )
      )
    )
  )
}

dashboardModule <- function(input, output, session, map_data) {
  ns <- session$ns
  
  # Median price by area plot
  output$median_price_plot <- renderPlotly({
    req(map_data$filtered_data)
    
    price_by_area <- map_data$filtered_data %>%
      group_by(town) %>%
      summarise(median_price = median(resale_price, na.rm = TRUE),
                count = n()) %>%
      filter(count > 5) %>%  # Filter out areas with few data points
      arrange(desc(median_price))
    
    p <- ggplot(price_by_area, aes(x = reorder(town, median_price), y = median_price/1000)) +
      geom_bar(stat = "identity", fill = "#2196F3") +
      theme_minimal() +
      coord_flip() +
      labs(x = "", y = "Median Price (SGD Thousands)", 
           title = "Median Property Prices by Area") +
      theme(axis.text.y = element_text(size = 8))
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Price trends plot
  output$price_trends_plot <- renderPlotly({
    req(map_data$filtered_data)
    
    data <- map_data$filtered_data %>%
      mutate(month = as.Date(paste0(month, "-01"))) %>%
      group_by(month) %>%
      summarise(median_price = median(resale_price, na.rm = TRUE))
    
    p <- ggplot(data, aes(x = month, y = median_price/1000)) +
      geom_line(color = "#4CAF50", size = 1) +
      geom_point(color = "#4CAF50", size = 2) +
      theme_minimal() +
      labs(x = "Month", y = "Median Price (SGD Thousands)", 
           title = "Price Trends Over Time") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Price distribution by property type plot
  output$price_distribution_plot <- renderPlotly({
    req(map_data$filtered_data)
    
    p <- ggplot(map_data$filtered_data, aes(x = flat_type, y = resale_price/1000, fill = flat_type)) +
      geom_boxplot() +
      theme_minimal() +
      scale_fill_viridis_d() +
      labs(x = "Property Type", y = "Price (SGD Thousands)", 
           title = "Price Distribution by Property Type") +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Placeholder plots for neighborhood explorer (to be replaced with actual data)
  output$amenities_plot <- renderPlotly({
    plot_ly(type = "bar") %>%
      add_trace(x = c("Supermarket", "Parks", "Restaurants", "Schools"), 
                y = c(3, 5, 8, 2),
                marker = list(color = "#8BC34A")) %>%
      layout(title = "Number of Amenities Within 1km",
             xaxis = list(title = ""),
             yaxis = list(title = "Count"))
  })
  
  output$transportation_plot <- renderPlotly({
    plot_ly(type = "bar") %>%
      add_trace(x = c("MRT Station", "Bus Stop", "Highway Access"), 
                y = c(0.8, 0.3, 1.5),
                marker = list(color = "#FF9800")) %>%
      layout(title = "Distance to Transportation (km)",
             xaxis = list(title = ""),
             yaxis = list(title = "Distance (km)"))
  })
  
  output$area_comparison_plot <- renderPlotly({
    # Radar chart data for area comparison
    categories <- c('Price', 'Transport', 'Schools', 'Parks', 'Shopping')
    
    plot_ly(
      type = 'scatterpolar',
      r = c(0.8, 0.9, 0.7, 0.6, 0.8),
      theta = categories,
      fill = 'toself',
      name = 'Area A'
    ) %>% add_trace(
      r = c(0.7, 0.8, 0.9, 0.8, 0.5),
      theta = categories,
      fill = 'toself',
      name = 'Area B'
    ) %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 1)
        )
      ),
      title = "Area Feature Comparison",
      showlegend = TRUE
    )
  })
  
  # Property table output
  output$property_table <- renderDT({
    req(map_data$filtered_data)
    
    # Prepare data for display
    display_data <- map_data$filtered_data %>%
      select(address, town, flat_type, floor_area_sqm, resale_price, storey_range, month) %>%
      mutate(
        resale_price = sprintf("$%s", format(resale_price, big.mark = ",", scientific = FALSE)),
        floor_area_sqm = sprintf("%s sqm", floor_area_sqm)
      ) %>%
      rename(
        "Address" = address,
        "Town" = town,
        "Type" = flat_type,
        "Floor Area" = floor_area_sqm,
        "Price" = resale_price,
        "Storey Range" = storey_range,
        "Transaction Date" = month
      )
    
    datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("property_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(map_data$filtered_data, file, row.names = FALSE)
    }
  )
}