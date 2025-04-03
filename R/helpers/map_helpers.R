# Map Helper Functions
# This file contains helper functions for map operations

# Initialize a leaflet map with standard configuration
initializeLeafletMap <- function() {
  leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    setView(lng = sg_lng, lat = sg_lat, zoom = sg_zoom) %>%
    addLayersControl(
      baseGroups = c("CartoDB", "Satellite"),
      position = "bottomright",
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addScaleBar(position = "bottomleft")
}

# Add property markers to map
addPropertyMarkers <- function(map_proxy, data, color_palette) {
  # Limit the number of markers for performance
  markers_data <- head(data, 500)
  
  map_proxy %>%
    addCircleMarkers(
      data = markers_data,
      lng = ~longitude, 
      lat = ~latitude,
      radius = 5,
      color = ~color_palette(resale_price),
      stroke = FALSE, 
      fillOpacity = 0.7,
      popup = ~paste(
        "<b>Address:</b>", address, "<br>",
        "<b>Price:</b> $", format(resale_price, big.mark = ","), "<br>",
        "<b>Type:</b>", flat_type, "<br>",
        "<b>Floor Area:</b>", floor_area_sqm, "sqm<br>",
        "<b>Date:</b>", month
      )
    )
}

# Add property heatmap to map
addPropertyHeatmap <- function(map_proxy, data) {
  map_proxy %>%
    addHeatmap(
      data = data,
      lng = ~longitude, 
      lat = ~latitude,
      intensity = ~resale_price,
      blur = 20, 
      max = 0.05,
      radius = 15
    )
}

# Add MRT stations to map
addMrtStations <- function(map_proxy, data) {
  map_proxy %>%
    addCircleMarkers(
      data = data,
      stroke = FALSE,
      radius = 5,
      color = "#1976D2",
      fillOpacity = 0.8,
      popup = ~paste0("<b>Station:</b> ", name)
    )
}

# Add planning areas to map
addPlanningAreas <- function(map_proxy, data) {
  map_proxy %>%
    addPolygons(
      data = data,
      fillColor = "#8BC34A",
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.2,
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.3,
        bringToFront = TRUE
      ),
      popup = ~paste0("<b>Area:</b> ", PLN_AREA_N)
    )
}