# Utility Functions
# This file contains helper functions used throughout the application

# UI Helper Functions ----------------------------------------------------

# Standard plotly configuration
configure_plotly <- function(p) {
  p %>% 
    config(displayModeBar = FALSE) %>%
    layout(
      margin = list(l = 50, r = 20, t = 50, b = 50),
      font = list(family = "Arial, sans-serif"),
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)"
    )
}

# Common ggplot theme for all visualizations
theme_property_app <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
}

# Color palette functions for different chart types
price_colors <- function(n) {
  colorRampPalette(c("#1976D2", "#2196F3", "#64B5F6"))(n)
}

area_colors <- function(n) {
  colorRampPalette(c("#388E3C", "#4CAF50", "#81C784"))(n)
}

comparison_colors <- function(n) {
  colorRampPalette(c("#FFA000", "#FFC107", "#FFD54F"))(n)
}

# Function to sort floor ranges in logical order (low to high)
sort_floor_ranges <- function(floor_ranges) {
  # Common pattern for HDB: "01 TO 03", "04 TO 06", etc.
  # Common pattern for condos: "HIGH", "MID", "LOW", etc.
  
  # Numeric extraction function
  extract_lowest_number <- function(x) {
    # Extract first number found in string
    num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x))
    if (is.na(num)) {
      # Handle text-based ranges by priority
      if (grepl("LOW", toupper(x))) return(1)
      if (grepl("MID", toupper(x))) return(2)
      if (grepl("HIGH", toupper(x))) return(3)
      if (grepl("PENT", toupper(x))) return(4)
      return(999) # Default high value for unknown types
    }
    return(num)
  }
  
  # Order the ranges
  floor_ranges[order(sapply(floor_ranges, extract_lowest_number))]
}

# Map Helper Functions --------------------------------------------------

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
