# UI Helper Functions
# This file contains helper functions for generating consistent UI components and plot styling

# Create a standardized analysis box
createAnalysisBox <- function(title, plot_output, height = "300px", status = "primary") {
  box(
    title = title, 
    status = status, 
    solidHeader = TRUE,
    width = NULL,
    plotlyOutput(plot_output, height = height)
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