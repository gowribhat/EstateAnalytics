# Find Your New Home: Data-Driven Property Search Platform
# UI File
# DBA3702 Team 3

# Load packages - will be handled by global.R but added here for clarity
# library(shiny)
# library(shinydashboard)
# library(shinythemes)
# library(leaflet)
library(shinyjs) # Add this line

ui <- fluidPage(
  useShinyjs(), # Add this line to enable shinyjs functions
  # Custom CSS for modern design
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    # Explicitly load DT dependencies
    tags$script(src = "shared/datatables/js/jquery.dataTables.min.js"),
    tags$script(src = "shared/datatables/js/dataTables.bootstrap.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "shared/datatables/css/dataTables.bootstrap.css"),
    # Load our custom overlays.js file
    tags$script(src = "js/overlays.js"),
    tags$style(HTML("\n      body, html {\n        height: 100%;\n        margin: 0;\n        overflow: hidden;\n        font-family: 'Roboto', sans-serif;\n      }\n      .map-container {\n        position: absolute;\n        top: 0;\n        left: 0;\n        right: 0;\n        bottom: 0;\n        z-index: 1;\n      }\n      .top-filters {\n        position: absolute;\n        top: 10px;\n        left: 50%;\n        transform: translateX(-50%);\n        z-index: 1000;\n        display: flex;\n        gap: 10px;\n      }\n      .top-filters .btn {\n        border-radius: 20px;\n        transition: all 0.3s ease;\n      }\n      .top-filters .btn:hover {\n        background-color: #007bff;\n        color: white;\n        transform: scale(1.1);\n      }\n      .left-overlay, .right-overlay {\n        position: absolute;\n        top: 50px;\n        bottom: 10px;\n        width: 300px;\n        background: rgba(255, 255, 255, 0.9);\n        border-radius: 15px;\n        padding: 15px;\n        /* REMOVED overflow-y: auto; */\n        overflow: hidden; /* Prevent outer scrolling */\n        z-index: 1000;\n        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);\n        transition: all 0.3s ease;\n      }\n      .left-overlay:hover, .right-overlay:hover {\n        box-shadow: 0 8px 16px rgba(0, 0, 0, 0.3);\n      }\n      .left-overlay {\n        left: 10px;\n      }\n      .right-overlay {\n        right: 10px;\n      }\n      /* Transactions overlay styling */\n      #transactions_overlay {\n        position: absolute;\n        bottom: 10px;\n        left: 50%;\n        transform: translateX(-50%);\n        width: calc(100% - 640px); /* Adjusted width */\n        max-width: 900px;\n        height: 60%; /* Increased height for analytics dashboard */\n        background: rgba(255, 255, 255, 0.95);\n        border-radius: 15px;\n        padding: 15px;\n        z-index: 1001;\n        box-shadow: 0 -4px 8px rgba(0, 0, 0, 0.2);\n        display: none; /* Use display: none initially */\n        flex-direction: column;\n        overflow: hidden; /* Prevent the main overlay from scrolling */\n      }\n\n      /* Style the container holding the conditional panels */\n      #analytics_dashboard_container {\n        flex-grow: 1; /* Allow container to fill space */\n        overflow: hidden; /* Prevent this container itself from scrolling */\n        /* Background, padding, border-radius are applied via inline style */\n      }\n\n      /* Style the inner div that should scroll (inside the conditionalPanel) */\n      #analytics_dashboard_container .conditionalPanel > div {\n         /* Height and overflow are set via inline style */\n         /* Ensure no conflicting styles here */\n      }\n\n      /* Ensure DataTable takes full width */\n      #building_transactions .dataTables_wrapper {\n          width: 100%;\n      }\n      @keyframes fadeIn {\n        from { opacity: 0; }\n        to { opacity: 1; }\n      }\n    ")),
    # Add JavaScript for responsive overlay behavior
    tags$script(HTML("
      function checkWindowSize() {
        var windowWidth = window.innerWidth;
        // Select only the left overlay for automatic hiding/showing based on width
        var leftOverlay = document.querySelector('.left-overlay');
        var rightOverlay = document.getElementById('right_overlay'); // Keep track of right overlay state

        // If window width is less than 1000px, hide the left overlay
        if (windowWidth < 1000) {
          if (leftOverlay) {
            leftOverlay.style.display = 'none';
          }
          // Update visibility state only if right overlay is also hidden
          if (rightOverlay && rightOverlay.style.display === 'none') {
            Shiny.setInputValue('overlays_visible', false);
          } else {
            Shiny.setInputValue('overlays_visible', true); // Right overlay might still be visible
          }
        } else {
          // If window width is >= 1000px, show the left overlay
          if (leftOverlay) {
            // Reset CSS properties for the left overlay
            leftOverlay.style.display = 'block';
            leftOverlay.style.opacity = '1';
            leftOverlay.style.background = 'rgba(255, 255, 255, 0.9)';
            // Force a repaint
            leftOverlay.offsetHeight;
          }
          // Always set overlays_visible to true if window is wide enough,
          // as either left or right (or both) could be visible.
          Shiny.setInputValue('overlays_visible', true);
        }
      }
      
      // Run on page load
      window.addEventListener('load', checkWindowSize);
      
      // Run whenever the window is resized
      window.addEventListener('resize', checkWindowSize);
      
      // Run when map is panned or zoomed to ensure correct overlay appearance
      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(function() {
          var leafletMap = document.querySelector('.leaflet-map-pane');
          if (leafletMap) {
            var observer = new MutationObserver(function() {
              var overlays = document.querySelectorAll('.left-overlay, .right-overlay');
              if (window.innerWidth >= 1200) {
                overlays.forEach(function(overlay) {
                  // Refresh overlay appearance when map changes
                  overlay.style.background = 'rgba(255, 255, 255, 0.9)';
                });
              }
            });
            
            observer.observe(leafletMap, {
              attributes: true,
              childList: true,
              subtree: true
            });
          }
        }, 1000); // Small delay to ensure map is loaded
      });
    "))
  ),

  # Full-screen map
  div(class = "map-container", leafletOutput("property_map", width = "100%", height = "100%")),

  # Top filters
  div(
    class = "top-filters",
    actionButton("filter_house_type", "House Type", class = "btn btn-primary"),
    actionButton("filter_budget", "Budget", class = "btn btn-primary"),
    actionButton("filter_area", "Area", class = "btn btn-primary"),
    actionButton("filter_floor_height", "Floor Height", class = "btn btn-primary"),
    actionButton("filter_facility", "Facility", class = "btn btn-primary")
  ),

  # Left overlay: Summary
  div(
    class = "left-overlay",
    # Main content area (scrollable)
    div(
      # Adjust height to account for the fixed legend below, add overflow for scrolling
      style = "height: calc(100% - 70px); display: flex; flex-direction: column; overflow-y: auto; padding-bottom: 10px;", # Adjusted height, added overflow-y and padding-bottom
      h4("Area Summary"),
      h5(textOutput("current_region_name", inline = TRUE)),
      plotOutput("summary_plot", height = "150px"),

      # Add household income statistics
      htmlOutput("income_stats")
    ),
    # Price legend at the absolute bottom (fixed position)
    div(
      # Give the legend container a fixed height
      style = "position: absolute; bottom: 10px; left: 15px; right: 15px; height: 60px;", # Added fixed height
      htmlOutput("price_legend")
    )
  ),

  # Right overlay: Property details (initially hidden)
  div(
    id = "right_overlay",
    class = "right-overlay",
    style = "display: none;", # Initially hidden
    div(
      style = "height: calc(100% - 10px); display: flex; flex-direction: column;",
      h4("Building Details"),
      
      # Loading UI - shown while content is being prepared
      conditionalPanel(
        condition = "typeof input.right_overlay_ready === 'undefined' || !input.right_overlay_ready",
        div(
          style = "display: flex; align-items: center; justify-content: center; height: 200px; flex-direction: column;",
          tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 24px; color: #4676a9; margin-bottom: 10px;"),
          p("Loading property details...", style = "color: #4676a9;")
        )
      ),
      
      # Property content - hidden initially, shown when ready
      conditionalPanel(
        condition = "typeof input.right_overlay_ready !== 'undefined' && input.right_overlay_ready",
        uiOutput("property_details"),
        plotOutput("building_plot", height = "180px"),
        plotOutput("facility_plot", height = "180px")
      )
    )
  ),
  
  # Building Analytics Dashboard overlay
  div(
    id = "transactions_overlay",
    class = "transactions-overlay", # Class is kept for potential JS selection, ID used for styling
    # Header with title and close button
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; flex-shrink: 0;",
      h4("Building Analytics Dashboard", style = "margin: 0;"),
      actionButton("close_transactions", "×", class = "btn btn-sm close-overlay", style = "border-radius: 50%; width: 30px; height: 30px; display: flex; justify-content: center; align-items: center; font-weight: bold; padding: 0;")
    ),
    # Container for the analytics dashboard content
    div(
      id = "analytics_dashboard_container",
      class = "scrollable-content", # Added class for scrollable content
      style = "height: calc(100% - 50px); background-color: white; border-radius: 0 0 10px 10px; padding: 10px; overflow: hidden;", # Added overflow: hidden inline as well for certainty

      # Loading UI
      conditionalPanel(
        condition = "typeof input.analytics_dashboard_ready === 'undefined' || !input.analytics_dashboard_ready",
        div(
          style = "display: flex; align-items: center; justify-content: center; height: 100%; flex-direction: column;",
          tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 48px; color: #4676a9; margin-bottom: 15px;"),
          h4("Loading building analytics...", style = "color: #4676a9;")
        )
      ),

      # Dashboard UI - hidden by default, shown when ready
      conditionalPanel(
        condition = "typeof input.analytics_dashboard_ready !== 'undefined' && input.analytics_dashboard_ready",
        # This div allows the content generated by uiOutput to scroll vertically
        div(
          class = "dashboard-content-scroll", # Added a class for potential targeting
          style = "height: 100%; overflow-y: auto; overflow-x: hidden;", # Explicitly hide horizontal scroll
          uiOutput("analytics_dashboard")
        )
      )
    )
  ),
  
  # Hidden input to track overlay visibility
  tags$input(id = "overlays_visible", type = "hidden", value = "false")
)