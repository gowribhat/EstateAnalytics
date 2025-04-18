# Find Your New Home: Data-Driven Property Search Platform
# UI File
# DBA3702 Team 3

# Load packages - will be handled by global.R but added here for clarity
# library(shiny)
# library(shinydashboard)
# library(shinythemes)
# library(leaflet)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),

  # Custom CSS for modern design
  tags$head(
    tags$link(
      rel  = "stylesheet",
      type = "text/css",
      href = "css/custom.css"
    ),
    tags$script(
      src = "shared/datatables/js/jquery.dataTables.min.js"
    ),
    tags$script(
      src = "shared/datatables/js/dataTables.bootstrap.min.js"
    ),
    tags$link(
      rel  = "stylesheet",
      type = "text/css",
      href = "shared/datatables/css/dataTables.bootstrap.css"
    ),
    tags$script(
      src = "js/overlays.js"
    ),

    # Screen dimension detection
    tags$script(HTML(
      "
// Function to send screen dimensions to Shiny server
function sendScreenDimensions() {
  var width  = window.innerWidth  || document.documentElement.clientWidth;
  var height = window.innerHeight || document.documentElement.clientHeight;
  Shiny.setInputValue('screenDimensions', {
    width: width,
    height: height,
    pixelRatio: window.devicePixelRatio || 1
  });
}

// Handle server requests for dimensions
Shiny.addCustomMessageHandler('getScreenDimensions', function(msg) {
  sendScreenDimensions();
});

// Enforce minimum zoom logic
Shiny.addCustomMessageHandler('setMinZoom', function(message) {
  // Get zoom level from message
  var minZoom = message.zoom;

  setTimeout(function() {
    // Find the Leaflet map instance - more robust method
    var mapElement = document.querySelector('.leaflet-container');
    if (!mapElement) return;

    // Find the map instance in the Leaflet registry
    for (var id in L.map._instances) {
      var map = L.map._instances[id];
      if (map && map._container === mapElement) {
        console.log('Setting min zoom to: ' + minZoom);

        // Set options directly
        map.options.minZoom = minZoom;

        // Force current zoom to match min zoom (prevents zooming out)
        if (map.getZoom() < minZoom) {
          map.setZoom(minZoom);
        }

        // Override the setZoom method to enforce minimum zoom
        var originalSetZoom = map.setZoom;
        map.setZoom = function(zoom, options) {
          if (zoom < this.options.minZoom) {
            zoom = this.options.minZoom;
          }
          return originalSetZoom.call(this, zoom, options);
        };

        // Also intercept zoom events
        map.off('zoom');
        map.on('zoom', function() {
          if (map.getZoom() < map.options.minZoom) {
            map.setZoom(map.options.minZoom);
          }
        });

        break;
      }
    }
  }, 500); // Small delay to ensure map is fully initialized
});

// Send dimensions on page load and window resize
document.addEventListener('DOMContentLoaded', function() {
  sendScreenDimensions();

  // Also send when window is resized
  window.addEventListener('resize', function() {
    sendScreenDimensions();
  });
});
      "
    )),

    # Custom inline CSS
    tags$style(HTML(
      "
body, html {
  height: 100%;
  margin: 0;
  overflow: hidden;
  font-family: 'Roboto', sans-serif;
}
.map-container {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  z-index: 1;
}
.top-filters {
  position: absolute;
  top: 10px;
  left: 50%;
  transform: translateX(-50%);
  z-index: 1000;
  display: flex;
  gap: 10px;
}
.top-filters .btn {
  border-radius: 20px;
  transition: all 0.3s ease;
}
.top-filters .btn:hover {
  background-color: #007bff;
  color: white;
  transform: scale(1.1);
}
.left-overlay, .right-overlay {
  position: absolute;
  top: 50px;
  bottom: 10px;
  width: 300px;
  background: rgba(255, 255, 255, 0.9);
  border-radius: 15px;
  padding: 15px;
  overflow: hidden; /* Prevent outer scrolling */
  z-index: 1000;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
  transition: all 0.3s ease;
}
.left-overlay:hover, .right-overlay:hover {
  box-shadow: 0 8px 16px rgba(0, 0, 0, 0.3);
}
.left-overlay {
  left: 10px;
}
.right-overlay {
  right: 10px;
}
/* Transactions overlay styling */
#transactions_overlay {
  position: absolute;
  bottom: 10px;
  left: 50%;
  transform: translateX(-50%);
  width: calc(100% - 640px); /* Adjusted width */
  max-width: 900px;
  height: 60%;      /* Fixed dashboard height */
  max-height: 60%;  /* Prevent growing beyond */
  background: rgba(255, 255, 255, 0.95);
  border-radius: 15px;
  padding: 15px;
  z-index: 1001;
  box-shadow: 0 -4px 8px rgba(0, 0, 0, 0.2);
  display: none; /* Use display: none initially */
  flex-direction: column;
  overflow: hidden; /* Prevent the main overlay from scrolling */
}

/* Style the container holding the conditional panels */
#analytics_dashboard_container {
  flex-grow: 1; /* Allow container to fill space */
  overflow: hidden; /* Prevent this container itself from scrolling */
  /* Background, padding, border-radius are applied via inline style */
}

/* Style the inner div that should scroll (inside the conditionalPanel) */
#analytics_dashboard_container .conditionalPanel > div {
   /* Height and overflow are set via inline style */
   /* Ensure no conflicting styles here */
}

/* Ensure DataTable takes full width */
#building_transactions .dataTables_wrapper {
    width: 100%;
}
@keyframes fadeIn {
  from { opacity: 0; }
  to   { opacity: 1; }
}
      "
    ))
  ),

  # Full-screen map
  div(
    class = "map-container",
    leafletOutput(
      outputId = "property_map",
      width    = "100%",
      height   = "100%"
    )
  ),

  # Top filters
  div(
    class = "top-filters",
    actionButton(
      inputId = "filter_house_type",
      label   = "House Type",
      class   = "btn btn-primary"
    ),
    actionButton(
      inputId = "filter_budget",
      label   = "Budget",
      class   = "btn btn-primary"
    ),
    actionButton(
      inputId = "filter_area",
      label   = "Area",
      class   = "btn btn-primary"
    ),
    actionButton(
      inputId = "filter_floor_height",
      label   = "Floor Height",
      class   = "btn btn-primary"
    ),
    actionButton(
      inputId = "filter_facility",
      label   = "Facility",
      class   = "btn btn-primary"
    )
  ),

  # Left overlay: Summary
  div(
    class = "left-overlay",
    # Main content area (scrollable)
    div(
      style = "height: calc(100% - 70px); display: flex; flex-direction: column; overflow-y: auto; padding-bottom: 10px;",
      h4("Area Summary"),
      h5(textOutput("current_region_name", inline = TRUE)),
      # Modified plot output without transparent overlay
      div(
        style = "position: relative;",
        plotOutput("summary_plot", height = "150px")
        # Removed the transparent overlay div
      ),
      # uiOutput("income_stats"), # Keep commented or remove
      # Replace plotlyOutput("income_donut") with the new uiOutput
      uiOutput("income_display_ui"), # Conditionally renders plot or text message
      uiOutput("facility_summary") # Added facility summary here
    ),
    # Price legend at the absolute bottom (fixed position)
    div(
      style = "position: absolute; bottom: 10px; left: 15px; right: 15px; height: 60px;",
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
        uiOutput("property_details") # This now contains the facility plot
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