# HomeExplorer Developer Guide

This comprehensive guide explains how to add new features to the application, covering all aspects from server-side components to UI rendering and styling.

## Application Structure Overview

Our application follows a modular architecture:
- `server/components/`: Contains R scripts for different functionalities
- `ui.R`: Defines the user interface layout
- `www/css/`: Contains styling for the application
- `www/js/`: Contains JavaScript for client-side interactions

## Complete Integration Process

### 1. Create the Server Component

**Location**: Add a new `.R` file in `server/components/`

**Example**:
```r
# server/components/property_comparison.R

#' Property Comparison Module
#' 
#' This component provides functionality to compare multiple properties
#' with visualization of key metrics and differences.

# Initialize reactive values
property_comparison <- reactiveVal(NULL)

# Create observers for user interactions
observeEvent(input$compare_properties, {
  selected_props <- input$property_selection
  
  if(length(selected_props) > 1) {
    comparison_data <- generate_comparison_data(selected_props)
    property_comparison(comparison_data)
  }
})

# Render comparison output
output$comparison_results <- renderUI({
  req(property_comparison())
  # Render comparison UI elements
})
```

### 2. Update Server Integration

Your component will be automatically included thanks to our dynamic loading system:

```r
# This happens in server.R - no changes needed if you follow the structure
component_files <- list.files("server/components", pattern = "\\.R$", full.names = TRUE)
lapply(component_files, function(file) source(file, local = TRUE))
```

### 3. Add UI Elements

**Location**: Update relevant sections in `ui.R` or create a new module

**Example**:
```r
# In ui.R where appropriate
tabPanel(
  "Compare Properties",
  div(class = "comparison-container",
    selectizeInput("property_selection", "Select Properties to Compare", 
                   choices = NULL, multiple = TRUE),
    actionButton("compare_properties", "Compare", class = "btn-primary action-button"),
    uiOutput("comparison_results")
  )
)
```

### 4. Add CSS Styling

**Location**: Add styles to `www/css/custom.css`

**Example**:
```css
/* Property comparison styles */
.comparison-container {
  background-color: white;
  padding: 20px;
  border-radius: 5px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.05);
}

.comparison-chart {
  height: 400px;
  margin: 15px 0;
}

@media (max-width: 768px) {
  .comparison-container {
    padding: 10px;
  }
}
```

### 5. Add JavaScript (if needed)

**Location**: Create or update files in `www/js/`

**Example**:
```js
// www/js/comparison.js
$(document).ready(function() {
  // Handle property comparison UI interactions
  $('#compare_properties').on('click', function() {
    // Show loading indicator
    $('.comparison-container').addClass('loading');
    
    // The actual comparison will be handled by the Shiny server
    // This is just for UI feedback
    setTimeout(function() {
      $('.comparison-container').removeClass('loading');
    }, 500);
  });
});
```

### 6. Test Your Integration

1. Run the application locally
2. Test all user interaction flows
3. Check both desktop and mobile responsiveness
4. Verify that your component works with other existing features

### 7. Documentation

1. Add inline comments to explain complex logic
2. Update README.md if adding major features
3. Document any new dependencies in packages.R

## Integration Checklist

- [ ] Server component created in the correct location
- [ ] Reactive values and observers properly defined
- [ ] UI elements added and properly connected to server logic
- [ ] CSS styling added for all new UI elements
- [ ] JavaScript interactions added (if needed)
- [ ] Responsive design implemented
- [ ] All features tested on desktop
- [ ] Code follows project's style conventions
- [ ] Documentation updated

## Troubleshooting Common Issues

1. **Component not loading**: Ensure file is in the correct directory with `.R` extension
2. **UI not showing**: Check for typos in output IDs between UI and server
3. **Styling not applying**: Verify CSS class names match between UI and CSS file
4. **JavaScript not working**: Check browser console for errors

## Best Practices

- **Modularization**: One component = one specific functionality
- **Error Handling**: Always use `req()` for dependencies and `tryCatch` for operations that might fail
- **Responsive Design**: Test all features on different screen sizes
- **Performance**: Optimize data operations and limit reactivity chains
- **Collaboration**: Create a GitHub branch for your feature and submit a PR when ready