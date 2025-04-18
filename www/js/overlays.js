// DBA3702 Team 3 - Find Your New Home
// JavaScript functions for handling overlay visibility

const SAFETY_MARGIN = 40; // Define a constant for the safety margin

$(document).ready(function() {
  // Handler for showing the right overlay with building details
  Shiny.addCustomMessageHandler('showRightOverlay', function(message) {
    console.log("JS: Showing right overlay with building details");
    var overlay = $('#right_overlay');
    
    // Set overlay as not ready initially (shows loading state)
    Shiny.setInputValue('right_overlay_ready', false, {priority: 'event'});
    
    // Make the overlay visible
    overlay.css('display', 'block');
    
    // Update hidden input to track general overlay state
    Shiny.setInputValue('overlays_visible', true);
    
    // Trigger window resize to ensure proper layout
    $(window).trigger('resize');

    // After a longer delay (matching the transactions overlay delay), mark overlay as ready
    setTimeout(function() {
      console.log("JS: Setting right overlay ready");
      // Signal readiness to show content
      Shiny.setInputValue('right_overlay_ready', true, {priority: 'event'});
      // Trigger visual change to help ensure plots render correctly
      $(document).trigger('shiny:visualchange');
    }, 800); // Longer delay to ensure DOM is ready
  });

  // Handler for hiding the right overlay
  Shiny.addCustomMessageHandler('hideRightOverlay', function(message) {
    console.log("JS: Hiding right overlay");
    var overlay = $('#right_overlay');
    
    // Hide the overlay
    overlay.css('display', 'none');
    
    // Update hidden input to track overlay state
    if ($('.left-overlay:visible, #transactions_overlay:visible').length === 0) {
      Shiny.setInputValue('overlays_visible', false);
    } else {
      Shiny.setInputValue('overlays_visible', true);
    }
  });
  
  // Handler for showing the analytics dashboard overlay
  Shiny.addCustomMessageHandler('showTransactionsOverlay', function(message) {
    console.log("JS: Showing analytics dashboard overlay");
    var overlay = $('#transactions_overlay');
    
    // Set dashboard as not ready initially
    Shiny.setInputValue('analytics_dashboard_ready', false);
    
    // Make the overlay visible immediately with the loading indicator
    overlay.css('display', 'flex');
    
    // Update hidden input to track general overlay state
    Shiny.setInputValue('overlays_visible', true);
    
    // Force Shiny to recalculate the analytics_dashboard UI immediately
    Shiny.setInputValue('refresh_analytics_dashboard', Date.now(), {priority: 'event'});
    
    // Trigger window resize to ensure proper layout
    $(window).trigger('resize');
    
    // After a short delay, trigger a more comprehensive update 
    // This ensures plots and other elements render properly
    setTimeout(function() {
      // Force Shiny to update all reactive elements
      Shiny.setInputValue('refresh_analytics_dashboard', Date.now() + 1, {priority: 'event'});
      
      // Notify Shiny that the dashboard is ready to be shown
      Shiny.setInputValue('analytics_dashboard_ready', true);
      
      // If there are any plots, manually trigger their update
      $(document).trigger('shiny:visualchange');
      
      // Fix scrolling with an additional small delay to allow content to load
      setTimeout(function() {
        // Calculate proper height for controls container
        var overlayHeight = $('#transactions_overlay').height();
        var headerHeight = $('#transactions_overlay > div:first-child').outerHeight(true);
        var containerPadding = 20; // Account for container padding
        var safeHeight = overlayHeight - headerHeight - containerPadding - SAFETY_MARGIN; // Use the constant for safety margin
        
        // Apply calculated height to controls
        $('.transactions-overlay .col-sm-4 > div').css({
          'height': safeHeight + 'px',
          'max-height': safeHeight + 'px',
          'overflow-y': 'auto',
          'overflow-x': 'hidden',
          'padding-bottom': '20px'
        });
        
        // Ensure the dashboard content can scroll properly
        $('.dashboard-content-scroll').css('height', 'calc(100% - 10px)');
      }, 200);
    }, 800); // Longer delay to ensure dashboard is fully rendered

    // Initial sizing for the analytics controls
    function adjustAnalyticsControlsHeight() {
      var overlayHeight = $('#transactions_overlay').height();
      var headerHeight = $('#transactions_overlay > div:first-child').outerHeight(true);
      var containerPadding = 20; // Account for container padding
      var safeHeight = overlayHeight - headerHeight - containerPadding - SAFETY_MARGIN; // Use the constant for safety margin
      
      // Apply calculated height to controls
      $('.transactions-overlay .col-sm-4 > div').css({
        'height': safeHeight + 'px',
        'max-height': safeHeight + 'px',
        'overflow-y': 'auto',
        'overflow-x': 'hidden'
      });
    }
    
    // Initial adjustment
    setTimeout(adjustAnalyticsControlsHeight, 300);
    
    // Attach resize handler for the analytics controls
    $(window).off('resize.analyticsControls').on('resize.analyticsControls', function() {
      if ($('#transactions_overlay').is(':visible')) {
        adjustAnalyticsControlsHeight();
      }
    });
  });

  // Handler for hiding the transactions overlay
  Shiny.addCustomMessageHandler('hideTransactionsOverlay', function(message) {
    console.log("JS: Hiding transactions overlay");
    var overlay = $('#transactions_overlay');

    // Set display to none to hide
    overlay.css('display', 'none');

    // Update hidden input to track overlay state
    if ($('.left-overlay:visible, .right-overlay:visible, #transactions_overlay:visible').length === 0) {
      Shiny.setInputValue('overlays_visible', false);
    } else {
      Shiny.setInputValue('overlays_visible', true);
    }

    // Remove the resize handler when hiding the overlay
    $(window).off('resize.analyticsControls');
  });
  
  // Close overlay when clicking the X button
  $(document).on('click', '.close-overlay', function() {
    console.log("JS: Close button clicked");
    var overlay = $(this).closest('.transactions-overlay');

    // Set display to none to hide
    overlay.css('display', 'none');

    // Trigger the Shiny input associated with the close button if it exists
    var closeButtonId = $(this).attr('id');
    if (closeButtonId) {
       Shiny.setInputValue(closeButtonId, Date.now(), {priority: 'event'}); // Trigger the R observer
    }

    // Update general overlay visibility state
    if ($('.left-overlay:visible, .right-overlay:visible, #transactions_overlay:visible').length === 0) {
      Shiny.setInputValue('overlays_visible', false);
    } else {
      Shiny.setInputValue('overlays_visible', true);
    }
  });
  
  // Store the overlay visibility state before responsive hiding
  let savedOverlayState = {
    leftOverlayVisible: false,
    rightOverlayVisible: false,
    transactionsOverlayVisible: false
  };
  // Track if the window was previously below the responsive threshold
  // Initialize based on current width
  let wasBelowThreshold = $(window).width() < 768; 

  // Add window resize handler for DataTable adjustments and responsive overlay behavior
  $(window).resize(function() {
    console.log("Resize event triggered."); // Log resize event

    // DataTable adjustments (existing code)
    if ($('#transactions_overlay').css('display') === 'flex') {
      if ($.fn.DataTable.isDataTable('#transactions_table')) {
        $('#transactions_table').DataTable().columns.adjust();
      }
    }

    // Responsive behavior for overlays
    const windowWidth = $(window).width();
    const RESPONSIVE_THRESHOLD = 768;
    const isBelowThreshold = windowWidth < RESPONSIVE_THRESHOLD;
    // More detailed logging
    console.log(`Current width: ${windowWidth}, Threshold: ${RESPONSIVE_THRESHOLD}, Below: ${isBelowThreshold}, Was Below: ${wasBelowThreshold}`); 

    if (isBelowThreshold) {
      // Only save state and hide if we are transitioning from above to below the threshold
      if (!wasBelowThreshold) {
          console.log("Transitioning to BELOW threshold. Saving state and hiding overlays.");
          savedOverlayState = {
            leftOverlayVisible: $('.left-overlay').is(':visible'),
            rightOverlayVisible: $('.right-overlay').is(':visible'),
            transactionsOverlayVisible: $('#transactions_overlay').is(':visible')
          };
          console.log('Saved overlay state:', savedOverlayState);

          // Hide all overlays
          $('.left-overlay, .right-overlay, #transactions_overlay').each(function() {
            if ($(this).is(':visible')) {
              $(this).hide();
              // Improved logging for identifying the overlay
              console.log(`Hiding overlay: #${$(this).attr('id') || $(this).attr('class').split(' ')[0]}`); 
            }
          });

          // Update Shiny input and state tracker
          Shiny.setInputValue('overlays_visible', false);
          wasBelowThreshold = true; // Update state AFTER processing
      } else {
          console.log("Already below threshold. No state change needed.");
      }

    } else { // isAboveThreshold (windowWidth >= RESPONSIVE_THRESHOLD)
      // Only restore if we are transitioning from below to above the threshold
      if (wasBelowThreshold) {
        console.log("Transitioning to ABOVE threshold. Restoring overlays based on saved state.");
        console.log('Using saved state:', savedOverlayState);

        let anyRestored = false; // Track if any overlay was actually restored

        // Restore Left Overlay if it was visible before hiding
        if (savedOverlayState.leftOverlayVisible) {
          $('.left-overlay').show();
          console.log('Restoring left overlay. Display:', $('.left-overlay').css('display'));
          anyRestored = true;
        }

        // Restore Right Overlay if it was visible before hiding
        if (savedOverlayState.rightOverlayVisible) {
          $('.right-overlay').show();
          console.log('Restoring right overlay. Display:', $('.right-overlay').css('display'));
          anyRestored = true;
        }

        // Restore Transactions Overlay if it was visible before hiding
        if (savedOverlayState.transactionsOverlayVisible) {
          $('#transactions_overlay').css('display', 'flex');
          console.log('Restoring transactions overlay. Display:', $('#transactions_overlay').css('display'));
          anyRestored = true;
        }

        // Update Shiny input only if an overlay was restored or if any overlay is now visible
        if (anyRestored || $('.left-overlay:visible, .right-overlay:visible, #transactions_overlay:visible').length > 0) {
            // Check current Shiny value to avoid redundant updates
            if (!Shiny.shinyapp.$inputValues.overlays_visible) { 
                Shiny.setInputValue('overlays_visible', true);
                console.log("Set overlays_visible to true.");
            }
        } else {
            // If nothing was restored and nothing is visible, ensure overlays_visible is false
             if (Shiny.shinyapp.$inputValues.overlays_visible) { // Avoid redundant updates
                 Shiny.setInputValue('overlays_visible', false);
                 console.log("No overlays restored/visible, set overlays_visible to false.");
             }
        }

        // Update state tracker AFTER processing
        wasBelowThreshold = false;

      } else {
        console.log("Already above threshold. No state change needed.");
        // Ensure overlays_visible reflects current state even if no transition occurred
        // This handles cases where overlays might be shown/hidden by other means while above threshold
        const anyVisible = $('.left-overlay:visible, .right-overlay:visible, #transactions_overlay:visible').length > 0;
        if (anyVisible && !Shiny.shinyapp.$inputValues.overlays_visible) { // Avoid redundant updates
            Shiny.setInputValue('overlays_visible', true);
            console.log("Ensured overlays_visible is true (already above threshold).");
        } else if (!anyVisible && Shiny.shinyapp.$inputValues.overlays_visible) { // Avoid redundant updates
            Shiny.setInputValue('overlays_visible', false);
            console.log("Ensured overlays_visible is false (already above threshold).");
        }
      }
    }
    console.log("Resize handler finished."); // Log end of handler
  });

  // Trigger resize handler once on load to set initial state correctly
  // Use a small delay to ensure Shiny inputs are available
  setTimeout(function() {
      console.log("Triggering initial resize check on load.");
      $(window).trigger('resize');
  }, 100); 

}); // End document.ready