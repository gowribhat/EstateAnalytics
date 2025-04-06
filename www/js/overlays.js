// DBA3702 Team 3 - Find Your New Home
// JavaScript functions for handling overlay visibility

$(document).ready(function() {
  // Handler for showing the transactions overlay
  Shiny.addCustomMessageHandler('showTransactionsOverlay', function(message) {
    console.log("JS: Showing transactions overlay");
    var overlay = $('#transactions_overlay');
    
    // Make the overlay visible
    overlay.css('display', 'flex');
    
    // Add a small delay before triggering resize to help DataTable initialize properly
    setTimeout(function() {
      $(window).trigger('resize');
      
      // If DataTable exists, adjust columns
      if ($.fn.DataTable.isDataTable('#transactions_table')) {
        $('#transactions_table').DataTable().columns.adjust().draw();
      }
    }, 100);
    
    // Update hidden input to track general overlay state
    Shiny.setInputValue('overlays_visible', true);
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
  
  // Add window resize handler for DataTable adjustments
  $(window).resize(function() {
    // Check if transaction overlay is visible
    if ($('#transactions_overlay').css('display') === 'flex') {
      // If DataTable exists, adjust columns after resize
      if ($.fn.DataTable.isDataTable('#transactions_table')) {
        $('#transactions_table').DataTable().columns.adjust();
      }
    }
  });
});