// DBA3702 Team 3 - Find Your New Home
// JavaScript functions for handling overlay visibility

$(document).ready(function() {
  // Helper function to safely refresh DataTables
  function refreshDataTable(tableId) {
    var table = $('#' + tableId);
    if (table.length > 0 && $.fn.DataTable.isDataTable('#' + tableId)) {
      table.DataTable().columns.adjust().draw(false);
      return true;
    }
    return false;
  }

  // Handler for showing the transactions overlay
  Shiny.addCustomMessageHandler('showTransactionsOverlay', function(message) {
    console.log("Showing transactions overlay");
    
    // Get left and right overlay widths
    var leftOverlayWidth = $('.left-overlay').outerWidth() + 20;
    var rightOverlayWidth = $('.right-overlay').outerWidth() + 20;
    
    // Calculate available width for the middle overlay
    var windowWidth = $(window).width();
    var availableWidth = windowWidth - leftOverlayWidth - rightOverlayWidth;
    
    // Set the width and position
    $('#transactions_overlay').css({
      'width': availableWidth + 'px',
      'left': leftOverlayWidth + 'px',
      'right': rightOverlayWidth + 'px',
      'transform': 'none' // Remove centering transform
    });
    
    // Show the overlay first
    $('#transactions_overlay').css('display', 'block');
    
    // Force DataTable redraw with multiple attempts
    function attemptTableAdjust(attempt) {
      console.log("Adjusting table, attempt: " + attempt);
      
      // Try to find the DataTable and adjust it
      if ($.fn.DataTable.isDataTable('#building_transactions')) {
        $('#building_transactions').DataTable().columns.adjust().draw(false);
        console.log("DataTable adjusted successfully");
      } else {
        console.log("DataTable not initialized yet");
        
        // Check if the table element exists
        if ($('#building_transactions').length > 0) {
          console.log("Table element exists, trying to trigger Shiny");
          
          // If we can find the table element but DataTable isn't initialized,
          // try to trigger a Shiny update
          Shiny.setInputValue('force_dt_refresh', new Date().getTime());
        }
        
        // Retry a few times with increasing delays
        if (attempt < 5) {
          setTimeout(function() {
            attemptTableAdjust(attempt + 1);
          }, attempt * 100); // Increasing delay
        }
      }
    }
    
    // Start table adjustment attempts immediately and after a small delay
    attemptTableAdjust(1);
    
    // Additional attempt with more delay to ensure it catches late initialization
    setTimeout(function() {
      attemptTableAdjust(1);
    }, 300);
    
    // Update hidden input to track overlay state
    Shiny.setInputValue('overlays_visible', true);
  });
  
  // Handler for hiding the transactions overlay
  Shiny.addCustomMessageHandler('hideTransactionsOverlay', function(message) {
    console.log("Hiding transactions overlay");
    
    // Hide the overlay - simple display none
    $('#transactions_overlay').css('display', 'none');
    
    // Reset the styling when hiding
    $('#transactions_overlay').css({
      'width': '', 
      'left': '50%',
      'transform': 'translateX(-50%)'
    });
    
    // Update hidden input to track overlay state
    if (!$('.overlay.visible').length) {
      Shiny.setInputValue('overlays_visible', false);
    }
  });
  
  // Handler to refresh a specific DataTable
  Shiny.addCustomMessageHandler('refreshDataTable', function(message) {
    var tableId = message.tableId;
    console.log("Refreshing DataTable: " + tableId);
    
    if ($.fn.DataTable.isDataTable('#' + tableId)) {
      $('#' + tableId).DataTable().columns.adjust().draw(false);
      console.log("DataTable refreshed successfully");
    } else {
      console.log("Cannot refresh DataTable - not initialized");
    }
  });

  // Close overlay when clicking the X button
  $(document).on('click', '.close-overlay', function() {
    console.log("Close button clicked");
    var overlay = $(this).closest('.transactions-overlay');
    
    // Hide the overlay
    overlay.css('display', 'none');
    
    // Reset the styling
    overlay.css({
      'width': '',
      'left': '50%',
      'transform': 'translateX(-50%)'
    });
    
    // Update hidden input to track overlay state
    if (!$('.overlay.visible, .transactions-overlay:visible').length) {
      Shiny.setInputValue('overlays_visible', false);
    }
  });
  
  // Adjust overlay positions when window is resized
  $(window).resize(function() {
    // Only adjust if the transactions overlay is visible
    if ($('#transactions_overlay').is(':visible')) {
      var leftOverlayWidth = $('.left-overlay').outerWidth() + 20;
      var rightOverlayWidth = $('.right-overlay').outerWidth() + 20;
      var windowWidth = $(window).width();
      var availableWidth = windowWidth - leftOverlayWidth - rightOverlayWidth;
      
      $('#transactions_overlay').css({
        'width': availableWidth + 'px',
        'left': leftOverlayWidth + 'px',
        'right': rightOverlayWidth + 'px',
        'transform': 'none'
      });
      
      // Refresh any DataTable when resizing
      if ($.fn.DataTable.isDataTable('#building_transactions')) {
        $('#building_transactions').DataTable().columns.adjust().draw(false);
      }
    }
  });
});