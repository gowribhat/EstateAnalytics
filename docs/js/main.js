// EstateAnalytics GitHub Pages JS
// Main JavaScript for the EstateAnalytics marketing website

document.addEventListener('DOMContentLoaded', function() {
  // Add fade-in animation to elements
  const animateElements = document.querySelectorAll('.hero-section h1, .hero-section p, .key-benefits .card');
  animateElements.forEach(function(element, index) {
    // Add animation with slight delay for each element
    setTimeout(function() {
      element.classList.add('fade-in');
    }, index * 100);
  });
  
  // Handle smooth scrolling for anchor links
  document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function(e) {
      e.preventDefault();
      
      const targetId = this.getAttribute('href');
      if (targetId === '#') return;
      
      const targetElement = document.querySelector(targetId);
      if (targetElement) {
        targetElement.scrollIntoView({
          behavior: 'smooth',
          block: 'start'
        });
      }
    });
  });
  
  // Add current year to copyright in footer
  const currentYear = new Date().getFullYear();
  const copyrightElements = document.querySelectorAll('.footer .small:last-child');
  copyrightElements.forEach(function(element) {
    element.textContent = element.textContent.replace('2023', currentYear);
  });
});