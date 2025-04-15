source("./scripts/packages.R")

# Function to safely load packages
load_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Package", pkg, "not found. Attempting to install..."))
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.rstudio.com/")
    if (!require(pkg, character.only = TRUE)) {
      stop(paste("Package", pkg, "could not be loaded"))
    }
  }
}

# Load all packages
invisible(sapply(required_packages, load_package))

# Ensure namespace is attached for commonly used functions
if (!"sf" %in% .packages()) library(sf)

print("All required packages are loaded!")
