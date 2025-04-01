source("./scripts/packages.R")

install_if_missing <- function(pkg) {
  if (!pkg %in% installed.packages()[, "Package"]) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Install missing packages
invisible(sapply(required_packages, install_if_missing))

print("All required packages are installed!")
