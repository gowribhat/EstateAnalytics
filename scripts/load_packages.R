source("./scripts/packages.R")

# Load all packages
lapply(required_packages, library, character.only = TRUE)

print("All required packages are loaded!")
