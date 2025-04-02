# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)
library(shinyWidgets)
library(leaflet.extras)

# Load cleaned datasets from RDS files
schools_data <- readRDS("./data/cleaned/cleaned_schools.RDS")
childcares_data <- readRDS("./data/cleaned/cleaned_childcares.RDS")


# Define color palettes, function and other constants for the app
