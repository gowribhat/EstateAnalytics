# PropExplorer

> A Comprehensive Property Search & Analysis Platform

## Overview

This project aims to develop a data analytics platform that revolutionizes how people search for and evaluate properties in Singapore. By leveraging multiple government data sources and advanced visualizations, the platform will provide insights into property values, neighborhood characteristics, and lifestyle factors to help users make informed housing decisions.

## Tech Stack

- **R & Shiny** – Interactive web application
- **ggplot2** – Data visualization
- **leaflet** – Interactive maps
- **dplyr & stringr** – Data cleaning & transformation
- **httr** – API connections

## Project Structure

```
Project/
├── app.R                                     # Main Shiny app entry point
├── global.R                                  # Shared libraries & configurations
├── server.R                                  # Server-side logic
├── ui.R                                      # UI layout
├── Project.Rproj                             # RStudio project file
│
├── data/
│   ├── raw/                                  # Raw datasets
│   │   ├── Generalinformationofschools.csv
│   │   ├── ListingofCentres.csv
│   │   └── ...
│   │
│   └── cleaned/                              # Cleaned datasets (R Data Serialization files)
│       ├── cleaned_schools.RDS
│       └── cleaned_childcares.RDS
│
├── scripts/
│   └── clean_data.R                          # Raw data cleaning functions
│
└── rsconnect/                                # Shinyapps.io deployment configs
    └── shinyapps.io/
        └── <shiny_username>/
            └── <R_Project_name>.dcf
```

## Getting Started

1.  **Install dependencies**

    Run script to install any missing packages for local developement.

    ```r
    source("scripts/install_packages.R")
    ```

1.  **Clean raw datasets**

    If the datasets have not been cleaned yet, run the following line to clean the raw datasets. It will generate cleaned RDS files in the `data/cleaned` directory.

    ```r
    source("scripts/clean_data.R")
    ```

1.  **Run the Shiny app**

    Open the `app.R` file in RStudio and click on the “Run App” button to start the Shiny app.

1.  **Deploy the app**

    To deploy the app to Shinyapps.io, create an account and follow the instructions in the [Shinyapps.io documentation](https://docs.rstudio.com/shinyapps.io/).

    _Files to deploy are the `app.R`, `global.R`, `server.R`, `ui.R` and cleaned datasets in the `data/cleaned` directory._

    **Note:** If datasets are updated, re-run `source("scripts/clean_data.R")` to update the cleaned datasets.

## Deployment Files

Ensure that the following files are included when deploying the app:

- `app.R` – Main Shiny app file
- `global.R` – Shared libraries and configurations
- `server.R` – Server-side logic
- `ui.R` – UI layout file
- `load_packages.R` and `packages.R` #TODO need to fix
- Cleaned datasets located in the `data/cleaned` directory
