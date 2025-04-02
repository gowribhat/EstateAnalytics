# HomeExplorer

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
├── global.R                                  # Shared libraries & configurations
├── server.R                                  # Server-side logic
├── ui.R                                      # UI layout
├── www/
│   └── styles.css                            # CSS styling file
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
│   └── install_packages.R                    # Script for local developement to install packages
│   └── packages.R                            # List of packages needed
│
├── .env                                      # Secrets (e.g. Google API Key)
│
└── rsconnect/                                # Shinyapps.io deployment configs
    └── shinyapps.io/
        └── <shiny_username>/
            └── <R_Project_name>.dcf
```

## Getting Started

1.  **Set environment variable**

    Create a file name `.env` in the project root with the following variable and set your private keys.

    ```bash
    GOOGLE_API_KEY="add_your_key"
    ```

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

    Click on the “Run App” button in Rstudio to start the Shiny app.

1.  **Deploy the app**

    To deploy the app to Shinyapps.io, create an account and follow the instructions in the [Shinyapps.io documentation](https://docs.rstudio.com/shinyapps.io/).

    **Note:** If datasets are updated or `clean_data.R` is changed, re-run `source("scripts/clean_data.R")` to update the cleaned datasets.

## Deployment Files

Ensure that the following files are included when deploying the app:

- `app.R` – Main Shiny app file
- `global.R` – Shared libraries and configurations
- `server.R` – Server-side logic
- `ui.R` – UI layout file
- CSS styling file in the `www/` directory
- Cleaned datasets located in the `data/cleaned` directory
