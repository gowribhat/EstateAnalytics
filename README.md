# HomeExplorer

> A Comprehensive Property Search & Analysis Platform

## Overview

This project aims to develop a data analytics platform that revolutionizes how people search for and evaluate properties in Singapore. By leveraging multiple government data sources and advanced visualizations, the platform will provide insights into property values, neighborhood characteristics, and lifestyle factors to help users make informed housing decisions.

## Project Structure

```
Project/
├── global.R                                  # Shared libraries & settings used across the app
|
├── ui.R                                      # Defines the user interface layout
|
├── server/
│   ├── server.R                              # Main server-side logic for the app
│   ├── components/                           # Modular server-side scripts
│   │   ├── area_details.R                    # Handles left panel: income stats & price trends
│   │   ├── building_details.R                # Handles right panel: building-specific info
│   │   ├── data_loading.R                    # Loads datasets like HDB, private properties, etc.
│   │   ├── filters.R                         # Filters data based on user choices
│   │   ├── map_logic.R                       # Manages map display, markers, and interactions
│   │   ├── modals.R                          # Pop-up windows for user filters
│   │   ├── transaction_overlay.R             # Manages transaction details overlay
│   │   └── utils.R                           # Helper functions for charts, maps, and UI
│   └── guide.md                              # Instructions for adding server components
|
├── www/                                      # Files for the web interface
│   ├── css/                                  # Custom stylesheets
│   │   └── custom.css                        # Styling for the app
│   └── js/                                   # Custom JavaScript files
│       └── overlays.js                       # Handles overlay visibility and actions
|
├── data/                                     # Pre-processed datasets
│   ├── schools.RDS                           # School locations and details
│   ├── childcares.RDS                        # Childcare center data
│   └── ...                                   # Other datasets (e.g., parks, supermarkets)
|
├── scripts/                                  # Setup scripts
│   ├── load_packages.R                       # Loads required R packages
│   ├── install_packages.R                    # Installs missing packages
│   └── packages.R                            # List of required packages
|
├── temp.R                                    # Temporary server logic for testing
|
├── .gitignore                                # Files to exclude from version control
|
├── .gitattributes                            # Git settings for the repository
|
├── .lintr                                    # Linting configuration for R code
|
└── .Rprofile                                 # Project-specific R settings
```

## Getting Started

1.  **Install dependencies**

    Run script to install any missing packages for local developement.

    ```r
    source("scripts/install_packages.R")
    ```

1.  **Run the Shiny app**

    Click on the “Run App” button in Rstudio to start the Shiny app.

1.  **Deploy the app**

    To deploy the app to Shinyapps.io, create an account and follow the instructions in the [Shinyapps.io documentation](https://docs.rstudio.com/shinyapps.io/).

## Deployment Files

Ensure that the following files are included when deploying the app:

- `app.R` – Main Shiny app file
- `global.R` – Shared libraries and configurations
- `server/` – Folder with server-side logic
- `ui.R` – UI layout file
- `www/` - CSS and JS files to be rendered on web browser
- `data/` - Cleaned datasets

## 🚀 GitHub Workflow Guide

### 🔁 BEFORE You Start Working

Always make sure your branch is up-to-date before making changes.

1. Open **GitHub Desktop**.
2. Switch to `master`:
   - Click **"Current Branch"** → select `master`.
   - Click **"Fetch Origin"** and **"Pull"** to get the latest version.
3. Switch to **your feature branch** (or create one if new).
4. Click **"Branch > Merge into current branch"**.
5. Select `master` and click **"Merge"**.

   ✅ Your feature branch is now updated with the latest `master`.

### 🛠️ While Working on Your Feature

1. Open your editor (**RStudio** or **VSCode**).
2. Make your changes locally.
3. **Commit frequently**:
   - Save your changes.
   - Go to **GitHub Desktop**.
   - Write a **short commit message** describing what you have done in this commit (e.g., `"Added proximity filter for schools"`).
   - Click **"Commit to `<your-branch>`"**.
4. Push your work:
   - Click **"Push origin"** to upload your branch changes to GitHub.

> 💡 Tip: Small, regular commits make it easier to review and fix issues.

### ✅ When You Finish a Feature

1. Push all your latest changes (see step above).
2. Go to [GitHub.com](https://github.com) and open the project.
3. Click on **"Pull Requests" > "New Pull Request"**.
4. Set:
   - **Base branch** → `master`
   - **Compare branch** → your feature branch
5. Add a short summary and **click "Create Pull Request"**.
6. Tag a teammate for review (or ping on group chat).
7. Once approved, click **"Merge"** to merge your work into `master`.

### 🔁 After Merging or When Others Merge to Master

To stay up-to-date:

1. Pull the latest `master` (see "Before You Start Working" section).
2. Merge it into your own branch if you're still working on other features.

### 🔑 Summary: What Everyone Should Always Do

- **Start your day by updating `master` and your branch**
- **Work on your own branch**
- **Commit frequently and push your changes**
- **Open a PR when your feature is ready**
- **Merge your PR only after review**
- **Update your local project regularly**
