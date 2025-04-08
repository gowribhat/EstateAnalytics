# HomeExplorer

> A Comprehensive Property Search & Analysis Platform

## Overview

This project aims to develop a data analytics platform that revolutionizes how people search for and evaluate properties in Singapore. By leveraging multiple government data sources and advanced visualizations, the platform will provide insights into property values, neighborhood characteristics, and lifestyle factors to help users make informed housing decisions.

## Project Structure

```
Project/
├── global.R                                  # Shared libraries & configurations
|
├── ui.R                                      # Main UI script
|
├── server/
│   ├── server.R                              # Main server-side logic
│   ├── components/                           # Modular server components
│   │   ├── area_details.R                    # Left overlay logic
│   │   ├── building_details.R                # Right overlay logic
│   │   ├── data_loading.R                    # Data loading logic
│   │   ├── filters.R                         # Data filtering logic
│   │   ├── map_logic.R                       # Map rendering and interaction logic
│   │   ├── modals.R                          # Modal dialog logic
│   │   ├── transaction_overlay.R             # Transactions overlay logic
│   │   └── utils.R                           # Utility functions
│   └── guide.md                              # Guide for adding server components
|
├── www/                                      # Elements to be rendered on web browser
│   ├── css/                                  # Custom CSS files
│   │   └── custom.css                        # Styling for the application
│   └── js/                                   # Custom JavaScript files
│       └── overlays.js                       # Overlay handling logic
|
├── data/                                     # Cleaned RDS Datasets
│   ├── schools.RDS
│   ├── childcares.RDS
│   └── ...
|
├── scripts/                                  # Scripts for setup
│   ├── load_packages.R                       # Load all relevant packages
│   ├── install_packages.R                    # Script for local development to install packages
│   └── packages.R                            # List of packages needed
|
├── temp.R                                    # Temporary server logic for testing
|
|
├── scripts/                                  # Scripts for setup
│   ├── load_packages.R                       # Load all relevant packages
│   ├── install_packages.R                    # Script for local development to install packages
│   └── packages.R                            # List of packages needed
|
├── temp.R                                    # Temporary server logic for testing
|
├── .gitignore                                # Files to excluding version control
|
├── .gitattributes                            # Git attributes file for defining repository settings
|
├── .lintr                                    # Configuration file for linting R code
|
└── .Rprofile                                 # R profile for project-specific settings

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
