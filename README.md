# HomeExplorer

> A Comprehensive Property Search & Analysis Platform

## Overview

This project aims to develop a data analytics platform that revolutionizes how people search for and evaluate properties in Singapore. By leveraging multiple government data sources and advanced visualizations, the platform will provide insights into property values, neighborhood characteristics, and lifestyle factors to help users make informed housing decisions.

## Tech Stack

- **R & Shiny** – Interactive web application
- **ggplot2** – Data visualization
- **leaflet** – Interactive maps
- **dplyr & stringr** – Data cleaning & transformation

## Project Structure

```
Project/
├── global.R                                  # Shared libraries & configurations
|
├── ui/
│   ├── components/                           # UI components for reuse
│   │   ├── navbar.R
|   |   └── ...
|   └── ui.R                                  # Main UI script
|
├── server/
│   └── server.R                              # Server-side logic
│   ├── modules/                              # Modular server components
│   │   ├── map_module.R                      # Server logic for interactive map
│   │   └── ...
|
├── www/
│   └── styles.css                            # CSS styling file
│
├── data/                                     # Cleaned RDS Datasets
│   ├── schools.RDS
│   └── childcares.RDS
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

1.  **Run the Shiny app**

    Click on the “Run App” button in Rstudio to start the Shiny app.

1.  **Deploy the app**

    To deploy the app to Shinyapps.io, create an account and follow the instructions in the [Shinyapps.io documentation](https://docs.rstudio.com/shinyapps.io/).

## Deployment Files

Ensure that the following files are included when deploying the app:

- `app.R` – Main Shiny app file
- `global.R` – Shared libraries and configurations
- `server.R` – Server-side logic
- `ui.R` – UI layout file
- CSS styling file in the `www/` directory
- Cleaned datasets located in the `data/cleaned` directory

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
