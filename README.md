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

### 1️⃣ Making Changes & Committing

1. Open your editor (**RStudio/VSCode**) and make changes.
2. **Save your changes locally.**
3. Open **GitHub Desktop**, and you will see the files you modified.
4. Write a **short commit message** describing your changes (e.g., `"Added interactive map"`).
5. Click **"Commit to `<your-branch>`"** to save your work.

### 2️⃣ Pushing Your Changes to GitHub

1. Click **"Push origin"** in GitHub Desktop.
2. Your changes are now uploaded to GitHub! 🎉

### 3️⃣ Keeping Your Branch Updated with Master

It's important to always keep your branch **in sync** with `master`:

1. In **GitHub Desktop**, switch to **`master`**:
   - Click **"Current Branch"** → Select **`master`**.
   - Click **"Fetch Origin"** and then **"Pull"** to get the latest updates.
2. Switch back to **your branch**.
3. Click **"Branch" > "Merge into current branch"**.
4. Select **`master`** and click **"Merge"**.
5. Now, your branch is **up to date with master** ✅.

### 4️⃣ Finishing Your Feature & Merging into Master (Pull Request)

Once your feature is **complete and tested**:

1. **Push all your changes** (Step 2 above).
2. **Go to GitHub.com** and open our project.
3. Click **"Pull Requests" > "New Pull Request"**.
4. Select:
   - **Base branch** → `master`
   - **Compare branch** → `<your-feature-branch>`
5. Add a **short description** and click **"Create Pull Request"**.
6. Wait for approval, then **merge it! 🎉**

### 🎯 Summary of Workflow

- **Edit code**, **commit often**, and **push your changes**.
- **Keep your branch updated with master** to avoid conflicts.
- **Create a Pull Request (PR) when your feature is ready**.
- **Merge into master** once approved.
