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

---

## 🤖 Optional: AI-Powered Setup (VS Code + GitHub Copilot + Ollama)

Want to boost your workflow with **AI assistance**? Set up **free AI tools** to help write, understand, and debug code for the project.

### 🛠️ 1. Install Visual Studio Code

1. Go to [https://code.visualstudio.com/](https://code.visualstudio.com/)
2. Download and install VS Code for your system.
3. Open the project folder:
   - Launch VS Code
   - Go to **File > Open Folder**
   - Select the project directory of cloned repo

### 🎓 2. Get GitHub Student Pack (Free Copilot Access) <a name="student-pack"></a>

1. Visit: [https://education.github.com/pack](https://education.github.com/pack)
2. Click **“Get Student Benefits”** / **Sign in for Student Developer Pack**
3. Sign in with your GitHub account using your NUS email
4. Follow the verification steps

> ⏳ Approval can take 1–2 days, but gives you:
>
> - ✅ Free GitHub Copilot
> - ✅ Free Codespaces
> - ✅ More dev tools (free!)

### 🤖 3. Enable GitHub Copilot in VS Code

Once your student access is approved:

1. Open **VS Code**
2. Go to the **Extensions tab** on the left (or press `Ctrl+Shift+X`)
3. Search for `GitHub Copilot` and `Github Copilot Chat` and click **Install**
4. Sign in with your GitHub account
5. Start coding — Copilot will suggest lines and functions as you type! Copilot chat allows you to ask question and edit files automatically by selecting the context files.

### 💬 4. Use GitHub Copilot Chat in Your Browser instead of VS Code

After your [student pack from step 2](#student-pack) is approved:

1. Open any file in your GitHub repository (like `ui.R`, `server.R`, etc.)
2. Click on the **Copilot Chat** icon (search for logo if unsure) in the **top-right corner**
3. Type a question in natural language, such as:
   - `"Explain what this R function does"`
   - `"How can I fix this error in my code?"`
   - `"Suggest improvements for this filter logic"`
   - `"What does leafletOutput() do in Shiny?"`
4. You can also go to [https://github.com/copilot/](https://github.com/copilot/) on you browser to start a chat and add the project repository or specific files as attachments.
