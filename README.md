# EstateAnalytics

> Confidence Sells. Data Closes.

## Introduction

EstateAnalytics is a subscription-based platform designed to help real estate agents communicate property data effectively to their clients. By leveraging multiple government data sources and advanced visualizations, the platform provides real-time market insights, interactive data visualizations, and actionable property analytics. Our mission is to empower agents with data-driven insights that increase successful deal closures, focusing on agent-client communication rather than traditional lead generation. Initially targeting Singapore's real estate market with plans to expand across Southeast Asia, EstateAnalytics aims to become an essential tool for real estate professionals.

## Project Structure

```
Project/
├── global.R                                    # Shared settings used across the app
|
├── ui.R                                        # Defines the user interface layout
|
├── app.R                                       # Main Shiny app file
|
├── server/
│   ├── server.R                                # Main server-side logic for the app
│   ├── components/                             # Modular server-side scripts
│   │   ├── ai_utils.R                          # AI-related utility functions
│   │   ├── area_details.R                      # Handles left overlay panel
│   │   ├── building_analytics.R                # Building-specific analytics functionality
│   │   ├── building_details.R                  # Handles right overlay panel
│   │   ├── data_loading.R                      # Loads datasets
│   │   ├── facility.R                          # Manages facility-related data and functions
│   │   ├── filters.R                           # Filters data based on user choices
│   │   ├── map_logic.R                         # Manages map interactions
│   │   ├── modals.R                            # Pop-up windows for user filters
│   │   └── utils.R                             # Helper functions for charts, maps, and UI
│   └── guide.md                                # Instructions for adding server components
|
├── www/                                        # Files for the web interface
│   ├── css/                                    # Custom stylesheets
│   │   └── custom.css                          # Styling for the app
│   ├── images/                                 # Image assets
│   │   ├── EstateAnalytics.png                 # Application logo
│   │   └── EstateAnalyticsWithTagline.png      # Application logo with tagline
│   └── js/                                     # Custom JavaScript files
│       └── overlays.js                         # Handles overlay visibility and actions
|
├── data/                                       
│   ├── clean/                                  # Cleaned data ready for application use
│   │   ├── childcares.RDS                      
│   │   └── ...                                 
│   └── raw/                                    # Original unprocessed data sources
│       ├── Generalinformationofschools.csv     
│       └── ...                                 # Other raw data files
|
├── scripts/                                    # Setup and data processing scripts
│   ├── load_packages.R                         # Loads required R packages
│   ├── install_packages.R                      # Installs missing packages
│   ├── packages.R                              # List of required packages
│   ├── viewRDS.R                               # Utility to view RDS file contents
│   └── data_cleaning/                          # Scripts for data preparation
│       ├── append_planning_area.R            
│       ├── clean_schools_childcares_data.R   
│       └── ...                               
|
├── .env                                        # Environment variables (GROQ API Key)
|
├── .gitignore                                  # Files to exclude from version control
|
├── .gitattributes                              # Git settings for the repository
|
├── .lintr                                      # Linting configuration for R code
|
└── .Rprofile                                   # Project-specific R settings
```

## Getting Started

1.  **Set up GROQ API Key**

      The application uses GROQ's API for AI-powered features. Follow these steps to obtain an API key:
   
      a. Visit [GROQ's website](https://console.groq.com/keys) and create an account if you don't have one.
   
      b. After logging in, navigate to the API Keys      section or developer dashboard.
   
      c. Create a new API key (you may need to provide a name for your project).
   
      d. Copy the generated API key - you'll need it for the next step.

2.  **Set up URA API Access Key**

      The application uses the [URA API](https://eservice.ura.gov.sg/maps/api/) for accessing private property transaction data.

      a. Visit the [URA Data Service portal](https://eservice.ura.gov.sg/maps/api/).

      b. Register for an account if you don't have one.
      
      c. Request an Access Key for the relevant APIs (e.g., Private Residential Property Transactions). Approval might be required.

      d. Once approved, copy your Access Key.

3.  **Set up OneMap API Credentials**

      The application uses the [OneMap API](https://www.onemap.gov.sg/apidocs/) for geocoding addresses (e.g., finding coordinates for HDB blocks). You need to register for an account to use their APIs.

      a. Visit the [OneMap portal](https://www.onemap.gov.sg/) and register for an account.

      b. Once registered, you will use your account email and password for API access.

4.  **Set up Google API Key**

      The data cleaning scripts use the `ggmap` package, which requires a Google Cloud Platform API key for geocoding postal codes.

      a. Go to the [Google Cloud Console](https://console.cloud.google.com/).

      b. Create a new project or select an existing one.
      
      c. Enable the "Geocoding API" and "Maps JavaScript API" for your project.

      d. Go to "Credentials" and create a new API key.

      e. Copy the generated API key.

5.  **Create a .env file**

      Create a file named `.env` in the project root directory with your API keys and credentials:

      ```
      GROQ_API_KEY=your_groq_api_key_here
      # Optional keys below (only needed for running data cleaning scripts)
      URA_ACCESSKEY=your_ura_access_key_here
      ONEMAP_EMAIL=your_onemap_email@example.com
      ONEMAP_EMAIL_PASSWORD=your_onemap_password
      GOOGLE_API_KEY=your_google_api_key_here
      ```

      Replace the placeholder values with your actual keys and credentials. This file is included in `.gitignore` to ensure your keys and credentials remain private.

      > **Note:** The `URA_ACCESSKEY`, `ONEMAP_EMAIL`, `ONEMAP_EMAIL_PASSWORD`, and `GOOGLE_API_KEY` are only required if you intend to run the data cleaning scripts located in the `scripts/data_cleaning/` directory. They are **not** needed to run the main Shiny application with the pre-cleaned data.

6.  **Run the Shiny app**

      Click on the "Run App" button in Rstudio to start the Shiny app. If using VS Code, open `ui.R` and "Run App".
      
      > **Note:** For best performance, use Chrome, Firefox, or Edge browsers. Safari may experience significant lag with this application.

7.  **Deploy the app**

      To deploy the app to Shinyapps.io, create an account and follow the instructions in the [Shinyapps.io documentation](https://docs.rstudio.com/shinyapps.io/).

<details>
<summary><h2>🚀 GitHub Workflow Guide (click to expand)</h2></summary>

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

</details>

<details>
<summary><h2>🤖 Optional: AI-Powered Setup (click to expand)</h2></summary>

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
2. Click **"Get Student Benefits"** / **Sign in for Student Developer Pack**
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

</details>
