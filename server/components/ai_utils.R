# AI Utilities for HomeExplorer
# This script contains functions for interacting with AI APIs (Groq)

#' Call Groq API to generate area analysis
#'
#' @param prompt The prompt to send to the Groq API
#' @param api_key The Groq API key
#' @return The generated text response
#' @export
call_groq_api <- function(prompt, api_key) {
  # Load required packages
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package httr is required but not installed")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package jsonlite is required but not installed")
  }
  
  # Set Groq API endpoint for Llama 3
  url <- "https://api.groq.com/openai/v1/chat/completions"
  
  # Prepare the API request
  body <- list(
    model = "llama3-70b-8192",  # Using Llama 3 model
    messages = list(
      list(
        role = "system",
        content = "You are an expert Singapore property advisor providing balanced analyses of neighborhoods. Focus on creating concise, well-structured bullet points for pros and cons based on the provided data. Be factual, specific, and relevant to home buyers."
      ),
      list(
        role = "user",
        content = prompt
      )
    ),
    temperature = 0.5,
    max_tokens = 800
  )
  
  # Make the API request with error handling
  tryCatch({
    response <- httr::POST(
      url,
      httr::add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key)
      ),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
    
    # Parse the response
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      return(parsed$choices[[1]]$message$content)
    } else {
      warning("API request failed with status code: ", httr::status_code(response))
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      return(paste("Error:", error_content))
    }
  }, error = function(e) {
    warning("API request error: ", e$message)
    return(paste("Error connecting to Groq API:", e$message))
  })
}

# Function to create a prompt for area analysis based on available data
create_area_analysis_prompt <- function(area_name, price_data = NULL, income_data = NULL, facilities = NULL, user_facilities = NULL) {
  # Start with the base prompt
  prompt <- paste0("Create a balanced analysis with pros and cons of buying property in ", area_name, " area of Singapore. ")
  
  # Add price information if available
  if (!is.null(price_data) && nrow(price_data) > 0) {
    property_type <- if("resale_price" %in% names(price_data)) "HDB" else "private property"
    median_price <- if("resale_price" %in% names(price_data)) median(price_data$resale_price) else median(price_data$price)
    min_price <- if("resale_price" %in% names(price_data)) min(price_data$resale_price) else min(price_data$price)
    max_price <- if("resale_price" %in% names(price_data)) max(price_data$resale_price) else max(price_data$price)
    
    prompt <- paste0(prompt, "Property information: The area has ", nrow(price_data), " ", property_type, 
                    " transactions visible with median price $", format(median_price, big.mark = ","), 
                    ", ranging from $", format(min_price, big.mark = ","), 
                    " to $", format(max_price, big.mark = ","), ". ")
  }
  
  # Add income information if available
  if (!is.null(income_data) && income_data$available) {
    income_text <- paste0("Household income distribution: ",
                         "No income (", income_data$percentages[1], "%), ",
                         "Low income <$3K (", income_data$percentages[2], "%), ",
                         "Middle income $3-9K (", income_data$percentages[3], "%), ",
                         "High income $9-20K (", income_data$percentages[4], "%), ",
                         "Affluent >$20K (", income_data$percentages[5], "%). ")
    prompt <- paste0(prompt, income_text)
  }
  
  # Add facilities information if available
  if (!is.null(facilities)) {
    facility_text <- paste0("Area facilities: ", 
                           facilities$schools, " schools, ",
                           facilities$childcare, " childcare centers, ",
                           facilities$mrt, " MRT/LRT stations, ",
                           facilities$gyms, " gyms, ",
                           facilities$parks, " parks, ",
                           facilities$supermarkets, " supermarkets. ")
    prompt <- paste0(prompt, facility_text)
  }
  
  # Add user's facility preferences if available
  if (!is.null(user_facilities) && length(user_facilities) > 0) {
    prompt <- paste0(prompt, "The home buyer considers the following facilities important: ", 
                    paste(user_facilities, collapse = ", "), ". ")
  }
  
  # Final instructions
  prompt <- paste0(prompt, 
                  "Analyze this area as a place to buy property. Format your response in two sections with bullet points: '### PROS' and '### CONS'. ",
                  "Focus on location benefits/drawbacks, value for money, amenities, demographics, and lifestyle considerations. ",
                  "Keep each bullet point concise (max 20 words) and highly specific to this area based on the data provided. ",
                  "Include 4-6 bullet points in each section.")
  
  return(prompt)
}
