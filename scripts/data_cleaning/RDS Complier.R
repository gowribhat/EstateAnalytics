# --- Configuration ---

# Input CSV file path
csv_file_path <- "Resources/URA_PrivateTransactions2025-04-02_region.csv" 
# Output RDS file path
rds_file_path <- sub("\\.csv$", ".rds", csv_file_path)

# Strings to be treated as NA (add more if needed)
na_strings <- c("NA", "", "NULL", "N/A", "na", "n/a", "<NA>", "NaN", " ")

# Factor Threshold: If the ratio of unique non-NA values to total non-NA values 
# is less than this, consider it a factor (unless it's logical, numeric, or date).
# Adjust this based on your data characteristics. E.g., 0.1 means < 10% unique values.
factor_threshold_ratio <- 0.1 
# OR: Maximum number of unique values for a column to be considered a factor
# factor_threshold_max_unique <- 100 # Alternative thresholding method

# Date/Time parsing formats (add more formats as needed by your data)
# Order matters: more specific formats should ideally come earlier if unambiguous
# Added "%Y-%m" which will be handled specially
date_formats <- c(
  "%Y-%m-%d",        # YYYY-MM-DD
  "%Y-%m",          # YYYY-MM (will assume day 01)
  "%m/%d/%Y",        # MM/DD/YYYY
  "%d-%b-%Y",        # DD-Mon-YYYY (e.g., 01-Jan-2023)
  "%Y/%m/%d"         # YYYY/MM/DD
)

datetime_formats <- c(
  "%Y-%m-%d %H:%M:%S", # YYYY-MM-DD HH:MM:SS
  "%Y/%m/%d %H:%M:%S", # YYYY/MM/DD HH:MM:SS
  "%m/%d/%Y %H:%M:%S", # MM/DD/YYYY HH:MM:SS
  "%Y-%m-%dT%H:%M:%S", # ISO 8601 often includes 'T'
  "%a %b %d %H:%M:%S %Y" # e.g. Tue Aug 17 10:00:00 2021
)

# --- Libraries ---
# Consider using 'lubridate' for more robust date/time parsing if needed
# install.packages("lubridate")
# library(lubridate)

# --- Helper Function: Guess Column Type ---

#' Attempts to guess the best data type for a column vector.
#'
#' @param col A vector (typically character initially).
#' @param na_strings Vector of strings to be treated as NA.
#' @param factor_threshold_ratio Ratio threshold for factor conversion.
#' @param date_formats Vector of possible date formats to try.
#' @param datetime_formats Vector of possible datetime formats to try.
#'
#' @return The column vector converted to the guessed type.
guess_col_type <- function(col, na_strings, factor_threshold_ratio, date_formats, datetime_formats) {
  
  # Store original NAs to compare later
  original_na <- is.na(col) | col %in% na_strings
  
  # Convert explicit NA strings to actual NA
  col[col %in% na_strings] <- NA
  
  # Remove NA values for analysis (but keep original NAs for final conversion)
  col_clean <- col[!is.na(col)]
  
  # If column is empty or all NA after cleaning, return as is (likely logical or character)
  if (length(col_clean) == 0) {
    if (all(is.na(col))) return(col) 
    return(as.character(col)) 
  }
  
  unique_vals <- unique(col_clean)
  n_unique <- length(unique_vals)
  n_total <- length(col_clean) # Number of non-NA values
  
  # 1. Check for Logical (Boolean)
  logical_patterns <- c("TRUE", "FALSE", "T", "F", "1", "0", "YES", "NO", "Y", "N")
  if (all(toupper(unique_vals) %in% logical_patterns)) {
    is_strictly_logical <- all(toupper(unique_vals) %in% c("TRUE", "FALSE", "T", "F", "YES", "NO", "Y", "N")) || 
                           all(toupper(unique_vals) %in% c("1", "0")) 
                           
    if(is_strictly_logical || n_unique <= 2) { 
      converted_col <- NA # Initialize
      if(all(toupper(unique_vals) %in% c("TRUE", "FALSE", "T", "F", "1", "0"))) {
         temp_col <- rep(NA, length(col))
         temp_col[!original_na] <- toupper(col[!original_na]) # Only work on non-NA originals
         converted_col <- as.logical(temp_col %in% c("TRUE", "T", "1"))
         # Re-apply original NAs
         is.na(converted_col) <- original_na
         # Check if conversion introduced *unexpected* NAs
         if (sum(is.na(converted_col)) == sum(original_na)) {
             message("  -> Converted to Logical (T/F/1/0)")
             return(converted_col)
         }
      } else { # Handle Yes/No Y/N explicitly
         temp_col <- rep(NA, length(col))
         temp_col[!original_na] <- toupper(col[!original_na]) # Only work on non-NA originals
         converted_col <- rep(NA, length(col))
         converted_col[temp_col %in% c("TRUE", "T", "1", "YES", "Y")] <- TRUE
         converted_col[temp_col %in% c("FALSE", "F", "0", "NO", "N")] <- FALSE
         is.na(converted_col) <- original_na # Re-apply original NAs
         
          if (sum(is.na(converted_col)) == sum(original_na)) {
             message("  -> Converted to Logical (Yes/No/Y/N)")
             return(as.logical(converted_col))
         }
      }
    }
  }
  
  # 2. Check for Numeric (Integer or Double)
  suppressWarnings(col_numeric <- as.numeric(col))
  if (sum(is.na(col_numeric)) == sum(original_na)) {
    col_numeric_clean <- col_numeric[!is.na(col_numeric)]
    # Check for potential floating point inaccuracies with floor
    # Use a small tolerance instead of direct equality
    tolerance <- .Machine$double.eps^0.5
    if (all(abs(col_numeric_clean - floor(col_numeric_clean)) < tolerance)) {
      message("  -> Converted to Integer")
      return(as.integer(col_numeric))
    } else {
      message("  -> Converted to Numeric (Double)")
      return(col_numeric)
    }
  }
  
  # 3. Check for Date/Time (POSIXct) - Attempt various formats
  for (fmt in datetime_formats) {
    suppressWarnings(col_datetime <- as.POSIXct(col, format = fmt, tz = "UTC")) # Specify TZ
    if (sum(is.na(col_datetime)) == sum(original_na)) {
       message(paste("  -> Converted to POSIXct (DateTime) using format:", fmt))
       return(col_datetime)
    }
  }
  
  # 4. Check for Date - Attempt various formats
  for (fmt in date_formats) {
    # Special handling for YYYY-MM format
    col_to_parse <- col
    format_to_use <- fmt
    if (fmt == "%Y-%m") {
        # Assume first day of the month
        # Only modify non-NA values that potentially match the pattern
        # Regex to check if it looks like YYYY-MM
        potential_matches <- grepl("^\\d{4}-\\d{2}$", col_to_parse[!original_na])
        if(any(potential_matches)) {
            col_to_parse[!original_na][potential_matches] <- paste0(col_to_parse[!original_na][potential_matches], "-01")
            format_to_use <- "%Y-%m-%d" # Use the full format for parsing now
             message(paste0("  (Attempting format ", fmt, " by appending '-01')"))
        } else {
             # If no values match the pattern, skip this format iteration
             next 
        }
    }

    # Use the potentially modified column and format
    suppressWarnings(col_date <- as.Date(col_to_parse, format = format_to_use))
    
    # Check if the NA count matches the *original* NA count
    if (sum(is.na(col_date)) == sum(original_na)) {
        message(paste("  -> Converted to Date using format:", format_to_use, "(original pattern:", fmt, ")"))
        # Ensure original NAs are preserved if parsing introduced NAs where there were none
        # Although the check above should handle this, an extra safety layer:
        is.na(col_date) <- original_na
        return(col_date)
    }
  }
  
  # --- Lubridate Alternative Placeholder ---
  # Consider adding robust lubridate parsing here if needed
  # --- End lubridate example ---

  # 5. Check for Factor (Categorical)
  if (n_total > 0 && (n_unique / n_total) < factor_threshold_ratio) {
    # Alternative check: if (n_unique <= factor_threshold_max_unique)
    message(paste0("  -> Converted to Factor (Unique Ratio: ", round(n_unique / n_total, 3), ")"))
    return(as.factor(col))
  }
  
  # 6. Default to Character
  message("  -> Kept as Character")
  return(as.character(col)) 
}

# --- Main Script ---

# Check if input file exists
if (!file.exists(csv_file_path)) {
  stop("Error: Input CSV file not found at: ", csv_file_path)
}

# 1. Read CSV - IMPORTANT: stringsAsFactors = FALSE
cat("Reading CSV file:", csv_file_path, "\n")
tryCatch({
  raw_data <- read.csv(csv_file_path, 
                       stringsAsFactors = FALSE, 
                       na.strings = "", # Read all as character first, handle NA in function
                       check.names = TRUE, 
                       comment.char = "" 
                       ) 
}, error = function(e) {
  stop("Error reading CSV file: ", e$message)
})

cat("Successfully read", nrow(raw_data), "rows and", ncol(raw_data), "columns.\n")

if (nrow(raw_data) == 0 && ncol(raw_data) == 0) {
    warning("CSV file is empty. Creating an empty data frame.")
    typed_data <- data.frame()
} else if (nrow(raw_data) == 0) {
     warning("CSV file has headers but no data rows.")
     # Try to guess types based on column names maybe? Or just keep structure?
     # For now, keep the structure but columns will likely be logical/character
     typed_data <- raw_data 
     # Optional: Could attempt to force character type for all if desired
     # for (col_name in names(typed_data)) {
     #     typed_data[[col_name]] <- character(0)
     # }
} else {
    
    # Store original classes for comparison
    original_classes <- sapply(raw_data, class)
    
    # 2. Initialize list to store converted columns
    converted_list <- vector("list", ncol(raw_data))
    names(converted_list) <- names(raw_data)
    
    cat("\nAttempting to convert column types...\n")
    
    # 3. Apply guessing function to each column
    for (col_name in names(raw_data)) {
      cat("Processing column:", col_name, "(Original class:", original_classes[col_name], ")\n")
      converted_list[[col_name]] <- guess_col_type(
        col = raw_data[[col_name]],
        na_strings = na_strings,
        factor_threshold_ratio = factor_threshold_ratio,
        date_formats = date_formats,
        datetime_formats = datetime_formats
      )
    }
    
    # 4. Reassemble into a data frame
    cat("\nReassembling data frame...\n")
    typed_data <- as.data.frame(converted_list, stringsAsFactors = FALSE) 
    typed_data <- typed_data[, names(raw_data), drop = FALSE] # Ensure original order
    
    # --- ADDED: Extract Date Components ---
    cat("\nChecking for Date/DateTime columns to extract components...\n")
    new_cols_to_add <- list() # Store new columns here temporarily
    cols_to_check <- names(typed_data) # Iterate over current columns
    
    for (col_name in cols_to_check) {
        current_col <- typed_data[[col_name]]
        
        # Check if it's a Date or POSIXct object
        if (inherits(current_col, "Date") || inherits(current_col, "POSIXct")) {
            cat("  Extracting Year, Month, Day from:", col_name, "\n")
            
            # Create new column names
            year_col_name <- paste0(col_name, "_year")
            month_col_name <- paste0(col_name, "_month")
            day_col_name <- paste0(col_name, "_day") # Day might be NA if source was YYYY-MM
            
            # Extract components using base R format function
            # Convert to integer, NAs will be preserved
            new_cols_to_add[[year_col_name]] <- as.integer(format(current_col, "%Y"))
            new_cols_to_add[[month_col_name]] <- as.integer(format(current_col, "%m"))
            new_cols_to_add[[day_col_name]] <- as.integer(format(current_col, "%d"))
            
            # Optional: Add Hour, Minute, Second if POSIXct
            if (inherits(current_col, "POSIXct")) {
                cat("    (Also extracting Hour, Minute, Second for POSIXct)\n")
                hour_col_name <- paste0(col_name, "_hour")
                min_col_name <- paste0(col_name, "_minute")
                sec_col_name <- paste0(col_name, "_second")
                
                new_cols_to_add[[hour_col_name]] <- as.integer(format(current_col, "%H"))
                new_cols_to_add[[min_col_name]] <- as.integer(format(current_col, "%M"))
                # format %S can include fractional seconds, floor ensures integer
                new_cols_to_add[[sec_col_name]] <- floor(as.numeric(format(current_col, "%OS"))) 
            }
        }
    }
    
    # Add the new columns to the data frame
    if (length(new_cols_to_add) > 0) {
        cat("Adding new date component columns to the data frame.\n")
        # Use cbind for potentially better performance than repeated `[[<-`
        typed_data <- cbind(typed_data, as.data.frame(new_cols_to_add))
    } else {
        cat("No Date or DateTime columns found to extract components from.\n")
    }
    # --- End Date Component Extraction ---

    # 5. Report results (original columns)
    cat("\n--- Type Conversion Summary (Original Columns) ---\n")
    final_classes_original <- sapply(typed_data[names(raw_data)], class) # Check only original columns
    final_classes_str_original <- sapply(final_classes_original, function(x) paste(x, collapse=", "))
    
    summary_df <- data.frame(
      Column = names(raw_data),
      Original_Class = original_classes,
      Converted_Class = final_classes_str_original[names(raw_data)], # Ensure order match
      stringsAsFactors = FALSE
    )
    print(summary_df, row.names = FALSE)
    cat("--------------------------------------------------\n")
    
    if (length(new_cols_to_add) > 0) {
      cat("\n--- Added Date Component Columns ---\n")
      added_col_names <- names(new_cols_to_add)
      added_col_classes <- sapply(typed_data[added_col_names], class)
      added_summary_df <- data.frame(
          Column = added_col_names,
          Class = sapply(added_col_classes, function(x) paste(x, collapse=", ")),
          stringsAsFactors = FALSE
      )
      print(added_summary_df, row.names = FALSE)
      cat("-------------------------------------\n")
    }
    
    # --- ADDED: Extract Floor Range Components ---
    cat("\nChecking for columns with 'floor' in the name to extract floor range components...\n")
    floor_cols_to_add <- list() # Store new floor columns here temporarily
    
    # Process floor columns
    for (col_name in names(typed_data)) {
      # Check if column name contains 'floor' (case insensitive)
      if (grepl("floor", col_name, ignore.case = TRUE) || grepl("storey", col_name, ignore.case = TRUE)) {
        cat("  Processing floor range column:", col_name, "\n")
        current_col <- typed_data[[col_name]]
        
        # Convert to character if it's a factor
        if (inherits(current_col, "factor")) {
          cat("    Converting factor to character for processing\n")
          current_col <- as.character(current_col)
        }
        
        # Skip if column is not character type (after potential factor conversion)
        if (!inherits(current_col, "character")) {
          cat("    Skipping - not a character or factor column\n")
          next
        }
        
        # Create new column names for min and max floors
        min_floor_col_name <- paste0(col_name, "_min")
        max_floor_col_name <- paste0(col_name, "_max")
        
        # Initialize vectors for min and max floors
        min_floors <- rep(NA_integer_, length(current_col))
        max_floors <- rep(NA_integer_, length(current_col))
        
        # Process each value
        for (i in seq_along(current_col)) {
          if (is.na(current_col[i])) next
          
          value <- trimws(current_col[i])
          
          if (value == "") {
            # Empty value - keep as NA
            next
          } else if (value == "-") {
            # Single dash indicates landed property - set both min and max to 1
            min_floors[i] <- 1
            max_floors[i] <- 1
          } else if (grepl("TO", value, ignore.case = TRUE)) {
            # Pattern like "10 TO 12"
            parts <- strsplit(value, "TO", fixed = FALSE)[[1]]
            min_floor <- as.integer(trimws(parts[1]))
            max_floor <- as.integer(trimws(parts[2]))
            min_floors[i] <- min_floor
            max_floors[i] <- max_floor
          } else if (grepl("^B\\d+-B\\d+$", value)) {
            # Pattern like "B1-B5" (Basement floors)
            parts <- strsplit(value, "-", fixed = TRUE)[[1]]
            # Remove 'B' and negate the values since basements are below ground
            min_floor <- as.integer(gsub("B", "", parts[1]))
            max_floor <- as.integer(gsub("B", "", parts[2]))
            min_floors[i] <- min_floor
            max_floors[i] <- max_floor
          } else if (grepl("^\\d+-\\d+$", value)) {
            # Pattern like "01-05" or "06-10"
            parts <- strsplit(value, "-", fixed = TRUE)[[1]]
            min_floor <- as.integer(parts[1])
            max_floor <- as.integer(parts[2])
            min_floors[i] <- min_floor
            max_floors[i] <- max_floor
          } else {
            # Try to parse as single integer
            tryCatch({
              floor_num <- as.integer(value)
              min_floors[i] <- floor_num
              max_floors[i] <- floor_num
            }, error = function(e) {
              # If parsing fails, leave as NA
            })
          }
        }
        
        # Add the extracted min and max floors to our list
        floor_cols_to_add[[min_floor_col_name]] <- min_floors
        floor_cols_to_add[[max_floor_col_name]] <- max_floors
      }
    }
    
    # Add the floor range columns to the data frame
    if (length(floor_cols_to_add) > 0) {
      cat("Adding", length(floor_cols_to_add), "new floor range columns to the data frame.\n")
      typed_data <- cbind(typed_data, as.data.frame(floor_cols_to_add, stringsAsFactors = FALSE))
      
      cat("\n--- Added Floor Range Columns ---\n")
      floor_col_names <- names(floor_cols_to_add)
      floor_col_classes <- sapply(typed_data[floor_col_names], class)
      floor_summary_df <- data.frame(
        Column = floor_col_names,
        Class = sapply(floor_col_classes, function(x) paste(x, collapse=", ")),
        stringsAsFactors = FALSE
      )
      print(floor_summary_df, row.names = FALSE)
      cat("-------------------------------------\n")
    } else {
      cat("No columns with 'floor' in the name found to extract range components from.\n")
    }
    # --- End Floor Range Component Extraction ---
    
} # End else block for non-empty data

# 6. Save the final typed data frame (with potential new columns) as RDS
cat("\nSaving final data frame to RDS file:", rds_file_path, "\n")
tryCatch({
  saveRDS(typed_data, file = rds_file_path)
  cat("Successfully saved RDS file.\n")
}, error = function(e) {
  stop("Error saving RDS file: ", e$message)
})

cat("\nScript finished.\n")