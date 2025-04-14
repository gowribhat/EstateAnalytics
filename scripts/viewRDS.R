# Load necessary library
library(tools)

# Get the current script directory
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))

# List all RDS files in the directory
rds_files <- list.files(path = script_dir, pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE)

# Load each RDS file and assign it to a variable named after the file (without extension)
for (file in rds_files) {
    object_name <- file_path_sans_ext(basename(file))
    assign(object_name, readRDS(file), envir = .GlobalEnv)
}

# Print a message indicating completion
cat("Loaded", length(rds_files), "RDS files as separate objects.\n")