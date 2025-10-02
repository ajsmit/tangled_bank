# Excel to Individual CSV Converter
# Converts each row of an Excel file into a separate CSV file with headers as markdown-style headings
#
# Usage:
#   source("excel_to_individual_csvs.R")
#   convert_excel_to_individual_csvs()

# Install required packages if not available
required_packages <- c("readxl", "dplyr", "stringr", "purrr", "fs")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(fs)
})

# ---- Configuration ----
# PII columns to exclude (modify as needed)
PII_COLUMNS <- c(
  "What is your name please (first and last)?",
  "Please supply your contact details.",
  "Is there anyone else you think we we should get to fill in this survey (please supply name and contact details)."
)

# Input and output paths
INPUT_FILE <- "AI/workshop_materials/Cleaned_Survey_Data_Translated_and_Processed_Fixed.xlsx"
OUTPUT_DIR <- "AI/workshop_materials/parsed_data"

# ---- Main Function ----

#' Convert Excel file to individual CSV files per row
#' @param input_file Path to the Excel file
#' @param output_dir Directory to save the individual CSV files
#' @param pii_columns Vector of column names to exclude for privacy
#' @param filename_prefix Prefix for the output CSV files (default: "Gansbaai_survey")
convert_excel_to_individual_csvs <- function(
  input_file = INPUT_FILE,
  output_dir = OUTPUT_DIR,
  pii_columns = PII_COLUMNS,
  filename_prefix = "Gansbaai_survey"
) {

  # Check if input file exists
  if (!file.exists(input_file)) {
    stop("Input Excel file not found: ", input_file)
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    cat("Creating output directory:", output_dir, "\n")
    dir.create(output_dir, recursive = TRUE)
  }

  # Read the Excel file
  cat("Reading Excel file:", input_file, "\n")
  data <- read_excel(input_file, col_types = "text")  # Read as text to preserve formatting

  # Get column names and filter out PII columns
  all_columns <- names(data)

  # Find PII columns (case-insensitive matching)
  pii_indices <- which(tolower(all_columns) %in% tolower(pii_columns))

  if (length(pii_indices) > 0) {
    cat("Excluding PII columns:", paste(all_columns[pii_indices], collapse = ", "), "\n")
    data <- data[, -pii_indices]
    filtered_columns <- names(data)
  } else {
    cat("No PII columns found to exclude\n")
    filtered_columns <- all_columns
  }

  cat("Processing", nrow(data), "rows with", ncol(data), "columns\n")

  # Process each row (starting from row 2, which is index 1 in R since headers are already separated)
  for (i in 1:nrow(data)) {
    row_number <- i + 1  # Add 1 because we're skipping the header row

    # Create filename
    filename <- paste0(filename_prefix, "_", row_number, ".csv")
    filepath <- file.path(output_dir, filename)

    # Prepare the content for this row
    content_lines <- character()

    for (col in filtered_columns) {
      # Add header with # prefix
      content_lines <- c(content_lines, paste0("# ", col))

      # Add the cell value (handle NA values)
      cell_value <- data[[col]][i]
      if (is.na(cell_value) || is.null(cell_value)) {
        cell_value <- ""
      }
      content_lines <- c(content_lines, as.character(cell_value))

      # Add empty line for readability (except after last column)
      if (col != filtered_columns[length(filtered_columns)]) {
        content_lines <- c(content_lines, "")
      }
    }

    # Write to file with UTF-8 encoding
    writeLines(content_lines, filepath, useBytes = FALSE)

    # Print progress
    cat("Saved row", row_number, "to", filename, "\n")
  }

  cat("Successfully created", nrow(data), "CSV files in", output_dir, "\n")
}

# ---- Helper Functions ----

#' Preview the first few rows to check column names and identify PII columns
#' @param input_file Path to the Excel file
preview_excel_file <- function(input_file = INPUT_FILE) {
  if (!file.exists(input_file)) {
    stop("Input Excel file not found: ", input_file)
  }

  cat("Previewing Excel file:", input_file, "\n")
  data <- read_excel(input_file, n_max = 3)

  cat("\nColumn names:\n")
  cat(paste(names(data), collapse = "\n"))
  cat("\n\nFirst 3 rows:\n")
  print(data)

  cat("\nCurrent PII columns to exclude:\n")
  cat(paste(PII_COLUMNS, collapse = "\n"))
}

#' Clean up generated CSV files
#' @param output_dir Directory containing the CSV files to remove
clean_csv_files <- function(output_dir = OUTPUT_DIR) {
  if (!dir.exists(output_dir)) {
    cat("Output directory does not exist:", output_dir, "\n")
    return()
  }

  csv_files <- list.files(output_dir, pattern = "*.csv", full.names = TRUE)

  if (length(csv_files) == 0) {
    cat("No CSV files found in:", output_dir, "\n")
    return()
  }

  cat("Removing", length(csv_files), "CSV files from", output_dir, "\n")
  file.remove(csv_files)
  cat("Cleanup complete\n")
}

# ---- Example Usage ----
# Uncomment to run:
# preview_excel_file()
# convert_excel_to_individual_csvs()