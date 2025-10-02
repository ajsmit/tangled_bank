# Sentiment Analysis for Survey TXT Files
# Analyzes sentiment of each survey response and creates a summary CSV
#
# Usage:
#   source("sentiment_analysis.R")
#   analyze_survey_sentiment()

# Install required packages if not available
required_packages <- c("dplyr", "stringr", "purrr", "readr")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
})

# ---- Configuration ----
INPUT_DIR <- "AI/workshop_materials/parsed_data"
OUTPUT_FILE <- "AI/workshop_materials/survey_sentiment_analysis.csv"

# ---- Built-in Sentiment Lexicons ----
positive_words <- c(
  "good", "great", "excellent", "amazing", "wonderful", "fantastic", "awesome", "brilliant",
  "happy", "pleased", "satisfied", "delighted", "thrilled", "excited", "optimistic",
  "love", "like", "enjoy", "appreciate", "benefit", "advantage", "positive", "success",
  "better", "best", "improve", "increase", "grow", "strong", "stable", "reliable",
  "opportunities", "potential", "hope", "confident", "trust", "support", "help",
  "easy", "convenient", "comfortable", "safe", "secure", "protected", "healthy"
)

negative_words <- c(
  "bad", "terrible", "awful", "horrible", "disappointing", "frustrating", "annoying",
  "worried", "concerned", "stressed", "anxious", "scared", "afraid", "fearful",
  "hate", "dislike", "problem", "issue", "challenge", "difficulty", "struggle",
  "worse", "worst", "decline", "decrease", "reduce", "weak", "unstable", "unreliable",
  "risk", "threat", "danger", "unsafe", "insecure", "vulnerable", "unhealthy",
  "hard", "difficult", "impossible", "complicated", "expensive", "costly", "limited",
  "pollution", "damage", "destroy", "loss", "fail", "failure", "crisis", "disaster"
)

# ---- Main Functions ----

#' Extract text content from a survey TXT file
#' @param file_path Path to the TXT file
#' @return Character vector of all text content (excluding headers)
extract_text_content <- function(file_path) {
  lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)

  # Remove header lines (those starting with #) and empty lines
  content_lines <- lines[!grepl("^#", lines) & nzchar(str_trim(lines))]

  # Combine all content into a single string
  paste(content_lines, collapse = " ")
}

#' Perform sentiment analysis using built-in word lists
#' @param text Character string to analyze
#' @return List with sentiment score and classification
analyze_sentiment_simple <- function(text) {
  if (is.na(text) || nchar(str_trim(text)) == 0) {
    return(list(positive = 0, negative = 0, sentiment = "neutral", score = 0))
  }

  # Convert to lowercase and split into words
  words <- tolower(unlist(str_split(text, "\\s+")))

  # Count positive and negative words
  positive_count <- sum(words %in% positive_words)
  negative_count <- sum(words %in% negative_words)

  # Calculate score
  score <- positive_count - negative_count

  # Classify sentiment
  if (score > 1) {
    sentiment <- "positive"
  } else if (score < -1) {
    sentiment <- "negative"
  } else {
    sentiment <- "neutral"
  }

  list(
    positive = positive_count,
    negative = negative_count,
    sentiment = sentiment,
    score = score,
    total_words = length(words)
  )
}

#' Analyze sentiment with confidence assessment
#' @param text Character string to analyze
#' @return List with sentiment analysis results
analyze_sentiment_combined <- function(text) {
  result <- analyze_sentiment_simple(text)

  # Determine confidence based on word counts and score magnitude
  total_sentiment_words <- result$positive + result$negative

  if (total_sentiment_words == 0) {
    confidence <- "low"
  } else if (abs(result$score) >= 3) {
    confidence <- "high"
  } else if (abs(result$score) >= 1) {
    confidence <- "medium"
  } else {
    confidence <- "low"
  }

  list(
    sentiment = result$sentiment,
    confidence = confidence,
    score = result$score,
    positive_words = result$positive,
    negative_words = result$negative,
    total_words = result$total_words,
    sentiment_word_ratio = ifelse(result$total_words > 0,
                                round(total_sentiment_words / result$total_words, 3), 0)
  )
}

#' Analyze sentiment for all survey TXT files
#' @param input_dir Directory containing the TXT files
#' @param output_file Path for the output CSV file
#' @return tibble with sentiment analysis results
analyze_survey_sentiment <- function(input_dir = INPUT_DIR, output_file = OUTPUT_FILE) {

  # Check if input directory exists
  if (!dir.exists(input_dir)) {
    stop("Input directory not found: ", input_dir)
  }

  # Get all TXT files
  txt_files <- list.files(input_dir, pattern = "Gansbaai_survey_.*\\.txt$", full.names = TRUE)

  if (length(txt_files) == 0) {
    stop("No survey TXT files found in: ", input_dir)
  }

  cat("Found", length(txt_files), "survey TXT files\n")
  cat("Using built-in sentiment word lists...\n")
  cat("Starting sentiment analysis...\n")

  # Process each file
  results <- map_dfr(txt_files, function(file_path) {
    # Extract respondent number from filename
    filename <- basename(file_path)
    respondent_number <- as.numeric(str_extract(filename, "\\d+"))

    cat("Processing respondent", respondent_number, "\n")

    # Extract text content
    text_content <- extract_text_content(file_path)

    # Perform sentiment analysis
    sentiment_result <- analyze_sentiment_combined(text_content)

    # Create result row
    tibble(
      respondent_number = respondent_number,
      sentiment = sentiment_result$sentiment,
      confidence = sentiment_result$confidence,
      score = sentiment_result$score,
      positive_words = sentiment_result$positive_words,
      negative_words = sentiment_result$negative_words,
      total_words = sentiment_result$total_words,
      sentiment_word_ratio = sentiment_result$sentiment_word_ratio,
      text_length = nchar(text_content)
    )
  })

  # Sort by respondent number
  results <- results %>% arrange(respondent_number)

  # Save full results
  write_csv(results, output_file)
  cat("Full sentiment analysis saved to:", output_file, "\n")

  # Create simple summary (respondent_number, sentiment)
  simple_summary <- results %>%
    select(respondent_number, sentiment)

  simple_output_file <- str_replace(output_file, "\\.csv$", "_simple.csv")
  write_csv(simple_summary, simple_output_file)
  cat("Simple summary saved to:", simple_output_file, "\n")

  # Print summary statistics
  cat("\n--- Sentiment Analysis Summary ---\n")
  sentiment_summary <- results %>%
    count(sentiment) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))

  print(sentiment_summary)

  cat("\nConfidence levels:\n")
  confidence_summary <- results %>%
    count(confidence) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))

  print(confidence_summary)

  return(results)
}

# ---- Helper Functions ----

#' Preview sentiment analysis for a single file
#' @param file_path Path to a single TXT file
preview_sentiment <- function(file_path) {
  cat("Analyzing file:", basename(file_path), "\n")

  text_content <- extract_text_content(file_path)
  cat("Text content preview (first 200 chars):\n")
  cat(substr(text_content, 1, 200), "...\n\n")

  sentiment_result <- analyze_sentiment_combined(text_content)

  cat("Sentiment Analysis Results:\n")
  cat("Overall Sentiment:", sentiment_result$sentiment, "\n")
  cat("Confidence:", sentiment_result$confidence, "\n")
  cat("Score:", sentiment_result$score, "\n")
  cat("Positive Words:", sentiment_result$positive_words, "\n")
  cat("Negative Words:", sentiment_result$negative_words, "\n")
  cat("Total Words:", sentiment_result$total_words, "\n")
}

# ---- Example Usage ----
# Uncomment to run:
# results <- analyze_survey_sentiment()
# preview_sentiment("AI/workshop_materials/parsed_data/Gansbaai_survey_2.txt")