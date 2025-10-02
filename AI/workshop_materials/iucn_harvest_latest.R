# iucn_harvest_latest.R
# Harvest latest global IUCN Red List assessments for a set of species.
# Requires: R >= 4.2, rredlist >= 1.0.0
#
# Usage examples:
#   harvest_iucn_latest("Pristis pristis,Rhincodon typus,Cheilinus undulatus")
#   harvest_iucn_latest(c("Pristis pristis", "Rhincodon typus", "Cheilinus undulatus"))
#   harvest_iucn_latest_from_csv("path/to/species.csv", col = "Scientific Name")
#
# Authentication:
#   Run rredlist::rl_use_iucn() once to register and store your API key, or set env var IUCN_REDLIST_KEY.
#   See: https://api.iucnredlist.org/api-docs  (v4) and rredlist docs.

# Install required packages if not available
required_packages <- c(
  "dplyr",
  "stringr",
  "purrr",
  "readr",
  "tidyr",
  "rredlist"
)
missing_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(missing_packages) > 0) {
  cat(
    "Installing missing packages:",
    paste(missing_packages, collapse = ", "),
    "\n"
  )
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(rredlist)
})

split_genus_species <- function(x) {
  parts <- str_split(x, "\\s+", n = 3)[[1]]
  if (length(parts) < 2) {
    return(list(genus = NA_character_, species = NA_character_))
  }
  list(genus = str_to_title(parts[1]), species = str_to_lower(parts[2]))
}

# ---- Fetch helpers ----
# Latest assessment metadata per species
fetch_latest <- function(binomen, scope = "1") {
  gs <- split_genus_species(binomen)
  if (is.na(gs$genus) || is.na(gs$species)) {
    return(tibble(input_name = binomen, status = "PARSE_ERROR"))
  }
  # rl_species() returns a tibble with assessments incl. url, red_list_category_code, year_published
  res <- tryCatch(
    {
      rredlist::rl_species(gs$genus, gs$species, scope = scope)
    },
    error = function(e) NULL
  )
  if (is.null(res) || nrow(res$assessments) == 0) {
    return(tibble(input_name = binomen, status = "NOT_FOUND"))
  }
  # Take the latest TRUE row if present; else max year_published
  tbl <- res$assessments
  if ("latest" %in% names(tbl) && any(tbl$latest)) {
    latest_row <- tbl %>% dplyr::filter(latest) %>% dplyr::slice_tail(n = 1)
  } else {
    latest_row <- tbl %>%
      dplyr::arrange(dplyr::desc(.data$year_published)) %>%
      dplyr::slice_head(n = 1)
  }
  latest_row <- latest_row %>%
    transmute(
      input_name = binomen,
      sis_taxon_id,
      assessment_id,
      iucn_url = url,
      red_list_category_code,
      year_published = as.integer(year_published)
    )
  latest_row
}

# Enrich with details from rl_assessment() (criteria, assessment_date, population_trend, citation, red_list_category label)
fetch_assessment_details <- function(assessment_id) {
  det <- tryCatch(
    {
      rredlist::rl_assessment(assessment_id)
    },
    error = function(e) NULL
  )
  if (is.null(det)) {
    return(tibble(
      assessment_id = assessment_id,
      criteria = NA,
      assessment_date = NA,
      red_list_category = NA,
      population_trend = NA,
      citation = NA
    ))
  }

  # Extract elements and ensure they are single values (take first if vector)
  criteria_val <- det$criteria %||% NA_character_
  if (length(criteria_val) > 1) {
    criteria_val <- paste(criteria_val, collapse = "; ")
  }

  assessment_date_val <- det$assessment_date %||% NA_character_
  if (length(assessment_date_val) > 1) {
    assessment_date_val <- assessment_date_val[1]
  }

  red_list_category_val <- det$red_list_category %||% NA_character_
  if (length(red_list_category_val) > 1) {
    red_list_category_val <- red_list_category_val[1]
  }

  population_trend_val <- det$population_trend %||% NA_character_
  if (length(population_trend_val) > 1) {
    population_trend_val <- population_trend_val[1]
  }

  citation_val <- det$citation %||% NA_character_
  if (length(citation_val) > 1) {
    citation_val <- citation_val[1]
  }

  tibble(
    assessment_id = assessment_id,
    criteria = criteria_val,
    assessment_date = assessment_date_val,
    red_list_category = red_list_category_val,
    population_trend = population_trend_val,
    citation = citation_val
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Main Functions ----

#' Harvest IUCN Red List assessments for species
#' @param species_input Either a character vector of species names or a comma-separated string
#' @param output_file Path for output CSV file (optional). If NULL, returns data without saving
#' @param delay Delay between API calls in seconds (default: 0.6)
#' @param scope Assessment scope code (default: "1" for Global)
#' @return tibble with IUCN assessment data
harvest_iucn_latest <- function(
  species_input,
  output_file = "iucn_latest_assessments.csv",
  delay = 0.6,
  scope = "1"
) {
  # Handle different input formats
  if (
    is.character(species_input) &&
      length(species_input) == 1 &&
      grepl(",", species_input)
  ) {
    # Comma-separated string
    species_vec <- str_split(species_input, ",")[[1]] %>%
      str_squish() %>%
      discard(~ .x == "")
  } else if (is.character(species_input)) {
    # Character vector
    species_vec <- species_input %>%
      str_squish() %>%
      discard(~ is.na(.x) || .x == "")
  } else {
    stop("species_input must be a character vector or comma-separated string")
  }

  species_vec <- unique(species_vec)
  cat("Processing", length(species_vec), "species:\n")

  # Fetch latest assessments
  latest_tbl <- purrr::map_dfr(species_vec, function(s) {
    cat("  Fetching:", s, "\n")
    out <- fetch_latest(s, scope = scope)
    Sys.sleep(delay)
    out
  })

  # Any found?
  if (nrow(latest_tbl) == 0) {
    warning("No assessments retrieved.")
    return(tibble())
  }

  # Get detailed assessment information
  cat("Fetching assessment details...\n")
  details_tbl <- latest_tbl %>%
    dplyr::filter(!is.na(assessment_id)) %>%
    purrr::pmap_dfr(function(
      input_name,
      sis_taxon_id,
      assessment_id,
      iucn_url,
      red_list_category_code,
      year_published
    ) {
      det <- fetch_assessment_details(assessment_id)
      Sys.sleep(delay)
      # Combine without duplicating assessment_id
      tibble(
        input_name = input_name,
        sis_taxon_id = sis_taxon_id,
        assessment_id = assessment_id,
        iucn_url = iucn_url,
        red_list_category_code = red_list_category_code,
        year_published = year_published,
        criteria = det$criteria,
        assessment_date = det$assessment_date,
        red_list_category = det$red_list_category,
        population_trend = det$population_trend,
        citation = det$citation
      )
    })

  # Normalise columns
  out <- details_tbl %>%
    transmute(
      species = input_name,
      iucn_category_code = red_list_category_code,
      iucn_category_label = red_list_category,
      iucn_criteria = criteria,
      iucn_assessment_date = assessment_date,
      iucn_year_published = year_published,
      population_trend = population_trend,
      iucn_assessment_id = assessment_id,
      iucn_url = iucn_url,
      iucn_citation = citation
    )

  # Save to file if requested
  if (!is.null(output_file)) {
    readr::write_csv(out, output_file)
    message("Wrote: ", output_file)
  }

  return(out)
}

#' Harvest IUCN Red List assessments from a CSV file
#' @param csv_path Path to CSV file containing species names
#' @param col Column name containing species names (default: "Scientific Name")
#' @param output_file Path for output CSV file (optional)
#' @param delay Delay between API calls in seconds (default: 0.6)
#' @param scope Assessment scope code (default: "1" for Global)
#' @return tibble with IUCN assessment data
harvest_iucn_latest_from_csv <- function(
  csv_path,
  col = "Scientific Name",
  output_file = "iucn_latest_assessments.csv",
  delay = 0.6,
  scope = "1"
) {
  if (!file.exists(csv_path)) {
    stop("CSV file not found: ", csv_path)
  }

  dat <- suppressMessages(readr::read_csv(csv_path, show_col_types = FALSE))

  if (!col %in% names(dat)) {
    stop(
      "Column '",
      col,
      "' not found in CSV. Available columns: ",
      paste(names(dat), collapse = ", ")
    )
  }

  species_vec <- dat[[col]] %>%
    as.character() %>%
    str_squish() %>%
    discard(~ is.na(.x) || .x == "") %>%
    unique()

  harvest_iucn_latest(species_vec, output_file, delay, scope)
}

# ---- Example Usage ----
# Uncomment to test:
# result <- harvest_iucn_latest("Pristis pristis,Rhincodon typus,Cheilinus undulatus")
# print(result)

result <- harvest_iucn_latest(
  "Pristis pristis,Rhincodon typus,Cheilinus undulatus"
)
