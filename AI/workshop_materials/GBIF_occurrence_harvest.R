# GBIF occurrence harvester for a vector of species names
# Input: a character vector of species names (comma-separated string)
# Output: long CSV file with species, country, and occurrence counts

# Install required packages if not available
required_packages <- c(
  "dplyr",
  "stringr",
  "purrr",
  "readr",
  "rgbif",
  "httr",
  "jsonlite"
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
  library(rgbif) # R interface to GBIF
  library(httr)
  library(jsonlite)
})

# --- helpers ---------------------------------------------------------------

# Normalise a binomen to "Genus species"
norm_binomen <- function(x) {
  x <- str_squish(as.character(x))

  # Handle vectorized input
  sapply(
    x,
    function(single_x) {
      if (length(single_x) == 0 || !nzchar(single_x)) {
        return(NA_character_)
      }
      parts <- str_split(single_x, "\\s+", n = 3)[[1]]
      paste0(
        stringr::str_to_title(parts[1]),
        " ",
        if (length(parts) >= 2) stringr::str_to_lower(parts[2]) else ""
      )
    },
    USE.NAMES = FALSE
  )
}

# Get GBIF occurrence counts by country for a single species
get_gbif_country_counts <- function(species_name) {
  cat("Processing:", species_name, "\n")

  # Get species key from GBIF
  name_lookup <- tryCatch(
    {
      name_backbone(name = species_name, rank = "species")
    },
    error = function(e) {
      warning("Failed to lookup species: ", species_name, " - ", e$message)
      return(NULL)
    }
  )

  if (is.null(name_lookup) || is.null(name_lookup$usageKey)) {
    warning("No GBIF key found for: ", species_name)
    return(tibble(
      species = species_name,
      country = NA_character_,
      country_code = NA_character_,
      occurrence_count = 0L
    ))
  }

  # Get occurrence search results with country facet
  search_results <- tryCatch(
    {
      occ_search(
        taxonKey = name_lookup$usageKey,
        facet = "country",
        facetLimit = 300,
        limit = 0
      )
    },
    error = function(e) {
      warning(
        "Failed to search occurrences for: ",
        species_name,
        " - ",
        e$message
      )
      return(NULL)
    }
  )

  if (
    is.null(search_results) ||
      is.null(search_results$facets) ||
      is.null(search_results$facets$country)
  ) {
    return(tibble(
      species = species_name,
      country = NA_character_,
      country_code = NA_character_,
      occurrence_count = 0L
    ))
  }

  # Extract country facet data
  country_data <- search_results$facets$country

  if (is.null(country_data) || nrow(country_data) == 0) {
    return(tibble(
      species = species_name,
      country = NA_character_,
      country_code = NA_character_,
      occurrence_count = 0L
    ))
  }

  # Return formatted data
  country_data %>%
    mutate(
      species = species_name,
      country_code = name,
      occurrence_count = as.integer(count)
    ) %>%
    # Get country names from country codes
    left_join(
      rgbif::enumeration_country() %>%
        select(iso2, title) %>%
        rename(country_code = iso2, country = title),
      by = "country_code"
    ) %>%
    select(species, country, country_code, occurrence_count) %>%
    arrange(desc(occurrence_count))
}

# Main function to process multiple species
fetch_gbif_occurrences <- function(species_vector) {
  spp <- unique(na.omit(norm_binomen(species_vector)))
  cat("Processing", length(spp), "species:\n")

  # Process each species and combine results
  map_dfr(spp, function(sp) {
    result <- get_gbif_country_counts(sp)
    Sys.sleep(0.5) # Be polite to GBIF API
    result
  }) %>%
    filter(occurrence_count > 0) %>% # Remove countries with 0 occurrences
    arrange(species, desc(occurrence_count))
}

# --- main user-facing function --------------------------------------------

#' Harvest GBIF occurrence data by country for a comma-separated vector of species
#' @param species_csv A single string like "Pristis pristis, Rhincodon typus, Cheilinus undulatus"
#' @param output_file Path for the output CSV file (optional)
#' @return tibble with species, country, country_code, occurrence_count
harvest_gbif_occurrences <- function(
  species_csv,
  output_file = "gbif_occurrences_by_country.csv"
) {
  vec <- str_split(species_csv, ",")[[1]] %>% str_squish()
  cat("Input species:", paste(vec, collapse = ", "), "\n")

  # Fetch occurrence data
  results <- fetch_gbif_occurrences(vec)

  # Save to CSV
  write_csv(results, output_file)
  cat("Results saved to:", output_file, "\n")
  cat("Total records:", nrow(results), "\n")
  cat("Species with data:", length(unique(results$species)), "\n")

  return(results)
}

# --- example with WIO species ----------------------------------------------------------------

# Test with three Western Indian Ocean species
wio_species <- "Pristis pristis, Rhincodon typus, Cheilinus undulatus"
results <- harvest_gbif_occurrences(
  wio_species,
  "gbif_wio_species_occurrences.csv"
)
print(results)
