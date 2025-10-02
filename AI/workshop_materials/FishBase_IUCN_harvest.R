# IUCN status harvester from FishBase for a vector of species names
# Input: a character vector c("Pristis pristis","Rhincodon typus", ...)
# Output: tibble with columns: species, iucn_status_fishbase, iucn_date_assessed, source

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(rvest)
  library(httr)
  library(rfishbase) # REST API to FishBase
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

# Build canonical FishBase summary URL (hyphen between Genus and species)
fb_url <- function(binomen) {
  if (is.na(binomen)) {
    return(NA_character_)
  }
  g <- str_split(binomen, "\\s+", n = 2)[[1]]
  paste0("https://www.fishbase.se/summary/", g[1], "-", g[2])
}

# Parse the FishBase species HTML page for the "IUCN Red List Status" line
scrape_fb_iucn <- function(binomen, pause = 0.75) {
  url <- fb_url(binomen)
  if (!nzchar(url)) {
    return(tibble(
      species = binomen,
      iucn_status_fishbase = NA_character_,
      iucn_date_assessed = NA_character_,
      source = NA_character_
    ))
  }
  # polite request
  ua <- httr::user_agent("WIO-fish-IUCN-scraper/1.0 (academic use)")
  resp <- httr::GET(url, ua, timeout(20))
  if (httr::http_error(resp)) {
    Sys.sleep(pause)
    return(tibble(
      species = binomen,
      iucn_status_fishbase = NA_character_,
      iucn_date_assessed = NA_character_,
      source = url
    ))
  }
  doc <- read_html(resp)
  text <- doc %>%
    html_elements("body") %>%
    html_text2()
  # Pull the "IUCN Red List Status" segment; keep the full status string incl. criteria codes
  line <- str_match(text, "IUCN\\s+Red\\s+List\\s+Status[^\n]*")[, 1]
  status <- if (!is.na(line)) {
    # after "Status" up to "Date assessed" (or EOL)
    s <- str_replace(line, ".*Status\\s*\\(?Ref\\.[^)]+\\)?:\\s*", "")
    s <- str_replace(s, "\\s*Date assessed:.*$", "")
    str_squish(s)
  } else {
    NA_character_
  }
  # Date assessed (free text date on page)
  date <- if (!is.na(line)) {
    d <- str_match(line, "Date assessed:\\s*([^\\.;\\n]+)")[, 2]
    str_squish(d)
  } else {
    NA_character_
  }

  Sys.sleep(pause)
  tibble(
    species = binomen,
    iucn_status_fishbase = if (nzchar(status)) status else NA_character_,
    iucn_date_assessed = if (nzchar(date)) date else NA_character_,
    source = url
  )
}

# Try rfishbase first; if fields absent/empty, fall back to scraping
# Note: rfishbase exposes the FishBase 'species' table; not all deployments surface an IUCN field.
# We therefore treat API return as advisory and rely on the HTML page for the normative string/date.   [oai_citation:3‡rOpenSci Docs](https://docs.ropensci.org/rfishbase/reference/species.html)
fetch_iucn <- function(x) {
  spp <- unique(na.omit(norm_binomen(x)))
  # attempt API call to species table (kept in case FB later adds explicit IUCN cols)
  fb_api <- tryCatch(
    species(spp, fields = NULL, version = "latest", server = "fishbase"),
    error = function(e) NULL
  )
  api_cols <- names(fb_api %||% data.frame())
  # heuristics for any IUCN-like field names, if present
  guess_cols <- grep(
    "iucn|red.?list|threat",
    api_cols,
    ignore.case = TRUE,
    value = TRUE
  )

  # per-species retrieval with fallback to HTML scraping
  map_dfr(spp, function(s) {
    api_hit <- NULL
    if (!is.null(fb_api) && length(guess_cols)) {
      row <- fb_api %>%
        filter(Species == s | paste(Genus, Species) == s) %>%
        slice_head(n = 1)
      if (nrow(row)) {
        # concatenate any guessed IUCN fields found
        api_hit <- row %>%
          select(any_of(guess_cols)) %>%
          pivot_longer(everything()) %>%
          filter(!is.na(value), nzchar(as.character(value))) %>%
          pull(value) %>%
          unique() %>%
          paste(collapse = " | ")
      }
    }
    # prefer scraping, as it reliably yields the exact status string + assessment date shown to users
    scr <- scrape_fb_iucn(s)
    scr$iucn_status_fishbase <- if (nzchar(scr$iucn_status_fishbase)) {
      scr$iucn_status_fishbase
    } else {
      api_hit %||% NA_character_
    }
    scr
  })
}

# --- main user-facing function --------------------------------------------

#' Harvest FishBase IUCN status for a comma-separated vector of binomina
#' @param species_csv A single string like "Pristis pristis, Rhincodon typus, Cheilinus undulatus"
#' @return tibble with species, iucn_status_fishbase, iucn_date_assessed, source
harvest_fishbase_iucn <- function(species_csv) {
  vec <- str_split(species_csv, ",")[[1]] %>% str_squish()
  out <- fetch_iucn(vec) %>%
    mutate(
      # standardise a compact code where present in the FishBase string (e.g., "Endangered (EN) (A2bd)")
      iucn_code = str_match(iucn_status_fishbase, "\\(([A-Z]{2})\\)")[, 2]
    ) %>%
    relocate(iucn_code, .after = iucn_status_fishbase)
  out
}

# --- example ----------------------------------------------------------------
# res <- harvest_fishbase_iucn("Pristis pristis, Rhincodon typus, Cheilinus undulatus")
# print(res)
# readr::write_csv(res, "fishbase_iucn_status.csv")

res <- harvest_fishbase_iucn(
  "Pristis pristis, Rhincodon typus, Cheilinus undulatus"
)
print(res)
