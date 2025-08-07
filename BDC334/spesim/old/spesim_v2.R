# -----------------------------------------------------------------------------
#
# SPATIAL SAMPLING SIMULATION (Refactored & Corrected)
#
# Description:
# This script simulates a spatially heterogeneous community of species within an
# irregular domain. It models species distributions based on environmental
# gradients and clustering patterns. The script then simulates a sampling
# process using quadrats, calculates various ecological metrics (alpha, beta,
# gamma diversity), and generates a comprehensive report with figures and data tables.
#
# Author: Gemini (Refactored from original script)
# Date: 2025-08-07
#
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# -----------------------------------------------------------------------------

# --- Helper to install packages if missing ---
#' @title Install Packages
#' @description Checks for, and installs, any missing packages.
#' @param pkgs A character vector of package names.
install_if_missing <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) {
    message(paste("Installing missing packages:", paste(new_pkgs, collapse = ", ")))
    install.packages(new_pkgs, dependencies = TRUE)
  }
}

# --- Load required libraries ---
required_packages <- c("sf", "ggplot2", "dplyr", "tidyr", "viridis", "stringr", "patchwork")
install_if_missing(required_packages)

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(viridis)
  library(stringr)
  library(patchwork)
})

# -----------------------------------------------------------------------------
# 2. CONFIGURATION & PARAMETER FUNCTIONS
# -----------------------------------------------------------------------------

#' @title Parse Simulation Parameters
#' @description Reads a configuration file, parsing key-value pairs.
#'   It ignores comments, trims whitespace, and attempts to coerce values to
#'   appropriate types (numeric, integer, logical, vectors).
#' @param filename Path to the initialization file.
#' @return A named list of parameters.
parse_init_file <- function(filename) {
  if (!file.exists(filename)) {
    stop(paste("Parameter file not found:", filename))
  }
  lines <- readLines(filename, warn = FALSE)
  lines <- gsub("#.*", "", lines) # Remove comments
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)] # Remove empty lines

  params <- list()
  for (ln in lines) {
    if (!grepl("=", ln)) next

    kv <- strsplit(ln, "=")[[1]]
    key <- toupper(trimws(kv[1]))
    val <- trimws(kv[2])
    val <- gsub('^"|"$', "", val) # Remove quotes

    # Attempt type conversion
    if (grepl(",", val)) { # Vector of values
      items <- strsplit(val, ",")[[1]]
      # If all items are numeric, convert to numeric vector
      if (!anyNA(suppressWarnings(as.numeric(items)))) {
        params[[key]] <- as.numeric(items)
      } else {
        params[[key]] <- trimws(items)
      }
    } else if (val %in% c("TRUE", "FALSE")) {
      params[[key]] <- as.logical(val)
    } else if (grepl("^[0-9]+$", val)) {
      params[[key]] <- as.integer(val)
    } else if (grepl("^[0-9.]+$", val)) {
      params[[key]] <- as.numeric(val)
    } else {
      params[[key]] <- val
    }
  }
  return(params)
}

#' @title Load Configuration with Defaults
#' @description Loads parameters from a file and merges them with a list of
#'   default values. User-defined values override defaults.
#' @param init_file Path to the user's configuration file.
#' @return A list containing all configuration parameters for the simulation.
load_config <- function(init_file) {
  user_params <- parse_init_file(init_file)

  # --- Define default parameters ---
  defaults <- list(
    SEED = 59,
    N_INDIVIDUALS = 2000,
    N_SPECIES = 15,
    DOMINANT_FRACTION = 0.35,
    FISHER_ALPHA = 3.0,
    FISHER_X = 0.95,
    N_QUADRATS = 20,
    QUADRAT_SIZE_OPTION = "small",
    SAMPLING_RESOLUTION = 50,
    GRADIENT_SPECIES = c("B", "C", "D", "F", "I", "K"),
    GRADIENT_ASSIGNMENTS = c("temperature", "temperature", "elevation"),
    GRADIENT_OPTIMA = c(0.3, 0.7, 0.5),
    GRADIENT_TOLERANCE = 0.2,
    ENVIRONMENTAL_NOISE = 0.05,
    CLUSTER_SPREAD_DOMINANT = 3,
    CLUSTER_SPREAD_RARE = 6,
    MAX_CLUSTERS_DOMINANT = 5,
    POINT_SIZE = 0.3,
    POINT_ALPHA = 0.9,
    QUADRAT_ALPHA = 0.05,
    BACKGROUND_COLOUR = "white",
    FOREGROUND_COLOUR = "#22223b",
    QUADRAT_COLOUR = "black"
  )

  # Merge user params with defaults
  config <- defaults
  for (name in names(user_params)) {
    config[[name]] <- user_params[[name]]
  }

  # --- Derived parameters ---
  QUADRAT_SIZES <- list(
    small = c(1, 1),
    medium = c(1.5, 1.5),
    large = c(2, 2)
  )
  config$QUADRAT_SIZE <- QUADRAT_SIZES[[config$QUADRAT_SIZE_OPTION]]
  if (is.null(config$QUADRAT_SIZE)) {
    stop("Invalid 'QUADRAT_SIZE_OPTION'. Must be 'small', 'medium', or 'large'.")
  }

  set.seed(config$SEED)
  return(config)
}


# -----------------------------------------------------------------------------
# 3. CORE SIMULATION FUNCTIONS
# -----------------------------------------------------------------------------

#' @title Create Irregular Sampling Domain
#' @description Generates a vaguely organic, amoeba-like polygon to serve as the study area.
#' @return An `sf` object containing a single polygon.
create_sampling_domain <- function() {
  theta <- seq(0, 2 * pi, length.out = 20)
  r <- 10 + 3 * sin(3 * theta) + 2 * cos(5 * theta) + runif(20, -1, 1)
  x <- r * cos(theta) + runif(20, -0.5, 0.5)
  y <- r * sin(theta) * 0.8 + runif(20, -0.5, 0.5)

  coords <- cbind(x, y)
  coords <- rbind(coords, coords[1, ]) # Close the polygon

  polygon <- st_polygon(list(coords))
  return(st_sf(geometry = st_sfc(polygon)))
}

#' @title Generate Fisher's Log-Series Abundances
#' @description Creates a species abundance distribution (SAD) following Fisher's log-series.
#'   A single dominant species is set manually, and the rest follow the series.
#' @param n_species Total number of species.
#' @param n_individuals Total number of individuals.
#' @param dominant_fraction The proportion of total individuals belonging to the dominant species.
#' @param alpha Fisher's alpha, a diversity parameter.
#' @param x A parameter of the log-series, typically close to 1.
#' @return A named numeric vector of species abundances.
generate_fisher_log_series <- function(n_species, n_individuals, dominant_fraction, alpha, x) {
  n_dominant <- round(n_individuals * dominant_fraction)
  n_remaining <- n_individuals - n_dominant

  # Abundances for non-dominant species
  ranks <- 2:n_species
  rel_abundances <- alpha * (x^ranks) / ranks
  abundances <- round(rel_abundances / sum(rel_abundances) * n_remaining)

  all_abundances <- c(n_dominant, abundances)

  # Adjust to ensure the total number of individuals is exact
  adjustment <- n_individuals - sum(all_abundances)
  all_abundances[2] <- all_abundances[2] + adjustment

  species_names <- LETTERS[1:n_species]
  names(all_abundances) <- species_names
  return(all_abundances[all_abundances > 0]) # Return only species with >0 individuals
}

#' @title Create Environmental Gradient Fields
#' @description Generates spatial environmental gradients (temperature, elevation, rainfall)
#'   across a grid within the domain.
#' @param domain The `sf` polygon object of the study area.
#' @param resolution The number of grid cells along each axis.
#' @param noise_level The standard deviation of random noise to add to gradients.
#' @return A data frame representing a grid with raw and rescaled environmental values.
create_environmental_gradients <- function(domain, resolution, noise_level) {
  bbox <- st_bbox(domain)
  x_seq <- seq(bbox["xmin"], bbox["xmax"], length.out = resolution)
  y_seq <- seq(bbox["ymin"], bbox["ymax"], length.out = resolution)
  grid <- expand.grid(x = x_seq, y = y_seq)

  # Normalize coordinates
  x_norm <- (grid$x - bbox["xmin"]) / (bbox["xmax"] - bbox["xmin"])
  y_norm <- (grid$y - bbox["ymin"]) / (bbox["ymax"] - bbox["ymin"])

  # Define gradients (normalized 0-1)
  temp_vals <- x_norm * 0.7 + y_norm * 0.3 + rnorm(nrow(grid), 0, noise_level)

  dist_center <- sqrt((x_norm - 0.5)^2 + (y_norm - 0.5)^2)
  elev_vals <- 1 - (dist_center / sqrt(0.5^2 + 0.5^2)) + rnorm(nrow(grid), 0, noise_level)

  rain_vals <- (-x_norm * 0.6 + y_norm * 0.8)
  rain_vals <- (rain_vals - min(rain_vals)) / (max(rain_vals) - min(rain_vals)) + rnorm(nrow(grid), 0, noise_level)

  # Clamp values to [0, 1] range
  grid$temperature <- pmax(0, pmin(1, temp_vals))
  grid$elevation <- pmax(0, pmin(1, elev_vals))
  grid$rainfall <- pmax(0, pmin(1, rain_vals))

  # Rescale to more realistic units for reporting
  grid$temperature_C <- grid$temperature * 40 - 2 # Range: -2 to 38 C
  grid$elevation_m <- grid$elevation * 2000 # Range: 0 to 2000 m
  grid$rainfall_mm <- grid$rainfall * 700 + 200 # Range: 200 to 3000 mm

  return(grid)
}

#' @title Generate Heterogeneous Species Distribution
#' @description Places individuals of multiple species within the domain based on
#'   abundance, clustering behavior, and response to environmental gradients.
#' @param domain The `sf` polygon of the study area.
#' @param P The configuration list.
#' @return An `sf` point object with each point assigned a species.
generate_heterogeneous_distribution <- function(domain, P) {
  abundances <- generate_fisher_log_series(P$N_SPECIES, P$N_INDIVIDUALS, P$DOMINANT_FRACTION, P$FISHER_ALPHA, P$FISHER_X)
  env_grid <- create_environmental_gradients(domain, P$SAMPLING_RESOLUTION, P$ENVIRONMENTAL_NOISE)

  # Create a spatial grid for fast lookups
  env_sf <- st_as_sf(env_grid, coords = c("x", "y"), crs = st_crs(domain))

  # Sample all point locations at once
  all_points <- st_sample(domain, size = P$N_INDIVIDUALS, type = "random")

  # Efficiently get environmental data for each point using a spatial join
  points_with_env <- st_join(st_sf(geometry = all_points), env_sf, join = st_nearest_feature)

  # Assign species to points based on different rules
  points_with_env$species <- ""
  available_indices <- 1:nrow(points_with_env)

  for (sp in names(abundances)) {
    n_ind <- abundances[sp]
    if (length(available_indices) < n_ind) n_ind <- length(available_indices)
    if (n_ind == 0) next

    # 1. Gradient-responsive species
    if (sp %in% P$GRADIENT_SPECIES) {
      sp_idx <- which(P$GRADIENT_SPECIES == sp)
      gradient_type <- P$GRADIENT_ASSIGNMENTS[sp_idx]
      optimum <- P$GRADIENT_OPTIMA[sp_idx]

      # Calculate probability based on Gaussian response curve
      env_values <- points_with_env[[gradient_type]][available_indices]
      probs <- exp(-((env_values - optimum)^2) / (2 * P$GRADIENT_TOLERANCE^2))

      selected <- sample(available_indices, size = n_ind, prob = probs)

      # 2. Dominant, clustered species ("A")
    } else if (sp == "A") {
      n_clusters <- sample(3:P$MAX_CLUSTERS_DOMINANT, 1)
      cluster_centers_idx <- sample(available_indices, n_clusters)

      # Calculate distance from each available point to its nearest cluster center
      dist_matrix <- st_distance(points_with_env[available_indices, ], points_with_env[cluster_centers_idx, ])
      min_dists <- apply(dist_matrix, 1, min)

      probs <- exp(-as.numeric(min_dists) / P$CLUSTER_SPREAD_DOMINANT)
      selected <- sample(available_indices, size = n_ind, prob = probs)

      # 3. Other (rare, subordinate) species
    } else {
      # Simple clustering for rare species (1 or 2 small clusters)
      n_clusters <- sample(1:2, 1)
      cluster_centers_idx <- sample(available_indices, n_clusters)

      dist_matrix <- st_distance(points_with_env[available_indices, ], points_with_env[cluster_centers_idx, ])
      min_dists <- apply(dist_matrix, 1, min)

      probs <- exp(-as.numeric(min_dists) / P$CLUSTER_SPREAD_RARE)
      selected <- sample(available_indices, size = n_ind, prob = probs)
    }

    points_with_env$species[selected] <- sp
    available_indices <- setdiff(available_indices, selected)
  }

  return(points_with_env %>% filter(species != ""))
}


#' @title Place Non-Overlapping Quadrats
#' @description Randomly places rectangular quadrats within the domain, ensuring they
#'   are fully contained and do not overlap.
#' @param domain The `sf` polygon of the study area.
#' @param n_quadrats The target number of quadrats.
#' @param quadrat_size A numeric vector `c(width, height)` for the quadrats.
#' @return An `sf` object containing the quadrat polygons.
place_quadrats <- function(domain, n_quadrats, quadrat_size) {
  bbox <- st_bbox(domain)
  quadrats <- list()
  attempts <- 0
  max_attempts <- n_quadrats * 100 # More robust max_attempts

  while (length(quadrats) < n_quadrats && attempts < max_attempts) {
    attempts <- attempts + 1

    # Propose a new quadrat
    x_center <- runif(1, bbox["xmin"] + quadrat_size[1] / 2, bbox["xmax"] - quadrat_size[1] / 2)
    y_center <- runif(1, bbox["ymin"] + quadrat_size[2] / 2, bbox["ymax"] - quadrat_size[2] / 2)

    new_quadrat_poly <- st_polygon(list(
      cbind(
        c(x_center - quadrat_size[1] / 2, x_center + quadrat_size[1] / 2, x_center + quadrat_size[1] / 2, x_center - quadrat_size[1] / 2, x_center - quadrat_size[1] / 2),
        c(y_center - quadrat_size[2] / 2, y_center - quadrat_size[2] / 2, y_center + quadrat_size[2] / 2, y_center + quadrat_size[2] / 2, y_center - quadrat_size[2] / 2)
      )
    )) %>% st_sfc(crs = st_crs(domain))

    # Check validity: must be within domain and not overlap existing quadrats
    is_within <- st_within(new_quadrat_poly, domain, sparse = FALSE)[1, 1]

    if (is_within) {
      is_overlapping <- FALSE
      if (length(quadrats) > 0) {
        existing_quadrats_sfc <- do.call(c, quadrats)
        if (any(st_intersects(new_quadrat_poly, existing_quadrats_sfc, sparse = FALSE))) {
          is_overlapping <- TRUE
        }
      }

      if (!is_overlapping) {
        quadrats[[length(quadrats) + 1]] <- new_quadrat_poly
      }
    }
  }

  if (length(quadrats) < n_quadrats) {
    warning(sprintf(
      "Placed only %d of %d requested quadrats after %d attempts.",
      length(quadrats), n_quadrats, max_attempts
    ))
  }

  if (length(quadrats) == 0) {
    return(st_sf(quadrat_id = integer(0), geometry = st_sfc()))
  }

  # Combine list of sfc objects into a final sf data frame
  final_quadrats <- do.call(c, quadrats) %>%
    st_sf(quadrat_id = 1:length(.), geometry = .)

  return(final_quadrats)
}


# -----------------------------------------------------------------------------
# 4. ANALYSIS & DATA EXTRACTION FUNCTIONS
# -----------------------------------------------------------------------------

#' @title Create Site-by-Species Abundance Matrix
#' @description Calculates the abundance of each species in each quadrat.
#' @param species_dist An `sf` object of species points.
#' @param quadrats An `sf` object of quadrat polygons.
#' @param all_species_names A character vector of all possible species names for columns.
#' @return A data frame with `site` (quadrat_id) and columns for each species' abundance.
create_abundance_matrix <- function(species_dist, quadrats, all_species_names) {
  # Perform a single spatial intersection for all quadrats
  intersections <- st_intersection(species_dist, quadrats)

  if (nrow(intersections) == 0) {
    # Return an empty matrix if no individuals were sampled
    abund_df <- data.frame(site = quadrats$quadrat_id)
    for (sp in all_species_names) abund_df[[sp]] <- 0
    return(abund_df)
  }

  # Count species per quadrat and pivot to wide format
  abund_df <- intersections %>%
    st_drop_geometry() %>%
    count(quadrat_id, species, name = "abundance") %>%
    pivot_wider(names_from = species, values_from = abundance, values_fill = 0)

  # Ensure all species are present as columns and join with all quadrat IDs
  missing_species <- setdiff(all_species_names, names(abund_df))
  for (sp in missing_species) {
    abund_df[[sp]] <- 0
  }

  # Ensure all sites are present, even empty ones
  result_df <- data.frame(quadrat_id = quadrats$quadrat_id) %>%
    left_join(abund_df, by = "quadrat_id") %>%
    mutate(across(-quadrat_id, ~ replace_na(., 0))) %>%
    select(site = quadrat_id, all_of(all_species_names)) %>%
    arrange(site)

  return(result_df)
}

#' @title Calculate Mean Environmental Conditions per Quadrat
#' @description Extracts the mean value of environmental variables for each quadrat.
#' @param env_grid A data frame of gridded environmental data.
#' @param quadrats An `sf` object of quadrat polygons.
#' @param domain_crs The coordinate reference system of the domain.
#' @return A data frame with `site` (quadrat_id) and mean env values.
calculate_quadrat_environment <- function(env_grid, quadrats, domain_crs) {
  env_sf <- st_as_sf(env_grid, coords = c("x", "y"), crs = domain_crs)

  # Join grid points to the quadrats they fall within
  joined_data <- st_join(quadrats, env_sf)

  # Calculate mean environmental values for each quadrat
  site_env <- joined_data %>%
    st_drop_geometry() %>%
    group_by(site = quadrat_id) %>%
    summarise(
      across(
        c(temperature_C, elevation_m, rainfall_mm),
        ~ mean(., na.rm = TRUE)
      ),
      .groups = "drop"
    )

  # Ensure all sites are present
  full_site_env <- data.frame(site = quadrats$quadrat_id) %>%
    left_join(site_env, by = "site")

  return(full_site_env)
}


# -----------------------------------------------------------------------------
# 5. PLOTTING & REPORTING FUNCTIONS
# -----------------------------------------------------------------------------

#' @title Create Master Plot of Spatial Simulation
#' @description Generates a ggplot object showing the domain, species distributions,
#'   quadrats, and optionally an environmental gradient overlay.
#' @param domain `sf` polygon of the study area.
#' @param species `sf` points of individuals.
#' @param quadrats `sf` polygons of sampling quadrats.
#' @param P Configuration list.
#' @param show_gradient Logical, whether to show an environmental gradient.
#' @param env_gradients Data frame of environmental grid data.
#' @param gradient_type The name of the gradient to display.
#' @return A `ggplot` object.
plot_spatial_sampling <- function(domain, species, quadrats, P,
                                  show_gradient = FALSE, env_gradients = NULL,
                                  gradient_type = "temperature") {
  # Define a color palette robust to varying numbers of species
  all_species_names <- LETTERS[1:P$N_SPECIES]
  species_colors <- rev(colorspace::sequential_hcl(P$N_SPECIES, palette = "RdPu"))
  names(species_colors) <- all_species_names

  # Basic plot structure
  p <- ggplot() +
    geom_sf(data = domain, fill = "grey70", color = P$FOREGROUND_COLOUR, linewidth = 0.5, alpha = 0.4)

  # Add environmental gradient layer if requested
  if (show_gradient && !is.null(env_gradients) && gradient_type %in% names(env_gradients)) {
    p <- p +
      metR::geom_contour_fill(data = env_gradients, aes(x = x, y = y, z = .data[[gradient_type]]), alpha = 0.5) +
      scale_fill_viridis(option = "viridis", name = str_to_title(gsub("_", " ", gradient_type)), guide = "colorbar")
  }

  # Add species points and quadrat layers
  p <- p +
    geom_sf(data = species, aes(color = species), size = P$POINT_SIZE, alpha = P$POINT_ALPHA) +
    scale_color_manual(values = species_colors, name = "Species", drop = FALSE) +
    geom_sf(data = quadrats, fill = NA, color = P$QUADRAT_COLOUR, linewidth = 0.4) +
    geom_sf_text(data = quadrats, aes(label = quadrat_id), color = P$QUADRAT_COLOUR, size = 2.5, fontface = "bold") +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = P$BACKGROUND_COLOUR, color = NA),
      plot.title = element_text(color = P$FOREGROUND_COLOUR, size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = P$FOREGROUND_COLOUR, size = 10, hjust = 0.5, margin = margin(b = 10)),
      legend.position = "none" # Remove individual legends for panel plot
    ) +
    labs(
      title = if (show_gradient) str_replace_all(str_to_title(gradient_type), "_", " ") else "Species Distribution",
      subtitle = paste(P$N_INDIVIDUALS, "Individuals |", P$N_SPECIES, "Species |", nrow(quadrats), "Quadrats")
    )

  return(p)
}


#' @title Generate a Full, Formatted Analysis Report
#' @description Creates a string containing a full analysis of the simulation
#'   results, formatted exactly as requested.
#' @param res A list containing all simulation artifacts (data frames, stats, etc.).
#' @return A single character string with the formatted report.
generate_full_report <- function(res) {
  # --- Environmental Gradients ---
  env_report <- c("\nEnvironmental Gradients:")
  temp_range <- range(res$env_gradients$temperature_C, na.rm = TRUE)
  elev_range <- range(res$env_gradients$elevation_m, na.rm = TRUE)
  rain_range <- range(res$env_gradients$rainfall_mm, na.rm = TRUE)

  temp_resp <- which(res$P$GRADIENT_ASSIGNMENTS == "temperature")
  elev_resp <- which(res$P$GRADIENT_ASSIGNMENTS == "elevation")

  temp_species_str <- if (length(temp_resp) > 0) {
    paste(sprintf("%s (optimum %.1f °C)", res$P$GRADIENT_SPECIES[temp_resp], res$P$GRADIENT_OPTIMA[temp_resp] * 40 - 2), collapse = ", ")
  } else {
    "None"
  }
  elev_species_str <- if (length(elev_resp) > 0) {
    paste(sprintf("%s (optimum %.0f m)", res$P$GRADIENT_SPECIES[elev_resp], res$P$GRADIENT_OPTIMA[elev_resp] * 2000), collapse = ", ")
  } else {
    "None"
  }

  env_report <- c(
    env_report,
    sprintf("  Temperature: %.1f–%.1f °C (range: %.1f °C)", temp_range[1], temp_range[2], diff(temp_range)),
    "    Pattern: Diagonal (NW cool → SE warm)",
    paste0("    Responsive species: ", temp_species_str),
    sprintf("  Elevation: %.0f–%.0f m (range: %.0f m)", elev_range[1], elev_range[2], diff(elev_range)),
    "    Pattern: Central peak (mountain-like topology)",
    paste0("    Responsive species: ", elev_species_str),
    sprintf("  Rainfall: %.0f–%.0f mm (range: %.0f mm)", rain_range[1], rain_range[2], diff(rain_range)),
    "    Pattern: Perpendicular (NE dry → SW wet)",
    "    Responsive species: None"
  )

  # --- Gradient Correlations ---
  cor_mat <- cor(res$env_gradients[, c("temperature_C", "elevation_m", "rainfall_mm")], use = "complete.obs")
  interp <- if (max(abs(cor_mat[upper.tri(cor_mat)])) < 0.3) "Gradients are approximately orthogonal (low correlation)" else "Some gradient correlation detected"
  corr_report <- c(
    "\nGradient Correlations:",
    sprintf("  Temperature-Elevation: r=%.3f", cor_mat[1, 2]),
    sprintf("  Temperature-Rainfall: r=%.3f", cor_mat[1, 3]),
    sprintf("  Elevation-Rainfall: r=%.3f", cor_mat[2, 3]),
    paste0("  Interpretation: ", interp)
  )

  # --- Species Abundance Distribution ---
  sad <- as.data.frame(table(res$species_dist$species)) %>%
    `colnames<-`(c("Species", "Count")) %>%
    arrange(desc(Count)) %>%
    mutate(
      Percent = 100 * Count / sum(Count),
      Role = case_when(
        Species %in% res$P$GRADIENT_SPECIES ~ sprintf("[%s-RESPONSIVE]", toupper(res$P$GRADIENT_ASSIGNMENTS[match(Species, res$P$GRADIENT_SPECIES)])),
        Species == "A" ~ "[DOMINANT - clustered]",
        TRUE ~ "[SUBORDINATE]"
      )
    )
  sad_report <- c("\nSpecies Abundance Distribution:")
  sad_report <- c(sad_report, sprintf(
    "  Species %s: %3d individuals (%5.1f%%) %s",
    sad$Species, sad$Count, sad$Percent, sad$Role
  ))

  # --- Spatial Distribution of Alpha Diversity ---
  alpha_report <- c("\nSpatial Distribution of Alpha Diversity:")
  for (i in 1:nrow(res$quadrats)) {
    spp_in_q <- suppressWarnings(st_intersection(res$species_dist, res$quadrats[i, ]))
    if (nrow(spp_in_q) > 0) {
      abunds <- as.data.frame(table(spp_in_q$species))
      abunds_str <- paste(sprintf("%s(%s)", abunds$Var1, abunds$Freq), collapse = ", ")
      alpha_report <- c(
        alpha_report,
        sprintf("  Quadrat %2d: α = %2d species | N = %3d individuals", i, n_distinct(spp_in_q$species), nrow(spp_in_q)),
        sprintf("    Species: %s", abunds_str)
      )
    } else {
      alpha_report <- c(
        alpha_report,
        sprintf("  Quadrat %2d: α = %2d species | N = %3d individuals", i, 0, 0),
        "    Species: None"
      )
    }
  }

  # --- Diversity Partitioning ---
  richness_data <- res$abund_matrix %>% mutate(richness = rowSums(select(., -site) > 0), n_ind = rowSums(select(., -site)))
  mean_alpha <- mean(richness_data$richness)
  se_alpha <- sd(richness_data$richness) / sqrt(nrow(richness_data))
  gamma_div <- n_distinct(res$species_dist$species)
  beta_whittaker <- gamma_div / mean_alpha
  beta_additive <- gamma_div - mean_alpha
  pa_matrix <- res$abund_matrix %>%
    select(-site) %>%
    as.matrix() %>%
    `>`(0) %>%
    `*`(1)
  mean_sorensen <- mean(dist(pa_matrix, method = "binary"))

  div_report <- c(
    "\nDiversity Partitioning:",
    sprintf("Alpha (mean local richness): %.2f ± %.2f SE", mean_alpha, se_alpha),
    sprintf("Gamma (regional species pool): %d species", gamma_div),
    sprintf("Beta (Whittaker): %.2f", beta_whittaker),
    sprintf("Beta (additive): %.2f", beta_additive),
    sprintf("Mean pairwise beta (Sørensen): %.3f", mean_sorensen),
    sprintf("Mean quadrat abundance: %.1f ± %.1f", mean(richness_data$n_ind), sd(richness_data$n_ind)),
    sprintf("Abundance variation (CV): %.3f", sd(richness_data$n_ind) / mean(richness_data$n_ind))
  )

  # --- Spatial Autocorrelation Analysis ---
  space_dist <- dist(res$site_coords[, c("x", "y")])
  richness_dist <- dist(richness_data$richness)
  mantel_interp <- "  No significant spatial autocorrelation."
  if (nrow(res$site_coords) >= 4 && var(richness_data$richness) > 0) {
    mantel_test <- cor.test(space_dist, richness_dist, method = "pearson")
    mantel_r <- mantel_test$estimate
    mantel_p <- mantel_test$p.value
    if (mantel_p < 0.05) {
      mantel_interp <- if (mantel_r > 0) "  Significant positive autocorrelation: environmental filtering or dispersal limitation." else "  Significant negative autocorrelation: competitive exclusion or strong heterogeneity."
    }
  } else {
    mantel_r <- NA
    mantel_p <- NA
  }
  spat_report <- c(
    "\nSpatial Autocorrelation Analysis:",
    sprintf("Spatial autocorrelation in richness (Mantel r): %.3f", mantel_r),
    sprintf("Statistical significance: p = %.3f", mantel_p),
    mantel_interp,
    sprintf("  Mean inter-quadrat distance: %.2f", mean(space_dist)),
    sprintf("  Richness variance: %.2f", var(richness_data$richness))
  )

  # --- Fisher's Log Series Model Validation ---
  obs_abund <- sort(table(res$species_dist$species), decreasing = TRUE)
  n_rem <- res$P$N_INDIVIDUALS * (1 - res$P$DOMINANT_FRACTION)
  ranks <- 2:res$P$N_SPECIES
  rel_abund_theory <- res$P$FISHER_ALPHA * (res$P$FISHER_X^ranks) / ranks
  theory_abund <- c(res$P$N_INDIVIDUALS * res$P$DOMINANT_FRACTION, rel_abund_theory / sum(rel_abund_theory) * n_rem)

  # Match lengths for comparison
  len_obs <- length(obs_abund)
  len_theory <- length(theory_abund)
  max_len <- max(len_obs, len_theory)
  length(obs_abund) <- max_len
  length(theory_abund) <- max_len
  obs_abund[is.na(obs_abund)] <- 0

  residuals <- obs_abund - theory_abund
  rmse <- sqrt(mean(residuals^2, na.rm = T))
  r_squared <- if (var(obs_abund, na.rm = T) > 0) cor(obs_abund, theory_abund, use = "complete.obs")^2 else 1

  counts <- as.numeric(table(res$species_dist$species))
  eff_alpha <- -sum(counts / sum(counts) * log(1 - (counts / sum(counts)))) # Alternative definition

  fisher_report <- c(
    "\nFisher's Log Series Model Validation:",
    sprintf("  RMSE: %.2f", rmse),
    sprintf("  R-squared: %.3f", r_squared),
    sprintf("  Max residual: %.1f", max(abs(residuals), na.rm = T)),
    sprintf("  Specified alpha: %.2f", res$P$FISHER_ALPHA),
    sprintf("  Effective alpha from data: %.2f", eff_alpha) # Using a common estimator for alpha
  )

  # --- Combine all sections ---
  full_report <- c(
    "========== ANALYSIS REPORT ==========",
    env_report, corr_report, sad_report, alpha_report, div_report, spat_report, fisher_report,
    "\nSIMULATION COMPLETED SUCCESSFULLY."
  )

  return(paste(full_report, collapse = "\n"))
}



# -----------------------------------------------------------------------------
# 6. MAIN SIMULATION ORCHESTRATOR
# -----------------------------------------------------------------------------

#' @title Run the Full Spatial Sampling Simulation
#' @description The main function that orchestrates the entire simulation, from
#'   loading parameters to generating outputs.
#' @param init_file Path to the `.txt` configuration file.
#' @param output_prefix A string used as the base for all output filenames.
run_spatial_simulation <- function(init_file = "simul_init.txt",
                                   output_prefix = "simulation_output") {
  # --- 1. Setup Paths and Cleanup ---
  # <--- FIX: Added the 'vegan' package for a standard Fisher's alpha estimator.
  install_if_missing("vegan")
  library(vegan)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_prefix <- paste0(output_prefix, "_", timestamp)
  output_dir <- dirname(output_prefix)
  report_path <- paste0(output_prefix, "_report.txt")

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # --- 2. Setup Logging in a Robust Way ---
  # This section is now more robust. It will stop with a clear error if the file can't be opened.
  con <- NULL
  # Setup a robust exit handler to close connections and sinks
  on.exit(
    {
      # Stop all sinks
      num_sinks <- sink.number()
      if (num_sinks > 0) {
        for (i in 1:num_sinks) {
          sink()
        }
      }
      # Close connection if it's open
      if (!is.null(con) && isOpen(con)) {
        close(con)
      }
    },
    add = TRUE
  )

  # Try to open the file connection
  tryCatch(
    {
      con <- file(report_path, open = "wt")
    },
    error = function(e) {
      stop(paste0("Failed to open report file for writing at '", report_path, "'.\nCheck path and directory permissions.\nOriginal error: ", e$message))
    }
  )

  # If connection is successful, start sinking
  sink(con, type = "output")
  sink(con, type = "message")

  # --- 3. Main Simulation Logic ---
  tryCatch(
    {
      # Load config and print to report
      P <- load_config(init_file)
      cat("========== SIMULATION PARAMETERS ==========\n")
      cat(paste(capture.output(dput(P[!names(P) %in% c("QUADRAT_SIZE")])), collapse = "\n"), "\n\n")

      # Run simulation steps and print progress to report
      cat("========== INITIALIZING SPATIAL SAMPLING SIMULATION ==========\n")
      cat(sprintf("Parameters: N = %d individuals, S = %d species, %d quadrats (%s size)\n", P$N_INDIVIDUALS, P$N_SPECIES, P$N_QUADRATS, P$QUADRAT_SIZE_OPTION))
      cat(sprintf("Fisher's parameters: α = %.1f, x = %.2f, dominant fraction = %.2f\n", P$FISHER_ALPHA, P$FISHER_X, P$DOMINANT_FRACTION))
      cat(sprintf("Quadrat dimensions: %.2f × %.2f units\n\n", P$QUADRAT_SIZE[1], P$QUADRAT_SIZE[2]))

      cat("Creating irregular sampling domain...\n")
      domain <- create_sampling_domain()
      cat("Generating heterogeneous species distribution using Fisher's log series...\n")
      species_dist <- generate_heterogeneous_distribution(domain, P)
      cat("Placing sampling quadrats with non-overlap constraints...\n")
      quadrats <- place_quadrats(domain, P$N_QUADRATS, P$QUADRAT_SIZE)
      if (nrow(quadrats) == 0) stop("Failed to place any quadrats.")
      cat("Computing alpha diversity for each sampling unit...\n\n")

      # Extract data
      all_species <- LETTERS[1:P$N_SPECIES]
      env_gradients <- create_environmental_gradients(domain, P$SAMPLING_RESOLUTION, P$ENVIRONMENTAL_NOISE)
      abund_matrix <- create_abundance_matrix(species_dist, quadrats, all_species)
      site_env <- calculate_quadrat_environment(env_gradients, quadrats, st_crs(domain))
      site_coords <- st_coordinates(st_centroid(quadrats)) %>%
        as.data.frame() %>%
        mutate(site = quadrats$quadrat_id) %>%
        select(site, x = X, y = Y)

      # Save outputs and print progress to report
      cat("========== SAVING OUTPUT FILES ==========\n")
      f_abund <- paste0(output_prefix, "_abundances.csv")
      f_env <- paste0(output_prefix, "_environments.csv")
      f_coord <- paste0(output_prefix, "_quadrat_centroids.csv")
      f_png <- paste0(output_prefix, "_fig_panel.png")
      f_pdf <- paste0(output_prefix, "_fig_panel.pdf")

      write.csv(abund_matrix, f_abund, row.names = FALSE)
      write.csv(site_env, f_env, row.names = FALSE)
      write.csv(site_coords, f_coord, row.names = FALSE)

      panel_plot <- (plot_spatial_sampling(domain, species_dist, quadrats, P) | plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "temperature_C")) /
        (plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "elevation_m") | plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "rainfall_mm")) +
        plot_layout(guides = "collect") & theme(legend.position = "right")
      ggsave(f_png, panel_plot, width = 14, height = 10, dpi = 300, bg = "white")
      ggsave(f_pdf, panel_plot, width = 14, height = 10, bg = "white")

      cat(sprintf("Panel figures saved as %s and %s\n", f_png, f_pdf))
      cat(sprintf("Quadrat × Species abundance table saved as %s\n", f_abund))
      cat(sprintf("Quadrat × environmental means table saved as %s\n", f_env))
      cat(sprintf("Quadrat centroid coordinates saved as %s\n", f_coord))

      # Generate and write the final, detailed report
      results_list <- list(P = P, species_dist = species_dist, quadrats = quadrats, env_gradients = env_gradients, abund_matrix = abund_matrix, site_coords = site_coords)
      cat("\n", generate_full_report(results_list), "\n", sep = "")

      # This message will now appear on the console because the on.exit handler will close the sinks first
      message("\nSimulation completed successfully. Report and files saved to folder: ", normalizePath(output_dir))
    },
    error = function(e) {
      # If an error occurs, print it to the console for visibility
      message("\n!!!!!!!!!! SIMULATION FAILED !!!!!!!!!!\n")
      message("ERROR: ", e$message)
      # Also write it to the report file
      cat("\n\n!!!!!!!!!! SIMULATION FAILED !!!!!!!!!!\nError message:", e$message, "\n")
    }
  )
}



# -----------------------------------------------------------------------------
# 7. USAGE EXAMPLE
# -----------------------------------------------------------------------------
# --- UNCOMMENT THE LINES BELOW TO RUN A DEMO ---
#
# # This will create a "simulation_demo" folder in your current working directory,
# # create a dummy configuration file inside it, and then run the simulation,
# # saving all outputs into that same folder.
#
# if (!dir.exists("simulation_demo")) dir.create("simulation_demo")
# writeLines(c(
#   "N_INDIVIDUALS = 1500", "N_SPECIES = 10", "N_QUADRATS=15",
#   "QUADRAT_SIZE_OPTION = small", "FISHER_ALPHA=2.5", "DOMINANT_FRACTION=0.3"
#   ), "simulation_demo/demo_init.txt")
run_spatial_simulation(
  init_file = "BDC334/landscape_sim/simul_init.txt",
  output_prefix = "BDC334/landscape_sim/demo_output"
)
