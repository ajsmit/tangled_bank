# -----------------------------------------------------------------------------
#
# SPATIAL SAMPLING SIMULATION
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

# Helper to install packages if missing
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

# Load required libraries
required_packages <- c(
  "sf", "ggplot2", "dplyr", "tidyr", "viridis",
  "stringr", "patchwork", "RANN", "lwgeom", "vegan",
  "rayshader", "rgl"
)
install_if_missing(required_packages)

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(vegan)
  library(viridis)
  library(stringr)
  library(patchwork)
  library(RANN)
  library(lwgeom)
  library(rayshader)
  library(rgl)
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
#'    default values. User-defined values override defaults. It now dynamically
#'    generates the interaction matrix based on the final N_SPECIES.
#' @param init_file Path to the user's configuration file.
#' @return A list containing all configuration parameters for the simulation.
load_config <- function(init_file) {
  user_params <- parse_init_file(init_file)

  # Define default parameters
  defaults <- list(
    SEED = 77,
    N_INDIVIDUALS = 2000,
    N_SPECIES = 15,
    DOMINANT_FRACTION = 0.35,
    FISHER_ALPHA = 3.0,
    FISHER_X = 0.95,
    QUADRAT_SIZE_OPTION = "small",
    SAMPLING_RESOLUTION = 50,
    GRADIENT_SPECIES = c("D", "E", "F", "G", "I", "K"),
    GRADIENT_ASSIGNMENTS = c("temperature", "temperature", "elevation", "temperature", "temperature", "temperature"),
    GRADIENT_OPTIMA = c(0.3, 0.7, 0.5),
    GRADIENT_TOLERANCE = 0.1,
    ENVIRONMENTAL_NOISE = 0.05,
    CLUSTER_SPREAD_DOMINANT = 3,
    CLUSTER_SPREAD_RARE = 6,
    MAX_CLUSTERS_DOMINANT = 5,
    POINT_SIZE = 0.2,
    POINT_ALPHA = 1.0,
    QUADRAT_ALPHA = 0.05,
    BACKGROUND_COLOUR = "white",
    FOREGROUND_COLOUR = "#22223b",
    QUADRAT_COLOUR = "black",
    # Sampling Scheme Parameters
    VORONOI_SEED_FACTOR = 10,
    SAMPLING_SCHEME = "transect", ## "tiled", "random", "systematic", "transect", "voronoi"
    N_QUADRATS = 15,
    N_TRANSECTS = 1,
    N_QUADRATS_PER_TRANSECT = 8,
    TRANSECT_ANGLE = 90,
    # Defines competition (<1) and facilitation (>1)
    INTERACTION_RADIUS = 2.5,
    # The INTERACTION_MATRIX will be generated dynamically below
    INTERACTION_MATRIX = NULL,
    ADVANCED_ANALYSIS = TRUE
  )

  # Merge user params with defaults
  config <- defaults
  for (name in names(user_params)) {
    config[[name]] <- user_params[[name]]
  }

  # --- DYNAMIC PARAMETER GENERATION ---
  # Generate the interaction matrix dynamically based on the final N_SPECIES
  # This ensures it always has the correct dimensions.
  if (is.null(config$INTERACTION_MATRIX)) {
    spp_names <- LETTERS[1:config$N_SPECIES]
    mat <- matrix(1.0,
      nrow = config$N_SPECIES, ncol = config$N_SPECIES,
      dimnames = list(spp_names, spp_names)
    )

    # Example Rule 1: Species A is a strong competitor against B and C
    # This check ensures the rule is only applied if these species exist
    # if (all(c("A", "B", "C") %in% spp_names)) {
    #   mat["A", c("B", "C")] <- 0.2
    #   mat[c("B", "C"), "A"] <- 0.2
    # }

    # Add other default interaction rules here if needed, with similar checks
    # Example 2: Species D facilitates E
    if (all(c("D", "E") %in% spp_names)) {
      mat["E", "D"] <- 3.0
    }
    config$INTERACTION_MATRIX <- mat
  }

  # Derived parameters for quadrat size
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

#' @title Assign Individuals by Clustering (Efficiently)
#' @description Helper function to assign individuals to locations based on proximity
#'   to random cluster centers. Uses a fast nearest-neighbour search (k-d tree)
#'   which is significantly faster than calculating a full distance matrix.
#' @param points_with_env The full sf data frame of all potential point locations.
#' @param available_indices Numeric vector of indices in `points_with_env` that are available for assignment.
#' @param n_ind_to_assign The number of individuals of the current species to assign.
#' @param n_clusters The number of clusters to form.
#' @param spread The spread parameter (sigma) for the exponential decay function.
#' @return A numeric vector of indices selected from `available_indices`.
assign_by_clustering_fast <- function(points_with_env, available_indices, n_ind_to_assign, n_clusters, spread) {
  # 1. Select cluster centers from available locations
  # Ensure we don't try to pick more cluster centers than available points
  n_clusters <- min(n_clusters, length(available_indices))
  if (n_clusters == 0) {
    return(integer(0))
  } # Return empty if no clusters can be formed
  cluster_centers_idx <- sample(available_indices, n_clusters)

  # 2. Extract coordinates for fast nearest-neighbour search
  # Data points are the cluster centers
  center_coords <- st_coordinates(points_with_env[cluster_centers_idx, ])
  # Query points are all available locations
  available_coords <- st_coordinates(points_with_env[available_indices, ])

  # 3. Perform fast nearest-neighbour search using RANN::nn2
  # nn2 returns a list with distances (nn.dists) and indices (nn.idx)
  # We only need the distance to the single nearest neighbour (k=1)
  nn_results <- RANN::nn2(data = center_coords, query = available_coords, k = 1)

  # The result is a matrix, we extract the distance column as a vector
  min_dists <- nn_results$nn.dists[, 1]

  # 4. Calculate sampling probabilities based on distance
  # Points closer to a cluster center have a higher probability of being selected
  probs <- exp(-min_dists / spread)

  # Handle cases where all probabilities might be zero (e.g., very large distances and small spread)
  if (all(probs == 0)) {
    probs <- rep(1, length(available_indices))
  }

  # 5. Sample from the available indices based on calculated probabilities
  selected_indices <- sample(available_indices, size = n_ind_to_assign, prob = probs)

  return(selected_indices)
}


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
  grid$temperature_C <- grid$temperature * 30 - 2 # Range: -2 to 28 C
  grid$elevation_m <- grid$elevation * 2000 # Range: 0 to 2000 m
  grid$rainfall_mm <- grid$rainfall * 700 + 200 # Range: 200 to 700 mm

  return(grid)
}


#' @title Generate Heterogeneous Species Distribution with Interspecific Interactions
#' @description Places individuals based on abundance, clustering, environmental
#'    gradients, and interspecific interactions (competition/facilitation).
#' @param domain The `sf` polygon of the study area.
#' @param P The configuration list, including INTERACTION_MATRIX and INTERACTION_RADIUS.
#' @return An `sf` point object with each point assigned a species.
generate_heterogeneous_distribution <- function(domain, P) {
  # --- Initial Setup (same as before) ---
  abundances <- generate_fisher_log_series(P$N_SPECIES, P$N_INDIVIDUALS, P$DOMINANT_FRACTION, P$FISHER_ALPHA, P$FISHER_X)
  env_grid <- create_environmental_gradients(domain, P$SAMPLING_RESOLUTION, P$ENVIRONMENTAL_NOISE)
  env_sf <- st_as_sf(env_grid, coords = c("x", "y"), crs = st_crs(domain))
  all_points <- st_sample(domain, size = P$N_INDIVIDUALS, type = "random")
  points_with_env <- st_join(st_sf(geometry = all_points), env_sf, join = st_nearest_feature)
  points_with_env$species <- ""
  available_indices <- 1:nrow(points_with_env)

  # Main Loop to Place Species Sequentially
  for (sp in names(abundances)) {
    n_ind <- abundances[sp]
    if (length(available_indices) < n_ind) n_ind <- length(available_indices)
    if (n_ind == 0) next

    # 1. Calculate Base Probability (from environment or clustering)
    base_probs <- rep(1.0, length(available_indices)) # Default uniform probability

    if (sp %in% P$GRADIENT_SPECIES) {
      sp_idx <- which(P$GRADIENT_SPECIES == sp)
      gradient_type <- P$GRADIENT_ASSIGNMENTS[sp_idx]
      optimum <- P$GRADIENT_OPTIMA[sp_idx]
      env_values <- points_with_env[[gradient_type]][available_indices]
      base_probs <- exp(-((env_values - optimum)^2) / (2 * P$GRADIENT_TOLERANCE^2))
    } else if (sp == "A") { # Conspecific clustering for dominant species
      # This logic is simplified for clarity; it could also use the fast clustering function
    }

    # 2. Calculate Inter-specific Interaction Modifier
    interaction_modifier <- rep(1.0, length(available_indices))
    assigned_indices <- which(points_with_env$species != "")

    if (length(assigned_indices) > 0) {
      k_neighbours <- 5
      available_coords <- st_coordinates(points_with_env[available_indices, ])
      assigned_coords <- st_coordinates(points_with_env[assigned_indices, ])

      # Find neighbours for each available point from the set of assigned points
      nn <- RANN::nn2(
        data = assigned_coords, query = available_coords, k = k_neighbours,
        searchtype = "radius", radius = P$INTERACTION_RADIUS
      )

      interaction_scores <- apply(nn$nn.idx, 1, function(neighbour_row) {
        valid_neighbours_in_assigned_set <- neighbour_row[neighbour_row > 0]
        if (length(valid_neighbours_in_assigned_set) == 0) {
          return(1.0) # No neighbours, no interaction effect
        }

        neighbour_species <- points_with_env$species[assigned_indices[valid_neighbours_in_assigned_set]]
        interaction_values <- P$INTERACTION_MATRIX[sp, neighbour_species]

        # --- ECOLOGICAL NOTE ON COMBINING INTERACTIONS ---
        # We combine the effects of all neighbours using the geometric mean,
        # which is `exp(mean(log(values)))`. This is appropriate for
        # multiplicative effects like competition (<1) and facilitation (>1).
        # It ensures that a single strong competitor (e.g., a value close
        # to 0) can override strong facilitation, which is more realistic
        # than a simple arithmetic mean.
        exp(mean(log(interaction_values)))
      })

      interaction_modifier <- interaction_scores
    }

    # 3. Calculate Final Probability and Assign Individuals
    final_probs <- base_probs * interaction_modifier
    if (all(is.na(final_probs)) || all(final_probs == 0)) {
      final_probs <- rep(1, length(available_indices))
    }

    selected <- sample(available_indices, size = n_ind, prob = final_probs, replace = FALSE)

    if (length(selected) > 0) {
      points_with_env$species[selected] <- sp
      available_indices <- setdiff(available_indices, selected)
    }
  }

  return(points_with_env %>% filter(species != ""))
}


#' @title Create a Quadrat Polygon from its Center
#' @description Helper function to generate an axis-aligned rectangular polygon
#'   from a center point and dimensions.
#' @param center_point An `sf` point geometry object.
#' @param size A numeric vector `c(width, height)`.
#' @return An `st_polygon` geometry object.
create_quadrat_from_center <- function(center_point, size) {
  half_w <- size[1] / 2
  half_h <- size[2] / 2
  # st_coordinates returns a matrix, so we get the first row
  coords <- st_coordinates(center_point)[1, ]
  st_polygon(list(cbind(
    c(coords["X"] - half_w, coords["X"] + half_w, coords["X"] + half_w, coords["X"] - half_w, coords["X"] - half_w),
    c(coords["Y"] - half_h, coords["Y"] - half_h, coords["Y"] + half_h, coords["Y"] + half_h, coords["Y"] - half_h)
  )))
}


#' @title Place Non-Overlapping Quadrats
#' @description Randomly places rectangular quadrats within the domain, ensuring they
#'    are fully contained and do not overlap.
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

    # Use the new helper function to create the polygon
    center_pt_sfc <- st_sfc(st_point(c(x_center, y_center)), crs = st_crs(domain))
    new_quadrat_poly <- create_quadrat_from_center(center_pt_sfc, quadrat_size) %>%
      st_sfc(crs = st_crs(domain))

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


#' @title Place Non-Overlapping Quadrats (Systematic Tiling Method)
#' @description Places rectangular quadrats within the domain using an efficient
#'   tiling method. It creates a grid of all possible non-overlapping quadrat
#'   locations and randomly samples from those that are fully contained
#'   within the domain. This is far more reliable and efficient than the
#'   original rejection sampling method.
#' @param domain The `sf` polygon of the study area.
#' @param n_quadrats The target number of quadrats.
#' @param quadrat_size A numeric vector `c(width, height)` for the quadrats.
#' @return An `sf` object containing the quadrat polygons.
place_quadrats_tiled <- function(domain, n_quadrats, quadrat_size) {
  # 1. Create a grid of potential non-overlapping quadrats (cells)
  # The cell size is equal to the quadrat size to ensure no overlap.
  candidate_grid <- st_make_grid(domain, cellsize = quadrat_size, what = "polygons")

  # 2. Filter the grid to find all candidates fully within the domain
  # st_within is the key predicate here.
  valid_locations <- candidate_grid[st_within(candidate_grid, domain, sparse = FALSE)]

  num_possible <- length(valid_locations)

  # 3. Check if enough valid locations were found
  if (num_possible == 0) {
    warning("Systematic placement failed: No quadrats of the given size can fit entirely within the domain.")
    return(st_sf(quadrat_id = integer(0), geometry = st_sfc(crs = st_crs(domain))))
  }

  if (num_possible < n_quadrats) {
    warning(sprintf(
      "Could only place %d of %d requested quadrats, as this is the maximum that can fit.",
      num_possible, n_quadrats
    ))
    n_quadrats <- num_possible
  }

  # 4. Randomly sample N quadrats from the list of all possible valid locations
  sampled_indices <- sample(1:num_possible, size = n_quadrats)
  final_quadrats_sfc <- valid_locations[sampled_indices]

  # 5. Finalize the sf object with IDs
  final_quadrats <- st_sf(quadrat_id = 1:n_quadrats, geometry = final_quadrats_sfc)

  return(final_quadrats)
}


#' @title Place Quadrats Using Voronoi Tessellation
#' @description Places quadrats by generating a Voronoi diagram from random seed
#'    points. It identifies cells large enough to contain a quadrat and places
#'    the quadrat at the cell's "pole of inaccessibility" (center of the
#'    largest inscribed circle), ensuring quadrats are well-spaced.
#' @param domain The `sf` polygon of the study area.
#' @param n_quadrats The target number of quadrats.
#' @param quadrat_size A numeric vector `c(width, height)`.
#' @return An `sf` object containing the quadrat polygons.
place_quadrats_voronoi <- function(domain, n_quadrats, quadrat_size, voronoi_seed_factor) {
  # 1. Generate more seed points than needed to ensure good coverage and choice.
  n_seeds <- n_quadrats * voronoi_seed_factor

  # Ensure the domain is a single polygon for st_voronoi to work correctly
  domain_union <- st_union(domain)
  seed_points <- st_sample(domain_union, size = n_seeds, type = "random")

  # 2. Create the Voronoi tessellation and clip it to the domain boundary.
  voronoi_polys <- st_voronoi(st_union(seed_points))
  voronoi_clipped <- st_intersection(st_cast(voronoi_polys), domain_union)

  # 3. For each Voronoi cell, find the largest possible circle that fits inside.
  # We suppress warnings as it can be noisy on complex geometries.
  inscribed_circles <- suppressWarnings(st_inscribed_circle(voronoi_clipped))

  # 4. Determine which cells are large enough to hold our quadrat.
  quadrat_half_diag <- sqrt(quadrat_size[1]^2 + quadrat_size[2]^2) / 2
  radii <- sqrt(st_area(inscribed_circles) / pi)
  suitable_indices <- which(radii >= quadrat_half_diag)

  # 5. Check if we found enough suitable locations.
  num_possible <- length(suitable_indices)
  if (num_possible == 0) {
    warning("Voronoi placement failed: No cells were large enough to contain a quadrat.")
    return(st_sf(quadrat_id = integer(0), geometry = st_sfc(crs = st_crs(domain))))
  }

  if (num_possible < n_quadrats) {
    warning(sprintf(
      "Voronoi placement could only find %d suitable locations out of %d requested.",
      num_possible, n_quadrats
    ))
    n_quadrats <- num_possible
  }

  # 6. Randomly sample from the suitable locations.
  sampled_indices <- sample(suitable_indices, size = n_quadrats)
  final_centers <- st_centroid(inscribed_circles[sampled_indices])

  # 7. Create the final axis-aligned quadrats using the helper function.
  quadrat_list <- lapply(st_geometry(final_centers), function(pt) {
    create_quadrat_from_center(pt, quadrat_size)
  })

  final_quadrats_sfc <- st_sfc(quadrat_list, crs = st_crs(domain))
  final_quadrats <- st_sf(quadrat_id = 1:length(final_quadrats_sfc), geometry = final_quadrats_sfc)

  return(final_quadrats)
}


#' @title Place Quadrats in a Systematic Grid
#' @description Generates a regular grid of points over the domain and places a
#'    quadrat at each point that falls completely within the domain.
#' @param domain The `sf` polygon of the study area.
#' @param n_quadrats The target number of quadrats (approximate).
#' @param quadrat_size A numeric vector `c(width, height)`.
#' @return An `sf` object containing the quadrat polygons.
place_quadrats_systematic <- function(domain, n_quadrats, quadrat_size) {
  bbox <- st_bbox(domain)

  # Determine grid spacing to approximate the desired number of quadrats
  aspect_ratio <- (bbox["ymax"] - bbox["ymin"]) / (bbox["xmax"] - bbox["xmin"])
  nx <- round(sqrt(n_quadrats / aspect_ratio))
  ny <- round(aspect_ratio * nx)

  # Create a grid of candidate center points
  candidate_centers <- st_make_grid(domain, n = c(nx, ny), what = "centers")

  # Create quadrat polygons around each candidate center using the helper function
  candidate_quadrats <- st_sfc(lapply(candidate_centers, function(pt) {
    create_quadrat_from_center(pt, quadrat_size)
  }), crs = st_crs(domain))

  # Keep only the quadrats that are fully within the domain
  is_within <- st_within(candidate_quadrats, domain, sparse = FALSE)[, 1]
  valid_quadrats <- candidate_quadrats[is_within]

  if (length(valid_quadrats) == 0) {
    warning("Systematic sampling failed to place any quadrats within the domain.")
    return(st_sf(quadrat_id = integer(0), geometry = st_sfc(crs = st_crs(domain))))
  }

  # Finalize the sf object
  final_quadrats <- st_sf(quadrat_id = 1:length(valid_quadrats), geometry = valid_quadrats)
  return(final_quadrats)
}


#' @title Place Quadrats Along Parallel Transects (First Principles Rewrite)
#' @description This is a complete rewrite using basic geometry and linear
#'    interpolation, avoiding advanced sf functions to bypass a persistent bug.
#' @param domain The `sf` polygon of the study area.
#' @param n_transects The number of parallel transect lines.
#' @param n_quadrats_per_transect The number of quadrats on each transect.
#' @param quadrat_size A numeric vector `c(width, height)`.
#' @param angle The compass direction of the transects in degrees (0=N, 90=E).
#' @return An `sf` object containing the quadrat polygons.
place_quadrats_transect <- function(domain, n_transects, n_quadrats_per_transect, quadrat_size, angle) {
  # 1. Create a "safe sampling area".
  buffer_dist <- sqrt(quadrat_size[1]^2 + quadrat_size[2]^2) / 2
  safe_domain <- st_buffer(domain, -buffer_dist)

  if (st_is_empty(safe_domain) || st_area(safe_domain) == 0) {
    warning("Domain is too small to create a safe sampling area for the given quadrat size.")
    return(st_sf(quadrat_id = integer(0), geometry = st_sfc(crs = st_crs(domain))))
  }

  # 2. Define the ideal mathematical transect lines.
  bbox <- st_bbox(domain)
  center <- st_centroid(st_as_sfc(bbox))
  diag_len <- sqrt((bbox["xmax"] - bbox["xmin"])^2 + (bbox["ymax"] - bbox["ymin"]^2)) * 1.5
  y_coords <- seq(bbox["ymin"], bbox["ymax"], length.out = n_transects + 2)[2:(n_transects + 1)]

  # 3. Rotate these lines to the desired angle.
  math_angle_deg <- 90 - angle
  math_angle_rad <- math_angle_deg * pi / 180
  rot_matrix <- matrix(c(cos(math_angle_rad), sin(math_angle_rad), -sin(math_angle_rad), cos(math_angle_rad)), 2, 2)

  horizontal_lines <- st_sfc(lapply(y_coords, function(y) {
    st_linestring(matrix(c(st_coordinates(center)[1] - diag_len / 2, st_coordinates(center)[1] + diag_len / 2, y, y), ncol = 2))
  }), crs = st_crs(domain))
  rotated_lines <- (horizontal_lines - center) * rot_matrix + center
  st_crs(rotated_lines) <- st_crs(domain)

  points_list <- list()
  for (i in seq_len(n_transects)) {
    line <- rotated_lines[i]
    segment <- st_intersection(line, safe_domain)

    if (st_is_empty(segment) || st_length(segment) == 0) next

    # Manually get the start and end coordinates of the valid segment.
    segment_coords <- st_coordinates(segment)
    start_pt <- segment_coords[1, c("X", "Y")]
    end_pt <- segment_coords[nrow(segment_coords), c("X", "Y")]

    # Manually calculate N points using linear interpolation.
    if (n_quadrats_per_transect > 1) {
      fractions <- seq(0, 1, length.out = n_quadrats_per_transect)
    } else {
      fractions <- 0.5 # Place a single point in the middle
    }

    x_coords <- start_pt[1] + fractions * (end_pt[1] - start_pt[1])
    y_coords <- start_pt[2] + fractions * (end_pt[2] - start_pt[2])

    points_list[[i]] <- st_as_sf(data.frame(x = x_coords, y = y_coords), coords = c("x", "y"), crs = st_crs(domain))
  }

  if (length(points_list) == 0) {
    warning("No transects intersected the safe sampling area.")
    return(st_sf(quadrat_id = integer(0), geometry = st_sfc(crs = st_crs(domain))))
  }

  candidate_centers <- do.call(rbind, points_list)

  # Create axis-aligned quadrats around the calculated centers using the helper.
  quadrat_geometries <- st_sfc(lapply(st_geometry(candidate_centers), function(pt) {
    create_quadrat_from_center(pt, quadrat_size)
  }), crs = st_crs(domain))

  final_quadrats <- st_sf(quadrat_id = 1:length(quadrat_geometries), geometry = quadrat_geometries)

  return(final_quadrats)
}


# -----------------------------------------------------------------------------
# 4. ADVANCED ANALYSES
# -----------------------------------------------------------------------------

#' @title Calculate Rank-Abundance Data (Base R Method)
#' @description Computes both the observed rank-abundance distribution from the
#'    simulation results and the theoretical distribution from Fisher's log-series
#'    parameters. This version uses base R's `table()` to avoid dplyr conflicts.
#' @param species_dist An `sf` object of all individuals with a `species` column.
#' @param P The configuration list for the simulation.
#' @return A single data frame containing ranked abundance data for both sources.
calculate_rank_abundance <- function(species_dist, P) {
  # 1. Calculate OBSERVED data using base R's table() to avoid n() error
  observed_counts <- table(species_dist$species)
  observed_data <- as.data.frame(observed_counts, stringsAsFactors = FALSE)
  names(observed_data) <- c("Species", "Abundance")

  # We can still use dplyr for safe operations like arranging and ranking
  observed_data <- observed_data %>%
    arrange(desc(Abundance)) %>%
    mutate(
      Rank = row_number(),
      Source = "Observed"
    ) %>%
    select(Rank, Abundance, Source)

  # 2. Calculate THEORETICAL data (this part was already safe)
  theoretical_abundances <- generate_fisher_log_series(
    P$N_SPECIES, P$N_INDIVIDUALS, P$DOMINANT_FRACTION, P$FISHER_ALPHA, P$FISHER_X
  )

  theoretical_data <- tibble(
    Abundance = sort(as.numeric(theoretical_abundances), decreasing = TRUE),
    Rank = seq_along(theoretical_abundances), # or 1:length(theoretical_abundances)
    Source = "Theoretical"
  )

  # 3. Combine and return
  bind_rows(observed_data, theoretical_data)
}


#' @title Plot Rank-Abundance Curve
#' @description Generates a ggplot object showing the rank-abundance curve(s).
#' @param rank_abundance_data A data frame produced by `calculate_rank_abundance`.
#' @return A `ggplot` object.
plot_rank_abundance <- function(rank_abundance_data) {
  ggplot(rank_abundance_data, aes(x = Rank, y = Abundance, color = Source)) +
    geom_line(aes(linetype = Source), linewidth = 1.1) +
    geom_point(aes(shape = Source), size = 3, fill = "white", stroke = 1.2) +
    # This is the original fix for the labels argument
    scale_y_log10(
      labels = function(x) parse(text = paste("10^", log10(x), sep = ""))
    ) +
    scale_color_manual(values = c("Observed" = "black", "Theoretical" = "#e41a1c")) +
    scale_linetype_manual(values = c("Observed" = "solid", "Theoretical" = "dashed")) +
    scale_shape_manual(values = c("Observed" = 21, "Theoretical" = 22)) +
    labs(
      title = "Species-Abundance Distribution (SAD)",
      subtitle = "Comparison of observed data vs. theoretical Fisher's log-series model",
      x = "Species Rank (Most to Least Abundant)",
      y = "Abundance (Log Scale)",
      color = "Distribution",
      linetype = "Distribution",
      shape = "Distribution"
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}


#' @title Calculate Occupancy-Abundance Data
#' @description Calculates the total abundance and occupancy (number of sites)
#'   for each species from a site-by-species abundance matrix.
#' @param abund_matrix A data frame with sites in rows and species in columns,
#'   containing abundance counts. The first column should be named "site".
#' @return A data frame with columns for Species, TotalAbundance, and Occupancy.
calculate_occupancy_abundance <- function(abund_matrix) {
  # Exclude the 'site' column for calculations using base R indexing
  abund_numeric <- abund_matrix[, -which(names(abund_matrix) == "site")]

  # Calculate total abundance and occupancy for each species (column)
  oa_data <- data.frame(
    Species = names(abund_numeric),
    TotalAbundance = colSums(abund_numeric),
    Occupancy = colSums(abund_numeric > 0),
    row.names = NULL # Ensure row names are not carried over
  )

  return(oa_data)
}

#' @title Plot Occupancy-Abundance Relationship
#' @description Generates a ggplot object showing the relationship between
#'   species abundance and occupancy, typically on a log-log scale.
#' @param oa_data A data frame produced by `calculate_occupancy_abundance`.
#' @return A `ggplot` object.
plot_occupancy_abundance <- function(oa_data) {
  # Add a check for empty data
  if (nrow(oa_data) == 0 || all(oa_data$TotalAbundance == 0)) {
    return(
      ggplot() +
        labs(
          title = "Occupancy-Abundance Relationship",
          subtitle = "No data to plot."
        ) +
        theme_void()
    )
  }

  ggplot(oa_data, aes(x = TotalAbundance, y = Occupancy)) +
    geom_point(alpha = 0.6, size = 3, color = "#2c7fb8") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", linewidth = 0.8) +
    scale_x_log10(labels = scales::label_log()) +
    scale_y_log10(labels = scales::label_log()) +
    labs(
      title = "Occupancy-Abundance Relationship",
      subtitle = "Widespread species tend to be more abundant",
      x = "Total Abundance (Log Scale)",
      y = "Number of Sites Occupied (Log Scale)"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold")
    )
}


#' @title Calculate Species-Area Relationship Data
#' @description Computes a species accumulation curve using the 'random' method
#'   from `vegan::specaccum`.
#' @param abund_matrix A site-by-species data frame of species abundances.
#'   The first column should be named "site".
#' @return A data frame with columns for Sites, Richness (mean), and SD (standard deviation).
calculate_species_area <- function(abund_matrix) {
  # Exclude the 'site' column for calculations
  abund_numeric <- abund_matrix[, -which(names(abund_matrix) == "site")]

  # Calculate the species accumulation curve using 100 random permutations
  sar_curve <- vegan::specaccum(abund_numeric, method = "random", permutations = 100)

  # Extract the results into a clean data frame for plotting
  sar_data <- data.frame(
    Sites = sar_curve$sites,
    Richness = sar_curve$richness,
    SD = sar_curve$sd
  )

  return(sar_data)
}


#' @title Plot Species-Area Relationship
#' @description Generates a ggplot object showing the species accumulation curve.
#' @param sar_data A data frame produced by `calculate_species_area`.
#' @return A `ggplot` object.
plot_species_area <- function(sar_data) {
  ggplot(sar_data, aes(x = Sites, y = Richness)) +
    # Shaded region represents the standard deviation
    geom_ribbon(aes(ymin = Richness - SD, ymax = Richness + SD), fill = "#41b6c4", alpha = 0.4) +
    geom_line(color = "#08519c", linewidth = 1.2) +
    labs(
      title = "Species-Area Relationship (SAR)",
      subtitle = "Cumulative species richness as sampling area increases",
      x = "Number of Quadrats (Area)",
      y = "Cumulative Number of Species"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold")
    )
}


#' @title Calculate Distance-Decay Data
#' @description Computes pairwise geographic distances and community dissimilarities
#'   between all sites.
#' @param abund_matrix A site-by-species data frame of species abundances.
#' @param site_coords A data frame with site coordinates (must contain 'x' and 'y' columns).
#' @return A data frame with 'Distance' and 'Dissimilarity' columns for plotting.
calculate_distance_decay <- function(abund_matrix, site_coords) {
  # Get numeric coordinates and abundance matrices
  coords <- site_coords[, c("x", "y")]
  abund_numeric <- abund_matrix[, -which(names(abund_matrix) == "site")]

  # Calculate Euclidean geographic distance matrix
  geo_dist <- dist(coords, method = "euclidean")

  # Calculate Sørensen dissimilarity matrix
  # This is equivalent to Bray-Curtis on presence-absence data
  comm_dissim <- vegan::vegdist(abund_numeric, method = "bray", binary = TRUE)

  # Combine the two distance vectors into a single data frame
  decay_data <- data.frame(
    Distance = as.vector(geo_dist),
    Dissimilarity = as.vector(comm_dissim)
  )

  return(decay_data)
}


#' @title Plot Distance-Decay Relationship
#' @description Generates a scatter plot of community dissimilarity versus
#'   geographic distance.
#' @param decay_data A data frame produced by `calculate_distance_decay`.
#' @return A `ggplot` object.
plot_distance_decay <- function(decay_data) {
  ggplot(decay_data, aes(x = Distance, y = Dissimilarity)) +
    # Use semi-transparent points to handle overplotting
    geom_point(alpha = 0.3, shape = 16, color = "#253494") +
    geom_smooth(method = "loess", color = "#e31a1c", se = TRUE, linewidth = 1.1) +
    # Ensure y-axis is scaled from 0 to 1
    ylim(0, 1) +
    labs(
      title = "Distance-Decay of Community Similarity",
      subtitle = "Community similarity decreases as geographic distance increases",
      x = "Geographic Distance Between Quadrats",
      y = "Community Dissimilarity (Sørensen Index)"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold")
    )
}


#' @title Calculate Rarefaction Curves
#' @description Computes species rarefaction curves for each site (row) in an
#'   abundance matrix.
#' @param abund_matrix A site-by-species data frame of species abundances.
#' @return A tidy data frame with columns for SiteID, SampleSize, and
#'   RarefiedRichness, suitable for plotting with ggplot2.
calculate_rarefaction <- function(abund_matrix) {
  # Exclude the 'site' column for calculations
  abund_numeric <- abund_matrix[, -which(names(abund_matrix) == "site")]
  site_ids <- abund_matrix$site

  # vegan::rarecurve calculates the expected richness for each step in sample size
  # It returns a list, with one element per site
  rarefaction_list <- vegan::rarecurve(abund_numeric, step = 1)

  # Process the list into a single, tidy data frame
  output_list <- list()
  for (i in seq_along(rarefaction_list)) {
    # The vector of rarefied richness values for the current site
    richness_values <- rarefaction_list[[i]]
    # The corresponding sample sizes are stored in the 'Subsample' attribute
    sample_sizes <- attr(richness_values, "Subsample")

    output_list[[i]] <- data.frame(
      SiteID = as.factor(site_ids[i]),
      SampleSize = sample_sizes,
      RarefiedRichness = richness_values
    )
  }

  # Combine the list of data frames into one
  rarefaction_data <- do.call(rbind, output_list)

  return(rarefaction_data)
}


#' @title Plot Rarefaction Curves
#' @description Generates a ggplot object showing rarefaction curves for all sites.
#' @param rarefaction_data A data frame produced by `calculate_rarefaction`.
#' @return A `ggplot` object.
plot_rarefaction <- function(rarefaction_data) {
  # Calculate the max richness to set a consistent color scale
  max_richness <- max(rarefaction_data$RarefiedRichness, na.rm = TRUE)

  ggplot(rarefaction_data, aes(x = SampleSize, y = RarefiedRichness, group = SiteID, color = SiteID)) +
    geom_line(linewidth = 0.8) +
    scale_color_viridis_d(option = "plasma") +
    labs(
      title = "Rarefaction Curves for Each Site",
      subtitle = "Comparing species richness at equivalent sample sizes",
      x = "Number of Individuals Sampled (Sample Size)",
      y = "Expected Number of Species (Rarefied Richness)",
      color = "Quadrat ID"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}


#' @title Plot 3D Elevation Map (Robust, Fixed Method)
#' @description Creates a 3D visualization with proper matrix formatting and error handling.
#' @param env_gradients The data frame of gridded environmental data (x, y, elevation_m).
#' @param filename The path to save the output PNG file.
#' @return NULL. The function saves the plot directly.
plot_3d_elevation_map <- function(env_gradients, filename) {
  # Check if required packages are available
  if (!requireNamespace("rayshader", quietly = TRUE) || !requireNamespace("rgl", quietly = TRUE)) {
    warning("Required packages 'rayshader' and/or 'rgl' not available. Skipping 3D plot.")
    return(invisible(NULL))
  }

  tryCatch({
    # 1. Create properly formatted matrix with explicit dimensions
    x_vals <- sort(unique(env_gradients$x))
    y_vals <- sort(unique(env_gradients$y))

    cat("Creating elevation matrix with dimensions:", length(y_vals), "x", length(x_vals), "\n")

    # Initialize matrix with proper row/column names
    elevation_matrix <- matrix(NA,
      nrow = length(y_vals),
      ncol = length(x_vals),
      dimnames = list(as.character(y_vals), as.character(x_vals))
    )

    # Fill matrix using vectorized approach for better performance
    for (i in 1:nrow(env_gradients)) {
      x_idx <- match(env_gradients$x[i], x_vals)
      y_idx <- match(env_gradients$y[i], y_vals)
      if (!is.na(x_idx) && !is.na(y_idx)) {
        elevation_matrix[y_idx, x_idx] <- env_gradients$elevation_m[i]
      }
    }

    # Handle missing values more robustly
    if (any(is.na(elevation_matrix))) {
      n_missing <- sum(is.na(elevation_matrix))
      cat("Matrix contains", n_missing, "NA values, filling with interpolated values...\n")

      # Use simple interpolation with neighbor averaging
      for (i in 1:nrow(elevation_matrix)) {
        for (j in 1:ncol(elevation_matrix)) {
          if (is.na(elevation_matrix[i, j])) {
            # Get neighboring values
            neighbors <- c()
            if (i > 1) neighbors <- c(neighbors, elevation_matrix[i - 1, j])
            if (i < nrow(elevation_matrix)) neighbors <- c(neighbors, elevation_matrix[i + 1, j])
            if (j > 1) neighbors <- c(neighbors, elevation_matrix[i, j - 1])
            if (j < ncol(elevation_matrix)) neighbors <- c(neighbors, elevation_matrix[i, j + 1])

            neighbors <- neighbors[!is.na(neighbors)]
            if (length(neighbors) > 0) {
              elevation_matrix[i, j] <- mean(neighbors)
            } else {
              elevation_matrix[i, j] <- mean(elevation_matrix, na.rm = TRUE)
            }
          }
        }
      }
    }

    # Ensure matrix is numeric and has reasonable values
    elevation_matrix <- matrix(as.numeric(elevation_matrix),
      nrow = nrow(elevation_matrix),
      ncol = ncol(elevation_matrix)
    )

    # Smooth the elevation matrix for more natural-looking terrain
    cat("Smoothing elevation matrix for realistic topography...\n")
    if (requireNamespace("fields", quietly = TRUE)) {
      # Use image.smooth from fields package if available
      elevation_matrix <- fields::image.smooth(elevation_matrix, theta = 0.5)$z
    } else {
      # Simple smoothing using a moving average filter
      smooth_matrix <- elevation_matrix
      for (i in 2:(nrow(elevation_matrix) - 1)) {
        for (j in 2:(ncol(elevation_matrix) - 1)) {
          # Average with 8 neighbors
          neighbors <- c(
            elevation_matrix[i - 1, j - 1], elevation_matrix[i - 1, j], elevation_matrix[i - 1, j + 1],
            elevation_matrix[i, j - 1],   elevation_matrix[i, j],   elevation_matrix[i, j + 1],
            elevation_matrix[i + 1, j - 1], elevation_matrix[i + 1, j], elevation_matrix[i + 1, j + 1]
          )
          smooth_matrix[i, j] <- mean(neighbors, na.rm = TRUE)
        }
      }
      elevation_matrix <- smooth_matrix
    }

    # Check matrix properties
    cat("Matrix range:", range(elevation_matrix, na.rm = TRUE), "\n")
    cat("Matrix dimensions:", dim(elevation_matrix), "\n")

    # Close any existing rgl devices to avoid conflicts
    if (rgl::rgl.cur() > 0) {
      rgl::close3d()
    }

    # 2. Create the 3D visualization with simplified pipeline
    cat("Generating height shade...\n")
    height_map <- rayshader::height_shade(elevation_matrix)

    cat("Generating ray shade...\n")
    ray_shadow <- rayshader::ray_shade(elevation_matrix,
      zscale = 80,
      sunangle = 315,
      sunaltitude = 45
    )

    cat("Generating ambient shade...\n")
    ambient_shadow <- rayshader::ambient_shade(elevation_matrix)

    cat("Combining layers...\n")
    final_map <- height_map %>%
      rayshader::add_shadow(ray_shadow, max_darken = 0.5) %>%
      rayshader::add_shadow(ambient_shadow, max_darken = 0.3)

    cat("Creating 3D plot...\n")
    rayshader::plot_3d(final_map,
      elevation_matrix,
      zscale = 80,
      fov = 0,
      theta = 125, # Slightly adjusted viewing angle
      phi = 25, # Lower viewing angle for better perspective
      zoom = 0.8, # Slightly closer zoom
      windowsize = c(1200, 800),
      water = FALSE,
      background = "white"
    )

    # 3. Add title with safer positioning (use relative coordinates)
    cat("Adding title...\n")
    tryCatch(
      {
        rayshader::render_label(final_map,
          x = nrow(elevation_matrix) * 0.1, # 10% from left
          y = ncol(elevation_matrix) * 0.9, # 90% from bottom
          z = max(elevation_matrix) * 1.1, # 10% above max height
          zscale = 70,
          text = "3D Elevation Gradient",
          textsize = 2,
          textcolor = "black",
          linecolor = "white",
          relativez = FALSE
        )
      },
      error = function(e) {
        cat("Warning: Could not add title label:", e$message, "\n")
      }
    )

    # 4. Add North arrow
    cat("Adding North arrow...\n")
    tryCatch(
      {
        # Add a compass/north arrow using render_compass
        rayshader::render_compass(elevation_matrix,
          x = nrow(elevation_matrix) * 0.9, # 90% from left (bottom right)
          y = ncol(elevation_matrix) * 0.1, # 10% from bottom
          z = max(elevation_matrix) * 0.3, # Lower on the terrain
          zscale = 70,
          compass_radius = min(nrow(elevation_matrix), ncol(elevation_matrix)) * 0.05,
          position = "NE"
        ) # Position in northeast corner
      },
      error = function(e) {
        cat("Warning: Could not add compass (trying alternative method):", e$message, "\n")

        # Alternative: Add a simple north arrow as text
        tryCatch(
          {
            rayshader::render_label(final_map,
              x = nrow(elevation_matrix) * 0.9,
              y = ncol(elevation_matrix) * 0.1,
              z = max(elevation_matrix) * 0.5,
              zscale = 70,
              text = "N ↑",
              textsize = 1.5,
              textcolor = "black",
              linecolor = "white",
              relativez = FALSE
            )
          },
          error = function(e2) {
            cat("Warning: Could not add north arrow:", e2$message, "\n")
          }
        )
      }
    )

    # 5. Render the final image
    cat("Rendering snapshot to", filename, "...\n")
    rayshader::render_snapshot(filename,
      clear = TRUE,
      instant_capture = TRUE
    )

    cat("3D elevation map successfully created!\n")
  }, error = function(e) {
    cat("Error in 3D elevation mapping:", e$message, "\n")

    # Clean up any open rgl windows
    if (rgl::rgl.cur() > 0) {
      rgl::close3d()
    }

    # Re-throw the error for higher-level handling
    stop("3D elevation mapping failed: ", e$message)
  }, finally = {
    # Always ensure rgl window is closed
    if (rgl::rgl.cur() > 0) {
      rgl::close3d()
    }
  })

  return(invisible(NULL))
}


#' @title Generate Advanced Analysis Panel
#' @description Gathers all 2D advanced analysis plots and arranges them into a
#'   single panel using patchwork.
#' @param res A list containing all simulation artifacts (data frames, stats, etc.).
#' @return A combined `patchwork` ggplot object.
generate_advanced_panel <- function(res) {
  # --- 1. Generate all individual plot objects ---

  # To make the panel cleaner, we'll reduce font sizes and remove some legends
  theme_panel <- theme(text = element_text(size = 11), legend.title = element_text(size = 10), legend.text = element_text(size = 9))

  p_rank <- plot_rank_abundance(calculate_rank_abundance(res$species_dist, res$P)) +
    labs(subtitle = NULL) + theme_panel + theme(legend.position = "bottom")

  p_oa <- plot_occupancy_abundance(calculate_occupancy_abundance(res$abund_matrix)) +
    labs(subtitle = NULL) + theme_panel

  p_sar <- plot_species_area(calculate_species_area(res$abund_matrix)) +
    labs(subtitle = NULL) + theme_panel

  p_decay <- plot_distance_decay(calculate_distance_decay(res$abund_matrix, res$site_coords)) +
    labs(subtitle = NULL) + theme_panel

  p_rare <- plot_rarefaction(calculate_rarefaction(res$abund_matrix)) +
    labs(subtitle = NULL) + theme_panel + guides(color = "none") # Legend too busy for panel

  # Create a placeholder for the 3D plot
  p_placeholder <- ggplot() +
    labs(
      title = "3D Elevation Map",
      subtitle = "\nThis 3D visualization is saved\nto a separate file:\n..._fig_3d_elevation.png"
    ) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, size = 10))

  # --- 2. Combine plots using patchwork ---

  final_panel <- (p_rank | p_oa) /
    (p_sar | p_decay) /
    (p_rare | p_placeholder) +
    plot_annotation(
      title = "Advanced Ecological Analysis Panel",
      theme = theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
    )

  return(final_panel)
}


# -----------------------------------------------------------------------------
# 5. ANALYSIS & DATA EXTRACTION FUNCTIONS
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
# 6. CORE PLOTTING & REPORTING FUNCTIONS
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
  rain_resp <- which(res$P$GRADIENT_ASSIGNMENTS == "rainfall") # Added for completeness

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
  rain_species_str <- if (length(rain_resp) > 0) {
    paste(sprintf("%s (optimum %.0f mm)", res$P$GRADIENT_SPECIES[rain_resp], res$P$GRADIENT_OPTIMA[rain_resp] * 700 + 200), collapse = ", ")
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
    paste0("    Responsive species: ", rain_species_str)
  )

  # --- Gradient Correlations (Unchanged) ---
  cor_mat <- cor(res$env_gradients[, c("temperature_C", "elevation_m", "rainfall_mm")], use = "complete.obs")
  interp <- if (max(abs(cor_mat[upper.tri(cor_mat)])) < 0.3) "Gradients are approximately orthogonal (low correlation)" else "Some gradient correlation detected"
  corr_report <- c(
    "\nGradient Correlations:",
    sprintf("  Temperature-Elevation: r=%.3f", cor_mat[1, 2]),
    sprintf("  Temperature-Rainfall: r=%.3f", cor_mat[1, 3]),
    sprintf("  Elevation-Rainfall: r=%.3f", cor_mat[2, 3]),
    paste0("  Interpretation: ", interp)
  )

  # --- Species Abundance Distribution (Unchanged) ---
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

  # --- Spatial Distribution of Alpha Diversity (Unchanged) ---
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

  # --- Diversity Partitioning (UPDATED) ---
  abund_data <- res$abund_matrix %>% select(-site)
  richness_data <- abund_data %>% mutate(richness = rowSums(. > 0), n_ind = rowSums(.))
  shannon_H <- diversity(abund_data, index = "shannon")
  simpson_D <- diversity(abund_data, index = "simpson") # vegan's "simpson" is 1-D
  mean_alpha <- mean(richness_data$richness)
  se_alpha <- sd(richness_data$richness) / sqrt(nrow(richness_data))
  gamma_div <- n_distinct(res$species_dist$species)
  beta_whittaker <- gamma_div / mean_alpha
  beta_additive <- gamma_div - mean_alpha
  pa_matrix <- abund_data %>%
    as.matrix() %>%
    `>`(0) %>%
    `*`(1)
  mean_sorensen <- mean(dist(pa_matrix, method = "binary"))

  div_report <- c(
    "\nDiversity Partitioning:",
    sprintf("Alpha (mean local richness): %.2f ± %.2f SE", mean_alpha, se_alpha),
    # --- New Reporting Lines ---
    sprintf("Shannon's H' (mean): %.3f ± %.3f SE", mean(shannon_H), sd(shannon_H) / sqrt(length(shannon_H))),
    sprintf("Simpson's (1-D, mean): %.3f ± %.3f SE", mean(simpson_D), sd(simpson_D) / sqrt(length(simpson_D))),
    # --- End New Lines ---
    sprintf("Gamma (regional species pool): %d species", gamma_div),
    sprintf("Beta (Whittaker): %.2f", beta_whittaker),
    sprintf("Beta (additive): %.2f", beta_additive),
    sprintf("Mean pairwise beta (Sørensen): %.3f", mean_sorensen),
    sprintf("Mean quadrat abundance: %.1f ± %.1f", mean(richness_data$n_ind), sd(richness_data$n_ind)),
    sprintf("Abundance variation (CV): %.3f", sd(richness_data$n_ind) / mean(richness_data$n_ind))
  )

  # --- Spatial Autocorrelation & Fisher's Validation (Unchanged) ---
  space_dist <- dist(res$site_coords[, c("x", "y")])
  richness_dist <- dist(richness_data$richness)
  mantel_interp <- "  No significant spatial autocorrelation."
  if (nrow(res$site_coords) >= 4 && var(richness_data$richness) > 0) {
    mantel_test <- suppressWarnings(cor.test(space_dist, richness_dist, method = "pearson"))
    mantel_r <- mantel_test$estimate
    mantel_p <- mantel_test$p.value
    if (!is.na(mantel_p) && mantel_p < 0.05) {
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

  obs_abund <- sort(table(res$species_dist$species), decreasing = TRUE)
  n_rem <- res$P$N_INDIVIDUALS * (1 - res$P$DOMINANT_FRACTION)
  ranks <- 2:res$P$N_SPECIES
  rel_abund_theory <- res$P$FISHER_ALPHA * (res$P$FISHER_X^ranks) / ranks
  theory_abund <- c(res$P$N_INDIVIDUALS * res$P$DOMINANT_FRACTION, rel_abund_theory / sum(rel_abund_theory) * n_rem)
  len_obs <- length(obs_abund)
  len_theory <- length(theory_abund)
  max_len <- max(len_obs, len_theory)
  length(obs_abund) <- max_len
  length(theory_abund) <- max_len
  obs_abund[is.na(obs_abund)] <- 0
  residuals <- obs_abund - theory_abund
  rmse <- sqrt(mean(residuals^2, na.rm = T))
  r_squared <- if (var(obs_abund, na.rm = T) > 0) cor(obs_abund, theory_abund, use = "complete.obs")^2 else 1
  eff_alpha <- fisher.alpha(table(res$species_dist$species))
  fisher_report <- c(
    "\nFisher's Log Series Model Validation:",
    sprintf("  RMSE: %.2f", rmse), sprintf("  R-squared: %.3f", r_squared),
    sprintf("  Max residual: %.1f", max(abs(residuals), na.rm = T)),
    sprintf("  Specified alpha: %.2f", res$P$FISHER_ALPHA),
    sprintf("  Effective alpha from data: %.2f", eff_alpha)
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
# 7. MAIN SIMULATION ORCHESTRATOR
# -----------------------------------------------------------------------------

#' #' @title Run the Full Spatial Sampling Simulation
#' #' @description The main function that orchestrates the entire simulation, from
#' #'   loading parameters to generating outputs.
#' #' @param init_file Path to the `.txt` configuration file.
#' #' @param output_prefix A string used as the base for all output filenames.
#' run_spatial_simulation <- function(init_file = "simul_init.txt",
#'                                    output_prefix = "simulation_output") {
#'   # 1. Setup Paths and Cleanup
#'   timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
#'   output_prefix <- paste0(output_prefix, "_", timestamp)
#'   output_dir <- dirname(output_prefix)
#'   report_path <- paste0(output_prefix, "_report.txt")
#'
#'   # Ensure the output directory exists
#'   if (!dir.exists(output_dir)) {
#'     dir.create(output_dir, recursive = TRUE)
#'   }
#'
#'   # 2. Setup Logging in a Robust Way
#'   # This section is now more robust. It will stop with a clear error if the file can't be opened.
#'   con <- NULL
#'   # Setup a robust exit handler to close connections and sinks
#'   on.exit(
#'     {
#'       # Stop all sinks
#'       num_sinks <- sink.number()
#'       if (num_sinks > 0) {
#'         for (i in 1:num_sinks) {
#'           sink()
#'         }
#'       }
#'       # Attempt to close connection, ignoring errors if it fails
#'       try(close(con), silent = TRUE)
#'     },
#'     add = TRUE
#'   )
#'
#'   # Try to open the file connection
#'   tryCatch(
#'     {
#'       con <- file(report_path, open = "wt")
#'     },
#'     error = function(e) {
#'       stop(paste0("Failed to open report file for writing at '", report_path, "'.\nCheck path and directory permissions.\nOriginal error: ", e$message))
#'     }
#'   )
#'
#'   # If connection is successful, start sinking
#'   sink(con, type = "output")
#'   sink(con, type = "message")
#'
#'   # 3. Main Simulation Logic
#'   tryCatch(
#'     {
#'       # Load config and print to report
#'       P <- load_config(init_file)
#'       cat("========== SIMULATION PARAMETERS ==========\n")
#'       cat(paste(capture.output(dput(P[!names(P) %in% c("QUADRAT_SIZE")])), collapse = "\n"), "\n\n")
#'
#'       # Run simulation steps and print progress to report
#'       cat("========== INITIALISING SPATIAL SAMPLING SIMULATION ==========\n")
#'       cat(sprintf("Parameters: N = %d individuals, S = %d species, %d quadrats (%s size)\n", P$N_INDIVIDUALS, P$N_SPECIES, P$N_QUADRATS, P$QUADRAT_SIZE_OPTION))
#'       cat(sprintf("Fisher's parameters: α = %.1f, x = %.2f, dominant fraction = %.2f\n", P$FISHER_ALPHA, P$FISHER_X, P$DOMINANT_FRACTION))
#'       cat(sprintf("Quadrat dimensions: %.2f × %.2f units\n\n", P$QUADRAT_SIZE[1], P$QUADRAT_SIZE[2]))
#'
#'       cat("Creating irregular sampling domain...\n")
#'       domain <- create_sampling_domain()
#'       cat("Generating heterogeneous species distribution using Fisher's log series...\n")
#'       species_dist <- generate_heterogeneous_distribution(domain, P)
#'
#'       # ...
#'       cat(sprintf("Placing sampling quadrats using '%s' scheme...\n", P$SAMPLING_SCHEME))
#'
#'       quadrats <- switch(P$SAMPLING_SCHEME,
#'         "random" = place_quadrats(
#'           domain, P$N_QUADRATS, P$QUADRAT_SIZE
#'         ),
#'         "tiled" = place_quadrats_tiled(
#'           domain, P$N_QUADRATS, P$QUADRAT_SIZE
#'         ),
#'         "systematic" = place_quadrats_systematic(
#'           domain, P$N_QUADRATS, P$QUADRAT_SIZE
#'         ),
#'         "transect" = place_quadrats_transect(
#'           domain, P$N_TRANSECTS, P$N_QUADRATS_PER_TRANSECT, P$QUADRAT_SIZE, P$TRANSECT_ANGLE
#'         ),
#'         "voronoi" = place_quadrats_voronoi(
#'           domain, P$N_QUADRATS, P$QUADRAT_SIZE, P$VORONOI_SEED_FACTOR
#'         ),
#'         stop("Invalid 'SAMPLING_SCHEME'. Must be 'random', 'systematic', 'transect', or 'voronoi'.")
#'       )
#'
#'       if (nrow(quadrats) == 0) stop("Failed to place any quadrats with the selected scheme.")
#'       # ...
#'
#'       cat("Computing alpha diversity for each sampling unit...\n\n")
#'
#'       # Extract data
#'       all_species <- LETTERS[1:P$N_SPECIES]
#'       env_gradients <- create_environmental_gradients(domain, P$SAMPLING_RESOLUTION, P$ENVIRONMENTAL_NOISE)
#'       abund_matrix <- create_abundance_matrix(species_dist, quadrats, all_species)
#'       site_env <- calculate_quadrat_environment(env_gradients, quadrats, st_crs(domain))
#'       site_coords <- suppressWarnings(st_coordinates(st_centroid(quadrats))) %>%
#'         as.data.frame() %>%
#'         mutate(site = quadrats$quadrat_id) %>%
#'         select(site, x = X, y = Y)
#'
#'       # Save outputs and print progress to report
#'       cat("========== SAVING OUTPUT FILES ==========\n")
#'       f_abund <- paste0(output_prefix, "_abundances.csv")
#'       f_env <- paste0(output_prefix, "_environments.csv")
#'       f_coord <- paste0(output_prefix, "_quadrat_centroids.csv")
#'       f_png <- paste0(output_prefix, "_fig_panel.png")
#'       f_pdf <- paste0(output_prefix, "_fig_panel.pdf")
#'
#'       write.csv(abund_matrix, f_abund, row.names = FALSE)
#'       write.csv(site_env, f_env, row.names = FALSE)
#'       write.csv(site_coords, f_coord, row.names = FALSE)
#'
#'       panel_plot <- (plot_spatial_sampling(domain, species_dist, quadrats, P) | plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "temperature_C")) /
#'         (plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "elevation_m") | plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "rainfall_mm")) +
#'         plot_layout(guides = "collect") & theme(legend.position = "right")
#'       ggsave(f_png, panel_plot, width = 14, height = 10, dpi = 300, bg = "white")
#'       ggsave(f_pdf, panel_plot, width = 14, height = 10, bg = "white")
#'
#'       cat(sprintf("Panel figures saved as %s and %s\n", f_png, f_pdf))
#'       cat(sprintf("Quadrat × Species abundance table saved as %s\n", f_abund))
#'       cat(sprintf("Quadrat × environmental means table saved as %s\n", f_env))
#'       cat(sprintf("Quadrat centroid coordinates saved as %s\n", f_coord))
#'
#'       if (P$ADVANCED_ANALYSIS) {
#'         cat("\n========== GENERATING ADVANCED ANALYSIS PLOTS ==========\n")
#'
#'         # 1. Rank-Abundance Curve
#'         cat("Calculating and plotting Rank-Abundance curve...\n")
#'         f_rank_abund <- paste0(output_prefix, "_fig_rank_abundance.png")
#'         rank_abund_data <- calculate_rank_abundance(species_dist, P)
#'         rank_abund_plot <- plot_rank_abundance(rank_abund_data)
#'         ggsave(f_rank_abund, rank_abund_plot, width = 8, height = 6, dpi = 300)
#'         cat(sprintf("Rank-Abundance plot saved as %s\n", f_rank_abund))
#'
#'         # 2. Occupancy-Abundance Relationship
#'         cat("Calculating and plotting Occupancy-Abundance relationship...\n")
#'         f_oa <- paste0(output_prefix, "_fig_occupancy_abundance.png")
#'         oa_data <- calculate_occupancy_abundance(abund_matrix)
#'         oa_plot <- plot_occupancy_abundance(oa_data)
#'         ggsave(f_oa, oa_plot, width = 8, height = 7, dpi = 300)
#'         cat(sprintf("Occupancy-Abundance plot saved as %s\n", f_oa))
#'
#'         # 3. Species-Area Relationship (SAR)
#'         cat("Calculating and plotting Species-Area Relationship...\n")
#'         f_sar <- paste0(output_prefix, "_fig_species_area.png")
#'         sar_data <- calculate_species_area(abund_matrix)
#'         sar_plot <- plot_species_area(sar_data)
#'         ggsave(f_sar, sar_plot, width = 8, height = 6, dpi = 300)
#'         cat(sprintf("Species-Area Relationship plot saved as %s\n", f_sar))
#'
#'         # 4. Distance-Decay of Similarity
#'         cat("Calculating and plotting Distance-Decay relationship...\n")
#'         f_decay <- paste0(output_prefix, "_fig_distance_decay.png")
#'         decay_data <- calculate_distance_decay(abund_matrix, site_coords)
#'         decay_plot <- plot_distance_decay(decay_data)
#'         ggsave(f_decay, decay_plot, width = 8, height = 6, dpi = 300)
#'         cat(sprintf("Distance-Decay plot saved as %s\n", f_decay))
#'
#'         # 5. Rarefaction Curves
#'         cat("Calculating and plotting Rarefaction Curves...\n")
#'         f_rare <- paste0(output_prefix, "_fig_rarefaction.png")
#'         rarefaction_data <- calculate_rarefaction(abund_matrix)
#'         rarefaction_plot <- plot_rarefaction(rarefaction_data)
#'         ggsave(f_rare, rarefaction_plot, width = 8, height = 6, dpi = 300)
#'         cat(sprintf("Rarefaction Curves plot saved as %s\n", f_rare))
#'
#'         # 6. 3D Elevation Map
#'         # Check if rayshader is installed before attempting to run
#'         if (requireNamespace("rayshader", quietly = TRUE)) {
#'           cat("Generating 3D elevation map with rayshader...\n")
#'           f_3d <- paste0(output_prefix, "_fig_3d_elevation.png")
#'           plot_3d_elevation_map(env_gradients, species_dist, P, f_3d)
#'           cat(sprintf("3D Elevation Map saved as %s\n", f_3d))
#'         } else {
#'           cat("Skipping 3D plot: 'rayshader' package not found.\n")
#'         }
#'       }
#'
#'       # Generate and write the final, detailed report
#'       results_list <- list(P = P, species_dist = species_dist, quadrats = quadrats, env_gradients = env_gradients, abund_matrix = abund_matrix, site_coords = site_coords)
#'       cat("\n", generate_full_report(results_list), "\n", sep = "")
#'
#'       # This message will now appear on the console because the on.exit handler will close the sinks first
#'       message("\nSimulation completed successfully. Report and files saved to folder: ", normalizePath(output_dir))
#'     },
#'     error = function(e) {
#'       # If an error occurs, print it to the console for visibility
#'       message("\n!!!!!!!!!! SIMULATION FAILED !!!!!!!!!!\n")
#'       message("ERROR: ", e$message)
#'       # Also write it to the report file
#'       cat("\n\n!!!!!!!!!! SIMULATION FAILED !!!!!!!!!!\nError message:", e$message, "\n")
#'     }
#'   )
#' }


#' @title Run the Full Spatial Sampling Simulation
#' @description The main function that orchestrates the entire simulation.
#' @param init_file Path to the `.txt` configuration file.
#' @param output_prefix A string for the base of all output filenames.
run_spatial_simulation <- function(init_file = "simul_init.txt",
                                   output_prefix = "simulation_output") {
  # --- 1. Setup (Paths, Logging, etc.) ---
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_prefix <- paste0(output_prefix, "_", timestamp)
  output_dir <- dirname(output_prefix)
  report_path <- paste0(output_prefix, "_report.txt")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  con <- NULL
  on.exit(
    {
      num_sinks <- sink.number()
      if (num_sinks > 0) {
        for (i in 1:num_sinks) {
          sink()
        }
      }
      try(close(con), silent = TRUE)
    },
    add = TRUE
  )

  tryCatch(
    {
      con <- file(report_path, open = "wt")
    },
    error = function(e) {
      stop("Failed to open report file.")
    }
  )
  sink(con, type = "output")
  sink(con, type = "message")

  # --- 2. Core Simulation Block ---
  results_list <- NULL
  tryCatch(
    {
      P <- load_config(init_file)
      cat("========== SIMULATION PARAMETERS ==========\n")
      cat(paste(capture.output(dput(P[!names(P) %in% "QUADRAT_SIZE"])), collapse = "\n"), "\n\n")

      cat("========== INITIALISING SPATIAL SAMPLING SIMULATION ==========\n")
      domain <- create_sampling_domain()
      species_dist <- generate_heterogeneous_distribution(domain, P)
      quadrats <- switch(P$SAMPLING_SCHEME,
        "random" = place_quadrats(domain, P$N_QUADRATS, P$QUADRAT_SIZE),
        "tiled" = place_quadrats_tiled(domain, P$N_QUADRATS, P$QUADRAT_SIZE),
        "systematic" = place_quadrats_systematic(domain, P$N_QUADRATS, P$QUADRAT_SIZE),
        "transect" = place_quadrats_transect(domain, P$N_TRANSECTS, P$N_QUADRATS_PER_TRANSECT, P$QUADRAT_SIZE, P$TRANSECT_ANGLE),
        "voronoi" = place_quadrats_voronoi(domain, P$N_QUADRATS, P$QUADRAT_SIZE, P$VORONOI_SEED_FACTOR),
        stop("Invalid 'SAMPLING_SCHEME'.")
      )
      if (nrow(quadrats) == 0) stop("Failed to place any quadrats.")

      all_species <- LETTERS[1:P$N_SPECIES]
      env_gradients <- create_environmental_gradients(domain, P$SAMPLING_RESOLUTION, P$ENVIRONMENTAL_NOISE)
      abund_matrix <- create_abundance_matrix(species_dist, quadrats, all_species)
      site_env <- calculate_quadrat_environment(env_gradients, quadrats, st_crs(domain))
      site_coords <- suppressWarnings(st_coordinates(st_centroid(quadrats))) %>%
        as.data.frame() %>%
        mutate(site = quadrats$quadrat_id) %>%
        select(site, x = X, y = Y)

      cat("========== SAVING OUTPUT FILES ==========\n")
      f_abund <- paste0(output_prefix, "_abundances.csv")
      write.csv(abund_matrix, f_abund, row.names = FALSE)
      f_env <- paste0(output_prefix, "_environments.csv")
      write.csv(site_env, f_env, row.names = FALSE)
      f_coord <- paste0(output_prefix, "_quadrat_centroids.csv")
      write.csv(site_coords, f_coord, row.names = FALSE)
      f_png <- paste0(output_prefix, "_fig_panel.png")
      panel_plot <- (plot_spatial_sampling(domain, species_dist, quadrats, P) | plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "temperature_C")) /
        (plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "elevation_m") | plot_spatial_sampling(domain, species_dist, quadrats, P, TRUE, env_gradients, "rainfall_mm")) +
        plot_layout(guides = "collect") & theme(legend.position = "right")
      ggsave(f_png, panel_plot, width = 14, height = 10, dpi = 300, bg = "white")
      cat(sprintf("Main panel figure saved as %s\n", f_png))
      cat(sprintf("Data tables saved.\n"))

      results_list <- list(
        P = P, domain = domain, species_dist = species_dist, quadrats = quadrats,
        env_gradients = env_gradients, abund_matrix = abund_matrix, site_coords = site_coords
      )
    },
    error = function(e) {
      message("\n!!!!!!!!!! CORE SIMULATION FAILED !!!!!!!!!!\n")
      message("ERROR: ", e$message)
      cat("\n\n!!!!!!!!!! CORE SIMULATION FAILED !!!!!!!!!!\nError message:", e$message, "\n")
    }
  )

  # --- 3. Advanced Analysis Block ---
  if (!is.null(results_list) && results_list$P$ADVANCED_ANALYSIS) {
    tryCatch(
      {
        cat("\n========== GENERATING ADVANCED ANALYSIS PLOTS ==========\n")

        # --- Generate and save the 2D panel plot ---
        cat("Generating advanced analysis panel...\n")
        f_adv_panel <- paste0(output_prefix, "_fig_advanced_panel.png")
        advanced_panel_plot <- generate_advanced_panel(results_list)
        ggsave(f_adv_panel, advanced_panel_plot, width = 12, height = 14, dpi = 300, bg = "white")
        cat(sprintf("Advanced analysis panel saved as %s\n", f_adv_panel))

        # --- Generate and save the 3D plot separately ---
        if (requireNamespace("rayshader", quietly = TRUE)) {
          cat("Generating 3D elevation map with rayshader...\n")
          f_3d <- paste0(output_prefix, "_fig_3d_elevation.png")
          plot_3d_elevation_map(results_list$env_gradients, f_3d)
        } else {
          cat("Skipping 3D plot: 'rayshader' package not found.\n")
        }
      },
      error = function(e) {
        message("\n!!!!!!!!!! ADVANCED ANALYSIS FAILED !!!!!!!!!!\n")
        message("ERROR: ", e$message)
        cat("\n\n!!!!!!!!!! ADVANCED ANALYSIS FAILED !!!!!!!!!!\nError:", e$message, "\n")
      }
    )
  }

  # --- 4. Final Reporting ---
  if (!is.null(results_list)) {
    cat("\n", generate_full_report(results_list), "\n", sep = "")
    message("\nSimulation completed. Report and files saved to folder: ", normalizePath(output_dir))
  }
}


# -----------------------------------------------------------------------------
# 8. USAGE EXAMPLE
# -----------------------------------------------------------------------------
# --- UNCOMMENT THE LINES BELOW TO RUN A DEMO ---
#
# # This will create a "simulation_demo" folder in the current working directory,
# # create a dummy configuration file inside it, and then run the simulation,
# # saving all outputs into that same folder.
#
# if (!dir.exists("simulation_demo")) dir.create("simulation_demo")
# writeLines(c(
#   "N_INDIVIDUALS = 1500", "N_SPECIES = 10", "N_QUADRATS=15",
#   "QUADRAT_SIZE_OPTION = small", "FISHER_ALPHA=2.5", "DOMINANT_FRACTION=0.3"
#   ), "simulation_demo/demo_init.txt")
run_spatial_simulation(
  init_file = "BDC334/spesim/spesim_init.txt",
  output_prefix = "BDC334/spesim/demo_output"
)
