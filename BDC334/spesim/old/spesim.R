# SPATIAL SAMPLING SIMULATION

run_spatial_simulation <- function(init_file = "simul_init.txt",
                                   output_prefix = "simulation_output") {
  # ---- Timestamp utility ----
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_prefix <- paste0(output_prefix, "_", timestamp)

  # ---- Color definitions for dark outlines/text ----
  DARK_COLOUR <- "#22223b" # Very dark blue-black for outlines and text

  # ---- Helper: Parse simulation parameters from file ----
  parse_init <- function(filename) {
    lines <- readLines(filename, warn = FALSE)
    lines <- gsub("#.*", "", lines) # Remove any comments, wherever they occur
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    params <- list()
    for (ln in lines) {
      if (!grepl("=", ln)) next
      kv <- strsplit(ln, "=")[[1]]
      key <- toupper(trimws(kv[1]))
      val <- trimws(kv[2])
      # Remove surrounding quotes if present
      val <- gsub('^"|"$', "", val)
      if (grepl(",", val)) {
        nums <- as.numeric(strsplit(val, ",")[[1]])
        if (all(!is.na(nums))) {
          params[[key]] <- nums
        } else {
          params[[key]] <- strsplit(val, ",")[[1]]
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

  # ---- Load settings ----
  if (!file.exists(init_file)) stop(paste("Parameter file", init_file, "not found."))
  P <- parse_init(init_file)
  set_default <- function(param, default) if (!is.null(P[[param]])) P[[param]] else default

  # --- SIMULATION PARAMETERS --
  set.seed(set_default("SEED", 13))
  N_INDIVIDUALS <- set_default("N_INDIVIDUALS", 2000)
  N_SPECIES <- set_default("N_SPECIES", 15)
  DOMINANT_FRACTION <- set_default("DOMINANT_FRACTION", 0.35)
  FISHER_ALPHA <- set_default("FISHER_ALPHA", 3.0)
  FISHER_X <- set_default("FISHER_X", 0.95)
  N_QUADRATS <- set_default("N_QUADRATS", 20)
  QUADRAT_SIZE_OPTION <- set_default("QUADRAT_SIZE_OPTION", "small")
  SAMPLING_RESOLUTION <- set_default("SAMPLING_RESOLUTION", 50)
  GRADIENT_SPECIES <- c("B", "C", "D")
  GRADIENT_ASSIGNMENTS <- c("temperature", "temperature", "elevation")
  GRADIENT_OPTIMA <- set_default("GRADIENT_OPTIMA", c(0.3, 0.7, 0.5))
  GRADIENT_TOLERANCE <- set_default("GRADIENT_TOLERANCE", 0.2)
  ENVIRONMENTAL_NOISE <- set_default("ENVIRONMENTAL_NOISE", 0.05)
  CLUSTER_SPREAD_DOMINANT <- set_default("CLUSTER_SPREAD_DOMINANT", 3)
  CLUSTER_SPREAD_RARE <- set_default("CLUSTER_SPREAD_RARE", 6)
  MAX_CLUSTERS_DOMINANT <- set_default("MAX_CLUSTERS_DOMINANT", 5)
  POINT_SIZE <- set_default("POINT_SIZE", 0.3)
  POINT_ALPHA <- set_default("POINT_ALPHA", 0.7)
  QUADRAT_ALPHA <- set_default("QUADRAT_ALPHA", 0.05)
  BACKGROUND_COLOUR <- "white"
  FOREGROUND_COLOUR <- DARK_COLOUR
  QUADRAT_COLOUR <- "black"
  QUADRAT_SIZES <- list(
    small = c(1, 1),
    medium = c(1.5, 1.5),
    large = c(2, 2)
  )
  QUADRAT_SIZE <- QUADRAT_SIZES[[QUADRAT_SIZE_OPTION]]

  # ---- Load required libraries ----
  suppressPackageStartupMessages({
    library(sf)
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(viridis)
    library(stringr)
    library(patchwork)
  })

  # ---- ALL FUNCTION DEFINITIONS ----

  # --- Irregular domain creator ----
  create_sampling_domain <- function() {
    theta <- seq(0, 2 * pi, length.out = 20)
    r <- 10 + 3 * sin(3 * theta) + 2 * cos(5 * theta) + runif(20, -1, 1)
    x <- r * cos(theta) + runif(20, -0.5, 0.5)
    y <- r * sin(theta) * 0.8 + runif(20, -0.5, 0.5)
    coords <- cbind(x, y)
    coords <- rbind(coords, coords[1, ])
    polygon <- st_polygon(list(coords))
    sampling_area <- st_sfc(polygon)
    return(st_sf(geometry = sampling_area))
  }

  # --- Fisher's log series abundance generator ----
  generate_fisher_log_series <- function(n_species = N_SPECIES, n_individuals = N_INDIVIDUALS,
                                         dominant_fraction = DOMINANT_FRACTION,
                                         alpha = FISHER_ALPHA, x = FISHER_X) {
    n_dominant <- round(n_individuals * dominant_fraction)
    n_remaining <- n_individuals - n_dominant
    ranks <- 2:n_species
    relative_abundances <- alpha * (x^ranks) / ranks
    relative_abundances <- relative_abundances / sum(relative_abundances)
    abundances <- round(relative_abundances * n_remaining)
    all_abundances <- c(n_dominant, abundances)
    adjustment <- n_individuals - sum(all_abundances)
    if (adjustment != 0) all_abundances[2] <- all_abundances[2] + adjustment
    species_names <- LETTERS[1:n_species]
    names(all_abundances) <- species_names
    return(all_abundances)
  }

  # --- Environmental gradient field ----
  create_environmental_gradients <- function(polygon, resolution = SAMPLING_RESOLUTION,
                                             noise_level = ENVIRONMENTAL_NOISE) {
    bbox <- st_bbox(polygon)
    x_seq <- seq(bbox["xmin"], bbox["xmax"], length.out = resolution)
    y_seq <- seq(bbox["ymin"], bbox["ymax"], length.out = resolution)
    grid <- expand.grid(x = x_seq, y = y_seq)
    x_norm <- (grid$x - bbox["xmin"]) / (bbox["xmax"] - bbox["xmin"])
    y_norm <- (grid$y - bbox["ymin"]) / (bbox["ymax"] - bbox["ymin"])
    temperature_values <- x_norm * 0.7 + y_norm * 0.3 + rnorm(length(x_norm), 0, noise_level)
    temperature_values <- pmax(0, pmin(1, temperature_values))
    center_x <- 0.5
    center_y <- 0.5
    distance_from_center <- sqrt((x_norm - center_x)^2 + (y_norm - center_y)^2)
    max_distance <- sqrt(0.5^2 + 0.5^2)
    elevation_values <- 1 - (distance_from_center / max_distance) + rnorm(length(x_norm), 0, noise_level)
    elevation_values <- pmax(0, pmin(1, elevation_values))
    rainfall_values <- (-x_norm * 0.6 + y_norm * 0.8)
    rainfall_values <- (rainfall_values - min(rainfall_values)) / (max(rainfall_values) - min(rainfall_values))
    rainfall_values <- rainfall_values + rnorm(length(x_norm), 0, noise_level)
    rainfall_values <- pmax(0, pmin(1, rainfall_values))
    grid$temperature <- temperature_values
    grid$elevation <- elevation_values
    grid$rainfall <- rainfall_values
    # ---- Real-world transformations ----
    grid$temperature_C <- grid$temperature * 40 - 2 # -2 to +38 °C
    grid$elevation_m <- grid$elevation * 2000 # 0 to 2000 m
    grid$rainfall_mm <- grid$rainfall * 2800 + 200 # 200 to 3000 mm
    return(grid)
  }

  # --- Heterogeneous species distribution generator ----
  generate_heterogeneous_distribution <- function(polygon, n_individuals = N_INDIVIDUALS,
                                                  n_species = N_SPECIES) {
    species_abundances <- generate_fisher_log_series(n_species, n_individuals)
    env_gradient <- create_environmental_gradients(polygon)
    all_points <- st_sample(polygon, size = n_individuals)
    coords <- st_coordinates(all_points)
    species_vector <- character(n_individuals)
    for (sp in names(species_abundances)) {
      n_ind <- species_abundances[sp]
      if (sp %in% GRADIENT_SPECIES) {
        sp_index <- which(GRADIENT_SPECIES == sp)
        optimum <- GRADIENT_OPTIMA[sp_index]
        gradient_type <- GRADIENT_ASSIGNMENTS[sp_index]
        env_values <- numeric(nrow(coords))
        for (i in 1:nrow(coords)) {
          distances <- sqrt((env_gradient$x - coords[i, 1])^2 + (env_gradient$y - coords[i, 2])^2)
          nearest_gradient <- env_gradient[[gradient_type]][which.min(distances)]
          env_values[i] <- exp(-((nearest_gradient - optimum)^2) / (2 * GRADIENT_TOLERANCE^2))
        }
        available_indices <- which(species_vector == "")
        if (length(available_indices) > n_ind) {
          probs <- env_values[available_indices]
          probs <- probs / sum(probs)
          selected <- sample(available_indices, size = n_ind, prob = probs)
          species_vector[selected] <- sp
        } else {
          species_vector[available_indices] <- sp
        }
      } else if (sp == "A") {
        available_indices <- which(species_vector == "")
        if (length(available_indices) >= n_ind) {
          n_clusters <- sample(3:MAX_CLUSTERS_DOMINANT, 1)
          cluster_centers <- sample(available_indices, min(n_clusters, length(available_indices)))
          selected_indices <- c()
          remaining <- n_ind
          for (center in cluster_centers) {
            if (remaining <= 0) break
            cluster_size <- min(sample(ceiling(n_ind / n_clusters * 0.5):ceiling(n_ind / n_clusters * 1.5), 1), remaining)
            center_coord <- coords[center, ]
            distances <- sqrt((coords[available_indices, 1] - center_coord[1])^2 + (coords[available_indices, 2] - center_coord[2])^2)
            probs <- exp(-distances / CLUSTER_SPREAD_DOMINANT)
            probs[available_indices %in% selected_indices] <- 0
            if (sum(probs) > 0) {
              probs <- probs / sum(probs)
              new_selections <- sample(available_indices, size = min(cluster_size, sum(probs > 0)), prob = probs)
              selected_indices <- c(selected_indices, new_selections)
              remaining <- remaining - length(new_selections)
            }
          }
          if (remaining > 0) {
            available_for_random <- setdiff(available_indices, selected_indices)
            if (length(available_for_random) > 0) {
              additional <- sample(available_for_random, min(remaining, length(available_for_random)))
              selected_indices <- c(selected_indices, additional)
            }
          }
          species_vector[selected_indices[1:n_ind]] <- sp
        }
      } else {
        available_indices <- which(species_vector == "")
        if (length(available_indices) >= n_ind) {
          n_clusters <- sample(1:2, 1)
          if (n_clusters == 1 || n_ind < 10) {
            center <- sample(available_indices, 1)
            center_coord <- coords[center, ]
            distances <- sqrt((coords[available_indices, 1] - center_coord[1])^2 + (coords[available_indices, 2] - center_coord[2])^2)
            probs <- exp(-distances / CLUSTER_SPREAD_RARE)
            probs <- probs / sum(probs)
            selected <- sample(available_indices, size = n_ind, prob = probs)
            species_vector[selected] <- sp
          } else {
            selected_indices <- c()
            remaining <- n_ind
            for (i in 1:n_clusters) {
              if (remaining <= 0) break
              cluster_size <- min(ceiling(remaining / (n_clusters - i + 1)), remaining)
              available_for_cluster <- setdiff(available_indices, selected_indices)
              if (length(available_for_cluster) > 0) {
                center <- sample(available_for_cluster, 1)
                center_coord <- coords[center, ]
                distances <- sqrt((coords[available_for_cluster, 1] - center_coord[1])^2 + (coords[available_for_cluster, 2] - center_coord[2])^2)
                probs <- exp(-distances / 5)
                probs <- probs / sum(probs)
                new_selections <- sample(available_for_cluster, size = min(cluster_size, length(available_for_cluster)), prob = probs)
                selected_indices <- c(selected_indices, new_selections)
                remaining <- remaining - length(new_selections)
              }
            }
            species_vector[selected_indices[1:n_ind]] <- sp
          }
        }
      }
    }
    species_data <- data.frame(species = species_vector, geometry = st_geometry(all_points)) %>%
      st_as_sf() %>%
      filter(species != "")
    return(species_data)
  }

  # --- Quadrat placement (non-overlapping, contained) ----
  place_quadrats <- function(polygon, n_quadrats = N_QUADRATS, quadrat_size = QUADRAT_SIZE) {
    bbox <- st_bbox(polygon)
    quadrats <- list()
    attempts <- 0
    max_attempts <- 2000
    while (length(quadrats) < n_quadrats && attempts < max_attempts) {
      attempts <- attempts + 1
      x_center <- runif(1, bbox["xmin"] + quadrat_size[1] / 2, bbox["xmax"] - quadrat_size[1] / 2)
      y_center <- runif(1, bbox["ymin"] + quadrat_size[2] / 2, bbox["ymax"] - quadrat_size[2] / 2)
      x_coords <- c(x_center - quadrat_size[1] / 2, x_center + quadrat_size[1] / 2, x_center + quadrat_size[1] / 2, x_center - quadrat_size[1] / 2, x_center - quadrat_size[1] / 2)
      y_coords <- c(y_center - quadrat_size[2] / 2, y_center - quadrat_size[2] / 2, y_center + quadrat_size[2] / 2, y_center + quadrat_size[2] / 2, y_center - quadrat_size[2] / 2)
      quadrat_coords <- cbind(x_coords, y_coords)
      quadrat_poly <- st_polygon(list(quadrat_coords))
      quadrat_sfc <- st_sfc(quadrat_poly)
      if (st_within(quadrat_sfc, polygon, sparse = FALSE)[1, 1]) {
        overlap <- FALSE
        if (length(quadrats) > 0) {
          existing_quadrats <- st_sfc(quadrats)
          if (any(st_intersects(quadrat_sfc, existing_quadrats, sparse = FALSE))) overlap <- TRUE
        }
        if (!overlap) quadrats[[length(quadrats) + 1]] <- quadrat_poly
      }
    }
    if (length(quadrats) < n_quadrats) {
      warning(sprintf("Placed %d of %d requested quadrats after %d attempts.", length(quadrats), n_quadrats, attempts))
    }
    quadrat_sf <- st_sf(quadrat_id = 1:length(quadrats), geometry = st_sfc(quadrats))
    return(quadrat_sf)
  }

  # --- Alpha richness per quadrat ----
  calculate_richness <- function(species_data, quadrats) {
    richness_data <- data.frame()
    for (i in 1:nrow(quadrats)) {
      species_in_quadrat <- suppressWarnings(st_intersection(species_data, quadrats[i, ]))
      if (nrow(species_in_quadrat) > 0) {
        richness <- length(unique(species_in_quadrat$species))
        species_list <- sort(unique(species_in_quadrat$species))
        richness_data <- rbind(richness_data, data.frame(
          quadrat_id = i, richness = richness,
          species_list = paste(species_list, collapse = ", "),
          n_individuals = nrow(species_in_quadrat)
        ))
      } else {
        richness_data <- rbind(richness_data, data.frame(
          quadrat_id = i, richness = 0, species_list = "None", n_individuals = 0
        ))
      }
    }
    return(richness_data)
  }

  # --- Visualisation ----
  plot_spatial_sampling <- function(domain, species, quadrats, richness_data,
                                    show_gradient = FALSE, env_gradients = NULL,
                                    gradient_type = "temperature") {
    mean_alpha_diversity <- mean(richness_data$richness)
    species_colors <- c(
      "A" = "#FF6B6B", "B" = "#4ECDC4", "C" = "#45B7D1", "D" = "#96CEB4",
      "E" = "#FFEAA7", "F" = "#DDA0DD", "G" = "#98D8C8", "H" = "#F7DC6F",
      "I" = "#F8B739", "J" = "#52B788", "K" = "#F72585", "L" = "#7209B7",
      "M" = "#3A0CA3", "N" = "#F1C40F", "O" = "#E74C3C"
    )
    p <- ggplot() +
      theme_void() +
      theme(
        panel.background = element_rect(fill = BACKGROUND_COLOUR, color = NA),
        plot.background = element_rect(fill = BACKGROUND_COLOUR, color = NA),
        plot.title = element_text(color = FOREGROUND_COLOUR, size = 16, face = "bold"),
        plot.subtitle = element_text(color = FOREGROUND_COLOUR, size = 12),
        plot.caption = element_text(color = FOREGROUND_COLOUR, size = 10, hjust = 0),
        legend.text = element_text(color = FOREGROUND_COLOUR, size = 8),
        legend.title = element_text(color = FOREGROUND_COLOUR, face = "bold"),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      geom_sf(data = domain, fill = "grey90", color = FOREGROUND_COLOUR, linewidth = 0.6, alpha = 0.3)
    if (show_gradient && !is.null(env_gradients) && gradient_type %in% c("temperature", "elevation", "rainfall")) {
      gradient_raster <- env_gradients %>%
        dplyr::select(x, y, gradient = all_of(gradient_type))
      p <- p +
        geom_tile(data = gradient_raster, aes(x = x, y = y, fill = gradient), alpha = 0.5) +
        scale_fill_viridis(option = "viridis", name = paste(str_to_title(gradient_type), "\nGradient"), guide = "none")
    }
    p <- p +
      geom_sf(data = species, aes(color = species), size = POINT_SIZE, alpha = POINT_ALPHA) +
      scale_color_manual(values = species_colors, name = "Species") +
      geom_sf(data = quadrats, fill = "white", alpha = QUADRAT_ALPHA, color = QUADRAT_COLOUR, linewidth = 0.3) +
      geom_sf_text(data = quadrats, aes(label = quadrat_id), color = FOREGROUND_COLOUR, size = 2.5) +
      labs(
        title = expression(paste("Spatial patterns of species richness (", alpha, "-diversity)")),
        subtitle = paste0(
          "Mean α-diversity: ", round(mean_alpha_diversity, 2),
          " | γ-diversity: ", length(unique(species$species)),
          " species | Fisher's logarithmic series"
        ),
        caption = paste0(
          "Community parameters: N = ", nrow(species),
          " individuals, α = ", FISHER_ALPHA, ", dominant fraction = ", DOMINANT_FRACTION,
          "\nQuadrat richness values: ", paste(richness_data$richness, collapse = ", "),
          "\nSpecies B & C gradient response; D = elevation"
        )
      ) +
      coord_sf(expand = TRUE) +
      guides(color = guide_legend(ncol = 2))
    return(p)
  }

  # --- Panel plotting function (returns patchwork object) ----
  make_panel <- function(domain, species, quadrats, richness, env_gradients) {
    p0 <- plot_spatial_sampling(domain, species, quadrats, richness, show_gradient = FALSE)
    pT <- plot_spatial_sampling(domain, species, quadrats, richness, show_gradient = TRUE, env_gradients = env_gradients, gradient_type = "temperature")
    pE <- plot_spatial_sampling(domain, species, quadrats, richness, show_gradient = TRUE, env_gradients = env_gradients, gradient_type = "elevation")
    pR <- plot_spatial_sampling(domain, species, quadrats, richness, show_gradient = TRUE, env_gradients = env_gradients, gradient_type = "rainfall")
    panel <- (p0 | pT) / (pE | pR)
    panel
  }

  # MAIN EXECUTION (all outputs go to file)

  reportfile <- paste0(output_prefix, ".txt")
  con <- file(reportfile, open = "wt")
  sink(con, split = FALSE, type = "output")
  sink(con, append = TRUE, type = "message")
  on.exit(
    {
      sink(type = "message")
      sink(type = "output")
      close(con)
    },
    add = TRUE
  )

  # --- Simulation steps (unchanged except filenames now include timestamp)  ----
  cat("========== SIMULATION PARAMETERS ==========\n")
  cat(paste(capture.output(dput(P)), collapse = "\n"), "\n\n")
  cat("========== INITIALIZING SPATIAL SAMPLING SIMULATION ==========\n")
  cat(sprintf(
    "Parameters: N = %d individuals, S = %d species, %d quadrats (%s size)\n",
    N_INDIVIDUALS, N_SPECIES, N_QUADRATS, QUADRAT_SIZE_OPTION
  ))
  cat(sprintf(
    "Fisher's parameters: α = %.1f, x = %.2f, dominant fraction = %.2f\n",
    FISHER_ALPHA, FISHER_X, DOMINANT_FRACTION
  ))
  cat(sprintf("Quadrat dimensions: %.2f × %.2f units\n\n", QUADRAT_SIZE[1], QUADRAT_SIZE[2]))

  cat("Creating irregular sampling domain...\n")
  sampling_domain <- create_sampling_domain()
  cat("Generating heterogeneous species distribution using Fisher's log series...\n")
  species_distribution <- generate_heterogeneous_distribution(sampling_domain)
  cat("Placing sampling quadrats with non-overlap constraints...\n")
  quadrats <- place_quadrats(sampling_domain)
  cat("Computing alpha diversity for each sampling unit...\n")
  richness_results <- calculate_richness(species_distribution, quadrats)
  env_gradients <- create_environmental_gradients(sampling_domain)

  # --- Panel figures ----
  cat("\n========== SAVING PANELLED FIGURES ==========\n")
  figpanel <- make_panel(sampling_domain, species_distribution, quadrats, richness_results, env_gradients)
  figpng <- paste0(output_prefix, "_fig_panel.png")
  figpdf <- paste0(output_prefix, "_fig_panel.pdf")
  ggsave(figpng, figpanel, width = 18, height = 12, dpi = 300, bg = "white")
  ggsave(figpdf, figpanel, width = 18, height = 12, bg = "white")
  cat(sprintf("Panel figures saved as %s and %s\n", figpng, figpdf))

  # --- CSV: Site x Species Abundances ----
  abundmat <- matrix(0, nrow = nrow(quadrats), ncol = N_SPECIES)
  colnames(abundmat) <- LETTERS[1:N_SPECIES]
  for (i in 1:nrow(quadrats)) {
    spp <- suppressWarnings(st_intersection(species_distribution, quadrats[i, ]))
    if (nrow(spp) > 0) {
      tb <- table(spp$species)
      abundmat[i, names(tb)] <- as.numeric(tb)
    }
  }
  site_sp_csv <- paste0(output_prefix, "_abundances.csv")
  abund_df <- as.data.frame(abundmat)
  abund_df$site <- 1:nrow(quadrats)
  abund_df <- abund_df[, c("site", LETTERS[1:N_SPECIES])]
  write.csv(abund_df, site_sp_csv, row.names = FALSE)
  cat(sprintf("Quadrat × Species abundance table saved as %s\n", site_sp_csv))

  # --- CSV: Site x Environmental Means ----
  site_env <- data.frame(
    site = 1:nrow(quadrats),
    temperature_C = NA, elevation_m = NA, rainfall_mm = NA
  )
  for (i in 1:nrow(quadrats)) {
    quad_poly <- quadrats[i, ]
    pts_in <- env_gradients[as.logical(st_within(
      st_as_sf(env_gradients, coords = c("x", "y"), crs = st_crs(sampling_domain)),
      quad_poly,
      sparse = FALSE
    )), ]
    if (nrow(pts_in) > 0) {
      site_env$temperature_C[i] <- mean(pts_in$temperature_C)
      site_env$elevation_m[i] <- mean(pts_in$elevation_m)
      site_env$rainfall_mm[i] <- mean(pts_in$rainfall_mm)
    }
  }
  site_env_csv <- paste0(output_prefix, "_environments.csv")
  write.csv(site_env, site_env_csv, row.names = FALSE)
  cat(sprintf("Quadrat × environmental means table saved as %s\n", site_env_csv))

  # --- CSV: Quadrat centroids (coordinates) ----
  quad_centroids <- st_centroid(quadrats)
  quad_coords <- st_coordinates(quad_centroids)
  quad_coords_df <- data.frame(
    site = quadrats$quadrat_id,
    x = quad_coords[, 1],
    y = quad_coords[, 2]
  )
  site_centroid_csv <- paste0(output_prefix, "_quadrat_centroids.csv")
  write.csv(quad_coords_df, site_centroid_csv, row.names = FALSE)
  cat(sprintf("Quadrat centroid coordinates saved as %s\n", site_centroid_csv))

  # ---- REPORT/RESULTS SECTION: (all printed to the report file) ----

  cat("\n========== ANALYSIS REPORT ==========\n")
  temp_range <- range(env_gradients$temperature_C)
  elev_range <- range(env_gradients$elevation_m)
  rain_range <- range(env_gradients$rainfall_mm)

  cat(sprintf("\nEnvironmental Gradients:\n"))
  cat(sprintf(
    "  Temperature: %.1f–%.1f °C (range: %.1f °C)\n",
    temp_range[1], temp_range[2], diff(temp_range)
  ))
  cat(sprintf("    Pattern: Diagonal (NW cool → SE warm)\n"))
  cat(sprintf(
    "    Responsive species: B (optimum %.1f °C), C (%.1f °C)\n",
    GRADIENT_OPTIMA[1] * 40 - 2, GRADIENT_OPTIMA[2] * 40 - 2
  ))
  cat(sprintf(
    "  Elevation: %.0f–%.0f m (range: %.0f m)\n",
    elev_range[1], elev_range[2], diff(elev_range)
  ))
  cat(sprintf("    Pattern: Central peak (mountain-like topology)\n"))
  cat(sprintf(
    "    Responsive species: D (optimum %.0f m)\n",
    GRADIENT_OPTIMA[3] * 2000
  ))
  cat(sprintf(
    "  Rainfall: %.0f–%.0f mm (range: %.0f mm)\n",
    rain_range[1], rain_range[2], diff(rain_range)
  ))
  cat(sprintf("    Pattern: Perpendicular (NE dry → SW wet)\n"))
  cat(sprintf("    Responsive species: None\n"))
  temp_elev_cor <- cor(env_gradients$temperature, env_gradients$elevation)
  temp_rain_cor <- cor(env_gradients$temperature, env_gradients$rainfall)
  elev_rain_cor <- cor(env_gradients$elevation, env_gradients$rainfall)
  cat(sprintf(
    "\nGradient Correlations:\n  Temperature-Elevation: r=%.3f\n  Temperature-Rainfall: r=%.3f\n  Elevation-Rainfall: r=%.3f\n",
    temp_elev_cor, temp_rain_cor, elev_rain_cor
  ))
  if (max(abs(c(temp_elev_cor, temp_rain_cor, elev_rain_cor))) < 0.3) {
    cat("  Interpretation: Gradients are approximately orthogonal (low correlation)\n")
  } else {
    cat("  Interpretation: Some gradient correlation detected\n")
  }

  cat("\nSpecies Abundance Distribution:\n")
  species_counts <- table(species_distribution$species)
  total_individuals <- sum(species_counts)
  for (sp in names(sort(species_counts, decreasing = TRUE))) {
    ecological_role <- dplyr::case_when(
      sp == "A" ~ "[DOMINANT - clustered]",
      sp == "B" ~ "[TEMP-RESPONSIVE]",
      sp == "C" ~ "[TEMP-RESPONSIVE]",
      sp == "D" ~ "[ELEVATION-RESPONSIVE]",
      TRUE ~ "[SUBORDINATE]"
    )
    cat(sprintf(
      "  Species %s: %3d individuals (%5.1f%%) %s\n",
      sp, species_counts[sp],
      100 * species_counts[sp] / total_individuals,
      ecological_role
    ))
  }

  cat("\nSpatial Distribution of Alpha Diversity:\n")
  for (i in 1:nrow(richness_results)) {
    spp <- suppressWarnings(st_intersection(species_distribution, quadrats[i, ]))
    if (nrow(spp) > 0) {
      species_abundances <- table(spp$species)
      species_abundances <- species_abundances[order(names(species_abundances))]
      abundance_details <- paste(names(species_abundances), "(", species_abundances, ")", sep = "", collapse = ", ")
      cat(sprintf(
        "  Quadrat %2d: α = %2d species | N = %3d individuals\n    Species: %s\n",
        richness_results$quadrat_id[i],
        richness_results$richness[i],
        richness_results$n_individuals[i],
        abundance_details
      ))
    } else {
      cat(sprintf(
        "  Quadrat %2d: α = %2d species | N = %3d individuals\n    Species: None\n",
        richness_results$quadrat_id[i],
        richness_results$richness[i],
        richness_results$n_individuals[i]
      ))
    }
  }

  mean_alpha <- mean(richness_results$richness)
  se_alpha <- sd(richness_results$richness) / sqrt(nrow(richness_results))
  gamma_diversity <- length(unique(species_distribution$species))
  beta_whittaker <- gamma_diversity / mean_alpha
  beta_additive <- gamma_diversity - mean_alpha

  if (nrow(richness_results) >= 2) {
    composition_matrix <- matrix(0, nrow = nrow(richness_results), ncol = length(unique(species_distribution$species)))
    colnames(composition_matrix) <- sort(unique(species_distribution$species))
    for (i in 1:nrow(richness_results)) {
      spp <- suppressWarnings(st_intersection(species_distribution, quadrats[i, ]))
      if (nrow(spp) > 0) {
        species_present <- unique(spp$species)
        composition_matrix[i, species_present] <- 1
      }
    }
    sorensen_distances <- dist(composition_matrix, method = "binary")
    mean_sorensen <- mean(sorensen_distances)
  }

  mean_local_abundance <- mean(richness_results$n_individuals)
  abundance_cv <- sd(richness_results$n_individuals) / mean(richness_results$n_individuals)
  cat("\nDiversity Partitioning:\n")
  cat(sprintf("Alpha (mean local richness): %.2f ± %.2f SE\n", mean_alpha, se_alpha))
  cat(sprintf("Gamma (regional species pool): %d species\n", gamma_diversity))
  cat(sprintf("Beta (Whittaker): %.2f\n", beta_whittaker))
  cat(sprintf("Beta (additive): %.2f\n", beta_additive))
  if (exists("mean_sorensen")) cat(sprintf("Mean pairwise beta (Sørensen): %.3f\n", mean_sorensen))
  cat(sprintf("Mean quadrat abundance: %.1f ± %.1f\n", mean_local_abundance, sd(richness_results$n_individuals)))
  cat(sprintf("Abundance variation (CV): %.3f\n", abundance_cv))

  if (nrow(quadrats) >= 4) {
    cat("\nSpatial Autocorrelation Analysis:\n")
    quadrat_coords <- st_coordinates(st_centroid(quadrats))
    spatial_distances <- as.matrix(dist(quadrat_coords))
    richness_distances <- as.matrix(dist(richness_results$richness))
    spatial_vec <- spatial_distances[upper.tri(spatial_distances)]
    richness_vec <- richness_distances[upper.tri(richness_distances)]
    mantel_correlation <- cor.test(spatial_vec, richness_vec, method = "pearson")
    cat(sprintf("Spatial autocorrelation in richness (Mantel r): %.3f\n", mantel_correlation$estimate))
    cat(sprintf("Statistical significance: p = %.3f\n", mantel_correlation$p.value))
    if (mantel_correlation$p.value < 0.05) {
      if (mantel_correlation$estimate > 0) {
        cat("  Significant positive autocorrelation: environmental filtering or dispersal limitation.\n")
      } else {
        cat("  Significant negative autocorrelation: competitive exclusion or strong heterogeneity.\n")
      }
    } else {
      cat("  No significant spatial autocorrelation.\n")
    }
    cat(sprintf("  Mean inter-quadrat distance: %.2f\n", mean(spatial_vec)))
    cat(sprintf("  Richness variance: %.2f\n", var(richness_results$richness)))
  }

  cat("\nFisher's Log Series Model Validation:\n")
  observed_abundances <- sort(as.numeric(species_counts), decreasing = TRUE)
  theoretical_abundances <- c(
    N_INDIVIDUALS * DOMINANT_FRACTION,
    FISHER_ALPHA * FISHER_X^(2:N_SPECIES) / (2:N_SPECIES) *
      (N_INDIVIDUALS * (1 - DOMINANT_FRACTION)) /
      sum(FISHER_ALPHA * FISHER_X^(2:N_SPECIES) / (2:N_SPECIES))
  )
  abundance_residuals <- observed_abundances - theoretical_abundances
  rmse <- sqrt(mean(abundance_residuals^2))
  r_squared <- cor(observed_abundances, theoretical_abundances)^2
  cat(sprintf(
    "  RMSE: %.2f\n  R-squared: %.3f\n  Max residual: %.1f\n",
    rmse, r_squared, max(abs(abundance_residuals))
  ))
  effective_alpha <- -sum(species_counts * log(FISHER_X)) / sum(species_counts)
  cat(sprintf(
    "  Specified alpha: %.2f\n  Effective alpha from data: %.2f\n",
    FISHER_ALPHA, effective_alpha
  ))

  cat("\nSIMULATION COMPLETED SUCCESSFULLY.\n")
}

# ---- USAGE EXAMPLE (uncomment to run) ----
run_spatial_simulation("BDC334/landscape_sim/simul_init.txt", "BDC334/landscape_sim/my_simulation")
