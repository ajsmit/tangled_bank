#!/usr/bin/env Rscript

# ============================================================================
# TANGLED BANK WEBSITE PACKAGE INSTALLER
# ============================================================================
# This script checks for and installs all R packages required by the 
# Tangled Bank Quarto website (https://github.com/ajsmit/tangled_bank)
# 
# Usage: 
#   - Run from R console: source("install_packages.R")
#   - Run from command line: Rscript install_packages.R
# 
# Author: Generated automatically from website analysis
# Date: 2025-08-26
# ============================================================================

cat("=================================================================\n")
cat("TANGLED BANK WEBSITE - R PACKAGE DEPENDENCY INSTALLER\n")
cat("=================================================================\n")
cat("Checking and installing required R packages...\n\n")

# Required CRAN packages (78 total packages identified)
cran_packages <- c(
  # Core Data Science & Tidyverse
  "tidyverse",       # Meta-package including dplyr, ggplot2, tidyr, etc.
  "dplyr",          # Data manipulation
  "ggplot2",        # Data visualization
  "tidyr",          # Data tidying
  "readr",          # Reading rectangular data
  "purrr",          # Functional programming tools
  "lubridate",      # Date/time manipulation
  "magrittr",       # Pipe operators
  
  # Statistical Analysis & Modeling
  "vegan",          # Community ecology package
  "cluster",        # Cluster analysis
  "factoextra",     # Extract and visualize multivariate analysis results
  "Hmisc",          # Harrell miscellaneous functions
  "e1071",          # Statistical functions
  "mvabund",        # Statistical methods for multivariate abundance data
  "missMDA",        # Missing values imputation using multivariate data analysis
  "minpack.lm",     # Non-linear least squares fitting
  "boot",           # Bootstrap functions and datasets
  "nlme",           # Nonlinear mixed-effects models
  
  # Visualization & Graphics
  "ggpubr",         # Publication-ready plots
  "ggthemes",       # Additional themes for ggplot2
  "ggcorrplot",     # Correlation plot visualization
  "viridis",        # Color scales
  "RColorBrewer",   # Color schemes
  "ggsci",          # Scientific journal color palettes
  "ggforce",        # Additional geoms for ggplot2
  "ggstatsplot",    # Statistical plots with details
  "ggHoriPlot",     # Horizon plots
  "scales",         # Scale functions for visualization
  "plotly",         # Interactive plots
  "colorspace",     # Color space manipulation
  "cowplot",        # Streamlined plot theme and plot annotations
  "gridExtra",      # Grid-based plotting
  "GGally",         # Extension to ggplot2
  "scatterPlotMatrix", # Scatter plot matrices
  
  # Spatial Analysis
  "sf",             # Simple features for spatial data
  "rnaturalearth",  # Natural Earth map data
  "rnaturalearthdata", # Natural Earth map data
  "rnaturalearthhires", # High resolution Natural Earth data
  "geodist",        # Geographic distance calculations
  "ggspatial",      # Spatial data visualization (replaces ggsn)
  "spdep",          # Spatial dependence analysis
  "maps",           # Geographical maps
  "ggmap",          # Spatial visualization with ggplot2
  
  # Biodiversity Analysis
  "betapart",       # Partitioning beta diversity
  "BiodiversityR",  # Biodiversity analysis
  "BAT",            # Biodiversity assessment tools
  "iNEXT",          # Interpolation and extrapolation for species diversity
  "coenocliner",    # Simulating ecological data
  
  # Climate & Environmental Analysis
  "heatwaveR",      # Marine heatwave detection
  "tidync",         # Working with NetCDF files
  "rerddap",        # ERDDAP data access
  
  # Data Processing & Management
  "plyr",           # Tools for splitting, applying, and combining data
  "data.table",     # Enhanced data.frame
  "zoo",            # Time series infrastructure
  "here",           # File path construction
  "countrycode",    # Country code conversion
  
  # Web Scraping & Data Import
  "rvest",          # Web scraping
  "readxl",         # Reading Excel files
  
  # Statistical Testing & Analysis
  "beeswarm",       # Bee swarm plots
  "rcompanion",     # Functions for statistical analysis
  "skimr",          # Compact data summaries
  "jmv",            # Statistical analyses (jamovi)
  "summarytools",   # Data summarization tools
  "datasauRus",     # Datasets for teaching statistics
  "Rmisc",          # Statistical functions and utilities
  
  # Reporting & Documentation
  "knitr",          # Dynamic report generation
  "kableExtra",     # Enhanced table formatting
  "palmerpenguins", # Palmer penguins dataset
  
  # Development & Installation Tools
  "devtools",       # Development tools
  "doParallel",     # Parallel processing support
  
  # Legacy/Utility packages
  "gdata"           # Data manipulation utilities
)

# GitHub packages that need special installation
github_packages <- list(
  # Add any GitHub packages here if discovered
  # Format: list(repo = "username/repository", package_name = "package")
)

# Function to check if a package is installed
is_installed <- function(package) {
  return(package %in% rownames(installed.packages()))
}

# Function to install a package safely
install_package <- function(package, from_cran = TRUE, repo = NULL) {
  if (is_installed(package)) {
    cat(sprintf("✓ %s is already installed\n", package))
    return(TRUE)
  }
  
  cat(sprintf("⧗ Installing %s...", package))
  
  tryCatch({
    if (from_cran) {
      install.packages(package, dependencies = TRUE, quiet = TRUE)
    } else {
      if (!is_installed("devtools")) {
        install.packages("devtools", quiet = TRUE)
      }
      devtools::install_github(repo, quiet = TRUE)
    }
    
    if (is_installed(package)) {
      cat(" ✓ SUCCESS\n")
      return(TRUE)
    } else {
      cat(" ✗ FAILED\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat(sprintf(" ✗ ERROR: %s\n", e$message))
    return(FALSE)
  })
}

# Install CRAN packages
cat("INSTALLING CRAN PACKAGES\n")
cat("========================\n")

failed_packages <- character(0)
successful_packages <- character(0)

for (package in cran_packages) {
  result <- install_package(package, from_cran = TRUE)
  if (result) {
    successful_packages <- c(successful_packages, package)
  } else {
    failed_packages <- c(failed_packages, package)
  }
}

# Install GitHub packages (if any)
if (length(github_packages) > 0) {
  cat("\nINSTALLING GITHUB PACKAGES\n")
  cat("==========================\n")
  
  for (pkg_info in github_packages) {
    result <- install_package(pkg_info$package_name, from_cran = FALSE, repo = pkg_info$repo)
    if (result) {
      successful_packages <- c(successful_packages, pkg_info$package_name)
    } else {
      failed_packages <- c(failed_packages, pkg_info$package_name)
    }
  }
}

# Summary report
cat("\n=================================================================\n")
cat("INSTALLATION SUMMARY\n")
cat("=================================================================\n")
cat(sprintf("Total packages processed: %d\n", length(cran_packages) + length(github_packages)))
cat(sprintf("Successfully installed/verified: %d\n", length(successful_packages)))
cat(sprintf("Failed installations: %d\n", length(failed_packages)))

if (length(failed_packages) > 0) {
  cat("\nFAILED PACKAGES:\n")
  for (pkg in failed_packages) {
    cat(sprintf("  ✗ %s\n", pkg))
  }
  cat("\nTry installing these manually:\n")
  cat("install.packages(c(")
  cat(paste(sprintf('"%s"', failed_packages), collapse = ", "))
  cat("))\n")
} else {
  cat("\n🎉 ALL PACKAGES SUCCESSFULLY INSTALLED!\n")
}

cat("\nNOTES:\n")
cat("------\n")
cat("• Some packages are meta-packages (like tidyverse) that install multiple packages\n")
cat("• Spatial packages may require system dependencies (GDAL, GEOS, PROJ)\n")
cat("• If you encounter issues, try updating R and running again\n")
cat("• For Ubuntu/Debian: sudo apt-get install libgdal-dev libproj-dev libgeos-dev\n")
cat("• For macOS: Consider using Homebrew for spatial dependencies\n")

# Check R version compatibility
r_version <- R.version.string
cat(sprintf("\nR Version: %s\n", r_version))

if (R.version$major >= "4") {
  cat("✓ R version is compatible with modern packages\n")
} else {
  cat("⚠ Consider updating R to version 4.0+ for best compatibility\n")
}

cat("\n=================================================================\n")
cat("INSTALLATION COMPLETE\n")
cat("=================================================================\n")

# Optional: Load key packages to verify they work
cat("\nTesting key package loading...\n")
test_packages <- c("tidyverse", "vegan", "ggplot2", "sf")

for (pkg in test_packages) {
  if (is_installed(pkg)) {
    tryCatch({
      library(pkg, character.only = TRUE, quietly = TRUE)
      cat(sprintf("✓ %s loads successfully\n", pkg))
    }, error = function(e) {
      cat(sprintf("✗ %s failed to load: %s\n", pkg, e$message))
    })
  }
}

cat("\nReady to render the Tangled Bank website! 🌱\n")