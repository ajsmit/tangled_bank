# This file lists the R package dependencies for the Tangled Bank project.
# To install all dependencies, run this script in R:
# source("dependencies.R")

# List of required packages
required_packages <- c(
    "AICcmodavg", "betapart", "boot", "broom", "car", "cluster", "coin", 
    "colorspace", "corrplot", "countrycode", "cowplot", "data.table", 
    "datasauRus", "dplyr", "e1071", "factoextra", "fitdistrplus", "geosphere", 
    "GGally", "ggcorrplot", "ggforce", "ggfortify", "ggHoriPlot", "ggplot2", 
    "ggpubr", "ggrepel", "ggsci", "ggspatial", "ggthemes", "glmnet", 
    "grid", "gridBase", "gridExtra", "gt", "Hmisc", "jmv", "kableExtra", 
    "lme4", "lmerTest", "lmtest", "lubridate", "MASS", "mgcv", "missMDA", 
    "mvabund", "nlme", "ncdf4", "nycflights13", "palmerpenguins", "patchwork", 
    "plyr", "purrr", "quantreg", "rcompanion", "RColorBrewer", "rnaturalearth", 
    "rnaturalearthdata", "rnaturalearthhires", "rvest", "scales", 
    "scatterPlotMatrix", "sf", "skimr", "spdep", "summarytools", 
    "TidyDensity", "tidyr", "tidyverse", "vegan", "viridis", "zoo"
)

# Function to install packages only if they are not already installed
install_if_missing <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
    }
}

# Apply the function to all required packages
sapply(required_packages, install_if_missing)

cat("All dependencies checked and installed.\n")
