# Tangled Bank Project - R Dependencies

# This script checks for and installs all required R packages for the Tangled Bank project.
# Running this script will ensure that the environment is correctly set up for reproducibility.

# List of required packages
required_packages <- c(
  "akima", "astrochron", "BAT", "betapart", "BiodiversityR", "boot", "broom", 
  "car", "cmocean", "cluster", "coenocliner", "colorspace", "corrplot", 
  "countrycode", "data.table", "datasauRus", "doParallel", "dplyr", "e1071", 
  "elevatr", "factoextra", "forecast", "geodist", "gganimate", "ggcorrplot", 
  "ggfortify", "ggHoriPlot", "ggplot2", "ggpubr", "ggsci", "ggspatial", 
  "ggthemes", "glmnet", "grid", "gridBase", "gridExtra", "gt", "heatwaveR", 
  "Hmisc", "iNEXT", "jmv", "kableExtra", "knitr", "lme4", "lmerTest", 
  "lmtest", "lubridate", "lwgeom", "MASS", "metR", "mgcv", "minpack.lm", 
  "missMDA", "mvabund", "ncdf4", "nlme", "palmerpenguins", "palr", 
  "patchwork", "plotly", "plyr", "quantreg", "RANN", "raster", "RColorBrewer", 
  "rerddap", "reticulate", "rnaturalearth", "rnaturalearthdata", 
  "rnaturalearthhires", "rvest", "scales", "sf", "skimr", "spdep", "stars", 
  "summarytools", "TidyDensity", "tidync", "tidyverse", "vegan", "viridis", 
  "WaveletComp", "zoo"
)

# Function to check and install packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Iterate over the list and install missing packages
sapply(required_packages, install_if_missing)

cat("All required packages are installed and loaded.\n")

