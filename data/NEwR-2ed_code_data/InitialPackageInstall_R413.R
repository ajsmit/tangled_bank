# ============================================================
# Script for users of "Numerical Ecology with R"             #
# by Daniel Borcard, Francois Gillet and Pierre Legendre     #
# Adapted to R 4.1.3 on 2022-03-19                           #
# ============================================================

# This script installs or provides guidelines to install all
# the packages necessary to run the code provided in the book,
# but that do not belong to the standard R distribution (steps 1 and 3).

# Step 1 must be run only once when installing or upgrading R if
# the packages needed for the exercises have not yet been installed.
#
# Step 2 replaces step 1 when upgrading R and the packages are already installed.
#
# Step 3 must be executed when installing the book material for the first time. Later, 
# when upgrading R, verify if the packages still work. If not, install them anew.
# 
# Step 4 is not mandatory.

# 1. Install packages from the main CRAN site
#    ----------------------------------------

install.packages(
  c(
    "sf",
    "ade4",
    "adegraphics",
    "adespatial",
    "agricolae",
    "ape",
    "cluster",
    "cocorresp",
    "colorspace",
    "dendextend",
    "ellipse",
    "FactoMineR",
    "FD",
    "gclus",
    "ggplot2",
    "googleVis",
    "igraph",
    "indicspecies",
    "labdsv",
    "MASS",
    "missMDA",
    "picante",
    "pvclust",
    "RColorBrewer",
    "rgexf",
    "RgoogleMaps",
    "rioja",
    "rrcov",
    "SoDA",
    "spdep",
    "taxize",
    "vegan",
    "vegan3d",
    "vegclust"
  ),
  dependencies = TRUE,
  type = "both"
)


# 2. Update installed packages
#    -------------------------
# This replaces step 1 if the packages have already been installed with an older
# version of R
update.packages(checkBuilt = TRUE, ask = FALSE)


# 3. Install packages that are no longer available from CRAN as binaries. 
#    This step must be executed when installing the book material for the first time.  
#    Later, when upgrading R, verify if the packages still work. If not, 
#    install them anew.

# Install mvpart and MVPARTwrap that are no longer available from CRAN:
# On Windows machines, Rtools (4.0 and above) must be installed first. Go to:
# https://cran.r-project.org/bin/windows/Rtools/
# After that:
install.packages("devtools")
library(devtools)
install_github("cran/mvpart", force = TRUE)
install_github("cran/MVPARTwrap", force = TRUE)

# Install package vegetarian v.1.2, which is no longer directly available from CRAN 
# as a binary file. The source file can be downloaded from this address:
# https://cran.r-project.org/src/contrib/Archive/vegetarian/vegetarian_1.2.tar.gz
# Place the file in your working directory, then type:
#    install.packages("vegetarian_1.2.tar.gz", repos = NULL, type = "source")
# If the source file has been placed elsewhere you must provide the full path, or type
#    install.packages(file.choose(), repos = NULL, type = "source")
# and find the "vegetarian_1.2.tar.gz" file wherever it is on your computer.


# 4. OPTIONAL (for power users): Install all R packages from
#    Environmetrics, a CRAN Task View for the Analysis of Ecological
#    and Environmental Data
#    See http://cran.r-project.org/web/views/Environmetrics.html
#    --------------------------------------------------------------------

install.packages("ctv")
library(ctv)
update.views("Environmetrics")

# Other potentially useful CRAN Task Views...
update.views("Cluster")
update.views("Multivariate")
update.views("Spatial")
update.views("MachineLearning")

# ========