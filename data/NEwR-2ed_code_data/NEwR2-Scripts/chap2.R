### CHAPTER 2: EXPLORATORY DATA ANALYSIS
###
### Online supporting material for: 
### 
### Borcard D., Gillet F. & Legendre P. 2018. Numerical Ecology with R,
### 2nd edition. Springer International Publishing AG. 
###
### Borcard D., Gillet F. & Legendre P. 2020. Numerical ecology with R, 
### 2nd Chinese edition. (Translation: J. Lai, Institute of Botany, 
### Chinese Academy of Sciences). Higher Education Press, Beijing. 

# Load packages, functions and data ===============================
library(vegan)
library(leaflet)
library(googleVis)
library(labdsv)

# Source additional functions that will be used later in this
# Chapter. Our scripts assume that files to be read are in
# the working directory.
source("panelutils.R")


# Load the data. File Doubs.Rdata is assumed to be in the 
# working directory
load("Doubs.RData")  
# The file Doubs.RData contains the following objects:
#    spe: species (community) data frame (fish abundances)
#    env: environmental data frame
#    spa: spatial data frame – cartesian coordinates
#    fishtraits: functional traits of fish species
#    latlong: spatial data frame – latitude and longitude

# -----------------------------------------------------------------
# IMPORTANT NOTE
# We assume in our R scripts that you use the .RData files provided 
# here and not the original dataset extracted from ade4. 
# -----------------------------------------------------------------


# Exploration of a data frame using basic R functions =============

spe                       # Display the whole data frame in the 
                          # console
                          # Not recommended for large datasets!
spe[1:5, 1:10]            # Display only 5 lines and 10 columns
head(spe)                 # Display only the first 6 lines
tail(spe)                 # Display only the last 6 rows
nrow(spe)                 # Number of rows (sites)
ncol(spe)                 # Number of columns (species)
dim(spe)                  # Dimensions of the data frame (rows, 
                          # columns)
colnames(spe)             # Column labels (descriptors = species)
rownames(spe)             # Row labels (objects = sites)
summary(spe)              # Descriptive statistics for columns


# Overall distribution of abundances (dominance codes) ============

# Minimum and maximum of abundance values in the whole data set
range(spe)
# Minimum and maximum value for each species
apply(spe, 2, range)
# Count the cases for each abundance class
(ab <- table(unlist(spe)))
# Create a graphic window with title
dev.new(title = "Distribution of abundance classes", 
  noRStudioGD = TRUE
)
# Barplot of the distribution, all species confounded
barplot(ab, 
  las = 1,
  xlab = "Abundance class",
  ylab = "Frequency",
  col = gray(5 : 0 / 5)
)
# Number of absences
sum(spe == 0)
# Proportion of zeros in the community data set
sum(spe == 0) / (nrow(spe) * ncol(spe))


# Map of the locations of the sites ===============================

# New graphic window 
dev.new(title = "Site Locations", width = 9, noRStudioGD = TRUE)
# Create an empty frame (proportional axes 1:1, with titles)
# Geographic coordinates x and y from the spa data frame
plot(spa, 
  asp = 1, 
  type = "n", 
  main = "Site Locations", 
  xlab = "x coordinate (km)", 
  ylab = "y coordinate (km)"
)
# Add a blue line connecting the sites along the Doubs River
lines(spa, col = "light blue")
# Add the site labels
text(spa, row.names(spa), cex = 1, col = "red")
# Add text blocks 
text(68, 20, "Upstream", cex = 1.2, col = "red")
text(15, 35, "Downstream", cex = 1.2, col = "red")

# Sites projected onto a Google Maps(R) background

# 1. Using googleVis - dynamic map

# By default the plot method of the googleVis package uses 
# the standard browser to display its output.

nom <- latlong$Site
latlong2 <- paste(latlong$LatitudeN, latlong$LongitudeE, sep = ":")
df <- data.frame(latlong2, nom, stringsAsFactors = FALSE)

mymap1 <- gvisMap(df, 
  locationvar = "latlong2", 
  tipvar = "nom", 
  options = list(showTip = TRUE)
)

plot(mymap1)


# 2. NOT IN THE BOOK - Using leaflet with RStudio

# Using leaflet - dynamic map
# Sites projected onto an Open Street Map background
# leaflet uses the viewer panel to display its output.
# Works when called from RStudio but no longer from the 
# R console.

longitude <- latlong$LongitudeE
latitude <- latlong$LatitudeN
site <- as.character(latlong$Site)
background <- addTiles(leaflet())
mymap1 <-
  addMarkers(
    background,
    lng = longitude,
    lat = latitude,
    label = site,
    labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE)
  )
mymap1

# End of NOT IN THE BOOK - Using leaflet


# Maps of some fish species =======================================

# New graphic window (size 9x9 inches)
dev.new(title = "Species Locations", 
  width = 11, 
  height = 8,
  noRStudioGD = TRUE
)
# Divide the plot window into 4 frames, 2 per row
par(mfrow = c(2,2))
# Plot four species
plot(spa, 
  asp = 1, 
  cex.axis = 0.8, 
  col = "brown", 
  cex = spe$Satr, 
  main = "Brown trout", 
  xlab = "x coordinate (km)", 
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(spa, 
  asp = 1, 
  cex.axis = 0.8, 
  col = "brown", 
  cex = spe$Thth, 
  main = "Grayling", 
  xlab = "x coordinate (km)", 
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(spa, 
  asp = 1, 
  cex.axis = 0.8, 
  col = "brown", 
  cex = spe$Baba, 
  main = "Barbel", 
  xlab = "x coordinate (km)", 
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(spa, 
  asp = 1, 
  cex.axis = 0.8, 
  col = "brown", 
  cex = spe$Abbr, 
  main = "Common bream", 
  xlab = "x coordinate (km)", 
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")


# Compare species: number of occurrences ==========================

# Compute the number of sites where each species is present
# To sum by columns, the second argument of apply(), MARGIN, 
# is set to 2
spe.pres <- apply(spe > 0, 2, sum)
# Sort the results in increasing order
sort(spe.pres)
# Compute percentage frequencies
spe.relf <- 100 * spe.pres/nrow(spe)
# Round the sorted output to 1 digit
round(sort(spe.relf), 1)
# Plot the histograms
dev.new(title = "Frequency Histograms", 
  width = 8, 
  height = 5,
  noRStudioGD = TRUE
)
# Divide the window horizontally
par(mfrow = c(1,2))
hist(spe.pres, 
  main = "Species Occurrences", 
  right = FALSE, 
  las = 1, 
  xlab = "Number of occurrences", 
  ylab = "Number of species", 
  breaks = seq(0, 30, by = 5),
  col = "bisque"
)
hist(spe.relf, 
  main = "Species Relative Frequencies", 
  right = FALSE, 
  las = 1,
  xlab = "Frequency of occurrences (%)", 
  ylab = "Number of species",
  breaks = seq(0, 100, by = 10),
  col = "bisque"
)


# Compare sites: species richness =================================

# Compute the number of species at each site
# To sum by rows, the second argument of apply(), MARGIN, is set 
# to 1
sit.pres <- apply(spe > 0, 1, sum)
# Sort the results in increasing order
sort(sit.pres)
dev.new(title = "Species Richness", 
  width = 12, 
  height = 5,
  noRStudioGD = TRUE)
par(mfrow = c(1, 2))
# Plot species richness vs. position of the sites along the river
plot(sit.pres,type = "s",
  las = 1, 
  col = "gray",
  main = "Species Richness vs. \n Upstream-Downstream Gradient",
  xlab = "Site numbers", 
  ylab = "Species richness"
)
text(sit.pres, row.names(spe), cex = .8, col = "red")
# Use geographic coordinates to plot a bubble map
plot(spa, 
  asp = 1, 
  main = "Map of Species Richness", 
  pch = 21, 
  col = "white", 
  bg = "brown", 
  cex = 5 * sit.pres / max(sit.pres), 
  xlab = "x coordinate (km)", 
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")


# Transformation and standardization of the species data ==========

# Get help on the decostand() function
?decostand

## Simple transformations

# Partial view of the raw data (abundance codes)
spe[1:5, 2:4]
# Transform abundances to presence-absence (1-0)
spe.pa <- decostand(spe, method = "pa")
spe.pa[1:5, 2:4]


## Standardization by columns (species)

# Scale abundances by dividing them by the maximum value of each 
# species
# Note: MARGIN = 2 (column, default value) for argument "max"
spe.scal <- decostand(spe, "max")
spe.scal[1:5, 2:4]
# Display the maximum in each transformed column
apply(spe.scal, 2, max)

# Scale abundances by dividing them by the species totals
# (relative abundance by species)
# Note: here, override the default MARGIN = 1 argument of "total"
spe.relsp <- decostand(spe, "total", MARGIN = 2)
spe.relsp[1:5, 2:4]
# Display the sum by column
# Classical: apply(spe.relsp, 2, sum)
colSums(spe.relsp)

## Standardization by rows (sites)

# Scale abundances by dividing them by the site totals
# (profiles of relative abundance by site)
spe.rel <- decostand(spe, "total") # default MARGIN = 1
spe.rel[1:5, 2:4]
# Display the sum of row vectors to determine if the scaling worked
# properly
# Classical: apply(spe.rel, 1, sum)
rowSums(spe.rel)

# Give a length (norm) of 1 to each row vector
# This is called the chord transformation
spe.norm <- decostand(spe, "normalize") # default MARGIN = 1
spe.norm[1 : 5, 2 : 4]
# Verify the norm of the transformed row vectors
# Write a 1-line function that computes the norm of vector x
vec.norm <- function(x) sqrt(sum(x ^ 2))
# Then, apply that function to the rows of matrix spe.norm
apply(spe.norm, 1, vec.norm)

# Compute square root of relative abundances per site
# This is called the Hellinger transformation
spe.hel <- decostand(spe, "hellinger")
spe.hel[1:5, 2:4]
# Check the norm of row vectors
apply(spe.hel, 1, vec.norm)


## Double standardization by columns and rows

# Chi-square transformation
spe.chi <- decostand(spe, "chi.square")
spe.chi[1:5, 2:4]
# Check what happened to site 8 where no species was found
spe.chi[7:9, ]
# Note: decostand produced values of 0 for 0/0 instead of NaN 

# Wisconsin standardization
# Abundances are first ranged by species maxima and then by site 
# totals
spe.wis <- wisconsin(spe)
spe.wis[1:5, 2:4]


## Boxplots of transformed abundances of a common species
# (the stone loach, species #4)
dev.new(title = "Loach", noRStudioGD = TRUE)
par(mfrow = c(2,2))
boxplot(spe$Babl,
  sqrt(spe$Babl), 
  log1p(spe$Babl),
  las = 1, 
  main = "Simple transformations",
  names = c("raw data", "sqrt", "log"), 
  col = "bisque"
)
boxplot(spe.scal$Babl, 
  spe.relsp$Babl,
  las = 1, 
  main = "Standardizations by species",
  names = c("max", "total"), 
  col = "lightgreen"
)
boxplot(spe.hel$Babl, 
  spe.rel$Babl, 
  spe.norm$Babl,
  las = 1, 
  main = "Standardizations by sites",
  names = c("Hellinger", "total", "norm"), 
  col = "lightblue"
)
boxplot(spe.chi$Babl, 
  spe.wis$Babl,
  las = 1, 
  main = "Double standardizations",
  names = c("Chi-square", "Wisconsin"), 
  col = "orange"
)

## Plot raw and transformed abundances along the upstream-
## downstream river gradient
dev.new(
  title = "Species transformed abundances", 
  width = 9, 
  height = 9,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
plot(env$dfs, 
  spe$Satr, 
  type = "l", 
  col = 4, 
  main = "Raw data",
  xlab = "Distance from the source [km]", 
  ylab = "Raw abundance code"
)
lines(env$dfs, spe$Thth, col = 3)
lines(env$dfs, spe$Baba, col = "orange")
lines(env$dfs, spe$Abbr, col = 2)
lines(env$dfs, spe$Babl, col = 1, lty = "dotted")

plot(env$dfs, 
  spe.scal$Satr, 
  type = "l", 
  col = 4, 
  main = "Species abundances ranged 
  by maximum",
  xlab = "Distance from the source [km]", 
  ylab = "Ranged abundance"
)
lines(env$dfs, spe.scal$Thth, col = 3)
lines(env$dfs, spe.scal$Baba, col = "orange")
lines(env$dfs, spe.scal$Abbr, col = 2)
lines(env$dfs, spe.scal$Babl, col = 1, lty = "dotted")

plot(env$dfs, 
  spe.hel$Satr, 
  type = "l", 
  col = 4, 
  main =  "Hellinger-transformed abundances", 
  xlab = "Distance from the source [km]", 
  ylab = "Standardized abundance"
)
lines(env$dfs, spe.hel$Thth, col = 3)
lines(env$dfs, spe.hel$Baba, col = "orange")
lines(env$dfs, spe.hel$Abbr, col = 2)
lines(env$dfs, spe.hel$Babl, col = 1, lty = "dotted")

plot(env$dfs, 
  spe.chi$Satr, 
  type = "l", 
  col = 4, 
  main = "Chi-square-transformed abundances", 
  xlab = "Distance from the source [km]", 
  ylab = "Standardized abundance"
)
lines(env$dfs, spe.chi$Thth, col = 3)
lines(env$dfs, spe.chi$Baba, col = "orange")
lines(env$dfs, spe.chi$Abbr, col = 2)
lines(env$dfs, spe.chi$Babl, col = 1, lty = "dotted")
legend("topright", 
  c("Brown trout", "Grayling", "Barbel", "Common bream", 
  "Stone loach"),
  col = c(4, 3, "orange", 2, 1), 
  lty = c(rep(1, 4), 3)
)

## Conversion of fish abundances using an arbitrary scale
current <- c(0, 1, 2, 3, 4, 5)
converted <- c(0, 1 ,5, 10, 20, 50)
spe.conv <- abundtrans(spe, current, converted)


# Bubble maps of some environmental variables =====================

dev.new(
  title = "Bubble maps", 
  width = 11, 
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
plot(spa, 
  asp = 1, 
  cex.axis = 0.8, 
  main = "Elevation", 
  pch = 21, 
  col = "white", 
  bg = "red", 
  cex = 5 * env$ele / max(env$ele), 
  xlab = "x", 
  ylab = "y"
)
lines(spa, col = "light blue")
plot(spa, 
  asp = 1, 
  cex.axis = 0.8, 
  main = "Discharge", 
  pch = 21, 
  col = "white", 
  bg = "blue",
  cex = 5 * env$dis / max(env$dis),
  xlab = "x", 
  ylab = "y"
)
lines(spa, col = "light blue")
plot(spa, 
  asp = 1, 
  cex.axis = 0.8, 
  main = "Oxygen", 
  pch = 21, 
  col = "white", 
  bg = "green3",
  cex = 5 * env$oxy / max(env$oxy),
  xlab =  "x", 
  ylab = "y"
)
lines(spa, col = "light blue")
plot(spa, 
  asp = 1,
  cex.axis = 0.8, 
  main = "Nitrate", 
  pch = 21,
  col = "white", 
  bg = "brown",
  cex = 5 * env$nit / max(env$nit),
  xlab = "x", 
  ylab = "y"
)
lines(spa, col = "light blue")


# Line plots ======================================================

dev.new(title = "Descriptor line plots", noRStudioGD = TRUE)
par(mfrow = c(2, 2))
plot(env$dfs, env$ele, 
  type = "l", 
  xlab = "Distance from the source (km)", 
  ylab = "Elevation (m)", 
  col = "red", main = "Elevation"
)
plot(env$dfs, env$dis, 
  type = "l", 
  xlab = "Distance from the source (km)", 
  ylab = "Discharge (m3/s)", 
  col = "blue", 
  main = "Discharge"
)
plot(env$dfs, env$oxy, 
  type = "l", 
  xlab = "Distance from the source (km)", 
  ylab = "Oxygen (mg/L)", 
  col = "green3", 
  main = "Oxygen"
)
plot(env$dfs, env$nit, 
  type = "l", 
  xlab = "Distance from the source (km)", 
  ylab = "Nitrate (mg/L)", 
  col = "brown", 
  main = "Nitrate"
)


# Scatter plots for all pairs of environmental variables ==========

# Bivariate plots with histograms on the diagonal and smooth 
# fitted curves
dev.new(
  title = "Bivariate descriptor plots",
  width = 10,
  height = 10,
  noRStudioGD = TRUE
)
pairs(env, 
  panel = panel.smooth, 
  diag.panel = panel.hist,
  main = "Bivariate Plots with Histograms and Smooth Curves"
)


# Simple transformation of an environmental variable ==============

range(env$slo)
# Log-transformation of the slope variable (y = ln(x))
# Compare histograms and boxplots of the raw and transformed values
dev.new(title = "Transformation and standardization of variable 
  slope", 
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
hist(env$slo, 
  col = "bisque", 
  right = FALSE
)
hist(log(env$slo), 
  col = "light green", 
  right = FALSE, 
  main = "Histogram of ln(env$slo)"
)
boxplot(env$slo, 
  col = "bisque", 
  main = "Boxplot of env$slo", 
  ylab = "env$slo"
)
boxplot(log(env$slo), 
  col = "light green", 
  main = "Boxplot of ln(env$slo)",
  ylab = "log(env$slo)"
)


# Standardization of all environmental variables ==================

# Center and scale = standardize the variables (z-scores)
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean)	# means = 0
apply(env.z, 2, sd)	# standard deviations = 1

# Same standardization using the scale() function (which returns 
# a matrix)
env.z <- as.data.frame(scale(env))
