# Module: BDC334
# Aim: To demonstrate the distance decay relationship and serial beta-diversity
# Author: AJ Smit
# Date: 5 August 2025

# Load the libraries
library(vegan)
library(ggplot2)
library(geodist) # to calculate geographic distances between lats/lons
library(ggpubr) # to arrange the multipanel graphs

# Read in the Doubs env data
env <- read.csv("BDC334/BDC334_data/DoubsEnv.csv")

# Standarise the data
env <- decostand(env[, 2:ncol(env)], method = "standardize", MARGIN = 2)

# Calculate the distance matrix
env_dist <- round(vegdist(env, diag = TRUE, method = "euclidean"), 2)

# convert to a full n×n matrix
mat <- as.matrix(env_dist)

# Beta-diversity: Plot the distance decay relationship
# Extract the first column of the distance matrix
dist_decay <- data.frame(
  x = 1:30,
  y = mat[1:30, 1],
  label = paste0("Site ", 1:30)
)

pltA <- ggplot(data = dist_decay, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Site number", y = "Environmental distance") +
  theme_minimal() +
  ggtitle("Distance Decay of Environmental Similarity") +
  theme(plot.title = element_text(hjust = 0.5))

# Beta-diversity: serial beta-diversity
# Extract the subdiagonal entries of the distance matrix
# number of sites (or samples)
n <- nrow(mat)

# extract the sub-diagonal entries: positions (2,1), (3,2), …, (n,n-1)
subdiag <- mat[cbind(2:n, 1:(n - 1))]

dist_serial <- data.frame(
  x = 1:(n - 1),
  y = subdiag,
  label = paste0("Site ", 2:n)
)

pltB <- ggplot(data = dist_serial, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Site number", y = "Environmental distance") +
  theme_minimal() +
  ggtitle("Serial Beta-Diversity of Environmental Similarity") +
  theme(plot.title = element_text(hjust = 0.5))

# Combine the two plots into a single figure
ggarrange(pltA, pltB, ncol = 1, nrow = 2)
