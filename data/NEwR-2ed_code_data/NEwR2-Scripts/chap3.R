### CHAPTER 3: ASSOCIATION MEASURES
###
### Borcard D., Gillet F. & Legendre P. 2018. Numerical Ecology with R,
### 2nd edition. Springer International Publishing AG. 
###
### Borcard D., Gillet F. & Legendre P. 2020. Numerical ecology with R, 
### 2nd Chinese edition. (Translation: J. Lai, Institute of Botany, 
### Chinese Academy of Sciences). Higher Education Press, Beijing. 

# Load packages, functions and data ===============================
library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(FD)

# Source additional functions that will be used later in this
# Chapter. Our scripts assume that files to be read are in
# the working directory.
source("coldiss.R")
source("panelutils.R")

# Load the data
# File Doubs.Rdata is assumed to be in the working directory
load("Doubs.Rdata")
# Remove empty site 8
spe <- spe[-8, ]
env <- env[-8, ]
spa <- spa[-8, ]


### Q-mode dissimilarity matrices

## Q-mode dissimilarity and distance measures for
## (semi-)quantitative data

# Percentage difference (aka Bray-Curtis) dissimilarity matrix
# on raw species data. ".db" means "distance Bray".
spe.db <- vegdist(spe)	# method = "bray" (default)
spe.db[1:6]
# Percentage difference (aka Bray-Curtis) dissimilarity matrix
# on log-transformed abundances
spe.dbln <- vegdist(log1p(spe))
spe.dbln[1:6]  # Function head() no longer accepts 'dist' objects
# Chord distance matrix
spe.dc <- dist.ldc(spe, "chord")
   # Alternate, two-step computation in vegan:
   spe.norm <- decostand(spe, "nor")
   spe.dc <- dist(spe.norm)
spe.dc[1:6]
# Hellinger distance matrix
spe.dh <- dist.ldc(spe) # Hellinger is the default distance
   # Alternate, two-step computation in vegan:
   spe.hel <- decostand(spe, "hel")
   spe.dh <- dist(spe.hel)
spe.dh[1:6]
# Log-chord distance matrix
spe.logchord <- dist.ldc(spe, "log.chord")
   # Alternate, three-step computation in vegan:
   spe.ln <- log1p(spe)
   spe.ln.norm <- decostand(spe.ln, "nor")
   spe.logchord <- dist(spe.ln.norm)
spe.logchord[1:6]

## Q-mode dissimilarity measures for binary data

# Jaccard dissimilarity matrix using function vegdist()
spe.dj <- vegdist(spe, "jac", binary = TRUE)
spe.dj[1:6]
sqrt(spe.dj)[1:6]
# Jaccard dissimilarity matrix using function dist()
spe.dj2 <- dist(spe, "binary")
spe.dj2[1:6]
# Jaccard dissimilarity matrix using function dist.binary()
spe.dj3 <- dist.binary(spe, method = 1)
spe.dj3[1:6]
# Sorensen dissimilarity matrix using function dist.ldc()
spe.ds <- dist.ldc(spe, "sorensen")
# Sorensen dissimilarity matrix using function vegdist()
spe.ds2 <- vegdist(spe, method = "bray", binary = TRUE)
# Sorensen dissimilarity matrix using function dist.binary()
spe.ds3 <- dist.binary(spe, method = 5)
spe.ds[1:6]
spe.ds2[1:6]
sqrt(spe.ds2)[1:6]
spe.ds3[1:6]
# Ochiai dissimilarity matrix
spe.och <- dist.ldc(spe, "ochiai")   # or
spe.och <- dist.binary(spe, method = 7)
spe.och[1:6]


## Graphical display of association matrices

# Colour plots (also called heat maps, or trellis diagrams in the
# data analysis literature) using the coldiss() function

# Usage:
# coldiss(D = dissimilarity.matrix,
#         nc = 4,
#         byrank = TRUE,
#         diag = FALSE)
# If D is not a dissimilarity matrix (max(D) > 1), then D is
# divided by max(D)
# nc number of colours (classes)
# byrank =  TRUE	equal-sized classes
# byrank =  FALSE	equal-length intervals
# diag = TRUE	print object labels also on the diagonal

## Compare dissimilarity and distance matrices obtained from the
## species data. Four colours are used with equal-length intervals

# Percentage difference (aka Bray-Curtis) dissimilarity matrix on
# raw species abundance data
dev.new(
  title = "Percentage difference (Bray-Curtis), raw data",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.db, byrank = FALSE, diag = TRUE)

# Same but on log-transformed data
dev.new(
  title = "Percentage difference (Bray-Curtis), ln(y+1) data",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.dbln, byrank = FALSE, diag = TRUE)

# Chord distance matrix
dev.new(
  title = "Chord",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.dc, byrank = FALSE, diag = TRUE)

# Hellinger distance matrix
dev.new(
  title = "Hellinger",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.dh, byrank = FALSE, diag = TRUE)

# log-chord distance matrix
dev.new(
  title = "Log-chord",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.logchord, byrank = FALSE, diag = TRUE)

# Jaccard distance matrix
dev.new(
  title = "Jaccard",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.dj, byrank = FALSE, diag = TRUE)

# Simple matching dissimilarity
# (called the Sokal and Michener index in ade4)
spe.s1 <- dist.binary(spe, method = 2)
dev.new(
  title = "S1 on species data",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.s1 ^ 2, byrank = FALSE, diag = TRUE)


## Compare distance matrices from environmental, species and
## spatial data. 16 colours with equal-size classes

# Remove the 'dfs' variable from the env dataset
env2 <- env[, -1]

# Euclidean distance matrix of the standardized env2 data frame
env.de <- dist(scale(env2))
dev.new(
  title = "Environment",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(env.de, nc = 16, diag = TRUE)

# Hellinger distance matrix of the species data
# Use nc = 16 equal-sized classes
dev.new(
  title = "Species",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.dh, nc = 16, diag = TRUE)

# Euclidean distance matrix on spatial coordinates (2D)
spa.de <- dist(spa)
dev.new(
  title = "x-y",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spa.de, nc = 16, diag = TRUE)

# Euclidean distance matrix on distance from the source (1D)
dfs.df <- as.data.frame(env$dfs, row.names = rownames(env))
riv.de <- dist(dfs.df)
dev.new(
  title = "Distance from the source",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(riv.de, nc = 16, diag = TRUE)


## Examples with artificial data

# Compute five binary variables with 30 objects each.
# Each variable has a predefined number of 0 and 1
# Variable 1: 10 x 1 and 20 x 0; the order is randomized
var1 <- sample(c(rep(1, 10), rep(0, 20)))
# Variable 2: 15 x 0 and 15 x 1, one block each
var2 <- c(rep(0, 15), rep(1, 15))
# Variable 3: alternation of 3 x 1 and 3 x 0 up to 30 objects
var3 <- rep(c(1, 1, 1, 0, 0, 0), 5)
# Variable 4: alternation of 5 x 1 and 10 x 0 up to 30 objects
var4 <- rep(c(rep(1, 5), rep(0, 10)), 2)
# Variable 5: 16 objects with randomized distribution of 7 x 1
# and 9 x 0, followed by 4 x 0 and 10 x 1
var5.1 <- sample(c(rep(1, 7), rep(0, 9)))
var5.2 <- c(rep(0, 4), rep(1, 10))
var5 <- c(var5.1, var5.2)

# Variables 1 to 5 are put into a data frame
(dat <- data.frame(var1, var2, var3, var4, var5))
dim(dat)

# Computation of a matrix of simple matching coefficients
# (called the Sokal and Michener index in ade4)
dat.s1 <- dist.binary(dat, method = 2)
dev.new(
  title = "S1 on fictitious data",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(dat.s1, diag = TRUE)


# Fictitious data for Gower (S15) index
# Random normal deviates with zero mean and unit standard deviation
var.g1 <- rnorm(30, 0, 1)
# Random uniform deviates from 0 to 5
var.g2 <- runif(30, 0, 5)
# Factor with 3 levels (10 objects each)
var.g3 <- gl(3, 10, labels = c("A", "B", "C"))
# Factor with 2 levels, orthogonal to var.g3
var.g4 <- gl(2, 5, 30, labels = c("D", "E"))
(dat2 <- data.frame(var.g1, var.g2, var.g3, var.g4))
summary(dat2)

# Computation of a matrix of Gower dissimilarity using
# function daisy()

# Complete data matrix (4 variables)
dat2.S15 <- daisy(dat2, "gower")
range(dat2.S15)
dev.new(
  title = "S15 on fictitious data - daisy",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(dat2.S15, diag = TRUE)

# Data matrix with the two orthogonal factors only
dat2partial.S15 <- daisy(dat2[, 3:4], "gower")
dev.new(
  title = "S15 on fictitious data, 2 factors - daisy",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(dat2partial.S15, diag = TRUE)
head(as.matrix(dat2partial.S15))

# What are the dissimilarity values in the dat2partial.S15 matrix?
levels(factor(dat2partial.S15))

# Computation of a matrix of Gower dissimilarity using
# function gowdis() of package FD
?gowdis
dat2.S15.2 <- gowdis(dat2)
range(dat2.S15.2)
dev.new(
  title = "S15 on fictitious data - gowdis",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(dat2.S15.2, diag = TRUE)

# Data matrix with the two orthogonal factors only
dat2partial.S15.2 <- gowdis(dat2[, 3:4])
dev.new(
  title = "S15 on fictitious data, 2 factors - gowdis",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(dat2partial.S15.2, diag = TRUE)
head(as.matrix(dat2partial.S15.2))

# What are the dissimilarity values in the dat2partial.S15.2 
# matrix? 
levels(factor(dat2partial.S15.2))


### R-mode dissimilarity matrices

# Transpose matrix of species abundances
spe.t <- t(spe)

# Chi-square pre-transformation followed by Euclidean distance
spe.t.chi <- decostand(spe.t, "chi.square")
spe.t.D16 <- dist(spe.t.chi)
dev.new(
  title = "D16 on fish species (R-mode)",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.t.D16, diag = TRUE)

# Jaccard index on fish presence-absence
spe.t.S7 <- vegdist(spe.t, "jaccard", binary = TRUE)
dev.new(
  title = "S7 on fish species (R-mode)",
  width = 10,
  height = 5,
  noRStudioGD = TRUE
)
coldiss(spe.t.S7, diag = TRUE)


## R-mode correlation matrices

# Pearson r linear correlation among environmental variables
env.pearson <- cor(env)	# default method = "pearson"
round(env.pearson, 2)

# Reorder the variables prior to plotting
env.o <- order.single(env.pearson)

# pairs() is a function to plot a matrix of bivariate scatter 
# plots.
# panelutils.R is a set of functions that add useful features to
# pairs():
# upper.panel = panel.cor: to print correlation coefficients in the
# upper panel, with significance levels;
# diag.panel = panel.hist: to plot histograms of the variables in
# the diagonal.
# Specify method for the choice of the correlation coefficient:
# by default, method = "pearson", other choices are "spearman"
# and "kendall".
# To get a plot in grey tones instead of colors, use no.col = TRUE
# and lower.panel = panel.smoothb.

dev.new(
  title = "Linear correlation matrix",
  width = 8,
  height = 8,
  noRStudioGD = TRUE
)
pairs(
  env[, env.o],
  lower.panel = panel.smooth,
  upper.panel = panel.cor,
  diag.panel = panel.hist,
  main = "Pearson Correlation Matrix"
)

# Kendall tau rank correlation among environmental variables,
# no colours
env.ken <- cor(env, method = "kendall")
env.o <- order.single(env.ken)
dev.new(
  title = "Rank correlation matrix",
  width = 8,
  height = 8,
  noRStudioGD = TRUE
)
pairs(
  env[, env.o],
  lower.panel = panel.smoothb,
  upper.panel = panel.cor,
  no.col = TRUE,
  method = "kendall",
  diag.panel = panel.hist,
  main = "Kendall Correlation Matrix"
)
