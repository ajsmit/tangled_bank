### CHAPTER 5: ORDINATION IN REDUCED SPACE
###
### Borcard D., Gillet F. & Legendre P. 2018. Numerical Ecology with R,
### 2nd edition. Springer International Publishing AG. 
###
### Borcard D., Gillet F. & Legendre P. 2020. Numerical ecology with R, 
### 2nd Chinese edition. (Translation: J. Lai, Institute of Botany, 
### Chinese Academy of Sciences). Higher Education Press, Beijing. 

# Load packages, functions and data ===============================
library(ade4)
library(vegan)
library(gclus)
library(ape)
library(missMDA)
library(FactoMineR)

# Source additional functions that will be used later in this
# Chapter. Our scripts assume that files to be read are in
# the working directory.
source("cleanplot.pca.R")
source("PCA.newr.R")
source("CA.newr.R")

# Load the Doubs data
# The file Doubs.Rdata is assumed to be in the working directory
load("Doubs.RData")
# The file Doubs.RData contains the following objects:
#    spe: species (community) data frame (fish abundances)
#    env: environmental data frame
#    spa: spatial data frame – cartesian coordinates
#    latlong: spatial data frame – latitude and longitude
# Remove empty site 8
spe <- spe[-8, ]
env <- env[-8, ]
spa <- spa[-8, ]

# Load the oribatid mite data. The file mite.Rdata is assumed
# to be in the working directory.
load("mite.RData")

# Principal component analysis (PCA) ==============================

## PCA on the full environmental dataset

# A reminder of the content of the env dataset
summary(env)

# PCA based on a correlation matrix
# Argument scale=TRUE calls for a standardization of the variables
env.pca <- rda(env, scale = TRUE)
env.pca
summary(env.pca) # Default scaling 2
summary(env.pca, scaling = 1)

# Examine and plot partial results from PCA output
?cca.object   # Explains how an ordination object
              # produced by vegan is structured and how to
              # extract its results.

# Eigenvalues
(ev <- env.pca$CA$eig)

# Scree plot and broken stick model
dev.new(title = "Scree plot of PCA eigenvalues", noRStudioGD = TRUE)
screeplot(env.pca, bstick = TRUE, npcs = length(env.pca$CA$eig))


## Two PCA biplots: scaling 1 and scaling 2

# Plots using biplot.rda
dev.new(width = 12,
   height = 6,
   title = "PCA biplots - environmental variables - biplot.rda", 
   noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
biplot(env.pca, scaling = 1, main = "PCA - scaling 1")
biplot(env.pca, main = "PCA - scaling 2")  # Default scaling 2

# Plots using cleanplot.pca
# A rectangular graphic window is needed to draw the plots together
dev.new(width = 12,
   height = 6,
   title = "PCA biplots - environmental variables - cleanplot.pca", 
   noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
cleanplot.pca(env.pca, scaling = 1, mar.percent = 0.08)
cleanplot.pca(env.pca, scaling = 2, mar.percent = 0.04)

# Plots with a subset of variables: ele, oxy, har, bod, 
# using cleanplot.pca
dev.new(
   title = "PCA plot with a subset of variables - cleanplot.pca",
   width = 12,
   height = 6, 
   noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
var.subset <- c(2, 6, 10, 11)
cleanplot.pca(
  env.pca,
  scaling = 1,
  select.spe = var.subset,
  mar.percent = 0.10
)
cleanplot.pca(
  env.pca,
  scaling = 2,
  select.spe = var.subset,
  mar.percent = 0.04
)

# Plots with a subset of variables: ele, oxy, har, bod, 
# using biplot {vegan}
dev.new(
   title = "PCA plot with a subset of variables - biplot vegan",
   width = 14,
   height = 7,
   noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
# Scaling 1
var.sc1.sub <-
  scores(env.pca, 
         scaling = 1, 
         display = "species")[c(2, 6, 10, 11), ]
biplot(env.pca,
       scaling = 1,
       main = "PCA scaling 1",
       type = "n")
text(env.pca,
     scaling = 1,
     display = "sites",
     cex = 0.7)
arrows(
  0,
  0,
  var.sc1.sub[, 1],
  var.sc1.sub[, 2],
  length = 0.10,
  angle = 10,
  col = "red"
)
text(
  var.sc1.sub[, 1],
  var.sc1.sub[, 2],
  labels = rownames(var.sc1.sub),
  col = "red",
  pos = 4
)
# Scaling 2
var.sc2.sub <- scores(env.pca, 
                      display = "species")[c(2, 6, 10, 11), ]
biplot(env.pca, type = "n", main = "PCA scaling 2")
text(env.pca,
     scaling = 2,
     display = "sites",
     cex = 0.7)
arrows(
  0,
  0,
  var.sc2.sub[, 1],
  var.sc2.sub[, 2],
  length = 0.10,
  angle = 10,
  col = "red"
)
text(
  var.sc2.sub[, 1],
  var.sc2.sub[, 2],
  labels = rownames(var.sc2.sub),
  col = "red",
  pos = 4
)


# Projecting position of new variables in a PCA

# PCA of the environmental variables minus oxy and bod
env.pca2 <- rda(env[, -c(10, 11)], scale = TRUE)
# Create data frame with oxy and bod (our "new" variables)
new.var <- env[, c(10, 11)]
# Compute position of new variables (arrow tips)
new.vscores <-
  predict(env.pca2,
          type = "sp",
          newdata = new.var,
          scaling = 2)
# Plot of the result - scaling 2
dev.new(
   title = "Supplementary variables in PCA",
   noRStudioGD = TRUE
)
biplot(env.pca2,
       scaling = 2,
       main = "Supplementary variables in PCA")
arrows(
  0,
  0,
  new.vscores[, 1],
  new.vscores[, 2],
  length = 0.05,
  angle = 30,
  col = "blue"
)
text(
  new.vscores[, 1],
  new.vscores[, 2],
  labels = rownames(new.vscores),
  cex = 0.8,
  col = "blue",
  pos = 2
)


# Projecting supplementary objects into a PCA biplot

# PCA and projection of supplementary sites 2, 9 and 23 using
# prcomp() {stats} and predict()
# Line numbers 8 and 22 are offset because of the deletion 
# of empty site #8

# PCA using prcomp()
# Argument 'scale.= TRUE' calls for a PCA on a correlation matrix;
# it is equivalent to 'scale = TRUE' in function rda()
env.prcomp <- prcomp(env[-c(2, 8, 22), ], scale. = TRUE)

# Plot of PCA results using biplot.prcomp().
# Functions text() and points() do not seem to work with this
# biplot(env.prcomp, scale = 0)

# Plot of PCA site scores using generic function plot()
dev.new(
  title = "Supplementary sites in PCA using prcomp", 
  noRStudioGD = TRUE
)
plot(
  env.prcomp$x[ ,1], 
  env.prcomp$x[ ,2], 
  type = "n",
  main = "PCA scaling 1 - sites with supplementary objects",
  xlab = "PCA 1",
  ylab = "PCA 2"
)
abline(h = 0, col = "gray")
abline(v = 0, col = "gray")
text(
  env.prcomp$x[ ,1],
  env.prcomp$x[ ,2], 
  labels = rownames(env[-c(2, 8, 22), ])
)

# Prediction of new site scores
new.sit <- env[c(2, 8, 22), ]
pca.newsit <- predict(env.prcomp, new.sit) 
# Projection of new site scores into the PCA plot
text(
   pca.newsit[, 1],
   pca.newsit[, 2],
   labels = rownames(pca.newsit),
   cex = 0.8,
   col = "blue"
)

# NOT IN THE BOOK--------------------------------------------------
# PCA of the environmental variables minus sites 2, 9 and 23
# Based on a PCA computed using function rda() of vegan
env.small <- env[-c(2, 8, 22), ]
env.pca3 <- rda(env.small, scale = TRUE)
# Create data frame with sites 2, 8 and 22 (our "new" sites)
new.sit <- env[c(2, 8, 22), ]

# Compute scores of new sites

# Extract Matrix U (called v in vegan)
U.mat <- env.pca3$CA$v
# Standardize new data with parameters of data used in PCA
env.mean <- apply(env.small, 2, mean)
env.sd <- apply(env.small, 2, sd)
newsit.stand <- scale(new.sit, center = env.mean, scale = env.sd)
# Raw scaling 1 scores of new sites
newsit.scores <- newsit.stand %*% U.mat
# Extraction of vegan site scores and retrieval of the vegan 
# scaling constant
env.scores1 <-
  scores(
    env.pca,
    display = "sites",
    choices = c(1, 2),
    scaling = 1
  )
const <- attributes(env.scores1)$const
# Compute scores for new sites - in the vegan universe:
newsit.scores.cons <- newsit.scores / const
# Plot of the result - scaling 1
dev.new(title = "Supplementary sites in PCA", noRStudioGD = TRUE)
biplot(env.pca3, scaling = 1)
text(
  newsit.scores.cons[, 1],
  newsit.scores.cons[, 2],
  labels = rownames(newsit.scores.cons),
  cex = 0.8,
  col = "blue"
)

# Manual computation of the vegan constant
n <- nrow(env[-c(2, 8, 22), ])
eigenv <- env.pca3$CA$eig
tot <- sum(eigenv)
const <- ((n - 1) * tot) ^ 0.25
# END NOT IN THE BOOK


# Combining clustering and ordination results

# Clustering the objects using the environmental data: Euclidean
# distance after standardizing the variables, followed by Ward
# clustering
env.w <- hclust(dist(scale(env)), "ward.D")
# Cut the dendrogram to yield 4 groups
gr <- cutree(env.w, k = 4)
grl <- levels(factor(gr))

# Extract the site scores, scaling 1
sit.sc1 <- scores(env.pca, display = "wa", scaling = 1)

# Plot the sites with cluster symbols and colours (scaling 1)
dev.new(title = "Ordination and clustering", noRStudioGD = TRUE)
p <- plot(
  env.pca,
  display = "wa",
  scaling = 1,
  type = "n",
  main = "PCA correlation + clusters"
)
abline(v = 0, lty = "dotted")
abline(h = 0, lty = "dotted")
for (i in 1:length(grl)) {
  points(sit.sc1[gr == i, ],
         pch = (14 + i),
         cex = 2,
         col = i + 1)
}
text(sit.sc1, row.names(env), cex = 0.7, pos = 3)
# Add the dendrogram
ordicluster(p, env.w, col = "dark grey")
# Add legend interactively
legend(
  locator(1),
  paste("Cluster", c(1:length(grl))),
  pch = 14 + c(1:length(grl)),
  col = 1 + c(1:length(grl)),
  pt.cex = 2
)


# PCA on the fish abundance data

# Hellinger pre-transformation of the species data
spe.h <- decostand(spe, "hellinger")
(spe.h.pca <- rda(spe.h))

# Scree plot and broken stick model
dev.new(
  title = "Scree plot of PCA eigenvalues - species data", 
  noRStudioGD = TRUE
)
screeplot(spe.h.pca,
  bstick = TRUE, 
  npcs = length(spe.h.pca$CA$eig)
)

# PCA biplots
spe.pca.sc1 <- scores(spe.h.pca, display = "species", scaling = 1)
spe.pca.sc2 <- scores(spe.h.pca, display = "species", scaling = 2)

dev.new(title = "PCA on fish species",
        width = 12,
        height = 6,
        noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
cleanplot.pca(spe.h.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(spe.h.pca, scaling = 2, mar.percent = 0.06)


# A posteriori projection of environmental variables in a PCA
# A PCA scaling 2 plot is produced in a new graphic window.
dev.new(
   title = "PCA biplot scaling 2 with environment",
   noRStudioGD = TRUE
)
biplot(spe.h.pca, main = "PCA fish abundances - scaling 2")
(spe.h.pca.env <-
    envfit(spe.h.pca, env, scaling = 2)) # Scaling 2 is default
# Plot significant variables with a user-selected colour
plot(spe.h.pca.env, p.max = 0.05, col = 3)
# This has added the significant environmental variables to the
# last biplot drawn by R.
# BEWARE: envfit must be given the same scaling as the plot to
# which its result is added!


# PCA on the environmental data using PCA.newr() and 
#    biplot.PCA.newr()

# PCA; scaling 1 is the default for biplots in this function
env.PCA.PL <- PCA.newr(env, stand = TRUE)
dev.new(
   title = "PCA on environmental variables - scaling 1",
   noRStudioGD = TRUE
)
biplot.PCA.newr(env.PCA.PL)

# PCA; scaling 2 in the biplot
dev.new(
   title = "PCA on environmental variables - scaling 2",
   noRStudioGD = TRUE
)
biplot.PCA.newr(env.PCA.PL, scaling = 2)


# Imputation of missing values in PCA - 1: 3 missing values

# Replacement of 3 selected values by NA
env.miss3 <- env
env.miss3[2, 5] <- NA    # pH
env.miss3[18, 7] <- NA   # pho
env.miss3[22, 11] <- NA  # dbo

# New means of the involved variables (without missing values)
mean(env.miss3[, 5], na.rm = TRUE)
mean(env.miss3[, 7], na.rm = TRUE)
mean(env.miss3[, 11], na.rm = TRUE)

# Imputation
env.imp <- imputePCA(env.miss3)

# Imputed values

env.imp$completeObs[2, 5]    # Original value: 8.0
env.imp$completeObs[18, 7]   # Original value: 0.60
env.imp$completeObs[22, 11]  # Original value: 16.4

# PCA on the imputed data
env.imp3 <- env.imp$completeObs
env.imp3.pca <- rda(env.imp3, scale = TRUE)

# Procrustes comparison of original PCA and PCA on imputed data
pca.proc <- procrustes(env.pca, env.imp3.pca, scaling = 1)


# Imputation of missing values in PCA - 2: 32 missing values

# Random replacement of 32 values (out of the 319) by NA
rnd <- matrix(sample(c(rep(1, 32), rep(0, 287))), 29, 11)
env.miss32 <- env
env.miss32[rnd == 1] <- NA
# How many NA in each site?
summary(t(env.miss32))
# Alternative way to display the number of NA:
# sapply(as.data.frame(t(env.miss32)), function(x) sum(is.na(x)))

# Imputation
env.imp2 <- imputePCA(env.miss32)

# PCA on the imputed data
env.imp32 <- env.imp2$completeObs
env.imp32.pca <- rda(env.imp32, scale = TRUE)

# Procrustes comparison of original PCA and PCA on imputed data
pca.proc32 <- procrustes(env.pca, env.imp32.pca, scaling = 1)

dev.new(title = "Imputation in PCA",
        width = 14,
        height = 7,
        noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
plot(pca.proc, 
     main = "Procrustes rotation of original and imputed PCA\n3 missing values")
points(pca.proc, display = "target", col = "red")
text(
  pca.proc,
  display = "target",
  col = "red",
  pos = 4,
  cex = 0.6
)
plot(pca.proc32, 
     main = "Procrustes rotation of original and imputed PCA\n32 missing values")
points(pca.proc32, display = "target", col = "red")
text(
  pca.proc32,
  display = "target",
  col = "red",
  pos = 4,
  cex = 0.6
)

# Comparison of the two PCAs in scaling 2
dev.new(title = "Original and imputed PCA, scaling 2",
        width = 14,
        height = 7,
        noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
biplot(env.pca, main = "Original PCA, scaling 2")
biplot(env.imp32.pca, main = "Imputed PCA, scaling 2")



# Correspondence analysis (CA) ====================================

## CA of the raw species dataset (original species abundances)

# Compute CA
(spe.ca <- cca(spe))
summary(spe.ca)		# default scaling 2
summary(spe.ca, scaling = 1)

# Scree plot and broken stick model using vegan's screeplot.cca()
dev.new(title = "Scree plot of CA eigenvalues", noRStudioGD = TRUE)
screeplot(spe.ca, bstick = TRUE, npcs = length(spe.ca$CA$eig))

# CA biplots
dev.new(title = "CA biplots",
        width = 14,
        height = 7,
        noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
# Scaling 1: sites are centroids of species
plot(spe.ca, 
     scaling = 1, 
     main = "CA fish abundances - biplot scaling 1"
)
# Scaling 2 (default): species are centroids of sites
plot(spe.ca, main = "CA fish abundances - biplot scaling 2")

# Projection of supplementary sites in a CA - scaling 1
sit.small <- spe[-c(7, 13, 22), ]  # Data set with 3 sites removed
sitsmall.ca <- cca(sit.small)
dev.new(
   title = "Supplementary sites in CA - scaling 1",
   noRStudioGD = TRUE
)
plot(sitsmall.ca, display = "sites", scaling = 1)
# Project 3 sites
newsit3 <- spe[c(7, 13, 22), ]
ca.newsit <- predict(
                 sitsmall.ca, 
                 newsit3, 
                 type = "wa", 
                 scaling = 1)
text(
  ca.newsit[, 1],
  ca.newsit[, 2],
  labels = rownames(ca.newsit),
  cex = 0.8,
  col = "blue"
)

# Projection of supplementary species in a CA - scaling 2
spe.small <- spe[, -c(1, 3, 10)]  # Data set with 3 species removed
spesmall.ca <- cca(spe.small)
dev.new(
   title = "Supplementary species in CA - scaling 2",
   noRStudioGD = TRUE
)
plot(spesmall.ca, display = "species", scaling = 2)
# Project 3 species
newspe3 <- spe[, c(1, 3, 10)]
ca.newspe <- predict(
                 spesmall.ca, 
                 newspe3, 
                 type = "sp", 
                 scaling = 2)
text(
  ca.newspe[, 1],
  ca.newspe[, 2],
  labels = rownames(ca.newspe),
  cex = 0.8,
  col = "blue"
)

# Curve fitting in a CA biplot
dev.new(
   title = "CA biplot with environmental curves",
   noRStudioGD = TRUE
)
plot(spe.ca, main = "CA fish abundances - scaling 2", 
     sub = "Fitted curves: discharge (red), ammonium (green)")
spe.ca.env <- envfit(spe.ca ~ dis + amm, env)
plot(spe.ca.env)  # Two arrows
ordisurf(spe.ca, env$dis, add = TRUE)
ordisurf(spe.ca, env$amm, add = TRUE, col = "green")

# Species data table ordered after the CA result
vegemite(spe, spe.ca)
# CA-ordered species table illustrated as a heat map
dev.new(
   title = "CA-ordered species table - heat map", 
   noRStudioGD = TRUE
)
tabasco(spe, spe.ca)

# CA using CA.newr() function

spe.CA.PL <- CA.newr(spe)
dev.new(title = "Species CA with CA() function",
        width = 12,
        height = 6,
        noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
biplot.CA.newr(spe.CA.PL, scaling = 1, cex = 1)
biplot.CA.newr(spe.CA.PL, scaling = 2, cex = 1)

# Ordering of the data table following the first CA axis
# The table is transposed, as in the vegemite() output
summary(spe.CA.PL)
t(spe[order(as.vector(spe.CA.PL$scaling1$sites[, 1])), 
      order(as.vector(spe.CA.PL$scaling1$species[, 1]))])

# Cumulative fit of species
spe.CA.PL$fit$cumulfit.spe
# Cumulative fit of sites
spe.CA.PL$fit$cumulfit.obj



# Multiple correspondence analysis (MCA) ==========================

## MCA on the oribatid mite environmental data set

# Preparation of supplementary data: mite classification 
# into 4 groups
# Hellinger transformation of oribatid mite species data
mite.h <- decostand(mite, "hel")
# Ward clustering of mite data
mite.h.ward <- hclust(dist(mite.h), "ward.D2")
# Cut the dendrogram to 4 groups
mite.h.w.g <- cutree(mite.h.ward, 4)
# Assembly of the data set
mite.envplus <- data.frame(mite.env, mite.h.w.g)

# MCA of qualitative environmental data plus supplementary
# variables: 
#   (1) quantitative environmental data, 
#   (2) 4-group mite classification.
#   Default: graph=TRUE.

mite.env.MCA <- MCA(mite.envplus, quanti.sup = 1:2, quali.sup = 6)
mite.env.MCA

# Contingency table crossing variables "Shrub" and "Topo"
table(mite.env$Shrub, mite.env$Topo)



# Principal coordinate analysis (PCoA) ============================

## PCoA on a percentage difference dissimilarity matrix of
## fish species

spe.bray <- vegdist(spe)
spe.b.pcoa <- cmdscale(spe.bray, k = (nrow(spe) - 1), eig = TRUE)
# Plot of the sites
dev.new(
   title = "PCoA on fish species - Percentage difference",
   noRStudioGD = TRUE
)
ordiplot(scores(spe.b.pcoa, choices = c(1, 2)),
         type = "t",
         main = "PCoA with species weighted averages")
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
# Add weighted average projection of species
spe.wa <- wascores(spe.b.pcoa$points[, 1:2], spe)
text(spe.wa, rownames(spe.wa), cex = 0.7, col = "red")
# A posteriori projection of environmental variables
(spe.b.pcoa.env <- envfit(spe.b.pcoa, env))
# Plot significant variables with a user-selected colour
plot(spe.b.pcoa.env, p.max = 0.05, col = 3)


# PCoA and projection of species vectors using function pcoa()
spe.h.pcoa <- pcoa(dist(spe.h))
# Biplots
dev.new(title = "PCoA with species vectors",
        width = 12,
        height = 7,
        noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
# First biplot: Hellinger-transformed species data
biplot.pcoa(spe.h.pcoa, spe.h, dir.axis1 = -1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
text(-0.5, 0.45, "a", cex = 2)
# Second biplot: standardized Hellinger-transformed species data
spe.std <- scale(spe.h)
biplot.pcoa(spe.h.pcoa, spe.std, dir.axis1 = -1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
text(-2.7, 2.45, "b", cex = 2)
# Third biplot: standardized Hellinger-transformed species data;
# only four species in the plot (figure not shown in the book)
dev.new(title = "PCoA with species vectors - 4 species",
        noRStudioGD = TRUE
)
spe.std <- scale(spe.h)
biplot.pcoa(spe.h.pcoa, spe.h[, c(2, 5, 11, 21)], dir.axis1 = -1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)


# Comparison of PCoA results with Euclidean and non-Euclidean
# dissimilarity matrices

# PCoA on a Hellinger distance matrix
is.euclid(dist(spe.h))
summary(spe.h.pcoa)
spe.h.pcoa$values

# PCoA on a percentage difference dissimilarity matrix
is.euclid(spe.bray)
spe.bray.pcoa <- pcoa(spe.bray)
spe.bray.pcoa$values        # Observe eigenvalues 18 and following

# PCoA on the square root of a percentage difference
# dissimilarity matrix
is.euclid(sqrt(spe.bray))
spe.braysq.pcoa <- pcoa(sqrt(spe.bray))
spe.braysq.pcoa$values	# Observe the eigenvalues

# PCoA on a percentage difference dissimilarity matrix with
# Lingoes correction
spe.brayl.pcoa <- pcoa(spe.bray, correction = "lingoes")
spe.brayl.pcoa$values	   # Observe the eigenvalues, col. 1 and 2

# PCoA on a percentage difference dissimilarity matrix with
# Cailliez correction
spe.brayc.pcoa <- pcoa(spe.bray, correction = "cailliez")
spe.brayc.pcoa$values	   # Observe the eigenvalues, col. 1 and 2



# Nonmetric multidimensional scaling (NMDS) =======================

## NMDS applied to the Doubs fish species - percentage difference
## dissimilarity matrix

spe.nmds <- metaMDS(spe, distance = "bray")
spe.nmds
spe.nmds$stress
dev.new(title = "NMDS on fish species - Percentage difference",
   noRStudioGD = TRUE
)
plot(
  spe.nmds,
  type = "t",
  main = paste(
    "NMDS/Percentage difference - Stress =",
    round(spe.nmds$stress, 3)
  )
)

# Shepard plot and goodness of fit
dev.new(title = "NMDS - Shepard plot",
        width = 12,
        height = 6,
        noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
stressplot(spe.nmds, main = "Shepard plot")
gof <- goodness(spe.nmds)
plot(spe.nmds, type = "t", main = "Goodness of fit")
points(spe.nmds, display = "sites", cex = gof * 300)


# Add colours from a clustering result to an NMDS plot

# Ward clustering of percentage difference dissimilarity matrix
# and extraction of four groups
spe.bray.ward <-
  hclust(spe.bray, "ward.D") # Here better than ward.D2 for 4 groups
spe.bw.groups <- cutree(spe.bray.ward, k = 4)
grp.lev <- levels(factor(spe.bw.groups))

# Combination with NMDS result
sit.sc <- scores(spe.nmds)
dev.new(title = "NMDS plot with cluster colors",
   noRStudioGD = TRUE
)
p <-
  ordiplot(sit.sc, type = "n", 
           main = "NMDS/% difference + clusters Ward/% difference")
for (i in 1:length(grp.lev))
{
  points(sit.sc[spe.bw.groups == i, ],
         pch = (14 + i),
         cex = 2,
         col = i + 1)
}
text(sit.sc, row.names(spe), pos = 4, cex = 0.7)
# Add the dendrogram
ordicluster(p, spe.bray.ward, col = "dark grey")
# Add a legend interactively
legend(
  locator(1),
  paste("Group", c(1:length(grp.lev))),
  pch = 14 + c(1:length(grp.lev)),
  col = 1 + c(1:length(grp.lev)),
  pt.cex = 2
)


# -----------------------------------------------------------------
# The Code It Yourself Corner #2

# A simple function to perform PCA

myPCA <- function(Y) {
  Y.mat <- as.matrix(Y)
  object.names <- rownames(Y)
  var.names <- colnames(Y)
  
  # Centre the data (needed to compute matrix F)
  Y.cent <- scale(Y.mat, center = TRUE, scale = FALSE)
  
  # Covariance matrix S
  Y.cov <- cov(Y.cent)
  
  # Eigenvectors and eigenvalues of S (Legendre and Legendre 2012,
  # eq. 9.1 and 9.2)
  Y.eig <- eigen(Y.cov)
  
  # Copy the eigenvectors to matrix U (used to represent variables
  # in scaling 1 biplots)
  U <- Y.eig$vectors
  rownames(U) <- var.names
  
  # Compute matrix F (used to represent objects in scaling 1 plots)
  F <- Y.cent %*% U			# eq. 9.4
  rownames(F) <- object.names
  
  # Compute matrix U2 (to represent variables in scaling 2 plots)
  # eq. 9.8
  U2 <- U %*% diag(Y.eig$values ^ 0.5)
  rownames(U2) <- var.names
  
  # Compute matrix G (to represent objects in scaling 2 plots)
  # eq. 9.14
  G <- F %*% diag(Y.eig$values ^ 0.5)
  rownames(G) <- object.names
  
  # Output of a list containing all the results
  result <- list(Y.eig$values, U, F, U2, G)
  names(result) <- c("eigenvalues", "U", "F", "U2", "G")
  result
}

# -----------------------------------------------------------------

# PCA on fish species using hand-written function
fish.PCA <- myPCA(spe.h)
summary(fish.PCA)
# Eigenvalues
fish.PCA$eigenvalues
# Eigenvalues expressed as percentages
(pv <- 
  round(100 * fish.PCA$eigenvalues / sum(fish.PCA$eigenvalues), 
  2))
# Alternate computation of total variation (denominator)
round(100 * fish.PCA$eigenvalues / sum(diag(cov(spe.h))), 2)
# Cumulative eigenvalues expressed as percentages
round(
  cumsum(100 * fish.PCA$eigenvalues / sum(fish.PCA$eigenvalues)), 
  2)

# Biplots
dev.new(title = "PCA using homemade function",
        width = 12,
        height = 8)
par(mfrow = c(1, 2))
# Scaling 1 biplot
biplot(fish.PCA$F, fish.PCA$U)
# Scaling 2 biplot
biplot(fish.PCA$G, fish.PCA$U2)

# Plots using generic R plot() function
dev.new(
   title = "PCA using homemade function - generic plot function",
   width = 12,
   height = 8,
   noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
# Scaling 1
# Plot objects
plot(
  fish.PCA$F[, 1],
  fish.PCA$F[, 2],
  asp = 1,
  main = "PCA scaling 1",
  xlab = paste("Axis 1 (", pv[1], "%)", sep = ""),
  ylab = paste("Axis 2 (", pv[2], "%)", sep = "")
)
# Plot variables
arrows(
  x0 = 0,
  y0 = 0,
  fish.PCA$U[, 1],
  fish.PCA$U[, 2],
  length = 0.1,
  col = "red"
)
# Add object numbers
text(
  fish.PCA$F[, 1],
  fish.PCA$F[, 2],
  labels = row.names(spe),
  pos = 3,
  cex = 0.8
)
# Add variable names
text(
  fish.PCA$U[, 1],
  fish.PCA$U[, 2],
  labels = colnames(spe),
  adj = c(-0.2, 0.2),
  col = "red",
  cex = 0.8
)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
# Scaling 2
plot(
  fish.PCA$G[, 1],
  fish.PCA$G[, 2],
  asp = 1,
  main = "PCA scaling 2",
  xlab = paste("Axis 1 (", pv[1], "%)", sep = ""),
  ylab = paste("Axis 2 (", pv[2], "%)", sep = "")
)
arrows(
  x0 = 0,
  y0 = 0,
  fish.PCA$U2[, 1],
  fish.PCA$U2[, 2],
  length = 0.1,
  col = "red"
)
text(
  fish.PCA$G[, 1],
  fish.PCA$G[, 2],
  labels = row.names(spe),
  pos = 3,
  cex = 0.8
)
text(
  fish.PCA$U2[, 1],
  fish.PCA$U2[, 2],
  labels = colnames(spe),
  col = "red",
  adj = c(-0.2, 0.2),
  cex = 0.8
)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)

   ```
