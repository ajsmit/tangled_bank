### CHAPTER 4: CLUSTER ANALYSIS
###
### Borcard D., Gillet F. & Legendre P. 2018. Numerical Ecology with R,
### 2nd edition. Springer International Publishing AG. 
###
### Borcard D., Gillet F. & Legendre P. 2020. Numerical ecology with R, 
### 2nd Chinese edition. (Translation: J. Lai, Institute of Botany, 
### Chinese Academy of Sciences). Higher Education Press, Beijing. 

# Load packages, functions and data ===============================

# Load the required packages
library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(pvclust)
library(RColorBrewer)
library(labdsv)
library(rioja)
library(indicspecies)
library(mvpart)
library(MVPARTwrap)
library(dendextend)
library(vegclust)
library(colorspace)
library(agricolae)
library(picante)

# Source additional functions that will be used later in this
# Chapter. Our scripts assume that files to be read are in
# the working directory.
source("drawmap.R")
source("drawmap3.R")
source("hcoplot.R")
source("test.a.R")
source("coldiss.R")
source("bartlett.perm.R")
source("boxplerk.R")
source("boxplert.R")

# Function to compute a binary dissimilarity matrix from clusters
grpdist <- function(X)
{
  require(cluster)
  gr <- as.data.frame(as.factor(X))
  distgr <- daisy(gr, "gower")
  distgr
}

# Load the data
# File Doubs.Rdata is assumed to be in the working directory
load("Doubs.Rdata")

# Remove empty site 8
spe <- spe[-8, ]
env <- env[-8, ]
spa <- spa[-8, ]
latlong <- latlong[-8, ]

# Compute and plot dendrograms ====================================

## Hierarchical agglomerative clustering of the species abundance 
## data

# Compute matrix of chord distance among sites
spe.norm <- decostand(spe, "normalize")
spe.ch <- vegdist(spe.norm, "euc")
# Alternate, direct computation:
spe.ch <- dist.ldc(spe, "chord")

# Attach site names to object of class 'dist'
attr(spe.ch, "Labels") <- rownames(spe)

# External graphical device to compare dendrograms
dev.new(
  title = "Compare clustering methods",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))

# Compute single linkage agglomerative clustering
spe.ch.single <- hclust(spe.ch, method = "single")
# Plot a dendrogram using the default options
plot(spe.ch.single, 
  labels = rownames(spe), 
  main = "Chord - Single linkage")

# Compute complete-linkage agglomerative clustering
spe.ch.complete <- hclust(spe.ch, method = "complete")
plot(spe.ch.complete, 
  labels = rownames(spe), 
  main = "Chord - Complete linkage")

# Compute UPGMA agglomerative clustering
spe.ch.UPGMA <- hclust(spe.ch, method = "average")
plot(spe.ch.UPGMA, 
  labels = rownames(spe), 
  main = "Chord - UPGMA")

# Compute centroid clustering
spe.ch.centroid <- hclust(spe.ch, method = "centroid")
plot(spe.ch.centroid, 
  labels = rownames(spe), 
  main = "Chord - Centroid")

dev.new(
  title = "Compare clustering methods (2)",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))

# Compute beta-flexible clustering using cluster::agnes()
# beta = -0.1
spe.ch.beta1 <- agnes(spe.ch, method = "flexible",
                      par.method = 0.55)
# beta = -0.25
spe.ch.beta2 <- agnes(spe.ch, method = "flexible",
                      par.method = 0.625)
# beta = -0.5
spe.ch.beta3 <- agnes(spe.ch, method = "flexible",
                      par.method = 0.75)
# Change the class of agnes objects
class(spe.ch.beta1)
spe.ch.beta1 <- as.hclust(spe.ch.beta1)
class(spe.ch.beta1)
spe.ch.beta2 <- as.hclust(spe.ch.beta2)
spe.ch.beta3 <- as.hclust(spe.ch.beta3)
plot(spe.ch.beta1, 
  labels = rownames(spe), 
  main = "Chord - Beta-flexible (beta=-0.1)")
plot(spe.ch.beta2, 
  labels = rownames(spe), 
  main = "Chord - Beta-flexible (beta=-0.25)")
plot(spe.ch.beta3, 
  labels = rownames(spe), 
  main = "Chord - Beta-flexible (beta=-0.5)")

# Compute Ward's minimum variance clustering
spe.ch.ward <- hclust(spe.ch, method = "ward.D2")
# Note: in R 3.0.3 or older, type:
#    spe.ch.ward <- hclust(spe.ch, method="ward")
#    spe.ch.ward$height <- sqrt(spe.ch.ward)
# The result will not be striclty the same, however.
# See the hclust help file and 'Numerical Ecology with R',
# 1st edition, p. 61-62.
plot(spe.ch.ward, 
  labels = rownames(spe), 
  main = "Chord - Ward")


# Cophenetic correlations =========================================

# Single linkage clustering
spe.ch.single.coph <- cophenetic(spe.ch.single)
cor(spe.ch, spe.ch.single.coph)
# Complete linkage clustering
spe.ch.comp.coph <- cophenetic(spe.ch.complete)
cor(spe.ch, spe.ch.comp.coph)
# Average clustering
spe.ch.UPGMA.coph <- cophenetic(spe.ch.UPGMA)
cor(spe.ch, spe.ch.UPGMA.coph)
# Ward clustering
spe.ch.ward.coph <- cophenetic(spe.ch.ward)
cor(spe.ch, spe.ch.ward.coph)

# Shepard-like diagrams
dev.new(
  title = "Cophenetic correlation",
  width = 8,
  height = 9,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
plot(
  spe.ch,
  spe.ch.single.coph,
  xlab = "Chord distance",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, sqrt(2)),
  ylim = c(0, sqrt(2)),
  main = c("Single linkage", paste("Cophenetic correlation =",
                        round(
                        cor(spe.ch, spe.ch.single.coph), 3
                        )))
)
abline(0, 1)
lines(lowess(spe.ch, spe.ch.single.coph), col = "red")
plot(
  spe.ch,
  spe.ch.comp.coph,
  xlab = "Chord distance",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, sqrt(2)),
  ylim = c(0, sqrt(2)),
  main = c("Complete linkage", paste("Cophenetic correlation =",
                        round(
                        cor(spe.ch, spe.ch.comp.coph), 3
                        )))
)
abline(0, 1)
lines(lowess(spe.ch, spe.ch.comp.coph), col = "red")
plot(
  spe.ch,
  spe.ch.UPGMA.coph,
  xlab = "Chord distance",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, sqrt(2)),
  ylim = c(0, sqrt(2)),
  main = c("UPGMA", paste("Cophenetic correlation =",
                          round(
                            cor(spe.ch, spe.ch.UPGMA.coph), 3
                          )))
)
abline(0, 1)
lines(lowess(spe.ch, spe.ch.UPGMA.coph), col = "red")
plot(
  spe.ch,
  spe.ch.ward.coph,
  xlab = "Chord distance",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, sqrt(2)),
  ylim = c(0, max(spe.ch.ward$height)),
  main = c("Ward", paste("Cophenetic correlation =",
                         round(
                           cor(spe.ch, spe.ch.ward.coph), 3
                         )))
)
abline(0, 1)
lines(lowess(spe.ch, spe.ch.ward.coph), col = "red")

# spe.ch.beta2.coph <- cophenetic(spe.ch.beta2)
# plot(spe.ch, spe.ch.beta2.coph, xlab="Chord distance",
#      ylab="Cophenetic distance", asp=1,
#      xlim=c(0, sqrt(2)),
#      ylim=c(0, max(spe.ch.beta2$height)),
#      main=c("Beta-flexible clustering",
#             paste("Cophenetic correlation =",
#             round(cor(spe.ch, spe.ch.beta2.coph), 3))))
# abline(0,1)
# lines(lowess(spe.ch, spe.ch.beta2.coph), col="red")


# Gower (1983) distance
(gow.dist.single <- sum((spe.ch - spe.ch.single.coph) ^ 2))
(gow.dist.comp <- sum((spe.ch - spe.ch.comp.coph) ^ 2))
(gow.dist.UPGMA <- sum((spe.ch - spe.ch.UPGMA.coph) ^ 2))
(gow.dist.ward <- sum((spe.ch - spe.ch.ward.coph) ^ 2))


# Graphs of fusion level values ===================================

dev.new(
  title = "Fusion levels",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
# Plot the fusion level values of the single linkage clustering
# plot(spe.ch.single$height, nrow(spe):2, type="S",
# 	main="Fusion levels - Chord - Single",
# 	ylab="k (number of clusters)", 
#           xlab="h (node height)",
#           col="grey")
# text(spe.ch.single$height, nrow(spe):2, nrow(spe):2, 
#      col="red", cex=0.8)
# Plot the fusion level values of the complete linkage clustering
plot(
  spe.ch.complete$height,
  nrow(spe):2,
  type = "S",
  main = "Fusion levels - Chord - Complete",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(spe.ch.complete$height,
     nrow(spe):2,
     nrow(spe):2,
     col = "red",
     cex = 0.8)
# Plot the fusion level values of the UPGMA clustering
plot(
  spe.ch.UPGMA$height,
  nrow(spe):2,
  type = "S",
  main = "Fusion levels - Chord - UPGMA",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(spe.ch.UPGMA$height,
     nrow(spe):2,
     nrow(spe):2,
     col = "red",
     cex = 0.8)
# Plot the fusion level values of the Ward clustering
plot(
  spe.ch.ward$height,
  nrow(spe):2,
  type = "S",
  main = "Fusion levels - Chord - Ward",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(spe.ch.ward$height,
     nrow(spe):2,
     nrow(spe):2,
     col = "red",
     cex = 0.8)
# Plot the fusion level values of the beta-flexible 
# clustering (-0.25)
plot(
  spe.ch.beta2$height,
  nrow(spe):2,
  type = "S",
  main = "Fusion levels - Chord - Beta-flexible",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(spe.ch.beta2$height,
     nrow(spe):2,
     nrow(spe):2,
     col = "red",
     cex = 0.8)


# Compare dendrograms =============================================

## Cut the trees to obtain k groups and compare the group contents
## using contingency tables

# Choose a common number of groups
k <- 4  # Number of groups where at least a small jump is present
# in all four graphs of fusion levels
# Cut the dendrograms
spech.single.g <- cutree(spe.ch.single, k = k)
spech.complete.g <- cutree(spe.ch.complete, k = k)
spech.UPGMA.g <- cutree(spe.ch.UPGMA, k = k)
spech.ward.g <- cutree(spe.ch.ward, k = k)
spech.beta.g <- cutree(spe.ch.beta2, k = k)

# Compare classifications by constructing contingency tables
# Single vs complete linkage
table(spech.single.g, spech.complete.g)
# Single linkage vs UPGMA
table(spech.single.g, spech.UPGMA.g)
# Single linkage vs Ward
table(spech.single.g, spech.ward.g)
# Complete linkage vs UPGMA
table(spech.complete.g, spech.UPGMA.g)
# Complete linkage vs Ward
table(spech.complete.g, spech.ward.g)
# UPGMA vs Ward
table(spech.UPGMA.g, spech.ward.g)
# beta-flexible vs Ward
table(spech.beta.g, spech.ward.g)


## Compare two dendrograms to highlight common subtrees

# Objects of class "hclust" must be first converted into objects of 
# class "dendrogram"
class(spe.ch.ward)
dend1 <- as.dendrogram(spe.ch.ward)
class(dend1)
dend2 <- as.dendrogram(spe.ch.complete)
dend12 <- dendlist(dend1, dend2)
dev.new(
  title = "Compare two dendrograms",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
tanglegram(
  untangle(dend12),
  sort = FALSE,
  common_subtrees_color_branches = TRUE,
  main_left = "Ward method",
  main_right = "Complete linkage"
)


# Multiscale bootstrap resampling =================================

# Hierarchical clustering with p-values via multiscale bootstrap
# resampling

# Compute p-values for all clusters (edges) of the dendrogram
spech.pv <-
  pvclust(t(spe.norm),
          method.hclust = "ward.D2",
          method.dist = "euc",
          parallel=TRUE)

# Plot dendrogram with p-values
dev.new(
  title = "Fish - Chord - pvclust",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 1))
plot(spech.pv)

# "This function plots a dendrogram with p-values for given object
# of class pvclust. AU p-value (printed in red color in default) 
# is the abbreviation of "approximately unbiased" p-value, which is 
# calculated by multiscale bootstrap resampling. BP value (printed
# in green color by default) is "bootstrap probability" value,
# which is less accurate than AU value as p-value. One can consider
# that clusters (edges) with high AU values (e.g. 95%) are strongly
# supported by data."

# Highlight clusters with high "au" p-values
pvrect(spech.pv, alpha = 0.95, pv = "au")
lines(spech.pv)
pvrect(spech.pv, alpha = 0.91, border = 4)



# Optimal number of clusters ======================================

## Select a dendrogram (Ward/chord) and apply three criteria
## to choose the optimal number of clusters

# Choose and rename the dendrogram ("hclust" object)
hc <- spe.ch.ward
# hc <- spe.ch.beta2
# hc <- spe.ch.complete

dev.new(
  title = "Optimal number of clusters",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))

# Average silhouette widths (Rousseeuw quality index)
Si <- numeric(nrow(spe))
for (k in 2:(nrow(spe) - 1))
{
  sil <- silhouette(cutree(hc, k = k), spe.ch)
  Si[k] <- summary(sil)$avg.width
}
k.best <- which.max(Si)
plot(
  1:nrow(spe),
  Si,
  type = "h",
  main = "Silhouette-optimal number of clusters",
  xlab = "k (number of clusters)",
  ylab = "Average silhouette width"
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(k.best,
       max(Si),
       pch = 16,
       col = "red",
       cex = 1.5
)
# Optimal number of clusters according to matrix correlation 
# statistic (Pearson)
kt <- data.frame(k = 1:nrow(spe), r = 0)
for (i in 2:(nrow(spe) - 1)) 
{
  gr <- cutree(hc, i)
  distgr <- grpdist(gr)
  mt <- cor(spe.ch, distgr, method = "pearson")
  kt[i, 2] <- mt
}
k.best <- which.max(kt$r)
plot(
  kt$k,
  kt$r,
  type = "h",
  main = "Matrix correlation-optimal number of clusters",
  xlab = "k (number of clusters)",
  ylab = "Pearson's correlation"
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(k.best,
       max(kt$r),
       pch = 16,
       col = "red",
       cex = 1.5)

# Optimal number of clusters according as per indicator species
# analysis (IndVal, Dufrene-Legendre; package: labdsv)
IndVal <- numeric(nrow(spe))
ng <- numeric(nrow(spe))
for (k in 2:(nrow(spe) - 1))
{
  iva <- indval(spe, cutree(hc, k = k), numitr = 1000)
  gr <- factor(iva$maxcls[iva$pval <= 0.05])
  ng[k] <- length(levels(gr)) / k
  iv <- iva$indcls[iva$pval <= 0.05]
  IndVal[k] <- sum(iv)
}
k.best <- which.max(IndVal[ng == 1]) + 1
col3 <- rep(1, nrow(spe))
col3[ng == 1] <- 3

dev.new(
  title = "IndVal-based search for optimal number of clusters",
  width = 12,
  height = 6,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
plot(
  1:nrow(spe),
  IndVal,
  type = "h",
  main = "IndVal-optimal number of clusters",
  xlab = "k (number of clusters)",
  ylab = "IndVal sum",
  col = col3
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(
  which.max(IndVal),
  max(IndVal),
  pch = 16,
  col = "red",
  cex = 1.5
)
text(28, 15.7, "a", cex = 1.8)

plot(
  1:nrow(spe),
  ng,
  type = "h",
  xlab = "k (number of clusters)",
  ylab = "Ratio",
  main = "Proportion of clusters with significant indicator species",
  col = col3
)
axis(1,
     k.best,
     paste("optimum", k.best, sep = "\n"),
     col = "red",
     font = 2,
     col.axis = "red")
points(k.best,
       max(ng),
       pch = 16,
       col = "red",
       cex = 1.5)
text(28, 0.98, "b", cex = 1.8)


# Final dendrogram with the selected clusters =====================

# Choose the number of clusters
k <- 4
# Silhouette plot of the final partition
spech.ward.g <- cutree(spe.ch.ward, k = k)
sil <- silhouette(spech.ward.g, spe.ch)
rownames(sil) <- row.names(spe)
dev.new(title = "Silhouette plot - Ward - k=4", noRStudioGD = TRUE)
plot(
  sil,
  main = "Silhouette plot - Chord - Ward",
  cex.names = 0.8,
  col = 2:(k + 1),
  nmax = 100
)

# Reorder clusters
spe.chwo <- reorder.hclust(spe.ch.ward, spe.ch)

# Plot reordered dendrogram with group labels
dev.new(
  title = "Final dendrogram",
  width = 8,
  height = 6,
  noRStudioGD = TRUE
)
plot(
  spe.chwo,
  hang = -1,
  xlab = "4 groups",
  sub = "",
  ylab = "Height",
  main = "Chord - Ward (reordered)",
  labels = cutree(spe.chwo, k = k)
)
rect.hclust(spe.chwo, k = k)

# Plot the final dendrogram with group colors (RGBCMY...)
# Fast method using the additional hcoplot() function:
# Usage:
# hcoplot(tree = hclust.object,
#   diss = dissimilarity.matrix,
#   lab = object labels (default NULL),
#   k = nb.clusters,
#   title = paste("Reordered dendrogram from",deparse(tree$call),
#   sep="\n"))
hcoplot(spe.ch.ward, spe.ch, lab = rownames(spe), k = 4)

# Plot the Ward clusters on a map of the Doubs River
# (see Chapter 2)
dev.new(title = "Four Ward clusters on river",
        width = 9,
        noRStudioGD = TRUE)
drawmap(xy = spa,
        clusters = spech.ward.g,
        main = "Four Ward clusters along the Doubs River")



# Miscellaneous graphical outputs =================================

# Convert the "hclust" object into a "dendrogram" object
dend <- as.dendrogram(spe.chwo)

# Plot the dendrogram with colored branches using the dendextend 
# syntax
dev.new(
  title = "Colored dendrogram",
  width = 8,
  height = 6,
  noRStudioGD = TRUE
)
dend %>% set("branches_k_color", k = k) %>% plot

# Use standard colors for clusters
clusters <- cutree(dend, k)[order.dendrogram(dend)]
dend %>% set("branches_k_color", 
             k = k, value = unique(clusters) + 1) %>% plot
# Add a colored bar
colored_bars(clusters + 1,
             y_shift = -0.5,
             rowLabels = paste(k, "clusters"))

# With a colored bar
# clusters <- cutree(dend, k)[order.dendrogram(dend)]
# dend %>% branches_attr_by_clusters(clusters) %>% plot
# cols <- rainbow_hcl(k)
# ord_cols <- rainbow_hcl(k)[order(unique(clusters))]
# tmp_cols  <- rep(1, length(clusters))
# tmp_cols[clusters != 0] <- ord_cols[clusters != 0][clusters]
# colored_bars(tmp_cols, 
#              y_shift = -0.5, 
#              rowLabels = paste(k, "clusters"))
# 
# # Use standard colors
# cols <- 2:(k + 1)
# ord_cols <- cols[order(unique(clusters))]
# tmp_cols  <- rep(1, length(clusters))
# tmp_cols[clusters != 0] <- ord_cols[clusters != 0][clusters]
# dend %>% branches_attr_by_clusters(clusters, values = cols) %>% plot
# colored_bars(tmp_cols, 
#              y_shift = -0.5, 
#              rowLabels = paste(k, "clusters"))

# Heat map of the dissimilarity matrix ordered with the dendrogram
dev.new(title = "Heatmap - sites", noRStudioGD = TRUE)
heatmap(
  as.matrix(spe.ch),
  Rowv = dend,
  symm = TRUE,
  margin = c(3, 3)
)

# Ordered community table
# Species are ordered by their weighted averages on site scores.
# Dots represent absences.
or <- vegemite(spe, spe.chwo)

# Heat map of the doubly ordered community table, with dendrogram
dev.new(title = "Heatmap - species", noRStudioGD = TRUE)
heatmap(
  t(spe[rev(or$species)]),
  Rowv = NA,
  Colv = dend,
  col = c("white", brewer.pal(5, "Greens")),
  scale = "none",
  margin = c(4, 4),
  ylab = "Species (weighted averages of sites)",
  xlab = "Sites"
)


# k-means partitioning of the pre-transformed species data ========

# With 4 groups
spe.kmeans <- kmeans(spe.norm, centers = 4, nstart = 100)
spe.kmeans

# k-means partitioning, 2 to 10 groups
spe.KM.cascade <-
  cascadeKM(
    spe.norm,
    inf.gr = 2,
    sup.gr = 10,
    iter = 100,
    criterion = "ssi"
  )
summary(spe.KM.cascade)
spe.KM.cascade$results
spe.KM.cascade$partition
dev.new(
  title = "CascadeKM",
  width = 10,
  height = 6,
  noRStudioGD = TRUE
)
plot(spe.KM.cascade, sortg = TRUE)

# Comparison with the 4-group classification derived from Ward 
# clustering. Cluster numbering in kmeans is arbitrary!
table(spe.kmeans$cluster, spech.ward.g)
table(spe.KM.cascade$partition[, 3], spech.ward.g)

# Reorder the sites according to the k-means result
spe.kmeans.g <- spe.kmeans$cluster
spe[order(spe.kmeans.g), ]

# Reorder sites and species using function vegemite()
ord.KM <- vegemite(spe, spe.kmeans.g)
spe[ord.KM$sites, ord.KM$species]

# Plot of the 4 k-means clusters on a map of the Doubs River
dev.new(title = "Four k-means clusters on river",
        width = 9,
        noRStudioGD = TRUE)
drawmap(xy = spa,
        clusters = spe.kmeans.g,
        main = "Four k-means clusters along the Doubs River")


# k-means optimization of a hierarchical clustering result ========

# k-means with Ward species means per group (centroids) as 
# starting points:

# Mean species abundances on Ward site clusters
groups <- as.factor(spech.ward.g)
spe.means <- matrix(0, ncol(spe), length(levels(groups)))
row.names(spe.means) <- colnames(spe)
for (i in 1:ncol(spe)) {
  spe.means[i, ] <- tapply(spe.norm[, i], spech.ward.g, mean)
}
# Mean species abundances as starting points
startpoints <- t(spe.means)
# k-means on starting points
spe.kmeans2 <- kmeans(spe.norm, centers = startpoints)

# A slightly different approach is to go back to the hierarchical
# clustering, identify the most 'typical' object in each group
# (cf. silhouette plot), and provide these medoids as starting 
# points to kmeans:

startobjects <- spe.norm[c(2, 17, 21, 23), ]
spe.kmeans3 <- kmeans(spe.norm, centers = startobjects)

# Comparison with the 4-group classification derived from Ward 
# clustering:
table(spe.kmeans2$cluster, spech.ward.g)
# Comparison among the two optimized 4-group classifications:
table(spe.kmeans2$cluster, spe.kmeans3$cluster)

# Silhouette plot of the final partition
spech.ward.gk <- spe.kmeans2$cluster
dev.new(
  title = "Silhouettes - Optimized partition",
  noRStudioGD = TRUE
)
par(mfrow = c(1, 1))
k <- 4
sil <- silhouette(spech.ward.gk, spe.ch)
rownames(sil) <- row.names(spe)
plot(sil,
     main = "Silhouette plot - Ward & k-means",
     cex.names = 0.8,
     col = 2:(k + 1))

# Plot of the optimized Ward clusters on a map of the Doubs River
dev.new(title = "Four optimized Ward clusters on river",
        width = 9,
        noRStudioGD = TRUE)
drawmap(xy = spa,
       clusters = spech.ward.gk,
       main = "Four optimized Ward clusters along the Doubs River")



# Partitioning around medoids (PAM) ===============================
# Computed on the chord distance matrix

# Choice of the number of clusters
# Loop: obtain average silhouette widths (asw) for 2 to 28 clusters
asw <- numeric(nrow(spe))
for (k in 2:(nrow(spe) - 1))
  asw[k] <- pam(spe.ch, k, diss = TRUE)$silinfo$avg.width
k.best <- which.max(asw)
dev.new(title = "PAM", noRStudioGD = TRUE)
plot(
  1:nrow(spe),
  asw,
  type = "h",
  main = "Choice of the number of clusters",
  xlab = "k (number of clusters)",
  ylab = "Average silhouette width"
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(k.best,
       max(asw),
       pch = 16,
       col = "red",
       cex = 1.5)

# PAM for k = 4 clusters
spe.ch.pam <- pam(spe.ch, k = 4, diss = TRUE)
summary(spe.ch.pam)
spe.ch.pam.g <- spe.ch.pam$clustering
spe.ch.pam$silinfo$widths

# Compare with classification from Ward clustering and from k-means
table(spe.ch.pam.g, spech.ward.g)
table(spe.ch.pam.g, spe.kmeans.g)
# Compare classifications from k-means and from optimized Ward 
# clustering
table(spe.kmeans.g, spech.ward.gk)

# Silhouette profile for k = 4 groups, k-means and PAM
dev.new(
  title = "Silhouettes - k-means and PAM",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
k <- 4
sil <- silhouette(spe.kmeans.g, spe.ch)
rownames(sil) <- row.names(spe)
plot(sil,
     main = "Silhouette plot - k-means",
     cex.names = 0.8,
     col = 2:(k + 1))
plot(
  silhouette(spe.ch.pam),
  main = "Silhouette plot - PAM",
  cex.names = 0.8,
  col = 2:(k + 1)
)



# Relationships between fish clusters and environmental variables
# based on the optimized partition (four groups) ==================

with(env, {
  # Boxplots of four quantitative environmental variables:
  # Elevation, Slope, Oxygen, and Ammonium (after some 
  # transformations)
  dev.new(
      title = "Boxplots of quantitative environmental variables",
      noRStudioGD = TRUE
  )
  par(mfrow = c(2, 2))
  boxplot(
    sqrt(ele) ~ spech.ward.gk,
    main = "Elevation",
    las = 1,
    ylab = "sqrt(alt)",
    col = (1:k) + 1,
    varwidth = TRUE
  )
  boxplot(
    log(slo) ~ spech.ward.gk,
    main = "Slope",
    las = 1,
    ylab = "log(slo)",
    col = (1:k) + 1,
    varwidth = TRUE
  )
  boxplot(
    oxy ~ spech.ward.gk,
    main = "Oxygen",
    las = 1,
    ylab = "oxy",
    col = (1:k) + 1,
    varwidth = TRUE
  )
  boxplot(
    sqrt(amm) ~ spech.ward.gk,
    main = "Ammonium",
    las = 1,
    ylab = "sqrt(amm)",
    col = (1:k) + 1,
    varwidth = TRUE
  )
})

with(env, {
  # Test of ANOVA assumptions
  # Normality of residuals
  shapiro.test(resid(aov(sqrt(ele) ~ as.factor(spech.ward.gk))))
  shapiro.test(resid(aov(log(slo) ~ as.factor(spech.ward.gk))))
  shapiro.test(resid(aov(oxy ~ as.factor(spech.ward.gk))))
  shapiro.test(resid(aov(sqrt(amm) ~ as.factor(spech.ward.gk))))
  
  # Homogeneity of variances
  bartlett.test(sqrt(ele), as.factor(spech.ward.gk))
  bartlett.test(log(slo), as.factor(spech.ward.gk))
  bartlett.test(oxy, as.factor(spech.ward.gk))
  bartlett.test(sqrt(amm), as.factor(spech.ward.gk))
  
  # ANOVA of the testable variables
  summary(aov(log(slo) ~ as.factor(spech.ward.gk)))
  summary(aov(oxy ~ as.factor(spech.ward.gk)))
  summary(aov(sqrt(amm) ~ as.factor(spech.ward.gk)))
  
  # Kruskal-Wallis test of variable alt
  kruskal.test(ele ~ as.factor(spech.ward.gk))
})

dev.new(
  title = "ANOVA and Kruskal-Wallis tests",
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
with(env, {
# Use boxplert() or boxplerk() to plot results with post-hoc tests
  boxplerk(
    ele,
    spech.ward.gk,
    xlab = "",
    ylab = "ele",
    main = "Elevation",
    bcol = (1:k) + 1,
    p.adj = "holm"
  )
  boxplert(
    log(slo),
    spech.ward.gk,
    xlab = "",
    ylab = "log(slo)",
    main = "Slope",
    bcol = (1:k) + 1,
    p.adj = "holm"
  )
  boxplert(
    oxy,
    spech.ward.gk,
    xlab = "",
    ylab = "oxy",
    main = "Oxygen",
    bcol = (1:k) + 1,
    p.adj = "holm"
  )
  boxplert(
    sqrt(amm),
    spech.ward.gk,
    xlab = "",
    ylab = "sqrt(amm)",
    main = "Ammonium",
    bcol = (1:k) + 1,
    p.adj = "holm"
  )
})


# Contingency table of two typologies =============================

# Environment-based typology (see Chapter 2)
env2 <- env[, -1]
env.de <- vegdist(scale(env2), "euc")
env.kmeans <- kmeans(env.de, centers = 4, nstart = 100)
env.kmeans.g <- env.kmeans$cluster

# Table crossing the species and environment 4-group typologies
table(spe.kmeans.g, env.kmeans.g)

# Test the relationship using a chi-square test
#    chisq.test(table(spe.kmeans.g, env.kmeans.g))
# Change the testing procedure to a permutation test
#    chisq.test(table(spe.kmeans.g, env.kmeans.g), 
#               simulate.p.value = TRUE)

# Test the relationship using a Fisher's exact test
fisher.test(table(spe.kmeans.g, env.kmeans.g))


# Mean species abundances on site clusters ========================

groups <- as.factor(spech.ward.gk)
spe.means <- matrix(0, ncol(spe), length(levels(groups)))
row.names(spe.means) <- colnames(spe)
for (i in 1:ncol(spe)) {
  spe.means[i, ] <- tapply(spe[, i], spech.ward.gk, mean)
}
# Mean species abundances of the four groups
group1 <- round(sort(spe.means[, 1], decreasing = TRUE), 2)
group2 <- round(sort(spe.means[, 2], decreasing = TRUE), 2)
group3 <- round(sort(spe.means[, 3], decreasing = TRUE), 2)
group4 <- round(sort(spe.means[, 4], decreasing = TRUE), 2)
# Species with abundances greater than group mean species abundance
group1.domin <- which(group1 > mean(group1))
group1
group1.domin
#... same for other groups


# Kendall's W coefficient of concordance ==========================

# Transformation of species data and transposition
spe.hel <- decostand(spe, "hellinger")
spe.std <- decostand(spe.hel, "standardize")
spe.t <- t(spe.std)

# First test of Kendall concordance involving all species
(spe.kendall.global1 <- kendall.global(spe.hel))

# k-means partitioning of species
spe.t.kmeans.casc <- cascadeKM(
  spe.t,
  inf.gr = 2,
  sup.gr = 8,
  iter = 100,
  criterion = "calinski"
)
dev.new(
  title = "Kmeans on species",
  width = 10,
  height = 6,
  noRStudioGD = TRUE
)
plot(spe.t.kmeans.casc, sortg = TRUE)

# The partition into 2 groups is found in column 1 of the 
# object $partition
(clusters2 <- spe.t.kmeans.casc$partition[, 1])

# Partitions into three or four groups:
(clusters3 <- spe.t.kmeans.casc$partition[, 2]) 
(clusters4 <- spe.t.kmeans.casc$partition[, 3])

# Concordance analysis
(spe.kendall.global2 <- kendall.global(spe.hel, clusters2))

# A posteriori tests
(spe.kendall.post2 <- 
     kendall.post(spe.hel, 
                  clusters2, 
                  nperm = 9999))

(spe.kendall.post3 <- 
     kendall.post(spe.hel, 
                  clusters3, 
                  nperm = 9999))


# Species assemblages on presence-absence data ====================

# Transform the data to presence-absence
spe.pa <- decostand(spe, "pa")
# Test the co-occurrence of species
# Many permutations to have enough decimal places for Holm 
# correction (see below).
res <- test.a(spe.pa, nperm = 99999)
summary(res)

# Compute a Holm correction on the matrix of p-values
(res.p.vec <- as.vector(res$p.a.dist))
(adjust.res <- p.adjust(res.p.vec, method = "holm"))
# Check that the number of permutations allows significant p-values
# after Holm's correction. Look for values <=0.05:
range(adjust.res)
# Significance threshold: among the corrected Holm p-values,
# find 0.05 or the closest value smaller than 0.05.
(adj.sigth <- max(adjust.res[adjust.res <= 0.05]))
# Now find the uncorrected p-value corresponding to adj.sigth:
(sigth <- max(res.p.vec[adjust.res <= 0.05]))
# Assign 1 to nonsignificant p-values after Holm's correction
res.pa.dist <- res$p.a.dist
res.pa.dist[res.pa.dist > sigth] <- 1
# How many (unadjusted) values are equal to or smaller than sigth?
length(which(res.p.vec <= sigth))

# Heat map of significant p-values
dev.new(
  title = "0-1 species associations, heat map",
  width = 12,
  height = 6,
  noRStudioGD = TRUE
)
coldiss(res.pa.dist,
        nc = 16,
        byrank = TRUE,
        diag = TRUE)


# Co-occurrence network ===========================================

# Load supplementary packages
# These will be unloaded at the end of this section because of
# conflicts with vegan function names
library(igraph)
library(rgexf)

# Adjacency matrix from the binary matrix of significant
# co-occurrences
adjm1 <- 1 - as.matrix(res.pa.dist)
diag(adjm1) <- 0

# Adjacency matrix from the "a" distance matrix
adjm2 <- 1 - as.matrix(res$p.a.dist)
adjm2[adjm2 < 0.5] <- 0
diag(adjm2) <- 0

# Adjacency matrix from the Spearman rank correlation matrix
adjm3 <- cor(spe, method = "spearman")
# Only positive associations (rho > 0.2)
# adjm[adjm3 < 0.2] <- 0
# adjm[adjm3 >= 0.2] <- 1  # binary co-occurrences
adjm2[adjm3 < 0.25] <- 0
adjm2[adjm3 >= 0.25] <- 1  # binary co-occurrences
diag(adjm3) <- 0

# Species co-occurrence dissimilarities (picante, Hardy 2008)
?species.dist
adjm4 <- species.dist(spe.pa, metric = "jaccard") # Jaccard!!
adjm4 <- as.matrix(adjm4)
adjm4[adjm4 < 0.4] <- 0

# Select an adjacency matrix
adjm <- adjm4
summary(as.vector(adjm))

dev.new(
  title = "Species co-occurrences",
  noRStudioGD = TRUE
)
# Plot histogram of adjacency values
hist(adjm)

# Build graph
go <- graph_from_adjacency_matrix(adjm, 
                                  weighted = TRUE, 
                                  mode = "undirected")
plot(go)

# Network structure detection: find densely connected subgraphs 
# (modules or "communities") in a graph
# wc <- cluster_walktrap(go)
# wc <- cluster_edge_betweenness(go)
# wc <- cluster_fast_greedy(go)
# wc <- cluster_leading_eigen(go)
# wc <- cluster_spinglass(go)
wc <- cluster_optimal(go)
modularity(wc)
membership(wc)
plot(wc, go)

# Export to gephi
gexfo <- igraph.to.gexf(go)
print(gexfo, file = "doubs0.gexf", replace = TRUE)

# Detach package rgexf
detach("package:rgexf", unload = TRUE)
# If not sufficient:
unloadNamespace("rgexf")
# Detach package igraph to avoid conflicts:
# detach("package:igraph", unload = TRUE)
# If not sufficient:
# unloadNamespace("igraph")



# Indicator species ===============================================

# IndVal species indicator values (Dufrene and Legendre)

# Divide the sites into 4 groups depending on the distance from
# the source of the river
dfs.D1 <- dist(data.frame(dfs = env[, 1], 
               row.names = rownames(env)))
dfsD1.kmeans <- kmeans(dfs.D1, centers = 4, nstart = 100)
# Cluster delimitation and numbering
dfsD1.kmeans$cluster
# The numbering is machine-dependent. To avoid confusions let us
# construct a vector of groups with consecutive numbers on the
# basis of dfsD1.kmeans$cluster:
grps <- rep(1:4, c(8, 10, 6, 5))
# Indicator species for this typology of the sites
(iva <- indval(spe, grps, numitr = 10000))

# Correction of the p-values for multiple testing
pval.adj <- p.adjust(iva$pval)

# Table of the significant indicator species
gr <- iva$maxcls[pval.adj <= 0.05]
iv <- iva$indcls[pval.adj <= 0.05]
pv <- iva$pval[pval.adj <= 0.05]
fr <- apply(spe > 0, 2, sum)[pval.adj <= 0.05]
fidg <- data.frame(
  group = gr,
  indval = iv,
  pvalue = pv,
  freq = fr
)
fidg <- fidg[order(fidg$group, -fidg$indval), ]
fidg
# Export the result to a CSV file (to be opened in a spreadsheet)
write.csv(fidg, "IndVal-dfs.csv")


# Indval with multipatt{indicspecies} with search for indicator
# species of pooled groups
(iva2 <- multipatt(
  spe,
  grps,
  max.order = 2,
  control = how(nperm = 999)
))

# Here again, correction of the p-values for multiple testing
(pval.adj2 <- p.adjust(iva2$sign$p.value))

summary(iva2, indvalcomp = TRUE)

# Indval with bootstrap confidence intervals of indicator values
(iva2.boot <- strassoc(spe, grps, func = "IndVal.g", nboot = 1000))

# Phi correlation index
(iva.phi <- multipatt(
  spe,
  grps,
  func = "r.g",
  max.order = 2,
  control = how(nperm = 999)
))
summary(iva.phi)
round(iva.phi$str, 3)
(iva.phi.boot <- strassoc(spe, grps, func = "r.g", nboot = 1000))



# Multivariate regression trees ===================================

# As of this writing, {mvpart} and {MVPARTwrap} must be installed 
# from github.
# If it is not already done, follow these lines:
# On Windows machines, Rtools (3.4 and above) must be installed
# first. Go to:
# https://cran.r-project.org/bin/windows/Rtools/
# Rtools must be reinstalled with every new installation of R.
# After that (for Windows and MacOS), type:
#   install.packages("devtools")
#   library(devtools)
#   install_github("cran/mvpart", force = TRUE)
#   install_github("cran/MVPARTwrap", force = TRUE)

dev.new(
  title = "Multivariate regression tree - all explanatory variables",
  width = 14,
  height = 7,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
spe.ch.mvpart <-
  mvpart(
    data.matrix(spe.norm) ~ .,
    env,
    margin = 0.08,
    cp = 0,
    xv = "pick",
    xval = nrow(spe),
    xvmult = 100
  )
# Here, click on the desired number of groups (for example 4)

summary(spe.ch.mvpart)
printcp(spe.ch.mvpart)

# Residuals of MRT
dev.new(
  title = "Residuals of MRT",
  width = 10,
  height = 6,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
hist(residuals(spe.ch.mvpart), col = "bisque")
plot(predict(spe.ch.mvpart, type = "matrix"),
     residuals(spe.ch.mvpart),
     main = "Residuals vs Predicted")
abline(h = 0, lty = 3, col = "grey")

# Group composition
spe.ch.mvpart$where
# Group identity
(groups.mrt <- levels(as.factor(spe.ch.mvpart$where)))
# Fish composition of first leaf
spe.norm[which(spe.ch.mvpart$where == groups.mrt[1]), ]
# Environmental variables of first leaf
env[which(spe.ch.mvpart$where == groups.mrt[1]), ]

# Table and pie charts of fish composition of leaves
leaf.sum <- matrix(0, length(groups.mrt), ncol(spe))
colnames(leaf.sum) <- colnames(spe)
for (i in 1:length(groups.mrt))
{
  leaf.sum[i, ] <-
    apply(spe.norm[which(spe.ch.mvpart$where == groups.mrt[i]), ],
          2, sum)
}
leaf.sum
dev.new(title = "Fish composition of 4 leaves", noRStudioGD = TRUE)
par(mfrow = c(2, 2))
for (i in 1:length(groups.mrt))
{
  pie(which(leaf.sum[i, ] > 0),
      radius = 1,
      main = paste("leaf #", groups.mrt[i]))
}

# Extracting MRT results from an mvpart object
# Packages MVPARTwrap and rdaTest must have been loaded
spe.ch.mvpart.wrap <-
  MRT(spe.ch.mvpart, percent = 10, species = colnames(spe))
summary(spe.ch.mvpart.wrap)

# Indicator species search on the MRT result
spe.ch.MRT.indval <- indval(spe.norm, spe.ch.mvpart$where)
# spe.ch.MRT.indval$pval		# Probability
pval.adj3 <- p.adjust(spe.ch.MRT.indval$pval)    # Corrected prob.

# For each significant species, find the leaf with the highest
# IndVal
# spe.ch.MRT.indval$maxcls[which(spe.ch.MRT.indval$pval <= 0.05)]
spe.ch.MRT.indval$maxcls[which(pval.adj3 <= 0.05)]

# IndVal value in the best leaf for each significant species
# spe.ch.MRT.indval$indcls[which(spe.ch.MRT.indval$pval <= 0.05)]
spe.ch.MRT.indval$indcls[which(pval.adj3 <= 0.05)]

# Partition of objects based on MRT
spech.mvpart.g <- factor(spe.ch.mvpart$where)
levels(spech.mvpart.g) <- 1:length(levels(spech.mvpart.g))
# Compare with partition from unconstrained clustering
table(spech.mvpart.g, spech.ward.g)

# Plot of the MRT clusters on a map of the Doubs River
dev.new(title = "Four MRT clusters on river",
        width = 9,
        noRStudioGD = TRUE)
drawmap(xy = spa,
        clusters = spech.mvpart.g,
        main = "Four MRT clusters along the Doubs River")


## MRT as a monothetic clustering method

# Method related to Williams & Lambert (1959) association analysis:
# spe.pa (presence-absence) is the response and the explanatory 
# matrix

spe.pa <- decostand(spe, "pa")

dev.new(
  title = "MRT for monothetic clustering",
  width = 14,
  height = 7,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))

# spe.pa is the response and the explanatory matrix
res.part1 <-
  mvpart(
    data.matrix(spe.pa) ~ .,
    data = spe.pa,
    margin = 0.08,
    xv = "p",
    xvmult = 100
  )
# Here, click on the desired tree size (suggested: 6)
res.part1$where

# spe.norm is the response, spe.pa is the explanatory matrix
res.part2 <-
  mvpart(
    data.matrix(spe.norm) ~ .,
    data = spe.pa,
    margin = 0.08,
    xv = "p",
    xvmult = 100
  )
# Here, click on the desired tree size (suggested: 5)
res.part2$where

# spe.norm is the response and spe (untransformed) is the 
# explanatory matrix
res.part3 <-
  mvpart(
    data.matrix(spe.norm) ~ .,
    data = spe,
    margin = 0.08,
    xv = "p",
    cp = 0,
    xvmult = 100
  )
# Here, click on the desired tree size (suggested: 6)


# Membership of objects to groups – presence-absence on both sides
res.part1$where
res.part1.g <- factor(res.part1$where)
levels(res.part1.g) <- 1:length(levels(res.part1.g))
# Compare with groups from unconstrained clustering
table(res.part1.g, spech.ward.g)
table(res.part1.g, spech.ward.gk)

# Plot of the MRT clusters on a map of the Doubs River
dev.new(title = "Six monothetic clusters on river",
        width = 9,
        noRStudioGD = TRUE)
drawmap3(xy = spa,
        clusters = res.part1.g,
        main = "Six monothetic clusters along the Doubs River")



# Clustering with sequential constraint ===========================

# NOT IN THE BOOK -- Sequential clustering using MRT

dev.new(
  title = "MRT with sequential constraint", 
  width = 14, 
  height = 7,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
spe.ch.seq <- mvpart(as.matrix(spe) ~ dfs, 
                     env, 
                     cp = 0, 
                     xv = "pick", 
                     margin = 0.08,
	         xval = nrow(spe), 
	         xvmult = 100, 
	         which = 4)
# Here, click on the desired number of groups

summary(spe.ch.seq)

# Group composition (labels of terminal nodes)
(gr <- spe.ch.seq$where)

# Renumber clusters sequentially
aa <- 1
gr2 <- rep(1, length(gr))
for (i in 2 : length(gr))
{
	if (gr[i] !=  gr[i-1]) aa <- aa + 1
	gr2[i] <- aa
}

# Plot the clusters on a map of the Doubs river
dev.new(title = "MRT groups on river", noRStudioGD = TRUE)
drawmap3(xy = spa,
        clusters = gr2,
        main = "MRT sequential clustering along the Doubs River")

# END NOT IN THE BOOK


# Default method CONISS
# On the percentage difference dissimilarity matrix
spe.chcl <- chclust(vegdist(spe))

# Compare the dispersion of the hierarchical classification to that
# obtained from a broken stick model
dev.new(title="Compare classification to broken stick model", 
  noRStudioGD=TRUE
)
bstick(spe.chcl, 10)

# Cut the dendrogram in 4 clusters
k <- 4
(gr4 <- cutree(spe.chcl, k = k))

# Plot the dendrogram
dev.new(
  title = "Constrained (sequential) clustering",
  width = 12,
  height = 6,
  noRStudioGD = TRUE
)
plot(spe.chcl, hang = -1, main = "CONISS clustering")
rect.hclust(spe.chcl, k = k)

# Dendrogram with observations plotted according to dfs
plot(spe.chcl,
     xvar = env$dfs,
     hang = -1,
     main = "CONISS clustering",
     cex = 0.8)

# Plot the clusters on a map of the Doubs River
dev.new(title = "Four sequential clusters on river",
        width = 9,
        noRStudioGD = TRUE)
drawmap(xy = spa,
        clusters = gr4,
        main = "Sequential clusters along the river")



# Fuzzy clustering ================================================

## Fuzzy c-means clustering of the fish species data

k <- 4		# Choose the number of clusters
spe.fuz <- fanny(spe.ch, k = k, memb.exp = 1.5)
summary(spe.fuz)

# Site fuzzy membership
spe.fuz$membership
# Nearest crisp clustering
spe.fuz$clustering
spefuz.g <- spe.fuz$clustering

# Silhouette plot
dev.new(title = "Fuzzy clustering of fish data - Silhouette plot",
        noRStudioGD = TRUE)
plot(
  silhouette(spe.fuz),
  main = "Silhouette plot - Fuzzy clustering",
  cex.names = 0.8,
  col = spe.fuz$silinfo$widths + 1
)

# Ordination of fuzzy clusters (PCoA)
# Step 1: ordination (PCoA) of the fish chord distance matrix
dc.pcoa <- cmdscale(spe.ch)
dc.scores <- scores(dc.pcoa, choices = c(1, 2))
# Step 2: ordination plot of fuzzy clustering result
dev.new(title = "Fuzzy clustering of fish data - Ordination plot",
        noRStudioGD = TRUE)
plot(dc.scores,
     asp = 1,
     type = "n",
     main = "Ordination of fuzzy clusters (PCoA)")
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
# Step 3: representation of fuzzy clusters
for (i in 1:k)
{
  gg <- dc.scores[spefuz.g == i, ]
  hpts <- chull(gg)
  hpts <- c(hpts, hpts[1])
  lines(gg[hpts, ], col = i + 1)
}
stars(
  spe.fuz$membership,
  location = dc.scores,
  key.loc = c(0.6, 0.4),
  key.labels = 1:k,
  draw.segments = TRUE,
  add = TRUE,
  # scale = FALSE,
  len = 0.075,
  col.segments = 2:(k + 1)
)

# Plot the fuzzy clusters on a map of the Doubs River
dev.new(title = "Four fuzzy clusters on river",
        width = 9,
        noRStudioGD = TRUE)
plot(
  spa,
  asp = 1,
  type = "n",
  main = "Fuzzy clusters along the river",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
text(65, 20, "Upstream", cex = 1.2)
text(15, 32, "Downstream", cex = 1.2)
# Add sectors to represent fuzzy membership
for (i in 1:k) {
  stars(
    spe.fuz$membership,
    location = spa,
    key.loc = c(150, 20),
    key.labels = 1:k,
    draw.segments = TRUE,
    add = TRUE,
    # scale = FALSE,
    len = 5,
    col.segments = 2:(k + 1)
  )
}


# Noise clustering (vegclust) =====================================

?vegclust

k <- 4
## Create noise clustering with 4 clusters. Perform 30 starts
## from random seeds and keep the best solution
spe.nc <- vegclust(
  spe.norm,
  mobileCenters = k,
  m = 1.5,
  dnoise = 0.75,
  method = "NC",
  nstart = 30
)
spe.nc

# Medoids of species
(medoids <- spe.nc$mobileCenters)

# Fuzzy membership matrix
spe.nc$memb

# Cardinality of fuzzy clusters (i.e., the number of objects
# belonging to each cluster)
spe.nc$size

# Obtain hard membership vector, with 'N' for objects that are
# unclassified
spefuz.g <- defuzzify(spe.nc$memb)$cluster
clNum <- as.numeric(as.factor(spefuz.g))

# Ordination of fuzzy clusters (PCoA)
dev.new(title = "Noise clustering of fish data - Ordination plot",
        noRStudioGD = TRUE)
plot(dc.scores,
     # main = "Ordination of fuzzy clusters (PCoA)",
     asp = 1,
     type = "n")
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
for (i in 1:k)
{
  gg <- dc.scores[clNum == i, ]
  hpts <- chull(gg)
  hpts <- c(hpts, hpts[1])
  lines(gg[hpts, ], col = i + 1)
}
stars(
  spe.nc$memb[, 1:4],
  location = dc.scores,
  key.loc = c(0.6, 0.4),
  key.labels = 1:k,
  draw.segments = TRUE,
  add = TRUE,
  # scale = FALSE,
  len = 0.075,
  col.segments = 2:(k + 1)
)

# Defuzzified site plot
dev.new(title = "Defuzzified site plot",
        noRStudioGD = TRUE)
plot(
  dc.pcoa,
  xlab = "MDS1",
  ylab = "MDS2",
  pch = clNum,
  col = clNum
)
legend(
  "topleft",
  col = 1:(k + 1),
  pch = 1:(k + 1),
  legend = levels(as.factor(spefuz.g)),
  bty = "n"
)

# Plot the fuzzy clusters on a map of the Doubs River
dev.new(title = "Four noise clusters on river",
        width = 9,
        noRStudioGD = TRUE)
plot(
  spa,
  asp = 1,
  type = "n",
  main = "Noise clusters along the river",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
text(65, 20, "Upstream", cex = 1.2)
text(15, 32, "Downstream", cex = 1.2)
# Add sectors to represent fuzzy membership
for (i in 1:k) {
  stars(
    spe.nc$memb[, 1:4],
    location = spa,
    key.loc = c(150, 20),
    key.labels = 1:k,
    draw.segments = TRUE,
    add = TRUE,
    # scale = FALSE,
    len = 5,
    col.segments = 2:(k + 1)
  )
}
