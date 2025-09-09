### CHAPTER 8 - COMMUNITY DIVERSITY
###
### Borcard D., Gillet F. & Legendre P. 2018. Numerical Ecology with R,
### 2nd edition. Springer International Publishing AG.
###
### Borcard D., Gillet F. & Legendre P. 2020. Numerical ecology with R,
### 2nd Chinese edition. (Translation: J. Lai, Institute of Botany,
### Chinese Academy of Sciences). Higher Education Press, Beijing.

# Load packages, functions and data ===============================
library(ade4)
library(adegraphics)
library(adespatial)
library(vegan)
# library(vegetarian)
library(ggplot2)
library(FD)
library(taxize)

# Source additional functions that will be used later in this
# Chapter. Our scripts assume that files to be read are in
# the working directory.
source("panelutils.R")
source("Rao.R")

# Load the Doubs data. The file Doubs.Rdata is assumed to be in
# the working directory
load("Doubs.RData")
# Remove empty site 8
spe <- spe[-8, ]
env <- env[-8, ]
spa <- spa[-8, ]

# Load the oribatid mite data. The file mite.Rdata is assumed
# to be in the working directory.
load("mite.RData")


# Alpha species diversity =========================================

# Get help on vegan's diversity() function
?vegan::diversity

# Compute alpha diversity indices of the fish communities
N0 <- rowSums(spe > 0) # Species richness
N0 <- specnumber(spe) # Species richness (alternate)
H <- diversity(spe) # Shannon entropy (base e)
Hb2 <- diversity(spe, base = 2) # Shannon entropy (base 2)
N1 <- exp(H) # Shannon diversity (base e)
# (number of abundant species)
N1b2 <- 2^Hb2 # Shannon diversity (base 2)
N2 <- diversity(spe, "inv") # Simpson diversity
# (number of dominant species)
J <- H / log(N0) # Pielou evenness
E10 <- N1 / N0 # Shannon evenness (Hill's ratio)
E20 <- N2 / N0 # Simpson evenness (Hill's ratio)
(div <- data.frame(N0, H, Hb2, N1, N1b2, N2, E10, E20, J))

# Correlations among diversity indices
cor(div)

dev.new(title = "Correlation matrix", width = 8, height = 8, noRStudioGD = TRUE)
pairs(
  div[-1, ],
  lower.panel = panel.smooth,
  upper.panel = panel.cor,
  diag.panel = panel.hist,
  main = "Pearson Correlation Matrix"
)


# Rarefaction analysis
# Number of species in the 70 moss or soil cores
(mite.nbsp <- specnumber(mite))
# Cores with minimum and maximum observed species richness
mite.nbsp[mite.nbsp == min(mite.nbsp)]
mite.nbsp[mite.nbsp == max(mite.nbsp)]
range(mite.nbsp)
# Total abundance in the 70 cores
(mite.abund <- rowSums(mite))
range(mite.abund)
# Abundance in the cores with smallest number of species
mite.abund[mite.nbsp == min(mite.nbsp)]
# Abundance in the core with largest number of species
mite.abund[mite.nbsp == max(mite.nbsp)]
# Number of species in the core with smallest abundance
mite.nbsp[mite.abund == min(mite.abund)]
# Number of species in the core with largest abundance
mite.nbsp[mite.abund == max(mite.abund)]

# Rarefaction to 80 individuals
mite.rare80 <- rarefy(mite, sample = 80)
# Compare ranking of observed and rarefied cores
sort(mite.nbsp)
sort(round(mite.rare80))
# Sites with minimum and maximum estimated species richness
mite.rare80[mite.rare80 == min(mite.rare80)]
mite.rare80[mite.rare80 == max(mite.rare80)]
# Observed core with smallest predicted species richness
mite[which(mite.rare80 == min(mite.rare80)), ]
# Observed core with largest predicted species richness
mite[which(mite.rare80 == max(mite.rare80)), ]

# Rarefaction curve
dev.new(
  title = "Rarefaction curves",
  noRStudioGD = TRUE
)
rarecurve(
  mite[-67, ],
  step = 1,
  sample = 80,
  xlab = "Number of individuals (Sample Size)",
  ylab = "Species",
  label = TRUE,
  col = "blue"
)

# Same plot but including core #67:
# rarecurve(
#   mite,
#   step = 1,
#   sample = 80,
#   xlab = "Number of individuals (Sample Size)",
#   ylab = "Species",
#   label = TRUE,
#   col = "blue"
# )

# Beta diversity, LCBD and SCBD of the fish data ==================

# Gamma richness and expected species pool
?specpool
(gobs <- ncol(spe))
(gthe <- specpool(spe))


# Multiplicative partitioning of Hill numbers (Jost 2006, 2007)
?d
# Mean alpha species richness
d(spe, lev = "alpha", q = 0)
# Mean alpha Shannon diversity
d(spe, lev = "alpha", q = 1)
# Mean alpha Simpson diversity
d(spe, lev = "alpha", q = 2, boot = TRUE)

# Multiplicative beta species richness
d(spe, lev = "beta", q = 0)
# Multiplicative beta Shannon diversity
d(spe, lev = "beta", q = 1)
# Multiplicative beta Simpson diversity
d(spe, lev = "beta", q = 2, boot = TRUE)

# Gamma species richness
d(spe, lev = "gamma", q = 0)
# Gamma Shannon diversity
d(spe, lev = "gamma", q = 1)
# Gamma Simpson diversity
d(spe, lev = "gamma", q = 2, boot = TRUE)

# Plot multiplicative beta diversity vs order
mbeta <- data.frame(order = 0:20, beta = NA, se = NA)
for (i in 1:nrow(mbeta)) {
  out <- d(spe, lev = "beta", q = mbeta$order[i], boot = TRUE)
  mbeta$beta[i] <- out$D.Value
  mbeta$se[i] <- out$StdErr
}
mbeta
dev.new(
  title = "Multiplicative beta diversity",
  width = 6,
  height = 4,
  noRStudioGD = TRUE
)
ggplot(mbeta, aes(order, beta)) +
  geom_point() +
  geom_line() +
  geom_errorbar(
    aes(order, beta, ymin = beta - se, ymax = beta + se),
    width = 0.2
  ) +
  labs(
    y = "Multiplicative beta diversity",
    x = "Order of the diversity measure"
  )


# MacArthur's homogeneity measure (MacArthur 1965)
hom <- data.frame(order = 0:20, homogeneity = NA, se = NA)
for (i in 1:nrow(hom)) {
  out <- M.homog(spe, q = hom$order[i], std = TRUE, boot = TRUE)
  hom$homogeneity[i] <- out$M
  hom$se[i] <- out$StdErr
}
hom
dev.new(
  title = "Homogeneity measures",
  width = 6,
  height = 4,
  noRStudioGD = TRUE
)
ggplot(hom, aes(order, homogeneity)) +
  geom_point() +
  geom_line() +
  geom_errorbar(
    aes(order, homogeneity, ymin = homogeneity - se, ymax = homogeneity + se),
    width = 0.2
  ) +
  labs(
    y = "MacArthur's homogeneity measure",
    x = "Order of the diversity measure"
  )


# Computation using beta.div {adespatial} on
# Hellinger-transformed species data
spe.beta <- beta.div(spe, method = "hellinger", nperm = 9999)
summary(spe.beta)
spe.beta$beta # SSTotal and BDTotal

# Which species have a SCBD larger than the mean SCBD?
spe.beta$SCBD[spe.beta$SCBD >= mean(spe.beta$SCBD)]

### NOT IN THE BOOK ###

# Plot of the species with large SCBD
dev.new(
  title = "SCBD of 5 fish species",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 3))
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  col = "brown",
  cex = spe$Satr,
  main = "Brown trout",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  col = "brown",
  cex = spe$Phph,
  main = "Eurasian minnow",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  col = "brown",
  cex = spe$Babl,
  main = "Stone loach",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  col = "brown",
  cex = spe$Ruru,
  main = "Roach",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  col = "brown",
  cex = spe$Alal,
  main = "Bleak",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

### END NOT IN THE BOOK ###

# LCBD values
spe.beta$LCBD
# p-values
spe.beta$p.LCBD
# Holm correction
p.adjust(spe.beta$p.LCBD, "holm")
# Sites with significant Holm-corrected LCBD value
row.names(spe[which(p.adjust(spe.beta$p.LCBD, "holm") <= 0.05), ])

# Plot the LCBD values on the river map
dev.new(title = "LCBD of the 29 sites", noRStudioGD = TRUE)
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  pch = 21,
  col = "white",
  bg = "brown",
  cex = spe.beta$LCBD * 70,
  main = "LCBD values",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
text(85, 11, "***", cex = 1.2, col = "red")
text(80, 92, "***", cex = 1.2, col = "red")


# Replacement, richness and nestedness indices of the fish data ===

# Jaccard-based Podani indices (presence-absence data)
fish.pod.j <- beta.div.comp(spe, coef = "J", quant = FALSE)
# What is in the output object?
summary(fish.pod.j)
# Display summary statistics:
fish.pod.j$part
# Extraction of the richness difference matrix
fish.rich <- as.matrix(fish.pod.j$rich)
# Plot of the richness difference with respect to site 30
fish.rich.30 <- fish.rich[29, ][-29]
site.names <- seq(1, 29)[-8]
dev.new(
  title = "Doubs fish data: richness difference with respect to site 30 - Jaccard",
  width = 12,
  height = 5,
  noRStudioGD = TRUE
)
plot(
  site.names,
  fish.rich.30,
  type = "n",
  xaxp = c(1, 29, 28),
  main = "Doubs fish data: richness difference with respect to site 30",
  xlab = "Site number",
  ylab = "Richness difference"
)
lines(site.names, fish.rich.30, pch = 24, col = "red")
points(
  site.names,
  fish.rich.30,
  pch = 24,
  cex = 1.5,
  col = "white",
  bg = "red"
)
text(3, 0.85, "Upstream", cex = 1, col = "red")
text(27, 0.85, "Downstream", cex = 1, col = "red")

# Extraction of the replacement matrix
fish.repl <- as.matrix(fish.pod.j$repl)
# Extraction of the Jaccard dissimilarity Dj matrix
fish.jac <- as.matrix(fish.pod.j$D)

# Plot of the Jaccard, replacement and richness difference indices
# between nearest neighbours
# First, extract the subdiagonals of the square dissimilarity
# matrices
fish.repl.neigh <- diag(fish.repl[-1, ]) # Replacement
fish.rich.neigh <- diag(fish.rich[-1, ]) # Richness difference
fish.jac.neigh <- diag(fish.jac[-1, ]) # Jaccard Dj index

dev.new(
  title = "Replacement - Richness difference - Jaccard - nearest neighbours",
  width = 12,
  height = 5,
  noRStudioGD = TRUE
)
absc <- c(2:7, 9:30) # Abscissa
label.pairs <- c(
  "1-2",
  "2-3",
  "3-4",
  "4-5",
  "5-6",
  "6-7",
  " ",
  "7-9",
  "9-10",
  "10-11",
  "11-12",
  "12-13",
  "13-14",
  "14-15",
  "15-16",
  "16-17",
  "17-18",
  "18-19",
  "19-20",
  "20-21",
  "21-22",
  "22-23",
  "23-24",
  "24-25",
  "25-26",
  "26-27",
  "27-28",
  "28-29",
  "29-30"
)
plot(
  absc,
  fish.jac.neigh,
  type = "n",
  xaxt = "n",
  main = "Replacement - Richness difference - Jaccard - nearest neighbours",
  xlab = "Site pairs",
  ylab = "Podani's indices"
)
axis(side = 1, 2:30, labels = label.pairs, las = 2, cex.axis = 0.9)
lines(absc, fish.jac.neigh, col = "black")
points(
  absc,
  fish.jac.neigh,
  pch = 21,
  cex = 2,
  col = "black",
  bg = "black"
)
lines(absc, fish.repl.neigh, col = "blue")
points(
  absc,
  fish.repl.neigh,
  pch = 22,
  cex = 2,
  col = "white",
  bg = "blue"
)
lines(absc, fish.rich.neigh, col = "red")
points(
  absc,
  fish.rich.neigh,
  pch = 24,
  cex = 2,
  col = "white",
  bg = "red"
)
legend(
  "top",
  c("Jaccard D", "Replacement", "Richness difference"),
  pch = c(16, 15, 17),
  col = c("black", "blue", "red")
)

# Triangular plots
# Jaccard
fish.pod.J <- beta.div.comp(spe, coef = "J", quant = FALSE)
# Sorensen
fish.pod.S <- beta.div.comp(spe, coef = "S", quant = FALSE)
# Ruzicka
fish.pod.qJ <- beta.div.comp(spe, coef = "J", quant = TRUE)
# Percentage difference
fish.pod.qS <- beta.div.comp(spe, coef = "S", quant = TRUE)
# Data frames for the triangular plots
fish.pod.J.3 <- cbind((1 - fish.pod.J$D), fish.pod.J$repl, fish.pod.J$rich)
colnames(fish.pod.J.3) <- c("Similarity", "Repl", "RichDiff")
fish.pod.S.3 <- cbind((1 - fish.pod.S$D), fish.pod.S$repl, fish.pod.S$rich)
colnames(fish.pod.S.3) <- c("Similarity", "Repl", "RichDiff")
fish.pod.qJ.3 <- cbind((1 - fish.pod.qJ$D), fish.pod.qJ$repl, fish.pod.qJ$rich)
colnames(fish.pod.qJ.3) <- c("Similarity", "Repl", "AbDiff")
fish.pod.qS.3 <- cbind((1 - fish.pod.qS$D), fish.pod.qS$repl, fish.pod.qS$rich)
colnames(fish.pod.qS.3) <- c("Similarity", "Repl", "AbDiff")

dev.new(
  title = "Similarity - Replacement - Richness difference - Triangular plots",
  width = 10,
  height = 10,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
triangle.plot(
  as.data.frame(fish.pod.J.3[, c(3, 1, 2)]),
  show = FALSE,
  labeltriangle = FALSE,
  addmean = TRUE
)
text(-0.45, 0.5, "RichDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "Jaccard similarity", cex = 1.5)
triangle.plot(
  as.data.frame(fish.pod.S.3[, c(3, 1, 2)]),
  show = FALSE,
  labeltriangle = FALSE,
  addmean = TRUE
)
text(-0.45, 0.5, "RichDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "Sørensen similarity", cex = 1.5)

triangle.plot(
  as.data.frame(fish.pod.qJ.3[, c(3, 1, 2)]),
  show = FALSE,
  labeltriangle = FALSE,
  addmean = TRUE
)
text(-0.45, 0.5, "AbDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "S = 1 – Ružička D", cex = 1.5)
triangle.plot(
  as.data.frame(fish.pod.qS.3[, c(3, 1, 2)]),
  show = FALSE,
  labeltriangle = FALSE,
  addmean = TRUE
)
text(-0.45, 0.5, "AbDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "S = 1 – Percentage difference", cex = 1.5)
# Display values of the mean points in the triangular plots
colMeans(fish.pod.J.3[, c(3, 1, 2)])
colMeans(fish.pod.S.3[, c(3, 1, 2)])
colMeans(fish.pod.qJ.3[, c(3, 1, 2)])
colMeans(fish.pod.qS.3[, c(3, 1, 2)])


# Explaining replacement and richness difference
# by means of db-RDA

# Replacement
repl.dbrda <- dbrda(fish.repl ~ ., data = env, add = "cailliez")
anova(repl.dbrda)
RsquareAdj(repl.dbrda)

# Richness difference
rich.dbrda <- dbrda(fish.rich ~ ., data = env, add = "cailliez")
anova(rich.dbrda)
RsquareAdj(rich.dbrda)

dev.new(
  title = "RDA Richness difference explained by environment",
  noRStudioGD = TRUE
)
plot(
  rich.dbrda,
  scaling = 1,
  display = c("lc", "cn"),
  main = "Richness difference explained by environmental variables"
)


# Functional and phylogenetic diversity ===========================

summary(fishtraits)
rownames(fishtraits)
names(spe)
names(fishtraits)
tra <- fishtraits[, 6:15]
tra

# Distance-based functional diversity indices
?dbFD
dev.new(
  title = "Functional groups",
  noRStudioGD = TRUE
)

# In the code below to create object res, run the function and
# then answer the questions as suggested below,
# i.e., g then 10.

res <-
  dbFD(
    tra,
    as.matrix(spe),
    asym.bin = 5:10,
    stand.FRic = TRUE,
    clust.type = "ward.D",
    CWM.type = "all",
    calc.FGR = TRUE
  )
# g  # cut the dendrogram using the number of groups as criterion
# 10 # choose the number of functional groups
res

dev.new(
  title = "Functional diversity along the Doubs river",
  width = 10,
  #  height = 5,
  height = 10,
  noRStudioGD = TRUE
)
# par(mfrow = c(1, 2))
par(mfrow = c(3, 2))
plot(
  spa,
  asp = 1,
  pch = 21,
  cex.axis = 0.8,
  col = "white",
  bg = "brown",
  cex = res$FRic * 5,
  main = "Functional richness",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  pch = 21,
  cex.axis = 0.8,
  col = "white",
  bg = "brown",
  cex = res$FEve * 6,
  main = "Functional evenness",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

plot(
  spa,
  asp = 1,
  pch = 21,
  cex.axis = 0.8,
  col = "white",
  bg = "brown",
  cex = res$FDiv * 5,
  main = "Functional divergence",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  pch = 21,
  cex.axis = 0.8,
  col = "white",
  bg = "brown",
  cex = res$FDis * 1.5,
  main = "Functional dispersion",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

plot(
  spa,
  asp = 1,
  pch = 21,
  cex.axis = 0.8,
  col = "white",
  bg = "brown",
  cex = res$RaoQ / 2,
  main = "Rao quadratic entropy",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  pch = 21,
  cex.axis = 0.8,
  col = "white",
  bg = "brown",
  cex = res$FGR / 2,
  main = "Functional group richness",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

# Add these indices to the div data frame
div$FRic <- res$FRic
div$FEve <- res$FEve
div$FDiv <- res$FDiv
div$FDis <- res$FDis
div$RaoQ <- res$RaoQ
div$FGR <- res$FGR
div

# Community-weighted mean trait values (CWMs)
functcomp(tra, as.matrix(spe), CWM.type = "all")
# dev.new(
#   title = "CWMs along the Doubs river",
#   width = 12,
#   height = 9,
#   noRStudioGD = TRUE
# )
# par(mfrow = c(2, 2))
# plot(spa,
#      asp = 1,
#      pch = 21,
#      col = "brown",
#      bg = "orange",
#      cex = (res$CWM$BodyLength - 100) / 40,
#      main = "CWM Body Length",
#      xlab = "x coordinate (km)",
#      ylab = "y coordinate (km)"
# )
# lines(spa, col = "light blue")
#
# plot(spa,
#      asp = 1,
#      pch = 21,
#      col = "brown",
#      bg = "orange",
#      cex = res$CWM$ShapeFactor,
#      main = "CWM Shape Factor",
#      xlab = "x coordinate (km)",
#      ylab = "y coordinate (km)"
# )
# lines(spa, col = "light blue")
#
# plot(spa,
#      asp = 1,
#      pch = 21,
#      col = "brown",
#      bg = "orange",
#      cex = res$CWM$TrophicLevel,
#      main = "CWM Trophic Level",
#      xlab = "x coordinate (km)",
#      ylab = "y coordinate (km)"
# )
# lines(spa, col = "light blue")

# Distance matrix based on a simplified phylogenetic classification
# Retrieve hierarchical classification from species list
splist <- as.character(fishtraits$LatinName)
# Database ncbi - less reliable than GBIF
# spcla <- classification(splist, db = "ncbi")
spcla <- classification(splist, db = "gbif")

# ------------------------------------------------------------
# WARNING: depending on the taxonomic status of the species,
# the function classification() may stop and ask the user to choose
# between two (or more) names. However, if this script is copied
# and pasted as a whole, then NAs will be produced and some species
# will be deleted. This will induce errors in the next steps of
# the analysis. To avoid that, run the "spcla <- classification..."
# only and make your choice. The function will then proceed.
# ------------------------------------------------------------

# Compute the distance matrix and the phylogenetic tree
tr <- class2tree(spcla)
tr$classification
tr$distmat
# Convert the tree to a cophenetic matrix
# constrained between 0 and 1
phylo.d <- cophenetic(tr$phylo) / 100
# Replace full species names by name codes
rownames(phylo.d) <- names(spe)
colnames(phylo.d) <- names(spe)

# Functional dissimilarity matrix (Gower dissimilarity)
trait.d <- gowdis(tra, asym.bin = 5:10)

# Plot the tree and the dendrogram
trait.gw <- hclust(trait.d, "ward.D2")
dev.new(
  title = "Taxonomic and functional species classification",
  width = 12,
  height = 7,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
plot(tr)
text(0.1, 27.5, "a", cex = 1.8)
plot(trait.gw, hang = -1, main = "")
text(26, 1.045, "b", cex = 1.8)

# Additive partitioning of TD, FD and PD ######
# (de Bello et al. 2010)
spe.rao <- Rao(
  sample = t(spe),
  dfunc = trait.d, # optional functional dissimilarity matrix
  dphyl = phylo.d, # optional phylogenetic distance matrix
  weight = FALSE,
  Jost = TRUE,
  structure = NULL
)
names(spe.rao)

# Species diversity (Simpson)
names(spe.rao$TD)
# Mean alpha Simpson diversity
spe.rao$TD$Mean_Alpha
# Gamma Simpson diversity
spe.rao$TD$Gamma
# Additive beta Simpson diversity
spe.rao$TD$Beta_add
spe.rao$TD$Gamma - spe.rao$TD$Mean_Alpha
# Beta diversity expressed as percentage of gamma
spe.rao$TD$Beta_prop
spe.rao$TD$Beta_add / spe.rao$TD$Gamma
# Multiplicative beta Simpson diversity
spe.rao$TD$Gamma / spe.rao$TD$Mean_Alpha

# Phylogenetic diversity (Rao)
names(spe.rao$PD)
# Mean alpha Rao phylogenetic diversity
spe.rao$PD$Mean_Alpha
# Gamma Rao phylogenetic diversity
spe.rao$PD$Gamma
# Additive beta Rao phylogenetic diversity
spe.rao$PD$Beta_add
spe.rao$PD$Gamma - spe.rao$PD$Mean_Alpha
# Beta phylogenetic diversity expressed as percentage of gamma
spe.rao$PD$Beta_prop
spe.rao$PD$Beta_add / spe.rao$PD$Gamma
# Multiplicative beta Rao phylogenetic diversity
spe.rao$PD$Gamma / spe.rao$PD$Mean_Alpha

# Functional diversity (Rao)
names(spe.rao$FD)
# Mean alpha Rao functional diversity
spe.rao$FD$Mean_Alpha
# Gamma Rao functional diversity
spe.rao$FD$Gamma
# Additive beta Rao functional diversity
spe.rao$FD$Beta_add
spe.rao$FD$Gamma - spe.rao$FD$Mean_Alpha
# Beta functional diversity expressed as percentage of gamma
spe.rao$FD$Beta_prop
spe.rao$FD$Beta_add / spe.rao$FD$Gamma
# Multiplicative beta Rao functional diversity
spe.rao$FD$Gamma / spe.rao$FD$Mean_Alpha

# Variation of alpha TD, PD and FD along the Doubs river
spe.rao$TD$Alpha
spe.rao$PD$Alpha
spe.rao$FD$Alpha

dev.new(
  title = "TD, PD and FD along the Doubs river",
  width = 10,
  height = 7.5,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  pch = 21,
  col = "white",
  bg = "brown",
  cex = spe.rao$TD$Alpha / 4,
  main = "Taxonomic diversity",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")
plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  pch = 21,
  col = "white",
  bg = "brown",
  cex = spe.rao$PD$Alpha * 1.5,
  main = "Phylogenetic diversity",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

plot(
  spa,
  asp = 1,
  cex.axis = 0.8,
  pch = 21,
  col = "white",
  bg = "brown",
  cex = spe.rao$FD$Alpha * 2.5,
  main = "Functional diversity",
  xlab = "x coordinate (km)",
  ylab = "y coordinate (km)"
)
lines(spa, col = "light blue")

# Add Rao-based diversity indices to the div data frame
div$alphaTD <- spe.rao$TD$Alpha
div$alphaPD <- spe.rao$PD$Alpha
div$alphaFD <- spe.rao$FD$Alpha

# Save the div data frame as a CSV file
write.csv(div, file = "diversity.csv", quote = FALSE)

# Save the species classification as a CSV file
write.csv(tr$classification, file = "classification.csv", quote = FALSE)
