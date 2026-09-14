# BDC334 Lab 2b: model Doubs River analysis (Tasks 1--4 and 8)
# Save this script and DoubsEnv.csv in the same working directory.
# Tasks 5--7, including the coastal-data interpretation, are answered
# separately in the lecture. This script uses base R only.
# Marks total 14: Q1 = 2 (code 1, output 1); Q2 = 0;
# Q3 = 2 (code 1, output 1); Q4 = 6 (two graph codes 2,
# two publication-quality graphs 2, two explanations 2); Q5--7 = 0;
# Q8 = 4 (distance code 1, distance figure 1, correlation code 1,
# correct pairwise correlations 1). There is no separate formatting mark.

# Q1. Import all 30 sites. The first column holds site identifiers.
doubs <- read.csv("DoubsEnv.csv", row.names = 1)
stopifnot(nrow(doubs) == 30, ncol(doubs) == 11,
          all(vapply(doubs, is.numeric, logical(1))), !anyNA(doubs))

# Q2 (no marks). The raw variables have different scales. Altitude spans
# 172--934, pH spans 7.7--8.6, and ammonium spans 0--1.8.
# Raw Euclidean distances would give much more weight to large numbers.
raw_summary <- data.frame(
  minimum = vapply(doubs, min, numeric(1)),
  maximum = vapply(doubs, max, numeric(1)),
  mean = vapply(doubs, mean, numeric(1)),
  SD = vapply(doubs, sd, numeric(1))
)
print(round(raw_summary, 3))

# Q1 continued. Centre and scale each variable using sample SDs.
# The site identifier is excluded; dfs (distance from source) is retained
# initially to match Lab 1's 11-variable definition of the environment.
doubs_z <- scale(doubs)
print(round(doubs_z[1:5, 1:6], 3))
print(round(rbind(mean = colMeans(doubs_z),
                  SD = apply(doubs_z, 2, sd)), 6))
stopifnot(max(abs(colMeans(doubs_z))) < 1e-10,
          max(abs(apply(doubs_z, 2, sd) - 1)) < 1e-10)

# Q3. Calculate site-to-site distances at full precision.
# The complete matrix is symmetric, non-negative and has a zero diagonal.
# Distances describe measured environmental differences, not kilometres.
doubs_D <- as.matrix(dist(doubs_z, method = "euclidean"))
print(round(doubs_D[1:5, 1:5], 3))
stopifnot(isTRUE(all.equal(doubs_D, t(doubs_D))),
          all(diag(doubs_D) == 0), all(doubs_D >= 0))
print(round(c(site_11_to_13 = doubs_D["11", "13"],
              site_1_to_25 = doubs_D["1", "25"]), 3))

# Q4. Two labelled graphs: a longitudinal gradient and a local perturbation.
# Each graph can earn 1 mark for code, 1 for a correct publication-quality
# figure with full axis labels and units, and 1 for its explanation.
old_par <- par(no.readonly = TRUE)
par(mar = c(4.5, 5, 1.5, 1), mgp = c(3, 0.8, 0), las = 1)
plot(doubs$dfs, doubs$alt, type = "b", pch = 16,
     col = "indianred", xlab = "Distance from source (km)",
     ylab = "Altitude (m)")
# Explanation 1: Altitude decreases downstream. This broad gradient
# contributes to environmental differences among sites. It does not
# establish that altitude caused all the other environmental changes.

plot(doubs$dfs, doubs$oxy, type = "n",
     xlab = "Distance from source (km)",
     ylab = expression("Dissolved oxygen (mg " * L^{-1} * ")"))
rect(doubs$dfs[23], par("usr")[3], doubs$dfs[25], par("usr")[4],
     col = adjustcolor("goldenrod", alpha.f = 0.15), border = NA)
lines(doubs$dfs, doubs$oxy, type = "b", pch = 16, col = "indianred")
par(old_par)
# Explanation 2: Low oxygen around Sites 23--25 followed by higher
# concentrations downstream is consistent with a local perturbation.
# Microbial processing of organic inputs could consume oxygen. The high
# oxygen demand at these sites supports investigating that explanation;
# oxygen supply and reaeration must also be measured.

# Supplementary exploration: inspect every raw variable along the river.
# Free vertical scales preserve each variable's original measurements.
# Sites are unequally spaced; dfs is a better spatial axis than row number.
old_par <- par(no.readonly = TRUE)
par(mfrow = c(4, 3), mar = c(3.2, 3.5, 2, 0.8), mgp = c(2, 0.65, 0))
for (variable in names(doubs)) {
  plot(doubs$dfs, doubs[[variable]], type = "b", pch = 16, cex = 0.5,
       col = "indianred", xlab = "Distance from source (km)",
       ylab = variable, main = variable)
  points(doubs$dfs[23:25], doubs[[variable]][23:25],
         pch = 1, cex = 1.2, col = "black")
}
par(old_par)

# Altitude decreases and flow generally increases downstream. Drainage
# and tributary inputs could explain increasing flow. Site 1 has an
# unusually steep slope. Sites 23--25 have high nutrient concentrations
# and oxygen demand, with low oxygen; downstream, several indicators
# move back towards earlier values. These observations need explanations.

# Q8. Recalculate the Euclidean distances and print the full table before
# the figure. Marks: 1 for distance code and 1 for the correct figure.
doubs_D <- as.matrix(dist(scale(doubs), method = "euclidean"))
print(round(doubs_D, 3))

# View the complete environmental-distance matrix.
# The axes are site identities in river order, not geographic coordinates.
old_par <- par(no.readonly = TRUE)
layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))
par(mar = c(4.5, 4.5, 2, 1))
distance_colours <- hcl.colors(40, "viridis")
image(seq_len(nrow(doubs)), seq_len(nrow(doubs)), doubs_D,
      col = distance_colours, zlim = range(doubs_D),
      xlab = "Site", ylab = "Site",
      main = "Standardised environmental distance", asp = 1)
# The colour key makes the size of a distance readable from the figure.
par(mar = c(4.5, 1, 2, 3))
key_values <- seq(0, max(doubs_D), length.out = 100)
image(1, key_values, matrix(key_values, nrow = 1),
      col = distance_colours, axes = FALSE, xlab = "", ylab = "")
axis(4, las = 1)
mtext("Distance", side = 3, line = 0.5)
par(old_par)

# Q8 continued: 1 mark for correlation code and 1 for correct pairwise
# correlations. Correlations compare variables across sites, not distances.
# Linear standardisation does not change Pearson's correlation.
doubs_cor <- cor(doubs, method = "pearson")
print(round(doubs_cor, 2))
stopifnot(isTRUE(all.equal(doubs_cor, cor(doubs_z))))

# Plot all pairwise relationships immediately after the correlation output.
# Lower panels: raw data; upper panels: Pearson's r; diagonal: variable codes.
# The column variable is on x and the row variable is on y in each scatterplot.
# Red points are Sites 23--25. Inspect unusual sites and possible curvature.
panel_cor <- function(x, y, ...) {
  old_usr <- par("usr")
  on.exit(par(usr = old_usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.5, sprintf("%.2f", cor(x, y)), cex = 0.9)
}
pairs(doubs, upper.panel = panel_cor,
      pch = 16, cex = 0.5, cex.labels = 0.9, gap = 0.4,
      col = ifelse(seq_len(nrow(doubs)) %in% 23:25, "indianred", "grey35"))

# Phosphate and ammonium correlate strongly (r about 0.97). Shared inputs
# are a hypothesis to test with samples above, within and below inputs.
# Oxygen and biological oxygen demand correlate negatively (r about -0.84).
# Microbial processing of organic inputs could consume oxygen, but oxygen
# also depends on supply, temperature and reaeration. Measure these terms
# before concluding which process caused the pattern.
# Hardness rises broadly downstream (dfs--har r about 0.70). Geology,
# groundwater and tributary chemistry are possible explanations to test.
# A matrix does not identify pollution sources or prove causation. These
# connected river sites are not automatically 30 independent replicates.

# Supplementary sensitivity check (no additional marks): remove longitudinal
# position from environmental distance.
# This changes the question, not the underlying measurements. All remaining
# variables are still standardised across all 30 sites. Correlated variables
# also retain shared information: z-scores do not remove that dependence.
conditions <- doubs[, names(doubs) != "dfs"]
conditions_D <- as.matrix(dist(scale(conditions)))
print(round(c(with_dfs = doubs_D["11", "13"],
              without_dfs = conditions_D["11", "13"]), 3))
plot(doubs_D[lower.tri(doubs_D)], conditions_D[lower.tri(conditions_D)],
     pch = 16, cex = 0.6, col = "indianred",
     xlab = "Distance using all 11 variables",
     ylab = "Distance excluding dfs")
abline(a = 0, b = 1, lty = 2)

# Conclude with the evidence: a broad downstream gradient plus a marked
# local water-quality change, with several plausible mechanisms. To infer
# community responses, we would also need the species data. To establish
# causes, we would need targeted measurements or interventions.
