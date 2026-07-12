library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(grid)
library(gridBase)

spp <- read.csv(here::here("data", "BCB743", "seaweed", "SeaweedSpp.csv"))
spp <- dplyr::select(spp, -1)
dim(spp)

Y.core <- betapart.core(spp)
Y.pair <- beta.pair(Y.core, index.family = "sor")

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)

load(here::here("data", "BCB743", "seaweed", "SeaweedEnv.RData"))
dim(env)

E1 <- dplyr::select(
  env,
  febMean,
  febRange,
  febSD,
  augMean,
  augRange,
  augSD,
  annMean,
  annRange,
  annSD
)

E1 <- decostand(E1, method = "standardize")

bioreg <- read.csv(here::here("data", "BCB743", "seaweed", "bioregions.csv"))
head(bioreg)

sites <- read.csv(here::here("data", "BCB743", "seaweed", "SeaweedSites.csv"))
sites <- sites[, c(2, 1)]
head(sites)

# fit the full model:
cap_full <- capscale(Y1 ~ ., E1)
# cap_full <- capscale(spp ~., E1, dist = "bray", add = TRUE)
cap_full

sum(cap_full$CCA$eig) # constrained
sum(cap_full$CA$eig) # unconstrained

sum(cap_full$CA$eig) + sum(cap_full$CCA$eig) # real total

# prop var explained for first axis:
cap_full$CCA$eig[1] / (sum(cap_full$CA$eig)