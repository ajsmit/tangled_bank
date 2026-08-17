<div id="quarto-document-content" class="content" role="main">

<div id="title-block-header" class="quarto-title-block default">

<div class="quarto-title">

# Linking Communities Across Landscapes: A Quantitative Introduction to Metacommunity Ecology TASK

</div>

<div class="quarto-title-meta">

<div>

<div class="quarto-title-meta-heading">

Author

</div>

<div class="quarto-title-meta-contents">

B Effendi, A Waglay, C.E McIntyre

</div>

</div>

<div>

<div class="quarto-title-meta-heading">

Published

</div>

<div class="quarto-title-meta-contents">

July 24, 2026

</div>

</div>

</div>

</div>

<div id="metacommunity-practice-task" class="section level1" number="1">

# <span class="header-section-number">1</span> Metacommunity Practice Task

The **Palozzi2017** dataset, available from the CESTES database, contains plant community composition, environmental measurements and species functional traits collected from ten boreal peatland sites. Unlike the worked example in this chapter, this dataset does not contain spatial coordinates, so it cannot be used to investigate dispersal limitation or spatial structure. Instead, it provides an opportunity to investigate how environmental gradients influence community composition and functional diversity.

The workbook contains the following sheets:

- `comm` – community abundance matrix
- `envir` – environmental variables
- `traits` – species functional traits
- `splist` – species information
- `sitelist` – site information

<div id="setup" class="section level3" number="1.0.1">

### <span class="header-section-number">1.0.1</span> Setup

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(readxl)
library(vegan)
library(FD)

#############################
# Community matrix
#############################

comm.raw <- read_excel(
  "Palozzi2017_AJ.xlsx",
  sheet = "comm",
  col_names = FALSE
)

# Species names are stored in the second row
species <- as.character(unlist(comm.raw[2, 3:ncol(comm.raw)]))

# Remove the two header rows
comm <- comm.raw[-c(1, 2), ]

# Site names
rownames(comm) <- comm[[1]]

# Keep only abundance data
comm <- comm[, 3:ncol(comm)]

# Assign species names
colnames(comm) <- species

# Convert to numeric
comm <- as.data.frame(lapply(comm, as.numeric))

#############################
# Environmental variables
#############################

env <- read_excel(
  "Palozzi2017_AJ.xlsx",
  sheet = "envir"
)

rownames(env) <- env$Site

# Remove site identifiers
env <- env[, 3:ncol(env)]

# Standardise environmental variables
env.std <- as.data.frame(scale(env))

#############################
# Species traits
#############################

traits <- read_excel(
  "Palozzi2017_AJ.xlsx",
  sheet = "traits"
)

rownames(traits) <- traits$Sp

# Remove species identifiers
traits <- traits[, 3:ncol(traits)]
```

</div>

</div>

------------------------------------------------------------------------

</div>

<div id="question-1-how-different-are-the-plant-communities" class="section level2" number="1.1">

## <span class="header-section-number">1.1</span> Question 1: How different are the plant communities?

Calculate Bray–Curtis dissimilarities among the ten peatland communities and visualise the results using Non-metric Multidimensional Scaling (NMDS).

**Consider the following questions:**

- Do any communities cluster together?
- Are some communities clearly different from the others?
- Is the NMDS stress value acceptable?

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center collapsed" bs-toggle="collapse" bs-target=".callout-1-contents" aria-controls="callout-1" aria-expanded="false" aria-label="Toggle callout">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Show the answer

</div>

<div class="callout-btn-toggle d-inline-block border-0 py-1 ps-1 pe-0 float-end">

</div>

</div>

<div id="callout-1" class="callout-1-contents callout-collapse collapse">

<div class="callout-body-container callout-body">

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
bc <- vegdist(comm,
              method = "bray")

nmds <- metaMDS(comm,
                distance = "bray")
```

</div>

<div class="cell-output cell-output-stdout">

    Square root transformation
    Wisconsin double standardization
    Run 0 stress 9.93322e-05 
    Run 1 stress 9.484843e-05 
    ... New best solution
    ... Procrustes: rmse 0.1107177  max resid 0.313176 
    Run 2 stress 9.425218e-05 
    ... New best solution
    ... Procrustes: rmse 0.09173391  max resid 0.2539589 
    Run 3 stress 9.934373e-05 
    ... Procrustes: rmse 0.02081204  max resid 0.05810051 
    Run 4 stress 9.679696e-05 
    ... Procrustes: rmse 0.09173216  max resid 0.2594741 
    Run 5 stress 9.70336e-05 
    ... Procrustes: rmse 0.01886825  max resid 0.05264011 
    Run 6 stress 9.912532e-05 
    ... Procrustes: rmse 0.06404592  max resid 0.1807838 
    Run 7 stress 9.900426e-05 
    ... Procrustes: rmse 0.09172543  max resid 0.2594581 
    Run 8 stress 9.071397e-05 
    ... New best solution
    ... Procrustes: rmse 0.04141252  max resid 0.1163566 
    Run 9 stress 9.33794e-05 
    ... Procrustes: rmse 0.02143348  max resid 0.06048349 
    Run 10 stress 9.184704e-05 
    ... Procrustes: rmse 0.02439442  max resid 0.06801107 
    Run 11 stress 9.748682e-05 
    ... Procrustes: rmse 0.02589232  max resid 0.07214837 
    Run 12 stress 9.896462e-05 
    ... Procrustes: rmse 0.0347097  max resid 0.09637649 
    Run 13 stress 9.271614e-05 
    ... Procrustes: rmse 0.05160914  max resid 0.1459799 
    Run 14 stress 9.249429e-05 
    ... Procrustes: rmse 0.05161635  max resid 0.1459997 
    Run 15 stress 9.581803e-05 
    ... Procrustes: rmse 0.07566114  max resid 0.2056204 
    Run 16 stress 9.517391e-05 
    ... Procrustes: rmse 0.0434122  max resid 0.1200732 
    Run 17 stress 9.605561e-05 
    ... Procrustes: rmse 0.03613039  max resid 0.1002602 
    Run 18 stress 9.466211e-05 
    ... Procrustes: rmse 0.05160572  max resid 0.1459716 
    Run 19 stress 8.725708e-05 
    ... New best solution
    ... Procrustes: rmse 0.05160219  max resid 0.145962 
    Run 20 stress 9.657282e-05 
    ... Procrustes: rmse 0.0788073  max resid 0.2194246 
    *** Best solution was not repeated -- monoMDS stopping criteria:
        20: stress < smin

</div>

<div class="cell-output cell-output-stderr">

    Warning in metaMDS(comm, distance = "bray"): stress is (nearly) zero: you may
    have insufficient data

</div>

<div class="code-copy-outer-scaffold">

``` r
plot(nmds)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

<div class="code-copy-outer-scaffold">

``` r
stressplot(nmds)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

<div id="interpretation" class="section level3" number="1.1.1">

### <span class="header-section-number">1.1.1</span> Interpretation

Bray–Curtis dissimilarity measures differences in species composition between communities. The NMDS ordination provides a low-dimensional representation of these compositional differences. Communities that plot close together have similar species compositions, whereas communities that are farther apart are compositionally distinct. The stress value indicates how faithfully the ordination represents the original dissimilarity matrix, with lower values indicating a better fit.

</div>

</div>

</div>

</div>

------------------------------------------------------------------------

</div>

<div id="question-2-which-environmental-gradients-best-explain-community-composition" class="section level2" number="1.2">

## <span class="header-section-number">1.2</span> Question 2: Which environmental gradients best explain community composition?

Ecologists rarely fit every measured environmental variable into a single model, particularly when the number of sampling sites is small. Instead, they select variables that are ecologically meaningful and address a specific research question.

For this exercise, investigate whether four environmental variables:

- Soil pH (`pH`)
- Electrical conductivity (`Ec`)
- Soil moisture (`Moisture`)
- Phosphate concentration (`PO4`)

help explain differences in plant community composition among the peatland sites.

**Consider the following questions:**

- Is the overall dbRDA model statistically significant?
- Which environmental variables contribute most strongly to the observed patterns?
- Which communities appear to be associated with similar environmental conditions?
- What does this suggest about environmental selection within the metacommunity?

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center collapsed" bs-toggle="collapse" bs-target=".callout-2-contents" aria-controls="callout-2" aria-expanded="false" aria-label="Toggle callout">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Show the answer

</div>

<div class="callout-btn-toggle d-inline-block border-0 py-1 ps-1 pe-0 float-end">

</div>

</div>

<div id="callout-2" class="callout-2-contents callout-collapse collapse">

<div class="callout-body-container callout-body">

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Select a subset of environmentally meaningful variables
env.sub <- env.std[, c("pH", "Ec", "Moisture", "PO4")]

# Fit a distance-based redundancy analysis
db <- capscale(comm ~ .,
               data = env.sub,
               distance = "bray")

# Test the overall model
anova(db)
```

</div>

<div class="cell-output cell-output-stdout">

    Permutation test for capscale under reduced model
    Permutation: free
    Number of permutations: 999

    Model: capscale(formula = comm ~ pH + Ec + Moisture + PO4, data = env.sub, distance = "bray")
             Df SumOfSqs      F Pr(>F)  
    Model     4  2.06333 2.9366   0.03 *
    Residual  5  0.87828                
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

</div>

<div class="code-copy-outer-scaffold">

``` r
# Test the contribution of each environmental variable
anova(db, by = "margin")
```

</div>

<div class="cell-output cell-output-stdout">

    Permutation test for capscale under reduced model
    Marginal effects of terms
    Permutation: free
    Number of permutations: 999

    Model: capscale(formula = comm ~ pH + Ec + Moisture + PO4, data = env.sub, distance = "bray")
             Df SumOfSqs      F Pr(>F)  
    pH        1  0.69927 3.9809  0.029 *
    Ec        1  0.13656 0.7774  0.513  
    Moisture  1  0.36555 2.0811  0.134  
    PO4       1  0.05263 0.2996  0.847  
    Residual  5  0.87828                
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

</div>

<div class="code-copy-outer-scaffold">

``` r
# Ordination plot
plot(db,
     display = c("sites", "bp"),
     main = "dbRDA of Plant Communities")
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

<div id="interpretation-1" class="section level3" number="1.2.1">

### <span class="header-section-number">1.2.1</span> Interpretation

Distance-based redundancy analysis (dbRDA) is a constrained ordination that relates variation in community composition to measured environmental variables. By focusing on a small number of ecologically relevant predictors, the analysis avoids overfitting and produces a more interpretable model.

A significant overall permutation test indicates that the selected environmental variables explain a meaningful proportion of the differences among plant communities. The marginal tests identify which variables contribute significantly after accounting for the others. Environmental vectors pointing towards groups of sites indicate the gradients most strongly associated with community composition.

Within Vellend’s framework, significant environmental effects provide evidence that **environmental selection** is an important process structuring these peatland plant communities.

</div>

</div>

</div>

</div>

------------------------------------------------------------------------

</div>

<div id="question-3-which-environmental-variables-are-most-strongly-associated-with-community-composition" class="section level2" number="1.3">

## <span class="header-section-number">1.3</span> Question 3: Which environmental variables are most strongly associated with community composition?

The dbRDA in Question 2 investigated whether selected environmental variables explain differences in community composition. Another commonly used approach is to fit environmental vectors directly onto an unconstrained ordination.

Using the NMDS ordination from Question 1, fit the measured environmental variables onto the ordination using the `envfit()` function.

**Consider the following questions:**

- Which environmental variables are significantly associated with community composition?
- Which variables show the strongest correlations with the NMDS axes?
- How do the fitted vectors help explain the clustering of sites observed in Question 1?
- Do these results support the conclusions from the dbRDA?

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center collapsed" bs-toggle="collapse" bs-target=".callout-3-contents" aria-controls="callout-3" aria-expanded="false" aria-label="Toggle callout">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Show the answer

</div>

<div class="callout-btn-toggle d-inline-block border-0 py-1 ps-1 pe-0 float-end">

</div>

</div>

<div id="callout-3" class="callout-3-contents callout-collapse collapse">

<div class="callout-body-container callout-body">

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Fit environmental variables to the NMDS ordination
fit <- envfit(nmds,
              env.sub,
              permutations = 999)

# View the results
fit
```

</div>

<div class="cell-output cell-output-stdout">


    ***VECTORS

                   NMDS1       NMDS2     r2 Pr(>r)   
    pH       -0.00291362  1.00000000 0.9058  0.002 **
    Ec        0.00025138 -1.00000000 0.4319  0.156   
    Moisture  0.00222011  1.00000000 0.4574  0.114   
    PO4       0.00153240  1.00000000 0.2734  0.379   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    Permutation: free
    Number of permutations: 999

</div>

<div class="code-copy-outer-scaffold">

``` r
# Plot the NMDS with fitted environmental vectors
plot(nmds,
     main = "NMDS with Environmental Vectors")

plot(fit,
     p.max = 0.05,
     col = "red")
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

<div id="interpretation-2" class="section level3" number="1.3.1">

### <span class="header-section-number">1.3.1</span> Interpretation

The `envfit()` function projects environmental variables onto an unconstrained ordination and tests whether each variable is significantly correlated with patterns in community composition.

The direction of each arrow indicates the direction in which that environmental variable increases, while the length of the arrow reflects the strength of its relationship with the community data. Longer arrows indicate stronger correlations with the ordination, whereas shorter arrows indicate weaker relationships.

Only variables with **P \< 0.05** are displayed, allowing attention to focus on the most important environmental gradients influencing community composition.

Because `envfit()` is applied to the unconstrained NMDS ordination, it provides an independent assessment of environmental selection. If the same variables identified in the dbRDA are also significant in the `envfit()` analysis, confidence increases that these environmental gradients are important drivers of community assembly.

</div>

</div>

</div>

</div>

------------------------------------------------------------------------

</div>

<div id="question-4-synthesising-evidence-for-community-assembly" class="section level2" number="1.4">

## <span class="header-section-number">1.4</span> Question 4: Synthesising evidence for community assembly

The three analyses in this exercise each provide different insights into the processes that structure ecological communities.

Using the results from the **NMDS**, **dbRDA**, and **environmental vector fitting (envfit)**, evaluate which of **Vellend’s four fundamental processes** is best supported by the Palozzi2017 dataset.

In your discussion, consider the following:

- Which process is most strongly supported by the evidence?
- Which analyses support your conclusion?
- Which of Vellend’s processes cannot be evaluated using this dataset?
- What additional data would be required to investigate those processes?

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center collapsed" bs-toggle="collapse" bs-target=".callout-4-contents" aria-controls="callout-4" aria-expanded="false" aria-label="Toggle callout">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Show the answer

</div>

<div class="callout-btn-toggle d-inline-block border-0 py-1 ps-1 pe-0 float-end">

</div>

</div>

<div id="callout-4" class="callout-4-contents callout-collapse collapse">

<div class="callout-body-container callout-body">

The NMDS ordination demonstrates that plant communities differ in species composition among the sampled peatland sites, indicating that community assembly varies across the landscape. However, NMDS alone does not explain why these differences occur.

The dbRDA shows whether measured environmental variables explain a significant proportion of this variation in community composition. The environmental vector fitting (envfit) analysis complements the dbRDA by identifying which environmental gradients are most strongly correlated with the observed patterns in the unconstrained ordination. When similar environmental variables are identified by both analyses, there is strong evidence that environmental gradients influence community composition.

Together, these results provide the strongest support for **environmental selection** as the dominant process structuring these peatland communities. Environmental conditions act as ecological filters that favour species with traits suited to particular habitats.

The remaining three processes cannot be evaluated directly using this dataset. **Dispersal** cannot be assessed because no spatial coordinates or connectivity information were collected. **Historical speciation** requires evolutionary or phylogenetic data collected over much longer time scales than represented here. **Ecological drift** cannot be measured directly from these data, although unexplained variation in community composition may reflect stochastic ecological processes or other unmeasured environmental variables.

This exercise illustrates an important principle of quantitative ecology: the ecological questions that can be answered depend on the information contained within the dataset. Selecting appropriate analytical methods therefore requires understanding both the strengths and limitations of the available data.

</div>

</div>

</div>

</div>

<div id="learning-outcomes" class="section level2" number="1.5">

## <span class="header-section-number">1.5</span> Learning Outcomes

After completing this exercise, you should be able to:

- calculate Bray–Curtis dissimilarities to quantify differences in community composition;
- visualise patterns of beta diversity using Non-metric Multidimensional Scaling (NMDS);
- investigate the influence of environmental gradients using distance-based redundancy analysis (dbRDA);
- identify environmental variables associated with community composition using environmental vector fitting (`envfit()`); and
- synthesise evidence from multiple quantitative analyses to evaluate community assembly using Vellend’s four-process framework.

</div>

</div>

</div>
