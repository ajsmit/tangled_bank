<div id="quarto-document-content" class="content" role="main">

<div id="title-block-header" class="quarto-title-block default">

<div class="quarto-title">

<div class="quarto-title-block">

<div>

# Proposed 17: Statistical Methods of Comparing

Code

</div>

</div>

</div>

<div class="quarto-title-meta">

<div>

<div class="quarto-title-meta-heading">

Author

</div>

<div class="quarto-title-meta-contents">

Maya Nduvheni, Isa Valashiya, Malusi Zitha

</div>

</div>

<div>

<div class="quarto-title-meta-heading">

Published

</div>

<div class="quarto-title-meta-contents">

2026/07/24

</div>

</div>

</div>

</div>

> Procrustes never asked whether the traveller’s legs were the right length, he only asked whether they fit the bed. Statisticians ask the same question of their data, and call it “goodness of fit.”

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>**Material Required for This Chapter**

</div>

</div>

<div class="callout-body-container callout-body">

| Type | Name | Link |
|:---|:---|:---|
| **Slides** | Comparison methods lecture slides | [💾 `BCB743_comparison_methods.pdf`](../slides/BCB743_comparison_methods.pdf) |
| **Data** | The Doubs River data | [💾 `Doubs.RData`](../data/BCB743/NEwR-2ed_code_data/NeWR2-Data/Doubs.RData) |
| **Data** | Seaweed data | [💾 `seaweed`](../data/BCB743/seaweed) |

</div>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>In This Chapter

</div>

</div>

<div class="callout-body-container callout-body">

- State the null hypothesis of the Mantel test and how it differs from a Pearson correlation’s
- Interpret the Mantel statistic and its permutation p-value
- Identify the main legitimate ecological use of the Mantel test, and situations where it should not be used
- Explain what it means for two ordinations of the same sites to “agree”
- Run `procrustes()` and `protest()` in R on any pair of ordination objects from this module’s data
- Interpret the Procrustes sum of squares (m²), the Procrustes correlation √(1-m²), and PROTEST’s permutation p-value
- Extract, map, and interpret the Procrustean Association Metric (PAM)
- Read and annotate both the superimposition plot (`kind = 1`) and the residual plot (`kind = 2`)

</div>

</div>

<div id="overview" class="section level2">

## Overview

Community ecologists rarely work with a single matrix. A typical study has a site-by-species matrix, a site-by-environment matrix, and sometimes a site-by-space or site-by-trait matrix, which all describe the *same* sites, but not necessarily telling the same ecological story. Earlier chapters showed how to build these separate descriptions; this chapter asks whether they agree. It covers two complementary tools: the **Mantel test**, which asks whether two distance matrices computed on the same objects are correlated, and **Procrustes analysis** together with its permutation test, **PROTEST**, which asks whether two ordination configurations share the same shape.

By the end of the chapter you should be able to run and interpret both tools in R, choose the appropriate one for a given ecological question, and extract site-level diagnostics from a Procrustes fit rather than a single yes/no answer.

</div>

<div id="why-compare-two-descriptions-of-the-same-sites" class="section level2">

## Why Compare Two Descriptions of the Same Sites?

Two ordinations of the same sites don’t automatically produce matching descriptions. Hirst and Jackson (2007) showed that different ordination methods applied to the same community data can recover quite different site configurations when gradient lengths are long or sampling intensity is low, even when the underlying community is identical. Congruence between ordinations is therefore an empirical question, not something to assume, which is exactly what the two methods in this chapter test.

</div>

<div id="the-mantel-test" class="section level2">

## The Mantel Test

The Mantel test was introduced by Nathan Mantel (1967) in an epidemiological study of disease clustering. His problem was to detect whether leukaemia cases were clustered in both space and time simultaneously: if cases cluster, those close together in space should also be close together in time. His solution was to compute the cross product of a spatial and a temporal distance matrix across all site pairs, then test whether it exceeded the value expected under random permutation. In ecology, “the Mantel test” has since come to mean any test relating two distance or resemblance matrices, whatever they represent.

<div id="the-statistic" class="section level3">

### The Statistic

Given two n × n distance matrices <span class="math inline">\\D_Y\\</span> and <span class="math inline">\\D_X\\</span>, the Mantel statistic <span class="math inline">\\r_M\\</span> is the Pearson correlation between the <span class="math inline">\\n(n-1)/2\\</span> upper-triangle elements of <span class="math inline">\\D_Y\\</span> and the corresponding elements of <span class="math inline">\\D_X\\</span>, treated as two long vectors. It ranges from -1 to +1. Significance is assessed by permuting the rows and columns of one matrix while holding the other fixed, recomputing <span class="math inline">\\r_M\\</span> each time to build a null distribution.

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span><span class="math inline">\\r_M\\</span> Is Not a Coefficient of Determination

</div>

</div>

<div class="callout-body-container callout-body">

Legendre and Fortin (2010) showed that the sum of squares partitioned by a Mantel test, <span class="math inline">\\SS(d_Y)\\</span>, is not equal to, nor a simple function of, the sum of squares of the raw response data, <span class="math inline">\\SS(Y)\\</span>. So <span class="math inline">\\r_M\\</span> is not a Pearson <span class="math inline">\\r\\</span> between the underlying variables, and <span class="math inline">\\r_M^2\\</span> cannot be read as a proportion of variance explained. Simulation confirms that the mean Mantel statistic is consistently smaller than the true correlation between the underlying variables (Legendre and Fortin (2010), Table 2). Legendre, Fortin and Borcard (2015) revisit the question directly and conclude that the Mantel test should be reserved for hypotheses that can only be formulated in terms of distances, not used as a general substitute for regression or canonical analysis on the raw data.

</div>

</div>

</div>

<div id="the-null-hypothesis" class="section level3">

### The Null Hypothesis

The null hypothesis of the Mantel test is not the independence of two random variables. It is that distances among objects in <span class="math inline">\\D_Y\\</span> are not (linearly or monotonically) related to the corresponding distances in <span class="math inline">\\D_X\\</span> \[Legendre and Legendre (2012, p. 600)\]. Because the two null hypotheses differ, a Mantel test and a correlation or canonical analysis of the same underlying data are testing different things, by using a Mantel test when raw-data analysis is possible usually means testing the wrong hypothesis.

</div>

</div>

<div id="applications-and-limitations" class="section level2">

## Applications and Limitations

**Not a Pearson correlation in disguise.** Pearson <span class="math inline">\\r\\</span> asks whether raw values of X predict raw values of Y; <span class="math inline">\\r_M\\</span> asks whether large distances in one matrix line up with large distances in the other. The two questions look similar but are structurally different.

**Sensitive to the choice of distance measure.** Jackson (1995) showed that switching to a distance measure that weights nearby sites more heavily can flip which relationships are significant, using the same underlying data.

**No support for more than two matrices, and no site-level diagnosis.** A single <span class="math inline">\\r_M\\</span> collapses the whole multivariate configuration into one number per site pair, so there is no way to see which individual sites are driving a mismatch. Working outside community ecology, Fortin & Borcard (2015) compared the Mantel test against Procrustes-based approaches on simulated paleocommunity data and reached the same conclusion reached elsewhere in this chapter: a Procrustes/PROTEST approach recovers known similarity structure more reliably than the Mantel test does.

</div>

<div id="worked-example-distance-decay-of-seaweed-composition-south-african-coastline" class="section level2">

## Worked Example: Distance-Decay of Seaweed Composition (South African Coastline)

We ask whether compositional dissimilarity between seaweed sections increases with along-coast geographic distance, a classic distance-decay pattern.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
library(vegan)
library(ggplot2)
 
#  Load seaweed data
spp   <- read.csv(here::here('data', 'BCB743', 'seaweed', 'SeaweedSpp.csv'))
spp   <- dplyr::select(spp, -1)     # drop row-index column
sites <- read.csv(here::here('data', 'BCB743', 'seaweed', 'SeaweedSites.csv'))
 
#  Bray-Curtis dissimilarity between seaweed sections 
sw.bc <- vegdist(spp, method = 'bray')
 
#  Along-coast geographic distance 
geo.sw <- dist(sites$Longitude)
 
# Simple Mantel test 
set.seed(2026)
mant.sw <- mantel(sw.bc, geo.sw, method = 'pearson', permutations = 999)
mant.sw
```

</div>

<div class="cell-output cell-output-stdout">


    Mantel statistic based on Pearson's product-moment correlation 

    Call:
    mantel(xdis = sw.bc, ydis = geo.sw, method = "pearson", permutations = 999) 

    Mantel statistic r: 0.832 
          Significance: 0.001 

    Upper quantiles of permutations (null model):
       90%    95%  97.5%    99% 
    0.0421 0.0581 0.0750 0.1024 
    Permutation: free
    Number of permutations: 999

</div>

</div>

The output reports the observed <span class="math inline">\\r_M\\</span>, the number of permutations, and the p-value (the proportion of permuted <span class="math inline">\\r_M\\</span> values equal to or greater than the observed one). A positive, significant <span class="math inline">\\r_M\\</span> means sites further apart along the coast tend to be more dissimilar in composition. Here, <span class="math inline">\\r_M = 0.832\\</span> (<span class="math inline">\\p = 0.001\\</span>): a strong, highly significant distance-decay signal, means nearby sections are compositionally similar, and similarity drops off steadily with distance.

</div>

<div id="mantel-correlogram" class="section level2">

## Mantel Correlogram

The Mantel correlogram \[Oden & Sokal 1986; Sokal 1986\] extends the test by computing <span class="math inline">\\r_M\\</span> between the response matrix and a series of binary indicator matrices, one per distance class. Plotting <span class="math inline">\\r_M\\</span> against distance class reveals the spatial scale at which similarity declines, and whether the decline is a smooth gradient or has distinct breaks. Borcard and Legendre (2012) showed by simulation that Mantel correlograms have good power to detect spatial autocorrelation in ecological data.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Mantel correlogram: at what along-coast scale does similarity decay? 
mant.corr <- mantel.correlog(
  D.eco  = sw.bc,
  D.geo  = geo.sw,
  nperm  = 999,
  cutoff = FALSE   # use all distance classes
)
plot(mant.corr,
     main = 'Mantel correlogram: seaweed composition vs along-coast distance')
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

Distance classes 1-4 are significantly positive (filled squares): nearby sites are more similar than expected by chance. Classes ~5-6 cross zero (the open square marks a non-significant class). From class 7 onward the correlation turns significantly negative, meaning far-apart sites are *less* similar than expected, which is consistent with species turnover at larger spatial scales. The small uptick at the far right (class ~15) is typical of low sample size in that bin rather than a genuine reversal, and can be checked by setting `cutoff = TRUE`.

</div>

<div id="distance-decay-of-similarity" class="section level2">

## Distance-decay of Similarity

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
#  Distance-decay plot 
df.sw <- data.frame(
  geo = as.vector(geo.sw),
  bc  = as.vector(sw.bc)
)
 
ggplot(df.sw, aes(x = geo, y = bc)) +
  geom_point(alpha = 0.25, colour = '#5B9BD5', size = 1.5) +
  geom_smooth(method = 'loess', colour = '#1A3A5C', se = TRUE) +
  labs(
    x = 'Along-coast distance (degrees longitude)',
    y = 'Bray-Curtis dissimilarity',
    title = 'Distance-decay of seaweed community similarity',
    subtitle = paste0('Mantel r = ', round(mant.sw$statistic, 3),
                      ', p = ', mant.sw$signif, ' (999 permutations)')
  ) +
  theme_classic(base_size = 13)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The loess trend is slightly non-linear which is a steeper decay at short distances, flattening in the middle, then steepening again toward the far end, which is typical of coastline biogeography, where underlying biogeographic breaks interrupt an otherwise smooth gradient.

</div>

<div id="procrustes-analysis" class="section level2">

## Procrustes Analysis

The Mantel test collapses each ordination into pairwise distances, discarding all information about individual site positions. Procrustes analysis keeps that information: given two sets of site scores from ordinations of the same <span class="math inline">\\n\\</span> sites, it finds the best geometric alignment between them using only translation, uniform scaling, and rotation/reflection. PROTEST then supplies a permutation test for whether that alignment is better than chance.

Procrustes analysis entered the ecological literature with Gower (1971) and was formalised for community data by Jackson (1995) through PROTEST. The name comes from the Greek myth of the figure who forced travellers to fit an iron bed by stretching or amputating their limbs, which is a fitting parallel for ecological datasets from different sources that rarely line up easily \[Lisboa et al. (2014)\].

<div id="the-three-permitted-transformations" class="section level3">

### The Three Permitted Transformations

Given two matrices of site scores, <span class="math inline">\\X\\</span> (the fixed “target”) and <span class="math inline">\\Y\\</span> (the “rotated” matrix), both with <span class="math inline">\\n\\</span> rows and <span class="math inline">\\p\\</span> ordination axes, the algorithm: (1) centres both configurations on their centroid; (2) rescales both to unit sum-of-squares, removing differences in overall spread; (3) finds, by singular value decomposition of <span class="math inline">\\X^\top Y\\</span>, the rotation/reflection that minimises the summed squared distances between matched points. Stretching individual axes independently is never allowed. This restriction is what makes the leftover residual a genuine measure of configuration disagreement rather than an artefact of deformation.

</div>

<div id="the-fit-statistic-m2-and-the-procrustes-correlation" class="section level3">

### The Fit Statistic <span class="math inline">\\m^2\\</span> and the Procrustes Correlation

The fit is summarised by the Procrustes sum of squares <span class="math inline">\\m^2\\</span> (Gower’s statistic): the residual squared distance between matched points after optimal superimposition, expressed as a proportion of the total variance in the scaled target. It ranges from 0 (perfect agreement) to 1 (complete disagreement). `vegan` reports <span class="math inline">\\\sqrt{1-m^2}\\</span>, the Procrustes correlation, which behaves like an ordinary correlation: values near 1 mean strong agreement, values near 0 mean none. Because <span class="math inline">\\m^2\\</span>’s null distribution depends on the geometry and dimensionality of each configuration, it carries no associated p-value on its own, hence PROTEST.

</div>

<div id="protest-testing-significance" class="section level3">

### PROTEST: Testing Significance

Jackson (1995) introduced PROTEST (PROcrustean Randomization TEst) to assess whether a Procrustes fit is better than chance, since a good fit can arise from dimensionality alone when the number of axes approaches the number of sites. PROTEST permutes the row labels of one configuration, recomputes the Procrustes fit each time (999 permutations by default in `vegan`), and builds a null distribution of correlations. The p-value is the proportion of permuted correlations equal to or exceeding the observed value, making it a one-tailed test \[Jackson (1995), p. 299\].

Jackson’s original application, on Ontario Lakes benthic invertebrate data, found significant concordance between community composition, water chemistry, and geographic location, and noted that PROTEST and the Mantel test can disagree because the Mantel result depends on the distance measure chosen. Peres-Neto and Jackson (2001) later used simulation to compare the two directly, finding PROTEST generally matched or exceeded Mantel’s power while producing lower Type II error rates across most scenarios tested.

</div>

</div>

<div id="worked-example-1-doubs-fish-composition-vs-environment" class="section level2">

## Worked Example 1: Doubs Fish Composition vs Environment

Using the Doubs river dataset \[Borcard et al. 2011\], we ask whether the site configuration in the fish-composition ordination agrees with the configuration in the environmental ordination, which is the first formal, site-by-site test of a pattern earlier chapters only argued qualitatively.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
#  Load Doubs data 
load(here::here('data', 'BCB743', 'NEwR-2ed_code_data', 'NeWR2-Data', 'Doubs.RData'))
 
# Remove site 8: no fish were recorded there 
spe <- spe[-8, ]
env2 <- env[-8, ]

#  Ordination 1: Hellinger-transformed PCA of fish composition (Ch. 8b) 
spe.hel    <- decostand(spe, method = 'hellinger')
pca.fish   <- rda(spe.hel)           # PCA via vegan::rda() on transformed data
 
#  Ordination 2: standardised PCA of environmental variables (Ch. 8a) 
env.z      <- decostand(env2, method = 'standardize')
pca.env    <- rda(env.z)
 
# Extract site scores on the first two axes of each ordination 
# Two axes give the same dimensionality for both configurations
scores.fish <- scores(pca.fish, display = 'sites', choices = 1:2)
scores.env  <- scores(pca.env,  display = 'sites', choices = 1:2)

# Procrustes analysis 
# symmetric = TRUE: result is independent of which matrix is treated as target
proc.fe <- procrustes(X = scores.env, Y = scores.fish, symmetric = TRUE)
summary(proc.fe)                     # rotation matrix, scaling, m2
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    procrustes(X = scores.env, Y = scores.fish, symmetric = TRUE) 

    Number of objects: 29    Number of dimensions: 2 

    Procrustes sum of squares:  
     0.4040709 
    Procrustes root mean squared error: 
     0.1180402 
    Quantiles of Procrustes errors:
            Min          1Q      Median          3Q         Max 
    0.007699917 0.064778287 0.087625586 0.135260775 0.308439228 

    Rotation matrix:
               [,1]       [,2]
    [1,] -0.9470534 -0.3210760
    [2,] -0.3210760  0.9470534

    Translation of averages:
                  [,1]          [,2]
    [1,] -2.066576e-17 -5.541161e-18

    Scaling of target:
    [1] 0.7719645

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# Procrustes correlation
sqrt(1 - proc.fe$ss)                
```

</div>

<div class="cell-output cell-output-stdout">

    [1] 0.7719645

</div>

</div>

The environment-fish Procrustes correlation (<span class="math inline">\\r = 0.772\\</span>, <span class="math inline">\\p = 0.001\\</span>) is significant but weaker than typical comparisons between two ordinations of the *same* community matrix, which is expected: relating species composition to an independently measured environmental dataset introduces additional variance. The rotation of roughly 19° off-diagonal indicates a moderate adjustment was needed to align the two configurations, consistent with a real but imperfect correspondence.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# PROTEST: is the fit better than chance? 
set.seed(2026)                       # for reproducibility
prot.fe <- protest(X = scores.env, Y = scores.fish, permutations = 999)
prot.fe                              # prints correlation, p-value, permutation summary
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    protest(X = scores.env, Y = scores.fish, permutations = 999) 

    Procrustes Sum of Squares (m12 squared):        0.4041 
    Correlation in a symmetric Procrustes rotation: 0.772 
    Significance:  0.001 

    Permutation: free
    Number of permutations: 999

</div>

</div>

PROTEST confirms the fit is stronger than expected by chance (<span class="math inline">\\p = 0.001\\</span>): the fish and environmental configurations are significantly more congruent than random permutation would produce.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Diagnostic plots 
# kind = 1: superimposition plot — arrows show per-site shift
plot(proc.fe, kind = 1,
     main = 'Procrustes: environment (PCA) vs fish (tb-PCA), Doubs River')
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# kind = 2: residuals by site quickly identifies which sites disagree most
plot(proc.fe, kind = 2,
     main = 'Procrustes residuals by site')
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The superimposition plot (`kind = 1`) shows the fish-ordination sites with arrows pointing to where each lands after optimal rotation/scaling onto the environmental ordination; most sites cluster tightly with short arrows, but a few, notably two on the lower right, fit poorly and pull down the overall correlation. The residual plot (`kind = 2`) makes this precise: site 24 stands out sharply, at roughly double the next-largest residual, as the worst-fitting site in the dataset.

</div>

<div id="worked-example-the-procrustean-association-metric-doubs-river-data" class="section level2">

## Worked Example: The Procrustean Association Metric (Doubs River Data)

A Procrustes fit and PROTEST answer whether two configurations agree overall, but not *where* they disagree. The **Procrustean Association Metric (PAM)**, which is the per site residual left over after superimposition, recovers exactly that. Lisboa et al. (2014) describe this as going “much beyond Mantel”: where the Mantel framework compresses multivariate structure into a single pairwise-distance matrix and discards site identity, the PAM retains a per-site number that can itself be tested against other variables. Having established a significant fish-environment fit above, we can now ask a sharper question: are the sites where fish composition and environment disagree most concentrated at a particular point along the river’s longitudinal gradient, say for example at the highest altitudes, where the physical template changes fastest?

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Reload the Doubs environmental 
load(here::here('data', 'BCB743', 'NEwR-2ed_code_data', 'NeWR2-Data', 'Doubs.RData'))
doubs.env <- env

doubs.env.no8 <- doubs.env[-8, ]

pam.doubs <- residuals(proc.fe)

cor.test(pam.doubs, doubs.env.no8$ele, method = 'spearman')
```

</div>

<div class="cell-output cell-output-stdout">


        Spearman's rank correlation rho

    data:  pam.doubs and doubs.env.no8$ele
    S = 3296, p-value = 0.3268
    alternative hypothesis: true rho is not equal to 0
    sample estimates:
          rho 
    0.1881773 

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# Visualise
ggplot(data.frame(Altitude = doubs.env.no8$ele, PAM = pam.doubs),
       aes(x = Altitude, y = PAM)) +
  geom_point(colour = '#5B2A41', size = 2.5) +
  geom_smooth(method = 'loess', colour = '#A2536B', se = TRUE) +
  labs(x = 'Altitude (m) — upstream to downstream',
       y = 'Procrustes residual (PAM)',
       title = 'Do high-altitude sites show greater fish-environment disagreement?') +
  theme_classic(base_size = 13)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

`residuals(proc.fe)` extracts one PAM value per site, exactly the per-site diagnostic a Mantel test cannot provide. The Spearman correlation between PAM and altitude tests whether disagreement between the fish and environmental ordinations is structured along the river’s longitudinal gradient rather than scattered at random. \[ρ = **0.1881773**, <span class="math inline">\\p =\\</span> **0.3268**\]: A non-significant result suggests that the residual disagreement is not tied to this particular gradient, and might reflect variables not included in the environmental matrix, or simple sampling noise. The loess trend in the plot above should be read together with the test statistic, not in place of it, a visual trend that isn’t backed by a significant correlation is not evidence of a real pattern.

</div>

<div id="worked-example-2-thomsen-et-al.-2016-j.-anim.-ecol.-moth-beetle-light-trap-data" class="section level2">

## Worked Example 2: Thomsen et al. (2016, *J. Anim. Ecol.*) Moth & Beetle Light-Trap Data

The same light trap, at the same site, was sampled every year from 1992-2009 (18 repeated annual samples, the “sampling units”). Two taxonomically independent communities, moths (Lepidoptera) and beetles (Coleoptera), were recorded simultaneously in each sample. If both responded to the same underlying driver (temperature, per the paper’s thesis), their year-to-year trajectories through community space should be congruent, even though the ordinations are built from entirely different species pools. Procrustes and PROTEST let us test that congruence formally, rather than just comparing two NMDS/PCoA plots by eye.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
library(tidyverse)
library(vegan)

#  Load & clean the raw data 
# Note: file has a non-UTF8 (Latin-1) character in the author name, so we
# specify the encoding explicitly.

raw <- read.csv(
  here::here("data", "BCB743", "Thomsen_1992_2009.csv"),
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

glimpse(raw)
```

</div>

<div class="cell-output cell-output-stdout">

    Rows: 44,088
    Columns: 7
    $ order       <chr> "LEPIDOPTERA", "LEPIDOPTERA", "LEPIDOPTERA", "LEPIDOPTERA"…
    $ family      <chr> "ACROLEPIIDAE", "ACROLEPIIDAE", "ACROLEPIIDAE", "ACROLEPII…
    $ name        <chr> "Acrolepiopsis assectella Zell.", "Acrolepiopsis assectell…
    $ year        <int> 1994, 1994, 1996, 1998, 1999, 1999, 2000, 2003, 2004, 2006…
    $ date1       <chr> "8/12/94", "8/22/94", "7/24/96", "7/13/98", "8/9/99", "8/3…
    $ date2       <chr> "8/21/94", "8/25/94", "7/25/96", "7/19/98", "8/10/99", "8/…
    $ individuals <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 3, 7, 4, 1, 5, 1, 2…

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# columns: order, family, name, year, date1, date2, individuals

# Following Thomsen et al.'s own methods, drop 1992 and 2009: these were
# partial/start-up seasons and were excluded from all their analyses.
raw <- raw %>% filter(year %in% 1993:2008)

#  Build year x species abundance matrices, one per taxon
build_matrix <- function(data, taxon) {
  data %>%
    filter(order == taxon) %>%
    group_by(year, name) %>%
    summarise(individuals = sum(individuals), .groups = "drop") %>%
    pivot_wider(names_from = name, values_from = individuals, values_fill = 0) %>%
    arrange(year) %>%
    column_to_rownames("year") %>%
    as.matrix()
}

moth_mat   <- build_matrix(raw, "LEPIDOPTERA")
beetle_mat <- build_matrix(raw, "COLEOPTERA")

dim(moth_mat)    
```

</div>

<div class="cell-output cell-output-stdout">

    [1]   16 1053

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
dim(beetle_mat)  
```

</div>

<div class="cell-output cell-output-stdout">

    [1]  16 461

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# Both matrices have the same 16 rows (1993-2008), in the same order
# this row-correspondence is the ONLY requirement for Procrustes: the two

stopifnot(identical(rownames(moth_mat), rownames(beetle_mat)))

# Ordinate each taxon's community separately 
# Bray-Curtis dissimilarity + PCoA is the standard vegan workflow for abundance community data covered in this module.

moth_dist   <- vegdist(moth_mat,   method = "bray")
beetle_dist <- vegdist(beetle_mat, method = "bray")

moth_pcoa   <- cmdscale(moth_dist,   k = 2, eig = TRUE)
beetle_pcoa <- cmdscale(beetle_dist, k = 2, eig = TRUE)
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Quick look: each taxon's own year-to-year trajectory
par(mfrow = c(1, 2))
plot(moth_pcoa$points, type = "n", main = "Moths - PCoA", xlab = "PCoA1", ylab = "PCoA2")
text(moth_pcoa$points, labels = rownames(moth_mat), cex = 0.7)
lines(moth_pcoa$points[order(rownames(moth_mat)), ], col = "grey60")

plot(beetle_pcoa$points, type = "n", main = "Beetles - PCoA", xlab = "PCoA1", ylab = "PCoA2")
text(beetle_pcoa$points, labels = rownames(beetle_mat), cex = 0.7)
lines(beetle_pcoa$points[order(rownames(beetle_mat)), ], col = "grey60")
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
par(mfrow = c(1, 1))
```

</div>

</div>

Two separate ordinations, one per taxon, each showing a year’s community position with lines connecting consecutive years before any Procrustes rotation, just each taxon’s own trajectory. The 1993 beetle sample sits as a clear outlier, far from the rest of the series.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Procrustes rotation + PROTEST 
# procrustes() finds the optimal rotation/scaling/translation of the TARGET config (moths) onto the REFERENCE config (beetles).

proc_fit <- procrustes(X = beetle_pcoa, Y = moth_pcoa, symmetric = TRUE)
summary(proc_fit)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    procrustes(X = beetle_pcoa, Y = moth_pcoa, symmetric = TRUE) 

    Number of objects: 16    Number of dimensions: 2 

    Procrustes sum of squares:  
     0.784416 
    Procrustes root mean squared error: 
     0.2214182 
    Quantiles of Procrustes errors:
           Min         1Q     Median         3Q        Max 
    0.01903503 0.07806166 0.14095780 0.23433393 0.51540980 

    Rotation matrix:
                [,1]        [,2]
    [1,] -0.99808226 -0.06190151
    [2,] -0.06190151  0.99808226

    Translation of averages:
                 [,1]         [,2]
    [1,] 1.483982e-18 1.120925e-17

    Scaling of target:
    [1] 0.4643103

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
set.seed(2026)
proc_test <- protest(X = beetle_pcoa, Y = moth_pcoa, permutations = 999)
proc_test
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    protest(X = beetle_pcoa, Y = moth_pcoa, permutations = 999) 

    Procrustes Sum of Squares (m12 squared):        0.7844 
    Correlation in a symmetric Procrustes rotation: 0.4643 
    Significance:  0.058 

    Permutation: free
    Number of permutations: 999

</div>

</div>

The Procrustes correlation (<span class="math inline">\\r = 0.464\\</span>) indicates the moth and beetle trajectories share some common structure across years, but a substantial share of year-to-year variation is taxon-specific rather than shared. PROTEST returns <span class="math inline">\\p = 0.058\\</span>: non-significant at the conventional threshold.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Visualise the Procrustes fit 

plot(proc_fit, kind = 1, main = "Procrustes fit: moths onto beetles")
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
plot(proc_fit, kind = 2, main = "Procrustes residuals per year")
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# A clearer ggplot version, coloured by year to show the temporal trajectory and highlight which years disagree most between taxa.
proc_df <- tibble(
  year        = as.integer(rownames(moth_mat)),
  beetle_x    = proc_fit$X[, 1],
  beetle_y    = proc_fit$X[, 2],
  moth_fit_x  = proc_fit$Yrot[, 1],
  moth_fit_y  = proc_fit$Yrot[, 2],
  residual    = residuals(proc_fit)
)

ggplot(proc_df) +
  geom_segment(aes(x = moth_fit_x, y = moth_fit_y,
                    xend = beetle_x, yend = beetle_y),
               arrow = arrow(length = unit(0.15, "cm")), colour = "grey50") +
  geom_point(aes(x = beetle_x, y = beetle_y, colour = year), size = 3) +
  geom_path(aes(x = beetle_x, y = beetle_y), colour = "steelblue", alpha = 0.4) +
  geom_text(aes(x = beetle_x, y = beetle_y, label = year),
            vjust = -1, size = 3) +
  scale_colour_viridis_c() +
  labs(
    title = "Procrustes superimposition: beetle vs moth community trajectories (1993-2008)",
    subtitle = paste0("PROTEST correlation r = ", round(proc_test$t0, 3),
                       ", P = ", format.pval(proc_test$signif, digits = 3)),
    x = "Dimension 1", y = "Dimension 2", colour = "Year"
  ) +
  theme_minimal(base_size = 12)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The `kind = 1` plot shows the moth points with arrows to where they land after being rotated/scaled/translated onto the beetle configuration; longer arrows mean worse agreement for that year, and the longest arrow corresponds to the poorly-fitting 1993 point. The `kind = 2` residual plot gives the same information as a per-year bar, in year order (index 1 = 1993 … index 16 = 2008): 1993 has by far the largest residual, with 1999 and 2002 also standing out. The coloured superimposition plot combines both views, beetle points in their own ordination space, arrows showing where the rotated moth points would need to move to match them, and a trajectory line tracing the beetle year-to-year path.

</div>

<div id="summary" class="section level2">

## Summary

The Mantel test and Procrustes analysis both ask whether two descriptions of the same sites tell the same story, but they answer it in different currencies: the Mantel test compares pairwise distances and cannot localise disagreement to individual sites, while Procrustes/PROTEST compares configurations directly and, via the PAM, hands back a per-site residual that can be related to other variables in its own right. Choosing between them, and knowing what each does and doesn’t tell you, is the practical skill this chapter has aimed at.

</div>

<div id="authors-contribution" class="section level2">

## Author’s contribution

All of the authors did research on the methods used in this chapter and formulated the theory content around it. Maya specifically wrote the Mantel test Theory, Isa and Malusi wrote the Procrustus and PROTEST theory content. All members participated in the search for datasets to use as examples, Maya executed the examples using code.

</div>

<div id="ai-declaration" class="section level2">

## AI Declaration

AI was used to generate code AI was also used to format the chapter page according to tangled bank layout AI was used to detect language errors

</div>

<div id="references" class="section level2">

## References

Borcard, D., Gillet, F., & Legendre, P. (2011). Numerical ecology with R. Springer.

Borcard, D., & Legendre, P. (2012). Is the Mantel correlogram powerful enough to be useful in ecological analysis? A simulation study. Ecology, 93, 1473–1481. https://doi.org/10.1890/11-1737.1

Forcino, F. L., Ritterbush, K. A., & Stafford, E. S. (2015). Evaluating the effectiveness of the Mantel test and Procrustes randomization test for exploratory ecological similarity among paleocommunities. Palaeogeography, Palaeoclimatology, Palaeoecology, 426, 199–208. https://doi.org/10.1016/j.palaeo.2015.03.023

Gower, J. C. (1971). Statistical methods of comparing different multivariate analyses of the same data. In F. R. Hodson, D. G. Kendall, & P. Tautu (Eds.), Mathematics in the archaeological and historical sciences (pp. 138–149). Edinburgh University Press.

Hirst, C. N., & Jackson, D. A. (2007). Reconstructing community relationships: The impact of sampling error, ordination approach, and gradient length. Diversity and Distributions, 13, 361–371. https://doi.org/10.1111/j.1472-4642.2007.00307.x

Jackson, D. A. (1995). PROTEST: A PROcrustean randomization test of community environment concordance. Écoscience, 2(3), 297–303.

Legendre, P., & Fortin, M.-J. (2010). Comparison of the Mantel test and alternative approaches for detecting complex multivariate relationships in the spatial analysis of genetic data. Molecular Ecology Resources, 10, 831–844. https://doi.org/10.1111/j.1755-0998.2010.02866.x

Legendre, P., Fortin, M.-J., & Borcard, D. (2015). Should the Mantel test be used in spatial analysis? Methods in Ecology and Evolution, 6(11), 1239–1247. https://doi.org/10.1111/2041-210X.12425

Legendre, P., & Legendre, L. (2012). Numerical ecology (3rd English ed.). Elsevier.

Lisboa, F. J. G., Peres-Neto, P. R., Chaer, G. M., Jesus, E. da C., Mitchell, R. J., Chapman, S. J., & Berbara, R. L. L. (2014). Much beyond Mantel: Bringing Procrustes association metric to the plant and soil ecologist’s toolbox. PLoS ONE, 9(6), e101238. https://doi.org/10.1371/journal.pone.0101238

Mantel, N. (1967). The detection of disease clustering and a generalized regression approach. Cancer Research, 27, 209–220.

Oden, N. L., & Sokal, R. R. (1986). Directional autocorrelation: An extension of spatial correlograms to two dimensions. Systematic Zoology, 35, 608–617. https://doi.org/10.2307/2413120

Peres-Neto, P. R., & Jackson, D. A. (2001). How well do multivariate data sets match? The advantages of a Procrustean superimposition approach over the Mantel test. Oecologia, 129, 169–178. https://doi.org/10.1007/s004420100720

Sokal, R. R. (1986). Spatial data analysis and historical processes. In E. Diday et al. (Eds.), Data analysis and informatics, IV (pp. 29–43). North-Holland.

</div>

<div id="quarto-appendix" class="default">

<div id="quarto-reuse" class="section quarto-appendix-contents">

## Reuse

<div class="quarto-appendix-contents">

<div>

<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/" rel="license">CC BY-NC-SA 4.0</a>

</div>

</div>

</div>

<div id="quarto-citation" class="section quarto-appendix-contents">

## Citation

<div>

<div class="quarto-appendix-secondary-label">

BibTeX citation:

</div>

``` code-with-copy
@online{smit2026,
  author = {Smit, A. J. and Nduvheni, Isa Valashiya, Malusi Zitha, Maya},
  title = {Proposed 17: {Statistical} {Methods} of {Comparing}},
  date = {2026-07-24},
  url = {https://tangledbank.netlify.app/BCB743/comparison_methods.html},
  langid = {en}
}
```

<div class="quarto-appendix-secondary-label">

For attribution, please cite this work as:

</div>

<div id="ref-smit2026" class="csl-entry quarto-appendix-citeas" role="listitem">

Smit AJ, Nduvheni, Isa Valashiya, Malusi Zitha M (2026) Proposed 17: Statistical Methods of Comparing. [https://tangledbank.netlify.app/BCB743/comparison_methods.html.](https://tangledbank.netlify.app/BCB743/comparison_methods.html)

</div>

</div>

</div>

</div>

</div>
