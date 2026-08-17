<div id="quarto-document-content" class="content" role="main">

<div id="title-block-header" class="quarto-title-block default">

<div class="quarto-title">

# 18b: From Ecological Gradients to Ecological Inference

</div>

<div class="quarto-title-meta-author">

<div class="quarto-title-meta-heading">

Authors

</div>

<div class="quarto-title-meta-heading">

Affiliation

</div>

<div class="quarto-title-meta-contents">

Anisah Karriem

</div>

<div class="quarto-title-meta-contents">

University of the Western Cape

</div>

<div class="quarto-title-meta-contents">

Naledi Jijana

</div>

<div class="quarto-title-meta-contents">

University of the Western Cape

</div>

<div class="quarto-title-meta-contents">

Emihle Ntsiko

</div>

<div class="quarto-title-meta-contents">

University of the Western Cape

</div>

</div>

<div class="quarto-title-meta">

</div>

</div>

<div id="overview" class="section level1" number="1">

# <span class="header-section-number">1</span> Overview

<div id="why-ecological-inference" class="section level2" number="1.1">

## <span class="header-section-number">1.1</span> Why ecological inference?

Throughout this module we have explored a range of quantitative tools for analysing ecological communities. We have learned how to visualise multivariate data, compare communities using measures of dissimilarity, partition beta diversity into turnover and nestedness components, and apply unconstrained and constrained ordination techniques to ecological datasets. Collectively, these methods provide a powerful toolkit for investigating patterns in biodiversity.

However, ecological analyses rarely end once the code has finished running. Producing an NMDS plot, fitting a constrained ordination or calculating a beta diversity partition does not, by itself, answer an ecological question. Instead, these analyses provide pieces of evidence that must be interpreted together to understand the processes shaping ecological communities. In other words, the objective is not simply to generate statistical outputs but to make ecological inferences from them.


The purpose of this chapter is therefore not to introduce another statistical method, but to develop a framework for ecological interpretation. We bring together the methods introduced throughout BCB743 and demonstrate how they can be combined to move from observed biodiversity patterns to evidence-based ecological explanations. Particular emphasis is placed on ecological gradients, constrained ordination and variation partitioning because these approaches allow us to distinguish between different drivers of community structure rather than simply describing patterns.

<div class="cell" warnings="false">

<div class="cell-output-display">

<div id="htmlwidget-aa4e24d880dea06f3dc3" class="grViz html-widget html-fill-item" style="width:75%;height:1087.5px;">

</div>

</div>

</div>

</div>

</div>

<div id="learning-outcomes" class="section level1" number="2">

# <span class="header-section-number">2</span> Learning outcomes

After completing this chapter, you should be able to:

- Explain the role of ecological gradients in shaping biodiversity patterns.
- Distinguish between describing community patterns and making ecological inferences.
- Interpret unconstrained and constrained ordination outputs within an ecological context.
- Explain how variation partitioning separates environmental and spatial components of community variation
- Integrate multiple quantitative analyses to evaluate competing ecological hypotheses.
- Recognise the assumptions and limitations associated with ecological inference.
- Use quantitative evidence to support conservation and ecological management decisions.

</div>

<div id="ecological-gradients-as-drivers-of-biodiversity" class="section level1" number="3">

# <span class="header-section-number">3</span> Ecological gradients as drivers of biodiversity

<div id="revisiting-ecological-gradients" class="section level2" number="3.1">

## <span class="header-section-number">3.1</span> Revisiting ecological gradients

Earlier chapters introduced ecological gradients as one of the fundamental concepts in community ecology. We saw that environmental conditions rarely change abruptly across landscapes. Instead, factors such as temperature, rainfall, salinity, elevation and nutrient availability usually vary continuously through space and time <span class="citation" cites="muller1998 mcdonnell1993 riesch2018">(<a href="#ref-muller1998" role="doc-biblioref">Müller 1998</a>; <a href="#ref-mcdonnell1993" role="doc-biblioref">McDonnell et al. 1993</a>; <a href="#ref-riesch2018" role="doc-biblioref">Riesch et al. 2018</a>)</span>. As these environmental conditions change, so too does the composition of ecological communities <span class="citation" cites="scheiner2005 pavoine2011">(<a href="#ref-scheiner2005" role="doc-biblioref">Scheiner and Willig 2005</a>; <a href="#ref-pavoine2011" role="doc-biblioref">Pavoine and Bonsall 2011</a>)</span>.


Rather than revisiting the ecological theory behind gradients, this chapter builds on that foundation by asking a different question:

*Once we observe a biodiversity pattern along a gradient, how do we determine what is actually driving it?*

</div>

<div id="from-observing-gradients-to-explaining-biodiversity-patterns" class="section level2" number="3.2">

## <span class="header-section-number">3.2</span> From observing gradients to explaining biodiversity patterns

Throughout the module you have learned how ordination methods reduce complex multivariate datasets into simpler visual representations <span class="citation" cites="jongman1995 borcard2011">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. These ordinations often reveal clear ecological gradients, showing that community composition changes predictably across environmental space <span class="citation" cites="terbraak1994 dray2012">(<a href="#ref-terbraak1994" role="doc-biblioref">Ter Braak 1994</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>. However, recognising a pattern is only the first step.

These gradual environmental changes are what ecologists mean by an ecological gradient, and they are one of the main reasons biodiversity differs from one location to another <span class="citation" cites="scheiner2005 riesch2018">(<a href="#ref-scheiner2005" role="doc-biblioref">Scheiner and Willig 2005</a>; <a href="#ref-riesch2018" role="doc-biblioref">Riesch et al. 2018</a>)</span>. As conditions shift along a gradient, individual species become more or less common depending on the conditions they can tolerate <span class="citation" cites="pavoine2011 he2024">(<a href="#ref-pavoine2011" role="doc-biblioref">Pavoine and Bonsall 2011</a>; <a href="#ref-he2024" role="doc-biblioref">He et al. 2024</a>)</span>, and the cumulative effect of many species responding this way is a change in community composition.


Consider a survey of rocky shore communities along the South African coastline. An NMDS ordination may reveal a gradual transition in community composition from the west coast to the east coast. At first glance, this appears to reflect a temperature gradient.

But several questions immediately arise.

- Is temperature actually responsible for the observed pattern?
- Could dispersal limitation produce the same result?
- Are neighbouring communities similar simply because they are geographically close?
- Could multiple environmental variables be acting simultaneously?

Ordination alone cannot answer these questions. Instead, it provides evidence that ecological structure exists and generates hypotheses about the processes responsible <span class="citation" cites="borcard2011 dray2012">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>. The challenge is moving from recognising biodiversity patterns to explaining them.

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Do It Now

</div>

</div>

<div class="callout-body-container callout-body">

Think back to the ordination methods covered earlier in BCB743.

Imagine an NMDS of coastal fish communities shows a clear separation between west coast and east coast sites.

Without looking ahead, discuss the following questions:

1.  What ecological gradient might explain this pattern?
2.  What alternative explanations could produce the same ordination?
3.  Which of the methods you have already learned could test these competing hypotheses?
4.  Which questions remain unanswered after the ordination?

Keep your answers in mind as you work through this chapter. By the end, you should be able to revisit these questions and explain not only what the pattern shows, but why it is likely to have occurred.

</div>

</div>

</div>

</div>

<div id="from-gradients-to-hypotheses" class="section level1" number="4">

# <span class="header-section-number">4</span> From gradients to hypotheses

Once ecological gradients have been identified, the next step is to transform these observed patterns into scientific hypotheses. One of the most important principles in ecology is that the same biodiversity pattern can often be explained by several different ecological processes <span class="citation" cites="scheiner2005 pavoine2011">(<a href="#ref-scheiner2005" role="doc-biblioref">Scheiner and Willig 2005</a>; <a href="#ref-pavoine2011" role="doc-biblioref">Pavoine and Bonsall 2011</a>)</span>. For this reason, ecologists avoid accepting the first explanation that appears reasonable and instead consider multiple competing hypotheses before drawing conclusions <span class="citation" cites="dietze2017">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>.

Every observed biodiversity pattern can generate several plausible ecological hypotheses.

For example, suppose an NMDS analysis of kelp-associated communities reveals a clear separation between west coast and south coast sampling sites.

Several explanations are possible:

- Sea surface temperature differs between regions.
- Wave exposure influences habitat suitability.
- Ocean currents restrict larval dispersal.
- Historical colonisation patterns have shaped present-day communities.
- Unmeasured environmental variables influence community composition.

Each explanation represents a hypothesis that can be evaluated using quantitative methods <span class="citation" cites="dietze2017">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>. Importantly, these hypotheses are not mutually exclusive. Environmental gradients, spatial processes and stochastic events frequently act simultaneously, meaning that robust ecological inference often requires integrating multiple analytical approaches <span class="citation" cites="dray2012 cressie2009">(<a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>; <a href="#ref-cressie2009" role="doc-biblioref">Cressie et al. 2009</a>)</span>.

Returning to the Doubs River example, differences in fish communities along the river may certainly be influenced by changing environmental conditions such as nutrient availability, dissolved oxygen or habitat characteristics. However, these are not the only possible explanations. Fish distributions could also be influenced by dispersal barriers, historical colonisation events or other ecological processes that are not directly measured in the environmental data <span class="citation" cites="borcard2011 dray2012">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>. The observed biodiversity pattern therefore does not automatically identify the mechanism responsible for creating it.

The same reasoning applies to the South African seaweed study. Although the ordination suggested that temperature was associated with changes in community composition, several additional explanations remain possible. Ocean currents may influence dispersal, wave exposure may affect species survival, or historical climatic events may have shaped present-day distributions. Because several environmental variables often change together, it is rarely possible to determine the true driver simply by observing an ordination plot <span class="citation" cites="dray2012 capblancq2021">(<a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>; <a href="#ref-capblancq2021" role="doc-biblioref">Capblancq and Forester 2021</a>)</span>. Instead, ecologists must recognise that multiple hypotheses can be equally plausible at the beginning of an investigation.

An important feature of ecological systems is that these explanations are not mutually exclusive. Rather than one process operating alone, biodiversity patterns often emerge from the interaction of several ecological mechanisms acting simultaneously <span class="citation" cites="pavoine2011 dray2012">(<a href="#ref-pavoine2011" role="doc-biblioref">Pavoine and Bonsall 2011</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>. This complexity explains why ecological inference requires careful hypothesis testing instead of relying solely on visual patterns observed in multivariate analyses <span class="citation" cites="dietze2017">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>.

At this stage, the chapter shifts from observing ecological patterns to asking scientific questions. Before selecting any statistical analysis, ecologists first decide exactly what they want to understand. They may ask whether communities differ significantly across sites, which environmental variables explain the greatest amount of variation, whether geography contributes independently to biodiversity patterns, or how much variation is explained by different ecological processes <span class="citation" cites="borcard2011 legendre2012">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-legendre2012" role="doc-biblioref">Legendre et al. 2012</a>)</span>. These questions determine the most appropriate analytical methods for testing competing hypotheses.

Ultimately, this section demonstrates that ecological research follows a logical progression. We begin by identifying ecological gradients, then use these patterns to develop alternative hypotheses, and finally apply appropriate statistical analyses to determine which explanations are best supported by the available evidence <span class="citation" cites="dietze2017 borcard2011">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. This transition from recognising patterns to testing explanations is the foundation of ecological inference and underpins much of modern community ecology <span class="citation" cites="dray2012">(<a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>.

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Questions that guide ecological inference

</div>

</div>

<div class="callout-body-container callout-body">

Before selecting any statistical method, an ecologist should first identify the ecological question being asked. Examples include:

1.  Do communities differ along an environmental gradient?
2.  Which environmental variables best explain observed biodiversity patterns?
3.  Does geographic distance influence community composition?
4.  How much variation is uniquely explained by environmental conditions?
5.  Are multiple ecological processes operating simultaneously?
6.  What evidence best supports competing ecological hypotheses?

These questions determine which analytical methods are appropriate and how their outputs should be interpreted <span class="citation" cites="borcard2011 dray2012">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>.

</div>

</div>

</div>

<div id="reading-ordinations-ecologically" class="section level1" number="5">

# <span class="header-section-number">5</span> Reading ordinations ecologically

<div id="from-visualisation-to-interpretation" class="section level2" number="5.1">

## <span class="header-section-number">5.1</span> From visualisation to interpretation

By now, you have worked through several ordination techniques including PCA, PCoA, NMDS, RDA and db-RDA. You have learned how to prepare community data, choose appropriate distance measures, perform ordinations in vegan, and generate publication-quality figures <span class="citation" cites="oksanen2001 borcard2011 qian">(<a href="#ref-oksanen2001" role="doc-biblioref">Oksanen et al. 2001</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-qian" role="doc-biblioref">Qian, n.d.</a>)</span>. While these are essential technical skills, they represent only one part of the analytical process.

An ordination plot should never be viewed as the final result of an analysis. Instead, it should be treated as a visual summary of complex multivariate relationships that requires ecological interpretation <span class="citation" cites="jongman1995 borcard2011">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. The position of sites, species and environmental variables within an ordination reflects underlying ecological gradients, but these gradients are not always obvious and they rarely provide direct evidence of ecological processes on their own <span class="citation" cites="terbraak1994 dray2012">(<a href="#ref-terbraak1994" role="doc-biblioref">Ter Braak 1994</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>.

The primary purpose of an ordination is therefore not simply to reduce dimensionality, but to help us recognise ecological structure within complex datasets <span class="citation" cites="jongman1995 borcard2011">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. Every pattern observed in an ordination should prompt further ecological questions rather than immediate conclusions <span class="citation" cites="dietze2017">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>.

</div>

<div id="what-information-does-an-ordination-actually-contain" class="section level2" number="5.2">

## <span class="header-section-number">5.2</span> What information does an ordination actually contain?

Regardless of the ordination method used, most ordination plots contain four broad types of ecological information <span class="citation" cites="jongman1995 borcard2011">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>:

- Patterns of similarity among sampling sites
- Relationships between species
- Relationships between environmental variables and communities
- Underlying ecological gradients

These components should always be interpreted together rather than independently.

<div id="site-relationships" class="section level3" number="5.2.1">

### <span class="header-section-number">5.2.1</span> Site relationships

The first feature most ecologists notice is the arrangement of sampling sites.

Sites that occur close together represent communities with relatively similar species composition, whereas sites positioned far apart are compositionally distinct <span class="citation" cites="jongman1995">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>)</span>. Importantly, ordination methods differ in how these distances are calculated, but the ecological interpretation remains broadly consistent <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

For example, an NMDS ordination based on Bray–Curtis dissimilarities preserves the rank order of ecological distances rather than absolute Euclidean distances <span class="citation" cites="oksanen2001">(<a href="#ref-oksanen2001" role="doc-biblioref">Oksanen et al. 2001</a>)</span>. Consequently, nearby sites represent relatively similar communities even though the axes themselves have no inherent ecological meaning <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

The immediate ecological question therefore becomes:

*Why are these communities similar?*

Possible explanations include shared environmental conditions, geographic proximity, dispersal processes or historical influences <span class="citation" cites="dray2012 dietze2017">(<a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>; <a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>. At this stage, the ordination identifies the pattern but not the mechanism.

</div>

<div id="species-relationships" class="section level3" number="5.2.2">

### <span class="header-section-number">5.2.2</span> Species relationships

Species can also be plotted within ordination space.

Species occurring close together often exhibit similar distributional patterns across sampling sites <span class="citation" cites="jongman1995 borcard2011">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. This may indicate shared habitat preferences, similar environmental tolerances or frequent co-occurrence <span class="citation" cites="pavoine2011">(<a href="#ref-pavoine2011" role="doc-biblioref">Pavoine and Bonsall 2011</a>)</span>.

However, ordinations cannot determine why species co-occur. Similar distributions may arise because species respond similarly to environmental gradients rather than interacting directly with one another <span class="citation" cites="borcard2011 dray2012">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>.

For this reason, ordination should not be used to infer ecological interactions without supporting evidence <span class="citation" cites="dietze2017">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>.

</div>

<div id="environmental-vectors" class="section level3" number="5.2.3">

### <span class="header-section-number">5.2.3</span> Environmental vectors

Constrained ordinations and fitted environmental vectors introduce additional ecological information <span class="citation" cites="terbraak1994 capblancq2021">(<a href="#ref-terbraak1994" role="doc-biblioref">Ter Braak 1994</a>; <a href="#ref-capblancq2021" role="doc-biblioref">Capblancq and Forester 2021</a>)</span>.

Environmental arrows represent the direction of increasing values for each variable. Their orientation indicates the direction of the environmental gradient, while arrow length reflects the strength of the relationship between that variable and the ordination configuration <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

Long vectors generally indicate variables strongly associated with variation in community composition <span class="citation" cites="capblancq2021">(<a href="#ref-capblancq2021" role="doc-biblioref">Capblancq and Forester 2021</a>)</span>.

Short vectors indicate weaker relationships.

However, students often make the mistake of assuming that longer arrows imply ecological importance. In reality, arrow length only reflects the strength of association within the fitted model and should always be interpreted alongside statistical tests and ecological understanding

</div>

<div id="ecological-gradients" class="section level3" number="5.2.4">

### <span class="header-section-number">5.2.4</span> Ecological gradients

Perhaps the most important feature of any ordination is the presence of ecological gradients.

Communities rarely separate into completely isolated clusters. Instead, they often form continuous transitions reflecting gradual environmental change <span class="citation" cites="muller1998 riesch2018">(<a href="#ref-muller1998" role="doc-biblioref">Müller 1998</a>; <a href="#ref-riesch2018" role="doc-biblioref">Riesch et al. 2018</a>)</span>.

For example, a sequence of sites extending along the first NMDS axis may represent increasing sea surface temperature, changing salinity or increasing elevation.

The ordination itself does not identify which gradient is responsible; it only provides evidence that one exists. Identifying the gradient requires additional analyses.

</div>

</div>

<div id="interpreting-ordinations-asking-the-right-questions" class="section level2" number="5.3">

## <span class="header-section-number">5.3</span> Interpreting ordinations: asking the right questions

Rather than asking “What does this figure show?”, ecologists should ask:

- Which sites appear compositionally similar?
- Which communities differ most strongly?
- Is there evidence of a continuous ecological gradient?
- Do clusters correspond with known habitat types?
- Could spatial location explain the observed pattern?
- Which environmental variables appear associated with community differences?
- Which alternative explanations remain possible?

These questions shift interpretation away from describing figures and towards generating ecological hypotheses <span class="citation" cites="dietze2017 dray2012">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>.

<div id="interpreting-pca" class="section level3" number="5.3.1">

### <span class="header-section-number">5.3.1</span> Interpreting PCA

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(ggplot2)
library(grid)


set.seed(15)

# Simulate correlated data
x <- rnorm(60)
y <- x*0.8 + rnorm(60, 0.5)
df <- data.frame(x, y)

# Run PCA
pca <- prcomp(df, scale. = TRUE)

# Extract loadings (directions of PCs)
loadings <- pca$rotation
pc1 <- loadings[,1] * 3   # scale arrows for visibility
pc2 <- loadings[,2] * 3

# Plot with annotations
ggplot(df, aes(x, y)) +
  geom_point(size = 3, alpha = 0.7) +
  annotate("segment", x = 0, y = 0, xend = pc1[1], yend = pc1[2],
           arrow = arrow(length = unit(0.3, "cm")),
           colour = "red", linewidth = 1.2) +
  annotate("segment", x = 0, y = 0, xend = pc2[1], yend = pc2[2],
           arrow = arrow(length = unit(0.3, "cm")),
           colour = "blue", linewidth = 1.2) +
  annotate("text", x = pc1[1]*0.8, y = pc1[2]*0.8,
           label = "PC1: Max variance", colour = "red", size = 5, hjust = 0) +
  annotate("text", x = pc2[1]*0.8, y = pc2[2]*0.8,
           label = "PC2: Remaining variance", colour = "blue", size = 5, hjust = 0) +
  theme_classic(base_size = 15) +
  labs(title = "PCA: Principal Components as New Axes",
       subtitle = "PC1 captures most variance; PC2 is orthogonal") +
  coord_cartesian(xlim = c(min(df$x)-1, max(df$x)+1),
                  ylim = c(min(df$y)-1, max(df$y)+1))
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The red arrow (PC1) shows the direction of maximum variance in the data — the axis along which the points are most spread out.

The blue arrow (PC2) is orthogonal to PC1 and represents the second principal component, capturing the remaining variance.

The examples in this chapter use the varespec and varechem datasets included with the vegan package. The species matrix (varespec) contains vegetation abundances recorded from multiple sites, while varechem contains environmental measurements collected from the same locations.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Load the data 

library(vegan)

data(varespec)
data(varechem)

comm <- varespec
env <- varechem
```

</div>

</div>

Examine the structure of each dataset.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# examine the environmental data 

dim(env)
```

</div>

<div class="cell-output cell-output-stdout">

    R> [1] 24 14

</div>

<div class="code-copy-outer-scaffold">

``` r
head(env)
```

</div>

<div class="cell-output cell-output-stdout">

    R>       N    P     K    Ca    Mg    S    Al   Fe    Mn   Zn  Mo Baresoil Humdepth
    R> 18 19.8 42.1 139.9 519.4  90.0 32.3  39.0 40.9  58.1  4.5 0.3     43.9      2.2
    R> 15 13.4 39.1 167.3 356.7  70.7 35.2  88.1 39.0  52.4  5.4 0.3     23.6      2.2
    R> 24 20.2 67.7 207.1 973.3 209.1 58.1 138.0 35.4  32.1 16.8 0.8     21.2      2.0
    R> 27 20.6 60.8 233.7 834.0 127.2 40.7  15.4  4.4 132.0 10.7 0.2     18.7      2.9
    R> 23 23.8 54.5 180.6 777.0 125.8 39.5  24.2  3.0  50.1  6.6 0.3     46.0      3.0
    R> 19 22.8 40.9 171.4 691.8 151.4 40.8 104.8 17.6  43.6  9.1 0.4     40.5      3.8
    R>     pH
    R> 18 2.7
    R> 15 2.8
    R> 24 3.0
    R> 27 2.8
    R> 23 2.7
    R> 19 2.7

</div>

</div>

notice: - each row represents one sampling site. - each column in env represents an environmental variable.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Examine the community data 

dim(comm)
```

</div>

<div class="cell-output cell-output-stdout">

    R> [1] 24 44

</div>

<div class="code-copy-outer-scaffold">

``` r
head(comm)
```

</div>

<div class="cell-output cell-output-stdout">

    R>    Callvulg Empenigr Rhodtome Vaccmyrt Vaccviti Pinusylv Descflex Betupube
    R> 18     0.55    11.13     0.00     0.00    17.80     0.07     0.00        0
    R> 15     0.67     0.17     0.00     0.35    12.13     0.12     0.00        0
    R> 24     0.10     1.55     0.00     0.00    13.47     0.25     0.00        0
    R> 27     0.00    15.13     2.42     5.92    15.97     0.00     3.70        0
    R> 23     0.00    12.68     0.00     0.00    23.73     0.03     0.00        0
    R> 19     0.00     8.92     0.00     2.42    10.28     0.12     0.02        0
    R>    Vacculig Diphcomp Dicrsp Dicrfusc Dicrpoly Hylosple Pleuschr Polypili
    R> 18     1.60     2.07   0.00     1.62     0.00      0.0     4.67     0.02
    R> 15     0.00     0.00   0.33    10.92     0.02      0.0    37.75     0.02
    R> 24     0.00     0.00  23.43     0.00     1.68      0.0    32.92     0.00
    R> 27     1.12     0.00   0.00     3.63     0.00      6.7    58.07     0.00
    R> 23     0.00     0.00   0.00     3.42     0.02      0.0    19.42     0.02
    R> 19     0.00     0.00   0.00     0.32     0.02      0.0    21.03     0.02
    R>    Polyjuni Polycomm Pohlnuta Ptilcili Barbhatc Cladarbu Cladrang Cladstel
    R> 18     0.13     0.00     0.13     0.12     0.00    21.73    21.47     3.50
    R> 15     0.23     0.00     0.03     0.02     0.00    12.05     8.13     0.18
    R> 24     0.23     0.00     0.32     0.03     0.00     3.58     5.52     0.07
    R> 27     0.00     0.13     0.02     0.08     0.08     1.42     7.63     2.55
    R> 23     2.12     0.00     0.17     1.80     0.02     9.08     9.22     0.05
    R> 19     1.58     0.18     0.07     0.27     0.02     7.23     4.95    22.08
    R>    Cladunci Cladcocc Cladcorn Cladgrac Cladfimb Cladcris Cladchlo Cladbotr
    R> 18     0.30     0.18     0.23     0.25     0.25     0.23     0.00     0.00
    R> 15     2.65     0.13     0.18     0.23     0.25     1.23     0.00     0.00
    R> 24     8.93     0.00     0.20     0.48     0.00     0.07     0.10     0.02
    R> 27     0.15     0.00     0.38     0.12     0.10     0.03     0.00     0.02
    R> 23     0.73     0.08     1.42     0.50     0.17     1.78     0.05     0.05
    R> 19     0.25     0.10     0.25     0.18     0.10     0.12     0.05     0.02
    R>    Cladamau Cladsp Cetreric Cetrisla Flavniva Nepharct Stersp Peltapht Icmaeric
    R> 18     0.08   0.02     0.02     0.00     0.12     0.02   0.62     0.02        0
    R> 15     0.00   0.00     0.15     0.03     0.00     0.00   0.85     0.00        0
    R> 24     0.00   0.00     0.78     0.12     0.00     0.00   0.03     0.00        0
    R> 27     0.00   0.02     0.00     0.00     0.00     0.00   0.00     0.07        0
    R> 23     0.00   0.00     0.00     0.00     0.02     0.00   1.58     0.33        0
    R> 19     0.00   0.00     0.00     0.00     0.02     0.00   0.28     0.00        0
    R>    Cladcerv Claddefo Cladphyl
    R> 18        0     0.25        0
    R> 15        0     1.00        0
    R> 24        0     0.33        0
    R> 27        0     0.15        0
    R> 23        0     1.97        0
    R> 19        0     0.37        0

</div>

</div>

Notice that:

- each row represents one sampling site.
- each column in comm represents a species.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Transform data 

comm.hel <- decostand(comm,
                     method = "hellinger")

# Run PCA

pca <- rda(comm.hel)

summary(pca)
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> Call:
    R> rda(X = comm.hel) 
    R> 
    R> Partitioning of variance:
    R>               Inertia Proportion
    R> Total          0.3647          1
    R> Unconstrained  0.3647          1
    R> 
    R> Eigenvalues, and their contribution to the variance 
    R> 
    R> Importance of components:
    R>                          PC1     PC2     PC3     PC4     PC5     PC6     PC7
    R> Eigenvalue            0.1459 0.07908 0.02866 0.02446 0.02209 0.01263 0.01179
    R> Proportion Explained  0.4000 0.21684 0.07860 0.06706 0.06057 0.03464 0.03233
    R> Cumulative Proportion 0.4000 0.61680 0.69540 0.76247 0.82304 0.85768 0.89000
    R>                            PC8     PC9     PC10     PC11     PC12     PC13
    R> Eigenvalue            0.008727 0.00755 0.006766 0.004064 0.003161 0.002597
    R> Proportion Explained  0.023930 0.02070 0.018553 0.011143 0.008669 0.007122
    R> Cumulative Proportion 0.913934 0.93464 0.953190 0.964333 0.973002 0.980124
    R>                           PC14     PC15      PC16      PC17     PC18      PC19
    R> Eigenvalue            0.001923 0.001516 0.0009004 0.0008052 0.000606 0.0004496
    R> Proportion Explained  0.005274 0.004158 0.0024691 0.0022081 0.001662 0.0012328
    R> Cumulative Proportion 0.985398 0.989556 0.9920249 0.9942330 0.995895 0.9971275
    R>                            PC20      PC21      PC22      PC23
    R> Eigenvalue            0.0004074 0.0003571 0.0001825 0.0001006
    R> Proportion Explained  0.0011171 0.0009791 0.0005004 0.0002759
    R> Cumulative Proportion 0.9982446 0.9992237 0.9997241 1.0000000

</div>

<div class="code-copy-outer-scaffold">

``` r
# Visualise 

plot(pca, scaling = 2)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The PCA displays sampling sites according to their species composition.

Ask yourself the following questions:

**1. Do sites cluster together?**

- Meaning: This asks if separate geographic locations share highly similar species compositions and abundances.
- How to see it: Look for groups of site points that pack closely together in the ordination space. If sites from “Treatment A” all crowd on the left and “Treatment B” crowd on the right, it proves your environmental groupings structurally determine the community composition <span class="citation" cites="jongman1995 borcard2011">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

**2. Are any sites isolated?**

- Meaning: This checks for ecological outliers—sites that host a bizarrely unique mix of species compared to everything else you sampled.
- How to see it: Look for individual site points sitting far away from the main clouds of data, lingering near the outer margins of the plot space. An isolated site often contains highly dominant unique species or suffered from a sampling anomaly <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

**3. Is there evidence of a gradual ecological gradient?**

- Meaning: This asks if your communities change continuously along an environmental continuum (e.g., smoothly transitioning from upstream to downstream, or low elevation to high elevation), rather than sorting into distinct, hard boundaries.
- How to see it: Look for site points that form a continuous linear band, trail, or curved arc stretching across the plot. If your sites naturally arrange themselves sequentially along an axis <span class="citation" cites="jongman1995 muller1998">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-muller1998" role="doc-biblioref">Müller 1998</a>)</span>.

**4. Which species contribute most strongly to each axis?**

- Meaning: This identifies the specific “driver” species whose presence or absence accounts for the majority of the variation between your sites. It tells you why the axes exist.
- How to see it: Look at the length and alignment of the species vectors (arrows or labels).
  - PC1 Drivers: Species with long horizontal arrows pointing far left or far right drive Axis 1.
  - PC2 Drivers: Species with long vertical arrows pointing straight up or straight down drive Axis 2.
  - Short arrows near the center <span class="math inline">\\(0,0)\\</span> contribute almost nothing to the principal components <span class="citation" cites="borcard2011 jongman1995">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>)</span>.

Remember that PCA axes are mathematical summaries of variation. They do not automatically represent environmental gradients.

</div>

<div id="interpreting-nmds" class="section level3" number="5.3.2">

### <span class="header-section-number">5.3.2</span> Interpreting NMDS

NMDS is based on ranked dissimilarities rather than Euclidean distances. It is therefore particularly useful for ecological community data containing many zeros <span class="citation" cites="oksanen2001 borcard2011">(<a href="#ref-oksanen2001" role="doc-biblioref">Oksanen et al. 2001</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

Unlike PCA, the orientation of the NMDS axes has no ecological meaning. Instead, interpretation focuses on the relative positions of sites.This means that sites that are close together have similar community composition while sites that are far apart have dissimilar communities <span class="citation" cites="borcard2011 jongman1995">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>)</span>.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Calculate a Bray–Curtis dissimilarity matrix and run an NMDS.

nmds <- metaMDS(comm,
                distance = "bray",
                k = 2)
```

</div>

<div class="cell-output cell-output-stdout">

    R> Square root transformation
    R> Wisconsin double standardization
    R> Run 0 stress 0.1843196 
    R> Run 1 stress 0.1852397 
    R> Run 2 stress 0.1825658 
    R> ... New best solution
    R> ... Procrustes: rmse 0.04162338  max resid 0.1517904 
    R> Run 3 stress 0.2376285 
    R> Run 4 stress 0.2396522 
    R> Run 5 stress 0.270462 
    R> Run 6 stress 0.1843196 
    R> Run 7 stress 0.209073 
    R> Run 8 stress 0.2291379 
    R> Run 9 stress 0.2169403 
    R> Run 10 stress 0.2194613 
    R> Run 11 stress 0.2143612 
    R> Run 12 stress 0.2109611 
    R> Run 13 stress 0.2174194 
    R> Run 14 stress 0.1825658 
    R> ... Procrustes: rmse 4.347435e-05  max resid 0.000146979 
    R> ... Similar to previous best
    R> Run 15 stress 0.1825658 
    R> ... New best solution
    R> ... Procrustes: rmse 4.573475e-06  max resid 1.392047e-05 
    R> ... Similar to previous best
    R> Run 16 stress 0.2154149 
    R> Run 17 stress 0.1843196 
    R> Run 18 stress 0.2258686 
    R> Run 19 stress 0.270245 
    R> Run 20 stress 0.1967393 
    R> *** Best solution repeated 1 times

</div>

<div class="code-copy-outer-scaffold">

``` r
nmds
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> Call:
    R> metaMDS(comm = comm, distance = "bray", k = 2) 
    R> 
    R> global Multidimensional Scaling using monoMDS
    R> 
    R> Data:     wisconsin(sqrt(comm)) 
    R> Distance: bray 
    R> 
    R> Dimensions: 2 
    R> Stress:     0.1825658 
    R> Stress type 1, weak ties
    R> Best solution was repeated 1 time in 20 tries
    R> The best solution was from try 15 (random start)
    R> Scaling: centring, PC rotation, halfchange scaling 
    R> Species: expanded scores based on 'wisconsin(sqrt(comm))'

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Plot the NMDS

library(ggplot2)

# Extract site scores and convert them to a data frame
scores_df <- as.data.frame(scores(nmds, display = "sites"))

# Now ggplot will work perfectly
ggplot(scores_df, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 3 ) +
  theme_classic()
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

**Measuring stress**

Stress measures how faithfully the 2D plot represents the original distance matrix <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

| Stress    | Interpretation |
|-----------|----------------|
| \<0.05    | Excellent      |
| \<0.10    | Very good      |
| 0.10–0.20 | Acceptable     |
| \>0.20    | Use cautiously |

<div class="cell">

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

This shows observed distances versus ordination distances. The closer points lie to the line, the better the ordination.

**Environmental Vectors**

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Fitting environmental vectors 

fit <- envfit(nmds,
              env)

fit
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> ***VECTORS
    R> 
    R>             NMDS1    NMDS2     r2 Pr(>r)    
    R> N        -0.05729 -0.99836 0.2536  0.058 .  
    R> P         0.61970  0.78484 0.1938  0.114    
    R> K         0.76643  0.64233 0.1809  0.137    
    R> Ca        0.68518  0.72838 0.4119  0.009 ** 
    R> Mg        0.63250  0.77456 0.4270  0.003 ** 
    R> S         0.19136  0.98152 0.1752  0.128    
    R> Al       -0.87161  0.49019 0.5269  0.001 ***
    R> Fe       -0.93603  0.35191 0.4450  0.002 ** 
    R> Mn        0.79872 -0.60171 0.5231  0.001 ***
    R> Zn        0.61754  0.78654 0.1879  0.116    
    R> Mo       -0.90308  0.42947 0.0609  0.514    
    R> Baresoil  0.92490 -0.38021 0.2508  0.048 *  
    R> Humdepth  0.93284 -0.36029 0.5200  0.002 ** 
    R> pH       -0.64800  0.76164 0.2308  0.055 .  
    R> ---
    R> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    R> Permutation: free
    R> Number of permutations: 999

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Plotting vectors 

plot(nmds)

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

Long arrows indicate stronger correlations with the ordination.

*However, these vectors still describe associations rather than causation.*

Environmental Drivers: If a group of site points is clustered around the tip of a red arrow labeled “Temperature”, those specific sites are characterized by having the highest temperature values in your dataset. Opposing Gradients: Sites sitting on the exact opposite side of the plot (180 degrees away from the arrow tip) have the lowest values for that variable. Orthogonal Variables: If two red arrows form a 90-degree right angle to one another, those two environmental variables are completely uncorrelated and act as independent pressures on your community dynamics.

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Common Mistakes

</div>

</div>

<div class="callout-body-container callout-body">

**Mistake 1** : Assuming the axes represent measured environmental gradients

Except in constrained ordinations, axes are statistical constructs rather than measured ecological variables.

**Mistake 2**: Assuming clusters imply ecological processes

Clusters identify similarity, not causation.

Communities may cluster because of shared environment, shared history, dispersal limitation or unmeasured variables.

**Mistake 3**: Ignoring unexplained variation

Ordinations simplify complex ecological data.

Even excellent ordinations represent only part of the variation present within communities.

**Mistake 4**: Overinterpreting small separations

Small differences between nearby sites may not be ecologically meaningful.

Permutation tests, confidence intervals and complementary analyses should always accompany interpretation.

</div>

</div>

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Do It Now

</div>

</div>

<div class="callout-body-container callout-body">

An NMDS ordination of coastal fish communities shows three distinct clusters corresponding to west coast, south coast and east coast sampling regions.

Before reading further, answer the following questions.

1.  Does the ordination demonstrate that temperature is responsible for these clusters?
2.  List three alternative explanations for the observed separation.
3.  Which additional analyses could help distinguish among these explanations?

*Discussion*

The ordination identifies compositional differences but does not identify their causes. Possible explanations include environmental gradients, dispersal limitation, historical biogeography or unmeasured habitat characteristics. Constrained ordination and variation partitioning provide ways of evaluating these competing hypotheses.

</div>

</div>

</div>

</div>

</div>

<div id="linking-communities-to-the-environment" class="section level1" number="6">

# <span class="header-section-number">6</span> Linking communities to the environment

Ordination methods such as PCA and NMDS allow us to describe multivariate patterns within ecological data, but they do not explicitly evaluate why those patterns occur <span class="citation" cites="jongman1995 borcard2011">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

Constrained ordination addresses this limitation by incorporating explanatory variables directly into the analysis. Instead of asking “How do communities differ?”, constrained ordination asks:

*“How much of the observed variation in community composition can be explained by measured environmental variables?”*

This shift from description to explanation represents one of the most important transitions in quantitative ecology <span class="citation" cites="capblancq2021">(<a href="#ref-capblancq2021" role="doc-biblioref">Capblancq and Forester 2021</a>)</span>.

<div id="what-is-constrained-ordination" class="section level2" number="6.1">

## <span class="header-section-number">6.1</span> What is constrained ordination?

Constrained ordination is a family of multivariate methods in which the arrangement of sampling sites is constrained by predictor variables <span class="citation" cites="terbraak1994">(<a href="#ref-terbraak1994" role="doc-biblioref">Ter Braak 1994</a>)</span>.

These predictor variables are usually environmental measurements such as:

- temperature
- rainfall
- nutrient concentrations
- salinity
- elevation
- habitat complexity

Unlike unconstrained ordination, which finds whatever major pattern exists within the data, constrained ordination is restricted to the patterns associated with the supplied explanatory variables <span class="citation" cites="capblancq2021">(<a href="#ref-capblancq2021" role="doc-biblioref">Capblancq and Forester 2021</a>)</span>.

Consequently, constrained ordinations directly evaluate ecological hypotheses.

</div>

<div id="the-logic-behind-constrained-ordination" class="section level2" number="6.2">

## <span class="header-section-number">6.2</span> The logic behind constrained ordination

The analytical logic is straightforward.

1.  Measure community composition.
2.  Measure environmental variables.
3.  Ask whether variation in community composition can be predicted by those environmental variables.
4.  Evaluate statistical significance using permutation tests.
5.  Interpret the ecological implications.

Importantly, a significant constrained ordination does not prove that environmental variables cause community differences. It indicates that measured environmental variables explain a significant proportion of variation in community composition <span class="citation" cites="dietze2017 dray2012">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>

Alternative explanations may still exist.

</div>

<div id="choosing-the-appropriate-constrained-ordination" class="section level2" number="6.3">

## <span class="header-section-number">6.3</span> Choosing the appropriate constrained ordination

<div id="redundancy-analysis-rda" class="section level3" number="6.3.1">

### <span class="header-section-number">6.3.1</span> Redundancy Analysis (RDA)

RDA is appropriate when:

species respond approximately linearly to environmental gradients Euclidean distances are appropriate transformed community data satisfy linear assumptions

<span class="citation" cites="capblancq2021">Capblancq and Forester (<a href="#ref-capblancq2021" role="doc-biblioref">2021</a>)</span> describe RDA as one of the most versatile multivariate techniques because it combines ordination with multiple regression and allows ecological hypotheses to be evaluated directly.

</div>

<div id="distance-based-redundancy-analysis-db-rda" class="section level3" number="6.3.2">

### <span class="header-section-number">6.3.2</span> Distance-based Redundancy Analysis (db-RDA)

Many ecological datasets violate Euclidean assumptions.

Species abundance data often contain many zeros, non-linear relationships or heterogeneous variances

db-RDA overcomes these limitations by performing constrained ordination using ecological dissimilarity matrices such as Bray–Curtis <span class="citation" cites="legendre2013 borcard2011">(<a href="#ref-legendre2013" role="doc-biblioref">Legendre and De Cáceres 2013</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

Consequently, db-RDA has become one of the most widely used constrained ordination techniques in community ecology.

</div>

</div>

<div id="interpreting-constrained-ordinations" class="section level2" number="6.4">

## <span class="header-section-number">6.4</span> Interpreting constrained ordinations

Several components require interpretation.

**Model significance**

Does the environmental model explain significantly more variation than expected by chance?

Permutation tests answer this question.

**Individual environmental variables**

Which environmental gradients contribute most strongly to community variation?

Not every measured variable will necessarily be significant.

**Explained variation**

How much of total community variation is explained?

Remember that ecological communities are influenced by many interacting processes.

Explaining 20–40% of variation may represent a biologically meaningful result <span class="citation" cites="borcard2011 dray2012">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>.

**Residual variation**

What remains unexplained?

Residual variation may reflect:

- spatial structure
- species interactions
- stochastic processes
- historical effects
- measurement error
- missing environmental variables

Understanding unexplained variation is often just as informative as understanding explained variation <span class="citation" cites="legendre2012 dietze2017">(<a href="#ref-legendre2012" role="doc-biblioref">Legendre et al. 2012</a>; <a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(ggplot2)

df <- data.frame(

x=c(1,2,5,6),

y=c(2,2.2,5,5.3),

lab=c("Site A","Site B","Site C","Site D")

)

ggplot(df,

aes(x,y))+

geom_point(size=5,

colour="blue")+

geom_text(

aes(label=lab),

nudge_y=0.25

)+

annotate(

"segment",

x=2,

y=2.2,

xend=5,

yend=5,

arrow=arrow()

)+

annotate(

"text",

3.8,

3.5,

label="Increasing ecological dissimilarity",

angle=45,

size=5

)+

theme_classic(base_size=15)+

labs(

title="Reading an Ordination Plot"

)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Do It Now

</div>

</div>

<div class="callout-body-container callout-body">

A db-RDA analysing estuarine fish communities produced the following result:

Overall model: P \< 0.001 Salinity explains 28% of the variation. Temperature explains 9%. Geographic distance was not included.

Answer the following.

Can you conclude that salinity is the primary ecological driver? What important information is still missing? Which analytical method introduced in the next section would strengthen your interpretation?

</div>

</div>

</div>

</div>

<div id="variation-partitioning-as-evidence" class="section level1" number="7">

# <span class="header-section-number">7</span> Variation partitioning as evidence

In the previous section, we saw how constrained ordination allows us to test whether measured environmental variables explain variation in community composition. Suppose, for example, that a db-RDA reveals that sea surface temperature explains a significant proportion of variation in kelp-associated communities along the South African coastline.

At first glance, this appears to answer our ecological question. Temperature seems to influence biodiversity.


Variation partitioning asks a more specific question than a standard constrained ordination: not simply whether environmental variables explain community variation, but how the total explained variation can be divided among different ecological drivers <span class="citation" cites="schulz2025">(<a href="#ref-schulz2025" role="doc-biblioref">Schulz et al. 2025</a>)</span>. \## Variation Partitioning with vegan

The varpart() function in vegan provides a simple way to answer this question <span class="citation" cites="oksanen2001">(<a href="#ref-oksanen2001" role="doc-biblioref">Oksanen et al. 2001</a>)</span>.

It calculates how much of the variation in an ecological community can be explained by different sets of environmental or spatial predictors, accounting for the fact that those predictors often overlap <span class="citation" cites="legendre2012 borcard2011">(<a href="#ref-legendre2012" role="doc-biblioref">Legendre et al. 2012</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. The response matrix contains the community data.

<div id="preparing-the-data" class="section level2" number="7.1">

## <span class="header-section-number">7.1</span> Preparing the data

<div class="cell" messages="false">

<div class="code-copy-outer-scaffold">

``` r
library(vegan)

data(varespec)
data(varechem)

comm <- varespec
env <- varechem

# Use a subset of the data 
# Environmental variables
env1 <- env[, c("N", "P", "K")]

# Spatial proxy (example)
coords <- data.frame(
  x = 1:nrow(comm),
  y = rnorm(nrow(comm))
)

library(adespatial)
library(spdep)

nb <- graph2nb(gabrielneigh(as.matrix(coords)), sym = TRUE)
listw <- nb2listw(nb)
MEM <- scores.listw(listw)

mem <- as.data.frame(MEM[,1:3])
```

</div>

</div>

Variation partitioning requires three components.

<div id="response-matrix" class="section level3" number="7.1.1">

### <span class="header-section-number">7.1.1</span> Response matrix

The response matrix contains the community data.

Each row represents a sampling site.

Each column represents a species.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
head(comm)
```

</div>

<div class="cell-output cell-output-stdout">

    R>    Callvulg Empenigr Rhodtome Vaccmyrt Vaccviti Pinusylv Descflex Betupube
    R> 18     0.55    11.13     0.00     0.00    17.80     0.07     0.00        0
    R> 15     0.67     0.17     0.00     0.35    12.13     0.12     0.00        0
    R> 24     0.10     1.55     0.00     0.00    13.47     0.25     0.00        0
    R> 27     0.00    15.13     2.42     5.92    15.97     0.00     3.70        0
    R> 23     0.00    12.68     0.00     0.00    23.73     0.03     0.00        0
    R> 19     0.00     8.92     0.00     2.42    10.28     0.12     0.02        0
    R>    Vacculig Diphcomp Dicrsp Dicrfusc Dicrpoly Hylosple Pleuschr Polypili
    R> 18     1.60     2.07   0.00     1.62     0.00      0.0     4.67     0.02
    R> 15     0.00     0.00   0.33    10.92     0.02      0.0    37.75     0.02
    R> 24     0.00     0.00  23.43     0.00     1.68      0.0    32.92     0.00
    R> 27     1.12     0.00   0.00     3.63     0.00      6.7    58.07     0.00
    R> 23     0.00     0.00   0.00     3.42     0.02      0.0    19.42     0.02
    R> 19     0.00     0.00   0.00     0.32     0.02      0.0    21.03     0.02
    R>    Polyjuni Polycomm Pohlnuta Ptilcili Barbhatc Cladarbu Cladrang Cladstel
    R> 18     0.13     0.00     0.13     0.12     0.00    21.73    21.47     3.50
    R> 15     0.23     0.00     0.03     0.02     0.00    12.05     8.13     0.18
    R> 24     0.23     0.00     0.32     0.03     0.00     3.58     5.52     0.07
    R> 27     0.00     0.13     0.02     0.08     0.08     1.42     7.63     2.55
    R> 23     2.12     0.00     0.17     1.80     0.02     9.08     9.22     0.05
    R> 19     1.58     0.18     0.07     0.27     0.02     7.23     4.95    22.08
    R>    Cladunci Cladcocc Cladcorn Cladgrac Cladfimb Cladcris Cladchlo Cladbotr
    R> 18     0.30     0.18     0.23     0.25     0.25     0.23     0.00     0.00
    R> 15     2.65     0.13     0.18     0.23     0.25     1.23     0.00     0.00
    R> 24     8.93     0.00     0.20     0.48     0.00     0.07     0.10     0.02
    R> 27     0.15     0.00     0.38     0.12     0.10     0.03     0.00     0.02
    R> 23     0.73     0.08     1.42     0.50     0.17     1.78     0.05     0.05
    R> 19     0.25     0.10     0.25     0.18     0.10     0.12     0.05     0.02
    R>    Cladamau Cladsp Cetreric Cetrisla Flavniva Nepharct Stersp Peltapht Icmaeric
    R> 18     0.08   0.02     0.02     0.00     0.12     0.02   0.62     0.02        0
    R> 15     0.00   0.00     0.15     0.03     0.00     0.00   0.85     0.00        0
    R> 24     0.00   0.00     0.78     0.12     0.00     0.00   0.03     0.00        0
    R> 27     0.00   0.02     0.00     0.00     0.00     0.00   0.00     0.07        0
    R> 23     0.00   0.00     0.00     0.00     0.02     0.00   1.58     0.33        0
    R> 19     0.00   0.00     0.00     0.00     0.02     0.00   0.28     0.00        0
    R>    Cladcerv Claddefo Cladphyl
    R> 18        0     0.25        0
    R> 15        0     1.00        0
    R> 24        0     0.33        0
    R> 27        0     0.15        0
    R> 23        0     1.97        0
    R> 19        0     0.37        0

</div>

</div>

</div>

<div id="environmental-variables" class="section level3" number="7.1.2">

### <span class="header-section-number">7.1.2</span> Environmental variables

Environmental variables are stored separately.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
head(env1)
```

</div>

<div class="cell-output cell-output-stdout">

    R>       N    P     K
    R> 18 19.8 42.1 139.9
    R> 15 13.4 39.1 167.3
    R> 24 20.2 67.7 207.1
    R> 27 20.6 60.8 233.7
    R> 23 23.8 54.5 180.6
    R> 19 22.8 40.9 171.4

</div>

</div>

Spatial variables are often generated using Moran’s Eigenvector Maps (MEMs).

These represent spatial structure at multiple spatial scales.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
head(mem)
```

</div>

<div class="cell-output cell-output-stdout">

    R>        MEM1      MEM2        MEM3
    R> 1 -1.201289 0.8006105  0.56336652
    R> 2 -1.617522 1.0489208  0.66408613
    R> 3 -1.753260 1.0290022  0.39682276
    R> 4 -1.371123 0.7611826  0.18455236
    R> 5 -1.567954 0.7666354 -0.01208303
    R> 6 -1.022282 0.2240347 -0.53804615

</div>

</div>

These MEM variables were introduced earlier in the spatial ecology chapter and are treated here as explanatory variables describing spatial relationships among sampling sites.

</div>

</div>

<div id="running-varpart" class="section level2" number="7.2">

## <span class="header-section-number">7.2</span> Running varpart()

The simplest form of variation partitioning compares two explanatory datasets.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
vp <- varpart(
  Y = comm,
  X = env1,
  mem
)
```

</div>

</div>

Notice that varpart() does not perform significance tests.

Instead, it partitions the explained variation into different fractions.

To view the results,

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
vp
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> Partition of variance in RDA 
    R> 
    R> Call: varpart(Y = comm, X = env1, mem)
    R> 
    R> Explanatory tables:
    R> X1:  env1
    R> X2:  mem 
    R> 
    R> No. of explanatory tables: 2 
    R> Total variation (SS): 41990 
    R>             Variance: 1825.7 
    R> No. of observations: 24 
    R> 
    R> Partition table:
    R>                      Df R.squared Adj.R.squared Testable
    R> [a+c] = X1            3   0.23842       0.12418     TRUE
    R> [b+c] = X2            3   0.52654       0.45552     TRUE
    R> [a+b+c] = X1+X2       6   0.62957       0.49884     TRUE
    R> Individual fractions                                    
    R> [a] = X1|X2           3                 0.04331     TRUE
    R> [b] = X2|X1           3                 0.37466     TRUE
    R> [c]                   0                 0.08087    FALSE
    R> [d] = Residuals                         0.50116    FALSE
    R> ---
    R> Use function 'rda' to test significance of fractions of interest

</div>

</div>

A typical output might look like this.

</div>

<div id="understanding-the-output" class="section level2" number="7.3">

## <span class="header-section-number">7.3</span> Understanding the output

The output can appear intimidating at first, but each fraction has a straightforward ecological interpretation.

**Fraction \[a\]**

Environment \| Space

This represents pure environmental variation.

It answers the question:

How much variation is explained by environmental variables after removing spatial effects?

If this value is large, environmental filtering probably plays an important role.

**Fraction \[b\]** Space \| Environment

This represents pure spatial variation.

It measures variation explained by spatial variables after removing measured environmental gradients.

Large values suggest dispersal limitation, historical effects or unmeasured spatially structured variables.

**Fraction \[c\]**

Shared variation.

Both environmental and spatial variables explain this component.

This is extremely common because environmental gradients often change geographically.

**Fraction \[d\]**

Residual variation.

Everything left unexplained.

**X**

In a varpart analysis, X1 and X2 represent distinct matrices or data frames of explanatory variables that you use to explain the patterns in your community data In this case, as we have chosen, X1 represents environmental conditions and X2 represents spatial structure (mem)

<div class="callout callout-style-simple callout-note">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

Residual variation often remains surprisingly large in ecological datasets. This is completely normal.

</div>

</div>

</div>

</div>

<div id="visualising-the-partition" class="section level2" number="7.4">

## <span class="header-section-number">7.4</span> Visualising the partition

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
knitr::include_graphics("varpart.jpeg")
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The partition can be plotted directly.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
plot(vp)
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The resulting Venn diagram illustrates how variation is divided among the explanatory datasets.

Students should remember that the diagram represents variation, not numbers of species or sampling sites.

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Common mistakes

</div>

</div>

<div class="callout-body-container callout-body">

When first using varpart(), students often make the following mistakes:

- Interpreting adjusted R² values as percentages of total biodiversity. They instead represent the proportion of variation explained by the explanatory variables.
- Assuming that a large shared fraction indicates causation. Shared variation simply reflects overlap between predictor sets.
- Ignoring significance testing. varpart() partitions variation but does not test whether fractions differ from zero.
- Using highly collinear environmental variables. Strong collinearity can inflate shared fractions and complicate interpretation.
- Focusing only on the Venn diagram. The diagram is useful for visualisation, but ecological interpretation should always be based on the adjusted R² values and permutation tests.

</div>

</div>

</div>

</div>

<div id="trait-environment-analyses" class="section level1" number="8">

# <span class="header-section-number">8</span> Trait-environment analyses


RLQ analysis and fourth-corner analysis were developed specifically to answer this question <span class="citation" cites="doledec1991 dray2008">(<a href="#ref-doledec1991" role="doc-biblioref">Dolédec and Chessel 1991</a>; <a href="#ref-dray2008" role="doc-biblioref">Dray and Legendre 2008</a>)</span>.

<div id="rlq-analysis" class="section level2" number="8.1">

## <span class="header-section-number">8.1</span> RLQ analysis

RLQ analysis is unique because it simultaneously considers three ecological tables: environment by sites, species by sites, and species by traits <span class="citation" cites="doledec1991 dray2008">(<a href="#ref-doledec1991" role="doc-biblioref">Dolédec and Chessel 1991</a>; <a href="#ref-dray2008" role="doc-biblioref">Dray and Legendre 2008</a>)</span>. By combining these three matrices, RLQ allows us to explore how environmental conditions filter species according to their functional traits.

In other words, instead of simply asking “Which sites are similar in species composition?”, RLQ asks “Which environmental conditions favour which ecological strategies?” This makes RLQ particularly powerful for studying functional ecology, where the goal is to understand how traits such as body size, dispersal ability or feeding strategy respond to environmental gradients <span class="citation" cites="pavoine2011 schmera2023">(<a href="#ref-pavoine2011" role="doc-biblioref">Pavoine and Bonsall 2011</a>; <a href="#ref-schmera2023" role="doc-biblioref">Schmera et al. 2023</a>)</span>.

**The three table problem**

The strength of RLQ lies in its ability to link three different datasets. The species table connects the environmental variables to the functional traits. By analysing all three simultaneously, RLQ can reveal whether certain traits are consistently favoured under particular environmental conditions. This moves beyond species replacement to highlight ecological filtering processes.

Consider two streams. One is cold, fast‑flowing and rocky and the other is warm, slow‑flowing and silty. Traditional analyses might ask which species differ between the two streams. RLQ instead asks whether these contrasting environments favour different ecological strategies. For example, cold, fast‑flowing streams may favour species with streamlined bodies and strong attachment traits, while warm, silty streams may favour species adapted to low oxygen or fine substrates. RLQ therefore provides insight into how traits, rather than species identities alone, are structured by the environment

</div>

<div id="interpreting-an-rlq" class="section level2" number="8.2">

## <span class="header-section-number">8.2</span> Interpreting an RLQ

This example uses the aravo dataset, which is built into the ade4 package and compiles the 3 matrices from alpine plant communities in the Aravo region of the French Alp

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(ade4)

# Load the built-in dataset
data(aravo)

# View the structural components
# Site by Environment (R)
head(aravo$env)    
```

</div>

<div class="cell-output cell-output-stdout">

    R>      Aspect Slope Form PhysD ZoogD Snow
    R> AR07      7     2    1    50    no  140
    R> AR71      1    35    3    40    no  140
    R> AR26      5     0    3    20    no  140
    R> AR54      9    30    3    80    no  140
    R> AR60      9     5    1    80    no  140
    R> AR70      1    30    3    40    no  140

</div>

<div class="code-copy-outer-scaffold">

``` r
# Site by Species Abundance (L)
head(aravo$spe)    
```

</div>

<div class="cell-output cell-output-stdout">

    R>      Agro.rupe Alop.alpi Anth.nipp Heli.sede Aven.vers Care.rosa Care.foet
    R> AR07         0         0         0         0         0         1         0
    R> AR71         0         0         0         0         0         2         0
    R> AR26         3         0         1         0         1         2         0
    R> AR54         0         0         0         2         0         2         0
    R> AR60         0         0         0         0         0         0         0
    R> AR70         0         0         0         0         0         3         0
    R>      Care.parv Care.rupe Care.semp Fest.laev Fest.quad Fest.viol Kobr.myos
    R> AR07         0         3         0         0         2         0         1
    R> AR71         1         0         0         0         0         0         3
    R> AR26         0         0         1         0         3         1         4
    R> AR54         0         0         0         0         1         0         1
    R> AR60         0         0         0         0         3         1         1
    R> AR70         1         1         0         0         0         0         4
    R>      Luzu.lute Poa.alpi Poa.supi Sesl.caer Alch.pent Alch.glau Alch.vulg
    R> AR07         0        1        0         0         0         0         0
    R> AR71         0        2        0         0         0         0         0
    R> AR26         1        0        0         0         0         1         0
    R> AR54         0        2        0         1         0         0         0
    R> AR60         0        0        0         1         0         0         0
    R> AR70         0        2        0         1         0         0         0
    R>      Andr.brig Ante.carp Ante.dioi Arni.mont Aste.alpi Bart.alpi Camp.sche
    R> AR07         0         0         0         0         0         0         0
    R> AR71         0         1         0         0         0         1         1
    R> AR26         0         1         0         0         0         0         1
    R> AR54         0         0         0         0         0         0         1
    R> AR60         0         0         0         0         0         0         1
    R> AR70         0         1         0         0         0         1         1
    R>      Card.alpi Cera.stri Cera.cera Leuc.alpi Cirs.acau Drab.aizo Drya.octo
    R> AR07         0         0         0         0         0         0         0
    R> AR71         0         0         0         0         0         0         1
    R> AR26         0         0         0         0         0         0         0
    R> AR54         0         0         0         0         0         1         0
    R> AR60         0         0         0         0         0         0         0
    R> AR70         0         1         0         0         0         0         1
    R>      Erig.unif Gent.camp Gent.acau Gent.vern Geum.mont Omal.supi Andr.vita
    R> AR07         0         0         0         0         0         0         0
    R> AR71         0         0         0         0         0         0         0
    R> AR26         1         0         1         1         2         0         0
    R> AR54         1         0         0         1         0         0         0
    R> AR60         0         0         0         1         0         0         1
    R> AR70         0         0         0         0         0         0         0
    R>      Hier.pili Homo.alpi Leon.pyre Ligu.muto Lloy.sero Minu.sedo Minu.vern
    R> AR07         0         0         0         0         0         1         1
    R> AR71         0         0         0         1         1         0         0
    R> AR26         0         0         0         0         0         0         1
    R> AR54         0         0         0         1         0         0         1
    R> AR60         0         0         0         0         0         0         1
    R> AR70         0         1         0         2         1         0         0
    R>      Phyt.orbi Plan.alpi Poly.vivi Pote.aure Pote.cran Pote.gran Puls.vern
    R> AR07         0         0         1         0         0         0         0
    R> AR71         0         0         1         0         0         0         0
    R> AR26         0         0         1         2         1         0         2
    R> AR54         0         0         0         0         0         0         0
    R> AR60         0         0         1         0         0         0         0
    R> AR70         0         0         1         0         0         0         0
    R>      Ranu.kuep Sagi.glab Sali.herb Sali.reti Sali.retu Sali.serp Saxi.pani
    R> AR07         0         0         0         0         0         0         1
    R> AR71         0         0         0         1         2         1         0
    R> AR26         0         0         0         0         0         0         0
    R> AR54         0         0         0         0         0         0         2
    R> AR60         0         0         0         0         0         0         1
    R> AR70         0         0         0         2         2         0         1
    R>      Sedu.alpe Semp.mont Sene.inca Sibb.proc Sile.acau Thym.poly Vero.alpi
    R> AR07         0         0         0         0         1         0         0
    R> AR71         0         1         0         0         1         0         0
    R> AR26         0         1         0         0         1         0         0
    R> AR54         0         1         0         0         0         2         0
    R> AR60         0         0         0         0         0         0         0
    R> AR70         0         1         0         0         2         0         0
    R>      Vero.alli Vero.bell Myos.alpe Tara.alpi Scab.luci Anth.alpe Oxyt.camp
    R> AR07         0         0         0         0         0         0         1
    R> AR71         0         0         0         0         0         1         0
    R> AR26         0         0         1         0         1         0         1
    R> AR54         0         1         1         0         0         0         0
    R> AR60         0         0         0         0         0         0         0
    R> AR70         0         1         0         0         0         0         0
    R>      Oxyt.lapp Lotu.alpi Trif.alpi Trif.badi Trif.thal
    R> AR07         1         0         0         0         0
    R> AR71         1         0         0         0         0
    R> AR26         0         0         0         0         0
    R> AR54         1         0         0         0         0
    R> AR60         1         0         0         0         0
    R> AR70         1         0         0         0         0

</div>

<div class="code-copy-outer-scaffold">

``` r
# Species by Traits (Q)
head(aravo$traits) 
```

</div>

<div class="cell-output cell-output-stdout">

    R>           Height Spread Angle  Area Thick  SLA N_mass Seed
    R> Agro.rupe      6     10    80  60.0  0.12  8.1 218.70 0.08
    R> Alop.alpi      5     20    20 190.9  0.20 15.1 203.85 0.21
    R> Anth.nipp     15      5    50 280.0  0.08 18.0 219.60 0.54
    R> Heli.sede      0     30    80 600.0  0.20 10.6 233.20 1.72
    R> Aven.vers     12     30    60 420.0  0.14 12.5 156.25 1.17
    R> Care.rosa     30     20    80 180.0  0.40  6.5 208.65 1.68

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
R <- aravo$env     # Environmental variables (75 sites)
L <- aravo$spe     # Species abundance data (75 sites x 82 species)
Q <- aravo$traits  # Functional traits (82 species)

# 2. Preliminary Ordinations (Corrected for mixed data types)
# - Correspondence Analysis (CA) on the species matrix (L)
pca_L <- dudi.coa(L, scannf = FALSE, nf = 2)

# - Use Hill-Smith instead of PCA for the environment matrix (R)
pca_R <- dudi.hillsmith(R, row.w = pca_L$lw, scannf = FALSE, nf = 2)

# - Use Hill-Smith for traits matrix (Q)
pca_Q <- dudi.hillsmith(Q, row.w = pca_L$cw, scannf = FALSE, nf = 2)

# 3. Run RLQ Analysis
rlq_results <- rlq(pca_R, pca_L, pca_Q, scannf = FALSE, nf = 2)
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(ggplot2)
library(ggrepel)
library(patchwork)

# 1. Prepare Environmental Variable Coordinates Safely
env_coords <- rlq_results$co
# Dynamically rename the first two columns to X and Y
colnames(env_coords)[1:2] <- c("X", "Y")
env_coords$Variable <- rownames(env_coords)

plot_env <- ggplot(env_coords, aes(x = X, y = Y, label = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_segment(aes(xend = X, yend = Y), x = 0, y = 0, 
               arrow = arrow(length = unit(0.2, "cm")), color = "#2c3e50", size = 0.8) +
  geom_label_repel(fill = "white", color = "#2c3e50", fontface = "bold", box.padding = 0.5) +
  labs(title = "RLQ: Environmental Drivers", x = "RLQ Axis 1", y = "RLQ Axis 2") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# 2. Prepare Plant Trait Coordinates Safely
trait_coords <- rlq_results$li
# Dynamically rename the first two columns to X and Y
colnames(trait_coords)[1:2] <- c("X", "Y")
trait_coords$Trait <- rownames(trait_coords)

plot_traits <- ggplot(trait_coords, aes(x = X, y = Y, label = Trait)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_point(color = "#e74c3c", size = 3, alpha = 0.7) +
  geom_text_repel(color = "#333333", max.overlaps = 15, box.padding = 0.3) +
  labs(title = "RLQ: Functional Traits", x = "RLQ Axis 1", y = "RLQ Axis 2") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# 3. Combine and display side-by-side
plot_env + plot_traits
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

Like other ordination methods, each RLQ axis represents an ecological gradient.

The first axis explains the greatest amount of joint variation between environmental variables and species traits.

Subsequent axes explain progressively smaller amounts of variation.

Rather than assigning meaning based solely on axis labels, ask:

1.  Which environmental variables define this axis?
2.  Which traits change along this gradient?
3.  What ecological process could explain this relationship?

For example:

Axis 1 may represent a gradient from shallow, high-energy habitats to deeper, sheltered habitats, accompanied by a shift from small, fast-growing species to larger, long-lived species.

The ecological interpretation comes from combining the environmental variables, traits and species positions rather than considering each independently. \## Fourth-corner analysis

While RLQ identifies broad relationships between environmental variables and functional traits, it does not specify which individual traits are associated with which environmental variables. Fourth-corner analysis addresses this gap. It is named after the “missing corner” in the three-table framework: the direct link between environment and traits <span class="citation" cites="dray2008">(<a href="#ref-dray2008" role="doc-biblioref">Dray and Legendre 2008</a>)</span>.

Imagine three tables: environment by sites, species by sites, and species by traits. The missing relationship is environment by traits. Fourth‑corner analysis provides a statistical framework to test this relationship directly.

**What does it test?**

Fourth-corner analysis evaluates every possible combination of environmental variable and trait. For each pair, a statistical test is performed to determine whether the association is stronger than expected by chance <span class="citation" cites="dray2008">(<a href="#ref-dray2008" role="doc-biblioref">Dray and Legendre 2008</a>)</span>. For example, altitude may be tested against body size, flow velocity against feeding habit, or temperature against reproductive strategy. Permutation tests are used to assess significance, ensuring that observed associations are not simply due to random variation <span class="citation" cites="dray2008 borcard2011">(<a href="#ref-dray2008" role="doc-biblioref">Dray and Legendre 2008</a>; <a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.

</div>

<div id="interpreting-a-fourth-corner-analysis" class="section level2" number="8.3">

## <span class="header-section-number">8.3</span> Interpreting a fourth-corner analysis

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(ade4)

data(aravo)

# aravo$spe    - site x species abundance table
# aravo$env    - site x environmental variable table
# aravo$traits - species x trait table

# Fourth-corner test: does the environment-trait relationship
# (the "missing corner" of the three tables) differ from random?
four_aravo <- fourthcorner(
  aravo$env, aravo$spe, aravo$traits,
  modeltype = 6,          # permutes both sites and species (recommended)
  nrepet = 999,
  p.adjust.method.G = "fdr",
  p.adjust.method.D = "fdr"
)

summary(four_aravo)
```

</div>

<div class="cell-output cell-output-stdout">

    R> Fourth-corner Statistics
    R> ------------------------
    R> Permutation method  Comb. 2 and 4  ( 999  permutations)
    R> 
    R> Adjustment method for multiple comparisons:   fdr 
    R>               Test Stat           Obs     Std.Obs     Alter Pvalue Pvalue.adj
    R> 1  Aspect / Height    r  -0.045735104 -1.16505202 two-sided  0.265  0.3634286
    R> 2   Slope / Height    r   0.094917344  1.81475716 two-sided  0.073  0.1523478
    R> 3    Form / Height    F  15.219879474  1.99247877   greater  0.040  0.0960000
    R> 4   PhysD / Height    r   0.113164322  1.79307888 two-sided  0.066  0.1440000
    R> 5   ZoogD / Height    F  15.227717714  1.20212759   greater  0.112  0.2108571
    R> 6    Snow / Height    r  -0.271739531 -2.52093936 two-sided  0.013  0.0445714
    R> 7  Aspect / Spread    r  -0.044170141 -1.12721716 two-sided  0.265  0.3634286
    R> 8   Slope / Spread    r  -0.017325425 -0.34791929 two-sided  0.727  0.8308571
    R> 9    Form / Spread    F   5.548173196  0.25968919   greater  0.258  0.3634286
    R> 10  PhysD / Spread    r  -0.051680330 -0.82232090 two-sided  0.415  0.5383784
    R> 11  ZoogD / Spread    F   0.120114487 -0.97556033   greater  0.982  0.9820000
    R> 12   Snow / Spread    r   0.065634673  0.66310130 two-sided  0.522  0.6424615
    R> 13  Aspect / Angle    r  -0.090837201 -1.80127566 two-sided  0.084  0.1165714
    R> 14   Slope / Angle    r   0.100281966  1.83434617 two-sided  0.065  0.1006452
    R> 15    Form / Angle    F  30.664664234  5.73000995   greater  0.003  0.0130909
    R> 16   PhysD / Angle    r   0.221380084  3.45878230 two-sided  0.002  0.0096000
    R> 17   ZoogD / Angle    F  28.051040522  3.02514506   greater  0.017  0.0544000
    R> 18    Snow / Angle    r  -0.269613756 -2.53417538 two-sided  0.008  0.0320000
    R> 19   Aspect / Area    r   0.031237858  0.76198233 two-sided  0.482  0.6088421
    R> 20    Slope / Area    r  -0.003864605 -0.08146422 two-sided  0.944  0.9640851
    R> 21     Form / Area    F  13.609309880  2.11308851   greater  0.044  0.1005714
    R> 22    PhysD / Area    r  -0.134371361 -2.20433953 two-sided  0.026  0.0734118
    R> 23    ZoogD / Area    F  49.672266332  6.81310945   greater  0.001  0.0053333
    R> 24     Snow / Area    r  -0.024574466 -0.19097967 two-sided  0.847  0.9057391
    R> 25  Aspect / Thick    r  -0.058466142 -1.43139280 two-sided  0.165  0.2640000
    R> 26   Slope / Thick    r   0.074151819  1.47867916 two-sided  0.150  0.2482759
    R> 27    Form / Thick    F  14.204346501  2.29483233   greater  0.035  0.0933333
    R> 28   PhysD / Thick    r   0.143161734  2.42515839 two-sided  0.013  0.0445714
    R> 29   ZoogD / Thick    F   2.825887968 -0.60121546   greater  0.648  0.7586341
    R> 30    Snow / Thick    r  -0.154660144 -1.55644638 two-sided  0.123  0.2108571
    R> 31    Aspect / SLA    r  -0.007694551 -0.13174404 two-sided  0.899  0.9370000
    R> 32     Slope / SLA    r  -0.235864886 -4.91093545 two-sided  0.001  0.0053333
    R> 33      Form / SLA    F 100.787472071 22.28114977   greater  0.001  0.0053333
    R> 34     PhysD / SLA    r  -0.275524984 -4.63429371 two-sided  0.001  0.0053333
    R> 35     ZoogD / SLA    F   0.984301951 -0.92824976   greater  0.886  0.9370000
    R> 36      Snow / SLA    r   0.481181824  4.91617727 two-sided  0.001  0.0053333
    R> 37 Aspect / N_mass    r  -0.061575524 -1.06333930 two-sided  0.302  0.3624000
    R> 38  Slope / N_mass    r  -0.201308154 -4.11858696 two-sided  0.001  0.0053333
    R> 39   Form / N_mass    F  70.042280400 14.54879074   greater  0.001  0.0053333
    R> 40  PhysD / N_mass    r  -0.212434381 -3.45241268 two-sided  0.001  0.0053333
    R> 41  ZoogD / N_mass    F  10.300092724  0.62727342   greater  0.196  0.3034839
    R> 42   Snow / N_mass    r   0.429271163  4.26566098 two-sided  0.001  0.0053333
    R> 43   Aspect / Seed    r   0.011598435  0.30159473 two-sided  0.776  0.8662326
    R> 44    Slope / Seed    r   0.077073974  1.54355050 two-sided  0.119  0.2108571
    R> 45     Form / Seed    F   5.561841954  0.20606027   greater  0.296  0.3946667
    R> 46    PhysD / Seed    r   0.078156305  1.23642053 two-sided  0.221  0.3315000
    R> 47    ZoogD / Seed    F   3.369068338 -0.50391464   greater  0.595  0.7140000
    R> 48     Snow / Seed    r  -0.177640721 -1.71437755 two-sided  0.087  0.1740000
    R>      
    R> 1    
    R> 2    
    R> 3   .
    R> 4    
    R> 5    
    R> 6   *
    R> 7    
    R> 8    
    R> 9    
    R> 10   
    R> 11   
    R> 12   
    R> 13   
    R> 14   
    R> 15  *
    R> 16 **
    R> 17  .
    R> 18  *
    R> 19   
    R> 20   
    R> 21   
    R> 22  .
    R> 23 **
    R> 24   
    R> 25   
    R> 26   
    R> 27  .
    R> 28  *
    R> 29   
    R> 30   
    R> 31   
    R> 32 **
    R> 33 **
    R> 34 **
    R> 35   
    R> 36 **
    R> 37   
    R> 38 **
    R> 39 **
    R> 40 **
    R> 41   
    R> 42 **
    R> 43   
    R> 44   
    R> 45   
    R> 46   
    R> 47   
    R> 48   
    R> 
    R> ---
    R> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

</div>

<div class="code-copy-outer-scaffold">

``` r
plot(four_aravo, alpha = 0.05, stat = "D2")
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

Remember that colour intensity encodes the strength/direction of association; the stars encode the adjusted p-value. A common student error is reading colour as significance and stars as effect size — it’s the reverse.

</div>

</div>

<div id="uncertainty-and-ecological-inference" class="section level1" number="9">

# <span class="header-section-number">9</span> Uncertainty and ecological inference

By this point in the chapter, it should be clear that no single statistical method can fully explain why ecological communities are structured the way they are. Each analysis contributes a different piece of the puzzle. Ordination methods help us visualise patterns in community composition, constrained ordinations allow us to test whether measured environmental variables explain those patterns, and variation partitioning separates the influence of environmental and spatial processes. Individually these methods are informative, but they become much more powerful when they are interpreted together. The goal is not simply to choose the “best” analysis, but to use a combination of complementary approaches to answer an ecological question using multiple lines of evidence <span class="citation" cites="jongman1995 terbraak1994">(<a href="#ref-jongman1995" role="doc-biblioref">Jongman et al. 1995</a>; <a href="#ref-terbraak1994" role="doc-biblioref">Ter Braak 1994</a>)</span>.


Variation partitioning extends this process even further, moving past the question of whether environmental variables explain community composition to ask how much of that explanation is uniquely environmental, how much is purely spatial, and how much is shared between the two <span class="citation" cites="legendre2012">(<a href="#ref-legendre2012" role="doc-biblioref">Legendre et al. 2012</a>)</span>. This distinction is important because environmental gradients are often spatially structured. Along a coastline, for example, sea surface temperature changes gradually with latitude, making it difficult to separate environmental filtering from spatial location. Variation partitioning helps disentangle these overlapping influences, but its results should always be interpreted alongside ordination and constrained ordination rather than in isolation <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>.


Recent ecological studies illustrate this well. <span class="citation" cites="he2024">He et al. (<a href="#ref-he2024" role="doc-biblioref">2024</a>)</span>, for example, combined ordination methods with variation partitioning to investigate freshwater macroinvertebrate communities across broad latitudinal gradients, showing that species sorting along environmental gradients explained biodiversity patterns more effectively than any single statistical method could on its own.

Ultimately, ecological inference is built through a logical sequence of analyses rather than a single statistical test. Ordination identifies patterns in community composition, constrained ordination tests whether measured environmental variables explain those patterns, and variation partitioning separates the environmental signal from the influence of spatial structure. Together, these approaches provide a much stronger basis for ecological interpretation than any one method on its own <span class="citation" cites="legendre2013">(<a href="#ref-legendre2013" role="doc-biblioref">Legendre and De Cáceres 2013</a>)</span>. The aim is not simply to produce figures or statistical outputs, but to build a well-supported ecological explanation that is transparent, reproducible, and grounded in multiple sources of evidence <span class="citation" cites="pavoine2011">(<a href="#ref-pavoine2011" role="doc-biblioref">Pavoine and Bonsall 2011</a>)</span>.

</div>

<div id="a-framework-for-ecological-inference" class="section level1" number="10">

# <span class="header-section-number">10</span> A framework for ecological inference

The analytical workflow developed throughout this chapter can be summarised in six questions.

**Step 1.** Is there a biodiversity pattern?

Begin by visualising the data. Ordination methods identify whether communities differ and whether ecological gradients appear to exist.

**Step 2.** What ecological hypotheses could explain this pattern?

Avoid jumping directly to environmental explanations.

Consider multiple possibilities.

For example:

- environmental filtering
- dispersal limitation
- historical processes
- disturbance
- stochasticity

**Step 3.** Which environmental variables explain community structure?

Constrained ordination allows environmental hypotheses to be evaluated directly.

At this stage we identify significant environmental predictors.

**Step 4.** Are environmental and spatial effects independent?

Variation partitioning separates overlapping sources of variation. Rather than asking whether temperature matters, we ask whether temperature explains biodiversity independently of spatial structure.

**Step 5.** How much uncertainty remains?

No ecological model explains all observed variation.

Students should always ask:

- What has not been measured?
- Which assumptions could influence interpretation?
- Could alternative hypotheses still explain these results?

**Step 6.** What ecological conclusion is supported?

Finally, combine all available evidence. Ecological inference is strongest when multiple complementary analyses support the same interpretation.

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Do It Now

</div>

</div>

<div class="callout-body-container callout-body">

Read the following statement.

“Temperature causes changes in community composition because the RDA was significant.”

1.  Do you agree?
2.  Identify at least three reasons why this conclusion may be too strong.

</div>

</div>

</div>

<div id="the-ecoevidence-package" class="section level1" number="11">

# <span class="header-section-number">11</span> The ‘ecoevidence’ package

Modern ecological datasets are increasingly complex, often containing information on hundreds of species together with environmental, spatial, and temporal variables. While statistical software such as vegan, adespatial, and betapart provides powerful tools for ordination, diversity analysis, clustering, constrained ordination, and variation partitioning, these packages generally stop at the point of statistical output. The challenge of interpreting results, developing hypotheses, and applying findings to conservation decisions remains with the researcher.

For students, this creates a gap between producing statistical figures and understanding their ecological meaning. Generating an NMDS ordination or variation partitioning diagram is straightforward, but deciding which processes explain the observed patterns, or how those results inform management, is far more difficult. The ecoevidence package was developed to bridge this gap. Rather than replacing existing tools, it builds upon them by offering a structured workflow that guides users from raw community data through statistical analysis, ecological interpretation, and ultimately evidence‑based inference. Originally created as part of an Honours project in Biodiversity and Conservation Biology, its aim is to provide a reproducible framework that supports both ecological learning and decision‑making.

Ecoevidence is not simply another set of R functions. It is a framework for ecological reasoning. By encouraging users to move systematically from identifying patterns to developing hypotheses, evaluating evidence, and considering alternative explanations, the package helps transform statistical outputs into transparent ecological interpretations that can inform conservation management, building on a long tradition of using species assemblages as indicators for conservation monitoring <span class="citation" cites="kremen1992">(<a href="#ref-kremen1992" role="doc-biblioref">Kremen 1992</a>)</span>.

<div id="philosophy-of-the-package" class="section level2" number="11.1">

## <span class="header-section-number">11.1</span> Philosophy of the package

The package is based on a simple principle:

Statistical significance is not ecological evidence.

Statistical analyses identify patterns and quantify relationships. Ecological inference requires those results to be interpreted within the context of ecological theory, environmental observations, spatial processes, sampling design, and existing scientific knowledge.

Rather than producing automatic conclusions, the package encourages users to ask a series of ecological questions.

1.  What patterns are present in the community?
2.  Which ecological processes could explain those patterns?
3.  What evidence supports each explanation?
4.  Are alternative mechanisms equally plausible?
5.  How confident are we in our interpretation?
6.  What additional evidence would strengthen our conclusions?
7.  What conservation actions follow from the available evidence?

</div>

<div id="how-to-use-ecoevidence" class="section level2" number="11.2">

## <span class="header-section-number">11.2</span> How to use ecoevidence

The link below takes you to a worked example of how to use the package.

<a href="Doubs_River_Practice.html" target="_blank">Open walk-through in New Tab</a>

</div>

</div>

<div id="worked-example" class="section level1" number="12">

# <span class="header-section-number">12</span> Worked example

<div id="case-study-macroinvertebrates-of-the-river-loire" class="section level2" number="12.1">

## <span class="header-section-number">12.1</span> Case study: macroinvertebrates of the River Loire

The sections above set out six questions that turn a biodiversity pattern into an ecological inference. This worked example applies that same sequence to a real dataset rather than a simulated one: `macroloire`, distributed with the **ade4** package. The data describe benthic macroinvertebrate assemblages sampled at sites along the River Loire (France), together with site-level environmental measurements, spatial information and a fuzzy-coded functional trait table for the taxa recorded. Because the dataset combines an abundance table, an environmental table and a trait table, it lets us extend the six-step framework into the trait–environment methods introduced later in the chapter — RLQ and the fourth-corner test — and into a beta-diversity decomposition, so that the worked example draws on nearly every tool covered in this module.

The guiding ecological question is the one already familiar from the coastal and Doubs River examples used earlier: *as we move along the river, from the source towards the confluence with larger rivers, does the invertebrate assemblage change in a way that can be attributed to measurable environmental conditions, or could the same pattern be produced by other processes?*

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(ade4)
library(vegan)
library(adespatial)
library(betapart)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

set.seed(2026)
```

</div>

</div>

<div id="step-1-is-there-a-biodiversity-pattern-inspecting-the-data-and-running-a-baseline-ordination" class="section level3" number="12.1.1">

### <span class="header-section-number">12.1.1</span> Step 1: Is there a biodiversity pattern? Inspecting the data and running a baseline ordination

Before writing any analysis code, it is worth confirming what the object actually contains rather than assuming that column names match another dataset used earlier in the course. The help file and the structure of the object are the authoritative source, not a previous script.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
data(macroloire, package = "ade4")
names(macroloire)
```

</div>

<div class="cell-output cell-output-stdout">

    R> [1] "fau"    "traits" "taxo"   "envir"  "labels"

</div>

<div class="code-copy-outer-scaffold">

``` r
str(macroloire, max.level = 1)
```

</div>

<div class="cell-output cell-output-stdout">

    R> List of 5
    R>  $ fau   :'data.frame':  40 obs. of  38 variables:
    R>  $ traits:'data.frame':  40 obs. of  11 variables:
    R>  $ taxo  :Classes 'taxo' and 'data.frame':   40 obs. of  3 variables:
    R>  $ envir :'data.frame':  38 obs. of  6 variables:
    R>  $ labels:'data.frame':  40 obs. of  1 variable:

</div>

</div>

From this output we can confirm which table holds the taxon abundances (`$fau`), which columns of `$env` describe distance from the source, altitude, damming and morphoregion, and how the trait table (`$traits`) is organised into blocks. With that confirmed, the assemblage table is transposed into the sites-by-taxa orientation used throughout the module, Hellinger-transformed, and summarised with an unconstrained correspondence analysis — the same starting point used for the Doubs and dune examples earlier in the chapter.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
fau <- as.data.frame(t(macroloire$fau))       # sites (rows) x taxa (columns)
fau_hel <- decostand(fau, method = "hellinger")

env_var <- macroloire$env$Distance             # distance from source, km

coa <- dudi.coa(fau, scannf = FALSE, nf = 2)
pca <- dudi.pca(fau_hel, scannf = FALSE, nf = 2)

cor(coa$li[, 1], env_var, method = "spearman")
```

</div>

<div class="cell-output cell-output-stdout">

    R> [1] 0.8498742

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
coa_v1 <- round(coa$eig[1] / sum(coa$eig) * 100, 1)
coa_v2 <- round(coa$eig[2] / sum(coa$eig) * 100, 1)

site_scores <- coa$li
site_scores$Site <- rownames(site_scores)
site_scores$Distance <- macroloire$env$Distance

fig1 <- ggplot(site_scores, aes(Axis1, Axis2)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(colour = Distance), size = 3) +
  geom_text_repel(aes(label = Site), size = 2.6, max.overlaps = Inf) +
  scale_colour_viridis_c(name = "Distance from\nsource (km)") +
  coord_equal() +
  labs(title = "Correspondence analysis of the Loire macroinvertebrate assemblage",
       subtitle = "Sites coloured by distance from source",
       x = paste0("CA1 (", coa_v1, "%)"), y = paste0("CA2 (", coa_v2, "%)")) +
  theme_bw(base_size = 10)

fig1
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

A correlation between the first ordination axis and distance from the source, together with a visible colour gradient along that axis in Figure 1, is evidence that a biodiversity pattern exists along the river. It is not yet evidence of what produces it. Site scores that trail continuously from source to confluence, rather than falling into a small number of discrete clusters, are consistent with the idea of a longitudinal river gradient rather than a small number of discrete habitat types — but a correlation coefficient and a coloured scatterplot cannot, on their own, distinguish a causal environmental effect from a spatial one.

</div>

<div id="step-2-what-ecological-hypotheses-could-explain-this-pattern" class="section level3" number="12.1.2">

### <span class="header-section-number">12.1.2</span> Step 2: What ecological hypotheses could explain this pattern?

Distance from the source is a convenient axis to plot, but it is also a summary of several processes that change together as a river flows downstream: channel width and depth, current velocity, substrate composition, temperature, nutrient concentration, and the degree of impoundment by dams. A change in assemblage composition with distance from the source is therefore consistent with several explanations that are not mutually exclusive:

- environmental filtering by measured variables such as altitude, damming or morphoregion;
- unmeasured environmental variables that also change longitudinally (for example fine-scale substrate or discharge);
- dispersal limitation among sites that are far apart along the river network;
- historical or biogeographic effects unrelated to current conditions.

The remaining steps use the additional information in `macroloire$env` and `macroloire$traits` to evaluate the first of these explanations directly, and to ask whether the trait composition of the assemblage — not just its taxonomic composition — responds to those environmental conditions.

</div>

<div id="step-3-which-environmental-variables-explain-community-structure-forward-selection" class="section level3" number="12.1.3">

### <span class="header-section-number">12.1.3</span> Step 3: Which environmental variables explain community structure? Forward selection

`macroloire$env` contains three numeric environmental variables (distance from source, altitude and an index of damming) and two categorical variables (morphoregion and confluence status). Following the same logic used earlier in the chapter for the Doubs and dune examples, these are standardised, combined with the categorical predictors, and offered to a redundancy analysis with forward selection so that only variables improving the model beyond what is expected by chance are retained.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
env_num <- macroloire$env[, c("Distance", "Altitude", "Dam")]
env_num_z <- decostand(env_num, method = "standardize")
env_fac <- macroloire$env[, c("Morphoregion", "Confluence")]
env_z <- data.frame(env_num_z, env_fac)
rownames(env_z) <- macroloire$env$SamplingSite

rda_full <- rda(fau_hel ~ ., data = env_z)
mod0 <- rda(fau_hel ~ 1, data = env_z)
sel <- ordiR2step(mod0, scope = formula(rda_full), direction = "forward")
```

</div>

<div class="cell-output cell-output-stdout">

    R> Step: R2.adj= 0 
    R> Call: fau_hel ~ 1 
    R>  
    R>                 R2.adjusted
    R> <All variables>   0.4016161
    R> + Morphoregion    0.2768819
    R> + Altitude        0.2424268
    R> + Dam             0.2373539
    R> + Distance        0.2135508
    R> + Confluence      0.1864794
    R> <model>           0.0000000
    R> 
    R>                Df     AIC      F Pr(>F)   
    R> + Morphoregion  2 -27.024 8.0836  0.002 **
    R> ---
    R> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    R> 
    R> Step: R2.adj= 0.2768819 
    R> Call: fau_hel ~ Morphoregion 
    R>  
    R>                 R2.adjusted
    R> <All variables>   0.4016161
    R> + Altitude        0.3768641
    R> + Dam             0.3280785
    R> + Distance        0.2914746
    R> <model>           0.2768819
    R> + Confluence      0.2342653
    R> 
    R>            Df    AIC      F Pr(>F)   
    R> + Altitude  1 -31.78 6.6158  0.002 **
    R> ---
    R> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    R> 
    R> Step: R2.adj= 0.3768641 
    R> Call: fau_hel ~ Morphoregion + Altitude 
    R>  
    R>                 R2.adjusted
    R> + Dam             0.4216194
    R> <All variables>   0.4016161
    R> + Distance        0.3895075
    R> <model>           0.3768641
    R> + Confluence      0.3530476

</div>

<div class="code-copy-outer-scaffold">

``` r
sel
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> Call: rda(formula = fau_hel ~ Morphoregion + Altitude, data = env_z)
    R> 
    R>               Inertia Proportion Rank
    R> Total          0.6296     1.0000     
    R> Constrained    0.2691     0.4274    3
    R> Unconstrained  0.3605     0.5726   29
    R> 
    R> Inertia is variance
    R> 
    R> Eigenvalues for constrained axes:
    R>    RDA1    RDA2    RDA3 
    R> 0.17526 0.05567 0.03817 
    R> 
    R> Eigenvalues for unconstrained axes:
    R>     PC1     PC2     PC3     PC4     PC5     PC6     PC7     PC8 
    R> 0.06898 0.05968 0.04979 0.03819 0.03330 0.02335 0.01832 0.01468 
    R> (Showing 8 of 29 unconstrained eigenvalues)

</div>

<div class="code-copy-outer-scaffold">

``` r
RsquareAdj(sel)
```

</div>

<div class="cell-output cell-output-stdout">

    R> $r.squared
    R> [1] 0.4273886
    R> 
    R> $adj.r.squared
    R> [1] 0.3768641

</div>

<div class="code-copy-outer-scaffold">

``` r
vif.cca(sel)
```

</div>

<div class="cell-output cell-output-stdout">

    R>  Morphoregiongranitic lowlands Morphoregionlimestone lowlands 
    R>                       2.008530                       2.573113 
    R>                       Altitude 
    R>                       2.520438

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
sel_v1 <- round(sel$CCA$eig[1] / sel$tot.chi * 100, 1)
sel_v2 <- round(sel$CCA$eig[2] / sel$tot.chi * 100, 1)

site_sc <- as.data.frame(scores(sel, display = "sites", choices = 1:2))
site_sc$Site <- macroloire$env$SamplingSite
site_sc$Morphoregion <- macroloire$env$Morphoregion

env_sc <- as.data.frame(scores(sel, display = "bp", choices = 1:2))
env_sc$Variable <- rownames(env_sc)

fig2 <- ggplot(site_sc, aes(RDA1, RDA2)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(colour = Morphoregion), size = 3, shape = 17) +
  geom_segment(data = env_sc, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(3, "mm"), type = "closed"),
               colour = "firebrick", linewidth = 0.6) +
  geom_text_repel(data = env_sc, aes(x = RDA1, y = RDA2, label = Variable),
                   colour = "black", size = 3, fontface = "bold") +
  scale_colour_brewer(palette = "Dark2", name = "Morphoregion") +
  coord_equal() +
  labs(title = "Forward-selected RDA of the Loire assemblage",
       x = paste0("RDA1 (", sel_v1, "%)"), y = paste0("RDA2 (", sel_v2, "%)")) +
  theme_bw(base_size = 10)

fig2
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

The variance inflation factors confirm whether the retained predictors are reasonably independent of one another, which matters for interpreting their individual contributions. A significant model with a sensible adjusted R tells us that the retained variables jointly explain a non-trivial share of variation in the assemblage — it does not, by itself, tell us whether that share is independent of the spatial arrangement of the sites, which is exactly the limitation flagged in Step 4 of the general framework and revisited below.

</div>

<div id="step-4-linking-assemblage-structure-to-traits-rlq-analysis" class="section level3" number="12.1.4">

### <span class="header-section-number">12.1.4</span> Step 4: Linking assemblage structure to traits — RLQ analysis

Distance from the source and the retained environmental predictors describe *where* the assemblage changes. They say nothing about *why*, in a functional sense — whether the taxa replacing one another downstream share particular traits, such as body size or feeding mode, that make them better suited to the conditions found there. This is exactly the three-table problem introduced earlier in the chapter, and it is addressed with RLQ analysis <span class="citation" cites="doledec1991 dray2008">(<a href="#ref-doledec1991" role="doc-biblioref">Dolédec and Chessel 1991</a>; <a href="#ref-dray2008" role="doc-biblioref">Dray and Legendre 2008</a>)</span>.

Row weights have to be threaded through the three separate ordinations by hand: the environmental analysis is weighted by the site weights from the correspondence analysis of the fauna table, and the trait analysis is weighted by the species weights from that same correspondence analysis.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
dudiL <- dudi.coa(fau, scannf = FALSE, nf = 2)

env_mix <- macroloire$env[, c("Distance", "Altitude", "Dam",
                                "Morphoregion", "Confluence")]
rownames(env_mix) <- macroloire$env$SamplingSite

dudiR <- dudi.hillsmith(env_mix, row.w = dudiL$lw, scannf = FALSE, nf = 2)

traits_fuzzy <- prep.fuzzy.var(macroloire$traits, col.blocks = c(4, 7),
                                row.w = dudiL$cw)
dudiQ <- dudi.fpca(traits_fuzzy, scannf = FALSE, nf = 2)

rlq_res <- rlq(dudiR, dudiL, dudiQ, scannf = FALSE, nf = 2)
summary(rlq_res)
```

</div>

<div class="cell-output cell-output-stdout">

    R> RLQ analysis
    R> 
    R> Class: rlq dudi
    R> Call: rlq(dudiR = dudiR, dudiL = dudiL, dudiQ = dudiQ, scannf = FALSE, 
    R>     nf = 2)
    R> 
    R> Total inertia: 0.07413
    R> 
    R> Eigenvalues:
    R>       Ax1       Ax2       Ax3       Ax4       Ax5 
    R> 7.363e-02 3.819e-04 1.037e-04 7.386e-06 2.647e-06 
    R> 
    R> Projected inertia (%):
    R>       Ax1       Ax2       Ax3       Ax4       Ax5 
    R> 99.331239  0.515155  0.139922  0.009963  0.003571 
    R> 
    R> Cumulative projected inertia (%):
    R>     Ax1   Ax1:2   Ax1:3   Ax1:4   Ax1:5 
    R>   99.33   99.85   99.99  100.00  100.00 
    R> 
    R> (Only 5 dimensions (out of 7) are shown)
    R> 
    R> 
    R> Eigenvalues decomposition:
    R>            eig      covar      sdR       sdQ      corr
    R> 1 0.0736335231 0.27135498 2.033869 0.2651448 0.5031897
    R> 2 0.0003818806 0.01954177 1.113781 0.1087682 0.1613102
    R> 
    R> Inertia & coinertia R (dudiR):
    R>     inertia      max     ratio
    R> 1  4.136624 4.379684 0.9445029
    R> 12 5.377133 6.138290 0.8759985
    R> 
    R> Inertia & coinertia Q (dudiQ):
    R>       inertia        max     ratio
    R> 1  0.07030175 0.07099767 0.9901980
    R> 12 0.08213228 0.08993587 0.9132316
    R> 
    R> Correlation L (dudiL):
    R>        corr       max     ratio
    R> 1 0.5031897 0.9699976 0.5187535
    R> 2 0.1613102 0.8729644 0.1847844

</div>

<div class="code-copy-outer-scaffold">

``` r
randtest(rlq_res, nrepet = 999)
```

</div>

<div class="cell-output cell-output-stdout">

    R> class: krandtest lightkrandtest 
    R> Monte-Carlo tests
    R> Call: randtest.rlq(xtest = rlq_res, nrepet = 999)
    R> 
    R> Number of tests:   2 
    R> 
    R> Adjustment method for multiple comparisons:   none 
    R> Permutation number:   999 
    R>      Test        Obs   Std.Obs   Alter Pvalue
    R> 1 Model 2 0.07412927 7.1165038 greater  0.001
    R> 2 Model 4 0.07412927 0.3365556 greater  0.278

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
rlq_v1 <- round(rlq_res$eig[1] / sum(rlq_res$eig) * 100, 1)
rlq_v2 <- round(rlq_res$eig[2] / sum(rlq_res$eig) * 100, 1)

env_arrows <- data.frame(rlq_res$l1)
env_arrows$Variable <- rownames(env_arrows)

trait_arrows <- data.frame(rlq_res$c1)
trait_arrows$Trait <- rownames(trait_arrows)

p_env <- ggplot(env_arrows, aes(RS1, RS2)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_segment(aes(x = 0, y = 0, xend = RS1, yend = RS2),
               arrow = arrow(length = unit(3, "mm"), type = "closed"), colour = "steelblue") +
  geom_label_repel(aes(label = Variable), colour = "steelblue", size = 3) +
  coord_equal() +
  labs(title = "RLQ \u2014 environmental gradient",
       x = paste0("Axis 1 (", rlq_v1, "%)"), y = paste0("Axis 2 (", rlq_v2, "%)")) +
  theme_bw(base_size = 10)

p_trait <- ggplot(trait_arrows, aes(CS1, CS2)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_segment(aes(x = 0, y = 0, xend = CS1, yend = CS2),
               arrow = arrow(length = unit(3, "mm"), type = "closed"), colour = "darkorange") +
  geom_label_repel(aes(label = Trait), colour = "darkorange", size = 3) +
  coord_equal() +
  labs(title = "RLQ \u2014 functional trait space",
       x = paste0("Axis 1 (", rlq_v1, "%)"), y = paste0("Axis 2 (", rlq_v2, "%)")) +
  theme_bw(base_size = 10)

p_env + p_trait
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

A significant permutation test for the RLQ tells us that the environment–trait association summarised by the two panels above is stronger than would be expected if traits were distributed at random with respect to the environmental gradient. Reading the two panels side by side — matching the direction of an environmental arrow on the left to a trait arrow pointing the same way on the right — generates specific, testable statements about which traits are favoured under which conditions. RLQ does not, however, tell us which individual trait–environment pairs drive that association; that is the role of the fourth-corner test.

</div>

<div id="step-5-testing-individual-traitenvironment-associations-the-fourth-corner-test" class="section level3" number="12.1.5">

### <span class="header-section-number">12.1.5</span> Step 5: Testing individual trait–environment associations — the fourth-corner test

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
four <- fourthcorner(env_mix, fau, macroloire$traits,
                      modeltype = 6, nrepet = 999,
                      p.adjust.method.G = "fdr",
                      p.adjust.method.D = "fdr")
summary(four)
```

</div>

<div class="cell-output cell-output-stdout">

    R> Fourth-corner Statistics
    R> ------------------------
    R> Permutation method  Comb. 2 and 4  ( 999  permutations)
    R> 
    R> Adjustment method for multiple comparisons:   fdr 
    R>                    Test Stat           Obs     Std.Obs     Alter Pvalue
    R> 1      Distance / Size1    r  -0.327299491 -1.26807304 two-sided  0.232
    R> 2      Altitude / Size1    r   0.485174657  1.61633019 two-sided  0.119
    R> 3           Dam / Size1    r  -0.350000074 -1.08211705 two-sided  0.321
    R> 4  Morphoregion / Size1    F 252.061169796 -0.27976663   greater  0.573
    R> 5    Confluence / Size1    F 100.630704578 -0.23050409   greater  0.433
    R> 6      Distance / Size2    r  -0.184727904 -0.69009180 two-sided  0.530
    R> 7      Altitude / Size2    r   0.233628893  0.75987030 two-sided  0.468
    R> 8           Dam / Size2    r  -0.237771284 -0.71142437 two-sided  0.524
    R> 9  Morphoregion / Size2    F  86.359349469 -0.91628346   greater  0.786
    R> 10   Confluence / Size2    F  63.484050572 -0.54398980   greater  0.659
    R> 11     Distance / Size3    r   0.379436434  1.75625587 two-sided  0.076
    R> 12     Altitude / Size3    r  -0.533695049 -2.15673366 two-sided  0.022
    R> 13          Dam / Size3    r   0.434998332  1.66720865 two-sided  0.093
    R> 14 Morphoregion / Size3    F 372.497652733  0.30279061   greater  0.270
    R> 15   Confluence / Size3    F 184.516191321  0.64210978   greater  0.193
    R> 16     Distance / Size4    r  -0.073283372 -0.30277837 two-sided  0.634
    R> 17     Altitude / Size4    r   0.124581776  0.46210934 two-sided  0.525
    R> 18          Dam / Size4    r  -0.076537306 -0.25588442 two-sided  0.710
    R> 19 Morphoregion / Size4    F   9.681468466 -0.37005289   greater  0.563
    R> 20   Confluence / Size4    F   3.465513205 -0.48354333   greater  0.642
    R> 21     Distance / Feed1    r  -0.178307703 -0.70266573 two-sided  0.477
    R> 22     Altitude / Feed1    r   0.128757606  0.23167881 two-sided  0.767
    R> 23          Dam / Feed1    r  -0.149169193 -0.40969521 two-sided  0.632
    R> 24 Morphoregion / Feed1    F  35.467262841 -0.55667071   greater  0.601
    R> 25   Confluence / Feed1    F  52.083916622 -0.11371076   greater  0.365
    R> 26     Distance / Feed2    r  -0.122233738 -0.40827974 two-sided  0.720
    R> 27     Altitude / Feed2    r   0.269489834  0.87069173 two-sided  0.428
    R> 28          Dam / Feed2    r  -0.229208949 -0.68135542 two-sided  0.560
    R> 29 Morphoregion / Feed2    F  89.497822514 -0.89462683   greater  0.809
    R> 30   Confluence / Feed2    F  78.960752627 -0.35169456   greater  0.541
    R> 31     Distance / Feed3    r  -0.418796982 -1.78379326 two-sided  0.090
    R> 32     Altitude / Feed3    r   0.630225624  2.31934922 two-sided  0.006
    R> 33          Dam / Feed3    r  -0.448212361 -1.53925545 two-sided  0.135
    R> 34 Morphoregion / Feed3    F 433.504577026  0.46462338   greater  0.248
    R> 35   Confluence / Feed3    F 161.356238377  0.34725195   greater  0.267
    R> 36     Distance / Feed4    r  -0.002242400 -0.17738793 two-sided  0.822
    R> 37     Altitude / Feed4    r  -0.006755574 -0.40697299 two-sided  0.615
    R> 38          Dam / Feed4    r   0.013894051  0.44290083 two-sided  0.549
    R> 39 Morphoregion / Feed4    F   1.007964778 -0.43310072   greater  0.930
    R> 40   Confluence / Feed4    F   0.214085231 -0.55018830   greater  1.000
    R> 41     Distance / Feed5    r  -0.187140712 -0.85951566 two-sided  0.333
    R> 42     Altitude / Feed5    r   0.136754228  0.33775553 two-sided  0.664
    R> 43          Dam / Feed5    r  -0.254745491 -1.10727485 two-sided  0.233
    R> 44 Morphoregion / Feed5    F 111.937081806 -0.01083213   greater  0.240
    R> 45   Confluence / Feed5    F  39.577681495 -0.09359419   greater  0.288
    R> 46     Distance / Feed6    r   0.409155560  1.85314681 two-sided  0.049
    R> 47     Altitude / Feed6    r  -0.615592062 -2.44666466 two-sided  0.008
    R> 48          Dam / Feed6    r   0.490084755  1.85208478 two-sided  0.049
    R> 49 Morphoregion / Feed6    F 491.542590937  0.81034060   greater  0.164
    R> 50   Confluence / Feed6    F 198.237433875  0.82712167   greater  0.157
    R> 51     Distance / Feed7    r  -0.001857970 -0.08259863 two-sided  0.908
    R> 52     Altitude / Feed7    r   0.020180421 -0.27298957 two-sided  0.691
    R> 53          Dam / Feed7    r  -0.010152552  0.30185282 two-sided  0.685
    R> 54 Morphoregion / Feed7    F   1.293367480 -0.59251586   greater  0.977
    R> 55   Confluence / Feed7    F   3.136841778 -0.67914927   greater  0.863
    R>    Pvalue.adj  
    R> 1     0.78158  
    R> 2     0.72722  
    R> 3     0.82500  
    R> 4     0.82500  
    R> 5     0.82500  
    R> 6     0.82500  
    R> 7     0.82500  
    R> 8     0.82500  
    R> 9     0.86460  
    R> 10    0.82500  
    R> 11    0.63938  
    R> 12    0.40333  
    R> 13    0.63938  
    R> 14    0.78158  
    R> 15    0.78158  
    R> 16    0.82500  
    R> 17    0.82500  
    R> 18    0.82500  
    R> 19    0.82500  
    R> 20    0.82500  
    R> 21    0.82500  
    R> 22    0.86092  
    R> 23    0.82500  
    R> 24    0.82500  
    R> 25    0.82500  
    R> 26    0.82500  
    R> 27    0.82500  
    R> 28    0.82500  
    R> 29    0.87245  
    R> 30    0.82500  
    R> 31    0.63938  
    R> 32    0.22000  
    R> 33    0.74250  
    R> 34    0.78158  
    R> 35    0.78158  
    R> 36    0.85302  
    R> 37    0.82500  
    R> 38    0.82500  
    R> 39    0.96509  
    R> 40    1.00000  
    R> 41    0.82500  
    R> 42    0.82500  
    R> 43    0.78158  
    R> 44    0.78158  
    R> 45    0.79200  
    R> 46    0.53900  
    R> 47    0.22000  
    R> 48    0.53900  
    R> 49    0.75167  
    R> 50    0.75167  
    R> 51    0.91500  
    R> 52    0.82500  
    R> 53    0.82500  
    R> 54    0.99509  
    R> 55    0.91279  
    R> 
    R> ---
    R> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
fc_long <- data.frame(
  combo = four$tabD2$names,
  stat  = four$tabD2$obs,
  padj  = four$tabD2$adj.pvalue
) |>
  separate(combo, into = c("Env_variable", "Trait"), sep = " / ")

fc_long$stars <- dplyr::case_when(
  fc_long$padj < 0.001 ~ "***",
  fc_long$padj < 0.01  ~ "**",
  fc_long$padj < 0.05  ~ "*",
  TRUE ~ ""
)

fig4 <- ggplot(fc_long, aes(Env_variable, Trait, fill = stat)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = stars), size = 5, vjust = -0.3, fontface = "bold") +
  scale_fill_gradient2(low = "#00AFBB", mid = "white", high = "#FC4E07",
                        midpoint = 0, name = "Statistic") +
  labs(title = "Fourth-corner: trait\u2013environment associations",
       subtitle = "* p<0.05, ** p<0.01, *** p<0.001 (FDR-adjusted)", x = NULL, y = NULL) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig4
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

Because a fourth-corner test performs many simultaneous comparisons — one for every environmental variable–trait combination — the false discovery rate correction applied above is essential; without it, a meaningful proportion of the associations flagged as significant would be expected by chance alone <span class="citation" cites="dray2008">(<a href="#ref-dray2008" role="doc-biblioref">Dray and Legendre 2008</a>)</span>. Even a large number of significant cells is evidence of association, not of mechanism: a trait that is more common at high-altitude sites may be favoured directly by altitude, or by a variable that itself co-varies with altitude, such as current velocity or temperature.

</div>

<div id="step-6-how-much-of-the-variation-is-spatially-structured-and-what-remains-unexplained-beta-diversity-along-the-river" class="section level3" number="12.1.6">

### <span class="header-section-number">12.1.6</span> Step 6: How much of the variation is spatially structured, and what remains unexplained? Beta diversity along the river

The final piece of evidence considered here addresses a question the RDA in Step 3 could not answer on its own: how the total dissimilarity among sites is distributed along the river, and whether particular sites contribute disproportionately to it. `beta.multi.abund()` <span class="citation" cites="legendre2013">(<a href="#ref-legendre2013" role="doc-biblioref">Legendre and De Cáceres 2013</a>)</span> partitions total assemblage dissimilarity into a replacement (turnover) component and a richness-difference (nestedness) component, while `beta.div()` computes the local contribution to beta diversity (LCBD) of each site — a measure of how much each site’s composition deviates from the average of all sites.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
beta_split <- beta.multi.abund(fau)
beta_split
```

</div>

<div class="cell-output cell-output-stdout">

    R> $beta.BRAY.BAL
    R> [1] 0.9013278
    R> 
    R> $beta.BRAY.GRA
    R> [1] 0.05132618
    R> 
    R> $beta.BRAY
    R> [1] 0.952654

</div>

<div class="code-copy-outer-scaffold">

``` r
lcbd <- beta.div(fau_hel, method = "hellinger", nperm = 999)
lcbd$p.LCBD
```

</div>

<div class="cell-output cell-output-stdout">

    R>    S1    S2    S3    S4    S5    S6    S7    S8    S9   S10   S11   S12   S13 
    R> 0.001 0.091 0.002 0.014 0.012 0.022 0.063 0.019 0.676 0.450 0.490 0.007 0.472 
    R>   S14   S15   S16   S17   S18   S19   S20   S21   S22   S23   S24   S25   S26 
    R> 0.970 0.001 0.578 0.556 0.870 0.932 0.993 0.962 0.967 0.987 0.999 0.795 0.984 
    R>   S27   S28   S29   S30   S31   S32   S33   S34   S35   S36   S37   S38 
    R> 0.995 0.884 0.995 0.954 1.000 0.238 0.998 0.393 0.867 0.755 0.753 0.777

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
lcbd_df <- data.frame(
  Site = macroloire$env$SamplingSite,
  Distance = macroloire$env$Distance,
  LCBD = lcbd$LCBD,
  p = lcbd$p.LCBD
)
lcbd_df$Significant <- ifelse(lcbd_df$p <= 0.05, "p \u2264 0.05", "n.s.")

fig5 <- ggplot(lcbd_df, aes(Distance, LCBD)) +
  geom_hline(yintercept = mean(lcbd_df$LCBD), linetype = "dashed", colour = "grey50") +
  geom_point(aes(colour = Significant, size = Significant)) +
  geom_text_repel(data = subset(lcbd_df, p <= 0.05), aes(label = Site), size = 3) +
  scale_colour_manual(values = c("p \u2264 0.05" = "firebrick", "n.s." = "grey60")) +
  scale_size_manual(values = c("p \u2264 0.05" = 3, "n.s." = 1.8)) +
  labs(title = "Local contributions to beta diversity along the Loire",
       subtitle = "Labelled sites are significant at p \u2264 0.05 (999 permutations)",
       x = "Distance from source (km)", y = "LCBD",
       colour = "Significance", size = "Significance") +
  theme_bw(base_size = 10)

fig5
```

</div>

<div class="cell-output-display">

<div>

<figure class="figure">
</figure>

</div>

</div>

</div>

If turnover, rather than nestedness, dominates the partition from `beta.multi.abund()`, that supports the earlier ordination evidence: sites differ mainly because they hold different taxa, not because downstream sites are simply impoverished subsets of upstream ones. Sites with a significant LCBD are those contributing disproportionately to overall compositional variation; whether these sites sit at particular positions along the river, near a dam, or at a confluence is itself a further, testable hypothesis rather than a conclusion.

</div>

<div id="bringing-the-evidence-together" class="section level3" number="12.1.7">

### <span class="header-section-number">12.1.7</span> Bringing the evidence together

None of the five analyses above is individually conclusive, which is the point of the framework introduced earlier in the chapter. Applied to this dataset, the same six questions can now be answered as follows:

1.  **Is there a biodiversity pattern?** Yes — the correspondence analysis and its correlation with distance from the source in Step 1 show that assemblage composition changes along the river.
2.  **What could explain it?** Several non-exclusive hypotheses were proposed in Step 2, including environmental filtering, unmeasured longitudinal variables, and dispersal-related spatial structure.
3.  **Which environmental variables explain community structure?** Forward selection in Step 3 identifies which of the measured variables (distance, altitude, damming, morphoregion, confluence) contribute significantly, and how much variation they jointly explain.
4.  **Are environmental and spatial effects independent?** `macroloire` does not include a ready-made set of spatial eigenvectors, so this worked example cannot formally separate a pure-space fraction from a pure-environment fraction the way the `varpart()` example earlier in the chapter does with Moran’s Eigenvector Maps. Because distance from the source is both an environmental and a spatial descriptor, this is a genuine limitation of the present dataset rather than of the method, and it is revisited in the section below.
5.  **How much uncertainty remains?** The RLQ and fourth-corner results in Steps 4 and 5 extend the explanation from taxonomic turnover to functional-trait filtering, but every association identified there is still correlational, and the permutation tests only establish that an association is unlikely to have arisen by chance — not that it is causal.
6.  **What ecological conclusion is supported?** Taken together, a longitudinal turnover in taxonomic composition (Step 1), partly attributable to measured environmental gradients (Step 3), accompanied by a coherent shift in functional traits along the same gradient (Steps 4–5), and concentrated at specific, identifiable sites (Step 6), is stronger collective evidence for environmental filtering along the river than any single analysis would be on its own.

</div>

</div>

</div>

<div id="limitations-and-future-recommendations" class="section level1" number="13">

# <span class="header-section-number">13</span> Limitations and future recommendations

The framework and worked example above make ecological inference more systematic, but they do not make it certain, and a few limitations are worth stating plainly.

First, every method covered in this chapter is correlational. Ordination, constrained ordination, RLQ, the fourth-corner test and variation partitioning can all show that a pattern is unlikely to have arisen by chance, but none of them can establish that an environmental variable *causes* a change in community composition. Experimental manipulation or long-term monitoring is needed to move from association to mechanism <span class="citation" cites="dietze2017">(<a href="#ref-dietze2017" role="doc-biblioref">Dietze 2017</a>)</span>.

Second, as the Loire case study shows, a formal separation of environmental and spatial effects depends on having an explicit spatial model, such as a set of Moran’s Eigenvector Maps, alongside the environmental data. When a dataset provides only a longitudinal proxy such as distance from source, environmental and spatial explanations remain partially confounded, and this should be reported as a limitation rather than resolved by assumption <span class="citation" cites="legendre2012 dray2012">(<a href="#ref-legendre2012" role="doc-biblioref">Legendre et al. 2012</a>; <a href="#ref-dray2012" role="doc-biblioref">Dray et al. 2012</a>)</span>.

Third, all the methods used here assume that the environmental and trait variables measured are the ones that matter. Unmeasured variables, sampling effort, and the spatial and temporal resolution of the survey design all constrain what can be inferred, regardless of how many complementary analyses are combined <span class="citation" cites="cressie2009 simmonds2024">(<a href="#ref-cressie2009" role="doc-biblioref">Cressie et al. 2009</a>; <a href="#ref-simmonds2024" role="doc-biblioref">Simmonds et al. 2024</a>)</span>.


</div>

<div id="task" class="section level1" number="14">

# <span class="header-section-number">14</span> Task

**Part A**

1.  Define each of the following terms in your own words. Your answer should explain both the statistical meaning and ecological relevance.

<!-- -->

1.  Ecological gradient (2)

2.  Community matrix (2)

3.  Ordination (2)

4.  Constrained ordination (2)

5.  Variation partitioning (2)

<!-- -->

2.  Explain how each of the following should be interpreted.

<!-- -->

1.  Two sites occur close together on an NMDS plot. (2)

2.  Species arrows point in the same direction on a PCA biplot. (2)

3.  An NMDS stress value of 0.06. (2)

4.  An NMDS stress value of 0.28. (2)

5.  The first PCA axis explains 58% of the variation. (2)

**Part B**

Using the provided dune dataset, answer the following question.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(vegan)

data(dune)

data(dune.env)
```

</div>

</div>

Complete the following analyses.

- NMDS
- envfit()
- dbRDA
- variation partitioning

Write approximately 500 words explaining how these methods work together to move from biodiversity patterns to ecological inference.(20)

Your answer should include:

1.  what each method contributes
2.  why no single analysis is sufficient
3.  how the analyses complement one another
4.  implications for conservation decision-making %. Using the limitations discussed in this chapter, identify one specific way in which your conclusionss

</div>

<div id="synthesis" class="section level1" number="15">

# <span class="header-section-number">15</span> Synthesis




The broader message for BCB743 is therefore not about learning more statistical techniques. The methods introduced throughout the module are already powerful tools. What matters is understanding the role each one plays in the process of ecological inference, recognising the questions it can answer, and being aware of the questions it cannot. Reliable ecological conclusions are rarely built on the output of a single analysis. Instead, they emerge by combining evidence from multiple complementary approaches and by interpreting that evidence within an ecological context. That progression—from recognising patterns to evaluating competing explanations—is the central idea that this chapter aims to reinforce

</div>

<div id="ai-declaration" class="section level1" number="16">

# <span class="header-section-number">16</span> AI Declaration

Generative artificial intelligence (ChatGPT-5.5 and Claude Sonnet 5) was used as a research support and educational assistance tool during the development of this chapter. AI assisted with brainstorming chapter structure, exploring and identifying potential gaps within the existing BCB743 curriculum, reviewing the validity od teaching activities, generating and refining R code for worked examples and figures, producing illustrative diagrams, improving the clarity and flow of written explanations, and checking grammar, spelling, and consistency of formatting.

All AI-generated text, code, figures, and suggestions were critically reviewed, extensively modified where necessary, tested for accuracy and reproducibility, and verified against the primary literature, the existing BCB743 course material, and the outputs produced from the accompanying R analyses before being incorporated into the final chapter. AI was used to support the writing and development process but did not replace critical thinking, scientific judgement, or scholarly responsibility.

</div>

<div id="author-contribution" class="section level1" number="17">

# <span class="header-section-number">17</span> Author contribution

The chapter was developed collaboratively by all three authors. Individual responsibilities were divided as follows:

- Anisah led the overall organisation and structure of the chapter, including planning the chapter flow, integrating content into a coherent teaching resource, and ensuring consistency with the style and pedagogical approach of the BCB743 Quantitative Ecology module.
- Naledi developed the accompanying ecoevidence R package, contributed to the design and implementation of the analytical workflow, and undertook substantial editing and revision of the chapter to ensure technical accuracy and reproducibility.Additionally led progress on the package review assignment
- Emihle developed the worked examples, figures, practical exercises, and teaching demonstrations, and contributed to the editing and refinement of the chapter. Additionally led the progress of the integrative dataset assignment.

</div>

<div id="references" class="section level1" number="18">

# <span class="header-section-number">18</span> References

<div id="refs" class="references csl-bib-body hanging-indent" role="list">

<div id="ref-borcard2011" class="csl-entry" role="listitem">

Borcard, Daniel, François Gillet, and Pierre Legendre. 2011. *Numerical Ecology with R*. Springer New York. <https://doi.org/10.1007/978-1-4419-7976-6>.

</div>

<div id="ref-browne2005" class="csl-entry" role="listitem">

Browne, William J., S. V. Subramanian, Kelvyn Jones, and Harvey Goldstein. 2005. “Variance Partitioning in Multilevel Logistic Models That Exhibit Overdispersion.” *Journal of the Royal Statistical Society Series A: Statistics in Society* 168 (3): 599–613. <https://doi.org/10.1111/j.1467-985X.2004.00365.x>.

</div>

<div id="ref-capblancq2021" class="csl-entry" role="listitem">

Capblancq, Thibaut, and Brenna R. Forester. 2021. “Redundancy Analysis: A Swiss Army Knife for Landscape Genomics.” *Methods in Ecology and Evolution* 12 (12): 2298–309. <https://doi.org/10.1111/2041-210X.13722>.

</div>

<div id="ref-cressie2009" class="csl-entry" role="listitem">

Cressie, Noel, Catherine A. Calder, James S. Clark, Jay M. Ver Hoef, and Christopher K. Wikle. 2009. “Accounting for Uncertainty in Ecological Analysis: The Strengths and Limitations of Hierarchical Statistical Modeling.” *Ecological Applications* 19 (3): 553–70. <https://doi.org/10.1890/07-0744.1>.

</div>

<div id="ref-dietze2017" class="csl-entry" role="listitem">

Dietze, Michael C. 2017. “Prediction in Ecology: A First-Principles Framework.” *Ecological Applications* 27 (7): 2048–60. <https://doi.org/10.1002/eap.1589>.

</div>

<div id="ref-doledec1991" class="csl-entry" role="listitem">

Dolédec, Sylvain, and Daniel Chessel. 1991. “Recent Developments in Linear Ordination Methods for Environmental Sciences.” *Advances in Ecology* 1: 133–55.

</div>

<div id="ref-dray2008" class="csl-entry" role="listitem">

Dray, Stéphane, and Pierre Legendre. 2008. “Testing the Species Traits–Environment Relationships: The Fourth-Corner Problem Revisited.” *Ecology* 89 (12): 3400–3412. <https://doi.org/10.1890/08-0349.1>.

</div>

<div id="ref-dray2012" class="csl-entry" role="listitem">

Dray, Stéphane, Raphaël Pélissier, Pierre Couteron, et al. 2012. “Community Ecology in the Age of Multivariate Multiscale Spatial Analysis.” *Ecological Monographs* 82 (3): 257–75. <https://doi.org/10.1890/11-1183.1>.

</div>

<div id="ref-hartig2024" class="csl-entry" role="listitem">

Hartig, Florian, Nerea Abrego, Alex Bush, et al. 2024. “Novel Community Data in Ecology—Properties and Prospects.” *Trends in Ecology & Evolution* 39 (3): 280–93. <https://doi.org/10.1016/j.tree.2023.09.017>.

</div>

<div id="ref-he2024" class="csl-entry" role="listitem">

He, Shan, Beixin Wang, Kai Chen, Nan Li, and Janne Soininen. 2024. “Species–Environment Sorting Explains Latitudinal Patterns in Spatiotemporal <span class="nocase">ß</span>-Diversity for Freshwater Macroinvertebrates.” *Ecography* 2024 (9): e07111. <https://doi.org/10.1111/ecog.07111>.

</div>

<div id="ref-jongman1995" class="csl-entry" role="listitem">

Jongman, Rob H. G., Cajo J. F. Ter Braak, and Olff F. R. Van Tongeren. 1995. *Data Analysis in Community and Landscape Ecology*. 1st ed. Cambridge University Press. <https://doi.org/10.1017/CBO9780511525575>.

</div>

<div id="ref-kremen1992" class="csl-entry" role="listitem">

Kremen, Claire. 1992. “Assessing the Indicator Properties of Species Assemblages for Natural Areas Monitoring.” *Ecological Applications* 2 (2): 203–17. <https://doi.org/10.2307/1941776>.

</div>

<div id="ref-legendre2012" class="csl-entry" role="listitem">

Legendre, Pierre, Daniel Borcard, and David W. Roberts. 2012. “Variation Partitioning Involving Orthogonal Spatial Eigenfunction Submodels.” *Ecology* 93 (5): 1234–40. <https://doi.org/10.1890/11-2028.1>.

</div>

<div id="ref-legendre2013" class="csl-entry" role="listitem">

Legendre, Pierre, and Miquel De Cáceres. 2013. “Beta Diversity as the Variance of Community Data: Dissimilarity Coefficients and Partitioning.” *Ecology Letters* 16 (8): 951–63. <https://doi.org/10.1111/ele.12141>.

</div>

<div id="ref-magurran2010" class="csl-entry" role="listitem">

Magurran, Anne E., Stephen R. Baillie, Stephen T. Buckland, et al. 2010. “Long-Term Datasets in Biodiversity Research and Monitoring: Assessing Change in Ecological Communities Through Time.” *Trends in Ecology & Evolution* 25 (10): 574–82. <https://doi.org/10.1016/j.tree.2010.06.016>.

</div>

<div id="ref-mcdonnell1993" class="csl-entry" role="listitem">

McDonnell, Mark J., Steward T. A. Pickett, and Richard V. Pouyat. 1993. “The Application of the Ecological Gradient Paradigm to the Study of Urban Effects.” In *Humans as Components of Ecosystems*, edited by Mark J. McDonnell and Steward T. A. Pickett. Springer New York. <https://doi.org/10.1007/978-1-4612-0905-8_15>.

</div>

<div id="ref-muller1998" class="csl-entry" role="listitem">

Müller, Felix. 1998. “Gradients in Ecological Systems.” *Ecological Modelling* 108 (1-3): 3–21. <https://doi.org/10.1016/S0304-3800(98)00015-5>.

</div>

<div id="ref-oksanen2001" class="csl-entry" role="listitem">

Oksanen, Jari, Gavin L. Simpson, F. Guillaume Blanchet, et al. 2001. *Vegan: Community Ecology Package*. <https://doi.org/10.32614/CRAN.package.vegan>.

</div>

<div id="ref-palmer2007" class="csl-entry" role="listitem">

Palmer, Michael W. 2007. “Species–Area Curves and the Geometry of Nature.” In *Scaling Biodiversity*, 1st ed., edited by David Storch, Pablo Marquet, and James Brown. Cambridge University Press. <https://doi.org/10.1017/CBO9780511814938.004>.

</div>

<div id="ref-pavoine2011" class="csl-entry" role="listitem">

Pavoine, Sandrine, and Michael B. Bonsall. 2011. “Measuring Biodiversity to Explain Community Assembly: A Unified Approach.” *Biological Reviews* 86 (4): 792–812. <https://doi.org/10.1111/j.1469-185X.2010.00171.x>.

</div>

<div id="ref-qian" class="csl-entry" role="listitem">

Qian, Song S. n.d. *Environmental and Ecological Statistics with R*.

</div>

<div id="ref-riesch2018" class="csl-entry" role="listitem">

Riesch, Rudiger, Martin Plath, and David Bierbach. 2018. “Ecology and Evolution Along Environmental Gradients.” *Current Zoology* 64 (2): 193–96. <https://doi.org/10.1093/cz/zoy015>.

</div>

<div id="ref-scheiner2005" class="csl-entry" role="listitem">

Scheiner, Samuel M., and Michael R. Willig. 2005. “Developing Unified Theories in Ecology as Exemplified with Diversity Gradients.” *The American Naturalist* 166 (4): 458–69. <https://doi.org/10.1086/444402>.

</div>

<div id="ref-schmera2023" class="csl-entry" role="listitem">

Schmera, Dénes, Carlo Ricotta, and János Podani. 2023. “Components of Functional Diversity Revisited: A New Classification and Its Theoretical and Practical Implications.” *Ecology and Evolution* 13 (10): e10614. <https://doi.org/10.1002/ece3.10614>.

</div>

<div id="ref-schulz2025" class="csl-entry" role="listitem">

Schulz, Tal, Marjo Saastamoinen, and Jarno Vanhatalo. 2025. “Model-Based Variance Partitioning for Statistical Ecology.” *Ecological Monographs* 95 (1): e1646. <https://doi.org/10.1002/ecm.1646>.

</div>

<div id="ref-selgrath2026" class="csl-entry" role="listitem">

Selgrath, Jennifer C., Sarah E. Gergel, and Amanda C. J. Vincent. 2026. “The Influence of Multiple Stressors on the Spatial Distribution of Corals.” *People and Nature* 8 (2): 342–58. <https://doi.org/10.1002/pan3.70208>.

</div>

<div id="ref-simmonds2024" class="csl-entry" role="listitem">

Simmonds, Emily G., Kwaku Peprah Adjei, Benjamin Cretois, et al. 2024. “Recommendations for Quantitative Uncertainty Consideration in Ecology and Evolution.” *Trends in Ecology & Evolution* 39 (4): 328–37. <https://doi.org/10.1016/j.tree.2023.10.012>.

</div>

<div id="ref-terbraak1994" class="csl-entry" role="listitem">

Ter Braak, Cajo J. F. 1994. “Canonical Community Ordination. Part i: Basic Theory and Linear Methods.” *Écoscience* 1 (2): 127–40. <https://doi.org/10.1080/11956860.1994.11682237>.

</div>

<div id="ref-udy2021" class="csl-entry" role="listitem">

Udy, Kris, Melanie Fritsch, Katrin M. Meyer, et al. 2021. “Environmental Heterogeneity Predicts Global Species Richness Patterns Better Than Area.” *Global Ecology and Biogeography* 30 (4): 842–51. <https://doi.org/10.1111/geb.13261>.

</div>

</div>

</div>

</div>
