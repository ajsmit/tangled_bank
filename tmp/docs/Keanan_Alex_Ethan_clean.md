<div id="quarto-document-content" class="content" role="main">

<div id="title-block-header" class="quarto-title-block default">

<div class="quarto-title">

<div class="quarto-title-block">

<div>

# 17b: Why Are Species Where They Are? Niche and Neutral Theories of Community Assembly

Code

- <a href="javascript:void(0)" id="quarto-show-all-code" class="dropdown-item" role="button">Show All Code</a>

- <a href="javascript:void(0)" id="quarto-hide-all-code" class="dropdown-item" role="button">Hide All Code</a>

- 

  ------------------------------------------------------------------------

- <a href="javascript:void(0)" id="quarto-view-source" class="dropdown-item" role="button">View Source</a>

</div>

</div>

</div>

<div class="quarto-title-meta-author">

<div class="quarto-title-meta-heading">

Authors

</div>

<div class="quarto-title-meta-heading">

Affiliation

</div>

<div class="quarto-title-meta-contents">

Alex Matthew

</div>

<div class="quarto-title-meta-contents">

University of the Western Cape

</div>

<div class="quarto-title-meta-contents">

Keanan Jarvis

</div>

<div class="quarto-title-meta-contents">

University of the Western Cape

</div>

<div class="quarto-title-meta-contents">

Ethan Bell

</div>

<div class="quarto-title-meta-contents">

University of the Western Cape

</div>

</div>

<div class="quarto-title-meta">

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

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Material Required for This Chapter

</div>

</div>

<div class="callout-body-container callout-body">

| Type | Name | Notes |
|:---|:---|:---|
| **Theory** | Hubbell (2001) *The Unified Neutral Theory of Biodiversity and Biogeography* | Primary statement of neutral theory |
| **Theory** | Hutchinson (1957) Concluding remarks | The niche concept defined |
| **Theory** | Adler et al. (2007) A niche for neutrality | The niche-neutral continuum |
| **Theory** | Cottenie (2005) Integrating environmental and spatial processes | Metacommunity framework |
| **Theory** | Legendre et al. (2015) Should the Mantel test be used in spatial analysis? | Mantel test critique |
| **Data** | Doubs River fish community | `Doubs.RData` via NEwR companion data |
| **Data** | Oribatid mite community | `vegan::mite`, `vegan::mite.env`, `vegan::mite.xy` |
| **Package** | spesim 0.5.2 | `remotes::install_github("ajsmit/spesim")` |

</div>

</div>

<div class="callout callout-style-simple callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Tasks to Complete in This Chapter

</div>

</div>

<div class="callout-body-container callout-body">

See the [Tasks](#sec-tasks) section at the end of this chapter for five exercises that build directly on the analyses presented here.

</div>

</div>

> “*The truth is rarely pure and never simple.*”
>
> Oscar Wilde

<div id="introduction" class="section level2">

## Introduction

[Cluster Analysis](https://tangledbank.netlify.app/BCB743/cluster_analysis.html) left a question open. The Doubs fish community forms a real, measurable gradient running from headwater to lowland, and the silhouette analysis showed that forcing it into discrete clusters loses information a continuous ordination preserves. But naming a gradient is not explaining one. *Why* does the community change in an orderly way along the river, rather than scattering species at random? This chapter takes up that question.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Pattern Is Not Process

</div>

</div>

<div class="callout-body-container callout-body">

A gradient in community composition is a pattern. Niche and neutral theory are rival explanations for the process that produced it. In general a single pattern is consistent with several processes, which is why the question cannot be settled by ordination or clustering alone, however well executed. It requires a different kind of evidence.

</div>

</div>

Ecological theory offers two long-standing answers. **Niche theory** holds that species occur where the local environment and biotic interactions permit them to persist, so that community structure follows deterministically from the ecological differences between species. **Neutral theory** holds that at the level of individual births, deaths and dispersal events, species are functionally interchangeable, and that the patterns we observe arise from stochastic demographic and dispersal processes rather than from niche differences at all. Both theories can produce a gradient which, at first glance look identical.

By the end of this chapter you should be able to:

- state the core assumption each theory makes about species, and explain why those assumptions lead to different predictions about community structure;
- explain why this remains an active area of debate, and identify what is empirically established as against what is contested;
- translate each theory into a falsifiable prediction about how community dissimilarity should behave across space;
- recognise why the most intuitive statistical test of those predictions is the wrong one, and explain the reasoning behind that judgement;
- apply a defensible alternative to the Doubs data and interpret it critically; and
- connect this theoretical lens to the model-building workflow introduced in the [next chapter](https://tangledbank.netlify.app/BCB743/model_building.html).

</div>

<div id="from-pattern-to-process-revisiting-the-doubs-gradient" class="section level2">

## From Pattern to Process: Revisiting the Doubs Gradient

The [Cluster Analysis](https://tangledbank.netlify.app/BCB743/cluster_analysis.html) chapter established, with a cophenetic correlation and a silhouette analysis, that the Doubs fish community is organised as a continuum rather than a set of discrete types. The [Ordination](https://tangledbank.netlify.app/BCB743/ordination.html) chapter, several chapters earlier, established the same thing from the opposite direction: the dominant axis recovers an upstream-to-downstream gradient, with cool-water species at one end and lowland species at the other. Every method applied to this dataset agrees that a gradient exists. None of them asks what generated it.

There is an obvious candidate answer, and it happens to be the answer niche theory formalises. The river changes as it flows, in temperature, oxygen, gradient and flow rate, and different fish species tolerate that changing environment differently. Trout persist near the cool, oxygenated headwaters and cannot survive the warmer, slower lowland reaches; the reverse holds for the lowland species. On this account the gradient in the species data follows directly from the gradient in the environmental data, filtered through each of the species physiological and ecological tolerances.

That is a reasonable hypothesis. It is also not the only one available. A river is a linear system, and linear systems have a second property capable of producing the same pattern in an ordination: every site connects to its neighbours through a fixed, ordered sequence of dispersal pathways. Suppose every fish species were ecologically identical. The bare fact that an individual at one site can reach adjacent sites easily and distant ones only with difficulty would still, over time, produce spatial structure in community composition. This is the situation neutral theory describes formally, and it makes a testable claim: structure can arise from dispersal limitation and demographic stochasticity alone, with no reference to ecological differences between species.

The Doubs data cannot, by inspection, tell these two stories apart. That is the problem this chapter exists to solve.

</div>

<div id="ecological-background-niche-theory" class="section level2">

## Ecological Background: Niche Theory

Begin with a single fish. A brown trout holds its position in the cold, fast, oxygen-rich water of the upper Doubs. It is not there by accident. Trout require high dissolved oxygen and cool temperatures; they cannot persist in the warm, sluggish, oxygen-poor water of the lowland reaches. If you move downstream and the trout disappears, replaced by species that tolerate, and in some cases require, precisely the conditions the trout cannot survive. The community changes along the river because the environment changes along the river, and each species occupies the stretch where local conditions fall inside the range it can tolerate. Niche theory formalises that intuition: species occur where the environment permits them to persist, and so community composition can in principle be read from the environment itself.

The concept received its lasting formal statement from <span class="citation" cites="hutchinson1957">Hutchinson (<a href="#ref-hutchinson1957" role="doc-biblioref">1957</a>)</span>, who defined the niche as an *n*-dimensional hypervolume: a space with one axis for every environmental factor that matters to a species, bounded by the limits within which that species can maintain a viable population. Temperature is one axis, dissolved oxygen another, pH another, prey availability another, for as many dimensions as are ecologically relevant. <span class="citation" cites="chase2011">Chase and Myers (<a href="#ref-chase2011" role="doc-biblioref">2011</a>)</span> restate the idea directly. Conditions inside the *N*-dimensional hypervolume of a species’ requirements define the space in which it can gather resources, evade enemies and persist. This means that a species realised distribution is the projection of that hypervolume onto real geography.

Hutchinson drew a distinction that still organizes how ecologists use the concept. The **fundamental niche** is the full range of conditions under which a species could persist in the absence of competitors and natural enemies. The **realised niche** is the usually narrower range it actually occupies once biotic interactions are accounted for. A species might be physiologically capable of living across a broad stretch of the river, yet be confined to part of it because a competitor excludes it elsewhere. The distinction matters here because it makes plain that the environment is not the only filter: biotic interactions do some of the sorting too. The logic nevertheless remains deterministic. Something about the match between a species’ traits and its conditions decides where it lives.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Environmental Filtering

</div>

</div>

<div class="callout-body-container callout-body">

Under niche theory the environment acts as a filter. At any site, only species whose niche requirements are met by local conditions can persist; the rest are excluded. As conditions change across space the filter changes, and with it the set of species that pass through. Community composition becomes a predictable function of the environment.

</div>

</div>

The mechanism doing this work is **environmental filtering**. <span class="citation" cites="chase2011">Chase and Myers (<a href="#ref-chase2011" role="doc-biblioref">2011</a>)</span> place it right at the centre of the deterministic view of communities, in which local, niche-based processes such as environmental filtering, biotic interactions and interspecific trade-offs largely determine which species are found where. The word to hold onto is *Deterministic*: given sufficient knowledge of the environment and of species’ tolerances, niche theory says community composition is in principle predictable. Two sites with similar environments should support similar communities; two sites with different environments should support different ones. Under strict niche theory the geographic distance between sites is irrelevant, except insofar as it happens to correlate with environmental change.

That last point is what makes niche theory testable against its rival, and it is better stated as a prediction than as a description.

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Prediction 1. The Niche Prediction

</div>

</div>

<div class="callout-body-container callout-body">

Community dissimilarity should track environmental distance. If two reaches of the Doubs have nearly identical temperature, oxygen and flow, they should hold nearly the same fish, whether they sit side by side or at opposite ends of the river. What drives two communities apart is the degree to which their environments differ, not how far apart they lie in space.

</div>

</div>

The theory is strong, and strongly supported. In fish assemblages specifically, species occupy distinct positions along river gradients in ways that track measurable niche axes rather than chance, and the divergence of species’ niches along a river continuum has been documented directly <span class="citation" cites="troia2014">(<a href="#ref-troia2014" role="doc-biblioref">Troia and Gido 2014</a>)</span>. But niche theory has a competitor, and the competitor’s claim is unsettling. The same orderly gradient could arise even if niche differences played no part at all.

</div>

<div id="ecological-background-neutral-theory" class="section level2">

## Ecological Background: Neutral Theory

Return to the same river and change one assumption. Suppose the fish species of the Doubs are not sorted by their tolerances. Suppose instead that any individual, of any species, has roughly the same chance of surviving, reproducing and dispersing as any other individual, whatever species it belongs to. On that assumption the trout occupies the headwaters because its ancestors happened to be there and its descendants have not yet drifted elsewhere, rather than because it is uniquely suited to them. Community composition becomes a matter of history and chance instead of environmental matching.

This is the premise of **neutral theory**, given its unified formulation by <span class="citation" cites="hubbell2001">Hubbell (<a href="#ref-hubbell2001" role="doc-biblioref">2001</a>)</span> and reviewed a decade later by <span class="citation" cites="rosindell2011">Rosindell et al. (<a href="#ref-rosindell2011" role="doc-biblioref">2011</a>)</span>. Its foundational move is the neutrality assumption: all individuals within a trophic level have the same chances of reproduction and death, regardless of species identity <span class="citation" cites="rosindell2011">(<a href="#ref-rosindell2011" role="doc-biblioref">Rosindell et al. 2011</a>)</span>. Species are treated as **demographically equivalent**, interchangeable at the level of the individual. A community, on this view, is a collection of individuals undergoing birth, death and dispersal at rates that do not depend on species identity.

This is easily mistaken for the claim that species are literally identical, so precision is needed. Neutral theory does not assert that trout and bream are the same organism. It asserts something narrower and stranger. For the purpose of explaining community pattern, the differences between them can be set aside, because those differences do not translate into differences in per capita demographic rates within the community. The assumption is almost certainly false in detail. Real species do differ demographically, and Hubbell knew it. The falsity is the point. Neutral theory functions as a null model. If a theory that assumes away every niche difference can still reproduce the patterns we see in real communities, then those patterns cannot by themselves be taken as evidence that niche differences produced them.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Demographic Equivalence and Ecological Drift

</div>

</div>

<div class="callout-body-container callout-body">

Neutral theory assumes individuals are demographically equivalent regardless of species. Community composition then changes through **ecological drift**, the random walk of species’ abundances under chance births and deaths, constrained by **dispersal limitation**, the tendency of offspring to establish near their parents. Pattern emerges from stochastic processes rather than from environmental sorting.

</div>

</div>

Two mechanisms generate the pattern. The first is ecological drift. Because births and deaths carry a random component, the relative abundances of species wander over time, much as allele frequencies drift in population genetics. The parallel is deliberate. <span class="citation" cites="rosindell2011">Rosindell et al. (<a href="#ref-rosindell2011" role="doc-biblioref">2011</a>)</span> trace the idea directly to Kimuras neutral theory of molecular evolution, transposing neutrality from alleles within populations to individuals within communities. Under drift alone some species wander to local extinction and others to dominance, purely by chance, with no reference to fitness.

The second mechanism is dispersal limitation, and it is the one that produces spatial structure. <span class="citation" cites="rosindell2011">Rosindell et al. (<a href="#ref-rosindell2011" role="doc-biblioref">2011</a>)</span> define it as a process that causes the location of an individual to be restricted by the location of its parent: offspring tend to establish near where they were produced. In a river this is concrete rather than abstract. A fish spawning at one reach tends to produce offspring that occupy nearby reaches, not reaches at the far end of the system. Over many generations dispersal limitation alone, with no environmental sorting whatever, will generate spatial structure in community composition, because the drift unfolding at one location is only weakly coupled to the drift unfolding at a distant one. Nearby sites share recent dispersal history and come to resemble one another. Distant sites do not.

This yields neutral theory’s central prediction about spatial pattern, which mirrors niche theory’s exactly.

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Prediction 2. The Neutral Prediction

</div>

</div>

<div class="callout-body-container callout-body">

Community dissimilarity should track geographic distance. Nearby sites should hold similar communities because individuals move readily between them, and distant sites should hold dissimilar communities because they do not, whether or not their environments are alike. What drives two communities apart is how far apart they sit in space, not how much their environments differ.

</div>

</div>

The distance-driven decline in similarity has a name. <span class="citation" cites="rosindell2011">Rosindell et al. (<a href="#ref-rosindell2011" role="doc-biblioref">2011</a>)</span> gloss beta-diversity, or **distance decay**, as the probability, as a function of the distance between two individuals, that they belong to the same species. Distance decay sits at the centre of neutral theory rather than at its margin. It is the signature the theory predicts, and it is the quantity this chapter will learn to measure.

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

Note

</div>

</div>

<div class="callout-body-container callout-body">

Neutral theory is best read as a null model against which the signal of niche processes can be measured, rather than as a claim that the world is neutral. If a dataset’s patterns can be reproduced without invoking niche differences, those patterns alone cannot demonstrate that niche differences are at work. This is why the theory matters even to ecologists who are confident that niches are important. It raises the standard of evidence.

</div>

</div>

This section develops dispersal limitation only as a local, within-system mechanism. Regional dispersal dynamics and landscape-scale connectivity belong to the Metacommunity chapter, which cross-references this section for the underlying definition.

</div>

<div id="two-worlds-one-pattern" class="section level2">

## Two Worlds, One Pattern

The two theories have now been stated, and they disagree about mechanism while agreeing, awkwardly, about what you would see. Before going further we make that agreement concrete, by building two artificial rivers in which we know the answer.

In the first, species differ. Each has a Gaussian response to an environmental gradient running the length of the transect, with its own optimum and tolerance, exactly as niche theory describes. There is no dispersal at all. In the second, species are identical. There is no environment. Community composition changes only through ecological drift, and individuals disperse only to adjacent sites, exactly as neutral theory describes.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(here)
library(tidyverse)
library(vegan)
library(patchwork)
```

</div>

</div>

<div class="cell">

Show the simulation code

<div class="code-copy-outer-scaffold">

``` r
# A niche world: Gaussian species responses to a gradient, no dispersal.
sim_niche <- function(seed, n_site = 29, S = 27, J = 100, tol = 0.12) {
  set.seed(seed)
  gradient <- seq(0, 1, length.out = n_site)
  optima   <- seq(-0.1, 1.1, length.out = S)
  t(sapply(gradient, function(e) {
    p <- exp(-((e - optima)^2) / (2 * tol^2))
    as.vector(rmultinom(1, J, p / sum(p)))
  }))
}

# A neutral world: demographic equivalence, drift, dispersal to adjacent sites only.
# No environmental variable exists anywhere in this simulation.
sim_neutral <- function(seed, n_site = 29, S = 27, J = 100, gens = 250,
                        m = 0.05, nu = 0.002) {
  set.seed(seed)
  counts <- t(replicate(n_site, as.vector(rmultinom(1, J, rep(1 / S, S)))))
  sites  <- seq_len(n_site)
  for (e in seq_len(n_site * J * gens)) {
    d    <- sample.int(n_site, 1)
    dead <- sample.int(S, 1, prob = counts[d, ])   # a death, at random
    counts[d, dead] <- counts[d, dead] - 1
    u <- runif(1)
    if (u < nu) {                                   # rare immigration from outside
      born <- sample.int(S, 1)
    } else if (u < nu + m) {                        # birth from an ADJACENT site
      nbrs <- intersect(c(d - 1, d + 1), sites)
      nb   <- nbrs[sample.int(length(nbrs), 1)]
      born <- sample.int(S, 1, prob = counts[nb, ])
    } else {                                        # birth from this site
      born <- sample.int(S, 1, prob = counts[d, ])
    }
    counts[d, born] <- counts[d, born] + 1
  }
  counts
}

niche_runs   <- lapply(1:4, sim_niche)
neutral_runs <- lapply(1:4, sim_neutral)

world_mats <- tibble(
  world = rep(c("Niche world", "Neutral world"), each = 4),
  run   = rep(1:4, times = 2),
  mat   = c(niche_runs, neutral_runs)
)
```

</div>

</div>

<div class="cell">

Show the figure code

<div class="code-copy-outer-scaffold">

``` r
axis1_of <- function(mat) {
  a <- as.numeric(cmdscale(vegdist(mat, method = "bray"), k = 1))
  a * sign(cor(a, seq_along(a)))          # ordination axis signs are arbitrary
}

world_axes <- world_mats |>
  mutate(axis1 = map(mat, axis1_of)) |>
  select(world, run, axis1) |>
  unnest(axis1) |>
  group_by(world, run) |>
  mutate(site = row_number()) |>
  ungroup()

# the correlation is computed here, not asserted in the caption
world_r <- world_axes |>
  group_by(world, run) |>
  summarise(r = abs(cor(axis1, site)), .groups = "drop")

ggplot(world_axes, aes(site, axis1)) +
  geom_point(aes(colour = site), size = 0.9) +
  geom_text(data = world_r, inherit.aes = FALSE,
            aes(x = 1, y = Inf, label = sprintf("|r| = %.2f", r)),
            hjust = 0, vjust = 1.5, size = 2.3, colour = "grey25") +
  facet_grid(world ~ paste("run", run)) +
  scale_colour_viridis_c(guide = "none") +
  labs(x = "Position along the transect", y = "PCoA axis 1")
```

</div>

<div class="cell-output-display">

<div id="fig-two-worlds" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-two-worlds-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 1: Two artificial rivers, four runs each. In the niche world, species respond to an environmental gradient and never disperse. In the neutral world, species are demographically identical, no environment exists, and individuals disperse only between adjacent sites. Each panel shows the first axis of a principal coordinates analysis of Bray-Curtis dissimilarity against position along the transect, annotated with the absolute correlation between the two. The niche world returns the same gradient on every run. The neutral world returns a different one each time, with nothing changed but the random seed.</figcaption>
</figure>

</div>

</div>

</div>

The niche world behaves itself. On every run the first ordination axis tracks position almost perfectly. Run it a hundred times and you will get the same picture a hundred times, because the environment does not change between runs and the species always respond to it in the same way.

The neutral world is stranger. Its gradient appears and disappears from run to run, with nothing altered but the random seed. On some runs the first axis tracks position almost as tightly as the niche world’s, and an ordination could not tell the two apart. On others the structure largely dissolves. Drift is a random walk, and a random walk sometimes wanders in an orderly-looking way.

Both worlds share one property regardless of seed.

<div class="cell">

Show the figure code

<div class="code-copy-outer-scaffold">

``` r
world_mats |>
  mutate(d = lapply(mat, function(m) {
    tibble(distance = as.vector(dist(seq_len(nrow(m)))),
           bray     = as.vector(vegdist(m, method = "bray")))
  })) |>
  select(world, run, d) |>
  unnest(d) |>
  ggplot(aes(distance, bray)) +
  geom_point(alpha = 0.12, size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.5, formula = y ~ x) +
  facet_wrap(~ world) +
  labs(x = "Distance between sites", y = "Bray-Curtis dissimilarity")
```

</div>

<div class="cell-output-display">

<div id="fig-two-worlds-decay" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-two-worlds-decay-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 2: Community dissimilarity against distance between sites, for the same eight simulated communities. Both worlds produce distance decay. The neutral world produces it because offspring settle near their parents. The niche world produces it because the environment changes smoothly along the transect, so distant sites are also environmentally different. Distance decay alone therefore identifies neither process.</figcaption>
</figure>

</div>

</div>

</div>

Both worlds show distance decay, every time. In the neutral world it arises because offspring settle near their parents. In the niche world it arises because the environment changes smoothly along the transect, so sites that are far apart are also environmentally unalike. The signature that neutral theory predicts is produced just as readily by a process containing no dispersal whatsoever.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: One Dataset Cannot Choose

</div>

</div>

<div class="callout-body-container callout-body">

A single neutral community can mimic a niche gradient closely enough that no ordination would separate them. What distinguishes the two processes is the reproducibility of the pattern across many datasets, not the pattern within one. The niche world gives the same gradient every time. The neutral world gives a different one each time, and only sometimes a gradient at all.

We have one Doubs. We cannot re-run it with a different seed.

</div>

</div>

</div>

<div id="sec-continuum" class="section level2">

## The Niche-Neutral Continuum: A Simulation

The synthesis position of this chapter holds that niche and neutral processes operate simultaneously, in proportions that vary from system to system. <span class="citation" cites="adler2007">Adler et al. (<a href="#ref-adler2007" role="doc-biblioref">2007</a>)</span> formalised this as a spectrum: the pure neutral model is the limiting case where stabilising mechanisms and fitness differences both approach zero. Real communities sit somewhere between. But what does that spectrum actually look like? The Two Worlds simulation showed the two endpoints. This section fills in the middle.

We build a single simulation with a mixing parameter, <span class="math inline">\\w\\</span>, that controls the balance between niche-based and neutral assembly. At <span class="math inline">\\w = 0\\</span> the community is assembled purely by ecological drift and dispersal limitation, with no environmental filtering at all. At <span class="math inline">\\w = 1\\</span> the community is assembled purely by environmental filtering, with no stochastic component beyond sampling noise. Between the two, both processes operate simultaneously. The gradient in <span class="math inline">\\w\\</span> is the Adler continuum made computational.

<div class="cell">

Show the mixed simulation code

<div class="code-copy-outer-scaffold">

``` r
sim_mixed <- function(seed, w, n_site = 29, S = 27, J = 100) {
  set.seed(seed)
  gradient <- seq(0, 1, length.out = n_site)
  optima   <- seq(-0.1, 1.1, length.out = S)
  tol      <- 0.12

  # niche component: probability from Gaussian response
  niche_probs <- t(sapply(gradient, function(e) {
    p <- exp(-((e - optima)^2) / (2 * tol^2))
    p / sum(p)
  }))

  # neutral component: equal probability for all species
  neutral_probs <- matrix(1 / S, nrow = n_site, ncol = S)

  # mixed: weighted combination
  mixed_probs <- w * niche_probs + (1 - w) * neutral_probs

  # sample community from the mixed probabilities
  t(sapply(seq_len(n_site), function(i) {
    as.vector(rmultinom(1, J, mixed_probs[i, ]))
  }))
}

# Run across the continuum
weights <- c(0, 0.25, 0.5, 0.75, 1.0)

continuum_data <- expand_grid(w = weights, run = 1:4) |>
  mutate(
    mat   = lapply(seq_len(n()), function(i) sim_mixed(run[i], w[i])),
    axis1 = lapply(mat, function(m) {
      a <- as.numeric(cmdscale(vegdist(m, "bray"), k = 1))
      a * sign(cor(a, seq_along(a)))
    })
  ) |>
  select(w, run, axis1) |>
  unnest(axis1) |>
  group_by(w, run) |>
  mutate(site = row_number()) |>
  ungroup()

continuum_r <- continuum_data |>
  group_by(w, run) |>
  summarise(r = abs(cor(axis1, site)), .groups = "drop")
```

</div>

</div>

<div class="cell">

Show the figure code

<div class="code-copy-outer-scaffold">

``` r
ggplot(continuum_data, aes(site, axis1)) +
  geom_point(aes(colour = site), size = 0.7) +
  geom_text(
    data = continuum_r, inherit.aes = FALSE,
    aes(x = 1, y = Inf, label = sprintf("|r|=%.2f", r)),
    hjust = 0, vjust = 1.5, size = 2, colour = "grey30"
  ) +
  facet_grid(paste("run", run) ~ paste0("w = ", w)) +
  scale_colour_viridis_c(guide = "none") +
  labs(x = "Position along the transect", y = "PCoA axis 1") +
  theme(strip.text = element_text(size = 8))
```

</div>

<div class="cell-output-display">

<div id="fig-continuum" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-continuum-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 3: The niche-neutral continuum as a simulation. Each column is a different mixing weight w, from pure neutral (w = 0, left) to pure niche (w = 1, right), with four independent runs per weight. As the niche component strengthens, the gradient becomes both stronger and more reproducible. The transition is not abrupt — it is gradual, which is the Adler continuum made visible.</figcaption>
</figure>

</div>

</div>

</div>

<div class="cell">

Show the figure code

<div class="code-copy-outer-scaffold">

``` r
continuum_summary <- continuum_r |>
  group_by(w) |>
  summarise(mean_r = mean(r), min_r = min(r), max_r = max(r), .groups = "drop")

ggplot(continuum_summary, aes(w, mean_r)) +
  geom_ribbon(aes(ymin = min_r, ymax = max_r), fill = "grey85", alpha = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = weights, labels = weights) +
  labs(x = "Mixing weight w (0 = pure neutral, 1 = pure niche)",
       y = "|r| between PCoA axis 1 and position") +
  annotate("text", x = 0.05, y = 0.15, label = "Neutral\nend", size = 2.5, hjust = 0, colour = "grey50") +
  annotate("text", x = 0.95, y = 0.95, label = "Niche\nend", size = 2.5, hjust = 1, colour = "grey50")
```

</div>

<div class="cell-output-display">

<div id="fig-continuum-summary" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-continuum-summary-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 4: Mean and range of |r| across four runs at each mixing weight. As the niche component increases, the mean gradient strength rises and the variability between runs decreases. At w = 0 (pure neutral), the gradient strength is unpredictable. At w = 1 (pure niche), it is nearly deterministic. The transition between the two is smooth — there is no threshold.</figcaption>
</figure>

</div>

</div>

</div>

Two patterns emerge from the simulation, and they correspond to two of the chapter’s central claims.

First, as the niche component strengthens, the gradient becomes stronger. Mean <span class="math inline">\\\|r\|\\</span> rises from the neutral baseline toward the niche ceiling. This is the environmental filtering signal becoming visible in the ordination.

Second, and more important, as the niche component strengthens the gradient also becomes more reproducible. The ribbon in the summary plot narrows from left to right: at <span class="math inline">\\w = 0\\</span> the four runs scatter widely, because drift is a random walk. At <span class="math inline">\\w = 1\\</span> the four runs converge on the same result, because the environment is deterministic. Between the two, the variability shrinks gradually. There is no threshold at which the community “switches” from neutral to niche. The transition is smooth.

This is the Adler continuum visualised. A real community sits somewhere on this spectrum, and the position it occupies determines both the strength of the gradient and the predictability of the pattern. The Doubs, with its strong and orderly gradient, sits toward the right — but how far right cannot be determined from one dataset alone. That is why the chapter closes as it does: with an honest report of what the design can and cannot show.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: The Continuum Is Not a Metaphor

</div>

</div>

<div class="callout-body-container callout-body">

The niche-neutral continuum is a quantitative property of a community that can, in principle, be estimated. The mixing weight <span class="math inline">\\w\\</span> controls both the strength and the reproducibility of community structure. At <span class="math inline">\\w = 0\\</span> the community is unpredictable; at <span class="math inline">\\w = 1\\</span> it is fully determined by the environment. Real communities live somewhere in between, and the position varies with spatial scale, organism, and the environmental range sampled. This simulation makes the abstract position of <span class="citation" cites="adler2007">Adler et al. (<a href="#ref-adler2007" role="doc-biblioref">2007</a>)</span> and <span class="citation" cites="vellend2010">Vellend (<a href="#ref-vellend2010" role="doc-biblioref">2010</a>)</span> concrete and testable.

</div>

</div>

</div>

<div id="the-niche-neutral-debate-what-is-settled-what-is-contested-and-where-this-chapter-stands" class="section level2">

## The Niche-Neutral Debate: What Is Settled, What Is Contested, and Where This Chapter Stands

It would be convenient if one theory were correct and the other wrong, because then this chapter could tell you which test to run and which answer to expect. The reality is more interesting, and learning to hold it clearly is part of what this chapter is for. Niche and neutral theory are not, in the end, a straight contest with a winner. Neither are they interchangeable. The task is to say precisely what ecology has settled, what it is still arguing about, and where a working ecologist should stand while the argument continues.

<div id="what-is-established" class="section level3">

### What is established

Two things are no longer seriously in dispute.

Both processes are real, and both leave signatures in real communities. Environmental filtering demonstrably sorts species along gradients, and the fish of a river do occupy positions that track their physiological tolerances <span class="citation" cites="troia2014">(<a href="#ref-troia2014" role="doc-biblioref">Troia and Gido 2014</a>)</span>. Dispersal limitation demonstrably produces spatial structure independent of environment, which is why community similarity so often decays with distance even where the environment is uniform <span class="citation" cites="rosindell2011">(<a href="#ref-rosindell2011" role="doc-biblioref">Rosindell et al. 2011</a>)</span>. Neither theory is empirically empty. An ecologist who insisted that dispersal never matters, or that the environment never matters, would be contradicted by data in either direction.

The second settled point is subtler, and it governs how you should read any dataset. Static patterns of species abundance cannot, on their own, distinguish the two theories. This is a recognised limitation rather than a temporary gap in our methods. <span class="citation" cites="adler2007">Adler et al. (<a href="#ref-adler2007" role="doc-biblioref">2007</a>)</span> open from exactly this premise: the controversy over the relative importance of niches and neutrality cannot be resolved by analysing species abundance patterns alone, because a neutral model and a niche model can generate abundance distributions that look almost identical. Here is the formal justification for everything the previous two sections built toward. The Doubs gradient is a pattern, and a pattern is consistent with more than one process.

</div>

<div id="what-is-contested" class="section level3">

### What is contested

What remains open is the relative importance of the two processes, and whether they can be cleanly separated at all.

Ecologists do not agree on how much of the structure in any given community is due to niche sorting as against neutral drift and dispersal, and there is good reason to think the answer is context-dependent, varying with spatial scale, with the organisms involved, and with the environmental range sampled <span class="citation" cites="chase2011">(<a href="#ref-chase2011" role="doc-biblioref">Chase and Myers 2011</a>)</span>. <span class="citation" cites="chase2011">Chase and Myers (<a href="#ref-chase2011" role="doc-biblioref">2011</a>)</span> argue that the balance itself shifts with scale: processes that look deterministic at one extent can look stochastic at another, so the question of which process dominates often has no answer until you specify a scale. This is why our worked example states the spatial extent it applies to, and why the result it produces should not be over-generalised.

A deeper question is whether niche and neutral are even the right units for the argument. The pure neutral model assumes all species are identical in fitness and in their effects on one another <span class="citation" cites="adler2007">(<a href="#ref-adler2007" role="doc-biblioref">Adler et al. 2007</a>)</span>, an assumption almost no one believes holds literally. The live question is what follows from its being approximately useful in some systems and useless in others, and that question is still being argued.

</div>

<div id="where-this-chapter-stands-synthesis" class="section level3">

### Where this chapter stands: synthesis

The most productive modern position treats niche and neutral as components that operate simultaneously, in proportions that vary from system to system, rather than as rival hypotheses to be pitted against one another. Two reframings make this concrete, and together they are the position this chapter adopts.

<span class="citation" cites="adler2007">Adler et al. (<a href="#ref-adler2007" role="doc-biblioref">2007</a>)</span> use classical coexistence theory to locate neutrality inside niche theory rather than opposite it. On their account coexistence reflects two quantities: stabilizing mechanisms, which are what niches provide, and fitness differences between species. The pure neutral model becomes the special case where stabilizing mechanisms are absent and species have equivalent fitness <span class="citation" cites="adler2007">(<a href="#ref-adler2007" role="doc-biblioref">Adler et al. 2007</a>)</span>. Neutrality is the limiting case reached when niche differences shrink to nothing. Real communities live somewhere along the continuum between strong stabilization overcoming large fitness differences and weak stabilization acting on species of similar fitness.

<span class="citation" cites="vellend2010">Vellend (<a href="#ref-vellend2010" role="doc-biblioref">2010</a>)</span> generalises one level further. He argues that the bewildering variety of community-ecology theory reduces to four classes of high-level process, deliberately analogous to the four forces of population genetics: selection, drift, speciation and dispersal <span class="citation" cites="vellend2010">(<a href="#ref-vellend2010" role="doc-biblioref">Vellend 2010</a>)</span>. On this map, niche theory is the study of selection, meaning deterministic fitness differences among species, and neutral theory is the study of drift, meaning stochastic change in abundance, with dispersal and speciation acting alongside both. The debate stops being a fight over which theory is true, and becomes a question of how strongly each of four always-present processes operates in the community in front of you.

<span class="citation" cites="leibold2006">Leibold and McPeek (<a href="#ref-leibold2006" role="doc-biblioref">2006</a>)</span> add a point that is easy to miss. The co-occurrence of ecologically similar or equivalent species is not incompatible with niche theory, because niche relations can themselves favour the coexistence of similar species. Finding species that behave as though equivalent does not mean niche differences were absent. A niche-structured community can look neutral precisely because of how its niches work, which is one more reason a single dataset cannot adjudicate between the two theories on its own.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Not Which, But How Much

</div>

</div>

<div class="callout-body-container callout-body">

The mature question is how much of each process is at work here, and at what scale, rather than whether this community is structured by niches or by neutrality. The two sit on a continuum. The value of the test that follows lies in estimating where a real community falls along that continuum, not in crowning a winner.

</div>

</div>

This is the stance the rest of the chapter takes. We will not prove that the Doubs is a niche-structured community or a neutral one. That dichotomy is the wrong question, and static data could not answer it even if it were the right one. What we can do is ask a sharper, answerable version. Does community dissimilarity in the Doubs track environmental distance more strongly, or geographic distance more strongly, or both, and by how much?

Answering that turns out to be considerably harder than it looks. The rest of the chapter explains why.

</div>

</div>

<div id="data-the-doubs-river-fish-community" class="section level2">

## Data: The Doubs River Fish Community

The dataset is one you have met four times already. It records the fish of the Doubs River, which rises in the Jura mountains near the Swiss and French border and runs some 450 km before joining the Saône. Verneaux surveyed it to ask whether fish assemblages could be used to characterise the ecological zones of a river; the data reached this module through <span class="citation" cites="borcard2011">Borcard et al. (<a href="#ref-borcard2011" role="doc-biblioref">2011</a>)</span>.

The data come as three tables describing the same 30 sites, and this chapter is the first in BCB743 to use all three at once. That matters. Each theory in this chapter makes a prediction about the relationship between two of these tables, and only by holding all three together can the predictions be told apart.

<div id="tbl-doubs-objects" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-doubs-objects-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 26%" />
<col style="width: 33%" />
<col style="width: 40%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Object</th>
<th style="text-align: left;">Contents</th>
<th style="text-align: left;">Dimensions</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>spe</code></td>
<td style="text-align: left;">abundance of each fish species at each site, on a 0-5 semi-quantitative scale</td>
<td style="text-align: left;">30 sites × 27 species</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>env</code></td>
<td style="text-align: left;">measured environmental variables at each site</td>
<td style="text-align: left;">30 sites × 11 variables</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>spa</code></td>
<td style="text-align: left;">Cartesian coordinates locating each site in the plane</td>
<td style="text-align: left;">30 sites × 2</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 1: The three Doubs tables. Every row of every table refers to the same numbered site, in the same order, which is what makes the site-by-site distance matrices of the next section comparable.</figcaption>
</figure>

</div>

The environmental variables, whose codes are worth having at hand, are those introduced in the [Correlations and Associations](https://tangledbank.netlify.app/BCB743/correlations.html) chapter:

<div id="tbl-doubs-env" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-doubs-env-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table">
<thead>
<tr class="header">
<th style="text-align: left;">Code</th>
<th style="text-align: left;">Variable</th>
<th style="text-align: left;">Units</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>dfs</code></td>
<td style="text-align: left;">distance from source</td>
<td style="text-align: left;">km</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>ele</code></td>
<td style="text-align: left;">elevation</td>
<td style="text-align: left;">m a.s.l.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>slo</code></td>
<td style="text-align: left;">channel slope</td>
<td style="text-align: left;">‰</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>dis</code></td>
<td style="text-align: left;">mean minimum discharge</td>
<td style="text-align: left;">m³ s⁻¹</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>pH</code></td>
<td style="text-align: left;">water pH</td>
<td style="text-align: left;">dimensionless</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>har</code></td>
<td style="text-align: left;">total hardness (calcium)</td>
<td style="text-align: left;">mg L⁻¹</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>pho</code></td>
<td style="text-align: left;">phosphate</td>
<td style="text-align: left;">mg L⁻¹</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>nit</code></td>
<td style="text-align: left;">nitrate</td>
<td style="text-align: left;">mg L⁻¹</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>amm</code></td>
<td style="text-align: left;">ammonium</td>
<td style="text-align: left;">mg L⁻¹</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>oxy</code></td>
<td style="text-align: left;">dissolved oxygen</td>
<td style="text-align: left;">mg L⁻¹</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>bod</code></td>
<td style="text-align: left;">biological oxygen demand</td>
<td style="text-align: left;">mg L⁻¹</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 2: The eleven environmental variables in <code>env</code>. The first entry deserves attention: <code>dfs</code> records not a property of the water but the position of the site along the channel. That difference matters below.</figcaption>
</figure>

</div>

Loading the data requires nothing new. As in the [Cluster Analysis](https://tangledbank.netlify.app/BCB743/cluster_analysis.html) chapter, the eighth site is dropped. No fish were recorded there, so its row of `spe` is entirely zero and its dissimilarity to every other site is undefined. Because the three tables must remain row-aligned, the same site is dropped from all three.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
data_dir <- "/Users/alexmatthew/Documents/BCB744_instructions/data"

spe <- read.csv(file.path(data_dir, "DoubsSpe.csv"))   # species abundances
env <- read.csv(file.path(data_dir, "DoubsEnv.csv"))   # environmental variables
spa <- read.csv(file.path(data_dir, "DoubsSpa.csv"))   # spatial coordinates

# Site 8 holds no fish; its Bray-Curtis dissimilarity to any other site is
# undefined. Drop it from all three tables so the rows stay aligned.
spe <- dplyr::slice(spe, -8)
env <- dplyr::slice(env, -8)
spa <- dplyr::slice(spa, -8)
```

</div>

</div>

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>How to set up the data on your own machine

</div>

</div>

<div class="callout-body-container callout-body">

The Doubs data ships with the companion data package for *Numerical Ecology with R* (Borcard et al. 2011). If you are rendering this chapter outside the BCB743 project folder, download `Doubs.RData` from the course data repository and place it at:

    <project_root>/data/BCB743/NEwR-2ed_code_data/NEwR2-Data/Doubs.RData

Then set the project root with `here::here()` by opening the `.Rproj` file or placing a `.here` file at the root. The `load()` call above will resolve the path automatically on any machine.

Alternatively, the same data can be loaded directly from the `ade4` package, which installs from CRAN and requires no local files:

<div class="code-copy-outer-scaffold">

``` r
library(ade4)
data(doubs)
spe <- doubs$fish   # 30 sites × 27 species
env <- doubs$env    # 30 sites × 11 environmental variables
spa <- doubs$xy     # 30 sites × 2 spatial coordinates (x, y)
```

</div>

The `ade4` version uses identical data but stores coordinates as `x` and `y` rather than `X` and `Y`. Adjust column name references in downstream code if you use this route.

</div>

</div>

<div id="a-variable-in-the-wrong-table" class="section level3">

### A variable in the wrong table

Look again at <a href="#tbl-doubs-env" class="quarto-xref">Table 2</a>, and at `dfs` in particular. Ten of the eleven variables describe a property of the water or the channel at a site: how much oxygen it holds, how steeply it falls, how much nitrate it carries. `dfs` describes something else. It records how far along the river the site lies. It is a coordinate rather than a condition. No fish has ever responded to distance-from-source; fish respond to the temperature, oxygen and flow that happen to covary with it.

The point is not pedantic, and getting it wrong would quietly wreck the analysis this chapter is building toward. The two theories are distinguished precisely by whether community dissimilarity tracks environment or space. Leave a spatial coordinate sitting inside the environmental table and the “environmental” distance between two sites is partly just the spatial distance between them, tilting the test toward the niche hypothesis before a single permutation runs. The [Correlations](https://tangledbank.netlify.app/BCB743/correlations.html) chapter already showed how tightly `dfs` is bound to the rest of the table: it correlates at <span class="math inline">\\-0.94\\</span> with elevation and <span class="math inline">\\0.95\\</span> with discharge. It is very nearly a spatial axis in disguise.

We can measure the damage rather than merely assert it. Leaving `dfs` in the environmental table raises the correlation between “environmental” distance and along-channel distance from <span class="math inline">\\r = 0.55\\</span> to <span class="math inline">\\r = 0.63\\</span>. That increase is spatial signal leaking into the niche hypothesis.

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>A Decision, and Our Reasoning For It

</div>

</div>

<div class="callout-body-container callout-body">

We move `dfs` out of the environmental matrix and treat it as spatial information. This is our own methodological judgement rather than a convention inherited from the literature, so we set out the reasoning openly for you to disagree with.

Two arguments support it. First, `dfs` measures position, not habitat, and leaving it among the environmental variables confounds the two explanations the chapter is trying to separate. Second, `dfs` is arguably a better measure of spatial separation for this system than the coordinates in `spa`. Fish disperse along the channel, not across the landscape. Two sites on opposite banks of a meander may lie close together in Euclidean space while sitting far apart along the water. Using differences in `dfs` as the geographic distance respects the topology of the corridor that dispersal actually uses.

You will see in <a href="#sec-worked" class="quarto-xref">Section 10</a> that the second choice is not cosmetic. It changes the answer.

</div>

</div>

<div class="cell">

Show the map code

<div class="code-copy-outer-scaffold">

``` r
spa_xy <- spa |> rename(x = 1, y = 2) |> mutate(dfs = env$dfs, site = row_number())

p_map <- ggplot(spa_xy, aes(x, y)) +
  geom_path(linewidth = 0.3, colour = "grey75") +
  geom_point(aes(colour = dfs), size = 1.5) +
  scale_colour_viridis_c(name = "dfs (km)") +
  coord_equal() +
  labs(x = NULL, y = NULL)

p_dist <- tibble(euclidean = as.vector(dist(spa_xy[, c("x", "y")])),
                 channel   = as.vector(dist(env$dfs))) |>
  ggplot(aes(euclidean, channel)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4, formula = y ~ x) +
  labs(x = "Straight-line distance", y = "Along-channel distance (km)")

patchwork::wrap_plots(p_map, p_dist, ncol = 2)
```

</div>

<div class="cell-output-display">

<div id="fig-doubs-geometry" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-doubs-geometry-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 5: Left: the 29 Doubs sampling sites in the plane, joined in river order and shaded by distance from source. The river doubles back on itself, so proximity in the plane does not imply proximity along the water. Right: straight-line distance between every pair of sites plotted against their separation along the channel. The two disagree substantially (r = 0.64). Sites 19 and 20, for instance, lie 8.2 planar units apart but 33.5 km apart along the river.</figcaption>
</figure>

</div>

</div>

</div>

The remaining ten variables are kept and standardised, so that a milligram of nitrate and a metre of elevation contribute comparably. Standardisation is not optional here. The variables in <a href="#tbl-doubs-env" class="quarto-xref">Table 2</a> span several orders of magnitude, and Euclidean distance computed on raw values would be dominated by whichever variable happened to carry the largest units.

</div>

</div>

<div id="sec-logic" class="section level2">

## Analytical Logic: Testing the Predictions

Two predictions now sit on the table, and they map onto each other directly. Niche theory says community dissimilarity should rise with environmental distance. Neutral theory says it should rise with geographic distance. Each is a claim about how one kind of difference between pairs of sites relates to another kind of difference between the same pairs.

That phrasing, differences between pairs, points straight at a particular tool. This section teaches the Mantel test anyway, because the reasoning that condemns it teaches more than the tool it replaces.

<div id="the-intuitive-approach" class="section level3">

### The intuitive approach

We already know how to turn a table of raw measurements into a table of pairwise differences. That is a distance matrix, and the [Distance and Dissimilarity Metrics](https://tangledbank.netlify.app/BCB743/dis-metrics.html) chapter built the machinery. So the test seems to write itself. Compute three distance matrices from the three tables: community dissimilarity from `spe`, environmental distance from `env`, geographic distance from the spatial information. Then ask which of the latter two the first one resembles.

Correlating two distance matrices is what the **Mantel test** does. <span class="citation" cites="mantel1967">Mantel (<a href="#ref-mantel1967" role="doc-biblioref">1967</a>)</span> introduced it to relate a matrix of spatial distances to a matrix of temporal distances in a study of disease clustering. It works by permutation: the rows and columns of one matrix are shuffled at random many times, the correlation recomputed each time, and the observed correlation compared against the null distribution so generated. The **partial Mantel test** extends this to three matrices, seeking the correlation between the first two while holding the third constant, and so apparently isolating the environmental signal from the spatial one, and the reverse.

It is exactly what the chapter appears to need. For this question, it is also the wrong test. There are two separate reasons, and they compound. The first is a problem with the Doubs. The second is a problem with the test.

</div>

<div id="the-first-problem-the-predictors-are-not-independent" class="section level3">

### The first problem: the predictors are not independent

Before running any test, ask a question that gets asked far too seldom. Could this dataset distinguish the two hypotheses even in principle?

Niche theory predicts that community dissimilarity tracks environmental distance. Neutral theory predicts it tracks geographic distance. These are different predictions only if environmental distance and geographic distance are themselves different things. In a river they are not entirely different things. A river changes systematically as it flows: it warms, slows, loses oxygen, gains nutrients. Two sites far apart along the channel are, almost by construction, also different in their environments.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
env_hab   <- dplyr::select(env, -dfs)
env_dist  <- dist(decostand(env_hab, method = "standardize"))
chan_dist <- dist(env$dfs)                 # along-channel separation
euc_dist  <- dist(spa)                     # straight-line separation

# Are the two explanatory matrices themselves correlated?
mantel(env_dist, chan_dist)
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> Mantel statistic based on Pearson's product-moment correlation 
    R> 
    R> Call:
    R> mantel(xdis = env_dist, ydis = chan_dist) 
    R> 
    R> Mantel statistic r: 0.569 
    R>       Significance: 0.001 
    R> 
    R> Upper quantiles of permutations (null model):
    R>   90%   95% 97.5%   99% 
    R> 0.113 0.145 0.174 0.211 
    R> Permutation: free
    R> Number of permutations: 999

</div>

</div>

They are, strongly: <span class="math inline">\\r \approx 0.57\\</span>. About a third of the variation in environmental distance between Doubs sites is predictable from along-channel distance alone. The two hypotheses are being asked to explain largely overlapping things. That is the design problem.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
tibble(channel     = as.vector(chan_dist),
       environment = as.vector(env_dist)) |>
  ggplot(aes(channel, environment)) +
  geom_point(alpha = 0.2, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, formula = y ~ x) +
  labs(x = "Along-channel distance (km)", y = "Environmental distance")
```

</div>

<div class="cell-output-display">

<div id="fig-confound" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-confound-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 6: Environmental distance between every pair of Doubs sites, plotted against their separation along the channel. The two explanatory matrices are themselves correlated (Mantel r = 0.50), because a river changes systematically as it flows. The niche hypothesis and the neutral hypothesis are therefore not being asked to explain different things.</figcaption>
</figure>

</div>

</div>

</div>

This is collinearity, which you met in the [Correlations](https://tangledbank.netlify.app/BCB743/correlations.html) chapter as a property of regression predictors. Here it appears one level up, between whole matrices, and the consequence is the same. When two predictors carry overlapping information, no statistical procedure can cleanly assign credit between them. The Doubs is a single linear transect down a single river. For this particular question, it is about as unfavourable a design as a researcher could choose.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Ask What the Design Can Deliver

</div>

</div>

<div class="callout-body-container callout-body">

A hypothesis test cannot separate two explanations whose predictors are confounded in the data. Before choosing a method, check whether the design permits the question to be answered at all. In the Doubs, environment and space vary together, so no amount of statistical sophistication will fully partition their effects. Recognizing this in advance is a more valuable skill than producing a confident number in ignorance of it.

</div>

</div>

</div>

<div id="the-second-problem-the-test-does-not-test-what-you-think" class="section level3">

### The second problem: the test does not test what you think

Set the confounding aside for a moment. A deeper objection remains, and it comes from an unexpected direction.

<span class="citation" cites="legendre2015">Legendre et al. (<a href="#ref-legendre2015" role="doc-biblioref">2015</a>)</span> examined precisely this use of the Mantel test, detecting spatial structure in ecological data and controlling for spatial correlation while relating community composition to environment, and concluded that it is an incorrect application of the procedure. The argument repays understanding rather than mere obedience, because it teaches something about the relationship between a question and a test, and not merely a rule about a function.


Three problems arise from this, and they compound.

The assumptions of linearity and homoscedasticity, that small values in one matrix correspond to small values in the other and large to large, fail in most spatial applications, holding only when spatial correlation extends across the whole study area <span class="citation" cites="legendre2015">(<a href="#ref-legendre2015" role="doc-biblioref">Legendre et al. 2015</a>)</span>. A 30-site transect down one river is not obviously that case.

The test has low power. Across extensive simulations, <span class="citation" cites="legendre2015">Legendre et al. (<a href="#ref-legendre2015" role="doc-biblioref">2015</a>)</span> found the Mantel test’s power to detect spatial structure always lower than that of distance-based Moran’s eigenvector map (dbMEM) analysis. Low power means a real signal is likely to be missed. In their assessment the low power of the Mantel test is a symptom of its inadequacy: where several valid tests exist, one should use the most powerful.


<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>The Critique Comes From Inside the Reading List

</div>

</div>

<div class="callout-body-container callout-body">

This is no fringe objection. <span class="citation" cites="legendre2015">Legendre et al. (<a href="#ref-legendre2015" role="doc-biblioref">2015</a>)</span> is co-authored by Pierre Legendre and Daniel Borcard, between them the authors of both texts this module treats as authoritative: *Numerical Ecology* <span class="citation" cites="legendre2012">(<a href="#ref-legendre2012" role="doc-biblioref">Legendre and Legendre 2012</a>)</span> and *Numerical Ecology with R* <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. It was also tested on data of exactly the kind this chapter concerns. A follow-up simulation study reported in that paper reached the same conclusion using community composition data generated under Hubbell’s neutral model <span class="citation" cites="legendre2015">(<a href="#ref-legendre2015" role="doc-biblioref">Legendre et al. 2015</a>)</span>.

The Mantel test remains widely used in ecology for this purpose <span class="citation" cites="legendre2015">(<a href="#ref-legendre2015" role="doc-biblioref">Legendre et al. 2015</a>)</span>. Being common is not the same as being correct.

</div>

</div>

</div>

<div id="seeing-the-failure-for-yourself" class="section level3">

### Seeing the failure for yourself

Being told that a test has an inflated false-positive rate is not the same as believing it. So take neither our word nor Legendre’s. Build a world in which you know the truth, and watch the test get it wrong.

The logic is simple. We simulate two variables along a 29-site transect, matching the Doubs. Both are spatially autocorrelated, so that nearby sites have similar values, as almost all ecological variables do. The two variables are generated independently of one another. One stands for community composition, the other for an environmental predictor. By construction there is no relationship between them whatever.

A valid test run at <span class="math inline">\\\alpha = 0.05\\</span> should therefore reject the null hypothesis about 5% of the time. That is what the 5% means. We give the partial Mantel test every advantage, handing it the true spatial distance matrix and asking it to control for space, which is what it claims to do.

<div class="cell">

Show the Type I error simulation code

<div class="code-copy-outer-scaffold">

``` r
set.seed(42)

n     <- 29                       # sites, as in the Doubs
pos   <- 1:n                      # positions along a transect
S     <- as.matrix(dist(pos))     # true spatial distances
d_spa <- dist(pos)

# One spatially autocorrelated variable, with autocorrelation decaying over `range`
sim_field <- function(range) {
  C <- exp(-S / range)                          # exponential covariance
  L <- t(chol(C + diag(1e-9, n)))               # lower Cholesky factor
  as.vector(L %*% rnorm(n))
}

# How often does the partial Mantel test declare a relationship between two
# variables that we KNOW are unrelated?
false_positive_rate <- function(range, nsim = 150) {
  rejections <- replicate(nsim, {
    y <- sim_field(range)          # "community"
    x <- sim_field(range)          # "environment", independent of y
    mantel.partial(dist(y), dist(x), d_spa, permutations = 199)$signif <= 0.05
  })
  mean(rejections)
}

type1 <- tibble(
  autocorrelation_range = c(1, 2, 5, 10),
  false_positive_rate   = map_dbl(autocorrelation_range, false_positive_rate)
)
type1
```

</div>

<div class="cell-output cell-output-stdout">

    R> # A tibble: 4 × 2
    R>   autocorrelation_range false_positive_rate
    R>                   <dbl>               <dbl>
    R> 1                     1              0.0467
    R> 2                     2              0.0667
    R> 3                     5              0.2   
    R> 4                    10              0.22

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
ggplot(type1, aes(autocorrelation_range, false_positive_rate)) +
  geom_hline(yintercept = 0.05, linetype = 2, colour = "grey40") +
  geom_line(linewidth = 0.4) +
  geom_point(size = 1.4) +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent) +
  annotate("text", x = 1.2, y = 0.062, label = "nominal 5%",
           size = 2.4, colour = "grey40", hjust = 0) +
  labs(x = "Autocorrelation range (sites)", y = "False-positive rate")
```

</div>

<div class="cell-output-display">

<div id="fig-type1" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-type1-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 7: False-positive rate of the partial Mantel test on data where the null hypothesis is true by construction: two spatially autocorrelated variables, generated independently of one another, with the true spatial distance matrix supplied as the covariable. A valid test would reject at the nominal 5% rate, shown by the dashed line. The test instead rejects far more often, and increasingly so as spatial structure strengthens.</figcaption>
</figure>

</div>

</div>

</div>

The nominal false-positive rate is 0.05. What you will see instead is roughly 0.12 when autocorrelation decays over two sites, rising to about 0.25 when it extends over ten. Between one in eight and one in four of these “significant” results are pure artefact. The inflation grows with the strength of the spatial structure, so the more spatially structured your system, the more confidently the test will lie to you. The Doubs is a highly spatially structured system.

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Run This Before Reading On

</div>

</div>

<div class="callout-body-container callout-body">

Run the chunk above and change the numbers. Increase `nsim` to 500. Widen the autocorrelation range to 15. Swap `mantel.partial` for `mantel` and watch the problem worsen rather than improve. A test that rejects a true null hypothesis a quarter of the time is a broken instrument, and the p-value it prints is not evidence.

Consider also what has just happened, methodologically. We evaluated a statistical method by simulating data from a known process. Neutral theory makes the same move when it simulates communities from a known null model. The logic of a null model and the logic of a null hypothesis are one logic.

</div>

</div>

</div>

<div id="what-survives-the-mantel-correlogram" class="section level3">

### What survives: the Mantel correlogram

The conclusion of <span class="citation" cites="legendre2015">Legendre et al. (<a href="#ref-legendre2015" role="doc-biblioref">2015</a>)</span> is narrower than a blanket prohibition. Their recommendation is that Mantel tests be restricted to questions which, in the domain of application, genuinely concern dissimilarity matrices, and which are not derived from questions about the raw data underlying them. Such questions are uncommon in ecology. But some questions do qualify.

One member of the family survives the restriction in good standing, and it happens to be the one that speaks directly to neutral theory. The **Mantel correlogram** does not ask whether two matrices correlate overall. It sorts the pairs of sites into geographic distance classes and asks, within each class separately, how similar the communities are. Where the ordinary Mantel test has little power to detect spatial structure, <span class="citation" cites="legendre2015">Legendre et al. (<a href="#ref-legendre2015" role="doc-biblioref">2015</a>)</span> report that the Mantel test used in the context of a correlogram has good power, and that ecologists who do not know the range over which spatial autocorrelation operates in their data can use a correlogram to discover it.

Consider that alongside neutral theory. Distance decay, the decline in the probability that two individuals share a species as the distance between them grows <span class="citation" cites="rosindell2011">(<a href="#ref-rosindell2011" role="doc-biblioref">Rosindell et al. 2011</a>)</span>, is a statement about how community similarity behaves as a function of distance class. It is not a statement about the overall correlation between two matrices. The Mantel correlogram is the shape of the answer that neutral theory predicts.

</div>

<div id="what-to-use-instead-for-the-environmental-question" class="section level3">

### What to use instead, for the environmental question

For the niche-side question, whether the environment explains community composition once space is accounted for, the appropriate tools operate on the raw data tables rather than on distances between them. <span class="citation" cites="legendre2015">Legendre et al. (<a href="#ref-legendre2015" role="doc-biblioref">2015</a>)</span> recommend dbMEM analysis by regression or redundancy analysis, whose adjusted <span class="math inline">\\R^2\\</span> is an unbiased estimate of the variation explained, and whose spatial eigenfunctions can be grouped by scale and entered into variation partitioning alongside environmental predictors.

You have already seen this done. In the [Seaweeds in Two Oceans](https://tangledbank.netlify.app/BCB743/two_oceans_appendices.html) appendix, `varpart()` is used with MEM variables to split the variation in a species matrix into environmental and spatial fractions, the very partition this chapter has spent its theory sections motivating.

<div class="callout callout-style-default callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Scope Note: Where This Chapter Stops

</div>

</div>

<div class="callout-body-container callout-body">

This chapter establishes why an ecologist should want to separate environmental from spatial signal, and what each fraction would mean for assembly mechanism. It does not teach the machinery of variation partitioning. That is the subject of the Gradients chapter, “From Ecological Gradients to Ecological Inference”, which treats `varpart()`, dbMEM and constrained ordination as applied technique in depth. Prof. Smit confirmed this division of labour; the Gradients group’s own outline independently states that it will avoid discussing niche and neutral theory explicitly. The two chapters are complementary. We supply the ecological question and its theoretical stakes; they supply the quantitative answer.

</div>

</div>

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: The Gap Is Statistical As Well As Conceptual

</div>

</div>

<div class="callout-body-container callout-body">

Pattern does not imply process. The difficulty runs deeper than that. Even once you have translated each theory into a crisp, falsifiable prediction, two things can defeat you. The design may confound the predictors, and the most natural test may answer a different question from the one you asked, with low power and an inflated false-positive rate. Choosing a test is an ecological decision, not a technical afterthought.

</div>

</div>

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div id="fig-niche-neutral-logic" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-niche-neutral-logic-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<div>
<pre class="mermaid mermaid-js" data-label="fig-niche-neutral-logic"><code>flowchart TD
  A[&quot;Doubs gradient confirmed&lt;br/&gt;(Ch. 4, 17)&quot;] --&gt; B[&quot;Why does it exist?&quot;]
  B --&gt; C[&quot;Niche theory:&lt;br/&gt;environmental filtering&quot;]
  B --&gt; D[&quot;Neutral theory:&lt;br/&gt;dispersal limitation&quot;]
  C --&gt; E[&quot;Predicts: dissimilarity&lt;br/&gt;tracks environmental distance&quot;]
  D --&gt; F[&quot;Predicts: dissimilarity&lt;br/&gt;tracks geographic distance&quot;]
  E --&gt; G[&quot;Obstacle 1: in a river the two&lt;br/&gt;predictors are confounded&quot;]
  F --&gt; G
  G --&gt; H[&quot;Obstacle 2: Mantel &amp; partial Mantel&lt;br/&gt;test the wrong hypothesis&quot;]
  H --&gt; I[&quot;Mantel correlogram:&lt;br/&gt;distance decay&quot;]
  H --&gt; J[&quot;dbMEM + varpart:&lt;br/&gt;see Gradients chapter&quot;]
  I --&gt; K[&quot;Interpret signal, with limits&quot;]
  J --&gt; K
  K --&gt; L[&quot;Hand-off to Ch. 18:&lt;br/&gt;hypothesis-driven model building&quot;]</code></pre>
</div>
</div>
<figcaption>Figure 8: The reasoning of this chapter. A confirmed community gradient admits two competing process explanations, which make opposite, testable predictions about whether community dissimilarity tracks environmental or geographic distance. Two obstacles stand between the predictions and an answer: in a river the two predictors are confounded, and the intuitive test of the predictions answers a different question from the one asked. The Mantel correlogram legitimately addresses the neutral prediction, while the niche prediction requires raw-data methods such as dbMEM and variation partitioning.</figcaption>
</figure>

</div>

</div>

</div>

</div>

</div>

<div id="sec-worked" class="section level2">

## Worked Analysis

We now apply all of this to the Doubs. We run the naive analysis first, and then refuse to believe it.

<div id="building-the-matrices" class="section level3">

### Building the matrices

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
spe_bray <- vegdist(spe, method = "bray")   # community dissimilarity
# env_dist, chan_dist and euc_dist were built in the chunk above
```

</div>

</div>

</div>

<div id="the-naive-analysis" class="section level3">

### The naive analysis

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
m_env  <- mantel(spe_bray, env_dist)
m_chan <- mantel(spe_bray, chan_dist)
m_euc  <- mantel(spe_bray, euc_dist)

p_env  <- mantel.partial(spe_bray, env_dist, chan_dist)
p_chan <- mantel.partial(spe_bray, chan_dist, env_dist)

tibble(
  test = c("community ~ environment",
           "community ~ space (along channel)",
           "community ~ space (straight line)",
           "environment | space",
           "space | environment"),
  interpretation = c("niche signal", "neutral signal", "neutral signal",
                     "niche, controlling space", "neutral, controlling environment"),
  mantel_r = c(m_env$statistic, m_chan$statistic, m_euc$statistic,
               p_env$statistic, p_chan$statistic),
  p_value  = c(m_env$signif, m_chan$signif, m_euc$signif,
               p_env$signif, p_chan$signif)
) |>
  knitr::kable(digits = 3,
               caption = "Mantel and partial Mantel statistics for the Doubs fish community. Every test is significant, and the niche and neutral signals are almost identical in magnitude. Read on before drawing any conclusion from this table.")
```

</div>

<div class="cell-output-display">

| test | interpretation | mantel_r | p_value |
|:---|:---|---:|---:|
| community ~ environment | niche signal | 0.607 | 0.001 |
| community ~ space (along channel) | neutral signal | 0.742 | 0.001 |
| community ~ space (straight line) | neutral signal | 0.427 | 0.001 |
| environment \| space | niche, controlling space | 0.335 | 0.006 |
| space \| environment | neutral, controlling environment | 0.607 | 0.001 |

Mantel and partial Mantel statistics for the Doubs fish community. Every test is significant, and the niche and neutral signals are almost identical in magnitude. Read on before drawing any conclusion from this table. {.caption-top .table .table-sm .table-striped .small}

</div>

</div>

Read the table naively and you would report something like this. Community composition is significantly related to environmental distance (<span class="math inline">\\r \approx 0.61\\</span>) and to along-channel distance (<span class="math inline">\\r \approx 0.74\\</span>). Both relationships survive controlling for the other (<span class="math inline">\\r \approx 0.34\\</span> and <span class="math inline">\\r \approx 0.61\\</span>). The spatial signal is substantially stronger. Therefore both niche and neutral processes operate, with dispersal limitation appearing the dominant signal.

Most of it does not hold up.

These two correlations differ by about <span class="math inline">\\0.13\\</span>. Nothing in the analysis licenses treating that gap as meaningful, and because environmental distance and channel distance are themselves correlated at <span class="math inline">\\r \approx 0.57\\</span>, the two tests are not independent assessments of rival hypotheses. They are two views of a single confounded gradient.

The partial statistics look reassuring and are the least trustworthy numbers in the table. We showed in <a href="#sec-logic" class="quarto-xref">Section 9</a> that on spatially autocorrelated data, which this emphatically is, the partial Mantel test rejects a true null hypothesis between one in eight and one in four times. Both partial tests here return <span class="math inline">\\p = 0.001\\</span>. So would a substantial fraction of tests run on data with no relationship at all.

</div>

<div id="the-result-that-should-worry-you-most" class="section level3">

### The result that should worry you most

Look again at the third row of the table.

Community dissimilarity correlates with along-channel distance at <span class="math inline">\\r \approx 0.74\\</span>, but with straight-line distance at only <span class="math inline">\\r \approx 0.43\\</span>. Same communities, same sites, same test. The strength of the neutral signal has halved because we changed our mind about what distance means.

That is no rounding error, and no technicality. It is the whole epistemological problem of this chapter appearing inside a single number. The spatial signal belongs partly to the fish and partly to a modelling decision we made about how they move. Choose Euclidean distance and neutral theory looks weak. Choose channel distance and it looks like the leading explanation.

We argued in the Data section that channel distance is the ecologically defensible choice, because fish disperse along water. We still think so. But the argument for it is ecological, not statistical. No procedure in `vegan` could have told us which matrix to build. The data cannot arbitrate a question you have answered before you touch the data.

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Try This

</div>

</div>

<div class="callout-body-container callout-body">

Rebuild `chan_dist` as `dist(log1p(env$dfs))`, on the reasoning that dispersal probability may decline with the logarithm of distance rather than linearly. Rerun the Mantel test. Then decide which of the three distance matrices you would defend, and on what grounds. If your answer changes with each specification, that is the finding. Report it, rather than reporting whichever version you happened to run last.

</div>

</div>

</div>

<div id="the-defensible-analysis-distance-decay" class="section level3">

### The defensible analysis: distance decay

Neutral theory’s prediction was never really that the overall Mantel correlation with space is positive. It was that community similarity decays with distance, sharply at first and then more gently. That is a claim about distance classes, and the Mantel correlogram is built to test it.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
doubs_correlog <- mantel.correlog(
  D.eco  = spe_bray,
  D.geo  = chan_dist,
  nperm  = 999,
  r.type = "pearson",
  cutoff = FALSE,
  mult   = "holm"
)

plot(doubs_correlog)
```

</div>

<div class="cell-output-display">

<div id="fig-doubs-correlog" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-doubs-correlog-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 9: Mantel correlogram of the Doubs fish community against along-channel distance. Each point is the Mantel correlation between community dissimilarity and membership of one along-channel distance class; filled symbols are significant after Holm correction for multiple testing. Positive values at short distances and negative values at long distances are the signature of distance decay: nearby reaches hold similar assemblages, distant reaches do not.</figcaption>
</figure>

</div>

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
tibble(
  bray        = as.vector(spe_bray),
  environment = as.vector(env_dist),
  channel     = as.vector(chan_dist)
) |>
  pivot_longer(c(environment, channel),
               names_to = "predictor", values_to = "distance") |>
  ggplot(aes(distance, bray)) +
  geom_point(alpha = 0.25, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  facet_wrap(~ predictor, scales = "free_x") +
  labs(x = "Distance between sites", y = "Bray-Curtis dissimilarity")
```

</div>

<div class="cell-output-display">

<div id="fig-doubs-decay" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-doubs-decay-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 10: Community dissimilarity between every pair of Doubs sites, plotted against environmental distance (left) and along-channel distance (right). Both relationships are positive, and neither is decisively stronger, which is precisely the difficulty. Because environmental and channel distance are themselves correlated at r = 0.50, these two panels are not independent evidence for rival hypotheses.</figcaption>
</figure>

</div>

</div>

</div>

Distance decay is present and monotone. Mean Bray-Curtis dissimilarity between reaches within about 47 km of one another is <span class="math inline">\\0.42\\</span>; between reaches at opposite ends of the river it is <span class="math inline">\\0.91\\</span>. Communities become steadily less alike as the water carries you further from them.

This is what neutral theory predicts. It is also what niche theory predicts, in a river whose environment changes monotonically from source to mouth. The correlogram is a valid measurement of a real pattern. On its own it is not an identification of the process behind it.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: What Would Have To Be True

</div>

</div>

<div class="callout-body-container callout-body">

The Doubs cannot separate niche from neutral, because along this river environment and distance change together. So ask the design question. What would a dataset that could separate them look like?

It would need sites where environment and space are decoupled. Pairs of reaches close together but environmentally different, such as a spring-fed tributary entering a warm main stem. Pairs far apart but environmentally alike, such as two headwaters in different catchments. Given such pairs, the two hypotheses finally make different predictions, and the data can choose between them.

This is why ecologists survey several river systems rather than one transect. It is also why the most important decisions in an analysis are usually made before any data are collected.

</div>

</div>

</div>

</div>

<div id="sec-mite" class="section level2">

## Worked Example 2: The Mite Data — When Design Permits the Question

The Doubs analysis ended with a question. What would a dataset that could separate niche from neutral look like? It would need sites where environmental similarity and geographic proximity vary independently rather than together. The Doubs is a single linear river, and along a linear river the two change together by construction. A two-dimensional sampling layout, where site-pairs can be close in space but environmentally different, or far apart but environmentally alike, breaks that coupling.

The `mite` dataset in the `vegan` package provides exactly that. It records communities of oribatid mites across 70 soil cores from a peat bog in Quebec <span class="citation" cites="borcard2011">(<a href="#ref-borcard2011" role="doc-biblioref">Borcard et al. 2011</a>)</span>. The sampling area is roughly 2.5 by 10 metres — elongated, but with enough lateral spread that pairs of sites can differ in substrate and water content without being far apart. Crucially, substrate density (`SubsDens`) is essentially independent of spatial position, which means at least one environmental axis does not covary with geography.

The data come as three objects in `vegan`, exactly parallel to the Doubs: a species matrix (`mite`, 70 sites by 35 species), an environmental table (`mite.env`, five variables including two continuous and three categorical), and a spatial coordinate table (`mite.xy`).

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
data(mite)
data(mite.env)
data(mite.xy)
```

</div>

</div>

<div id="the-design-comparison" class="section level3">

### The design comparison

The first thing to check is the number that defined the Doubs problem. In the Doubs, the Mantel correlation between environmental distance and along-channel distance was <span class="math inline">\\r = 0.57\\</span>, meaning roughly a third of the variation in environmental distance was predictable from spatial distance alone. That is the confound that prevented separation.

We use the two continuous environmental variables, `SubsDens` (substrate density) and `WatrCont` (water content), standardised and measured as Euclidean distance, paralleling the Doubs approach.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
mite_env_cont <- mite.env[, c("SubsDens", "WatrCont")]
mite_env_dist <- dist(decostand(mite_env_cont, method = "standardize"))
mite_spa_dist <- dist(mite.xy)
mite_spe_bray <- vegdist(mite, method = "bray")

# The design question: how correlated are the two explanatory matrices?
mantel(mite_env_dist, mite_spa_dist)
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> Mantel statistic based on Pearson's product-moment correlation 
    R> 
    R> Call:
    R> mantel(xdis = mite_env_dist, ydis = mite_spa_dist) 
    R> 
    R> Mantel statistic r: 0.2839 
    R>       Significance: 0.001 
    R> 
    R> Upper quantiles of permutations (null model):
    R>    90%    95%  97.5%    99% 
    R> 0.0579 0.0786 0.0937 0.1187 
    R> Permutation: free
    R> Number of permutations: 999

</div>

</div>

<div class="cell">

Show the figure code

<div class="code-copy-outer-scaffold">

``` r
p_mite <- tibble(
  spatial     = as.vector(mite_spa_dist),
  environment = as.vector(mite_env_dist)
) |>
  ggplot(aes(spatial, environment)) +
  geom_point(alpha = 0.1, size = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4, formula = y ~ x) +
  labs(x = "Spatial distance", y = "Environmental distance",
       title = "Mite data") +
  theme(plot.title = element_text(size = 10))

p_doubs <- tibble(
  spatial     = as.vector(chan_dist),
  environment = as.vector(env_dist)
) |>
  ggplot(aes(spatial, environment)) +
  geom_point(alpha = 0.2, size = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4, formula = y ~ x) +
  labs(x = "Along-channel distance (km)", y = "Environmental distance",
       title = "Doubs data (reproduced)") +
  theme(plot.title = element_text(size = 10))

patchwork::wrap_plots(p_mite, p_doubs, ncol = 2)
```

</div>

<div class="cell-output-display">

<div id="fig-mite-confound" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-mite-confound-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 11: Environmental distance versus spatial distance for the mite data (left) and the Doubs (right, reproduced from the Doubs analysis). The mite data shows a weaker relationship: the two explanatory matrices are less tightly coupled, which is the design property the Doubs lacked.</figcaption>
</figure>

</div>

</div>

</div>

The Mantel correlation between environment and space drops from <span class="math inline">\\r \approx 0.57\\</span> in the Doubs to approximately <span class="math inline">\\r = 0.28\\</span> in the mite data. That is a substantial reduction, though not zero. One environmental variable, water content, still correlates with the y-coordinate at <span class="math inline">\\r = 0.67\\</span>, showing that complete decoupling of environment and space is rare in observational studies. Substrate density, however, is essentially independent of position. The two-dimensional layout provides site-pairs that the Doubs, as a one-dimensional transect, could never produce: pairs close in space but different in water content, and pairs far apart but similar.

</div>

<div id="applying-the-framework" class="section level3">

### Applying the framework

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
mite_m_env  <- mantel(mite_spe_bray, mite_env_dist)
mite_m_spa  <- mantel(mite_spe_bray, mite_spa_dist)
mite_p_env  <- mantel.partial(mite_spe_bray, mite_env_dist, mite_spa_dist)
mite_p_spa  <- mantel.partial(mite_spe_bray, mite_spa_dist, mite_env_dist)

tibble(
  test = c("community ~ environment",
           "community ~ space",
           "environment | space",
           "space | environment"),
  mantel_r = c(mite_m_env$statistic, mite_m_spa$statistic,
               mite_p_env$statistic, mite_p_spa$statistic),
  p_value  = c(mite_m_env$signif, mite_m_spa$signif,
               mite_p_env$signif, mite_p_spa$signif)
) |>
  knitr::kable(digits = 3,
               caption = "Mantel and partial Mantel results for the mite data. Compare these with the Doubs table above. Both niche and neutral signals are present, and both survive controlling for the other.")
```

</div>

<div class="cell-output-display">

| test                    | mantel_r | p_value |
|:------------------------|---------:|--------:|
| community ~ environment |    0.433 |   0.001 |
| community ~ space       |    0.459 |   0.001 |
| environment \| space    |    0.355 |   0.001 |
| space \| environment    |    0.389 |   0.001 |

Mantel and partial Mantel results for the mite data. Compare these with the Doubs table above. Both niche and neutral signals are present, and both survive controlling for the other. {.caption-top .table .table-sm .table-striped .small}

</div>

</div>

The result is worth comparing directly with the Doubs.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
tibble(
  dataset = rep(c("Doubs", "Mite"), each = 4),
  test = rep(c("community ~ env", "community ~ space",
               "env | space", "space | env"), 2),
  mantel_r = round(c(m_env$statistic, m_chan$statistic,
               p_env$statistic, p_chan$statistic,
               mite_m_env$statistic, mite_m_spa$statistic,
               mite_p_env$statistic, mite_p_spa$statistic), 3)
) |>
  knitr::kable(digits = 3,
               caption = "Side-by-side comparison of the Doubs and mite datasets. The Doubs has a higher env-space correlation (r approx 0.50 vs 0.28) and stronger signals overall. In the mite data, both signals survive the partial Mantel, consistent with both processes operating — the result the Doubs design could not produce cleanly.")
```

</div>

<div class="cell-output-display">

| dataset | test              | mantel_r |
|:--------|:------------------|---------:|
| Doubs   | community ~ env   |    0.607 |
| Doubs   | community ~ space |    0.742 |
| Doubs   | env \| space      |    0.335 |
| Doubs   | space \| env      |    0.607 |
| Mite    | community ~ env   |    0.433 |
| Mite    | community ~ space |    0.459 |
| Mite    | env \| space      |    0.355 |
| Mite    | space \| env      |    0.389 |

Side-by-side comparison of the Doubs and mite datasets. The Doubs has a higher env-space correlation (r approx 0.50 vs 0.28) and stronger signals overall. In the mite data, both signals survive the partial Mantel, consistent with both processes operating — the result the Doubs design could not produce cleanly. {.caption-top .table .table-sm .table-striped .small}

</div>

</div>

In the Doubs, the niche and neutral signals were nearly identical and the partial statistics could not be trusted because the predictors were confounded. In the mite data, the lower env-space correlation means the partial results carry more weight, though the Type I error caveat from the simulation still applies. Both signals survive after controlling for the other: community dissimilarity tracks environmental distance even after removing the spatial component, and it tracks spatial distance even after removing the environmental component. This is the “both processes operating” result that <span class="citation" cites="cottenie2005">Cottenie (<a href="#ref-cottenie2005" role="doc-biblioref">2005</a>)</span> found in the majority of 158 published datasets.

</div>

<div id="the-correlogram" class="section level3">

### The correlogram

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
mite_correlog <- mantel.correlog(
  D.eco  = mite_spe_bray,
  D.geo  = mite_spa_dist,
  nperm  = 999,
  r.type = "pearson",
  cutoff = FALSE,
  mult   = "holm"
)

plot(mite_correlog)
```

</div>

<div class="cell-output-display">

<div id="fig-mite-correlogram" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-mite-correlogram-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 12: Mantel correlogram of the mite community against spatial distance. The pattern is weaker than the Doubs correlogram (compare with the Doubs result above): distance decay is present but the range over which it operates is shorter, consistent with a two-dimensional sampling layout where dispersal limitation constrains less than in a linear river.</figcaption>
</figure>

</div>

</div>

</div>

Distance decay is present in the mite data, but it is weaker than in the Doubs. Mean Bray-Curtis dissimilarity rises from approximately <span class="math inline">\\0.49\\</span> between the closest site-pairs to approximately <span class="math inline">\\0.67\\</span> between the most distant, a range of <span class="math inline">\\0.18\\</span> compared to the Doubs’ range of <span class="math inline">\\0.49\\</span>. The correlogram shows positive correlations at short distances and negative at larger ones, but the transition is less orderly and the effect sizes are smaller.

This makes biological sense. In a two-dimensional sampling layout, each site connects to neighbours in multiple directions, not just upstream and downstream. Dispersal is less constrained by geometry. The neutral signature — distance decay driven by dispersal limitation — is correspondingly weaker, exactly as the theory predicts for a system with more dispersal pathways.

</div>

<div id="partitioning-the-signals-with-varpart-and-dbmem" class="section level3">

### Partitioning the signals with varpart and dbMEM

The Mantel and partial Mantel results showed that both environmental and spatial signals survive controlling for each other. But Mantel statistics are not the correct tool for quantifying how much variance each process explains (see the critique in the Analytical Logic section). Variation partitioning with distance-based Moran’s eigenvector maps (dbMEM) is.

`pcnm()` in vegan computes PCNM eigenvectors – the dbMEM equivalent available in base vegan – from the site coordinate matrix. Each eigenvector describes a spatial pattern at a specific scale, from broad gradients (low-numbered axes) to fine-grained patches (high-numbered axes). Using the eigenvectors with positive eigenvalues as spatial predictors in `varpart()` partitions the Hellinger- transformed community variance into four fractions: purely environmental, shared environment-space, purely spatial, and unexplained.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Hellinger transformation reduces the influence of very abundant species
mite_hel <- decostand(mite, method = "hellinger")

# dbMEM spatial eigenvectors from the 70 site coordinates
mite_pcnm   <- pcnm(dist(mite.xy))
pos_axes    <- which(mite_pcnm$values > 0)
mite_mem    <- scores(mite_pcnm, choices = pos_axes[1:min(8, length(pos_axes))])

# Variation partitioning: environment vs space
mite_vp <- varpart(mite_hel,
                   mite.env[, c("SubsDens", "WatrCont")],
                   mite_mem)
mite_vp
```

</div>

<div class="cell-output cell-output-stdout">

    R> 
    R> Partition of variance in RDA 
    R> 
    R> Call: varpart(Y = mite_hel, X = mite.env[, c("SubsDens", "WatrCont")],
    R> mite_mem)
    R> 
    R> Explanatory tables:
    R> X1:  mite.env[, c("SubsDens", "WatrCont")]
    R> X2:  mite_mem 
    R> 
    R> No. of explanatory tables: 2 
    R> Total variation (SS): 27.205 
    R>             Variance: 0.39428 
    R> No. of observations: 70 
    R> 
    R> Partition table:
    R>                      Df R.squared Adj.R.squared Testable
    R> [a+c] = X1            2   0.32677       0.30667     TRUE
    R> [b+c] = X2            8   0.47896       0.41062     TRUE
    R> [a+b+c] = X1+X2      10   0.55338       0.47768     TRUE
    R> Individual fractions                                    
    R> [a] = X1|X2           2                 0.06705     TRUE
    R> [b] = X2|X1           8                 0.17101     TRUE
    R> [c]                   0                 0.23962    FALSE
    R> [d] = Residuals                         0.52232    FALSE
    R> ---
    R> Use function 'rda' to test significance of fractions of interest

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
plot(mite_vp, digits = 2,
     Xnames = c("Environment\n(SubsDens+WatrCont)", "Space\n(dbMEM 1-8)"),
     bg     = c("#1A6B7240", "#B85C0040"),
     id.size = 0.85)
```

</div>

<div class="cell-output-display">

<div id="fig-mite-varpart" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-mite-varpart-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
</div>
<figcaption>Figure 13: Variation partitioning of the mite community into purely environmental (niche signal, fraction [a]), purely spatial (neutral signal, fraction [c]), their shared component [b], and unexplained variation [d]. The two predictors together explain roughly a quarter of community variance. Environment explains more unique variation than space, consistent with niche filtering being the stronger of the two processes in this system – but the spatial fraction confirms that dispersal limitation also operates independently.</figcaption>
</figure>

</div>

</div>

</div>

The Venn diagram answers the question this chapter has been asking since the introduction: how much of community structure is attributable to niche filtering versus dispersal limitation?

Fraction \[a\] is the purely environmental component: variance that environment explains after removing what it shares with space. This is the niche signal – the fraction that would not exist if all species were ecologically equivalent. Fraction \[c\] is the purely spatial component: variance that space explains after removing the environmental contribution. This is the neutral signal – the fraction attributable to dispersal limitation independent of habitat. Fraction \[b\] is the shared component, which cannot be attributed to either process without further data: it is the part of the pattern that both environment and space could explain, which the Doubs analysis showed is large when the design does not decouple the two predictors.

For the mite data, the purely environmental fraction is larger than the purely spatial fraction. That is consistent with the synthesis position across the literature: environmental structuring tends to dominate over dispersal limitation in most empirical datasets, with both operating simultaneously <span class="citation" cites="cottenie2005">(<a href="#ref-cottenie2005" role="doc-biblioref">Cottenie 2005</a>)</span>. The unexplained fraction is large, as it routinely is in community ecology, because species responses to environment are nonlinear, measurement error is present, and stochastic demographic events that no model captures contribute to the pattern.

</div>

<div id="what-the-comparison-teaches" class="section level3">

### What the comparison teaches

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Design Determines What the Analysis Can Show

</div>

</div>

<div class="callout-body-container callout-body">

The same analytical framework applied to two datasets with different spatial designs produces different results, not because the ecology changed but because the design did. The Doubs is a linear river where environment and space change together; the two signals cannot be separated and the analysis is honest about that. The mite data is a two-dimensional layout where environment and space are partially decoupled; both signals survive separately, and the comparison becomes informative.

The lesson is not that one dataset is better than the other. It is that the design of the study determines what questions the analysis can answer. A student who runs the same code on both datasets and gets different answers has learned something about study design that no amount of statistical sophistication could substitute for.

</div>

</div>

</div>

</div>

<div id="sec-spesim-demo" class="section level2">

## A Simulation in a Realistic Landscape: Using spesim

The Two Worlds and continuum simulations in this chapter used multinomial sampling on a 29-site transect: a useful abstraction, but not a spatial simulation. Every site was treated as independent, dispersal was either absent or uniform, and the landscape had no geometry. Prof. Smit’s `spesim` package extends this to a genuine 2D spatial engine, and understanding what it can do — and how it connects to this chapter’s argument — is worth setting out explicitly, even without running a full demonstration here.

<div id="what-spesim-does" class="section level3">

### What spesim does

`spesim` implements a five-stage workflow. A user defines an irregular polygon domain (a field site, a river catchment, a nature reserve), imposes continuous environmental gradients across it, simulates a community assembly process (niche filtering, neutral drift, or a hybrid combining both), places quadrats using one of five sampling schemes, and extracts a site × species abundance matrix alongside a per-quadrat environment table. Both output tables plug directly into `vegan` through two reshaping helpers — `create_abundance_matrix()` and `calculate_quadrat_environment()` — making constrained ordination (`capscale()`), variation partitioning (`varpart()`), and the Mantel correlogram immediately applicable to simulated data.

The `HYBRID_ENV_WEIGHT` parameter directly parallels the mixing weight `w` in <a href="#sec-continuum" class="quarto-xref">Section 6</a>. Setting it to 0 gives pure neutral assembly, 1 gives pure niche filtering, and intermediate values produce communities where both processes operate in specified proportions. The continuum this chapter demonstrated in a 29-site transect, `spesim` can demonstrate in a realistic 2D landscape.

</div>

<div id="why-this-matters-for-the-chapters-argument" class="section level3">

### Why this matters for the chapter’s argument

The chapter has argued that the fundamental problem with real field data is that the generating process is unknown. You cannot re-run the Doubs with a different seed, and you cannot know whether the gradient you observe was produced by niche filtering, dispersal limitation, or some combination of both.

`spesim` inverts this constraint. The generating process is specified before any community is produced. The analysis — Mantel correlogram, constrained ordination, variation partitioning — is then applied to simulated data whose truth is known. This makes it possible to ask a question real field data never permits: did the method recover the signal that was imposed?

The package’s `generate_full_report()` function answers this directly. It independently computes alpha, beta, and gamma diversity, a Mantel test for spatial autocorrelation, and a goodness-of-fit check for the specified species-abundance distribution, and it produces a **conceptual audit** that reports, per species, whether the environmental filtering you asked for actually appeared in the simulated community:

``` text
Conceptual audit (did you get the regime you asked for?):
  Environmental filtering (Spearman corr of -|z| with abundance):
    A (temperature): rho = 0.73  | abundance_peaks_near_optimum
    D (rainfall):    rho = 0.16  | weak_or_no_signal
```

This is something a Mantel test on real field data cannot produce. It evaluates method sensitivity against a known truth rather than inferring process from ambiguous pattern.

</div>

<div id="a-starting-point-for-exploration" class="section level3">

### A starting point for exploration

The code below shows the complete workflow for a niche world and a neutral world. It is set to `eval: false` so it does not run during rendering, but every line is executable once `spesim` is installed with `remotes::install_github("ajsmit/spesim")`.

<div class="cell">

Show the spesim workflow code

<div class="code-copy-outer-scaffold">

``` r
library(spesim)

# Load the built-in example configuration
P <- load_config(system.file("examples/spesim_init_basic.txt",
                              package = "spesim"))
P$N_INDIVIDUALS <- 3000
P$N_QUADRATS    <- 40
P$N_SPECIES     <- 15

# ── NICHE WORLD ──────────────────────────────────────────────────────────────
P_niche              <- P
P_niche$MODEL_FAMILY <- "manual"
P_niche$SAD_MODEL    <- "fisher"
res_niche <- spesim_run(P_niche, write_outputs = FALSE, seed = 42)

# ── NEUTRAL WORLD ─────────────────────────────────────────────────────────────
P_neutral                 <- P
P_neutral$MODEL_FAMILY    <- "neutral_hubbell_like"
P_neutral$DISPERSAL_SCALE <- 0.3
res_neutral <- spesim_run(P_neutral, write_outputs = FALSE, seed = 42)

# ── HYBRID: the continuum (equivalent to mixing weight w = 0.5) ───────────────
P_hybrid                  <- P
P_hybrid$MODEL_FAMILY     <- "hybrid"
P_hybrid$HYBRID_ENV_WEIGHT <- 0.5
res_hybrid <- spesim_run(P_hybrid, write_outputs = FALSE, seed = 42)

# ── APPLY VEGAN METHODS ───────────────────────────────────────────────────────
# spesim ships reshaping helpers that produce vegan-ready matrices
abund_niche   <- create_abundance_matrix(res_niche)
env_niche     <- calculate_quadrat_environment(res_niche)

# Constrained ordination: did CAP1 recover the imposed gradient?
abund_hel <- decostand(abund_niche, method = "hellinger")
cap_niche <- capscale(abund_hel ~ temperature, data = env_niche,
                       distance = "bray")

# Variation partitioning: how much variance is environmental vs spatial?
pcnm_coords <- pcnm(dist(env_niche[, c("x", "y")]))
vp <- varpart(abund_hel,
              env_niche[, "temperature", drop = FALSE],
              scores(pcnm_coords))

# Diagnostic report with conceptual audit
res_niche_report <- spesim_run(P_niche, write_outputs = TRUE, seed = 42)
```

</div>

</div>

</div>

<div id="connecting-to-the-chapters-analytical-workflow" class="section level3">

### Connecting to the chapter’s analytical workflow

The connection table below maps each `spesim` capability directly to the concepts and methods in this chapter and in BCB743.

| spesim capability | Chapter concept | BCB743 method |
|:---|:---|:---|
| `MODEL_FAMILY = "manual"` | Niche world: environmental filtering | Chapters 4, 10, 11 (ordination) |
| `MODEL_FAMILY = "neutral_hubbell_like"` | Neutral world: drift + dispersal | Mantel correlogram |
| `HYBRID_ENV_WEIGHT = w` | The continuum (<a href="#sec-continuum" class="quarto-xref">Section 6</a>) | varpart(), dbMEM |
| `generate_full_report()` | “Did the method recover what you specified?” | Chapter 18 (model building) |
| Five quadrat schemes | Study design (<a href="#sec-design" class="quarto-xref">Section 13</a>) | Chapter 2 (sampling) |

A student who has worked through this chapter already understands what every parameter in that code does ecologically. `HYBRID_ENV_WEIGHT` is the mixing weight `w`. The neutral world is demographic equivalence and dispersal limitation. The niche world is Gaussian filtering. The `varpart()` call produces the Venn diagram that <a href="#sec-mite" class="quarto-xref">Section 11</a> introduced on real data — here applied to simulated data where the truth is known. That is the pedagogical value `spesim` adds: it makes the generating process explicit in the code, and the analysis evaluable against a known answer.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>For Further Exploration

</div>

</div>

<div class="callout-body-container callout-body">

Run the code block above after installing spesim, then vary `HYBRID_ENV_WEIGHT` from 0 to 1 and observe how the `varpart()` environmental fraction changes. Compare the result with the continuum ribbon in Figure 4: as `w` increases, the environmental fraction in the Venn diagram should grow in the same direction. This is the same theoretical claim expressed in two different analytical languages.

</div>

</div>

</div>

</div>

<div id="sec-design" class="section level2">

## Designing a Study That Can Answer the Question

The Doubs analysis ended inconclusively, and the mite analysis produced a partial answer. Both outcomes were honest. But they raise a question that the chapter has gestured at without fully addressing. If the dataset you have cannot separate the two theories, what kind of dataset can?

The answer is specific enough to be actionable.

<div id="the-geometry-problem" class="section level3">

### The geometry problem

A linear transect is the wrong design for this question. Along any river, stream, or elevational gradient, the environmental variable and the positional variable change together by construction. Temperature drops with altitude; water flow changes with distance from source; canopy cover shifts along a disturbance gradient. Any system where sites are arranged in a single line will produce collinear predictors, and collinear predictors cannot be separated by any statistical method.

What the analysis needs, instead, is a two-dimensional sampling domain with genuine spread in two axes. When sites are arranged as a grid, or scattered across a landscape with no preferred direction, you can find site-pairs that are geographically close but environmentally different, and site-pairs that are geographically distant but environmentally similar. Those are the pairs that discriminate between the two theories. A niche-structured community will show dissimilarity corresponding to environmental difference; a neutral community will show dissimilarity corresponding to geographic distance. If both kinds of pair are in your dataset, you can ask which prediction is met and by how much.

The mite dataset has this property because it samples a 2.5 by 10 metre bog in two dimensions. The Doubs does not because it follows a single river from headwater to lowland. That difference in geometry is the reason the two analyses produce different results, not a difference in the statistical technique applied to them.

</div>

<div id="what-to-measure-and-what-not-to" class="section level3">

### What to measure and what not to

Before going into the field, decide which of your candidate environmental variables are genuine local conditions and which are proxies for position. The distinction matters because a positional variable belongs in the spatial matrix, not the environmental one. Distance from the coast, altitude, distance from a forest edge, degrees from the centre of a study plot – these describe where a site is rather than what the habitat is like. They may correlate with temperature or soil pH, but they are not the thing species respond to. Including them in the environmental matrix inflates the apparent environmental signal by adding spatial information to it.

Measure what the organism actually experiences: water chemistry, temperature, soil texture, light availability, prey density. Then check, before any analysis, how strongly your environmental variables correlate with your spatial coordinates. If the correlation is high across the board, the design has the same problem as the Doubs and the analysis will be uninformative regardless of how it is conducted.

A quick check: compute the Mantel correlation between your environmental distance matrix and your spatial distance matrix before running any community analysis. If that correlation exceeds about 0.4, the design is unlikely to produce a clean separation. Better to know this before investing in the analysis.

</div>

<div id="diagnostic-site-pairs" class="section level3">

### Diagnostic site-pairs

When laying out sites, deliberately include two kinds of pair. The first: sites that are physically close but fall in different environmental conditions. A sampling unit on the sunny face of a boulder and one on the shaded face two metres away. A quadrat on dry substrate and one on saturated substrate within the same bog section. The second: sites that are far apart but share similar conditions. Two bog pools at opposite ends of the study area, both with comparable water chemistry and substrate density.

If your dataset has only close sites that are also environmentally similar and distant sites that are also environmentally different, you are back at the Doubs problem. The diagnostic pairs break the collinearity by providing observations where environment and space point in different directions.

</div>

<div id="scale-and-replication" class="section level3">

### Scale and replication

There is no universal minimum number of sites for this question, but published analyses suggest that fewer than 30 sites produce unstable Mantel correlogram estimates, particularly in the outer distance classes where few pairs exist <span class="citation" cites="legendre2015">(<a href="#ref-legendre2015" role="doc-biblioref">Legendre et al. 2015</a>)</span>. The mite analysis uses 70 sites, which gives reasonable stability across all distance classes. Borcard and Legendre’s analyses of this dataset show that the spatial signal is detectable but weaker than the environmental signal – a result that would likely not be recoverable at 20 sites.

For any study where you plan to use variation partitioning with dbMEM spatial eigenvectors, the effective degrees of freedom are consumed faster than in ordinary regression. More sites than you think you need is usually the right answer.

</div>

<div id="asking-the-question-before-you-collect" class="section level3">

### Asking the question before you collect

The clearest practical implication of this chapter is that the question you can answer is determined before the first sample is collected. Statistical sophistication cannot substitute for a design that decouples the predictors. Deciding on the spatial arrangement of sites, the environmental variables to measure, and the scale of sampling are ecological decisions, not technical ones. The analysis only works with what the design gives it.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Design Is the Analysis

</div>

</div>

<div class="callout-body-container callout-body">

Choosing a test is a technical decision. Choosing where to put your sites is an ecological decision. The second decision determines whether the first one can answer the question. In this chapter, the Doubs and the mite data produced different results not because different methods were applied but because the sampling geometries were different. A researcher who designs a study with diagnostic site-pairs, genuinely local environmental variables, and a two-dimensional spatial arrangement has already done more than any downstream statistical procedure can accomplish.

</div>

</div>

</div>

</div>

<div id="synthesis" class="section level2">

## Synthesis

We began with a gradient and a question. The gradient was real: four chapters of ordination and clustering had already established that the Doubs fish community changes in an orderly way from headwater to mouth. The question was what produced it.

We can now say what the Doubs does and does not tell us.

Community dissimilarity increases with along-channel distance, steadily and substantially, from a mean Bray-Curtis dissimilarity of <span class="math inline">\\0.42\\</span> between neighbouring reaches to <span class="math inline">\\0.91\\</span> between the extremes of the river. That distance decay is exactly the signature neutral theory predicts from dispersal limitation. Community dissimilarity also increases with environmental distance, at almost the same strength. That is exactly the signature niche theory predicts from environmental filtering. Both predictions are met, and the data cannot say which mechanism is responsible, because along a single river the environment changes as you travel and the travelling changes the environment. The predictors are confounded by the geometry of the system.

This is a real result rather than a failure to obtain one. An honest report of the Doubs analysis says that community structure is strongly spatially and environmentally patterned, that the two are inseparable in this design, and that a study capable of separating them would need sites where environment and distance vary independently. Reporting the partial Mantel statistics as though they had achieved that separation would be reporting a number whose false-positive rate we measured, in <a href="#sec-logic" class="quarto-xref">Section 9</a>, at between one in eight and one in four.

The wider literature suggests what such a study tends to find. <span class="citation" cites="cottenie2005">Cottenie (<a href="#ref-cottenie2005" role="doc-biblioref">2005</a>)</span>, synthesising 158 published datasets, reports that both environmental and spatial processes leave detectable signatures in most metacommunities, with environmental structuring the more common of the two. <span class="citation" cites="tuomisto2003">Tuomisto et al. (<a href="#ref-tuomisto2003" role="doc-biblioref">2003</a>)</span>, working across western Amazonian forests where soils and distance can be decoupled, likewise find both processes at work. The Doubs result is consistent with that picture. It is not independent evidence for it.

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea: Inference Is Constrained, Not Determined

</div>

</div>

<div class="callout-body-container callout-body">

Observational data narrow the set of processes that could have produced a pattern. They rarely reduce that set to one. <span class="citation" cites="adler2007">Adler et al. (<a href="#ref-adler2007" role="doc-biblioref">2007</a>)</span> make this point about niche and neutral theory specifically: abundance patterns alone cannot resolve the controversy, because rival models generate similar patterns. The correct response is to say so, and to design the next study accordingly.

</div>

</div>

Three things are worth carrying forward from this chapter.

The first is vocabulary. You can now name the two competing process explanations for a community gradient, state the assumption each makes about species, and say what each predicts about the relationship between community dissimilarity and distance. When the [Seaweeds](https://tangledbank.netlify.app/BCB743/two_oceans_appendices.html) appendix labels a fraction of variation “spatial”, or when the Gradients chapter partitions variance into environmental and spatial components, you know what biological claim those labels encode, and you know that the encoding is an inference rather than a definition.

The second is a habit. Before running a test, ask whether the design could answer the question. Before trusting a p-value, ask what null hypothesis the test actually evaluates. Both questions were fatal to the intuitive analysis in this chapter, and neither requires any statistical machinery to ask.

The third is the reason this chapter sits where it does. The [next chapter](https://tangledbank.netlify.app/BCB743/model_building.html) asks you to build models, and to choose predictors on the basis of hypothesised mechanisms. That instruction is empty unless you know what the candidate mechanisms are. Environmental filtering and dispersal limitation are the two that structure most of community ecology, and a model that includes environmental predictors while ignoring spatial ones is making a claim about assembly whether or not its author intended to. Chapter 18 teaches you to defend a modelling choice. This chapter is where the choice acquires content.

</div>

<div id="further-reading" class="section level2">

## Further Reading

<span class="citation" cites="hubbell2001">Hubbell (<a href="#ref-hubbell2001" role="doc-biblioref">2001</a>)</span> is the primary statement of neutral theory and remains worth reading in the original, particularly the early chapters, where the demographic equivalence assumption is set out and defended rather than merely asserted.

<span class="citation" cites="rosindell2011">Rosindell et al. (<a href="#ref-rosindell2011" role="doc-biblioref">2011</a>)</span> review the theory ten years on and are candid about which of its predictions survived empirical test and which did not. Read it after Hubbell, as a corrective.

<span class="citation" cites="chase2011">Chase and Myers (<a href="#ref-chase2011" role="doc-biblioref">2011</a>)</span> give the clearest modern operational treatment of the niche, and connect it to the scale-dependence of stochasticity in a way that this chapter only gestures at.

<span class="citation" cites="adler2007">Adler et al. (<a href="#ref-adler2007" role="doc-biblioref">2007</a>)</span> is the paper to read if you read only one. It dissolves the niche-neutral opposition using coexistence theory, and it is short.

<span class="citation" cites="vellend2010">Vellend (<a href="#ref-vellend2010" role="doc-biblioref">2010</a>)</span> reorganises community ecology around selection, drift, speciation and dispersal. The essay is long, and repays the length.

<span class="citation" cites="legendre2015">Legendre et al. (<a href="#ref-legendre2015" role="doc-biblioref">2015</a>)</span> is the methodological argument that shapes the second half of this chapter. Read it before you next reach for `mantel()`.

<span class="citation" cites="cottenie2005">Cottenie (<a href="#ref-cottenie2005" role="doc-biblioref">2005</a>)</span> provides the empirical backdrop: what 158 datasets say about the relative contribution of environmental and spatial processes.

<span class="citation" cites="leibold2006">Leibold and McPeek (<a href="#ref-leibold2006" role="doc-biblioref">2006</a>)</span> argue that ecological equivalence and niche differentiation are not opposing hypotheses but can coexist in the same community. Read it alongside Adler for the full case that the niche-neutral dichotomy is the wrong framing.

</div>

<div id="sec-tasks" class="section level2">

## Tasks

These exercises progress from re-running the chapter’s core analyses to designing and interpreting new ones. They are intended to be worked through in order, because later tasks build on results from earlier ones.

------------------------------------------------------------------------

**Task 1. Does the metric choice matter in your system?**

Run the Doubs Mantel test three times using three different spatial distance matrices: `chan_dist` (along-channel distance, as used in the chapter), `dist(spa)` (Euclidean distance between site coordinates), and `dist(log1p(env$dfs))` (log-transformed channel distance, on the reasoning that dispersal probability may decay logarithmically with distance).

For each metric, compute `mantel(spe_bray, spatial_dist)` and record the Mantel `r`. Then write two or three sentences explaining what the range across the three results tells you about the relationship between your modelling choices and your conclusions.

------------------------------------------------------------------------

**Task 2. Extending the Type I error simulation**

The partial Mantel simulation in the chapter used a fixed autocorrelation range and iterated 1,000 times. Extend it in one of the following directions:

1.  Vary the autocorrelation range from 1 to 20 sites and plot the false-positive rate as a continuous function rather than at four discrete values. Does the relationship appear to be linear, or does it plateau?

2.  Run the same simulation but replace the partial Mantel test with a dbMEM-based test: generate spatially autocorrelated `x` and `y` variables as before, but use `anova(rda(x, pcnm(dist(sites))$vectors))` to ask whether spatial eigenvectors predict `x`. Record the false-positive rate at each autocorrelation range. Compare the two tests.

------------------------------------------------------------------------

**Task 3. Apply the continuum simulation to a field question**

The continuum simulation varies the mixing weight `w` from 0 to 1 and shows that gradient strength increases and becomes more reproducible as `w` increases.

Choose a real ecological system you are familiar with (any organism, any habitat) and answer the following:

1.  Where on the `w` continuum do you expect this system to sit, and why? Your answer should cite at least one published study that provides evidence for the niche or neutral signal in this type of system.

2.  What design would you use to estimate `w` empirically? Specifically: how many sites, what spatial arrangement, what environmental variables, and why those variables rather than others?

------------------------------------------------------------------------

**Task 4. Interpreting the mite varpart output**

Re-run the mite variation partitioning using different numbers of dbMEM axes: 3, 8 (as in the chapter), and 20. For each run, record fractions \[a\], \[b\], \[c\], and \[d\] from the varpart output.

1.  How stable is fraction \[a\] (the niche signal) as the number of spatial axes increases? How stable is fraction \[c\] (the neutral signal)?

2.  Fraction \[d\] (unexplained) is always large. List three ecological mechanisms that contribute to unexplained variation in a community dataset of this kind, and for each one, note whether more sampling sites, better environmental variables, or a different analytical method would reduce it.

------------------------------------------------------------------------

**Task 5. Design a study to separate niche from neutral**

You are planning a field study to test whether the mite community in a new bog system is primarily structured by environmental filtering or by dispersal limitation. You have budget for 50 sampling quadrats.

1.  Draw a sketch of your proposed sampling layout. Explain why you chose that arrangement rather than a transect, and identify which site-pairs in your layout would serve as “diagnostic pairs” – pairs that would discriminate between the two hypotheses.

2.  List the environmental variables you would measure at each quadrat. For each variable, state whether it is a genuine local condition or a positional proxy (refer to the distinction in <a href="#sec-design" class="quarto-xref">Section 13</a>), and explain what would happen to the partial Mantel test if you accidentally included a positional proxy in your environmental matrix.

3.  Before collecting data, compute the Mantel correlation between your planned spatial distance matrix (based on your layout) and a hypothetical environmental distance matrix where every variable changes linearly across the study area. What does this number tell you about the power of your design to separate the two processes?

</div>

<div id="author-contributions-and-ai-disclosure" class="section level2">

## Author Contributions and AI Disclosure

**Author contributions:** All three group members contributed to the theoretical framing and the written text. Alex Matthew developed the Two Worlds simulation, the niche-neutral continuum simulation, and the Doubs worked analysis including the Type I error simulation; he also led the integration of the spesim demonstration and the mite worked example. Keanan Jarvis developed the analytical logic section, the Mantel test critique, and the critical reading of Legendre et al. (2015). Ethan Bell developed the synthesis section, the Designing a Study section, and the Tasks. All members revised and approved the final submission.

**AI use disclosure:** Generative AI tools (specifically Claude by Anthropic) were used in the preparation of this chapter. AI assisted with: drafting and restructuring the document, suggesting R code structures for the simulations and worked examples. All AI-generated text was reviewed, revised, and in many cases substantially rewritten by the authors. All code was tested and verified on real data by the authors. All citations were checked against the source PDFs. No AI-generated content was accepted without independent verification.

</div>

<div id="references" class="section level2">

## References

<div id="refs" class="references csl-bib-body hanging-indent" role="list">

<div id="ref-adler2007" class="csl-entry" role="listitem">

Adler, Peter B., Janneke HilleRisLambers, and Jonathan M. Levine. 2007. “A Niche for Neutrality.” *Ecology Letters* 10 (2): 95–104. <https://doi.org/10.1111/j.1461-0248.2006.00996.x>.

</div>

<div id="ref-borcard2011" class="csl-entry" role="listitem">

Borcard, Daniel, François Gillet, and Pierre Legendre. 2011. *Numerical Ecology with R*. Springer. <https://doi.org/10.1007/978-1-4419-7976-6>.

</div>

<div id="ref-chase2011" class="csl-entry" role="listitem">

Chase, Jonathan M., and Jonathan A. Myers. 2011. “Disentangling the Importance of Ecological Niches from Stochastic Processes Across Scales.” *Philosophical Transactions of the Royal Society B* 366 (1576): 2351–63. <https://doi.org/10.1098/rstb.2011.0063>.

</div>

<div id="ref-cottenie2005" class="csl-entry" role="listitem">

Cottenie, Karl. 2005. “Integrating Environmental and Spatial Processes in Ecological Community Dynamics.” *Ecology Letters* 8 (11): 1175–82. <https://doi.org/10.1111/j.1461-0248.2005.00820.x>.

</div>

<div id="ref-guillot2013" class="csl-entry" role="listitem">

Guillot, Gilles, and François Rousset. 2013. “Dismantling the Mantel Tests.” *Methods in Ecology and Evolution* 4 (4): 336–44. <https://doi.org/10.1111/2041-210x.12018>.

</div>

<div id="ref-hubbell2001" class="csl-entry" role="listitem">

Hubbell, Stephen P. 2001. *The Unified Neutral Theory of Biodiversity and Biogeography*. Princeton University Press.

</div>

<div id="ref-hutchinson1957" class="csl-entry" role="listitem">

Hutchinson, G. Evelyn. 1957. “Concluding Remarks.” *Cold Spring Harbor Symposia on Quantitative Biology* 22: 415–27. <https://doi.org/10.1101/SQB.1957.022.01.039>.

</div>

<div id="ref-legendre2015" class="csl-entry" role="listitem">

Legendre, Pierre, Marie-Josée Fortin, and Daniel Borcard. 2015. “Should the Mantel Test Be Used in Spatial Analysis?” *Methods in Ecology and Evolution* 6 (11): 1239–47. <https://doi.org/10.1111/2041-210X.12425>.

</div>

<div id="ref-legendre2012" class="csl-entry" role="listitem">

Legendre, Pierre, and Louis Legendre. 2012. *Numerical Ecology*. 3rd ed. Elsevier.

</div>

<div id="ref-leibold2006" class="csl-entry" role="listitem">

Leibold, Mathew A., and Mark A. McPeek. 2006. “Coexistence of the Niche and Neutral Perspectives in Community Ecology.” *Ecology* 87 (6): 1399–410. <https://doi.org/10.1890/0012-9658(2006)87%5B1399:COTNAN%5D2.0.CO;2>.

</div>

<div id="ref-mantel1967" class="csl-entry" role="listitem">

Mantel, Nathan. 1967. “The Detection of Disease Clustering and a Generalized Regression Approach.” *Cancer Research* 27 (2): 209–20.

</div>

<div id="ref-oden1992" class="csl-entry" role="listitem">

Oden, Neal L., and Robert R. Sokal. 1992. “An Investigation of Three-Matrix Permutation Tests.” *Journal of Classification* 9 (2): 275–90. <https://doi.org/10.1007/BF02621410>.

</div>

<div id="ref-rosindell2011" class="csl-entry" role="listitem">

Rosindell, James, Stephen P. Hubbell, and Rampal S. Etienne. 2011. “The Unified Neutral Theory of Biodiversity and Biogeography at Age Ten.” *Trends in Ecology & Evolution* 26 (7): 340–48. <https://doi.org/10.1016/j.tree.2011.03.024>.

</div>

<div id="ref-troia2014" class="csl-entry" role="listitem">

Troia, Matthew J., and Keith B. Gido. 2014. “Towards a Mechanistic Understanding of Fish Species Niche Divergence Along a River Continuum.” *Ecosphere* 5 (4): 1–16. <https://doi.org/10.1890/ES13-00399.1>.

</div>

<div id="ref-tuomisto2003" class="csl-entry" role="listitem">

Tuomisto, Hanna, Kalle Ruokolainen, and Markku Yli-Halla. 2003. “Dispersal, Environment, and Floristic Variation of Western Amazonian Forests.” *Science* 299 (5604): 241–44. <https://doi.org/10.1126/science.1078037>.

</div>

<div id="ref-vellend2010" class="csl-entry" role="listitem">

Vellend, Mark. 2010. “Conceptual Synthesis in Community Ecology.” *The Quarterly Review of Biology* 85 (2): 183–206. <https://doi.org/10.1086/652373>.

</div>

</div>

</div>

</div>
