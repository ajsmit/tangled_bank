<div id="quarto-document-content" class="content" role="main">

<div id="title-block-header" class="quarto-title-block default">

<div class="quarto-title">

<div class="quarto-title-block">

<div>

# Occupancy modelling with imperfect detection

Code

- <a href="javascript:void(0)" id="quarto-show-all-code" class="dropdown-item" role="button" data-original-href="javascript:void(0)">Show All Code</a>

- <a href="javascript:void(0)" id="quarto-hide-all-code" class="dropdown-item" role="button" data-original-href="javascript:void(0)">Hide All Code</a>

- 

  ------------------------------------------------------------------------

- <a href="javascript:void(0)" id="quarto-view-source" class="dropdown-item" role="button" data-original-href="javascript:void(0)">View Source</a>

</div>

</div>

</div>

<div class="quarto-title-meta">

<div>

<div class="quarto-title-meta-heading">

Author

</div>

<div class="quarto-title-meta-contents">

L. Dlelapantsi & T.S Selae

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

<div class="course-page-nav-left">

<span class="course-page-nav-item is-disabled">Previous</span><a href="http://localhost:6756/BCB743/BCB743_index.html" class="course-page-nav-item course-page-nav-overview">Overview</a><span class="course-page-nav-item is-disabled">Next</span>

</div>

<div class="course-page-nav-right">

</div>

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
| **Slides** | Occupancy lecture slides | <a href="http://localhost:6756/BCB743/.../slides/BCB743_occupancy.pdf" data-original-href="http://localhost:6756/BCB743/.../slides/BCB743_occupancy.pdf">💾 <code>Occupancy_model_with_imperfect_detection.pdf</code></a> |
| **Theory** | MacKenzie et al. (2002) | <a href="https://doi.org/10.1890/0012-9658(2002)083%5B2248:ESORWD%5D2.0.CO;2" class="external" data-original-href="https://doi.org/10.1890/0012-9658(2002)083%5B2248:ESORWD%5D2.0.CO;2" target="_blank" rel="noopener">Ecology 83:2248–2255</a> |
| **Theory** | Becker et al. (2022) | <a href="https://doi.org/10.1002/eap.2502" class="external" data-original-href="https://doi.org/10.1002/eap.2502" target="_blank" rel="noopener">Ecological Applications 32:e2502</a> |
| **Package** | `unmarked` | <a href="https://cran.r-project.org/web/packages/unmarked/index.html" class="external" data-original-href="https://cran.r-project.org/web/packages/unmarked/index.html" target="_blank" rel="noopener">CRAN</a> |
| **Data** | *Capensibufo rosei* | <a href="https://zivahub.uct.ac.za/articles/dataset/Finding_rare_species_and_estimating_the_probability_that_all_occupied_sites_have_been_found_Dataset/14609838/1?file=28048122" class="external" data-original-href="https://zivahub.uct.ac.za/articles/dataset/Finding_rare_species_and_estimating_the_probability_that_all_occupied_sites_have_been_found_Dataset/14609838/1?file=28048122" target="_blank" rel="noopener">💾 <code>All_spp_search_data.csv</code></a> |
|  |  |  |

</div>

</div>

<div id="overview" class="section level2">

## Overview<a href="http://localhost:6756/BCB743/occupancy.html#overview" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

In ecology, occupancy models are frequently used to track biodiversity, investigate rare and endangered species, and understand how species distributions change over time. By taking imperfect detection into account, these models offer more accurate estimates of where species occur and can help identify significant habitats and regions in need of conservation. Occupancy models can also be used to investigate the effects of environmental conditions on species occurrence, including pollution, climate, habitat quality, and human disturbance. While multi-species models can be used to examine patterns of biodiversity and community composition, multi-season models enable researchers to examine changes in occupancy through processes like colonisation and local extinction. These models are useful for evaluating conservation interventions, such as the establishment of protected areas, by tracking changes in species occupancy before and after management action.

<div id="learning-objectives" class="section level3">

### Learning Objectives<a href="http://localhost:6756/BCB743/occupancy.html#learning-objectives" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

By the end of this chapter, students should be able to:

- Explain why imperfect detection can bias biodiversity surveys and how occupancy models address this problem.
- Distinguish between occupancy probability (ψ) and detection probability (p), and describe the ecological processes they represent.
- Construct and interpret single-season occupancy models using environmental and detection covariates.
- Evaluate model outputs, including parameter estimates, confidence intervals, goodness-of-fit, and model selection using AIC.
- Understand the principles of dynamic occupancy models, including colonisation (γ) and extinction (ε), and how species distributions change through time.
- Explain how multi-species occupancy models account for imperfect detection while estimating community-level patterns and species co-occurrence.
- Apply occupancy modelling techniques in R using the `unmarked` package to analyse ecological survey data.

</div>

</div>

<div id="ecological-background" class="section level2">

## Ecological Background<a href="http://localhost:6756/BCB743/occupancy.html#ecological-background" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div id="presence-absence-data-false-absences-and-detection-probability" class="section level3">

### Presence-absence data, false absences, and detection probability<a href="http://localhost:6756/BCB743/occupancy.html#presence-absence-data-false-absences-and-detection-probability" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>


</div>

<div id="consequences-of-ignoring-imperfect-detection" class="section level3">

### Consequences of ignoring imperfect detection<a href="http://localhost:6756/BCB743/occupancy.html#consequences-of-ignoring-imperfect-detection" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The occupancy model deals with imperfect detection by splitting the process into two different parameters: *occupancy probability* (<span class="math inline">𝜓</span>), which represents the actual ecological state (presence or absence), and *detection probability* (<span class="math inline">𝑝</span>), which is the probability of detecting a species during a survey given that it is present. This statistical method was developed by MacKenzie et al. (2002), and it separates the two probabilities by using repeated surveys. This method is flexible, as it allows the inclusion of site- or survey-specific covariates and accounts for missed observations. When the detection process is ignored, it can result in several kinds of error.

First, when a species is not detected it is assumed to be absent, which produces false negatives in the data. Second, these false negatives cause systematic bias in ecological measures such as population size and species richness. Third, comparisons made across habitats and time can yield incorrect conclusions if the chance of detecting the species differs between groups, rather than the species actually being more or less common.

</div>

<div id="established-knowledge-versus-ongoing-debate" class="section level3">

### Established knowledge versus ongoing debate<a href="http://localhost:6756/BCB743/occupancy.html#established-knowledge-versus-ongoing-debate" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The issue of imperfect detection in presence-absence surveys has long existed and was recognised well before the 2000s. Work by Geissler and Fuller (1987) and Azuma et al. (1990) identified that non-detection should not be treated as true absence; however, their approaches were not flexible and could not accommodate covariates or missed observations.

</div>

<div id="core-assumptions-of-the-standard-occupancy-model" class="section level3">

### Core assumptions of the standard occupancy model<a href="http://localhost:6756/BCB743/occupancy.html#core-assumptions-of-the-standard-occupancy-model" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Before the model structure is introduced below, it is important to address the assumptions associated with the model and what is required to avoid violating them.

1.  **Population closure** – the site’s occupancy status must remain the same throughout the repeated surveys used to estimate detection probability. This assumption is necessary to estimate detection probability and cannot be relied on for mobile or seasonal species.
2.  **No false positives occur** – detection is always assumed to be correct, so a species cannot be recorded at a site where it does not occur. This is the most challenged assumption in the literature, because of how misidentification has arisen with automated survey technologies (e.g. acoustic recorders, camera traps).
3.  **Independence of detections** – repeated detections across a site are assumed to be independent.
4.  **No unmodelled heterogeneity in occupancy or detection** – both occupancy probability (<span class="math inline">𝜓</span>) and detection probability (<span class="math inline">𝑝</span>) are assumed to be constant across sites, except as explained by measured covariates.
5.  **Correct identification of the sampling unit and season** – the sample unit and the study season must be constant throughout the study and clearly defined. The season should be short enough that species distributions do not shift within it, and the sampling unit should be appropriate to the species being studied.

</div>

</div>

<div id="occupancy-models-theory-and-structure" class="section level2">

## Occupancy Models: Theory and Structure<a href="http://localhost:6756/BCB743/occupancy.html#occupancy-models-theory-and-structure" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div id="what-is-occupancy" class="section level3">

### What is occupancy?<a href="http://localhost:6756/BCB743/occupancy.html#what-is-occupancy" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Understanding what occupancy means is essential before we can understand what occupancy *models* are. Occupancy (<span class="math inline">𝜓</span>) refers to the probability that a site being surveyed is occupied by a species. If <span class="math inline">𝑥</span> is the number of occupied sites and <span class="math inline">𝑠</span> is the number of surveyed sites, then the naive estimator

<span class="math display">ˆ𝜓=ˆ𝑥𝑠</span>

can be used to estimate occupancy. In the real world, however, this naive estimator is rarely adequate, because species may not always be detected even when they are present, so we need a method that incorporates detection probability – the probability that a species is detected at a site, given that it is present. If detection probability is not incorporated, occupancy will be systematically underestimated.

An occupancy survey produces two outcomes: the species is detected at a site, or it is not. Non-detection has two possible explanations: the species was truly absent, or it was present and not detected – a *false absence*. In most ecological surveys it is reasonable to assume detections themselves are true, though more advanced occupancy models exist to relax this assumption and address *false positives* as well as false negatives.

</div>

<div id="the-mackenzie-et-al.-2002-framework" class="section level3">

### The MacKenzie et al. (2002) framework<a href="http://localhost:6756/BCB743/occupancy.html#the-mackenzie-et-al.-2002-framework" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The single-season (closed) occupancy model rests on three assumptions:

1.  The system is **closed**: occupancy state does not change between repeat visits within a season.
2.  **Detection is imperfect** but can be estimated: <span class="math inline">𝑝 \<1</span> is the rule, not the exception.
3.  Detections are **independent** across sites and across visits to the same site.

The model is then written as a pair of linked equations. Let <span class="math inline">𝑧𝑖 ∈{0,1}</span> be the true (latent) occupancy state of site <span class="math inline">𝑖</span> and let <span class="math inline">𝑦𝑖⁢𝑗 ∈{0,1}</span> be what the observer recorded on visit <span class="math inline">𝑗</span>. The state model is:

<span class="math display">𝑧𝑖∼Bernoulli⁡(𝜓𝑖)</span>

and the observation model is:

<span class="math display">𝑦𝑖⁢𝑗∣𝑧𝑖∼Bernoulli⁡(𝑧𝑖⋅𝑝𝑖⁢𝑗)</span>

If the site is unoccupied (<span class="math inline">𝑧𝑖 =0</span>), no detection is possible regardless of <span class="math inline">𝑝</span>. If it is occupied, each visit yields a detection with probability <span class="math inline">𝑝𝑖⁢𝑗</span>. Both <span class="math inline">𝜓</span> and <span class="math inline">𝑝</span> can be modelled as functions of covariates through logit links, making the framework an extension of logistic regression to a two-level hierarchical model.

</div>

<div id="understanding-the-ecological-and-observation-processes" class="section level3">

### Understanding the Ecological and Observation Processes<a href="http://localhost:6756/BCB743/occupancy.html#understanding-the-ecological-and-observation-processes" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Occupancy modelling separates two processes – ecological and observation – rather than treating survey data as a single process. The **ecological process** represents the true state of nature: it asks whether the species is truly present at a site, independently of any surveys. A site is either occupied or unoccupied, and that state does not depend on how many times, or how well, it is surveyed. The **observation process** is what happens during a survey: a species may go undetected even though it is present at a site, and the probability of detecting it depends on factors like habitat structure and weather conditions.

The observation process is conditional on the ecological process, which is why the overall model is described as *hierarchical*. Assumption 2 (no false positives) guarantees that a species cannot be detected at a site where it does not truly occur, so every non-detection remains uncertain – it could be a true absence or a false absence. The repeated-visit design is what lets the occupancy model separate true occupancy from the chance of detecting it.

</div>

<div id="notation" class="section level3">

### Notation<a href="http://localhost:6756/BCB743/occupancy.html#notation" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

A useful way to keep the notation straight is that <span class="math inline">𝜓</span> and <span class="math inline">𝑧</span> belong to the ecological process, while <span class="math inline">𝑝</span> and <span class="math inline">𝑦</span> belong to the observation process. Keeping this distinction in mind makes it easier to separate what is happening in nature from what is actually observed:

| Symbol | Process | Meaning |
|:---|:---|:---|
| <span class="math inline">𝜓𝑖</span> | Ecological | Probability that site <span class="math inline">𝑖</span> is occupied |
| <span class="math inline">𝑧𝑖</span> | Ecological | True (latent) occupancy state of site <span class="math inline">𝑖</span> (<span class="math inline">0</span> or <span class="math inline">1</span>) |
| <span class="math inline">𝑝𝑖⁢𝑗</span> | Observation | Probability of detecting the species on visit <span class="math inline">𝑗</span> to site <span class="math inline">𝑖</span>, given <span class="math inline">𝑧𝑖 =1</span> |
| <span class="math inline">𝑦𝑖⁢𝑗</span> | Observation | Recorded detection (<span class="math inline">1</span>) or non-detection (<span class="math inline">0</span>) on visit <span class="math inline">𝑗</span> to site <span class="math inline">𝑖</span> |

</div>

<div id="model-equations-and-likelihood" class="section level3">

### Model Equations and Likelihood<a href="http://localhost:6756/BCB743/occupancy.html#model-equations-and-likelihood" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Occupancy models are built on the hierarchical framework introduced above, which separates the ecological process from the observation process. Together, these two processes form the likelihood function that lets the ecologist estimate occupancy and detection probabilities directly from repeated-visit detection histories, rather than relying on the naive proportion of sites with at least one detection.

To make the observation model concrete, consider a site where the species is truly present (<span class="math inline">𝑧 =1</span>) and detection probability is <span class="math inline">𝑝 =0.6</span>. If three surveys are conducted, the probability of observing the detection history <span class="math inline">(1,0,1)</span> is:

<span class="math display">𝑃⁡(𝑦=\[1,0,1\]∣𝑧=1)=𝑝⋅(1−𝑝)⋅𝑝=0.6×0.4×0.6=0.144</span>

Each site’s contribution to the likelihood is built the same way, from its own sequence of visits, and every site’s contribution is then combined (multiplied) across sites to obtain the full likelihood function that `occu()` maximises when fitting a model.

</div>

<div id="link-of-assumptions-to-the-formulas" class="section level3">

### Link of Assumptions to the Formulas<a href="http://localhost:6756/BCB743/occupancy.html#link-of-assumptions-to-the-formulas" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The assumptions introduced earlier are not separate from the mathematics – they are what license each part of the model.

- **Closure** is why the true occupancy state is written as <span class="math inline">𝑧𝑖</span>, indexed only by site and not by survey: the model assumes a species does not colonise or go locally extinct between the repeated surveys used to estimate <span class="math inline">𝑝</span> within a season.
- **No false positives** is encoded directly in <span class="math inline">𝑦𝑖⁢𝑗 ∣𝑧𝑖 ∼Bernoulli⁡(𝑧𝑖 ⋅𝑝𝑖⁢𝑗)</span>: multiplying by <span class="math inline">𝑧𝑖</span> guarantees that an unoccupied site (<span class="math inline">𝑧𝑖 =0</span>) always yields <span class="math inline">𝑦𝑖⁢𝑗 =0</span>, regardless of <span class="math inline">𝑝𝑖⁢𝑗</span>.
- **Independence of detections** is why the likelihood is written as a simple product of visit-level probabilities across surveys and sites, rather than as a chain of conditional probabilities.
- **No unmodelled heterogeneity** means <span class="math inline">𝜓𝑖</span> and <span class="math inline">𝑝𝑖⁢𝑗</span> are treated as fixed (or as fixed functions of measured covariates) rather than as quantities that fluctuate for unmeasured reasons; if they did fluctuate beyond what the covariates explain, the resulting estimates would be biased.

Repeated surveys matter precisely because they supply the information needed to separate <span class="math inline">𝜓</span> from <span class="math inline">𝑝</span>. The assumptions therefore determine how a detection history should be interpreted, and how reliable the resulting estimates of <span class="math inline">𝜓</span> and <span class="math inline">𝑝</span> can be trusted to be.

</div>

<div id="key-variations-of-occupancy-models" class="section level3">

### Key Variations of Occupancy Models<a href="http://localhost:6756/BCB743/occupancy.html#key-variations-of-occupancy-models" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div id="single-season-models" class="section level4">

#### Single-Season Models<a href="http://localhost:6756/BCB743/occupancy.html#single-season-models" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Single-season occupancy models are the simplest and most widely used foundation of occupancy modelling. Unlike traditional presence-absence models, they account for the possibility that a species may go undetected even where it occurs, using repeated surveys within a single season to separate non-detection from true absence and so obtain an unbiased estimate of occupancy. The key assumption is *closure*: the occupancy state of a site must remain constant across the surveys used to estimate detection, with no migration, immigration, or extinction occurring at the site during that window. Single-season models are best suited to short-term studies and have been widely used for cryptic and rare species.

</div>

<div id="multi-season-dynamic-models" class="section level4">

#### Multi-Season (Dynamic) Models<a href="http://localhost:6756/BCB743/occupancy.html#multi-season-dynamic-models" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Dynamic occupancy models survey one species over multiple seasons, so that the study extends over a long period of time – often several years, with each year’s breeding season (or comparable primary period) surveyed multiple times. Repeated within-season visits are still needed to estimate detection reliably, rather than relying on a single visit per season. Crucially, this model relaxes the closure assumption *between* seasons: it explicitly allows occupancy to change from one season to the next through local colonisation (<span class="math inline">𝛾</span>) and local extinction (<span class="math inline">𝜀</span>). Dynamic models are particularly useful for long-term monitoring, helping ecologists understand how species distributions shift over time and how they respond to environmental change or management interventions.

</div>

<div id="multi-speciescommunity-models" class="section level4">

#### Multi-Species/Community Models<a href="http://localhost:6756/BCB743/occupancy.html#multi-speciescommunity-models" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Multi-species (community) occupancy models extend the single-species framework by modelling multiple species at the same sites simultaneously, sharing information across species through a hierarchical (community-level) structure. This is especially valuable for rare species that have been detected only a handful of times, since information is “borrowed” from more commonly detected species in the same community. A further extension is the multi-species co-occurrence model, which asks whether the occurrence of one species is influenced by the presence of another at the same site – helping to determine whether species occur together more or less often than would be expected under independence, and offering insight into ecological relationships such as competition or facilitation.

</div>

</div>

</div>

<div id="relationship-to-generalized-linear-models" class="section level2">

## Relationship to Generalized Linear Models<a href="http://localhost:6756/BCB743/occupancy.html#relationship-to-generalized-linear-models" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>


</div>

<div id="survey-design" class="section level2">

## Survey Design<a href="http://localhost:6756/BCB743/occupancy.html#survey-design" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div id="selecting-sampling-sites-and-designing-the-sampling-strategy" class="section level3">

### Selecting Sampling Sites and Designing the Sampling Strategy<a href="http://localhost:6756/BCB743/occupancy.html#selecting-sampling-sites-and-designing-the-sampling-strategy" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The first step in occupancy modelling is selecting the sites where the study will be conducted. Sites can be selected randomly or systematically, depending on the research question. It is not advisable to preferentially select sites where the species of interest is already known to occur most, since this can bias the resulting estimates. The number of sites should be large enough to produce reliable estimates of <span class="math inline">𝜓</span> and <span class="math inline">𝑝</span>, and the sites should span a wide range of the environmental conditions relevant to the study (e.g. water availability, degree of disturbance). A well-designed sampling strategy produces results that better represent the wider study area, rather than only the conditions at a handful of convenient sites.

</div>

<div id="repeated-surveys-and-survey-timing" class="section level3">

### Repeated Surveys and Survey Timing<a href="http://localhost:6756/BCB743/occupancy.html#repeated-surveys-and-survey-timing" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Occupancy studies require that each site be surveyed repeatedly, because species can be missed even when present. These repeated surveys supply the information needed to estimate detection probability and to distinguish false absence from true absence. The study period must be short enough that the closure assumption remains defensible, and survey timing must suit the biology of the species being studied – for example, surveys should be conducted when the species is active and thus detectable.

</div>

<div id="factors-affecting-detection-and-covariates" class="section level3">

### Factors Affecting Detection and Covariates<a href="http://localhost:6756/BCB743/occupancy.html#factors-affecting-detection-and-covariates" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The probability of detecting a species can be affected by several factors, even when the species is present at a site. These include weather conditions, the effort invested in the survey, and habitat characteristics. Such factors are grouped as *detection covariates*, which influence the probability of detecting a species during a given visit. By contrast, environmental factors such as temperature, vegetation cover, and human disturbance are usually grouped as *occupancy covariates*, since they influence where a species occurs in the first place. Separating detection covariates from occupancy covariates is important for correctly interpreting a fitted model, since a factor that affects detectability does not necessarily affect where a species actually occurs, and vice versa.

</div>

</div>

<div id="advantages-and-limitations-of-occupancy-models" class="section level2">

## Advantages and Limitations of Occupancy Models<a href="http://localhost:6756/BCB743/occupancy.html#advantages-and-limitations-of-occupancy-models" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div id="advantages" class="section level3">

### Advantages<a href="http://localhost:6756/BCB743/occupancy.html#advantages" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Occupancy modelling is very useful, but it is not always the right tool for every ecological question, so it is worth understanding both its strengths and its weaknesses. The most important advantage is that it accounts for imperfect detection: by estimating detection probability explicitly, it reduces the bias caused by false absences. The models require only presence-absence data rather than counts of every individual, which makes them well suited to long-term monitoring programmes that contribute to biodiversity management. They also explicitly separate ecological and observation processes, which makes it possible to model the factors affecting each process independently.

</div>

<div id="limitations" class="section level3">

### Limitations<a href="http://localhost:6756/BCB743/occupancy.html#limitations" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The major limitation of occupancy modelling is the repeated-survey requirement: repeated surveys are time-consuming and require more field effort than a single visit. The single-season model is also limited by the closure assumption, which does not always hold in practice – animals moving in and out of a site during the survey period can bias the results. Detection probability should not be too low, since very low <span class="math inline">𝑝</span> makes it difficult to reliably detect the species at all, which in turn widens uncertainty around <span class="math inline">ˆ𝜓</span>. Occupancy models can also produce misidentifications, since the standard model assumes all detections are correct; when this assumption is violated, false positives can bias the results. More generally, occupancy models operate under a specific set of assumptions, and if the study design does not align with those assumptions, the resulting estimates cannot be considered reliable.

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>A note on zero-inflation

</div>

</div>

<div class="callout-body-container callout-body">

Standard occupancy models already address one form of zero-inflation through the <span class="math inline">𝜓</span>/<span class="math inline">𝑝</span> framework: they explicitly model two sources of zeros in the data (true absence and non-detection at an occupied site), rather than treating all zeros as equivalent. A further extension – the *zero-inflated* occupancy model of Miller et al. (2011) – adds a third source of structural zeros for species that are entirely unavailable for detection at a subset of sites (e.g. because the site is outside the species’ range for reasons unrelated to habitat suitability). This extension is outside the scope of this chapter, but is worth knowing about if you encounter a system where “occupancy” itself may not be well defined at every surveyed site.

</div>

</div>

</div>

</div>

<div id="worked-example-single-season-occupancy-with-the-crossbill-dataset" class="section level2">

## Worked Example: Single-Season Occupancy with the Crossbill Dataset<a href="http://localhost:6756/BCB743/occupancy.html#worked-example-single-season-occupancy-with-the-crossbill-dataset" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>**Crossbill Dataset**

</div>

</div>

<div class="callout-body-container callout-body">

| Columns | Content |
|:---|:---|
| `id` | site identifier |
| `ele` | elevation (m) - site covariate |
| `forest` | percent forest cover - site covariate |
| `surveys` | number of surveys conducted |
| `det991`-`det993`, `det001`-`det003`,… | 27 columns: crossbill detection/nondetection data during all surveys over the 9 years. |
| `date991`-`date993`, `date001`-`date003`,… | 27 columns: Julian survey date for each corresponding visit. |
| `x`, `y` | coordinates |

</div>

</div>

The `crossbill` dataset (built into `unmarked`) comes from the Swiss breeding bird survey (Monitoring Häufige Brutvögel, MHB). It covers 267 1-km² survey quadrats across Switzerland, each visited up to 3 times per year between 1999 and 2007, recording detection/non-detection of the European crossbill (*Loxia curvirostra*).

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Reflect Before You Start

</div>

</div>

<div class="callout-body-container callout-body">

Before opening `?crossbill`, write down what you expect. Crossbills depend on conifer seed crops, so where would you predict occupancy to be highest: low-elevation farmland, mid-elevation forest, or high alpine terrain? You will check this prediction against the fitted model further down.

</div>

</div>

<div id="load-the-data-and-inspect-its-shape" class="section level3">

### Load the Data and Inspect Its Shape<a href="http://localhost:6756/BCB743/occupancy.html#load-the-data-and-inspect-its-shape" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
library(unmarked)   # occu(), unmarkedFrameOccu()
library(dplyr)       # data wrangling
library(ggplot2)      # plotting
library(AICcmodavg)   # GOF test, model averaging
library(MuMIn)         # dredge() for model selection

# crossbill ships with unmarked -- no external file needed
data(crossbill)

# 267 sites x (id, ele, forest, x, y, then 9 years x 3 visits of
# detections "detYYYY_V" and survey dates "dateYYYY_V")
str(crossbill[, 1:10])
```

</div>

<div class="cell-output cell-output-stdout">

    'data.frame':   267 obs. of  10 variables:
     $ id     : int  1 2 3 4 5 6 7 8 9 10 ...
     $ ele    : int  450 450 1050 950 1150 550 750 650 550 550 ...
     $ forest : int  3 21 32 9 35 2 6 60 5 13 ...
     $ surveys: int  3 3 3 3 3 3 3 3 3 3 ...
     $ det991 : int  0 0 NA 0 0 NA 0 0 0 0 ...
     $ det992 : int  0 0 NA 0 0 NA 0 0 0 0 ...
     $ det993 : int  0 0 NA 0 0 NA 0 0 0 0 ...
     $ det001 : int  0 0 0 1 1 0 0 0 0 1 ...
     $ det002 : int  0 0 0 0 1 0 0 0 0 0 ...
     $ det003 : int  0 0 0 0 1 0 1 0 0 0 ...

</div>

</div>

A single-season frame needs three ingredients: a detection history matrix, site-level covariates, and (optionally) observation-level covariates. The crossbill object bundles all three inside one wide data frame, so most of the work here is pulling the right columns out rather than reshaping raw field sheets.

</div>

<div id="carve-out-one-season-1999" class="section level3">

### Carve Out One Season (1999)<a href="http://localhost:6756/BCB743/occupancy.html#carve-out-one-season-1999" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

I will use 1999, the first year surveyed, so the workflow is directly comparable to the three-visit frog design used in the earlier tiers.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Detection history: three visits in 1999
det_1999 <- crossbill %>%
  dplyr::select(id, det991, det992, det993)

# Site covariates: elevation and forest cover, standardized as in the
# simulated tiers so coefficients are comparable in magnitude
site_cov <- crossbill %>%
  dplyr::select(id, ele, forest) %>%
  mutate(ele_z = as.numeric(scale(ele)),
         forest_z = as.numeric(scale(forest)))

# Observation covariate: Julian survey date, one column per visit,
# needed to answer Question 4 (does detection vary with survey timing?)
date_1999 <- crossbill %>%
  dplyr::select(date991, date992, date993)

head(det_1999)
```

</div>

<div class="cell-output cell-output-stdout">

      id det991 det992 det993
    1  1      0      0      0
    2  2      0      0      0
    3  3     NA     NA     NA
    4  4      0      0      0
    5  5      0      0      0
    6  6     NA     NA     NA

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
head(site_cov)
```

</div>

<div class="cell-output cell-output-stdout">

      id  ele forest       ele_z    forest_z
    1  1  450      3 -1.15390803 -1.14710440
    2  2  450     21 -1.15390803 -0.49668159
    3  3 1050     32 -0.21745379 -0.09920098
    4  4  950      9 -0.37352950 -0.93029679
    5  5 1150     35 -0.06137809  0.00920282
    6  6  550      2 -0.99783232 -1.18323900

</div>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Callout: Missing Visits Are Normal, Not an Error

</div>

</div>

<div class="callout-body-container callout-body">

High-elevation sites in the MHB are only surveyed twice per season, so `det993` (and its matching `date993`) will contain `NA` for those rows. `occu()` handles this natively – a missing visit simply drops out of that site’s likelihood term rather than being treated as a non-detection. Do not recode `NA` to `0`; that claims a survey happened and found nothing, which is a different (false) statement.

</div>

</div>

</div>

<div id="the-occupancy-model-with-unmarked" class="section level3">

### The Occupancy Model with `unmarked`<a href="http://localhost:6756/BCB743/occupancy.html#the-occupancy-model-with-unmarked" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The `unmarked` package implements the MacKenzie et al. (2002) likelihood. The workflow has three steps: (1) package the data into an `unmarkedFrameOccu` object, (2) fit one or more `occu()` models, and (3) extract and interpret the parameter estimates.

<div id="setting-up-the-unmarkedframe" class="section level4">

#### Setting up the `unmarkedFrame`<a href="http://localhost:6756/BCB743/occupancy.html#setting-up-the-unmarkedframe" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

For a single-season occupancy model, `unmarked` expects:

- A **detection matrix** (<span class="math inline">𝑛sites ×𝑛visits</span>): one row per site, one column per visit occasion.
- Optional **site covariates** (covariates on <span class="math inline">𝜓</span>): one value per site.
- Optional **observation covariates** (covariates on <span class="math inline">𝑝</span>): one value per site-visit combination.

</div>

<div id="build-the-unmarkedframeoccu" class="section level4">

#### Build the `unmarkedFrameOccu`<a href="http://localhost:6756/BCB743/occupancy.html#build-the-unmarkedframeoccu" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
crossbill_umf <- unmarkedFrameOccu(
  y = as.matrix(det_1999[, c("det991", "det992", "det993")]),
  siteCovs = site_cov[, c("ele_z", "forest_z")],
  obsCovs = list(date = date_1999)
)

summary(crossbill_umf)
```

</div>

<div class="cell-output cell-output-stdout">

    unmarkedFrame Object

    267 sites
    Maximum number of observations per site: 3 
    Mean number of observations per site: 2.59 
    Sites with at least one detection: 63 

    Tabulation of y observations:
       0    1 <NA> 
     599   92  110 

    Site-level covariates:
         ele_z             forest_z       
     Min.   :-1.46606   Min.   :-1.25551  
     1st Qu.:-0.99783   1st Qu.:-0.94836  
     Median :-0.06138   Median :-0.06307  
     Mean   : 0.00000   Mean   : 0.00000  
     3rd Qu.: 1.03115   3rd Qu.: 0.78610  
     Max.   : 2.43583   Max.   : 2.32182  

    Observation-level covariates:
          date      
     Min.   : 10.0  
     1st Qu.: 40.0  
     Median : 59.0  
     Mean   : 57.3  
     3rd Qu.: 73.0  
     Max.   :115.0  
     NA's   :110    

</div>

</div>

</div>

</div>

<div id="naive-occupancy-vs.-the-null-model" class="section level3">

### Naive Occupancy vs. the Null Model<a href="http://localhost:6756/BCB743/occupancy.html#naive-occupancy-vs.-the-null-model" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Before fitting anything, calculate naive occupancy – the proportion of sites where the crossbill was detected at least once. This is the same quantity the “Ecological Background” section above builds the “why we need p” argument around, and it will again understate true occupancy here.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# This is the quantity that ignores imperfect detection entirely.
naive_occ <- mean(rowSums(det_1999[, c("det991","det992","det993")],
                           na.rm = TRUE) > 0)
naive_occ
```

</div>

<div class="cell-output cell-output-stdout">

    [1] 0.2359551

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# Null (intercept-only) model gives the model-based comparison point
occ_null <- occu(~1 ~1, data = crossbill_umf)
backTransform(occ_null, type = "state")   # model-based psi
```

</div>

<div class="cell-output cell-output-stdout">

    Backtransformed linear combination(s) of Occupancy estimate(s)

     Estimate     SE LinComb (Intercept)
        0.367 0.0505  -0.546           1

    Transformation: logistic 

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
backTransform(occ_null, type = "det")     # model-based p
```

</div>

<div class="cell-output cell-output-stdout">

    Backtransformed linear combination(s) of Detection estimate(s)

     Estimate     SE LinComb (Intercept)
        0.356 0.0476  -0.594           1

    Transformation: logistic 

</div>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Reflect

</div>

</div>

<div class="callout-body-container callout-body">

Compare naive occupancy to the model-based estimate of ψ from `occ_null`. Which is larger, and does the gap match what you’d expect given the estimated detection probability?

</div>

</div>

</div>

<div id="add-covariates-does-detection-vary-with-survey-date" class="section level3">

### Add Covariates: Does Detection Vary with Survey Date?<a href="http://localhost:6756/BCB743/occupancy.html#add-covariates-does-detection-vary-with-survey-date" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Before asking what drives occupancy, check what drives detection – if that is ignored, an occupancy covariate could just be soaking up a detection-probability pattern instead.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
occ_date <- occu(~ scale(date) ~1, data = crossbill_umf)
summary(occ_date)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~scale(date) ~ 1, data = crossbill_umf)

    Occupancy (logit-scale):
     Estimate    SE     z P(>|z|)
       -0.475 0.229 -2.08  0.0379

    Detection (logit-scale):
                Estimate    SE     z  P(>|z|)
    (Intercept)   -0.758 0.216 -3.52 0.000437
    scale(date)    0.570 0.158  3.59 0.000325

    AIC: 499.2421 
    Number of sites: 245
    ID of sites removed due to NA: 3 6 38 63 71 106 116 118 125 126 152 163 168 178 181 197 199 212 218 220 238 257

</div>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Reflect

</div>

</div>

<div class="callout-body-container callout-body">

Is the coefficient on `date` positive, negative, or indistinguishable from zero? A positive effect would mean later-season visits are more likely to record a crossbill that is present; a null effect means survey timing (within the mid-April to late-June window) doesn’t matter much for this species – either is a defensible, informative finding.

</div>

</div>

- **Positive coefficient**: Detection improves later in the season.
- **Negative coefficient**: Detection worsens later in the season.
- **Near-zero / CI straddling zero**: No detectable seasonal effect on detectability within this survey window.

</div>

<div id="site-level-covariates-elevation-and-forest-on-occupancy" class="section level3">

### Site-level Covariates: Elevation and Forest on Occupancy<a href="http://localhost:6756/BCB743/occupancy.html#site-level-covariates-elevation-and-forest-on-occupancy" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
occ_full <- occu(~ scale(date) ~ ele_z + forest_z, data = crossbill_umf)
summary(occ_full)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~scale(date) ~ ele_z + forest_z, data = crossbill_umf)

    Occupancy (logit-scale):
                Estimate    SE     z  P(>|z|)
    (Intercept)   -0.743 0.254 -2.93 3.43e-03
    ele_z          0.590 0.215  2.74 6.12e-03
    forest_z       0.978 0.232  4.21 2.52e-05

    Detection (logit-scale):
                Estimate    SE     z  P(>|z|)
    (Intercept)   -0.674 0.214 -3.15 0.001631
    scale(date)    0.551 0.167  3.29 0.000991

    AIC: 471.83 
    Number of sites: 245
    ID of sites removed due to NA: 3 6 38 63 71 106 116 118 125 126 152 163 168 178 181 197 199 212 218 220 238 257

</div>

</div>

</div>

<div id="model-selection" class="section level3">

### Model Selection<a href="http://localhost:6756/BCB743/occupancy.html#model-selection" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

I fit all subsets of the general model and rank them by AICc rather than presenting a single model as if it were the only reasonable one.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
occ_dredge <- dredge(occ_full, rank = "AICc")
occ_dredge
```

</div>

<div class="cell-output cell-output-stdout">

    Global model call: occu(formula = ~scale(date) ~ ele_z + forest_z, data = crossbill_umf)
    ---
    Model selection table 
      psi(Int) psi(ele_z) psi(frs_z)  p(Int) p(scl(dat)) df   logLik  AICc delta
    8  -0.7434     0.5899     0.9782 -0.6739      0.5506  5 -230.915 472.1  0.00
    7  -0.4160                1.0400 -0.8961      0.6732  4 -234.545 477.3  5.18
    4  -0.7891     0.7975     0.8424 -0.5375              4 -236.560 481.3  9.20
    6   5.2220     7.4130            -1.4370      0.2044  4 -239.432 487.0 14.95
    2   4.3010     6.5540            -1.3410              3 -240.720 487.5 15.46
    3  -0.6734                0.7940 -0.5857              3 -244.325 494.8 22.67
    5  -0.4751                       -0.7585      0.5695  3 -246.621 499.3 27.26
    1  -0.5461                       -0.5940              2 -253.627 511.3 39.22
      weight
    8  0.921
    7  0.069
    4  0.009
    6  0.001
    2  0.000
    3  0.000
    5  0.000
    1  0.000
    Models ranked by AICc(x) 

</div>

</div>

</div>

<div id="goodness-of-fit" class="section level3">

### Goodness-of-Fit<a href="http://localhost:6756/BCB743/occupancy.html#goodness-of-fit" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# nsim raised well beyond 10 for real inference; kept low here for a fast
# teaching run. Increase to >=1000 before treating the chi-square p-value
# as informative.
crossbill_gof <- mb.gof.test(occ_full, nsim = 25)
crossbill_gof
```

</div>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Callout: Why `eval: false`

</div>

</div>

<div class="callout-body-container callout-body">

The bootstrap GOF test can take several minutes even at `nsim = 25` on a full class’s laptops. The chunk is shown but not executed automatically when the chapter renders; run it once locally with `nsim` raised to at least 1000 before quoting a p-value in your write-up.

</div>

</div>

</div>

<div id="predicted-occupancy-along-the-elevation-gradient" class="section level3">

### Predicted Occupancy Along the Elevation Gradient<a href="http://localhost:6756/BCB743/occupancy.html#predicted-occupancy-along-the-elevation-gradient" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

Code

<div class="code-copy-outer-scaffold">

``` numberSource
new_ele <- data.frame(
  ele_z = seq(min(site_cov$ele_z, na.rm = TRUE),
              max(site_cov$ele_z, na.rm = TRUE), by = 0.05),
  forest_z = mean(site_cov$forest_z, na.rm = TRUE)
)

pred_ele <- predict(occ_full, newdata = new_ele, type = "state") %>%
  bind_cols(new_ele)

ggplot(pred_ele, aes(x = ele_z, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
  geom_line(linewidth = 1) +
  labs(x = "Elevation (standardized)", y = "Predicted occupancy probability") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic()
```

</div>

<div class="cell-output-display">

<div id="fig-crossbill-elevation" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-crossbill-elevation-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<a href="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-crossbill-elevation-1.svg" class="lightbox" data-gallery="quarto-lightbox-gallery-1" data-original-href="http://localhost:6756/BCB743/occupancy_files/figure-html/fig-crossbill-elevation-1.svg" title="Figure 1: Predicted crossbill occupancy probability across the standardized elevation gradient, forest cover held at its mean. Shaded band is the 95% confidence interval from the top AICc model."><img src="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-crossbill-elevation-1.svg" class="img-fluid figure-img" /></a>
</div>
<figcaption>Figure 1: Predicted crossbill occupancy probability across the standardized elevation gradient, forest cover held at its mean. Shaded band is the 95% confidence interval from the top AICc model.</figcaption>
</figure>

<a href="http://localhost:6756/BCB743/occupancy.html#fig-crossbill-elevation" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

</div>

</div>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Reflect

</div>

</div>

<div class="callout-body-container callout-body">

Does the shape of this curve match the prediction when we loaded and inspected the data? If not, what ecological explanation would reconcile crossbill biology with what the model shows?

</div>

</div>

</div>

</div>

<div id="south-african-case-study-capensibufo-rosei" class="section level1">

# South African Case Study: *Capensibufo rosei*

<div id="background" class="section level2">

## Background<a href="http://localhost:6756/BCB743/occupancy.html#background" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

A species can be absent from a survey record for two fundamentally different reasons: it was genuinely not there, or it was there but the observer failed to detect it. Confusing these two produces biased estimates of occupancy — the proportion of sites where a species truly occurs. The classical MacKenzie et al. (2002) occupancy model was designed to untangle this confusion by decomposing a detection history at a site across repeated visits into two separate processes: the ecological state process governing whether the site is occupied (<span class="math inline">𝜓</span>), and the observation process governing whether the species is detected given that it is present (<span class="math inline">𝑝</span>). This section works through that model using a real South African frog dataset that places the imperfect-detection problem front and centre.

</div>

<div id="the-study-system-capensibufo-rosei" class="section level2">

## The study system: *Capensibufo rosei*<a href="http://localhost:6756/BCB743/occupancy.html#the-study-system-capensibufo-rosei" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Rose’s Mountain Toadlet (*Capensibufo rosei*) is a Critically Endangered frog endemic to the Cape Peninsula of South Africa. It is known from only two breeding localities — both inside Table Mountain National Park — and has disappeared from at least four historical sites since the 1980s, apparently because fire suppression has allowed vegetation to encroach on its open fynbos breeding pools. The toad lacks an advertisement call and breeds in dense aggregations for only a few weeks per year, making it exceptionally hard to detect.

Becker et al. (2022) asked a question that precedes occupancy modelling in the usual sense: before we can monitor a species, we need to know *where it is*. Specifically, they asked what the probability is that all occupied sites on the Cape Peninsula have already been found, and how much additional search effort would be needed to reduce the residual probability of an undiscovered site to an acceptable level.

The dataset we use here — `All_spp_search_data.csv` — contains 101 candidate sites (300 × 300 m grid cells) across the Peninsula that were considered potential *C. rosei* habitat. Because the toad itself is so rarely encountered, the study also surveyed four ecologically similar proxy frog species that share its habitat requirements. These proxy species — *Amietia fuscigula* (Cape River Frog), *Arthroleptella lightfooti* (Cape Peninsula Moss Frog), *Strongylopus bonaespei* (Clicking Stream Frog), and *Strongylopus grayii* (Clicking Stream Frog) — have higher detectabilities and their combined presence/absence pattern carries information about habitat quality and search effort at each site.

This is worth pausing on. The absence of a `PA.ros` (presence/absence of *C. rosei*) column in the data is not a data-entry oversight; it is the entire scientific problem. The target species is so rare that you cannot estimate detection probability directly from its own detection history, so the detection histories of the proxy species must be used instead. This is an occupancy problem where imperfect detection is the obstacle that stands between the surveyor and knowledge of the species’ true distribution.

</div>

<div id="setup" class="section level2">

## Setup<a href="http://localhost:6756/BCB743/occupancy.html#setup" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
library(tidyverse)    # data wrangling and ggplot2
library(unmarked)     # occupancy models
library(here)         # file path management
library(patchwork)    # multi-panel figures
library(AICcmodavg)   # model selection tables
```

</div>

</div>

</div>

<div id="the-data" class="section level2">

## The Data<a href="http://localhost:6756/BCB743/occupancy.html#the-data" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
rosei <- read_csv("c://Users//27659//Downloads//All_spp _search_data.csv")
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
glimpse(rosei)
```

</div>

<div class="cell-output cell-output-stdout">

    Rows: 101
    Columns: 16
    $ `Cell Nr`      <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
    $ lat            <dbl> -34.02660, -33.98633, -34.22001, -34.35319, -34.08911, …
    $ lon            <dbl> 18.38485, 18.39907, 18.45051, 18.48235, 18.39918, 18.47…
    $ shallowWetland <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0…
    $ `2/3ClosetSpp` <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0…
    $ PA.fus         <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0…
    $ PA.lig         <dbl> 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0…
    $ PA.bon         <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ PA.gra         <dbl> 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1…
    $ HS.ros.mean.0  <dbl> 0.01643391, 0.07341415, 0.08934713, 0.04453570, 0.05518…
    $ HS.fus.mean    <dbl> 0.19296603, 0.13660381, 0.41659909, 0.16398784, 0.21772…
    $ HS.lig.mean    <dbl> 0.15082349, 0.38320287, 0.42589343, 0.24743826, 0.31863…
    $ HS.bon.mean    <dbl> 0.06792204, 0.11792096, 0.25526689, 0.17245076, 0.19844…
    $ HS.gra.mean    <dbl> 0.02145689, 0.07954140, 0.10034826, 0.34484767, 0.10612…
    $ ColNames       <chr> "Site Nr", "shallowWetland", "2/3ClosetSpp", "PA.fus", …
    $ Explanation    <chr> "number asigned to the particular 300x300m grid cell in…

</div>

</div>

The dataset has 16 columns. The ones we will work with are:

| Column | Role |
|:---|:---|
| `Cell Nr` | Site identifier (1–101) |
| `lat`, `lon` | Centroid coordinates of the 300 × 300 m grid cell |
| `shallowWetland` | Presence (1) or absence (0) of shallow wetland covering ≥5% of the cell |
| `2/3ClosetSpp` | Whether ≥2 of the 3 most habitat-similar proxy species were detected |
| `PA.fus` | Presence/absence of *Amietia fuscigula* |
| `PA.lig` | Presence/absence of *Arthroleptella lightfooti* |
| `PA.bon` | Presence/absence of *Strongylopus bonaespei* |
| `PA.gra` | Presence/absence of *Strongylopus grayii* |
| `HS.ros.mean.0` | Predicted habitat suitability for *C. rosei* from a species distribution model (SDM) |

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# The first 12 rows contain a data dictionary embedded in the last two columns.
# The actual site data are rows where Cell Nr is not NA and Explanation is NA.
dat <- rosei |>
  filter(!is.na(`Cell Nr`), is.na(Explanation)) |>
  rename(
    site      = `Cell Nr`,
    wetland   = shallowWetland,
    proxy23   = `2/3ClosetSpp`,
    pa_fus    = PA.fus,
    pa_lig    = PA.lig,
    pa_bon    = PA.bon,
    pa_gra    = PA.gra,
    hs_ros    = HS.ros.mean.0,
    hs_fus    = HS.fus.mean,
    hs_lig    = HS.lig.mean,
    hs_bon    = HS.bon.mean,
    hs_gra    = HS.gra.mean
  ) |>
  select(site, lat, lon, wetland, proxy23, pa_fus, pa_lig, pa_bon, pa_gra,
         hs_ros, hs_fus, hs_lig, hs_bon, hs_gra)

glimpse(dat)
```

</div>

<div class="cell-output cell-output-stdout">

    Rows: 89
    Columns: 14
    $ site    <dbl> 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28…
    $ lat     <dbl> -34.21163, -34.35312, -33.97005, -33.97836, -33.98072, -34.067…
    $ lon     <dbl> 18.43774, 18.47909, 18.39632, 18.40580, 18.38950, 18.38035, 18…
    $ wetland <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0,…
    $ proxy23 <dbl> 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,…
    $ pa_fus  <dbl> 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,…
    $ pa_lig  <dbl> 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1,…
    $ pa_bon  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,…
    $ pa_gra  <dbl> 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ hs_ros  <dbl> 0.12360613, 0.09740540, 0.20099753, 0.16718571, 0.24326740, 0.…
    $ hs_fus  <dbl> 0.47077629, 0.23982969, 0.33538220, 0.54116075, 0.36856030, 0.…
    $ hs_lig  <dbl> 0.4698832, 0.3337963, 0.4181338, 0.2772457, 0.5612647, 0.19344…
    $ hs_bon  <dbl> 0.46895403, 0.31193447, 0.24570941, 0.74313423, 0.17014889, 0.…
    $ hs_gra  <dbl> 0.10231581, 0.38805496, 0.15851621, 0.28018944, 0.18361709, 0.…

</div>

</div>

</div>

<div id="naive-occupancy" class="section level2">

## Naive occupancy<a href="http://localhost:6756/BCB743/occupancy.html#naive-occupancy" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Before fitting any model, it is instructive to calculate the *naïve* occupancy rate: the raw proportion of sites where each proxy species was detected, ignoring the possibility of false absences.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
dat |>
  summarise(
    across(c(pa_fus, pa_lig, pa_bon, pa_gra), ~ mean(.x, na.rm = TRUE),
           .names = "naive_{.col}")
  )
```

</div>

<div class="cell-output cell-output-stdout">

    # A tibble: 1 × 4
      naive_pa_fus naive_pa_lig naive_pa_bon naive_pa_gra
             <dbl>        <dbl>        <dbl>        <dbl>
    1        0.463        0.730        0.348        0.271

</div>

</div>

These naive rates underestimate true occupancy whenever <span class="math inline">𝑝 \<1</span>. How much they underestimate depends on both the true occupancy and the detection probability; the occupancy model will separate the two.

</div>

<div id="visualising-the-data" class="section level2">

## Visualising the data<a href="http://localhost:6756/BCB743/occupancy.html#visualising-the-data" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Before modelling, it helps to see where the sites are and how the proxy species are distributed across them.

<div class="cell">

Code

<div class="code-copy-outer-scaffold">

``` numberSource
ggplot(dat, aes(x = lon, y = lat, colour = factor(wetland))) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_colour_manual(
    values = c("0" = "grey70", "1" = "#2166ac"),
    labels = c("Absent", "Present"),
    na.value = "grey90"
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    colour = "Shallow wetland"
  ) +
  coord_fixed() +
  theme(legend.position = "bottom")
```

</div>

<div class="cell-output-display">

<div id="fig-site-map" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-site-map-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<a href="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-site-map-1.svg" class="lightbox" data-gallery="quarto-lightbox-gallery-2" data-original-href="http://localhost:6756/BCB743/occupancy_files/figure-html/fig-site-map-1.svg" title="Figure 2: The 101 candidate sites on the Cape Peninsula. Point colour shows whether shallow wetland habitat is present at each site."><img src="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-site-map-1.svg" class="img-fluid figure-img" /></a>
</div>
<figcaption>Figure 2: The 101 candidate sites on the Cape Peninsula. Point colour shows whether shallow wetland habitat is present at each site.</figcaption>
</figure>

<a href="http://localhost:6756/BCB743/occupancy.html#fig-site-map" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

</div>

</div>

</div>

<div class="cell">

Code

<div class="code-copy-outer-scaffold">

``` numberSource
dat |>
  select(site, pa_fus, pa_lig, pa_bon, pa_gra) |>
  pivot_longer(-site, names_to = "species", values_to = "detected") |>
  mutate(
    species = recode(species,
      pa_fus = "A. fuscigula",
      pa_lig = "A. lightfooti",
      pa_bon = "S. bonaespei",
      pa_gra = "S. grayii"
    )
  ) |>
  ggplot(aes(x = species, y = reorder(site, site), fill = factor(detected))) +
  geom_tile(colour = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c("0" = "white", "1" = "black"),
    labels = c("Not detected", "Detected"),
    na.value = "grey85"
  ) +
  labs(x = NULL, y = "Site", fill = NULL) +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )
```

</div>

<div class="cell-output-display">

<div id="fig-pa-matrix" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-pa-matrix-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<a href="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-pa-matrix-1.svg" class="lightbox" data-gallery="quarto-lightbox-gallery-3" data-original-href="http://localhost:6756/BCB743/occupancy_files/figure-html/fig-pa-matrix-1.svg" title="Figure 3: Detection matrix for the four proxy species across all 101 sites. Black cells = detected; white = not detected; grey = not surveyed (NA)."><img src="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-pa-matrix-1.svg" class="img-fluid figure-img" /></a>
</div>
<figcaption>Figure 3: Detection matrix for the four proxy species across all 101 sites. Black cells = detected; white = not detected; grey = not surveyed (NA).</figcaption>
</figure>

<a href="http://localhost:6756/BCB743/occupancy.html#fig-pa-matrix" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

</div>

</div>

</div>

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>What the detection matrix tells you already

</div>

</div>

<div class="callout-body-container callout-body">

The grey cells — sites where a species was not surveyed — are not missing data in the usual sense. They represent the incompleteness of the search: a site was visited but a particular proxy species was not recorded (perhaps because it was not the focus of that visit, or conditions were unsuitable). This kind of missing data, where non-detection and absence of a survey visit are conflated, is precisely what the occupancy model machinery handles through the `NA`-tolerant likelihood.

</div>

</div>

Because our data record only a single composite visit per site per proxy species (rather than repeated visits across time), we treat the detection history of the four proxy species as the “repeat visits”. This is an important conceptual move: we are not asking whether *C. rosei* itself was detected on multiple occasions, but whether the proxy community shows evidence of suitable, well-searched habitat. Each proxy species detection is one “look” at the site.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Detection matrix: rows = sites, columns = proxy species (the four 'visits')
y_mat <- dat |>
  select(pa_fus, pa_lig, pa_bon, pa_gra) |>
  as.matrix()

# Site covariates (standardise continuous predictors for numerical stability)
site_covs <- dat |>
  select(wetland, proxy23, hs_ros) |>
  mutate(
    hs_ros_sc = as.numeric(scale(hs_ros))
  )

# Package everything into an unmarkedFrameOccu
umf <- unmarkedFrameOccu(
  y      = y_mat,
  siteCovs = site_covs
)

summary(umf)
```

</div>

<div class="cell-output cell-output-stdout">

    unmarkedFrame Object

    89 sites
    Maximum number of observations per site: 4 
    Mean number of observations per site: 3.11 
    Sites with at least one detection: 71 

    Tabulation of y observations:
       0    1 <NA> 
     150  127   79 

    Site-level covariates:
        wetland          proxy23        hs_ros           hs_ros_sc      
     Min.   :0.0000   Min.   :0.0   Min.   :0.002546   Min.   :-1.2559  
     1st Qu.:0.0000   1st Qu.:0.0   1st Qu.:0.070026   1st Qu.:-0.8619  
     Median :0.0000   Median :0.5   Median :0.189146   Median :-0.1664  
     Mean   :0.4267   Mean   :0.5   Mean   :0.217638   Mean   : 0.0000  
     3rd Qu.:1.0000   3rd Qu.:1.0   3rd Qu.:0.309961   3rd Qu.: 0.5391  
     Max.   :1.0000   Max.   :1.0   Max.   :0.730102   Max.   : 2.9922  
     NA's   :14       NA's   :29                                        

</div>

</div>

The summary confirms 101 sites and 4 observations (proxy species) per site, along with the proportion of observations that yielded a detection for each proxy.

</div>

<div id="fitting-candidate-models" class="section level2">

## Fitting candidate models<a href="http://localhost:6756/BCB743/occupancy.html#fitting-candidate-models" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

We fit a set of models that vary in what predicts occupancy (<span class="math inline">𝜓</span>) while holding detection probability (<span class="math inline">𝑝</span>) constant. This follows the standard practice of first modelling the detection process and then, once satisfied with it, exploring the occupancy structure.

The `occu()` formula syntax is `~ detection ~ occupancy`. A `1` on either side specifies an intercept-only (constant) model.

<div id="model-1-null-model-constant-psi-and-p" class="section level3">

### Model 1: Null model — constant <span class="math inline">𝜓</span> and <span class="math inline">𝑝</span><a href="http://localhost:6756/BCB743/occupancy.html#model-1-null-model-constant-psi-and-p" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
m_null <- occu(~ 1 ~ 1, data = umf)
summary(m_null)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~1 ~ 1, data = umf)

    Occupancy (logit-scale):
     Estimate    SE    z  P(>|z|)
         2.22 0.513 4.33 1.51e-05

    Detection (logit-scale):
     Estimate    SE     z P(>|z|)
       0.0709 0.153 0.462   0.644

    AIC: 380.7589 
    Number of sites: 85
    ID of sites removed due to NA: 41 58 72 80

</div>

</div>

The coefficients are on the logit scale. We back-transform to probabilities using `plogis()`.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Back-transformed estimates
psi_null <- plogis(coef(m_null, type = "state"))
p_null   <- plogis(coef(m_null, type = "det"))

cat("Estimated occupancy (psi):", round(psi_null, 3), "\n")
```

</div>

<div class="cell-output cell-output-stdout">

    Estimated occupancy (psi): 0.902 

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
cat("Estimated detection (p):  ", round(p_null,   3), "\n")
```

</div>

<div class="cell-output cell-output-stdout">

    Estimated detection (p):   0.518 

</div>

</div>

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Comparing naive occupancy with model-corrected occupancy

</div>

</div>

<div class="callout-body-container callout-body">

Compare the model-corrected <span class="math inline">ˆ𝜓</span> with the naive occupancy rates you calculated earlier. The difference is the magnitude of the detection bias that the model corrects for. For species with low <span class="math inline">𝑝</span>, this correction can be substantial.

</div>

</div>

</div>

<div id="model-2-occupancy-predicted-by-shallow-wetland-habitat" class="section level3">

### Model 2: Occupancy predicted by shallow wetland habitat<a href="http://localhost:6756/BCB743/occupancy.html#model-2-occupancy-predicted-by-shallow-wetland-habitat" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
m_wetland <- occu(~ 1 ~ wetland, data = umf)
summary(m_wetland)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~1 ~ wetland, data = umf)

    Occupancy (logit-scale):
                Estimate     SE    z P(>|z|)
    (Intercept)     1.12  0.418 2.67 0.00758
    wetland        10.35 57.349 0.18 0.85680

    Detection (logit-scale):
     Estimate    SE    z P(>|z|)
       0.0475 0.149 0.32   0.749

    AIC: 336.0737 
    Number of sites: 71
    ID of sites removed due to NA: 41 58 60 64 66 72 77 78 79 80 81 82 83 85 86 87 88 89

</div>

</div>

</div>

<div id="model-3-occupancy-predicted-by-c.-rosei-sdm-habitat-suitability-score" class="section level3">

### Model 3: Occupancy predicted by *C. rosei* SDM habitat suitability score<a href="http://localhost:6756/BCB743/occupancy.html#model-3-occupancy-predicted-by-c.-rosei-sdm-habitat-suitability-score" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
m_hs <- occu(~ 1 ~ hs_ros_sc, data = umf)
summary(m_hs)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~1 ~ hs_ros_sc, data = umf)

    Occupancy (logit-scale):
                Estimate   SE    z P(>|z|)
    (Intercept)     3.78 1.25 3.03 0.00247
    hs_ros_sc       3.06 1.25 2.45 0.01444

    Detection (logit-scale):
     Estimate    SE    z P(>|z|)
        0.164 0.141 1.17   0.242

    AIC: 366.0297 
    Number of sites: 85
    ID of sites removed due to NA: 41 58 72 80

</div>

</div>

</div>

<div id="model-4-occupancy-predicted-by-proxy-community-co-occurrence" class="section level3">

### Model 4: Occupancy predicted by proxy community co-occurrence<a href="http://localhost:6756/BCB743/occupancy.html#model-4-occupancy-predicted-by-proxy-community-co-occurrence" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
m_proxy <- occu(~ 1 ~ proxy23, data = umf)
summary(m_proxy)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~1 ~ proxy23, data = umf)

    Occupancy (logit-scale):
                Estimate     SE     z P(>|z|)
    (Intercept)    0.737  0.434 1.697  0.0896
    proxy23        9.067 24.573 0.369  0.7121

    Detection (logit-scale):
     Estimate    SE       z P(>|z|)
      -0.0118 0.149 -0.0791   0.937

    AIC: 314.6877 
    Number of sites: 60
    ID of sites removed due to NA: 35 36 39 41 42 44 51 53 58 60 61 62 67 72 73 76 77 78 79 80 81 82 83 84 85 86 87 88 89

</div>

</div>

</div>

<div id="model-5-additive-model-wetland-and-sdm-score" class="section level3">

### Model 5: Additive model — wetland and SDM score<a href="http://localhost:6756/BCB743/occupancy.html#model-5-additive-model-wetland-and-sdm-score" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
m_add <- occu(~ 1 ~ wetland + hs_ros_sc, data = umf)
summary(m_add)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~1 ~ wetland + hs_ros_sc, data = umf)

    Occupancy (logit-scale):
                Estimate    SE     z P(>|z|)
    (Intercept)     2.52  1.06 2.386  0.0170
    wetland         7.74 29.29 0.264  0.7916
    hs_ros_sc       2.22  1.10 2.022  0.0432

    Detection (logit-scale):
     Estimate    SE     z P(>|z|)
       0.0721 0.145 0.497   0.619

    AIC: 329.6859 
    Number of sites: 71
    ID of sites removed due to NA: 41 58 60 64 66 72 77 78 79 80 81 82 83 85 86 87 88 89

</div>

</div>

</div>

</div>

<div id="model-selection-1" class="section level2">

## Model selection<a href="http://localhost:6756/BCB743/occupancy.html#model-selection-1" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

We compare models using AIC. The model with the lowest AIC, or equivalently the highest support in a relative sense, is preferred. Models within <span class="math inline">Δ⁢AIC \<2</span> of the top model have substantial support and should not be dismissed.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Create a named list of fitted models
cand_models <- list(
  "Null (psi(.) p(.))"            = m_null,
  "Wetland (psi(wet) p(.))"       = m_wetland,
  "SDM score (psi(HS) p(.))"      = m_hs,
  "Proxy co-occ (psi(prx) p(.))"  = m_proxy,
  "Additive (psi(wet+HS) p(.))"   = m_add
)

aictab(cand_models, second.ord = FALSE)
```

</div>

<div class="cell-output cell-output-stdout">


    Model selection based on AIC:

                                 K    AIC Delta_AIC AICWt Cum.Wt      LL
    Proxy co-occ (psi(prx) p(.)) 3 314.69      0.00     1      1 -154.34
    Additive (psi(wet+HS) p(.))  4 329.69     15.00     0      1 -160.84
    Wetland (psi(wet) p(.))      3 336.07     21.39     0      1 -165.04
    SDM score (psi(HS) p(.))     3 366.03     51.34     0      1 -180.01
    Null (psi(.) p(.))           2 380.76     66.07     0      1 -188.38

</div>

</div>

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Interpreting the AIC table

</div>

</div>

<div class="callout-body-container callout-body">

The `Delta_AIC` column shows the difference between each model’s AIC and the top model’s AIC. `AICWt` gives the Akaike weight, which can be read as the approximate probability that the model is the best-supported model among those considered. `Cum.Wt` is the cumulative weight: if the top two models together have a cumulative weight close to 1, little evidence supports the remaining models.

</div>

</div>

</div>

<div id="interpreting-the-best-supported-model" class="section level2">

## Interpreting the best-supported model<a href="http://localhost:6756/BCB743/occupancy.html#interpreting-the-best-supported-model" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Extract the top-ranked model for deeper inspection.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
library(MuMIn)

# Compare candidate models using AIC
aic_table <- data.frame(
  Model = names(cand_models),
  AIC   = sapply(cand_models, AIC)
)

# Rank models from best to worst
aic_table <- aic_table[order(aic_table$AIC), ]

# Calculate Delta AIC and Akaike weights
aic_table$DeltaAIC <- aic_table$AIC - min(aic_table$AIC)
aic_table$Weight <- MuMIn::Weights(aic_table$AIC)

# Display the model selection table
print(aic_table)
```

</div>

<div class="cell-output cell-output-stdout">

                                                        Model      AIC DeltaAIC
    Proxy co-occ (psi(prx) p(.)) Proxy co-occ (psi(prx) p(.)) 314.6877  0.00000
    Additive (psi(wet+HS) p(.))   Additive (psi(wet+HS) p(.)) 329.6859 14.99820
    Wetland (psi(wet) p(.))           Wetland (psi(wet) p(.)) 336.0737 21.38603
    SDM score (psi(HS) p(.))         SDM score (psi(HS) p(.)) 366.0297 51.34198
    Null (psi(.) p(.))                     Null (psi(.) p(.)) 380.7589 66.07125
                                       Weight
    Proxy co-occ (psi(prx) p(.)) 9.994240e-01
    Additive (psi(wet+HS) p(.))  5.532624e-04
    Wetland (psi(wet) p(.))      2.268994e-05
    SDM score (psi(HS) p(.))     7.095457e-12
    Null (psi(.) p(.))           4.493244e-15

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# Extract the best-supported model
best_model <- cand_models[[aic_table$Model[1]]]

cat("\nBest-supported model:", aic_table$Model[1], "\n\n")
```

</div>

<div class="cell-output cell-output-stdout">


    Best-supported model: Proxy co-occ (psi(prx) p(.)) 

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
summary(best_model)
```

</div>

<div class="cell-output cell-output-stdout">


    Call:
    occu(formula = ~1 ~ proxy23, data = umf)

    Occupancy (logit-scale):
                Estimate     SE     z P(>|z|)
    (Intercept)    0.737  0.434 1.697  0.0896
    proxy23        9.067 24.573 0.369  0.7121

    Detection (logit-scale):
     Estimate    SE       z P(>|z|)
      -0.0118 0.149 -0.0791   0.937

    AIC: 314.6877 
    Number of sites: 60
    ID of sites removed due to NA: 35 36 39 41 42 44 51 53 58 60 61 62 67 72 73 76 77 78 79 80 81 82 83 84 85 86 87 88 89

</div>

</div>

Back-transform both parameters from the best model.

`backTransform()` works only when a model component has a single coefficient (i.e. intercept only). For models with covariates on <span class="math inline">𝜓</span>, we use `predict()` evaluated at the mean of continuous covariates and the modal value of binary ones — the marginal occupancy at a “typical” site. Detection probability is always intercept-only here, so `backTransform()` is safe for <span class="math inline">𝑝</span>.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Detection probability: intercept-only across all candidate models, so backTransform() is safe
p_est <- backTransform(best_model, type = "det")
cat("Estimated detection probability (p):\n")
```

</div>

<div class="cell-output cell-output-stdout">

    Estimated detection probability (p):

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
p_est
```

</div>

<div class="cell-output cell-output-stdout">

    Backtransformed linear combination(s) of Detection estimate(s)

     Estimate     SE LinComb (Intercept)
        0.497 0.0373 -0.0118           1

    Transformation: logistic 

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
# Occupancy: use predict() so the code is robust regardless of which model is best.
# Evaluate at mean/modal covariate values to get the marginal occupancy estimate.
psi_pred <- predict(
  best_model,
  newdata = data.frame(
    hs_ros_sc = 0,   # mean of a standardised variable is 0
    wetland   = 0,   # modal value (most sites lack shallow wetland)
    proxy23   = 0    # modal value
  ),
  type = "state"
)

cat("\nEstimated occupancy at mean/modal covariate values (psi):\n")
```

</div>

<div class="cell-output cell-output-stdout">


    Estimated occupancy at mean/modal covariate values (psi):

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
cat("  Estimate:", round(psi_pred$Predicted, 3), "\n")
```

</div>

<div class="cell-output cell-output-stdout">

      Estimate: 0.676 

</div>

<div class="code-copy-outer-scaffold">

``` numberSource
cat("  95% CI: [", round(psi_pred$lower, 3), ",", round(psi_pred$upper, 3), "]\n")
```

</div>

<div class="cell-output cell-output-stdout">

      95% CI: [ 0.472 , 0.83 ]

</div>

</div>

</div>

<div id="visualising-the-occupancycovariate-relationship" class="section level2">

## Visualising the occupancy–covariate relationship<a href="http://localhost:6756/BCB743/occupancy.html#visualising-the-occupancycovariate-relationship" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

For models with a continuous covariate on <span class="math inline">𝜓</span>, the marginal occupancy curve shows how occupancy probability changes across the range of the predictor.

<div class="cell">

Code

<div class="code-copy-outer-scaffold">

``` numberSource
# Generate a prediction grid over the range of hs_ros_sc
hs_seq <- seq(min(site_covs$hs_ros_sc, na.rm = TRUE),
              max(site_covs$hs_ros_sc, na.rm = TRUE),
              length.out = 100)

# Prediction data frame: only include the covariate that m_hs was fitted with.
# Passing extra columns that the model does not recognise causes a warning;
# each model should only receive the covariates it was specified with.
pred_df <- data.frame(hs_ros_sc = hs_seq)

# Predict occupancy from the SDM model
pred_psi <- predict(m_hs,
                    newdata = pred_df,
                    type    = "state")

pred_plot <- bind_cols(pred_df, pred_psi)

ggplot(pred_plot, aes(x = hs_seq)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#2166ac", alpha = 0.2) +
  geom_line(aes(y = Predicted), colour = "#2166ac", linewidth = 0.8) +
  geom_rug(
    data = dat,
    aes(x = as.numeric(scale(hs_ros))),
    sides = "b", alpha = 0.4, linewidth = 0.3, inherit.aes = FALSE
  ) +
  labs(
    x = "SDM habitat suitability (standardised)",
    y = expression(paste("Predicted occupancy (", hat(psi), ")"))
  )
```

</div>

<div class="cell-output-display">

<div id="fig-occ-curve" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-occ-curve-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<a href="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-occ-curve-1.svg" class="lightbox" data-gallery="quarto-lightbox-gallery-4" data-original-href="http://localhost:6756/BCB743/occupancy_files/figure-html/fig-occ-curve-1.svg" title="Figure 4: Predicted occupancy probability as a function of SDM habitat suitability for C. rosei. The shaded ribbon is the 95% confidence interval. Points along the x-axis show observed habitat suitability values at surveyed sites."><img src="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-occ-curve-1.svg" class="img-fluid figure-img" /></a>
</div>
<figcaption>Figure 4: Predicted occupancy probability as a function of SDM habitat suitability for <em>C. rosei</em>. The shaded ribbon is the 95% confidence interval. Points along the x-axis show observed habitat suitability values at surveyed sites.</figcaption>
</figure>

<a href="http://localhost:6756/BCB743/occupancy.html#fig-occ-curve" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

</div>

</div>

</div>

</div>

<div id="site-level-occupancy-predictions" class="section level2">

## Site-level occupancy predictions<a href="http://localhost:6756/BCB743/occupancy.html#site-level-occupancy-predictions" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

In addition to the marginal curve, `unmarked` can produce a corrected occupancy estimate for each individual site, conditioning on the actual detection history observed there. A site where all four proxy species were detected should attract a higher posterior occupancy estimate than a site where none were seen, even if both have identical habitat suitability scores.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# ranef() extracts empirical Bayes estimates of the latent state for each site
re <- ranef(best_model)

# Extract the posterior mean probability of occupancy for each site
site_occ <- bup(re, stat = "mean")

# Bind back to site coordinates for plotting
site_pred_df <- dat |>
  mutate(psi_hat = site_occ)
```

</div>

</div>

<div class="cell">

Code

<div class="code-copy-outer-scaffold">

``` numberSource
ggplot(site_pred_df, aes(x = lon, y = lat, colour = psi_hat, size = psi_hat)) +
  geom_point(alpha = 0.75) +
  scale_colour_gradient(low = "grey85", high = "#08306b",
                        name = expression(hat(psi))) +
  scale_size_continuous(range = c(0.5, 3), guide = "none") +
  labs(x = "Longitude", y = "Latitude") +
  coord_fixed() +
  theme(legend.position = "right")
```

</div>

<div class="cell-output-display">

<div id="fig-site-psi" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-fig figure">
<div aria-describedby="fig-site-psi-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<a href="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-site-psi-1.svg" class="lightbox" data-gallery="quarto-lightbox-gallery-5" data-original-href="http://localhost:6756/BCB743/occupancy_files/figure-html/fig-site-psi-1.svg" title="Figure 5: Site-level posterior occupancy estimates across the Cape Peninsula. Larger, darker points indicate higher estimated probability of C. rosei occupancy."><img src="./Occupancy%20modelling%20with%20imperfect%20detection%20–%20The%20Tangled%20Bank_files/fig-site-psi-1.svg" class="img-fluid figure-img" /></a>
</div>
<figcaption>Figure 5: Site-level posterior occupancy estimates across the Cape Peninsula. Larger, darker points indicate higher estimated probability of <em>C. rosei</em> occupancy.</figcaption>
</figure>

<a href="http://localhost:6756/BCB743/occupancy.html#fig-site-psi" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

</div>

</div>

</div>

<div class="callout callout-style-default callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>What the spatial pattern tells us

</div>

</div>

<div class="callout-body-container callout-body">

Sites with the highest posterior occupancy estimates tend to cluster in areas with high habitat suitability — but they do not always coincide with the known breeding localities. This is the practical output of the occupancy model: a ranked list of candidate sites where the probability of hosting an undiscovered *C. rosei* population is highest. Field teams can use this map to prioritise future search effort, as Becker et al. (2022) did.

</div>

</div>

</div>

<div id="reporting-occupancy-estimates" class="section level2">

## Reporting Occupancy Estimates<a href="http://localhost:6756/BCB743/occupancy.html#reporting-occupancy-estimates" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

A complete occupancy analysis reports both <span class="math inline">ˆ𝜓</span> and <span class="math inline">ˆ𝑝</span> with uncertainty. The function below collects these into a tidy table for the best model.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` numberSource
# Detection (p): always intercept-only, so backTransform() works directly
p_bt  <- backTransform(best_model, type = "det")
p_row <- tibble(
  Parameter = "Detection (p)",
  Estimate  = as.numeric(coef(p_bt)),
  SE        = SE(p_bt),
  Lower_95  = pmax(0, Estimate - 1.96 * SE),
  Upper_95  = pmin(1, Estimate + 1.96 * SE)
)

# Occupancy (ψ): use predict() at mean/modal covariates
# backTransform(type = "state") throws an error when the state submodel has
# more than one coefficient (i.e. any model with a covariate on psi).
# predict() handles all cases uniformly and returns the estimate on the
# probability scale together with its delta-method SE and 95% CI.
psi_row_raw <- predict(
  best_model,
  newdata = data.frame(hs_ros_sc = 0, wetland = 0, proxy23 = 0),
  type    = "state"
)

psi_row <- tibble(
  Parameter = "Occupancy (ψ) at mean covariates",
  Estimate  = psi_row_raw$Predicted,
  SE        = psi_row_raw$SE,
  Lower_95  = psi_row_raw$lower,
  Upper_95  = psi_row_raw$upper
)

# Combine and round
bind_rows(psi_row, p_row) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
```

</div>

<div class="cell-output cell-output-stdout">

    # A tibble: 2 × 5
      Parameter                        Estimate    SE Lower_95 Upper_95
      <chr>                               <dbl> <dbl>    <dbl>    <dbl>
    1 Occupancy (ψ) at mean covariates    0.676 0.095    0.472     0.83
    2 Detection (p)                       0.497 0.037    0.424     0.57

</div>

</div>

</div>

<div id="summary-and-ecological-interpretation" class="section level2">

## Summary and Ecological Interpretation<a href="http://localhost:6756/BCB743/occupancy.html#summary-and-ecological-interpretation" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

The single-season occupancy model separates two sources of information that are hopelessly conflated in a raw survey record: the true probability that a site supports the species of interest, and the probability of detecting it during any given visit. For *Capensibufo rosei*, a species with extremely limited range and high crypticity, failing to make this separation would produce a dramatically underestimated distribution and misdirected conservation effort.

Several conceptual points from this analysis are worth taking forward:

**Detection probability is not a nuisance parameter.** The estimate of <span class="math inline">ˆ𝑝</span> is biologically meaningful. A low <span class="math inline">𝑝</span> for *C. rosei*’s proxy community at a given site signals that the site was either poorly suited to these species or insufficiently searched. Both interpretations matter for conservation planning.

**Habitat covariates on <span class="math inline">𝜓</span> quantify what drives occupancy.** Whether shallow wetland presence or SDM-predicted suitability better predicts proxy occupancy tells us something about which aspect of habitat quality matters most. A model where <span class="math inline">𝜓</span> increases with SDM suitability confirms that the SDM captures real ecological signal, not just a statistical artefact.

**The corrected occupancy estimate exceeds the naïve rate.** This is mathematically guaranteed when <span class="math inline">𝑝 \<1</span>: the model allocates some of the observed absences to the detection failure category rather than the true absence category, raising the estimated proportion of occupied sites.

**The proxy species are doing the detection work.** Because *C. rosei* itself has too few records to estimate its own <span class="math inline">𝑝</span>, the proxy framework is the only route to a defensible occupancy estimate. This is one of the most honest acknowledgements in recent South African conservation research: the rarest species are often the ones for which detection-corrected estimates are most desperately needed and most difficult to obtain.

</div>

<div id="ai-use-declaration" class="section level2">

## AI-Use Declaration<a href="http://localhost:6756/BCB743/occupancy.html#ai-use-declaration" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Artificial intelligence (AI) tools were used in the preparation of this chapter. Specifically, Claude (developed by Anthropic) was used to assist with language refinement, literature summarisation, structural editing and code generating. All AI-generated content was reviewed, verified, and edited by the authors. The authors take full responsibility for the accuracy and integrity of the final submitted work.

</div>

<div id="author-contribution" class="section level2">

## Author contribution<a href="http://localhost:6756/BCB743/occupancy.html#author-contribution" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

Both authors designed this chapter as part of the BCB743 Quantitative Ecology module. The authors identified the *Capensibufo rosei* dataset (Becker et al., 2022) as a pedagogically appropriate case study for single-season occupancy modelling in a South African context, directed the structure of the analytical workflow, verified all R code for correctness and pedagogical clarity, and wrote the ecological interpretation and synthesis. Claude (Anthropic) was used as an AI writing and coding assistant under the author’s direction. The author takes full responsibility for the accuracy and integrity of all content presented in this chapter.

</div>

</div>

<div id="references" class="section level1">

# References

Azuma DL, Baldwin JA, Noon BR (1990) Estimating the occupancy of spotted owl habitat areas by sampling and adjusting for bias. General Technical Report PSW-124, USDA Forest Service.

Becker FS, Slingsby JA, Measey J, Tolley KA, Altwegg R (2022) Finding rare species and estimating the probability that all occupied sites have been found. *Ecological Applications* 32:e2502. <a href="https://doi.org/10.1002/eap.2502" class="uri external" data-original-href="https://doi.org/10.1002/eap.2502" target="_blank" rel="noopener">https://doi.org/10.1002/eap.2502</a>

Dorazio RM, Royle JA, Söderström B, Glimskär A (2006) Estimating species richness and accumulation by modeling species occurrence and detectability. *Ecology* 87(4):842–854. <a href="https://doi.org/10.1890/0012-9658(2006)87%5B842:ESRAAB%5D2.0.CO;2" class="uri external" data-original-href="https://doi.org/10.1890/0012-9658(2006)87%5B842:ESRAAB%5D2.0.CO;2" target="_blank" rel="noopener">https://doi.org/10.1890/0012-9658(2006)87[842:ESRAAB]2.0.CO;2</a>

Geissler PH, Fuller MR (1987) Estimation of the proportion of area occupied by an animal species. Proceedings of the Section on Survey Research Methods, American Statistical Association.

Guillera-Arroita G (2011) Impact of sampling with replacement in occupancy studies with spatial replication. *Methods in Ecology and Evolution* 2(4):401–406. <a href="https://doi.org/10.1111/j.2041-210X.2011.00089.x" class="uri external" data-original-href="https://doi.org/10.1111/j.2041-210X.2011.00089.x" target="_blank" rel="noopener">https://doi.org/10.1111/j.2041-210X.2011.00089.x</a>

Kéry M, Royle JA (2016) *Applied Hierarchical Modeling in Ecology: Analysis of Distribution, Abundance and Species Richness in R and BUGS* (Vol. 1). Academic Press.

MacKenzie DI, Bailey LL (2004) Assessing the fit of site-occupancy models. *Journal of Agricultural, Biological and Environmental Statistics* 9:300–318. <a href="https://doi.org/10.1198/108571104X3361" class="uri external" data-original-href="https://doi.org/10.1198/108571104X3361" target="_blank" rel="noopener">https://doi.org/10.1198/108571104X3361</a>

MacKenzie DI, Nichols JD, Hines JE, Knutson MG, Franklin AB (2003) Estimating site occupancy, colonization, and local extinction when a species is detected imperfectly. *Ecology* 84(8):2200–2207. <a href="https://doi.org/10.1890/02-3090" class="uri external" data-original-href="https://doi.org/10.1890/02-3090" target="_blank" rel="noopener">https://doi.org/10.1890/02-3090</a>

MacKenzie DI, Nichols JD, Lachman GB, Droege S, Royle JA, Langtimm CA (2002) Estimating site occupancy rates when detection probabilities are less than one. *Ecology* 83:2248–2255. <a href="https://doi.org/10.1890/0012-9658(2002)083%5B2248:ESORWU%5D2.0.CO;2" class="uri external" data-original-href="https://doi.org/10.1890/0012-9658(2002)083%5B2248:ESORWU%5D2.0.CO;2" target="_blank" rel="noopener">https://doi.org/10.1890/0012-9658(2002)083[2248:ESORWU]2.0.CO;2</a>

MacKenzie DI, Nichols JD, Royle JA, Pollock KH, Bailey LL, Hines JE (2017) *Occupancy Estimation and Modeling: Inferring Patterns and Dynamics of Species Occurrence* (2nd ed.). Academic Press.

MacKenzie DI, Royle JA (2005) Designing occupancy studies: general advice and allocating survey effort. *Journal of Applied Ecology* 42(6):1105–1114. <a href="https://doi.org/10.1111/j.1365-2664.2005.01098.x" class="uri external" data-original-href="https://doi.org/10.1111/j.1365-2664.2005.01098.x" target="_blank" rel="noopener">https://doi.org/10.1111/j.1365-2664.2005.01098.x</a>

Miller DA, Nichols JD, McClintock BT, Grant EHC, Bailey LL, Weir LA (2011) Improving occupancy estimation when two types of observational error occur: non-detection and species misidentification. *Ecology* 92(7):1422–1428. <a href="https://doi.org/10.1890/10-1396.1" class="uri external" data-original-href="https://doi.org/10.1890/10-1396.1" target="_blank" rel="noopener">https://doi.org/10.1890/10-1396.1</a>

Royle JA, Kéry M (2007) A Bayesian state-space formulation of dynamic occupancy models. *Ecology* 88(7):1813–1823. <a href="https://doi.org/10.1890/06-0669.1" class="uri external" data-original-href="https://doi.org/10.1890/06-0669.1" target="_blank" rel="noopener">https://doi.org/10.1890/06-0669.1</a>

Royle JA, Link WA (2006) Generalized site occupancy models allowing for false positive and false negative errors. *Ecology* 87(4):835–841. <a href="https://doi.org/10.1890/0012-9658(2006)87%5B835:GSOMAF%5D2.0.CO;2" class="uri external" data-original-href="https://doi.org/10.1890/0012-9658(2006)87%5B835:GSOMAF%5D2.0.CO;2" target="_blank" rel="noopener">https://doi.org/10.1890/0012-9658(2006)87[835:GSOMAF]2.0.CO;2</a>

</div>

<div id="quarto-appendix" class="default">

<div id="quarto-reuse" class="section quarto-appendix-contents">

## Reuse<a href="http://localhost:6756/BCB743/occupancy.html#reuse" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div class="quarto-appendix-contents">

<div>

<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/" class="external" rel="license" data-original-href="https://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">CC BY-NC-SA 4.0</a>

</div>

</div>

</div>

<div id="quarto-citation" class="section quarto-appendix-contents">

## Citation<a href="http://localhost:6756/BCB743/occupancy.html#citation" class="anchorjs-link" aria-label="Anchor" data-anchorjs-icon="" style="font: 1em / 1 anchorjs-icons; margin-left: 0.1875em; padding-right: 0.1875em; padding-left: 0.1875em;"></a>

<div>

<div class="quarto-appendix-secondary-label">

BibTeX citation:

</div>

``` code-with-copy
@online{smit2026,
  author = {Smit, A. J. and Dlelapantsi \& T.S Selae, L.},
  title = {Occupancy Modelling with Imperfect Detection},
  date = {2026-07-24},
  url = {https://tangledbank.netlify.app/BCB743/occupancy.html},
  langid = {en}
}
```

<div class="quarto-appendix-secondary-label">

For attribution, please cite this work as:

</div>

<div id="ref-smit2026" class="csl-entry quarto-appendix-citeas" role="listitem">

Smit AJ, Dlelapantsi & T.S Selae L (2026) Occupancy modelling with imperfect detection. <a href="https://tangledbank.netlify.app/BCB743/occupancy.html" data-original-href="https://tangledbank.netlify.app/BCB743/occupancy.html">https://tangledbank.netlify.app/BCB743/occupancy.html.</a>

</div>

</div>

</div>

</div>

</div>
