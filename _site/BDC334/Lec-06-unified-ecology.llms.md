> **NOTE:**
>
> **This material must be reviewed by BCB743 students in Week 1 of Quantitative Ecology.**

> **NOTE:**
>
> **Please see the [BDC334 Lecture Transcript](../BDC334/BDC334-Lecture-Transcripts.llms.md) for the main content of all lectures.**

> **TIP:**
>
> - [Lab 4. Species Distribution Patterns](../BDC334/Lab-04-biodiversity.llms.md)

> **TIP:**
>
> - [Matthews and Whittaker (2015)](../docs/Matthews_and_Whittaker_2015.pdf)
> - [Shade et al. (2018)](../docs/Shade_et_al_2018.pdf)

# 1 Learning outcomes

By the end of this lecture, you should be able to:

- explain why univariate diversity summaries are useful but insufficient for mechanistic ecological inference,
- distinguish species-distribution pattern families and what ecological questions each can address,
- connect $\alpha$-, $\beta$-, and $\gamma$-diversity ideas to broader multivariate pattern analysis,
- evaluate assumptions and interpretation limits when fitting community-level pattern models, and
- map theory-based decisions to practical implementation in [Lab 4](../BDC334/Lab-04-biodiversity.llms.md).

In this lecture, *pattern families* means recurring classes of community-level patterns such as SADs, OARs, rarefaction, and distance-decay. Each one is a different transformation of the same site × species matrix.

# 2 Data structure and notation

Assume a site × species matrix

$$\mathbf{Y} = [y_{ij}],\quad i=1,\dots,n\;\text{sites},\; j=1,\dots,S\;\text{species},$$

where $y_{ij}$ is abundance (or incidence) of species $j$ at site $i$.

From this matrix, define:

- local richness: $\alpha_i = \sum_{j=1}^{S} I(y_{ij}>0)$,
- mean local richness: $\bar{\alpha} = n^{-1}\sum_{i=1}^{n}\alpha_i$,
- regional richness: $\gamma = \sum_{j=1}^{S} I\!\left(\sum_{i=1}^{n}y_{ij}>0\right)$,
- additive $\beta$-diversity: $\beta_A = \gamma - \bar{\alpha}$,
- multiplicative $\beta$-diversity: $\beta_M = \gamma/\bar{\alpha}$.

For relative abundance $p_{ij}=y_{ij}/\sum_j y_{ij}$, common Lecture 4 indices follow as:

$$H'_i = -\sum_{j=1}^{S} p_{ij}\ln(p_{ij}),\qquad
\lambda_i = \sum_{j=1}^{S} p_{ij}^2.$$

This notation is useful because we can interpret each analysis in Lab 4 as a structured transformation of $\mathbf{Y}$.

``` mermaid
flowchart TD
    A["Site × species matrix (Y)"] --> B["Species totals"]
    B --> C["Species-abundance distribution(SAD)"]
    A --> D["Site occupancy"]
    D --> E["Occupancy-abundance relationship (OAR)"]
    A --> F["Cumulative sampling"]
    F --> G["Species accumulation curve"]
    A --> H["Resampling"]
    H --> I["Rarefaction curve"]
    A --> J["Pairwise dissimilarity"]
    J --> K["Distance-decay function"]
```

Matrix transformations: multiple pattern analyses from one site × species matrix.

## How pattern families transform the same matrix

Each pattern family is a different transformation of the same site × species matrix $\mathbf{Y}$.

| Pattern family | Transformation of $\mathbf{Y}$ | Typical computation |
|----|----|----|
| SADs | Aggregate by species: $N_j=\sum_i y_{ij}$, then relative abundance $q_j=N_j/\sum_j N_j$ | Sort $q_j$ (or $N_j$) to evaluate abundance-distribution shape |
| OARs | For each species: occupancy $O_j=\sum_i I(y_{ij}>0)$ and conditional abundance $\bar{A}_j=\sum_i y_{ij}/O_j$ | Regress or correlate $\bar{A}_j$ against $O_j$ (often on log scale) |
| Species accumulation / area curves | Cumulative richness over ordered/accumulated sites: $S(m)=\left|\bigcup_{i=1}^{m}\{j:y_{ij}>0\}\right|$ | Compute $S(m)$ across random permutations or spatially ordered additions |
| Rarefaction / extrapolation | Standardise to comparable effort from pooled counts | Estimate expected richness at fixed sample size/coverage |
| Distance-decay | Build community dissimilarity matrix from rows of $\mathbf{Y}$: $D^{(Y)}_{ik}=d(\mathbf{y}_{i*},\mathbf{y}_{k*})$, compare to geographic/environmental distance $d_{ik}$ | Model similarity (or dissimilarity) as a function of distance |
| Directional turnover | Order sites along gradient $g_i$, then evaluate pairwise/community change along that ordering | Compute dissimilarity along gradient position or adjacent-site contrasts |

This is what the code is doing in Lab 4: each workflow selects a transformation of $\mathbf{Y}$, estimates a pattern, and then evaluates whether the pattern supports a process-level interpretation.

# 3 Why “unified ecology”?

Lecture 6 is a synthesis lecture. It integrates ideas developed in:

- [Lecture 2. Overview of Ecosystems](../BDC334/Lec-02-ecosystems.llms.md): biodiversity is the core ecological concept; it has scale dependence and leads to a macroecological framing;
- [Lecture 3. Ecological Gradients](../BDC334/Lec-03-gradients.llms.md): species responses are individualistic, have niche (and neutral) structure, and result in turnover along environmental gradients;
- [Lecture 4: Biodiversity Concepts](../BDC334/Lec-04-biodiversity.llms.md): $\alpha$–$\beta$–$\gamma$ partitioning, diversity indices, and multivariate resemblance.

**One community dataset can be viewed through several complementary analytical lenses**. Richness, diversity indices, dissimilarity matrices, and species-distribution patterns are not rival descriptions. They are different views of the same underlying structure.

A practical implication follows. If we report only one metric (for example Shannon diversity), we compress community structure into a single value and lose information needed for process-level inference. Different assembly processes can produce similar univariate outcomes. Strong inference therefore requires triangulation across pattern families.

# 4 Macroecological principles → community inference

Lecture 2 highlighted that macroecology seeks general principles from large comparative datasets and linked local ecological mechanisms to broad-scale pattern. Lecture 6 applies that view directly to site × species tables. The goal is to describe local communities *and* to determine whether recurring patterns are consistent with hypothesised process classes such as:

- environmental filtering (niche),
- dispersal limitation and/or stochastic demographic dynamics (neutral),
- mixed deterministic–stochastic assembly.

This transition from description to explanation is the core of unified ecology.

# 5 Gradient → patterns

Lecture 3 showed that species often display unimodal responses across gradients (coenoclines), with different optima and tolerance ranges and with realised distributions constrained by biotic interactions. At the community level, overlapping species responses generate compositional turnover. Lecture 6 extends this by asking:

- how turnover appears in dissimilarity and distance-decay space,
- whether occupancy and abundance co-vary with gradient position,
- whether observed richness trajectories reflect ecological structure or sampling effects.

Gradient ecology provides the mechanistic layer for the pattern families used here. Lecture 3 asked why communities differ; this lecture asks how those differences appear when we analyse the data in several complementary ways.

# 6 Measures → models

We must recognise an important distinction between **descriptive measures** and **inferential models**.

- Measures (e.g., richness, Shannon, Bray-Curtis) describe observed structure.
- Models evaluate whether candidate mechanisms could plausibly generate that structure.

For example, two communities can have similar Shannon diversity but different structure: Community A may contain many moderately common species, whereas Community B may contain one dominant species and many rare species. SAD analysis can separate these structures, while Shannon alone cannot.

Pattern description is necessary, but it is not causal proof. Mechanistic interpretation requires:

1.  explicit competing hypotheses,
2.  predicted signatures for each hypothesis,
3.  assumptions that are stated and checked,
4.  uncertainty reporting and sensitivity checks.

This distinction is captured by the progression from Lecture 2 → Lecture 4: theoretical underpinning → pattern quantification → constrained inference.

# 7 Core pattern families from one matrix

Following Shade et al. ([2018](#ref-shade2018macroecology)), one site × species table can support multiple complementary pattern families:

- species-abundance distributions (SADs),
- occupancy-abundance relationships (OARs),
- species accumulation / species-area curves,
- rarefaction and extrapolation curves,
- distance-decay functions,
- directional turnover along environmental gradients.

``` mermaid
flowchart LR
    A["Site × species matrix (Y)"] --> B["Pattern families"]
    B --> C["Ecological inference"]
```

Analytical flow from data object to ecological inference.

| Pattern family | Primary ecological focus | Typical interpretive use |
|----|----|----|
| Species-abundance distributions (SADs) | Dominance, commonness, rarity | Diagnose abundance structure and candidate assembly regimes |
| Occupancy-abundance relationships (OARs) | Coupling between site occupancy and local abundance | Evaluate range-abundance structure and detectability-aware interpretation |
| Species accumulation / species-area curves | Richness growth with effort or area | Assess sampling completeness and scaling behaviour |
| Rarefaction / extrapolation | Richness under standardised sampling effort | Make fair diversity comparisons under unequal sample sizes |
| Distance-decay functions | Similarity loss with geographic/environmental distance | Evaluate turnover intensity and possible filtering/dispersion processes |
| Directional turnover along gradients | Compositional change along ordered environmental axes | Link pattern shifts to gradient-based mechanisms from Lecture 3 |

Each family preserves a different part of community structure. Inferential strength increases when conclusions are consistent across more than one family.

# 8 Linking alpha, beta, and gamma to pattern analysis

In Lecture 4 we developed biodiversity partitioning and resemblance-based approaches as a theoretical foundation. In the unified ecological framework, partitioning becomes a means to derive interpretations.

- $\alpha$ reflects local assembly outcomes (dominance, evenness, local constraints).
- $\beta$ reflects among-site replacement and nestedness structure.
- $\gamma$ reflects the size of the regional species pool and its historical/environmental constraints.

| Pattern family | Biodiversity component emphasised |
|----|----|
| SAD | $\alpha$ structure (evenness, dominance) |
| OAR | Population distribution across sites (occupancy-abundance structure) |
| Species accumulation | $\gamma$ estimation |
| Rarefaction | Standardised $\alpha$ comparison |
| Distance-decay | $\beta$ turnover |

This first table is a high-level guide to the dominant biodiversity component each pattern family emphasises. The next table adds detail by showing how each family can still carry secondary $\alpha$-, $\beta$-, and $\gamma$-signals that matter during interpretation.

| Pattern family | Dominant $\alpha$ signal | Dominant $\beta$ signal | Dominant $\gamma$ signal |
|----|----|----|----|
| SADs | Local dominance/evenness structure within sites | Indirect (via among-site shifts in dominance profiles) | Constrained by size/composition of regional species pool |
| OARs | Mean local abundance of occupied species | Occupancy variation among sites | Upper occupancy-abundance envelope shaped by regional pool |
| Species accumulation / area curves | Early slope reflects local richness contribution | Rate of new-species appearance across sites (turnover) | Asymptote approaches regional richness |
| Rarefaction / extrapolation | Standardised local richness/evenness comparisons | Limited direct signal | Extrapolated richness constrained by regional pool and sampling domain |
| Distance-decay | Indirect local contribution through within-site composition | Primary signal: compositional turnover with distance | Regional context sets maximum attainable dissimilarity structure |
| Directional turnover along gradients | Local composition at each gradient position | Primary signal: replacement/nestedness along ordered gradient | Regional pool determines which taxa can enter/exit along gradient |

The patterns we will uncover in Lab 4 can be interpreted through the views stemming from these two tables. For example, steep SADs and high dominance often align with low local evenness ($\alpha$ structure), while strong distance-decay typically signals elevated compositional turnover ($\beta$ structure).

# 9 Practical integration with Lab 4 workflow

[Lab 4](../BDC334/Lab-04-biodiversity.llms.md) is the implementation companion to this lecture. Use the same decision sequence in each practical:

1.  define the ecological question (e.g. do communities become more dissimilar with increasing environmental distance?),
2.  choose the most informative pattern family (e.g. distance-decay analysis),
3.  standardise/transform data appropriately (e.g. transform species abundances when required),
4.  fit and visualise (e.g. fit an exponential decay model to dissimilarity vs distance),
5.  interpret against assumptions (e.g. check spatial autocorrelation and sampling comparability),
6.  cross-validate interpretation with at least one additional pattern family (e.g. compare with turnover along measured gradients).

## 9.1 A. Species-abundance distributions (Practical A)

See [Practical A](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-sad).

> **NOTE:**
>
> **Theory:** The species-abundance distribution describes the frequency distribution of species abundances.
>
> **Statistical object:** Distribution of column sums of $\mathbf{Y}$, i.e. $N_j=\sum_i y_{ij}$.
>
> **Lab implementation:** Species counts are extracted from the site × species matrix and ranked or grouped to estimate abundance-distribution shape.
>
> **Example tools in R:** `vegan::radfit()`, `vegan::radlattice()`, `vegan::fisherfit()`, `vegan::prestondistr()`.

SADs describe dominance structure, commonness, and rarity. For this lecture, use log-series and log-normal as the two core model examples (with other forms treated as extensions). A low AIC is useful, but ecological interpretation should also consider plausibility and robustness to data handling.

## 9.2 B. Occupancy-abundance relationships (Practical B)

See [Practical B](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-oar).

> **NOTE:**
>
> **Theory:** Occupancy-abundance analysis tests whether widespread species are also locally abundant.
>
> **Statistical object:** Relationship between column occupancy $O_j=\sum_i I(y_{ij}>0)$ and column mean abundance $\bar{A}_j=\sum_i y_{ij}/O_j$.
>
> **Lab implementation:** Compute occupancy and conditional mean abundance per species, then model/plot their association (often on log scale).
>
> **Example tools in R:** `colSums()`, `dplyr`, `ggplot2::geom_point()`, `stats::lm()`.

Define occupancy for species $j$ as:

$$O_j = \sum_{i=1}^{n} I(y_{ij}>0),$$

and conditional mean local abundance as:

$$\bar{A}_j = \frac{\sum_{i=1}^{n}y_{ij}}{O_j},\quad O_j>0.$$

The OAR is usually positive but can deviate under strong habitat specificity, fragmented occupancy, or sampling artifacts. Interpretation should separate ecological signals from detectability effects.

## 9.3 C. Species accumulation and species-area curves (Practical C)

See [Practical C](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-accum).

> **NOTE:**
>
> **Theory:** Species accumulation/area curves describe how observed richness increases as sampling units or sampled area increase.
>
> **Statistical object:** Cumulative richness function over added sites, $S(m)=\left|\bigcup_{i=1}^{m}\{j:y_{ij}>0\}\right|$.
>
> **Lab implementation:** Add sites in specified order(s), recompute cumulative richness, and summarise with envelopes or fitted forms.
>
> **Example tools in R:** `vegan::specaccum()`, `vegan::fitspecaccum()`.

These analyses quantify how richness grows with sampling effort and area. Initial slope reflects discovery rate; flattening indicates diminishing returns. Permutation envelopes are not optional decoration—they are the uncertainty structure of the curve.

Where fitted models such as Arrhenius ($S=cA^z$) are used, compare $z$ across systems cautiously and in context of sampling design and spatial grain.

## 9.4 D. Rarefaction and extrapolation (Practical D)

See [Practical D](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-rare).

> **NOTE:**
>
> **Theory:** Rarefaction/extrapolation standardises biodiversity comparison to common effort or sample completeness.
>
> **Statistical object:** Expected richness (or Hill diversity) conditional on standardised sample size/coverage from $\mathbf{Y}$.
>
> **Lab implementation:** Re-sample observed communities to estimate expected diversity at matched effort, then compare curves with uncertainty.
>
> **Example tools in R:** `vegan::rarecurve()`, `vegan::rarefy()`, `iNEXT::iNEXT()`.

Rarefaction standardises effort and enables fair comparisons among samples with unequal counts. Keep the methodological distinction explicit:

- accumulation traces observed discovery as effort increases,
- rarefaction estimates expected richness under standardised effort.

Failing to separate these leads to one of the most common interpretation errors in biodiversity analysis.

## 9.5 E. Distance-decay and gradient turnover (Practical E)

See [Practical E](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-dd).

> **NOTE:**
>
> **Theory:** Distance-decay quantifies how community similarity declines with increasing geographic or environmental separation.
>
> **Statistical object:** Regression of a community dissimilarity matrix against a geographic/environmental distance matrix.
>
> **Lab implementation:** Compute pairwise community dissimilarities from rows of $\mathbf{Y}$, pair with site-distance values, and estimate decay rate.
>
> **Example tools in R:** `vegan::vegdist()`, `stats::dist()`, `stats::lm()`, `vegan::mantel()`.

Distance-decay can be represented as:

$$\text{Similarity}_{ij} = a\,e^{-b d_{ij}},$$

where $d_{ij}$ is geographic or environmental distance and $b$ is decay rate.

In practice, similarity is typically derived from species dissimilarity matrices (Lecture 4), commonly using Bray-Curtis for abundance data or Jaccard/Sørensen for presence-absence data.

Interpretation should follow Lecture 3 ideas: gradients are often proxy axes. Species respond to proximate environmental factors covarying with elevation, latitude, depth, or spatial separation. Distance-decay patterns should therefore be interpreted alongside environmental-distance structure (Lecture 4) rather than in geographic space alone.

# 10 Assumptions, diagnostics, and interpretation

Inference quality depends less on software choice than on assumption handling. A minimum checklist is:

1.  **Sampling comparability** (or explicit standardisation),
2.  **Detection process awareness** (absence vs non-detection),
3.  **Scale matching** (grain/extent aligned with hypothesis),
4.  **Metric suitability** (index meaning fits data type),
5.  **Dependence structure** (spatial autocorrelation and pseudoreplication),
6.  **Uncertainty reporting** (intervals, permutations, sensitivity analyses).

Apply this checklist explicitly in Lab 4 write-ups.

# 11 Hypothesis-testing frame for unified ecology

A defensible sequence is:

1.  state competing process hypotheses,
2.  predict expected signatures across at least two pattern families,
3.  estimate patterns/models with uncertainty,
4.  evaluate concordance or conflict among outputs,
5.  conclude conditionally and state remaining uncertainty.

This prevents over-interpretation and aligns with the module’s broader quantitative philosophy.

# 12 Common failures

Frequent errors include:

- treating descriptive outputs as causal proof,
- comparing raw richness under unequal effort,
- mixing incidence and abundance frameworks without clarification,
- over-interpreting ordination geometry,
- selecting “best” models on AIC alone without ecological diagnostics.

Corrective strategy: triangulate pattern families, align claims to assumptions, and report limitations directly in the main interpretation.

# 13 Bridge to Lab 4 demonstrations

Use the following links while completing [Lab 4](../BDC334/Lab-04-biodiversity.llms.md):

- [Practical A. SADs](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-sad)
- [Practical B. OARs](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-oar)
- [Practical C. Species accumulation](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-accum)
- [Practical D. Rarefaction](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-rare)
- [Practical E. Distance-decay and gradients](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-dd)
- [Interpretation Checklist](../BDC334/Lab-04-biodiversity.llms.md#sec-lab04-interpret)

A robust reporting structure for tests/exams is:

1.  question and hypothesis,
2.  data structure and preparation,
3.  analytical choice and justification,
4.  key empirical result,
5.  uncertainty and assumptions,
6.  ecological interpretation and limits.

# 14 Example questions

> **NOTE:**
>
> **Question 1. Integrating Lectures 2–4 into unified ecology**
>
> 1.  Explain how macroecological framing from Lecture 2 motivates the use of multiple pattern families for one community dataset. (5)
>
> 2.  Explain how the gradient framework from Lecture 3 informs interpretation of distance-decay and occupancy-abundance relationships. (7)
>
> 3.  Show how $\alpha$-, $\beta$-, and $\gamma$-diversity from Lecture 4 can be used to interpret species-distribution patterns in Lab 4. (8)
>
> Total: **20 marks**
>
> ------------------------------------------------------------------------
>
> **Question 2. Measures vs models in ecological inference**
>
> 1.  Distinguish clearly between descriptive biodiversity measures and inferential ecological models. (6)
>
> 2.  Provide two examples where similar univariate diversity values could arise from different underlying community structures. (6)
>
> 3.  Propose an analysis strategy using at least two pattern families to discriminate among competing assembly hypotheses. (8)
>
> Total: **20 marks**
>
> ------------------------------------------------------------------------
>
> **Question 3. Assumptions and interpretation quality**
>
> 1.  Explain why standardisation of sampling effort is essential when comparing biodiversity among communities. (4)
>
> 2.  Describe three assumption checks that should accompany distance-decay analysis. (6)
>
> 3.  Explain why AIC-based model ranking is insufficient on its own for ecological interpretation. (4)
>
> 4.  Provide a short reporting template that communicates result, uncertainty, and limitations in one paragraph. (6)
>
> Total: **20 marks**

# 15 Summary

Unified ecology is the coordinated interpretation of biodiversity partitioning, gradient structure, and species-distribution pattern families within one inferential workflow. The objective is to produce explanations that are transparent, testable, and ecologically defensible, rather than to maximise the number of indices reported. [Lab 4](../BDC334/Lab-04-biodiversity.llms.md) operationalises this framework through reproducible pattern analyses and interpretation discipline.

## References

Shade A, Dunn RR, Blowes SA, Keil P, Bohannan BJ, Herrmann M, Küsel K, Lennon JT, Sanders NJ, Storch D, others (2018) Macroecology to unite all life, large and small. Trends in ecology & evolution 33:731–744.

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit2026,
  author = {Smit, A. J. and J. Smit, A.},
  title = {Lecture 6: {Unified} Ecology},
  date = {2026-08-19},
  url = {https://tangledbank.netlify.app/BDC334/Lec-06-unified-ecology.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ, J. Smit A (2026) Lecture 6: Unified ecology. [https://tangledbank.netlify.app/BDC334/Lec-06-unified-ecology.html.](https://tangledbank.netlify.app/BDC334/Lec-06-unified-ecology.html)
