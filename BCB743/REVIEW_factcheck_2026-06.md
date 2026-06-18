# BCB743 — Fact-check and Style Review

**Date:** 2026-06-15
**Scope:** All BCB743 chapters (26 `.qmd`), all practice/assessed Tasks (A, A1–A3, B–N), the integrative assignment, and the course index.
**Brief:** Facts-first technical review (ecological, statistical, mathematical, R-code accuracy), with a lighter pass on writing style against the *Academic Communicator* (SOUL.md) standard. Apply unambiguous fixes; report judgement calls.

**Method.** The corpus was reviewed in seven parallel passes against authoritative sources (the vegan and ade4 reference manuals, Legendre 2014 Appendix S1, Clarke 1993, purrr/mgcv/lme4 documentation, Smit et al. 2017). Where claims were data-dependent, they were checked against the actual data files (e.g. the Doubs environmental correlations were recomputed from `DoubsEnv.csv`; the seaweed species count from `SeaweedSpp.csv`; the pyrifos transformation from the vegan manual).

> **Note on numeric outputs.** R could not be executed in the review environment, so values produced by live code chunks (eigenvalue percentages, stress values, AIC, coefficients) could not be recomputed. Items that depend on rendered output are collected in [§4](#4-verify-against-rendered-output). Hard-coded prose numbers that sit alongside dynamically computed labels are the main maintenance risk.

---

## 1. Executive summary

The corpus is in good technical shape. The conceptual core of every chapter is sound: the chi-square geometry and double-zero argument in CA, the cause of the arch, detrending and rescaling in DCA, PCA-as-special-case of PCoA, the rank-vs-distance distinction in nMDS, the Baselga/Podani/Legendre β-diversity algebra, the REML-versus-ML handling in mixed models, Tjur's R², and the `adonis2` p = 1 diagnosis were all checked and are correct.

Issues cluster in three places: (a) a small number of precise statistical statements that are wrong or imprecise; (b) hard-coded numbers that can drift from the live code; and (c) two cross-document inconsistencies that need an author decision — the **assessment-weighting policy** and the **seaweed species count**.

I applied **32 unambiguous fixes** across 15 files ([§2](#2-fixes-applied)). The remaining items are judgement calls ([§3](#3-outstanding-items-author-decision)), render-checks ([§4](#4-verify-against-rendered-output)), and style notes ([§5](#5-style-assessment-soulmd)).

**Highest-priority items for the author (not auto-fixed):**

1. **Assessment policy contradiction** (Critical). The index Assessment Policy describes three assessed tasks (A1 Integrative, A2 Chapter Contribution, A3 spesim Review), student-weighted 20–40 % each to 100 %, with no separate "Textbook" component. `assessments/BCB743_integrative_assignment.qmd` instead states the Integrative Assignment is worth a fixed 50 % with "the Textbook" the other 50 %. These schemes are mutually exclusive and must be reconciled.
2. **Seaweed species count, 846 vs 847** (Major). The data file `SeaweedSpp.csv` contains **847** species columns (after the leading ID column the chapters drop with `select(-1)`); the published paper (Smit et al. 2017) reports **846**. Three files say 847, two say 846. Decide on one figure and add a one-line reconciliation.
3. **DCA gradient-length conclusion** (Major). `DCA.qmd` line 142 places gradients that "exceed 3 SD and approach 4" in the band it has just called "not clear-cut," yet concludes unimodal methods "are preferred." Either adopt the conventional 2 SD / 4 SD thresholds or soften the conclusion.

---

## 2. Fixes applied

All edits are unambiguous corrections of fact, code, or grammar. None changes a statistical result or a pedagogical judgement.

| # | File | Issue | Change made |
| :-- | :--- | :--- | :--- |
| 1 | `tasks/Task_A.qmd` | Ex. 2 names `oxy`-`bod` (−0.84) as 2nd-strongest negative; recomputed from `DoubsEnv.csv` the 2nd is `ele`-`dis` (−0.87), and `slice_min(n=2)` would print it — prose contradicted the code | Replaced with `ele`-`dis` (−0.87) |
| 2 | `tasks/Task_A.qmd` | Ex. 5 mechanism list followed the wrong pair | Swapped to `ele`-`dis`; kept `oxy`-`bod` as an explicitly-flagged extra example |
| 3 | `ordination.qmd` | DCA bullet said DCA "creates" the arch (it removes it) and that detrending "linearises the species response" (it does not) | Rewrote: CA produces the arch, DCA removes it by detrending; detrending rescales axes into SD of turnover and leaves unimodal responses unchanged |
| 4 | `ordination.qmd` | PCA "also applicable to species dissimilarities" (conflates PCA with PCoA) | "…also for species abundance data after an appropriate transformation (e.g. Hellinger)" |
| 5 | `ordination.qmd` | nMDS "works on … rank-order distance matrices" (input is a dissimilarity matrix) | "…works on a dissimilarity or distance matrix, preserving the rank order of the dissimilarities" |
| 6 | `randomisation.qmd` | `purrr::rerun()` recommended (deprecated in purrr 1.0.0) | `purrr::map(1:n, ~ ...)`, with deprecation note |
| 7 | `dis-metrics.qmd` | Canberra "can also be affected by the presence of zeros" (double zeros are excluded by vegan's NZ normalisation) | Rewrote: sensitive to *single* zeros/small values; double zeros excluded |
| 8 | `dis-metrics.qmd` | Gower introduced as a "similarity coefficient" then given a distance formula | "general dissimilarity coefficient" |
| 9 | `PCA.qmd` | `env_pca$CA$eig` labelled "eigenvectors" (it holds eigenvalues); self-contradicts the table below | "eigenvectors (`$CA$v`/`$CA$u`)…; the eigenvalues are in `$CA$eig`" |
| 10 | `PCA.qmd` | "only a few numbers has the message" | "…numbers carry the message" |
| 11 | `PCA_examples.qmd` | Code comment "species scores" on a line extracting site scores | "site scores (the individual flowers here)" |
| 12 | `PCA_SDG_example.qmd` | "collieanrity" | "collinearity" |
| 13 | `PCA_SDG_example.qmd` | "will benefit from a cluster analyses" | "would benefit from a cluster analysis" (+ "seems"→"seem") |
| 14 | `CA.qmd` | species scores "equivalently … linear combinations of the original species data" (contradicts the non-linear/reciprocal-averaging framing) | Rewrote as reciprocal averaging |
| 15 | `CA.qmd` | "Individual eigenvalues in CA can be greater than 1" (they lie in [0, 1]) | "…lie between 0 and 1 (each is a squared correlation…)" |
| 16 | `CA.qmd` | stray `)` breaking the GUSTA ME link | removed |
| 17 | `nMDS.qmd` | stress rule "<0.3 provides a poor representation" (self-contradictory) | Clarke (1993) bands: <0.05 excellent, <0.1 good, <0.2 usable, 0.2–0.3 poor, >0.3 arbitrary |
| 18 | `nMDS_diatoms.qmd` | `betadisper` H0 stated as "population variances are equal" | "multivariate dispersions (average distance to group centroid) are equal" |
| 19 | `unconstrained-summary.qmd` | CA data type "Categorical or count data" | "Count or frequency data (… contingency tables)" |
| 20 | `unconstrained-summary.qmd` | CA via "`formula = ~.`" (would give a CCA/error) | `cca(spe)` or `cca(spe ~ 1)` |
| 21 | `PCoA.qmd` | species scores described as "weighted sums of the community matrix" (×2) | "weighted averages of the site scores (wascores)" |
| 22 | `PCoA.qmd` | garbled "…which is why PCoA also offers constrained options, with the data on the left" | Rewrote: `~ 1` gives the unconstrained case; predictors on the right give db-RDA |
| 23 | `multiple_regression.qmd` | "four water-quality measures" then lists five | "five water-quality measures" |
| 24 | `multiple_regression.qmd` | "excesive VIFs" | "excessive" |
| 25 | `glm.qmd` | table header "Default link" but R defaults Gamma to inverse, not log | Header → "Typical link"; Gamma row → "log (R's default is inverse)" |
| 26 | `glm.qmd` | "establish it by checking it" (doubled object) | "…one you can establish only by checking it specifically" |
| 27 | `glm.qmd` | "heteroscedastic from the onset" | "from the outset" |
| 28 | `mixed_models.qmd` | "the interaction earns is real" (garbled) | "the interaction is real (it earns its place in the model)" |
| 29 | `tasks/Task_A3.qmd` | "The package as a real teaching product being considered…" (no verb) | "Treat the package as a real teaching product…" |
| 30 | `BCB743_index.qmd` | "Stephen J. Gould" | "Stephen Jay Gould" |
| 31 | `BCB743_index.qmd` | "completion of this modules alignment with … the workspace" (broken grammar) | "completion of this module align with … the workplace" |
| 32 | `tasks/Task_K.qmd` | stale hard-coded origination date `2024/07/02` | `date: last-modified` |

---

## 3. Outstanding items (author decision)

### 3.1 Critical

**Assessment-weighting contradiction** — `BCB743_index.qmd` (Assessment Policy) vs `assessments/BCB743_integrative_assignment.qmd`.
The index describes three student-weighted assessed tasks (each 20–40 %, summing to 100 %); the assignment file states a fixed 50 % Integrative + 50 % Textbook split. Reconcile to one scheme and update the other. **Related:** the index links the integrative assessment to `tasks/Task_A1.qmd`, while Tasks H and K link to `../assessments/BCB743_integrative_assignment.qmd`. Confirm which file is canonical and point all references at it. (Both files exist; `Task_A1.qmd`, `Task_A2.qmd`, `Task_A3.qmd` are the three assessed briefs.)

### 3.2 Major

| File | Item | Recommendation |
| :--- | :--- | :--- |
| `constrained_ordination.qmd`, `_v2.qmd`, `cluster_analysis.qmd` (847) vs `two_oceans_appendices.qmd`, `datasets.qmd` (846) | Seaweed species count. Data file = 847 species columns; paper = 846 | Pick one figure; add a note reconciling the data file (847) with the published 846 |
| `DCA.qmd` (≈ line 142) | Gradient-length conclusion places 3–4 SD data in the "not clear-cut" band yet "prefers" unimodal methods; lower threshold conventionally 2 SD, not 3 | Adopt 2 SD / 4 SD, or soften line 142 to "ambiguous band; fit and compare both" |
| `PCoA.qmd` | Negative-eigenvalue corrections (Cailliez, Lingoes, square-root) are never mentioned | Add to the negative-eigenvalue callout; note `ape::pcoa(correction=)`, `cmdscale(add=TRUE)` |
| `PCoA.qmd` (scree, ≈ line 244) | `variance_explained <- eigenvalues / sum(eigenvalues)` includes negative eigenvalues in the denominator, inconsistent with the file's own positive-only calculation elsewhere | Use `pos_eig <- eigenvalues[eigenvalues > 0]` for the denominator |
| `nMDS_diatoms.qmd` | `metaMDS(spp.log, …)` runs on log-transformed data with default `autotransform = TRUE`, risking a silent second transformation and desyncing the nMDS input from the PERMANOVA input | Set `autotransform = FALSE`; pass the same `spp.log.dis` to both |
| `tasks/Task_H.qmd` (Ex. 1) | `prop_constr <- dbrda$CCA$tot.chi / dbrda$tot.chi` is unreliable for a Bray-Curtis `capscale` object with negative eigenvalues | Read the constrained proportion from `summary()` and verify against the rendered ordination |
| `tasks/Task_N.qmd` (Ex. 1) | `rowSums(pyrifos)` labelled "total abundance" — pyrifos is log-transformed `ln(10·x+1)`, so the row sum is not a community total (the "ln-transformed" comment itself is **correct**, verified against the vegan manual) | Relabel as a "summed log-abundance / community-size index"; do not change "ln" |
| `tasks/Task_L.qmd` (Ex. 3–4) | `rowSums(sipoo, na.rm = TRUE)` / `na.omit()` imply missing values that the complete sipoo incidence matrix does not contain | Drop the defensive `na.rm`/`na.omit`, or comment that they are defensive only |
| `tasks/multiple_regression.qmd` predict step | `predict(final_model, new_site = data.frame(ele, oxy, bod))` errors unless `stepAIC` retains exactly those three terms | Verify the final model's terms match the supplied columns |

### 3.3 Minor (judgement)

- `correlations.qmd`: (i) "Correlation … (non-causal)" overstates — correlation is silent on causation, not non-causal by definition; (ii) Pearson's *r* needs no distributional assumption as a descriptor (only its significance test does); (iii) note that abundance `vegdist(method="jaccard")` is vegan's quantitative Jaccard `2B/(1+B)`, distinct from the binary set-Jaccard of the worked example; (iv) one sentence begins lower-case ("understood this way…").
- `CA.qmd`: `envfit(spe_ca, env, scaling = 2)` — the `scaling` argument and the "Scaling 2 is default" comment are misleading for `envfit`; verify or drop.
- `nMDS.qmd`: add a one-line note that `metaMDS()` applies `autotransform = TRUE` by default; add a forward pointer to the `betadisper`/PERMDISP dispersion caveat handled in the diatom chapter.
- `nMDS_diatoms.qmd`: note that `decostand(method="log")` uses base 2 by default.
- `deep_dive.qmd`: bioregion integer cuts (BMP 1–16, B-ATZ 17–21, …) differ from the prose ranges used elsewhere (BMP 1–17, B-ATZ 18–22, …); annotate as an operationalisation. Citation key `@bolton2002seaweed` is used for the four-bioregion scheme that is elsewhere `@Bolton2004`; verify. A "(To be updated…)" placeholder and a stale "Topic 3" comment remain.
- `two_oceans_appendices.qmd`: `\textbeta` / `\textsubscript` LaTeX macros will not render in HTML (use `$\beta$`); `sites.csv` vs `SeaweedSites.csv` path differs from sibling chapters (verify both exist); the manual `E.Y2.sign.ax <- "annMean"` hack will silently desync if the data/version change; `@Sauer1988` vs `@sauer1991plant` keys for the same quotation.
- `glm.qmd`: offset model interpretation ("share of the community") is defensible but `total` includes the focal species; a one-line clarification helps.
- `tasks/Task_I.qmd`: `plot(hc_fish, labels = grp)` relabels dendrogram tips with cluster numbers; `rownames(spe)` is more informative.
- `BCB743_index.qmd`: "Plagiarism … dealt with **concisely**" is almost certainly meant as "decisively" or "seriously" (word choice — left for you). The `Task_A` (practice) vs `Task_A1/A2/A3` (assessed) naming is collision-prone; consider renaming the assessed trio.
- `gam.qmd`: `s()` uses thin-plate regression splines by default; the "thin-plate or cubic regression splines" phrasing may imply cubic splines are in use.

---

## 4. Verify against rendered output

R was unavailable, so these hard-coded prose numbers should be checked against the rendered chunks (several sit beside dynamically computed labels and will drift if the data or package versions change):

- `correlations.qmd`: r ≈ −0.34 / −0.82 and 0.97 / 0.72; "p-values below 1e-8".
- `ordination.qmd`, `PCA.qmd`: PC1 ≈ 54.3 %, PC2 ≈ 19.7 %, cumulative ≈ 74 %; a commented line mislabels PC1's 54.3 % as "PC2".
- `CA.qmd`: total inertia ≈ 1.17; CA1 51.5 %, CA2 12.4 %.
- `PCoA.qmd`: imaginary/real split ≈ −0.30 / 7.06; MDS1 ≈ 52 %.
- `nMDS.qmd`: stress ≈ 0.07. `nMDS_diatoms.qmd`: stress ≈ 0.19; centroid 0.1 vs spread 0.5.
- `DCA.qmd`: both first-axis gradient lengths (confirm they fall in 3–4 SD).
- `constrained_ordination*.qmd`: adj R² ≈ 0.85, constrained ≈ 91 %, CAP1 ≈ 82 %, arch regressions 90 % / 53 %; Mantel r = 0.938 (v2) is hard-coded beside a live chunk.
- `cluster_analysis.qmd`: cophenetic ≈ 0.83 / 0.89; silhouette widths; South Africa HIV/life-expectancy figures.
- `multiple_regression.qmd`: which predictors VIF pruning and `stepAIC` remove, and the final model's significance claims.
- `glm.qmd`: dispersion ratio, θ, odds ratio "one third". `gam.qmd`: edf = 1 for elevation, AIC comparison, concurvity. `mixed_models.qmd`: ICC values, the singular-fit claim, n = 42 subsamples vs 16 plants (16 × 3 = 48, so several plants must have < 3 — confirm).

---

## 5. Style assessment (SOUL.md)

Reviewed lightly, per the facts-first brief. **Overall the corpus already meets the *Academic Communicator* standard well:** chapters open by framing what is being discussed and why, define terms before using them, lead with a concrete example (the two-site distance opener in PCoA, the iris biplot, the Doubs gradient), and vary sentence length effectively. The strongest chapters (PCA, CA, the constrained-ordination suite, mixed models) read like a lecturer who also publishes.

Recurring, mostly local departures worth a light edit pass:

- **Heavy nominalisations** in a few abstract lead sentences — e.g. "statistical adequacy and ecological sufficiency" (PCA), "the coherence of the river gradient" (Task A), "distributional asymmetries characteristic of ecological data" (CA). Each is unpacked immediately afterward; leading with the plain version first would let the abstraction land.
- **Defining by negation** as a rhetorical closer — "not a peculiarity of the data, but a consequence of…", "descriptive, not causal", "not a different structure". Effective in moderation; a couple per chapter is plenty.
- **Physical metaphors for abstract relations** — "the heart of constrained ordination", "pins the river gradient to…", "abandons the eigenvalue machinery", "the centre of gravity in BCB743". Mostly fine; the pear-and-shadow analogy in `ordination.qmd` does real work but runs long.
- **Repeated anthropomorphic verbs across the model chapters** — predictors "earn their complexity", "pay their way", smooths "chase" noise and "win"; the "honesty of the uncertainty" framing recurs in both `glm`/`gam` and `mixed_models`. Diversify.
- **Placeholders and register slips to resolve before publication** — "(To be updated…)" in `deep_dive.qmd`; "I was too lazy to write neater code" in `deep_dive`/`two_oceans`. The conversational asides ("something univariate statistics seriously frowns upon") are charming but occasionally undercut precision.
- **Pacing** — a handful of ~190-word single sentences (the PCA "species scores" terminology paragraph; the `unconstrained-summary` envfit/constrained/varpart paragraph) would read better split.

These are minor. No chapter needs structural rework on style grounds.

---

## 6. Files reviewed

Chapters: `review`, `correlations`, `dis-metrics`, `ordination`, `randomisation`, `PCA`, `PCA_examples`, `PCA_SDG_example`, `CA`, `DCA`, `PCoA`, `nMDS`, `nMDS_diatoms`, `unconstrained-summary`, `constrained_ordination`, `constrained_ordination_v2`, `two_oceans_appendices`, `deep_dive`, `cluster_analysis`, `model_building`, `multiple_regression`, `glm`, `gam`, `mixed_models` (plus the recently added `datasets`, `spesim`).
Tasks: `A`, `A1`, `A2`, `A3`, `B`–`N`; `assessments/BCB743_integrative_assignment`; `BCB743_index`.
