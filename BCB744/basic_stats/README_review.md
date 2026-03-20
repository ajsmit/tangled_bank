# BCB744 Basic Stats – Critical Review Report

**Date:** 2026-03-20
**Scope:** Full read of all 25 `.qmd` files in `BCB744/basic_stats/`.
**Standard reference:** `README_standard.md` and `README_progress.md`.

---

## Overview

The sequence is in good shape overall. All twenty-five chapters open correctly, the core worked-example chapters (07–09, 11, 13–16, 21, 24) follow the gold-standard workflow well, and the three placeholder chapters (12, 20, 22) are clearly flagged. What follows is a chapter-by-chapter list of specific defects, followed by a cross-cutting section that groups issues appearing in more than one file.

---

## Chapter-by-Chapter Findings

### 01 – Statistical Landscape

No defects detected. Well-structured introductory chapter with no code to check.

---

### 02 – Summarise and Describe

Status: Updated on 2026-03-20.

- **Addressed.** The grouped-statistics section (ChickWeight example) now ends with a formal `Reporting` callout containing Methods / Results / Discussion.
- **"Do It Now!" tasks** are not part of the gold standard template but add pedagogical value; their use is acceptable.

---

### 03 – Visualise

Status: Updated on 2026-03-20.

- **Addressed.** The `random_not.csv` exercise file is now present at `data/BCB744/random_not.csv`, which matches the path used in the chapter.
- No other defects. Reporting is not expected for a visualisation chapter.

---

### 04 – Distributions, Sampling, and Uncertainty

Status: Updated on 2026-03-20.

- **Addressed.** The simple confidence-interval example now includes interpretive sentences linking the computed interval back to uncertainty and precision.
- No other defects.

---

### 05 – Inference

No defects detected. The hypothesis-testing logic, Type I/II error discussion, and general reporting requirements box are all excellent.

---

### 06 – Assumptions and Transformations

No defects detected. Two full Reporting boxes present and well written.

---

### 07 – t-Tests

Status: Updated on 2026-03-20.

- **Addressed.** The typo in the "In This Chapter" bullet has been corrected to "Two-sample *t*-tests".
- All other elements are exemplary. This chapter is the de facto structural model for the sequence.

---

### 08 – ANOVA

No defects detected. Both Reporting boxes are complete and well-written. Non-parametric alternatives are handled cleanly.

---

### 09 – Correlation and Association

Status: Updated on 2026-03-20.

- **Addressed.** `fig-corr1` now uses `theme_grey()`.
- **Addressed.** The unused `ggthemes` import was removed.
- **Addressed.** The YAML was normalised: the missing `author` field was added and the one-off `number-sections` inconsistency was removed as part of the sequence-wide cleanup.
- **Addressed.** The Spearman and Kendall Reporting boxes now report concrete numerical outcomes.
- **Addressed.** Example 2 (Spearman) now includes an EDA step before the test.
- **Addressed.** Examples 2 and 3 now include explicit displayed `H_{0}` and `H_{a}` equations.
- Remaining issue: none noted from this review pass.

---

### 10 – Choosing the Right Test

Status: Updated on 2026-03-20.

- **Addressed.** The `Tasks to Complete` callout was added.
- **Addressed.** The missing `author` field was added to the YAML.
- **Addressed at sequence level.** The YAML inconsistency around `number-sections` was resolved as part of the sequence-wide cleanup.
- **Very short chapter.** The chapter is prose-only with no code or visual decision aid. The existing TODO in `README_progress.md` already flags the missing visual decision aid; that remains the main gap.

---

### 11 – Simple Linear Regression

Status: Updated on 2026-03-20.

- **Addressed.** The duplicate hypothesis display in Example 2 was removed.
- **Addressed.** The library chunk was renamed to `code-libraries`.
- **Addressed.** The `Outliers` section was moved to after `Core Equations`.
- **Addressed.** `fig-penguin-results` was moved to just before the Reporting callout and is now referenced from within the write-up text.
- **Addressed at sequence level.** The YAML inconsistency around `number-sections` was resolved sequence-wide.

---

### 12 – Polynomial Regression

Status: Updated on 2026-03-20. Still a placeholder chapter, as already flagged in the progress tracker.

Additional notes beyond the tracker:

- **Addressed.** The chapter now includes the standard `ggplot2::theme_set(ggplot2::theme_grey(base_size = 8))` call in the setup chunk.
- **Addressed.** A `Tasks to Complete` callout is present.
- Remaining issue: the chapter is still a structured placeholder and still needs a full worked example before it can be considered complete.

---

### 13 – Multiple Regression and Model Specification

Status: Updated on 2026-03-20.

- **Addressed.** `aes_string()` was replaced with tidy evaluation using `.data[[predictor]]`.
- **Addressed.** `ggfortify` usage was verified; it is needed because `autoplot()` is used for model diagnostics. No removal required on that point.
- `viridis` remains in use for palette setup and is not a defect from this review item.
- **`Tasks to Complete: None`.** Fine, but the callout is present.

---

### 14 – Interaction Effects

Status: Updated on 2026-03-20.

- **Addressed.** The interaction-hypotheses section now displays both `H_{0}` and `H_{a}` as consecutive equations.
- **Only `tidyverse` is loaded.** This is fine for the current content, but if additional visualisation tools are needed when a centred-predictor example is added (see TODO), the library block will need expanding.

---

### 15 – Collinearity, Confounding, and Measurement Error

Status: Updated on 2026-03-20.

- **Addressed.** The `Graham2003` key was verified in the project bibliography.
- **Addressed.** The confounding section now contains a full worked example with simulated biological-look-alike data, including EDA, hypotheses, competing models, interpretation, and a numerical Reporting box.
- `Tasks to Complete: None` is present.

---

### 16 – Model Checking and Evaluation

Status: Updated on 2026-03-20.

- **Addressed.** Dedicated `R Functions` and `Nature of the Data and Assumptions` sections were added near the start of the chapter.
- **Addressed.** `MASS` was removed from the setup chunk because it was not needed here.
- **Addressed.** The chapter now includes a worked 10-fold cross-validation example comparing the selected and larger seaweed models, together with interpretation of the predictive trade-off.

---

### 17 – Pseudoreplication

Status: Updated on 2026-03-20.

- **Addressed.** The major section headings were promoted from `##` to `#`.
- **Addressed.** `reference-location: margin` was added to the YAML; the absence of a knitr setup block remains intentional because the chapter contains no R code.
- **Addressed.** Example 4 now reports the seasonal comparison as a result rather than restating the method.
- Remaining issue: none noted from this review pass.

---

### 18 – Dependence and Mixed Models

Status: Updated on 2026-03-20.

- **Addressed.** A `Tasks to Complete` callout was added.
- **Addressed.** The missing `author` field issue noted in the original review has been resolved.
- **No worked example, no EDA, no Reporting box.** Already acknowledged in the progress tracker (add a small repeated-measures or site-level random-effect example). This is the most functionally incomplete chapter in the "complete" set. At present it is purely conceptual with only `#| eval: false` code stubs.

---

### 19 – Generalised Linear Models

Status: Updated on 2026-03-20.

- **Addressed.** The Reporting box now uses a concrete fitted illustrative example rather than placeholder language.
- **Addressed.** A `Tasks to Complete` callout is present.
- **Addressed.** The setup block now includes `ggplot2::theme_set()`.
- Remaining issue: the chapter still remains lighter on worked examples than the core regression chapters, as already noted in the progress tracker.

---

### 20 – Generalised Additive Models

Status: Updated on 2026-03-20. Still a placeholder chapter, as already flagged in the progress tracker.

Additional note:

- **Addressed.** The chapter has an `author` field in the YAML.
- **Addressed.** A `Tasks to Complete in This Chapter` callout is present.
- Remaining issue: the chapter is still a structured placeholder and still needs a full worked example and final Reporting section before it can be considered complete.

---

### 21 – Nonlinear Regression

Status: Updated on 2026-03-20.

- **Addressed.** The theme base size was changed from 11 to 8.
- **Addressed.** The chapter-level `dev = "svglite"` setting was removed.
- **Addressed with clarification.** `mgcv` remains loaded because it is used for the GAM panel in `fig-main-families`, and the setup block now includes a note explaining that the dedicated GAM workflow belongs to Chapter 20.

---

### 22 – Quantile Regression

**Status: Placeholder.** Already flagged in progress tracker.

Additional note: No `Tasks to Complete` callout and no `ggplot2::theme_set()` in setup block.

---

### 23 – Prediction and Explanation

Status: Updated on 2026-03-20.

- **Addressed.** A `Tasks to Complete` callout was added.
- **Pure prose, no code executed, no example.** Already flagged in the progress tracker (add a paired example showing the same dataset motivating different model choices under explanatory vs. predictive aims). The chapter is useful as a framing essay but it has no analysis of its own.

---

### 24 – Regularisation

Status: Updated on 2026-03-20.

- **Addressed.** The data-loading chunk was moved to after the opening callouts in a labelled setup block.
- **Addressed.** The core equations now use consistent `\hat{Y}` notation.
- **Addressed.** The setup block now includes `ggplot2::theme_set()`.
- Remaining issue: none noted from this review pass.

---

### 25 – Reproducible Workflow

Status: Updated on 2026-03-20.

- **Addressed.** A `Tasks to Complete` callout was added.
- **Very thin: no concrete Quarto example.** Already flagged in the progress tracker (add a concrete Quarto-based example from this project). As written, the chapter is a brief set of principles without any code, workflow demonstration, or project structure illustration.

---

## Cross-Cutting Issues

These issues appear in more than one chapter or affect the sequence as a whole.

### 1. YAML Header Inconsistency

Status: Resolved on 2026-03-20. `author` and `reference-location: margin` were normalised across the sequence, and the one-off `number-sections: true` inconsistency was removed.

The following YAML fields are applied inconsistently across the 25 files:

| Field | Present in | Absent from |
|---|---|---|
| `author: "A. J. Smit"` | 11–25 (most) | 09, 10 |
| `number-sections: true` | 09 only | all others |
| `reference-location: margin` | 09, 11, 13, 14, 15, 16, 21, 23, 24 | others |

A decision should be made project-wide and applied uniformly. In particular, `number-sections: true` in Chapter 09 alone produces a different HTML structure for that one chapter.

### 2. Placeholder Reporting Boxes (Results Field Not Filled)

Status: Resolved on 2026-03-20. The cited placeholder Results fields in Chapters 09, 17, and 19 were replaced with concrete example-specific reporting text.

Four Reporting boxes across the sequence contain generic meta-descriptions rather than actual results:

- **Ch09, Example 2 (Spearman):** "In a journal Results section, the Spearman coefficient, sample size, and *p*-value would be reported directly from the `cor.test()` output…"
- **Ch09, Example 3 (Kendall):** "In a Marine Biology-style Results section, the Kendall coefficient, sample size, and *p*-value would be reported together…"
- **Ch17, Example 4 Reporting:** "The analysis therefore quantified the seasonal contrast using variation among replicated bay or year units…" (method restatement, not a result)
- **Ch19, Reporting box:** "The fitted model indicated that the probability of occurrence changed systematically…" (generic template)

In each case the Results field should be replaced with a statement that reports a specific numerical outcome from the example just analysed, in the style required by the Reporting standard.

### 3. Hypothesis Format Violations

Status: Resolved on 2026-03-20. The missing displayed alternatives were added in Chapter 09 Examples 2 and 3 and in the Chapter 14 interaction section.

The hypothesis standard requires two consecutive displayed equations (`$$H_{0}...$$` then `$$H_{a}...$$`) with a brief biological-question lead sentence and a symbol explanation sentence.

- **Ch09, Examples 2 and 3:** No `H_{a}` is displayed at all. Hypotheses are stated only verbally.
- **Ch14, interaction hypotheses:** Only `H_{0}` is displayed as an equation; `H_{a}` is described in prose.

### 4. Figure Theme Inconsistency

Status: Resolved on 2026-03-20. `fig-corr1` in Chapter 09 now uses `theme_grey()`.

The figure standard specifies `theme_grey()` for all ordinary ggplot2 figures unless there is a strong pedagogic reason to deviate.

- **Ch09 `fig-corr1`:** Uses `theme_pubclean()` from `ggpubr`. Should be changed to `theme_grey()`.

### 5. Deprecated `aes_string()` in Chapter 13

Status: Resolved on 2026-03-20. The plotting loop was rewritten using tidy evaluation with `.data[[predictor]]`.

`aes_string()` was deprecated in ggplot2 ≥ 3.0.0. The multi-panel scatter-plot loop in the seaweed EDA section should be rewritten using `.data[[predictor]]` or `across()` tidy evaluation so that the chapter continues to render without warnings when ggplot2 is updated.

### 6. Chapter 17 Heading Level Inconsistency

Status: Resolved on 2026-03-20. Major section headings in Chapter 17 were promoted from `##` to `#`.

All major sections in Chapter 17 use level-2 headings (`##`) instead of level-1 (`#`). Every other chapter uses `#` for major sections and `##` for sub-sections. This produces a collapsed or incorrectly nested table of contents for Chapter 17 and makes it visually inconsistent with the rest of the sequence. All `##` major headings should be promoted to `#`.

### 7. Library Chunks Not Named to Standard

Status: Partly resolved on 2026-03-20. Chapter 11 was renamed to `code-libraries` and the unused `ggthemes` import was removed from Chapter 09. The placeholder chapters 12, 20, and 22 still do not have library chunks, which remains deferred until worked examples are developed.

- **Ch11:** The library-loading chunk is named `code-knitr-opts-chunk-set` instead of `code-libraries`.
- **Ch12 / Ch20 / Ch22:** These are placeholders and currently do not have a library chunk; one should be added when worked examples are developed.
- **Ch09:** Loads `ggthemes` but does not appear to use any `ggthemes` functions in the chapter body. Remove if unused.

### 8. `MASS` Package Collision Risk in Chapter 16

Status: Resolved on 2026-03-20. `MASS` was removed from Chapter 16 because it was not needed there.

`MASS` is loaded in the Chapter 16 setup chunk. `MASS::select()` masks `dplyr::select()` unless explicitly namespace-qualified. If `MASS` is not actually needed in this chapter, it should be removed. If it is used (e.g., for `stepAIC()`), confirm that `dplyr::select()` calls are protected.

### 9. Chapter 21 Theme Scale Mismatch

Status: Resolved on 2026-03-20. Chapter 21 now uses `theme_grey(base_size = 8)`.

Chapter 21 sets `theme_set(theme_grey(base_size = 11))`. Every other chapter with a `theme_set()` call uses `base_size = 8`. This produces noticeably larger in-figure text in Chapter 21 relative to the rest of the sequence.

### 10. Missing `Tasks to Complete` Callout in Several Chapters

Status: Resolved on 2026-03-20 for the chapters named here. The missing callouts were added to Chapters 10, 12, 18, 19, 20, 22, 23, 24, and 25.

The following chapters lack the expected `{.callout-important}` "Tasks to Complete in This Chapter" box:

- Ch10 (no callout at all)
- Ch12 (placeholder – add when worked example is developed)
- Ch18 (no callout at all)
- Ch19 (not confirmed – verify in full file)
- Ch20 (placeholder)
- Ch22 (placeholder)
- Ch23 (no callout at all)
- Ch24 (not confirmed – verify in full file)
- Ch25 (no callout at all)

Chapters with no student tasks should still carry the callout with "None" as the task entry.

### 11. Data Loading Before Opening Callout in Chapter 24

Status: Resolved on 2026-03-20. The Chapter 24 data-loading chunk was moved to after the opening callouts in a labelled setup chunk.

The seaweed matrix preparation in Chapter 24 happens in a code chunk that appears before the `In This Chapter` callout. The standard places library loading and data reading after the opening callouts, either in labelled setup chunks set to `echo: false` or inside the worked-example section. Move the data-loading chunk to after the `In This Chapter` box, or at minimum add `#| include: false` to suppress all output from that chunk.

---

## Summary Priority Table

| Priority | Chapter | Issue |
|---|---|---|
| High | 09 | [x] Replace placeholder Results in Spearman and Kendall Reporting boxes with real outputs |
| High | 09 | [x] Add EDA step and `H_a` equation to Example 2 (Spearman) |
| High | 11 | [x] Remove duplicate hypothesis display in Example 2 |
| High | 13 | [x] Replace deprecated `aes_string()` |
| High | 17 | [x] Promote all `##` major section headings to `#` |
| High | 17 | [x] Replace generic Results in Example 4 Reporting box |
| High | 19 | [x] Replace placeholder Results in Reporting box with real output or label clearly as a template |
| High | Sequence-wide | [x] Resolve `number-sections`, `author`, and `reference-location` YAML inconsistencies |
| Medium | 09 | [x] Fix `theme_pubclean()` → `theme_grey()` in `fig-corr1` |
| Medium | 09 | [x] Remove unused `ggthemes` library |
| Medium | 11 | [x] Rename library chunk `code-knitr-opts-chunk-set` → `code-libraries` |
| Medium | 11 | [x] Move `Outliers` section to after `Core Equations` |
| Medium | 14 | [x] Add `$$H_{a}: ...$$` displayed equation |
| Medium | 15 | [x] Verify `Graham2003` key in project `.bib` file |
| Medium | 16 | [x] Add `R Functions` and `Nature of Data` sections at start |
| Medium | 16 | [x] Remove `MASS` if unused, or namespace `select()` calls |
| Medium | 18 | [x] Add `Tasks to Complete` callout; add `author` to YAML |
| Medium | 21 | [x] Change `base_size = 11` → `base_size = 8` |
| Medium | 21 | [x] Remove `dev = "svglite"` if not deliberately chosen |
| Medium | 24 | [x] Move data-loading chunk to after `In This Chapter` callout |
| Medium | 24 | [x] Normalise `\hat{y}` → `\hat{Y}` in core equations |
| Low | 02 | [x] Add formal Reporting box to grouped-statistics ChickWeight example |
| Low | 03 | [x] Verify `random_not.csv` path |
| Low | 04 | [x] Add 1–2 interpretive sentences to the simple CI example |
| Low | 07 | [x] Fix typo: "Two-sample s *t*-tests" → "Two-sample *t*-tests" |
| Low | 10 | [x] Add `Tasks to Complete` callout and `author` to YAML |
| Low | 13 | [x] Verify `ggfortify` usage; remove if unused |
| Low | 23, 25 | [x] Add `Tasks to Complete` callout |

---

*This review covers structural and formatting issues visible from a full read of the source files. Content correctness (statistical accuracy of explanations, correctness of R output narratives) has not been separately audited here.*
