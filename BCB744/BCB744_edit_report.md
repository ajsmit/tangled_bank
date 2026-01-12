# BCB744 edit report

## Summary of edits

- Expanded contractions throughout BCB744 prose and user-facing strings, including plot titles and comments where applicable (e.g., `BCB744/intro_r/03-data-in-R.qmd`, `BCB744/basic_stats/03-visualise.qmd`).
- Corrected conjunction and comma errors introduced by earlier passes, with Oxford commas added where appropriate (e.g., `BCB744/intro_r/05-graphics.qmd`, `BCB744/intro_r/02-working-with-data.qmd`, `BCB744/basic_stats/03-visualise.qmd`).
- Fixed possessives and grammar around “it is/its,” and removed malformed conjunctions (e.g., `BCB744/intro_r/05-graphics.qmd`, `BCB744/intro_r/12-tidy.qmd`, `BCB744/intro_r/13-tidier.qmd`, `BCB744/basic_stats/02-summarise-and-describe.qmd`).
- Repaired inline-code lists and missing separators in prose, ensuring proper spacing and conjunctions between code spans (e.g., `BCB744/intro_r/02-working-with-data.qmd`, `BCB744/intro_r/03-data-in-R.qmd`).
- Corrected spelling and clarity issues (e.g., “internse” → “intense” in `BCB744/intro_r/05-graphics.qmd`, “hapazzardly” → “haphazardly” in `BCB744/basic_stats/02-summarise-and-describe.qmd`).

## Notable file-level changes (examples)

- `BCB744/intro_r/05-graphics.qmd`: fixed possessives, list punctuation, and clarity; revised ggplot2 discussion sentences for grammar and flow.
- `BCB744/intro_r/02-working-with-data.qmd`: tightened CSV/TSV descriptions, corrected punctuation, and clarified file-extension guidance.
- `BCB744/intro_r/03-data-in-R.qmd`: cleaned list punctuation and conjunctions in data-type descriptions; clarified binary-variable examples.
- `BCB744/basic_stats/02-summarise-and-describe.qmd`: corrected multiple grammar/logic issues in explanatory paragraphs and definitions.
- `BCB744/basic_stats/03-visualise.qmd`: corrected histogram description and inline R code wording.
- `BCB744/tasks/BCB744_Task_G.qmd`, `BCB744/tasks/BCB744_Task_H.qmd`: clarified rubric phrasing and removed malformed conjunctions.

## Inconsistencies, ambiguities, or clarity recommendations

- **Terminology consistency:** “dataframe” vs “data frame” appears in multiple files; consider standardising to one form (e.g., use “data frame” for prose, keep `data.frame` for code).
- **Capitalisation consistency:** “Base R/base R”, “tidyverse/Tidyverse”, and “Chapter/Section” appear with mixed casing across files; standardise for a more consistent style guide.
- **Hyphenation consistency:** terms like “first time/first‑time”, “real time/real‑time”, and “long term/long‑term” appear with mixed usage; consider standardising.
- **Clarify audience intent:** some instructional asides (e.g., humour or asides about student reading behaviour) could be softened or attributed if intended for formal documents.

If you want a full, line-by-line audit, I can generate a diff‑indexed appendix per file.
