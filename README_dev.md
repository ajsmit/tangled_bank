# Teaching Base R (vs) Tidyverse

Audience/level: you’re teaching absolute beginners who need to become independently functional R users, not just productive inside one style. Under that goal, yes—there is still clear merit in teaching some base R alongside the Tidyverse, but the “some” matters more than the label.

The core reason is transfer. Beginners eventually read other people’s code, debug errors, interpret help pages, and work with packages whose examples and return objects are written in base idioms. Base R is the shared substrate: the object system, indexing rules, vectorisation, recycling, missing values, and the basic data structures. If students only learn a pipeline grammar, they can become fluent at expressing transformations while staying fragile when anything falls outside that grammar (an unfamiliar class, a list-column, a matrix, a model object, a weird return type, or an error message that assumes you understand indexing).

A second reason is conceptual clarity. Many statistical ideas in R map directly onto base structures: vectors, factors, matrices, lists, data frames, formulas, and model objects. When you teach these explicitly, students can predict behaviour instead of memorising “how to do it in dplyr.” That tends to pay off quickly when they hit modelling, simulation, mixed data types, or custom functions.

A third reason is robustness and longevity. The Tidyverse is stable and widely used, but base R has the strongest backward compatibility guarantees, and it is always available. In constrained environments (minimal installs, some HPC nodes, locked-down teaching labs), relying on base skills reduces friction. More importantly, base literacy reduces dependence on any single ecosystem’s design choices.

That said, it’s usually a mistake to frame the course as “base versus Tidyverse,” or to teach base as a parallel alternative for every task. That approach increases cognitive load, creates style confusion, and often slows beginners down. A practical compromise is: teach one primary workflow for day-to-day data analysis (often Tidyverse), and teach a targeted base “core” that supports reading, debugging, and interfacing with the broader R ecosystem.

A workable division of labour looks like this.

Teach early (base “core” that unlocks everything):
	•	Data types and structures: atomic vectors, lists, data frames; factors and dates at a basic level.
	•	Indexing and subsetting: [, [[, $; logical indexing; the idea that “subsetting drives most data work.”
	•	Missing values and common pitfalls: NA, NaN, Inf, and how they propagate; is.na.
	•	Functions: writing simple functions, argument defaults, return values; using ... at least conceptually.
	•	Reading help and errors: ?foo, str(), class(), attributes(), traceback() (even just one demo helps).
	•	The apply family at a light level (lapply, sapply) for lists, because lists are where many beginners get stuck.

Teach as the main analysis workflow (Tidyverse “production”):
	•	Data import, cleaning, reshaping, summarising, joining, and plotting as a coherent narrative.
	•	The pipe, grouped operations, and the idea of tidy data.
	•	A consistent approach to strings, factors, dates, and visualisation (where the ecosystem shines pedagogically).

Then connect them explicitly:
	•	Show that a tibble is a data frame with stricter printing and a few behavioural differences; show how to drop back to base (as.data.frame, pull, [[) when needed.
	•	When something fails in a pipeline, model a debugging workflow that uses base inspection tools (str, names, indexing) to locate the issue.
	•	When teaching modelling, show that many modelling functions return rich objects; use base extraction (coef, residuals, $, [[) to make those objects legible, even if you tidy outputs afterward.

When might you lean more heavily into base from the start?
	•	If the course is preparing students for methods-heavy statistics (simulation, matrix-based methods, custom estimators), where vectors/lists/matrices are constantly in play.
	•	If students will maintain or extend existing codebases that are primarily base.
	•	If teaching time is short but you need students to handle arbitrary package objects and error messages confidently (base inspection and indexing skills give high return per minute).

When might you lean heavily into the Tidyverse with minimal base?
	•	If the course is primarily “data analysis for domain scientists” with a strong emphasis on wrangling and visualisation, and you can rely on a consistent Tidyverse-based environment for all assignments.
	•	If the objective is rapid productivity and you can postpone deeper R mechanics to a later module.

A concrete recommendation for first-time users: make the Tidyverse the visible workflow, and teach base R as the underlying mechanics in deliberately chosen moments. Students end up productive quickly, but they also learn how R behaves when the training wheels come off.
