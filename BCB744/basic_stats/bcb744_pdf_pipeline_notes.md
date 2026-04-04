# Producing LaTeX-Quality Standalone PDFs from a Quarto Website

## Overview

The clean solution is to treat the website and the PDFs as two parallel build targets from the same `.qmd` source, but to make the PDF path its own LaTeX-first pipeline.

The key design choice is this: keep Quarto as the authoring layer and use LaTeX as the final typesetting layer. Quarto already lets you target formats from the command line with `--to`, supports injecting LaTeX into the preamble with `include-in-header`, supports raw LaTeX that only affects LaTeX-based outputs, and supports custom formats and extensions built around Pandoc templates. That gives you a route where your website YAML stays focused on HTML, while a separate PDF build system produces highly controlled `.tex` and `.pdf` files from the same chapter source.

The recommended architecture is:

- **Authoring layer:** your existing `.qmd` chapters
- **Web output:** your normal Quarto website build
- **Print output:** a custom `bcb744-pdf` format or external build script that renders each chapter through a custom LaTeX template and Lua filter
- **Automation:** a watcher script for local use, plus optional CI for batch rebuilds on commit

My recommendation, stated directly, is this:

Use one-source authoring in Quarto, but create a separate print pipeline based on a custom Pandoc LaTeX template, a Lua filter for structure cleanup, and a watcher script that rebuilds PDFs whenever the `.qmd` or LaTeX assets change. That gives you automatic propagation from Quarto edits to PDF while preserving full control over professional typesetting.

---

## Core Design

Keep your current website project as the canonical source. Then create a separate PDF build layer inside the same repository.

A conceptual structure looks like this:

```text
project/
  _quarto.yml
  bcb744/
    01-introduction.qmd
    02-probability.qmd
    03-regression.qmd
  pdf/
    template/
      bcb744.tex
      preamble.tex
      fonts.tex
      macros.tex
    filters/
      print-cleanup.lua
    build/
      render_pdf.py
      watch_pdf.py
    out/
      tex/
      pdf/
```

The important part is that `bcb744.tex` is your custom Pandoc/LaTeX template. This is where the professional typesetting lives: page geometry, running heads, chapter openings, theorem styles, callout boxes, code listing treatment, fonts, microtype, bibliography style, figure caption style, widow/orphan control, table layout, and package-level tuning.

The `.qmd` files remain the single source of truth, but the PDF build logic lives outside the chapter front matter.

---

## Build Strategy

Do not put the PDF format into each chapter YAML if you want to keep the website files clean. Instead, keep a separate metadata file or command-line build configuration for the PDF pipeline.

The workflow is:

1. Take `chapter.qmd`
2. Render it to LaTeX with your PDF-specific template and filters
3. Compile that `.tex` with `latexmk`
4. Write the finished PDF into `pdf/out/pdf/`

A simple command pattern is:

```bash
quarto render bcb744/02-probability.qmd \
  --to pdf \
  --output-dir pdf/out/pdf
```

In practice, the better approach is to render to `.tex` first, then run `latexmk` yourself. That gives you more control and makes debugging easier. You can inspect the generated `.tex`, patch problems with a Lua filter, and keep the final LaTeX compile independent from Quarto.

---

## Conditional Content

Your website chapters may contain material that works well online but poorly in print: navigation cues, website callouts, embedded interactive elements, or pedagogical sidebars. Quarto supports format-conditional content, so you can keep one `.qmd` and selectively show or hide blocks for print versus HTML. It also allows raw LaTeX that affects only LaTeX-based outputs.

Example:

```markdown
::: {.content-visible when-format="html"}
Online-only note or widget reference
:::

::: {.content-visible when-format="pdf"}
\begin{tcolorbox}[title=Key Point]
This version is optimized for print reading.
\end{tcolorbox}
:::
```

For small typographic interventions:

```markdown
\newpage
```

That command will be ignored outside LaTeX-based output.

---

## Automation

A watch script is a good idea. It should watch the source files directly rather than the rendered website.

A robust rule set is:

- If any `.qmd` in `bcb744/` changes, rebuild the corresponding PDF
- If `preamble.tex`, `macros.tex`, `bcb744.tex`, bibliography files, or the Lua filter changes, rebuild all BCB744 PDFs

A conceptual watcher script looks like this:

```python
from pathlib import Path
import subprocess
import time

CHAPTERS = Path("bcb744").glob("*.qmd")
TEX_SUPPORT = [
    Path("pdf/template/bcb744.tex"),
    Path("pdf/template/preamble.tex"),
    Path("pdf/template/macros.tex"),
    Path("pdf/filters/print-cleanup.lua"),
]

def build_one(chapter):
    subprocess.run([
        "quarto", "render", str(chapter),
        "--to", "pdf",
        "--output-dir", "pdf/out/pdf"
    ], check=True)

def build_all():
    for chapter in Path("bcb744").glob("*.qmd"):
        build_one(chapter)

def snapshot():
    files = list(Path("bcb744").glob("*.qmd")) + TEX_SUPPORT
    return {str(f): f.stat().st_mtime for f in files if f.exists()}

state = snapshot()
while True:
    time.sleep(2)
    new_state = snapshot()
    changed = [f for f in new_state if new_state[f] != state.get(f)]
    if changed:
        if any(f.endswith(".qmd") for f in changed):
            for f in changed:
                if f.endswith(".qmd"):
                    build_one(Path(f))
        else:
            build_all()
        state = new_state
```

This is only the basic logic. In a production version, the script should:

- render to `.tex` first
- run `latexmk` separately
- keep a chapter manifest
- map source files to output names
- rebuild selectively when possible

---

## Chapter Manifest

A manifest makes the build system more reliable. For example:

```yaml
chapters:
  - source: bcb744/01-introduction.qmd
    slug: bcb744-01-introduction
  - source: bcb744/02-probability.qmd
    slug: bcb744-02-probability
```

Then the build script can render each source to `pdf/out/tex/{slug}.tex` and compile to `pdf/out/pdf/{slug}.pdf`.

---

## Why a Lua Filter Matters

This is the part that makes the workflow genuinely powerful.

The LaTeX template handles global typography. The Lua filter handles structural conversion issues that Pandoc alone will not polish the way you want. For example, the filter can:

- convert Quarto callouts into custom theorem-like or `tcolorbox` environments
- rewrite website-style admonitions into print-side margin notes or shaded boxes
- normalize figure placements
- adjust heading spacing
- collapse web-specific divs
- map code chunks to your preferred listing environment
- enforce print-friendly captions and cross-reference phrasing

This lets you keep authoring in Quarto while still achieving a bespoke LaTeX output.

---

## Suggested Implementation Plan

A minimal implementation plan would be:

1. Build one chapter end-to-end as a prototype.
2. Create `bcb744.tex`, `preamble.tex`, and `print-cleanup.lua`.
3. Make the script render one `.qmd` to `.tex`, then compile with `latexmk`.
4. Once the prototype looks right, batch it across all BCB744 chapters.
5. Add the watcher for automatic local rebuilds.
6. Optionally add GitHub Actions or Netlify build hooks to regenerate the PDFs on push.

The main limitation is structural: if some parts of your website rely heavily on HTML-specific constructs, they will need either conditional content blocks or Lua-filter handling for print. But that is a manageable conversion problem and much easier than trying to force the default Quarto PDF path to behave like a carefully designed book or monograph.

---

## Repository Layout

I would separate authoring, website build, and print build very explicitly. That keeps the Quarto site stable while giving the PDF pipeline room to become genuinely LaTeX-first.

A good layout would look like this:

```text
tangled-bank/
├── _quarto.yml                     # main website project config
├── index.qmd
├── about.qmd
├── styles.scss
├── references.bib
├── _variables.yml                 # optional shared metadata
│
├── bcb744/                        # teaching material source
│   ├── index.qmd                  # landing page for biostats section
│   ├── 01-introduction.qmd
│   ├── 02-data-visualisation.qmd
│   ├── 03-probability.qmd
│   ├── 04-models.qmd
│   ├── 05-regression.qmd
│   ├── 06-inference.qmd
│   ├── images/
│   │   ├── lecture01/
│   │   ├── lecture02/
│   │   └── shared/
│   ├── tables/
│   └── data/
│
├── _extensions/                   # optional Quarto extension route
│   └── bcb744-pdf/
│       ├── _extension.yml
│       ├── bcb744-pdf.lua
│       ├── templates/
│       │   └── bcb744.tex
│       ├── includes/
│       │   ├── preamble.tex
│       │   ├── fonts.tex
│       │   ├── macros.tex
│       │   └── titlepage.tex
│       └── assets/
│           └── logo.pdf
│
├── pdf/                           # standalone print pipeline
│   ├── config/
│   │   ├── chapters.yml           # manifest of chapters to build
│   │   ├── pdf-meta.yml           # print-specific metadata
│   │   └── latexmkrc
│   │
│   ├── templates/
│   │   ├── bcb744.tex             # main Pandoc/LaTeX template
│   │   ├── preamble.tex
│   │   ├── fonts.tex
│   │   ├── macros.tex
│   │   ├── environments.tex
│   │   └── titlepage.tex
│   │
│   ├── filters/
│   │   ├── print-cleanup.lua      # convert callouts, fix headings, etc.
│   │   ├── figures.lua
│   │   └── codeblocks.lua
│   │
│   ├── scripts/
│   │   ├── render_pdf.py          # build one/all chapters
│   │   ├── watch_pdf.py           # watch qmd/template changes
│   │   ├── clean_pdf.py
│   │   └── utils.py
│   │
│   ├── out/
│   │   ├── tex/                   # generated intermediate .tex
│   │   ├── pdf/                   # final chapter PDFs
│   │   ├── aux/                   # latexmk aux files
│   │   └── logs/
│   │
│   └── README.md
│
├── .github/
│   └── workflows/
│       ├── build-website.yml
│       └── build-bcb744-pdfs.yml  # optional CI rebuild on push
│
├── .gitignore
├── README.md
└── Makefile                       # optional convenience commands
```

The logic of this layout is straightforward.

The `bcb744/` directory remains the single source of truth. Those `.qmd` files feed both outputs: the website and the PDFs. That is what preserves automatic pull-through when you edit content.

The `pdf/` directory is a separate production pipeline. It contains everything that is specific to print: LaTeX template files, filters, a build manifest, build scripts, and generated outputs. This prevents your website YAML from becoming crowded with PDF-only settings.

The `_extensions/` directory is optional. There are two valid implementation paths.

### Path 1: Direct `pdf/` Pipeline

Keep everything in `pdf/` and let `render_pdf.py` call Quarto and LaTeX directly. This is usually the best route for a prototype.

### Path 2: Quarto Extension

Package the PDF rules as a custom Quarto extension, for example `bcb744-pdf`. Then your script can call a custom format rather than passing many command-line options. That becomes more elegant once the format stabilises.

---

## Reduced Prototype Layout

I would start with this reduced version:

```text
tangled-bank/
├── _quarto.yml
├── bcb744/
│   ├── 01-introduction.qmd
│   ├── 02-data-visualisation.qmd
│   ├── 03-probability.qmd
│   └── images/
├── pdf/
│   ├── config/
│   │   ├── chapters.yml
│   │   └── pdf-meta.yml
│   ├── templates/
│   │   ├── bcb744.tex
│   │   ├── preamble.tex
│   │   └── macros.tex
│   ├── filters/
│   │   └── print-cleanup.lua
│   ├── scripts/
│   │   ├── render_pdf.py
│   │   └── watch_pdf.py
│   └── out/
│       ├── tex/
│       └── pdf/
└── Makefile
```

That is enough to prove the concept.

---

## Role of Key Files

### `pdf/config/chapters.yml`

This is the manifest that tells the script which `.qmd` files belong in the BCB744 PDF series and what filenames to use.

Example:

```yaml
chapters:
  - source: bcb744/01-introduction.qmd
    slug: 01-introduction
    title: "Introduction"
  - source: bcb744/02-data-visualisation.qmd
    slug: 02-data-visualisation
    title: "Data Visualisation"
  - source: bcb744/03-probability.qmd
    slug: 03-probability
    title: "Probability"
```

### `pdf/config/pdf-meta.yml`

This stores print-only metadata so you do not have to place it in each chapter YAML.

Example:

```yaml
documentclass: scrreprt
fontsize: 11pt
linestretch: 1.08
geometry:
  - top=28mm
  - bottom=30mm
  - inner=30mm
  - outer=24mm
colorlinks: true
toc: false
numbersections: true
bibliography: references.bib
```

### `pdf/templates/bcb744.tex`

This is the main Pandoc template. It controls the page model and document structure. In practice, this becomes the centre of the print design.

### `pdf/templates/preamble.tex`

This loads packages and sets the typographic rules: font packages, `microtype`, theorem environments, caption styling, headers, footers, and code presentation.

### `pdf/templates/macros.tex`

This holds your reusable commands, such as statistical notation, emphasis macros, boxed definitions, or teaching-specific structural commands.

### `pdf/filters/print-cleanup.lua`

This is where you solve the “Quarto is good for authoring but print needs different structure” problem. For example, you can convert:

- Quarto callouts into `tcolorbox`
- website-only divs into nothing
- custom classes into LaTeX environments
- long figure captions into a more disciplined format

### `pdf/scripts/render_pdf.py`

This script should do three things:

1. read `chapters.yml`
2. render each `.qmd` to `.tex`
3. compile each `.tex` to `.pdf` with `latexmk`

That means your pipeline is inspectable at the `.tex` stage.

### `pdf/scripts/watch_pdf.py`

This script watches these inputs:

- all `bcb744/*.qmd`
- all `pdf/templates/*.tex`
- all `pdf/filters/*.lua`
- `references.bib`
- `pdf/config/*.yml`

Then it selectively rebuilds affected outputs.

### `Makefile`

This is optional but useful. For example:

```make
pdf-all:
	python pdf/scripts/render_pdf.py all

pdf-one:
	python pdf/scripts/render_pdf.py one bcb744/03-probability.qmd

pdf-watch:
	python pdf/scripts/watch_pdf.py

pdf-clean:
	python pdf/scripts/clean_pdf.py
```

That gives you simple commands such as `make pdf-all` and `make pdf-watch`.

---

## Authoring Rule

A useful structural rule is:

Keep print-sensitive authoring signals inside the `.qmd`, but keep print styling outside the `.qmd`.

So in chapter files you might allow:

```markdown
::: {.content-visible when-format="html"}
Website-only prompt or interactive note.
:::

::: {.content-visible when-format="pdf"}
This sentence can be expanded slightly for print readers.
:::
```

But the appearance of print definitions, examples, callouts, and exercises should be controlled in the LaTeX template and Lua filter, not by repeated local formatting in each chapter. That keeps the source clean and makes the series visually consistent.

---

## Clean Evolution Path

The cleanest evolution path is:

### Prototype phase

Use `bcb744/` plus `pdf/` only.

### Stable phase

Move the PDF rules into `_extensions/bcb744-pdf/`, keep `pdf/scripts/` for automation, and let the extension define the print format.

That version would look like this:

```text
_extensions/
└── bcb744-pdf/
    ├── _extension.yml
    ├── bcb744-pdf.lua
    ├── templates/bcb744.tex
    └── includes/
        ├── preamble.tex
        └── macros.tex
```

Then your build script can simply call the custom format for each chapter.

My recommendation is to start with the simpler `pdf/` pipeline and only promote it to an extension once the typography and structure are working exactly as you want.
