# BCB744 Standalone PDF Pipeline

This directory contains the print-specific build pipeline for the 26 numbered
BCB744 biostatistics chapters.

The website remains HTML-first. The PDF build is separate and on demand.

## Use the Wrapper Command

Run from the repository root:

Build one chapter:

```sh
pdf/scripts/bcb744_pdf build 01-statistical-landscape
```

Build all chapters:

```sh
pdf/scripts/bcb744_pdf build all
```

Watch and rebuild on changes:

```sh
pdf/scripts/bcb744_pdf watch
```

Clean outputs:

```sh
pdf/scripts/bcb744_pdf clean
```

## Outputs

- `pdf/out/pdf/` final PDFs
- `pdf/out/tex/` generated LaTeX
- `pdf/out/aux/` LaTeX aux and log files
- `pdf/out/tmp/` temporary resolved metadata and staging files

## Main Components

- `pdf/config/chapters.yml` chapter manifest
- `pdf/config/pdf-meta.yml` print metadata
- `pdf/config/latexmkrc` TeX compilation settings
- `pdf/filters/print-cleanup.lua` print-side structural cleanup
- `pdf/templates/*.tex` LaTeX styling and helpers
- `pdf/scripts/render_pdf.py` chapter renderer
- `pdf/scripts/watch_pdf.py` file watcher
- `pdf/scripts/clean_pdf.py` artefact cleaner

## Notes

- The PDF pipeline renders to LaTeX first, then compiles with `latexmk`.
- Final PDFs are structurally validated before being accepted.
- Watch mode uses a lock file; only one watcher may run at a time.
- The detailed operator guide is in `local_docs/BCB744_PDF_PIPELINE.md`.
