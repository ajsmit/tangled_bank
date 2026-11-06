This page reproduces critical values of the studentized range statistic $q$ used in Tukey’s honestly significant difference procedure. The table is generated for

- $\alpha = 0.50, 0.20, 0.10, 0.05, 0.025, 0.01, 0.005, 0.001$
- $k = 2$ to $20$, then $22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 50, 60, 70, 80, 90, 100$
- $\nu = 1$ to $20$, then $24, 30, 40, 60, 120, \infty$

The source files are available here:

- [R script](../../tables/make_tukey_q_table.R)
- [CSV values](../../tables/tukey_q_critical_values.csv)
- [LaTeX source](../../tables/tukey_q_critical_values.tex)
- [PDF table](../../tables/tukey_q_critical_values.pdf)

> **IMPORTANT:**
>
> This table corresponds to a table of the same same by Zar ([1999](#ref-zar1999biostatistical)). It was created by taking a screeenshot of the table in the book, asking GPT-5.3-codex to create an R script to reproduce the table and typeset it in LaTeX.

The generated outputs are committed and served as static resources, so the Tukey table is not recomputed during routine site builds. To regenerate it explicitly, run:

``` numberSource
Rscript tables/make_tukey_q_table.R
```

The embedded PDF could not be displayed here. Use the download links above.

## References

Zar JH (1999) Biostatistical analysis. Pearson Education India

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit2026,
  author = {Smit, A. J.},
  title = {31. {Critical} Values of the \emph{q} Distribution},
  date = {2026-11-06},
  url = {https://tangledbank.netlify.app/BCB744/basic_stats/31-tukey-q-critical-values.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ (2026) 31. Critical values of the *q* distribution. [https://tangledbank.netlify.app/BCB744/basic_stats/31-tukey-q-critical-values.html.](https://tangledbank.netlify.app/BCB744/basic_stats/31-tukey-q-critical-values.html)
