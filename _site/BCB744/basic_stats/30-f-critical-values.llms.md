This page reproduces a set of upper-tail critical values of the *F* distribution for selected numerator degrees of freedom

$$v_1 = 1, 2, \ldots, 20, 22, 24, 26, 28, 30, 40, 50, \ldots, 100, 120, 140, 200, \infty$$

across a standard set of denominator degrees of freedom $v_2$ and upper-tail probabilities. The printed $\alpha(1)$ row gives the one-tailed probability used directly for the *F* quantile, and the $\alpha(2)$ row gives the corresponding doubled value used when relating these cutoffs to two-sided procedures.

The source files are available here:

- [R script](../../tables/make_f_critical_values_table.R)
- [CSV values](../../tables/f_critical_values_v1_1_to_6.csv)
- [LaTeX source](../../tables/f_critical_values_v1_1_to_6.tex)
- [PDF table](../../tables/f_critical_values_v1_1_to_6.pdf)

These assets are stored in the repository and copied to the site as static resources, so standard site builds do not recompute the table. To rebuild it explicitly, run:

``` numberSource
Rscript tables/make_f_critical_values_table.R
```

> **IMPORTANT:**
>
> This table corresponds to a table of the same same by Zar ([1999](#ref-zar1999biostatistical)). It was created by taking a screeenshot of the table in the book, asking GPT-5.3-codex to create an R script to reproduce the table and typeset it in LaTeX.

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
  title = {30. {Critical} Values of the \emph{{F}} Distribution},
  date = {2026-11-06},
  url = {https://tangledbank.netlify.app/BCB744/basic_stats/30-f-critical-values.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ (2026) 30. Critical values of the *F* distribution. [https://tangledbank.netlify.app/BCB744/basic_stats/30-f-critical-values.html.](https://tangledbank.netlify.app/BCB744/basic_stats/30-f-critical-values.html)
