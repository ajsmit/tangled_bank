This page reproduces the classic printed table of critical values for the *t* distribution, with degrees of freedom from $\nu = 1$ to $\nu = 1000$ and an additional asymptotic row for $\nu = \infty$.

The continuation beyond 50 follows the same sampled degrees of freedom shown in the printed reference table: even values to 100, then progressively wider intervals through 1000.

The two header rows give equivalent tail areas:

- $\alpha(2)$ for the two-sided test,
- $\alpha(1)$ for the one-sided test.

The source files are available here:

- [R script](../../tables/make_t_critical_values_table.R)
- [CSV values](../../tables/t_critical_values_extended.csv)
- [LaTeX source](../../tables/t_critical_values_extended.tex)
- [PDF table](../../tables/t_critical_values_extended.pdf)

The table assets are committed to the repository and shipped as static site resources, so they are not regenerated during a normal Quarto or Netlify build. To rebuild them explicitly, run:

``` numberSource
Rscript tables/make_t_critical_values_table.R
```

> **IMPORTANT:**
>
> This table corresponds to a table of the same same by Zar ([1999](#ref-zar1999biostatistical)). It was created by taking a screeenshot of the table in the book, asking GPT-5.3-codex to create an R script to reproduce the table and typeset it in LaTeX.

The PDF below is produced by the R script and typeset in LaTeX.

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
  title = {28. {Critical} Values of the \emph{t} Distribution},
  date = {2026-11-06},
  url = {https://tangledbank.netlify.app/BCB744/basic_stats/28-t-critical-values.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ (2026) 28. Critical values of the *t* distribution. [https://tangledbank.netlify.app/BCB744/basic_stats/28-t-critical-values.html.](https://tangledbank.netlify.app/BCB744/basic_stats/28-t-critical-values.html)
