This page reproduces the classic one-tailed normal table. Each cell gives the right-tail probability

$$P(Z \ge z)$$

for a standard normal deviate, where the row gives the first decimal place and the column gives the second decimal place. For example, the entry at row `1.5` and column `1` corresponds to $z = 1.51$ and gives `0.0655`.

The source files are available here:

- [R script](../../tables/make_normal_tail_table.R)
- [CSV values](../../tables/normal_curve_tail_proportions.csv)
- [LaTeX source](../../tables/normal_curve_tail_proportions.tex)
- [PDF table](../../tables/normal_curve_tail_proportions.pdf)

The generated table files are committed and published as static resources, so they are not rebuilt during routine site renders. To regenerate them explicitly, run:

``` numberSource
Rscript tables/make_normal_tail_table.R
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
  title = {29. {Proportions} of the Normal Curve},
  date = {2026-11-06},
  url = {https://tangledbank.netlify.app/BCB744/basic_stats/29-normal-tail-proportions.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ (2026) 29. Proportions of the normal curve. [https://tangledbank.netlify.app/BCB744/basic_stats/29-normal-tail-proportions.html.](https://tangledbank.netlify.app/BCB744/basic_stats/29-normal-tail-proportions.html)
