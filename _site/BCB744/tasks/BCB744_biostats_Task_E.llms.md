# 1 [Assessment sheet](BCB744_Task_E_Surname.xlsx)

# 2 2. Exploring with summaries and descriptions

## 2.1 Question 1

1.  Explain the output of [`dimnames()`](https://rdrr.io/r/base/dimnames.html) when applied to the `penguins` dataset. **(/2)**
2.  Explain the output of [`str()`](https://rdrr.io/r/utils/str.html) when applied to the `penguins` dataset. **(/3)**

## 2.2 Question 2

How would you manually calculate the mean value for the `normal_data` we generated in the lecture? **(/3)**

## 2.3 Question 3

Find the `faithful` dataset and describe both variables in terms of their measures of central tendency. Include graphs in support of your answers (use `ggplot()`), and conclude with a brief statement about the data distribution. **(/10)**

## 2.4 Question 4

Manually calculate the variance and SD for the `normal_data` we generated in the lecture. Make sure your answer is the same as those reported there. **(/5)**

## 2.5 Question 5

Write a few lines of code to demonstrate that the $(0-0.25]$, $(0.25-0.5]$, $(0.5-0.75]$,$(0.75-1]$ quantiles of the `normal_data` we generated in the lecture indeed conform to the formal definition for what quantiles are. I.e., show manually how you can determine that 25% of the observations indeed fall below -0.66 for the `normal_data`. Explain the rationale to your approach. **(/10)**

## 2.6 Question 6

Why is it important to consider the grouping structures that might be present within our datasets? **(/2)**

## 2.7 Question 7

Explain the output of [`summary()`](https://rdrr.io/r/base/summary.html) when applied to the `penguins` dataset. **(/3)**

# 3 3. Exploring with figures

## 3.1 Question 8

1.  Using a tidy workflow, assemble a summary table of the **palmerpenguins** dataset that has a similar appearance as that produced by `psych::describe(penguins)`. **(/5)**

    - For bonus marks (which will not count anything) of up to 10% added to Task E, apply a beautiful, creative styling to the table using the [**kable**](https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html) package. Try and make it as publication ready as possible. Refer to a few journal articles to see how to professionally typeset tables.

2.  Still using the **palmerpenguins** dataset, perform an exploratory data analysis to investigate the relationship between penguin species, their morphological traits (bill length and bill depth, flipper length, and body mass). Employ the tidyverse approaches learned earlier in the module to explore the data, account for the grouping structures present within the dataset. **(/10)**

3.  Provide visualisations (use Figure 4 as inspiration) and summary statistics to support your findings and elaborate on any observed patterns or trends. **(/10)**

4.  Ensure your presentation is professional and adhere to the standards required by scientific publications. State the major aims of your analysis and the patterns you seek. Using the combined findings from the EDA and the figures produced here, discuss the findings in a formal Results section. **(/5)**

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit,
  author = {Smit, A. J.},
  title = {BCB744 {Task} {E}},
  url = {https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_E.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ BCB744 Task E. [https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_E.html.](https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_E.html)
