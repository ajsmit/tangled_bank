# 1 [Assessment sheet](BCB744_Task_G_Surname.xlsx)

# 2 8. Analysis of variance (ANOVA)

Refer to the [lecture material](https://tangledbank.netlify.app/BCB744/basic_stats/08-anova.html) to see the questions in context (see **Task G.x: Do it now!**).

## 2.1 Questions 1

Why should we not just apply *t*-tests once per each of the pairs of comparisons we want to make? **(/3)**

## 2.2 Question 2

1.  What does the outcome say about the chicken masses? Which ones are different from each other? **(/2)**
2.  Devise a graphical display of this outcome. **(/4)**

## 2.3 Question 3

Look at the help file for the [`TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html) function to better understand what the output means.

1.  How does one interpret the results? What does this tell us about the effect that that different diets has on the chicken weights at Day 21? **(/3)**
2.  Figure out a way to plot the Tukey HSD outcomes in **ggplot**. **(/10)**
3.  Why does the ANOVA return a significant result, but the Tukey test shows that not all of the groups are significantly different from one another? **(/3)**

## 2.4 Question 4

1.  How is time having an effect? **(/3)**
2.  What hypotheses can we construct around time? **(/2)**

## 2.5 Question 5

1.  What do you conclude from the above series of ANOVAs? **(/3)**
2.  What problem is associated with running multiple tests in the way that we have done here? **(/2)**

## 2.6 Question 6

1.  Write out the hypotheses for this ANOVA. **(/2)**
2.  What do you conclude from the above ANOVA? **(/3)**

## 2.7 Question 7

1.  What question are we asking with the above line of code? **(/3)**
2.  What is the answer? **(/2)**
3.  Why did we wrap `Time` in [`as.factor()`](https://rdrr.io/r/base/factor.html)? **(/2)**

## 2.8 Question 8

How do these results differ from the previous set? **(/3)**

## 2.9 Question 9

Yikes! That is a massive amount of results. What does all of this mean, and why is it so verbose? **(/5)**

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit,
  author = {Smit, A. J.},
  title = {BCB744 {Task} {G}},
  url = {https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_G.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ BCB744 Task G. [https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_G.html.](https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_G.html)
