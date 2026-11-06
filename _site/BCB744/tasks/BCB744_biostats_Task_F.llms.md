# 1 [Assessment sheet](BCB744_Task_F_Surname.xlsx)

# 2 7. *t*-tests

## 2.1 Question 1

Please report back on Task F.1 presented in the lecture. Write up formal Methods and Results sections. **(/15)**

## 2.2 Question 2

Please refer to the [two-sided two-sample *t*-test in the lecture.](https://tangledbank.netlify.app/BCB744/basic_stats/07-t_tests.html#two-sided-two-sample-t-test). It is recreated here:

``` numberSource
# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE)
```

    R> 
    R>  Two Sample t-test
    R> 
    R> data:  dat by sample
    R> t = -1.9544, df = 38, p-value = 0.05805
    R> alternative hypothesis: true difference in means between group A and group B is not equal to 0
    R> 95 percent confidence interval:
    R>  -1.51699175  0.02670136
    R> sample estimates:
    R> mean in group A mean in group B 
    R>        4.001438        4.746584

1.  Repeat this analyses using the Welch’s [`t.test()`](https://rdrr.io/r/stats/t.test.html). **(/5)**
2.  Repeat your analysis, above, using the even more old-fashioned Equation 4 in the lecture. Show the code, talk us through the step you followed to read the *p*-values off the table of *t*-statistics. **(/10)**

## 2.3 Question 3

Please report back on Task F.3 presented in the lecture. Write up formal Methods and Results sections. **(/15)**

## 2.4 Question 4

Please report back the analysis and results for Task F.4. in the lecture. Write up formal Methods and Results sections. **(/15)**

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit,
  author = {Smit, A. J.},
  title = {BCB744 {Task} {F}},
  url = {https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_F.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ BCB744 Task F. [https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_F.html.](https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_F.html)
