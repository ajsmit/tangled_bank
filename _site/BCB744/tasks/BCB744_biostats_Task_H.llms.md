# 1 [Assessment sheet](BCB744_Task_H_Surname.xlsx)

# 2 9. Simple linear regressions

## 2.1 Question 1

1.  Examine the content of the regression model object `sparrows.lm` produced in the [linear regression](https://tangledbank.netlify.app/BCB744/basic_stats/09-regressions.html) chapter. Explain the meaning of the components within, and tell us how they relate to the model summary produced by `summary(sparrows.lm)`. **(/5)**
2.  Using the values inside of the model object, write some R code to show how you can reconstruct the observed values for the dependent variable from the residuals, the fitted values. **(/5)**
3.  Fit a linear regression through the model residuals (use `sparrows.lm`). Explain your findings. **(/5)**
4.  Similarly, fit a linear regression through the fitted values. Explain. **(/5)**

## 2.2 Question 2

Find your own **two datasets** and do a full regression analysis on it. Briefly describe the data and the reason for their existence. Start with setting the appropriate hypotheses. Follow with an EDA, make some exploratory figures, fit the linear model, make a figure with the fitted linear model, provide diagnostic plots to test assumptions, and present the output in a Results section suitable for publication. **(/20)**

### 2.2.1 Rubric

**Criterion** \| **Excellent (Full Marks)** \| **Partial Credit** \| **Absent / Poor** \| **Marks** \|\
— — — — — \| — — — — — — — — — –\| — — — — — — — \| — — — — — — –\| — — — –\|\
**1. Dataset Choice and Justification (/2)** \| Two datasets are clearly described, relevant, and non-trivial; rationale for use is coherent, shows critical thought. \| Datasets are described but may be simplistic or rationale is weakly justified. \| Datasets are vague and trivial, or arbitrarily chosen; little to no justification. \| /2 \|\
**2. Hypothesis Framing (/2)** \| Null and alternative hypotheses are clearly stated in statistical terms and contextualised to the data. \| Hypotheses are present but somewhat vague or generic. \| Hypotheses are missing, poorly framed, or irrelevant. \| /2 \|\
**3. Exploratory Data Analysis (/3)** \| EDA is systematic and insightful, summarising variable distributions, identifying patterns, and flagging potential issues (*e.g.*, collinearity, missingness). \| Some descriptive statistics, plots are provided but interpretation is limited or scattered. \| Minimal or no EDA; data analysed without exploration. \| /3 \|\
**4. Exploratory Figures (/2)** \| Visuals (*e.g.*, scatterplots, histograms) are well-chosen, clearly labelled, and aid interpretation. \| Plots are included but may be unclear, redundant, or poorly formatted. \| Plots are missing, irrelevant. \| /2 \|\
**5. Model Specification and Fitting (/3)** \| Linear model is appropriate for the data and research question; code and output are clearly reported. \| Model is mostly appropriate but poorly justified or inconsistently executed. \| Model is ill-suited, poorly fitted, or lacks documentation. \| /3 \|\
**6. Visualisation of Fitted Model (/2)** \| Plot with fitted regression line is clear, with aesthetic attention to axis labels, units, and legends; enhances interpretation. \| A fitted model is plotted but poorly presented, not fully interpretable. \| No fitted model plot or plot is meaningless. \| /2 \|\
**7. Diagnostic Checks (/3)** \| At least two appropriate diagnostic plots (*e.g.*, residuals vs fitted, QQ-plot) are shown, interpreted in light of linear model assumptions (normality and etc.). \| Diagnostics shown but with weak interpretation or partially inappropriate diagnostics. \| Diagnostics missing, or plots are included without explanation. \| /3 \|\
**8. Written Results Section (/3)** \| Results are reported in a concise, publication-ready format with appropriate terminology (*e.g.*, coefficient estimates, *p*-values, R²), clear narrative flow. \| Results are understandable but lack polish and completeness, or clarity; some technical terms used inaccurately. \| Results are disorganised, incorrectly interpreted, or copied without synthesis. \| /3 \|

**Total: /20**

# 3 10. Correlations

## 3.1 Question 3

Find your own **two datasets** and do a full correlation analysis on it. Briefly describe the data and the reason for their existence. Start with setting the appropriate hypotheses. Follow with an EDA, make some exploratory figures, fit the correlation, make figures with the fitted correlation line, provide diagnostic plots to test assumptions, and present the output in a Results section suitable for publication. **(/20)**

### 3.1.1 Rubric

**Criterion** \| **Excellent (Full Marks)** \| **Partial Credit** \| **Absent / Poor** \| **Marks** \|\
— — — — — \| — — — — — — — — — –\| — — — — — — — \| — — — — — — –\| — — — –\|\
**1. Dataset Choice and Justification (/2)** \| Two variables (from one or more datasets) are clearly described and justified as candidates for correlation analysis; rationale is thoughtful and contextually informed. \| Variables are chosen and described but rationale is vague or unconvincing. \| Variable selection appears arbitrary or trivial; little or no justification given. \| /2 \|\
**2. Hypothesis Framing (/2)** \| Null and alternative hypotheses are explicitly stated and aligned with the correlation analysis (*e.g.*, *H₀*: ρ = 0). Contextual meaning is clearly explained. \| Hypotheses are present but poorly articulated, lacking in contextual relevance. \| Hypotheses are missing or incorrect, or misaligned with the analysis. \| /2 \|\
**3. Exploratory Data Analysis (/3)** \| EDA includes summary statistics, variable distribution inspection, and consideration of linearity, monotonicity. Potential issues (*e.g.* or outliers) are noted. \| EDA is attempted but lacks depth or overlooks important features such as skewness or linearity. \| No meaningful EDA is performed before conducting correlation. \| /3 \|\
**4. Exploratory Figures (/2)** \| Appropriate visualisation (*e.g.*, scatterplot with smoothing line, marginal histograms) is clear or labelled, and supports interpretation. \| Plot is included but unclear, poorly formatted, or not well interpreted. \| No plot provided, or plot is irrelevant, uninformative. \| /2 \|\
**5. Correlation Method and Calculation (/3)** \| Correlation method is appropriate to data characteristics (Pearson/Spearman chosen with justification). Code and output are correct and clearly reported. \| Method is used correctly but without justification or with some reporting issues. \| Correlation is applied blindly or incorrectly; code or output is missing. \| /3 \|\
**6. Significance and Effect Size (/2)** \| *p*-value and correlation coefficient (r or ρ) are reported with interpretation of both statistical and practical significance. \| Results are reported but not clearly interpreted or contextualised. \| Misinterpretation of *p*-value or correlation coefficient; missing output. \| /2 \|\
**7. Assumption Checking and Discussion (/3)** \| Addresses assumptions of correlation method (*e.g.*, normality, linearity, absence of outliers), supported by appropriate plots, discussion. \| Some assumptions discussed or partially checked; reasoning may be unclear. \| No discussion or evidence of assumption checking. \| /3 \|\
**8. Written Results Section (/3)** \| Results are presented in a clear, concise, publication-ready format, with technical correctness, logical flow from EDA to conclusion. \| Results are readable but disorganised or use imprecise language; conclusions may not follow cleanly from evidence. \| Results are unclear and incorrect, or unstructured; poor communication of findings. \| /3 \|

**Total: /20**

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit,
  author = {Smit, A. J.},
  title = {BCB744 {Task} {G}},
  url = {https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_H.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ BCB744 Task G. [https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_H.html.](https://tangledbank.netlify.app/BCB744/tasks/BCB744_biostats_Task_H.html)
