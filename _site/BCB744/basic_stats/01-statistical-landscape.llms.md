We use statistics to describe variation, quantify uncertainty, compare groups, assess relationships, and build models that link observed patterns to underlying processes. We do this to *explain* natural phenomena.[^1] In biology and ecology, statistical thinking *should* begin at the moment you ask a question. It shapes how you design a study, what data you collect, and how you interpret the results. The adage “rubbish in, rubbish out” applies here too. A rubbish experiment or sampling design will give you a rubbish interpretation of biological phenomena.

Answering questions about the natural world using the scientific method requires that we draw on many years of accumulated knowledge and experience. A scientific workflow practised by most biologists unpacks into roughly the following sequence:

1.  **Observe a pattern or phenomenon.** Look around you at the world. Be curious about it. Ask questions to figure out an explanation for the pattern or phenomenon that tickled your interest. Children do this naturally and sadly this curiosity is often lost as we grow older. Maybe we are unfortunate to have parents that don’t encourage questions. Or school education is sufficiently soul-destroying, and the creative, questioning mindset is discouraged. But, as a scientist, you need to keep this curiosity alive. Scientists have the privilege of being paid to be curious, to ask questions, to encounter things which we cannot explain, and to find answers. To always occupy a position of uncertainty. This is a wonderful thing, and it is a privilege that should not be squandered.

2.  **Questions and hypotheses.** Create an unambiguous statement of the question you want to answer, think about what is causing the pattern or phenomenon you observed, and consider how you might measure the response, the thing you observed initially. This leads to the first *formal* step in the scientific method; that is, the formulation of a hypothesis. A hypothesis is not a question. It is an unambiguous statement that can be disproved (in the frequentist view), or for which we can amass evidence in support of prior beliefs (the Bayesian interpretation of probability). The philosopher Sir Karl Popper argued that the key feature of a scientific hypothesis is its *falsifiability*, the capacity for it to be empirically tested and potentially refuted. For Popper, the progress of science depended not on the accumulation of irrefutable truths but on the iterative process of conjecture and refutation, thus subjecting hypotheses to testing and discarding them when contradicted by evidence. He emphasised the *fallibility* of scientific knowledge, asserting that no hypothesis can ever be definitively proven true, only provisionally accepted until it is falsified. This provisional nature of all scientific theories is central to scientific practice today. In Popper’s view, the scientist’s superpower lies in their willingness to embrace uncertainty and systematically eliminate errors, not in the infallibility of their conclusions.

3.  **Design a study that can produce relevant data**. Design an experiment or sampling campaign to collect data that will allow you to test this hypothesis.

4.  **Identify the variables and experimental units**. Clearly understand what the data you’ll collect will look like, both for the response and the explanatory variables. For example, do you have a categorical or continuous predictor, is the response continuous, binary, ordinal, etc.? For this, you should have a firm grasp of the various kinds of [Data Classes and Structures in R](https://tangledbank.netlify.app/BCB744/basic_stats/01-data-in-R.html).

5.  **Consider alternative explanations and potential confounders.** Think deeply about any confounding influences that might affect your data, and specify exactly what additional data you will have to collect to isolate the hypothesised influence in your analysis. You need to fully understand all the ways that factors not considered in your hypothesis might affect your study’s outcome. Omissions cannot be rectified after the fact without repeating the entire experiment or sampling work. It requires knowledge and experience to avoid confounding influences ruining your work.

6.  **Select an analysis that matches the design and data structure.** Depending on your experiment’s design (3) and the nature of the data you’ll obtain (3, 4), choose the appropriate statistical methods to analyse them. You should be able to develop a good idea of what statistical methods you’ll use, even before the experiment has been done! Decide on the parametric test, or, should the statistical god with the die not provide an outcome that favours your expectations, you can also decide upfront on a non-parametric equivalent. It is important not to decide on the statistical method after you’ve collected the data. This is called *p*-hacking, and it is almost a cardinal sin in science.

7.  Do the experiment or go out into the world to sample, and collect the data. Have fun. This is why we do science, afterall!

8.  Go have a few drinks after a hard day’s work and celebrate your success.

9.  **Analyse your newly-collected data.** This will include explaratory data analyses (see [Exploring With Summaries and Descriptions](https://tangledbank.netlify.app/BCB744/basic_stats/02-summarise-and-describe.html) and [Exploring With Figures](https://tangledbank.netlify.app/BCB744/basic_stats/03-visualise.html)), and then the application of the statistical methods you chose in step 6.

10. **Communicate your results.** Your science will be pointless unless you communicate your findings and explanations to an audience. The first step in that process is to develop the necessary tables and figures for inclusion in your manuscript.

So, what is clear is that the analysis does not start at the end. It begins at the design stage. If the design does not align with the question, the analysis will struggle to produce meaningful conclusions.

This textbook deals with many of these steps. Much of this knowledge is codified in the form of the statistical method, which provides a systematic framework for collecting,[^2] analysing, interpreting, and presenting data. In this chapter, I will introduce the foundational concepts of *inferential statistics*, which allow you to make inferences about populations based on sample data.

You will encounter a whole list of statistical tests with seemingly arcane names. The emphasis here is not on memorising these named tests (although, eventually you’ll get to know them). Instead, the goal is that you understand how different methods fit into a broader statistical analysis framework and how to choose among them in a principled way.

> **IMPORTANT:**
>
> A researcher notices that bird nestlings in urban parks weigh less than those in rural landscapes. Working in pairs, sketch the full sequence of steps (from initial observation to final communication) that would constitute a defensible investigation of this finding. For each step, write one decision that, if made incorrectly, could undermine the whole analysis. How many steps did you identify? Compare your sequence with another pair.

> **IMPORTANT:**
>
> Choose a research question you find interesting. Write down one clear research question about it. For that question, identify: (1) the **response variable** (what you would measure), (2) the **predictor(s)** (what you would manipulate or observe), and (3) whether each variable is continuous, discrete, or categorical. Compare your question with the person sitting next to you — do you agree on the variable types?

# 1 Important concepts

A small number of ideas recur throughout the module:

- Statistical reasoning links the question, the nature of the study system, the data, and the model.
- Inference depends on how well the design, analysis, and interpretation align.
- Models provide structured descriptions of how a response varies with predictors.
- Explanation and prediction serve different goals and should be treated separately.
- Assumptions are part of the analysis and must be examined explicitly.

These ideas will reappear in different contexts.

> **IMPORTANT:**
>
> Look at the five key concepts listed above. For each one, write a single sentence giving a concrete example from biology or ecology (real or plausible). For instance, what does it mean in practice for a model to provide a “structured description of how a response varies with predictors”? What does it look like when assumptions are “examined explicitly”? You do not need to use R — just think through what each concept means in the context of real data.

# 2 Core principles

The following practical rules guide analysis across all chapters:

- Start with a clear biological question before choosing a method.
- Understand how the data were generated and what the experimental units represent.
- Choose a model that reflects the process you aim to explain.
- Check assumptions directly and treat deviations as informative.
- Decide whether your goal is explanation or prediction before interpreting results.

These rules help you avoid treating statistical methods as isolated tools and instead use them as part of a coherent series of analytical decisions.

# 3 Core skills

By the end of this module, you should be able to:

- summarise and visualise biological data in a way that reveals structure and variability;
- select appropriate statistical methods for common biological questions;
- fit and interpret linear and generalised models;
- evaluate model assumptions and explain how violations affect inference;
- integrate analysis into a reproducible workflow;
- communicate methods and results clearly.

These skills are cumulative and each chapter adds a layer. Your goal is to develop your understanding into a consistent way of thinking about your data, your models, and your biological questions. You should be able to put what you learned into action, and you will be assessed in the capstone module and in the research project (and BCB744-specific assessments too, of course).

> **IMPORTANT:**
>
> Rate your current confidence in each of the six core skills listed above on a scale from 1 (cannot do at all) to 5 (fully confident). Be honest — this is for your own reference. Write the date next to your ratings. At the end of the module, revisit this list and re-rate yourself. What changed? Which skills improved the most?

# 4 Section structure

If you are new to statistics, follow the parts in order. If you have prior training, you may use early chapters as review before focusing on inference and modelling. The material is organised into five parts with each addressing a different type or stage of statistical application:

## 4.1 Part I. Foundations

These are the fundamental concepts of data description, variation, and uncertainty:

- [The Statistical Landscape](../../BCB744/basic_stats/01-statistical-landscape.llms.md)
- [Summarising Biological Data](../../BCB744/basic_stats/02-summarise-and-describe.llms.md)
- [Visualising Data](../../BCB744/basic_stats/03-visualise.llms.md)
- [Distributions, Sampling, and Uncertainty](../../BCB744/basic_stats/04-distributions-sampling-uncertainty.llms.md)

## 4.2 Part II. Inference

The formal inference tests for comparing groups and assessing associations is introduced:

- [Statistical Inference](../../BCB744/basic_stats/05-inference.llms.md)
- [Raw-Data Assumptions and Transformations](../../BCB744/basic_stats/06-assumptions-and-transformations.llms.md)
- [*t*-Tests](../../BCB744/basic_stats/07-t_tests.llms.md)
- [ANOVA](../../BCB744/basic_stats/08-anova.llms.md)
- [Correlation and Association](../../BCB744/basic_stats/09-correlation-and-association.llms.md)
- [Choosing the Right Test](../../BCB744/basic_stats/10-test-selection.llms.md)

## 4.3 Part III. Modelling

This part is about models that explain variation or to predict from our data:

- [Residuals and Model-Based Diagnostics](../../BCB744/basic_stats/11-residuals-and-model-based-diagnostics.llms.md)
- [Simple Linear Regression](../../BCB744/basic_stats/12-simple-linear-regression.llms.md)
- [Polynomial Regression](../../BCB744/basic_stats/13-polynomial-regression.llms.md)
- [Multiple Regression and Model Specification](../../BCB744/basic_stats/14-multiple-regression-and-model-specification.llms.md)
- [Interaction Effects](../../BCB744/basic_stats/15-interaction-effects.llms.md)
- [Collinearity, Confounding, and Measurement Error](../../BCB744/basic_stats/16-collinearity-confounding-measurement-error.llms.md)
- [Model Checking and Evaluation](../../BCB744/basic_stats/17-model-checking-and-evaluation.llms.md)

## 4.4 Part IV. Extensions

Here I address non-independence, flexible model structures, and extended modelling frameworks — these are beyond the basic toolkit:

- [Pseudoreplication](../../BCB744/basic_stats/18-pseudoreplication.llms.md)
- [Dependence and Mixed Models](../../BCB744/basic_stats/19-dependence-and-mixed-models.llms.md)
- [Generalised Linear Models](../../BCB744/basic_stats/20-generalised-linear-models.llms.md)
- [Generalised Additive Models](../../BCB744/basic_stats/21-generalised-additive-models.llms.md)
- [Nonlinear Regression](../../BCB744/basic_stats/22-nonlinear-regression.llms.md)
- [Quantile Regression](../../BCB744/basic_stats/23-quantile-regression.llms.md)
- [Prediction and Explanation](../../BCB744/basic_stats/24-prediction-and-explanation.llms.md)
- [Regularisation](../../BCB744/basic_stats/25-regularisation.llms.md)

## 4.5 Part V. Reproducibility

Statistical analysis ties to transparent and repeatable scientific workflows (a part of reproducible research), which is discussed here:

- [Reproducible Workflow](../../BCB744/basic_stats/26-reproducible-workflow.llms.md)

# 5 Assessment and practice

Use the assessment materials alongside the chapter sequence:

## 5.1 [Biostatistics self-assessment](../../BCB744/assessments/BCB744_Biostatistics_Self-Assessment.llms.md)

Use this to check conceptual understanding after completing each major block.

## 5.2 [Biostatistics Example 1](../../BCB744/assessments/BCB744_BioStats_Example_1.llms.md)

Use this to practise integrated analysis and interpretation across multiple concepts.

Consistent practice and reflection are essential for developing reliable statistical judgement.

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit2026,
  author = {Smit, A. J. and J. Smit, A.},
  title = {1. {The} Statistical Landscape},
  date = {2026-11-06},
  url = {https://tangledbank.netlify.app/BCB744/basic_stats/01-statistical-landscape.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ, J. Smit A (2026) 1. The statistical landscape. [https://tangledbank.netlify.app/BCB744/basic_stats/01-statistical-landscape.html.](https://tangledbank.netlify.app/BCB744/basic_stats/01-statistical-landscape.html)

[^1]: It is physicist David Deutsch’s opinion that the primary purpose of science is to offer explanations for how things are.

[^2]: Yes, statistics also informs us about how to collect data.
