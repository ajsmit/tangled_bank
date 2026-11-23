> *“We have become, by the power of a glorious evolutionary accident called intelligence, the stewards of life’s continuity on earth. We did not ask for this role, but we cannot abjure it. We may not be suited to it, but here we are.”*
>
> Stephen Jay Gould

[![](../images/PhD_comics3.JPG)](../images/PhD_comics3.JPG)

**Welcome to BCB743 Quantitative Ecology. This page is the entry point to the module syllabus, lecture material, workflows, and data.**

## 1 Honours coordinator

For queries about the Honours programme in general, please consult Dr. Patrick O’Farrell (Room 4.111).

## 2 Module coordinator

The module coordinator and lecturer is Prof AJ Smit (Room 4.103), and the teaching assistant for the module is Mr. Isma-eel Jattiem (4035085@myuwc.ac.za).

## 3 Module description

Quantitative ecology uses statistics and computation to study ecological structure. In practice, that means working with multivariate data, testing ecological hypotheses, building models, and deciding which patterns are biologically meaningful rather than merely visually appealing.

In this module, you will cover the following topics:

- **Ecological Structure:** You will explore the main principles underlying the environmental structuring of ecosystems (ecosystem structure).

- **Ecological Data Analysis:** In this section, you will examine approaches to analyse ecological data, including hypothesis testing, regression analysis, and multivariate analysis.

- **Multivariate Analyses:** Here, you will learn how to use multivariate statistics to make sense of complex systems, predict ecological outcomes, and understand the underlying mechanisms that drive ecological processes.

- **Spatial Ecology:** You will acquire knowledge on how to analyse and model spatial patterns in ecological data, including the distribution of species and habitats across landscapes.

- **Community Ecology:** The theory covered in this section will prepare you to analyse and model the interactions between species within ecological communities, such as competition, predation, and mutualism.

- **Ecosystem Ecology:** You will learn how to model and analyse the flow of energy and nutrients through ecosystems, including the roles of producers, consumers, and decomposers in ecological processes.

The centre of gravity in BCB743 is multivariate analysis. Ecological datasets usually contain many species, many sites, and many environmental variables at once. Ordination, dissimilarity-based methods, regression, and constrained analyses help us reduce that complexity without pretending it is simple.

You will work with methods such as PCA, CA, PCoA, NMDS, multiple regression, and db-RDA not as isolated techniques, but as tools for answering ecological questions about gradients, community structure, species turnover, and assembly processes.

## 4 Module content and framework

These links point to online resources such as datasets and R scripts in support of the video and PDF lecture material. It is essential that you work through these examples and workflows. The 2026 syllabus is below:

Lectures run on **Mondays and Thursdays**, with **Tuesdays** used in the busier weeks to fit the additional regression chapters. Rows marked *self-study* are not lectured in class; work through them on your own alongside that week’s material. Chapter numbers match the sidebar and the chapter titles.

| Week | Date | Chapter | Topic | Notes | Slides | Reading | Tasks | Due |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| **W1** · Foundations | Mon 15 Jun | L0 | Overview & Introduction |  | [Slides](../slides/BCB743_01_intro.pdf) | [BES Code](../docs/BES-Reproducible-Code-2017.pdf) [BES Data](../docs/BES-Data-Guide-2017_web.pdf) |  |  |
|  |  |  | Integrative Assessment |  |  |  | [Integrative Assessment](../BCB743/tasks/Task_A1.llms.md) | 24 Jul |
|  |  |  | Chapter Contribution: presentation |  |  |  | [Chapter Contribution](../BCB743/tasks/Task_A2.llms.md) | 17 Jul |
|  |  |  | Assessment weighting declaration |  |  |  | [Assessment Policy](#sec-policy) | 17 Jul |
|  |  |  | Chapter Contribution: final chapter |  |  |  | [Chapter Contribution](../BCB743/tasks/Task_A2.llms.md) | 24 Jul |
|  |  |  | spesim Package Review |  |  |  | [spesim Package Review](../BCB743/tasks/Task_A3.llms.md) | 24 Jul |
|  |  | 1 · *self-study* | Revision: biodiversity (BDC334) | [Labs 1–4](../BDC334/BDC334_syllabus.llms.md) |  | [Smit 2013](../docs/Smit_et_al_2013.pdf) |  |  |
|  | Thu 18 Jun | 2 | Correlations & Associations | [Notes](../BCB743/correlations.llms.md) | [Slides](../slides/BCB743_06_correlations.pdf) | [Biostats Ch. 9](../BCB744/basic_stats/09-correlation-and-association.llms.md) | [Task A](../BCB743/tasks/Task_A.llms.md) |  |
|  |  | 3 · *self-study* | Distance Metrics | [Notes](../BCB743/dis-metrics.llms.md) |  |  |  |  |
|  |  | 4 | Intro to Ordination | [Notes](../BCB743/ordination.llms.md) | [Slides](../slides/BCB743_07_ordination.pdf) |  |  |  |
| **W2** · Unconstrained ordination I | Mon 22 Jun | 5 | PCA | [Notes](../BCB743/PCA.llms.md) | [Slides](../slides/BCB743_08_PCA.pdf) |  | [Task B](../BCB743/tasks/Task_B.llms.md) |  |
|  |  | 6 · *self-study* | PCA: Additional Examples | [Notes](../BCB743/PCA_examples.llms.md) |  |  |  |  |
|  |  | 7 · *self-study* | PCA: WHO SDGs | [Notes](../BCB743/PCA_SDG_example.llms.md) |  |  | [Task C](../BCB743/tasks/Task_C.llms.md) |  |
|  | Tue 23 Jun | 8 | CA | [Notes](../BCB743/CA.llms.md) | [Slides](../slides/BCB743_09_CA.pdf) |  | [Task D](../BCB743/tasks/Task_D.llms.md) |  |
|  |  | 9 | DCA | [Notes](../BCB743/DCA.llms.md) |  |  | [Task E](../BCB743/tasks/Task_E.llms.md) |  |
|  | Wed 24 Jun | 10 | PCoA | [Notes](../BCB743/PCoA.llms.md) | [Slides](../slides/BCB743_10_PCoA.pdf) |  | [Task F](../BCB743/tasks/Task_F.llms.md) |  |
|  | Thu 25 Jul | 11 | nMDS | [Notes](../BCB743/nMDS.llms.md) | [Slides](../slides/BCB743_11_nMDS.pdf) |  | [Task G](../BCB743/tasks/Task_G.llms.md) |  |
|  |  | 12 · *self-study* | nMDS: PERMANOVA (diatoms) | [Notes](../BCB743/nMDS_diatoms.llms.md) |  | [Mayombo 2019](../docs/Mayombo_et_al_2019.pdf) |  |  |
|  |  | 13 · *self-study* | Unconstrained Ordination: Summary | [Notes](../BCB743/unconstrained-summary.llms.md) |  |  |  |  |
| **W3** · Assignment work (self) |  |  |  |  |  |  |  |  |
| **W4** · Constrained ordination & clustering | Thu 9 Jul | 14 | Distance-based Redundancy Analysis | [Notes](../BCB743/constrained_ordination.llms.md) | [Slides](../slides/BCB743_12_constrained_ordination.pdf) | [Smit 2017](../docs/Smit_et_al_2017.pdf) [Seaweed data](../docs/Smit_the_seaweed_data.pdf) | [Task H](../BCB743/tasks/Task_H.llms.md) |  |
|  |  | 15 · *self-study* | db-RDA: Revisiting the β-diversity Partition | [Notes](../BCB743/constrained_ordination_v2.llms.md) |  |  |  |  |
|  |  | 16 · *self-study* | Seaweeds in Two Oceans | [Notes](../BCB743/two_oceans_appendices.llms.md) |  | [Smit 2017](../docs/Smit_et_al_2017.pdf) [Seaweed data](../docs/Smit_the_seaweed_data.pdf) |  |  |
|  | Fri 10 Jul | 17 | Cluster Analysis | [Notes](../BCB743/cluster_analysis.llms.md) |  |  | [Task I](../BCB743/tasks/Task_I.llms.md) |  |
| **W5** · Regression I | Mon 13 Jul | 18 | Model Building | [Notes](../BCB743/model_building.llms.md) |  | [Biostats Ch. 17](../BCB744/basic_stats/17-model-checking-and-evaluation.llms.md) [Ch. 24](../BCB744/basic_stats/24-prediction-and-explanation.llms.md) | [Task J](../BCB743/tasks/Task_J.llms.md) |  |
|  |  | 19 · *self-study* | Deep Dive Into Gradients | [Notes](../BCB743/deep_dive.llms.md) |  | [Smit 2013](../docs/Smit_et_al_2013.pdf) [Smit 2017](../docs/Smit_et_al_2017.pdf) |  |  |
|  | Tue 14 Jul | 20 | Multiple Regression | [Notes](../BCB743/multiple_regression.llms.md) |  | [Biostats Ch. 14](../BCB744/basic_stats/14-multiple-regression-and-model-specification.llms.md) [Ch. 16](../BCB744/basic_stats/16-collinearity-confounding-measurement-error.llms.md) | [Task K](../BCB743/tasks/Task_K.llms.md) |  |
|  | Thu 16 Jul | 21 | Generalised Linear Models | [Notes](../BCB743/glm.llms.md) |  | [Biostats Ch. 20](../BCB744/basic_stats/20-generalised-linear-models.llms.md) | [Task L](../BCB743/tasks/Task_L.llms.md) |  |
| **W6** · Regression II & wrap-up | Mon 20 Jul | 22 | Generalised Additive Models | [Notes](../BCB743/gam.llms.md) |  | [Biostats Ch. 21](../BCB744/basic_stats/21-generalised-additive-models.llms.md) | [Task M](../BCB743/tasks/Task_M.llms.md) |  |
|  | Tue 21 Jul | 23 | Mixed Models | [Notes](../BCB743/mixed_models.llms.md) |  | [Mayombo 2019](../docs/Mayombo_et_al_2019.pdf) [Biostats Ch. 18](../BCB744/basic_stats/18-pseudoreplication.llms.md) [Ch. 19](../BCB744/basic_stats/19-dependence-and-mixed-models.llms.md) | [Task N](../BCB743/tasks/Task_N.llms.md) |  |
|  |  | 24 · *self-study* | Complete Multivariate Analysis (Doubs) | [Notes](../BCB743/doubs_multivariate.llms.md) |  |  |  |  |
|  | Thu 23 Jul |  | Review & Project Clinic |  |  |  |  |  |
|  | Fri 24 Jul |  | **Final Submission and THE END** |  |  |  |  |  |

The **Notes** column links to the chapter page, **Slides** to the lecture deck, an author–year to the reading, **Biostats Ch. N** to the matching chapter in the BCB744 Biostatistics book, and **Task** to the exercise. The three assessed assignments are set on Day 1; their deadlines are shown at the top.

**Core theoretical framework** Ecological hypotheses about how species assemble across space and time, including niche-based mechanisms, neutral processes, and historical contingencies; the major distributional patterns of organisms at local, regional, and global scales; and sampling designs that allow those processes to be tested quantitatively.

**Competence** Data collection and preparation for quantitative testing of ecological hypotheses; reproducible analysis in R; multivariate methods such as NMDS, PCA, RDA, and cluster analysis; and graphical summaries that support defensible interpretation.

## 5 Outcomes of BCB743

By the end of this module, students will be able to:

- Understand the concepts of $\alpha$-, $\beta$- and $\gamma$-diversity
- Know and understand the current hypotheses that explain species assembly processes in space and time (e.g. neutral and niche mechanisms)
- Collect ecological data at the appropriate scale, which would lend themselves to a quantitative analysis of points 1 and 2, above
- Use the R software and associated packages to undertake the analyses required in point 3, above
- Interpret the outcomes of the above analyses and use it to quantitatively characterise points 1 and 2, above
- Communicate the findings by written and oral means

------------------------------------------------------------------------

## 6 Assessment policy

There are various tasks in this module. Only the three assignments listed below are formally assessed:

- **[Assessment 1: Integrative Assignment](../BCB743/tasks/Task_A1.llms.md)**: a group research-project assignment in which you extend the analysis and interpretation of an ecological dataset beyond the parent publication.
- **[Assessment 2: Chapter Contribution](../BCB743/tasks/Task_A2.llms.md)**: a group chapter-writing assignment in which you prepare a new BCB743-style teaching chapter. Within this assignment, the written chapter contributes 80% of the task mark and the presentation contributes 20%.
- **[Assessment 3: spesim Package Review](../BCB743/tasks/Task_A3.llms.md)**: a group professional review of the `spesim` package as ecological teaching software for BCB743.

You must work in a fixed group of three students for all three assessed assignments. The same three students must submit Task A1, Task A2, and Task A3 together. Each submission must include an author-contribution statement that identifies what each group member contributed.

You may decide how these three assessed tasks are weighted in your final module mark. Your chosen weights must meet three rules:

- the three weights must add to 100%
- each task must carry at least 20%
- no task may carry more than 40%

This means that an equal weighting is allowed, for example 33.3%, 33.3%, and 33.4%. So are uneven weightings such as 40%, 40%, and 20%, or 40%, 30%, and 30%. The weighting declaration changes only how the three task marks are combined; it does not change the marking criteria for any task.

Each student must submit their weighting declaration by **17 July**, one week before the final submission date. The declaration is individual, so members of the same group may choose different weightings. If you do not submit a valid declaration by the deadline, the default weighting will be equal weighting across the three tasks.

For all assessed assignments, claims must be properly cited or tied to direct evidence, and the Quarto source files must be made available. For literature-based claims, every cited paper must be kept as a local PDF with the relevant supporting passages highlighted. Work must be proofread carefully before submission; marks may be lost for weak structure, unclear language, unsupported claims, poor formatting, incomplete referencing, or code that does not render.

The remaining lettered practice Tasks (A–N), including the practice file named `Task_A.qmd`, are for self-practice and will not be formally assessed. The three assessed assignments are the separate Assessment 1–3 pages linked above. We can discuss your solutions in class if necessary, should you require additional help.

## 7 Graduate attributes

The [**graduate attributes**](../pages/graduate_attributes.llms.md) resulting from completion of this module align with the expectations of the workplace across diverse organisations and institutions where graduates typically find employment.

## 8 Data and reading in support of the syllabus

The table above includes the core papers for each week. You need to read them.

Many other references are cited in each Chapter. These serve several functions in that they:

- Add additional theory relevant to some ecological concepts
- Provide background to some of the datasets used in my examples
- Discuss derivations of some equations used to calculate diversity concepts
- Provide example walkthroughs of some of the computational aspects of the methods covered in the Labs
- Collectively supplement the discussion about these concepts covered in the lectures

Students who read actively and connect the papers to the analyses usually perform far better than students who only skim the slides.

### 8.1 Reading

You are **expected to read additional material** in support of the content covered in class and on this website.

A **compulsory** reference is [‘Numerical Ecology with R’](http://adn.biol.umontreal.ca/~numericalecology/numecolR/) by Daniel Borcard, François Gillet, and Pierre Legendre ([Borcard et al. 2011](#ref-borcard2011numerical)). Much of the class’ content and many of the examples (and code) that I use have been adapted from this source. There is also the excellent book by Legendre and Legendre ([2012](#ref-legendre2012numerical)) called ‘Numerical Ecology’ which provides everything the former book has, but in greater detail and with less focus on R. Both should be considered a ‘gold standard’ reference for Quantitative Ecology.

A third **highly recommended text** is the book [Tree Diversity Analysis](http://apps.worldagroforestry.org/downloads/Publications/PDFS/b13695.pdf) by Roeland Kindt and Richard Coe.

I can also recommend these amazing websites with excellent content:

- David Zelený’s [Analysis of Community Ecology Data in R](https://www.davidzeleny.net/anadat-r/doku.php)
- [Guide to Statistical Analysis in Microbial Ecology (GUSTA ME)](https://sites.google.com/site/mb3gustame/)

Note that the URLs with links to additional reading that appear with the worked-through example code should **not be seen as optional**. They are there for a reason and should be consulted even though I might not necessarily refer to each of them in class. Use these materials liberally.

Should you want to download the source code for the BCB743, you may find it on [ GitHub](https://github.com/ajsmit/R_courses).

### 8.2 Datasets used in this module

Please see the [Datasets](../BCB743/datasets.llms.md) Chapter for the data we will use in this module.

## 9 Prerequisites

You should have moderate numerical literacy and prior programming experience. Such experience will have been obtained in the [BCB744](../BCB744/BCB744_index.llms.md) module, which is a module about doing statistics in R. If you have reasonable experience in coding and statistical analysis, you should find yourself well prepared. You should also thoroughly revise [BDC334](../BDC334/BDC334_syllabus.llms.md) by the end of the first week of this module.

## 10 Method of instruction

You are provided with reading material (lecture slides, code, and website content) that you are expected to consume **prior to the class**. Classes will involve brief introductions to new concepts and will be followed by working on exercises in class that cover those concepts. The workshop is designed to be as interactive as possible, so while you are working on exercises, the tutor and I will circulate among you and engage with you to help you understand any material or associated code you are uncomfortable with. Often this will result in discussions of new applications and alternative approaches to the data analysis challenges you are required to solve. More challenging concepts might emerge during the assignments (typically these will be submitted the following day), and any such challenges will be dealt with in class prior to learning new concepts.

Although the module is theory-heavy, a large part of it is also about coding. It is up to you to take your coding skills to the next level and move beyond what I teach in class. Coding is a bit like learning a language, and as such, programming is a skill that is best learned by doing.

## 11 Learning collaboratively

> **NOTE:**
>
> Please refer to my [advice about how to learn](../pages/How_to_learn.llms.md).

Discuss the BCB743 workshop activities with your peers as you work on them. Also use the WhatsApp group set up for the module for discussion purposes (I might assist via this medium if necessary, provided your questions/comments have relevance to the whole class). A better option is to use [GitHub Issues](https://github.com/ajsmit/R_courses/issues). You will learn more in this module if you work with your friends than if you do not. Ask questions, answer questions, and share ideas liberally. The three assessed assignments must be completed in the same fixed group of three students, and each submission must identify all group members by name.

**Cooperative learning is not a licence for plagiarism. Plagiarism is a serious offence and will be dealt with decisively. Consequences of cheating are severe; they range from 0% for the assignment or exam to dismissal from the module for a second offence.**

## 12 Reusing code found elsewhere

A huge volume of code is available on the web and it can be adapted to solve your own problems. You may use online resources, e.g. from [StackOverflow](https://stackoverflow.com/), a widely used source of discussion about [R code](https://stackoverflow.com/questions/tagged/r), but you **MUST** clearly indicate (cite) that your solution relies on found code, regardless of how much you have modified it for your own needs. Reused code that is discovered via a web search and which is not explicitly cited is plagiarism and it will be treated as such. On assignments you may not directly share code with your peers in this workshop.

## 13 Software

In this module we will rely entirely on [R](https://cran.r-project.org/) running within the [RStudio](https://www.rstudio.com/) IDE. The use of R was covered extensively in the [BCB744](https://tangledbank.netlify.app/BCB744/BCB744_index.html) module where the [installation process](https://tangledbank.netlify.app/BCB744/intro_r/01-RStudio.html) was discussed. We will primarily use the [**vegan**](https://cran.r-project.org/web/packages/vegan/index.html) package, but some useful functions are also provided by the package [**BiodiversityR**](https://github.com/cran/BiodiversityR) (and [here](http://apps.worldagroforestry.org/downloads/Publications/PDFS/b13695.pdf) and [here](https://rpubs.com/Roeland-KINDT)). Various other R packages offer overlapping and additional methods, but **vegan** should accommodate \>90% of your Quantitative Ecology needs.

## 14 Computers

You are encouraged to provide your own laptops and to install the necessary software before the module starts. Limited support can be provided if required. There are also computers with R and RStudio (and the necessary add-on libraries) available in the 5th floor lab in the BCB Department.

## 15 Attendance

This workshop-based, hands-on module can only deliver acceptable outcomes if you attend all classes. The schedule is set and cannot be changed. Sometimes an occasional absence cannot be avoided. Please be courteous and notify me or the tutor in advance of any absence. If you work with a partner in class, notify them too. Keep up with the reading assignments while you are away and we will all work with you to get you back up to speed on what you miss. If you do miss a class, however, the assignments must still be submitted on time (also see [**Late submission of CA**](#sec-policy)).

Since you may decide to work with a peer on tasks and assignments, please keep this person informed in case an emergency makes you unavailable for a period of time. Someone might depend on your input and contributions; do not leave someone unable to complete a task in your absence.

## 16 Support

Some tricky aspects of the module will take time to master, and the best way to master difficult material is to practise, practise some more, and then ask questions. Trying for 10 minutes and then giving up is not enough. I will be more sympathetic if you can show that you tried for a full day before asking me. When you ask questions about a challenge, explain your attempts to solve the problem and how those attempts failed. *I will not help you if you have not tried to help yourself first* (perhaps with advice from friends). There will be time in class to do this, typically before we start a new topic. You are also encouraged to bring up related questions that arise in your own B.Sc. (Hons.) research project.

Should you require more time with me, find out when I am ‘free’ and set an appointment by sending me a calendar invitation. I am happy to have a personal meeting with you via Zoom, but I prefer face-to-face in my office.

### 16.1 Help via BCB744 and BCB743 issues on GitHub

All discussion for the BCB744 and BCB743 workshops will be held in the [Issues](https://github.com/ajsmit/R_courses/issues) of [this repository](https://github.com/ajsmit/R_courses). Please post all content-related questions there, and use email only for personal matters. This is a public repository, so be professional in your writing here (grammar, etc.).

To start a new thread, create a **New issue**. Tag your peers using their handle, e.g. `@ajsmit`, to get their attention.

Once a question has been answered, the issue will be closed, so useful answers might end up in closed issues. Look there when searching for answers; you can use the **Search** feature on this repository to find answers to the same or similar problems.

**Guidelines for Posting Questions:**

- First search existing issues (open or closed) for answers. If the question has already been answered, you are done! If there is an open issue, feel free to contribute to it. Or feel free to open a closed issue if you believe the answer is not satisfactory.
- Give your issue an informative title.
  - Good: “Error: could not find function”ggplot””
  - Bad: “My code does not work!” Note that you can edit an issue’s title after it is been posted.
- Format your questions nicely using markdown and code formatting. Preview your issue prior to posting.
- As I explained above, your peers and I will more sympathetic to your cause if you can show *all the things you have tried as you, yourself, tried to fix the issue first*.
- Include code and example data so the person trying to help you have something to work with (and which results in the error, perhaps)
- Where appropriate, provide links to specific files, or even lines within them, in the body of your issue. This will help your peers understand your question. Note that only the teaching team will have access to private repos.
- (Optional) Tag someone or some group of people. Start by typing their GitHub username prefixed with the @ symbol. of course this supposes that each of you have a GitHub account and username.
- Hit **Submit new issue** when you are ready to post.

## 17 References

Borcard D, Gillet F, Legendre P, others (2011) Numerical ecology with R. Springer

Legendre P, Legendre L (2012) Numerical ecology. Elsevier

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit2026,
  author = {Smit, A. J.},
  title = {BCB743: {Quantitative} {Ecology}},
  date = {2026-11-06},
  url = {https://tangledbank.netlify.app/BCB743/BCB743_index.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ (2026) BCB743: Quantitative Ecology. [https://tangledbank.netlify.app/BCB743/BCB743_index.html.](https://tangledbank.netlify.app/BCB743/BCB743_index.html)
