<div id="quarto-document-content" class="content" role="main">

<div id="title-block-header" class="quarto-title-block default">

<div class="quarto-title">

# Building the ecoevidence R Package from Scratch

</div>

<div class="quarto-title-meta">

<div>

<div class="quarto-title-meta-heading">

Author

</div>

<div class="quarto-title-meta-contents">

Naledi Jijana

</div>

</div>

<div>

<div class="quarto-title-meta-heading">

Published

</div>

<div class="quarto-title-meta-contents">

July 22, 2026

</div>

</div>

</div>

</div>

<div id="introduction" class="section level1" number="1">

# <span class="header-section-number">1</span> Introduction

<div id="overview" class="section level2" number="1.1">

## <span class="header-section-number">1.1</span> Overview

The **ecoevidence** package was developed as part of an Honours research project in Biodiversity and Conservation Biology. The purpose of the package is to provide a reproducible framework that guides users from ecological community data through statistical analysis, ecological interpretation, and finally evidence-based conservation recommendations.

Unlike many ecological packages that focus only on statistical analyses, **ecoevidence** was designed as a teaching and decision-support tool. The package encourages users to think like ecologists by structuring analyses around ecological questions, community observations, pattern detection, hypothesis generation, evidence evaluation, ecological inference, and conservation recommendations.

This document serves as a complete development journal describing how the package was constructed from scratch. Every major stage of development is documented, including package creation, function development, documentation, debugging, package checking, and testing.

Throughout this tutorial, executable R code is provided together with explanations of why each step is necessary and how it contributes to the final package.

> **Development Note**
>
> This document reflects the complete development process of the package. During development, numerous debugging sessions were required to resolve documentation issues, namespace errors, missing exported functions, package structure problems, example data issues, and package checking warnings. These debugging steps are described throughout the tutorial so that future package developers can avoid similar problems.

------------------------------------------------------------------------

</div>

</div>

<div id="learning-objectives" class="section level1" number="2">

# <span class="header-section-number">2</span> Learning Objectives

By the end of this tutorial you should be able to

- Understand the structure of an R package.
- Create a package using **usethis**.
- Write documented R functions using **roxygen2**.
- Export functions correctly.
- Build S3 classes.
- Create custom print methods.
- Include datasets and external files.
- Debug package errors.
- Successfully run `devtools::check()`.
- Build a complete ecological analysis workflow.

------------------------------------------------------------------------

</div>

<div id="chapter-1-creating-the-package" class="section level1" number="3">

# <span class="header-section-number">3</span> Chapter 1 — Creating the Package

<div id="why-build-an-r-package" class="section level2" number="3.1">

## <span class="header-section-number">3.1</span> Why Build an R Package?

Many ecological analyses begin as individual scripts. While scripts are useful for exploratory analyses, they become difficult to maintain as projects grow. Functions are often duplicated, documentation is scattered across files, and reproducibility becomes increasingly difficult.

Packaging code solves these problems by organising functions into a structured framework with built-in documentation, version control, and reproducible workflows.

For the Honours project, the objective was not simply to analyse ecological data, but to create a reusable tool that could support ecological reasoning and evidence-based decision making.

------------------------------------------------------------------------

</div>

<div id="creating-the-package" class="section level2" number="3.2">

## <span class="header-section-number">3.2</span> Creating the Package

The package was created using the **usethis** package.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(usethis)

# Create the package
# Uncomment to create a new package

# create_package("ecoevidence")
```

</div>

</div>

<div id="explanation" class="section level3" number="3.2.1">

### <span class="header-section-number">3.2.1</span> Explanation

The `create_package()` function automatically generates the standard R package directory structure.

The most important components created include

| Folder      | Purpose                                   |
|-------------|-------------------------------------------|
| R           | Stores all package functions              |
| man         | Stores automatically generated help files |
| DESCRIPTION | Stores package metadata                   |
| NAMESPACE   | Controls exported functions               |
| .Rproj      | RStudio project file                      |

> **Side Note**
>
> The package directory should never be modified manually unless necessary. Functions such as `devtools::document()` automatically update documentation files and the namespace.

------------------------------------------------------------------------

</div>

</div>

<div id="opening-the-package" class="section level2" number="3.3">

## <span class="header-section-number">3.3</span> Opening the Package

Once created, the package can be opened as an RStudio project.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
# Example only

# open_project("ecoevidence")
```

</div>

</div>

Opening the package as an RStudio project ensures that relative file paths function correctly and package-building tools operate as expected.

------------------------------------------------------------------------

</div>

<div id="installing-required-packages" class="section level2" number="3.4">

## <span class="header-section-number">3.4</span> Installing Required Packages

Several packages were required during development.

install.packages(c( “devtools”, “roxygen2”, “usethis”, “vegan”, “ggplot2”, “dplyr”, “tidyr” ))

<div id="why-these-packages" class="section level3" number="3.4.1">

### <span class="header-section-number">3.4.1</span> Why These Packages?

| Package  | Purpose                              |
|----------|--------------------------------------|
| devtools | Build, document and check packages   |
| roxygen2 | Generate documentation automatically |
| usethis  | Package development tools            |
| vegan    | Ecological statistics                |
| ggplot2  | Graphics                             |
| dplyr    | Data manipulation                    |
| tidyr    | Data tidying                         |

------------------------------------------------------------------------

</div>

</div>

<div id="loading-the-development-environment" class="section level2" number="3.5">

## <span class="header-section-number">3.5</span> Loading the Development Environment

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(devtools)
library(roxygen2)
library(usethis)
library(vegan)
```

</div>

<div class="cell-output cell-output-stderr">

    Loading required package: permute

</div>

<div class="cell-output cell-output-stderr">


    Attaching package: 'permute'

</div>

<div class="cell-output cell-output-stderr">

    The following object is masked from 'package:devtools':

        check

</div>

</div>

At this point the package development environment is ready.

The next stage involves writing the first package function.

</div>

</div>

<div id="chapter-2-developing-the-first-function" class="section level1" number="4">

# <span class="header-section-number">4</span> Chapter 2 — Developing the First Function

<div id="why-start-with-eco_import" class="section level2" number="4.1">

## <span class="header-section-number">4.1</span> Why Start with `eco_import()`?

Every ecological workflow begins by importing and validating data. Rather than allowing users to analyse incorrectly formatted datasets, the package first checks that the community and environmental data are suitable for analysis.

The `eco_import()` function was therefore designed as the foundation of the package. All subsequent analyses rely on the object created by this function.

The design goals were to:

- standardise user inputs;
- check for common errors before analyses begin;
- combine related datasets into a single object; and
- create a consistent data structure for downstream functions.

------------------------------------------------------------------------

</div>

<div id="writing-the-first-function" class="section level2" number="4.2">

## <span class="header-section-number">4.2</span> Writing the First Function

The first version of the function was written in the `R/` directory of the package.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_import <- function(
  community,
  environment = NULL,
  coordinates = NULL
) {

  if (!is.data.frame(community)) {
    stop("community must be a data frame.")
  }

  study <- list(
    community = community,
    environment = environment,
    coordinates = coordinates
  )

  class(study) <- "EcoStudy"

  return(study)
}
```

</div>

</div>

<div id="understanding-the-function" class="section level3" number="4.2.1">

### <span class="header-section-number">4.2.1</span> Understanding the Function

The function accepts up to three datasets:

| Argument      | Description                                            |
|---------------|--------------------------------------------------------|
| `community`   | Species abundance or presence–absence data             |
| `environment` | Environmental variables associated with sampling sites |
| `coordinates` | Spatial coordinates for sampling locations             |

The function stores these components together in a single list and assigns the custom class `"EcoStudy"`.

This approach simplifies later analyses because downstream functions only need a single object rather than multiple independent datasets.

------------------------------------------------------------------------

</div>

</div>

<div id="creating-a-custom-class" class="section level2" number="4.3">

## <span class="header-section-number">4.3</span> Creating a Custom Class

A key design decision was to create an S3 class rather than returning an ordinary list.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
class(study) <- "EcoStudy"
```

</div>

</div>

Using an S3 class allows custom methods, such as printing and summarising objects, to be developed later in the package.

> **Side Note**
>
> S3 classes are one of the simplest object-oriented systems in R. They are widely used because they are flexible and easy to extend while remaining compatible with base R.

------------------------------------------------------------------------

</div>

<div id="testing-the-function" class="section level2" number="4.4">

## <span class="header-section-number">4.4</span> Testing the Function

Before adding documentation, the function should be tested using a small example dataset.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(vegan)

data(varespec)

study <- eco_import(
  community = varespec
)

study
```

</div>

</div>

If the package has not yet been installed, the function can be tested using:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::load_all()
```

</div>

</div>

which loads the package directly from the development folder without installing it.

------------------------------------------------------------------------

</div>

<div id="early-design-decisions" class="section level2" number="4.5">

## <span class="header-section-number">4.5</span> Early Design Decisions

During development several important decisions were made.

<div id="returning-a-single-object" class="section level3" number="4.5.1">

### <span class="header-section-number">4.5.1</span> Returning a Single Object

Instead of returning multiple datasets separately, all information was stored within one object.

This made later functions much easier to write because every function could expect exactly the same input structure.

</div>

<div id="using-named-components" class="section level3" number="4.5.2">

### <span class="header-section-number">4.5.2</span> Using Named Components

The object stores information using meaningful names.

``` text
EcoStudy
│
├── community
├── environment
└── coordinates
```

This improves readability and reduces programming errors.

------------------------------------------------------------------------

</div>

</div>

<div id="development-reflection" class="section level2" number="4.6">

## <span class="header-section-number">4.6</span> Development Reflection

Although `eco_import()` is one of the smallest functions in the package, it established the overall architecture used throughout the project.

Many later functions—including `eco_summary()`, `eco_plot()`, `eco_nmds()`, and `eco_workflow()`—were designed to accept an `EcoStudy` object as input. Establishing this common structure early in development made the package easier to extend, maintain, and document.

</div>

</div>

<div id="chapter-3-understanding-the-package-structure" class="section level1" number="5">

# <span class="header-section-number">5</span> Chapter 3 — Understanding the Package Structure

<div id="why-package-structure-matters" class="section level2" number="5.1">

## <span class="header-section-number">5.1</span> Why Package Structure Matters

An R package is much more than a collection of R scripts. Every file and folder has a specific purpose and contributes to making the package reproducible, maintainable, and easy to distribute.

When the **ecoevidence** package was first created, R automatically generated the basic package framework. As development progressed, additional files and folders were added to support documentation, example data, and package metadata.

Understanding this structure was an important part of the development process because many debugging issues were traced back to files being in the wrong location or missing entirely.

------------------------------------------------------------------------

</div>

<div id="the-package-directory" class="section level2" number="5.2">

## <span class="header-section-number">5.2</span> The Package Directory

A typical package structure is shown below.

``` text
ecoevidence/
│
├── DESCRIPTION
├── NAMESPACE
├── R/
├── man/
├── inst/
├── data/
├── tests/
├── vignettes/
└── ecoevidence.Rproj
```

Each component serves a different purpose.

------------------------------------------------------------------------

</div>

<div id="the-r-folder" class="section level2" number="5.3">

## <span class="header-section-number">5.3</span> The R Folder

The **R/** directory contains the source code for every function in the package.

Examples include:

- `eco_import.R`
- `eco_summary.R`
- `eco_plot.R`
- `eco_distance.R`
- `eco_cluster.R`
- `eco_nmds.R`
- `eco_detect_patterns.R`
- `eco_generate_hypotheses.R`
- `eco_evaluate.R`
- `eco_infer.R`
- `eco_workflow.R`

Each function was saved in its own script to improve readability and simplify maintenance.

> **Development Decision**
>
> Separating functions into individual files made debugging much easier than storing every function in one large script.

------------------------------------------------------------------------

</div>

<div id="the-description-file" class="section level2" number="5.4">

## <span class="header-section-number">5.4</span> The DESCRIPTION File

The DESCRIPTION file stores important information about the package.

Typical contents include:

- Package name
- Version
- Author
- Maintainer
- Description
- License
- Imports
- Encoding

Whenever a new dependency was introduced, the DESCRIPTION file had to be updated so that R knew which packages were required.

------------------------------------------------------------------------

</div>

<div id="the-namespace-file" class="section level2" number="5.5">

## <span class="header-section-number">5.5</span> The NAMESPACE File

The NAMESPACE file controls which functions are available to users after the package is loaded.

Rather than editing this file manually, it is automatically generated using:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::document()
```

</div>

</div>

This command reads the roxygen2 comments above each function and updates both the documentation and the NAMESPACE file.

------------------------------------------------------------------------

</div>

<div id="the-man-folder" class="section level2" number="5.6">

## <span class="header-section-number">5.6</span> The man Folder

The **man/** directory contains the help files generated from the roxygen2 documentation.

These files should never be edited manually because they are recreated each time `devtools::document()` is run.

------------------------------------------------------------------------

</div>

<div id="the-inst-folder" class="section level2" number="5.7">

## <span class="header-section-number">5.7</span> The inst Folder

The **inst/** directory stores files that need to be installed with the package but are not R code.

For the **ecoevidence** package, this directory was used to store supporting resources such as external data files.

One example was:

``` text
inst/extdata/mechanisms.csv
```

This file was later accessed using:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
system.file(
  "extdata",
  "mechanisms.csv",
  package = "ecoevidence"
)
```

</div>

</div>

Using `system.file()` ensures that files can be found regardless of where the package is installed.

------------------------------------------------------------------------

</div>

<div id="common-beginner-mistakes" class="section level2" number="5.8">

## <span class="header-section-number">5.8</span> Common Beginner Mistakes

Several issues encountered during development are common among first-time package developers.

| Problem | Solution |
|----|----|
| Editing the NAMESPACE manually | Use `devtools::document()` |
| Editing files in `man/` | Update roxygen comments instead |
| Placing data in the wrong folder | Use `data/` or `inst/extdata/` appropriately |
| Forgetting to update DESCRIPTION | Add required package dependencies before checking |

Recognising these conventions early helped make the package much easier to maintain.

------------------------------------------------------------------------

</div>

<div id="chapter-summary" class="section level2" number="5.9">

## <span class="header-section-number">5.9</span> Chapter Summary

At this stage of development, the package had a well-defined structure consisting of organised source code, automatically generated documentation, package metadata, and supporting files.

This foundation made it possible to continue developing new functionality while maintaining a reproducible and organised workflow.

In the next chapter, we will document how roxygen2 was used to generate help files and export functions automatically. \# Chapter 4 — Documenting Functions with roxygen2

</div>

<div id="why-documentation-matters" class="section level2" number="5.10">

## <span class="header-section-number">5.10</span> Why Documentation Matters

Writing functions is only one part of package development. For a package to be useful, every exported function should include clear documentation describing:

- what the function does;
- the purpose of each argument;
- what the function returns;
- examples of how to use it; and
- references where appropriate.

Rather than writing help files manually, the **ecoevidence** package uses the **roxygen2** package to generate documentation automatically.

This approach ensures that the documentation always stays synchronized with the source code.

------------------------------------------------------------------------

</div>

<div id="the-structure-of-a-roxygen-block" class="section level2" number="5.11">

## <span class="header-section-number">5.11</span> The Structure of a roxygen Block

A typical roxygen block appears immediately above the function definition.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
#' Import ecological datasets
#'
#' Imports community, environmental and optional spatial data into
#' a single EcoStudy object.
#'
#' @param community A data frame containing community data.
#' @param environment Optional environmental data.
#' @param coordinates Optional sampling coordinates.
#'
#' @return An object of class "EcoStudy".
#'
#' @export
eco_import <- function(
  community,
  environment = NULL,
  coordinates = NULL
){

}
```

</div>

</div>

Every line beginning with `#'` contributes to the function’s help page.

------------------------------------------------------------------------

</div>

<div id="understanding-the-tags" class="section level2" number="5.12">

## <span class="header-section-number">5.12</span> Understanding the Tags

The most common roxygen tags used during development are described below.

| Tag           | Purpose                               |
|---------------|---------------------------------------|
| `@param`      | Describes each function argument      |
| `@return`     | Describes what the function returns   |
| `@examples`   | Provides example code                 |
| `@export`     | Makes the function available to users |
| `@seealso`    | Links related functions               |
| `@references` | Adds literature references            |

These tags are converted automatically into `.Rd` help files.

------------------------------------------------------------------------

</div>

<div id="generating-documentation" class="section level2" number="5.13">

## <span class="header-section-number">5.13</span> Generating Documentation

Once the documentation has been written, it is compiled using:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::document()
```

</div>

</div>

Running this command performs two important tasks:

1.  updates the **man/** folder; and
2.  regenerates the **NAMESPACE** file.

During development, this command was run frequently after creating or modifying functions.

------------------------------------------------------------------------

</div>

<div id="viewing-help-files" class="section level2" number="5.14">

## <span class="header-section-number">5.14</span> Viewing Help Files

Once the package is loaded, documentation can be viewed directly from R.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
?eco_import
```

</div>

</div>

or

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
help("eco_import")
```

</div>

</div>

This allows users to access documentation without opening the source code.

------------------------------------------------------------------------

</div>

<div id="exporting-functions" class="section level2" number="5.15">

## <span class="header-section-number">5.15</span> Exporting Functions

One of the most common mistakes during package development is forgetting to export a function.

For example:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
#' @export
eco_summary <- function(study){

}
```

</div>

</div>

Without the `@export` tag, the function exists inside the package but cannot be called by users after loading the package.

A missing export often produces errors such as:

    could not find function "eco_summary"

During development, several functions initially appeared unavailable because the documentation and namespace had not yet been regenerated. Running `devtools::document()` after adding the appropriate roxygen tags resolved these issues.

------------------------------------------------------------------------

</div>

<div id="development-reflection-1" class="section level2" number="5.16">

## <span class="header-section-number">5.16</span> Development Reflection

Learning to use **roxygen2** was one of the most valuable aspects of package development.

Initially, it seemed unnecessary to document every function while the package was still under construction. However, as the number of functions increased, documentation became essential for keeping track of each function’s purpose, arguments, and expected outputs.

Automatic generation of the `man/` folder and `NAMESPACE` file also reduced the likelihood of human error and ensured that documentation remained consistent throughout the project.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-1" class="section level2" number="5.17">

## <span class="header-section-number">5.17</span> Chapter Summary

By the end of this stage, every major function in the **ecoevidence** package had:

- descriptive documentation;
- clearly defined input arguments;
- documented return values;
- executable examples; and
- automatic help pages generated using **roxygen2**.

In the next chapter, we will examine how the package was built around custom S3 classes and methods, allowing ecological data to be organised into reusable objects throughout the analysis workflow.

</div>

</div>

<div id="chapter-5-building-an-s3-class-for-ecological-data" class="section level1" number="6">

# <span class="header-section-number">6</span> Chapter 5 — Building an S3 Class for Ecological Data

<div id="why-use-an-s3-class" class="section level2" number="6.1">

## <span class="header-section-number">6.1</span> Why Use an S3 Class?

As the package grew, it became clear that passing multiple datasets between functions was inefficient and prone to errors.

For example, many analyses require:

- community data,
- environmental variables,
- sampling coordinates,
- metadata, and
- analysis results.

Passing each of these separately to every function would result in long function calls that are difficult to read and maintain.

Instead, all information was stored within a single object called an **EcoStudy**.

This object forms the foundation of the entire **ecoevidence** workflow.

------------------------------------------------------------------------

</div>

<div id="what-is-an-s3-class" class="section level2" number="6.2">

## <span class="header-section-number">6.2</span> What is an S3 Class?

An S3 class is one of R’s object-oriented programming systems.

Rather than creating completely new data structures, an S3 class simply assigns a class name to an existing object.

For example,

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
study <- list(
  community = community,
  environment = environment,
  coordinates = coordinates
)

class(study) <- "EcoStudy"
```

</div>

</div>

Although `study` is still a list internally, R now recognises it as an object of class **EcoStudy**.

------------------------------------------------------------------------

</div>

<div id="advantages-of-using-an-s3-class" class="section level2" number="6.3">

## <span class="header-section-number">6.3</span> Advantages of Using an S3 Class

Creating an S3 object provides several advantages.

<div id="standardised-inputs" class="section level3" number="6.3.1">

### <span class="header-section-number">6.3.1</span> 1. Standardised Inputs

Every function in the package receives the same type of object.

Instead of writing

<div class="code-copy-outer-scaffold">

``` r
eco_summary(community, environment, coordinates)
```

</div>

the workflow becomes

<div class="code-copy-outer-scaffold">

``` r
eco_summary(study)
```

</div>

This greatly simplifies the function interface.

------------------------------------------------------------------------

</div>

<div id="custom-print-methods" class="section level3" number="6.3.2">

### <span class="header-section-number">6.3.2</span> 2. Custom Print Methods

Objects can display meaningful summaries rather than large lists.

For example,

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
print(study)
```

</div>

</div>

can produce output such as

    EcoStudy Object

    Community matrix:
    96 sampling sites
    44 species

    Environmental variables:
    14 variables

    Coordinates:
    Available

instead of displaying every value stored in the object.

------------------------------------------------------------------------

</div>

<div id="future-expansion" class="section level3" number="6.3.3">

### <span class="header-section-number">6.3.3</span> 3. Future Expansion

New information can be added to the object without changing the function interface.

For example,

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
study$distance

study$nmds

study$clusters

study$evidence
```

</div>

</div>

can all be added as the analysis progresses.

This design makes the package highly modular.

------------------------------------------------------------------------

</div>

</div>

<div id="writing-a-print-method" class="section level2" number="6.4">

## <span class="header-section-number">6.4</span> Writing a Print Method

S3 classes allow custom methods to be written.

A simple print method might look like this.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
print.EcoStudy <- function(x, ...) {

  cat("EcoStudy Object\n")
  cat("----------------\n")

  cat("Community data:",
      nrow(x$community),
      "sites\n")

  if(!is.null(x$environment))
    cat("Environmental variables:",
        ncol(x$environment),
        "\n")

  invisible(x)
}
```

</div>

</div>

Whenever the user types

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
study
```

</div>

</div>

R automatically calls

<div class="code-copy-outer-scaffold">

``` r
print.EcoStudy()
```

</div>

without the user needing to know that the method exists.

------------------------------------------------------------------------

</div>

<div id="the-workflow-architecture" class="section level2" number="6.5">

## <span class="header-section-number">6.5</span> The Workflow Architecture

The package was intentionally designed so that every major function accepts an **EcoStudy** object.

    Community Data
            │
    Environmental Data
            │
    Coordinates
            │
            ▼
       eco_import()
            │
            ▼
         EcoStudy
            │
     ┌──────┼───────────┐
     │      │           │
     ▼      ▼           ▼
    Summary Plot    Distance
     │
     ▼
    NMDS
     │
     ▼
    Pattern Detection
     │
     ▼
    Hypothesis Generation
     │
     ▼
    Evidence Evaluation
     │
     ▼
    Inference
     │
     ▼
    Recommendations

This modular structure makes it possible to extend the package without rewriting existing functions.

------------------------------------------------------------------------

</div>

<div id="design-decisions" class="section level2" number="6.6">

## <span class="header-section-number">6.6</span> Design Decisions

Several important software engineering decisions were made during development.

<div id="encapsulation" class="section level3" number="6.6.1">

### <span class="header-section-number">6.6.1</span> Encapsulation

All related ecological information is stored in a single object.

</div>

<div id="reusability" class="section level3" number="6.6.2">

### <span class="header-section-number">6.6.2</span> Reusability

Functions do not need to know where the data originated.

</div>

<div id="consistency" class="section level3" number="6.6.3">

### <span class="header-section-number">6.6.3</span> Consistency

Every downstream function expects the same input format.

</div>

<div id="scalability" class="section level3" number="6.6.4">

### <span class="header-section-number">6.6.4</span> Scalability

Additional analyses can be added without changing previous functions.

------------------------------------------------------------------------

</div>

</div>

<div id="development-reflection-2" class="section level2" number="6.7">

## <span class="header-section-number">6.7</span> Development Reflection

Choosing to use an S3 class was one of the most influential design decisions made during development.

Although the package could have been written using ordinary data frames and lists, the use of an **EcoStudy** object resulted in cleaner function interfaces, improved readability, and a more maintainable code base.

This decision also made the package resemble many well-established R packages that rely on object-oriented programming to organise complex analytical workflows.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-2" class="section level2" number="6.8">

## <span class="header-section-number">6.8</span> Chapter Summary

At this stage of development, the package had evolved beyond a collection of independent functions.

Instead, it became a coherent analytical framework built around a reusable **EcoStudy** object that could be passed consistently between functions throughout the ecological workflow.

In the next chapter, we will begin developing the analytical functions that transform ecological data into evidence-based ecological inference.

</div>

</div>

<div id="chapter-6-developing-the-core-analytical-functions" class="section level1" number="7">

# <span class="header-section-number">7</span> Chapter 6 — Developing the Core Analytical Functions

<div id="introduction-1" class="section level2" number="7.1">

## <span class="header-section-number">7.1</span> Introduction

Once the package infrastructure had been established, the next stage involved developing the analytical functions that transform ecological datasets into meaningful ecological information.

Rather than creating one large function to perform every analysis, the package was deliberately designed using a **modular workflow**. Each function performs one specific task before passing its results to the next stage of the analysis.

This design improves readability, simplifies debugging, and makes it possible to reuse individual functions independently.

The major analytical functions developed during this stage included:

- `eco_summary()`
- `eco_plot()`
- `eco_distance()`
- `eco_cluster()`
- `eco_nmds()`

Together these functions provide the exploratory analyses required before ecological inference can begin.

------------------------------------------------------------------------

</div>

</div>

<div id="summarising-ecological-data" class="section level1" number="8">

# <span class="header-section-number">8</span> Summarising Ecological Data

The first analytical function developed was `eco_summary()`.

Its purpose is to provide a quick overview of the imported ecological dataset before more advanced analyses are performed.

Typical information reported includes

- number of sampling sites,
- number of species,
- environmental variables,
- missing values,
- species richness,
- abundance summaries.

A simplified implementation is shown below.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_summary <- function(study){

  stopifnot(inherits(study, "EcoStudy"))

  summary(study$community)

}
```

</div>

</div>

<div id="why-this-function-is-important" class="section level3" number="8.0.1">

### <span class="header-section-number">8.0.1</span> Why this function is important

Exploratory summaries allow users to identify obvious problems before statistical analyses begin.

For example,

- missing values
- empty sampling sites
- species recorded only once
- incorrect data formats

can often be detected immediately.

------------------------------------------------------------------------

</div>

</div>

<div id="visualising-ecological-data" class="section level1" number="9">

# <span class="header-section-number">9</span> Visualising Ecological Data

Following numerical summaries, the next step is graphical exploration.

The `eco_plot()` function provides quick visualisations of community data.

Possible plots include

- species abundance distributions
- richness histograms
- boxplots
- ordination plots
- environmental gradients

Example usage

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_plot(study)
```

</div>

</div>

Graphical exploration is an essential component of ecological data analysis because important patterns are often easier to recognise visually than numerically.

------------------------------------------------------------------------

</div>

<div id="calculating-ecological-distances" class="section level1" number="10">

# <span class="header-section-number">10</span> Calculating Ecological Distances

Most multivariate ecological analyses begin by calculating dissimilarities among sampling sites.

The package therefore includes the function

<div class="code-copy-outer-scaffold">

``` r
eco_distance()
```

</div>

which calculates ecological distance matrices.

Example

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_distance(
    study,
    method = "bray"
)
```

</div>

</div>

Several ecological distance measures may be supported, including

- Bray–Curtis
- Euclidean
- Jaccard
- Gower

Choosing an appropriate distance measure depends on the ecological question being investigated.

------------------------------------------------------------------------

</div>

<div id="cluster-analysis" class="section level1" number="11">

# <span class="header-section-number">11</span> Cluster Analysis

Cluster analysis groups sampling sites according to similarities in species composition.

The package implements this using

<div class="code-copy-outer-scaffold">

``` r
eco_cluster()
```

</div>

Example

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_cluster(
    study,
    method = "average"
)
```

</div>

</div>

Cluster analysis is particularly useful for identifying ecological communities, habitat types, and groups of similar sampling sites.

------------------------------------------------------------------------

</div>

<div id="non-metric-multidimensional-scaling-nmds" class="section level1" number="12">

# <span class="header-section-number">12</span> Non-metric Multidimensional Scaling (NMDS)

One of the most important analytical functions in the package is

<div class="code-copy-outer-scaffold">

``` r
eco_nmds()
```

</div>

NMDS is widely used in community ecology because it preserves ecological relationships without assuming linearity.

Example

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_nmds(
    study,
    distance = "bray"
)
```

</div>

</div>

The resulting ordination provides a low-dimensional representation of community similarity.

Sampling sites located close together have similar species composition, whereas sites located far apart are ecologically distinct.

------------------------------------------------------------------------

</div>

<div id="why-nmds-was-selected" class="section level1" number="13">

# <span class="header-section-number">13</span> Why NMDS Was Selected

Several ordination techniques are available in ecology.

Examples include

- PCA
- CA
- PCoA
- DCA
- NMDS

NMDS was selected because

- it performs well with ecological community data,
- it does not assume linear relationships,
- it handles many zero values,
- it is widely used in vegetation ecology.

These characteristics make it particularly suitable for biodiversity datasets.

------------------------------------------------------------------------

</div>

<div id="the-modular-workflow" class="section level1" number="14">

# <span class="header-section-number">14</span> The Modular Workflow

By this stage, the package workflow had evolved into the following sequence.

    eco_import()

    ↓

    eco_summary()

    ↓

    eco_plot()

    ↓

    eco_distance()

    ↓

    eco_cluster()

    ↓

    eco_nmds()

Each function performs a single well-defined task.

This modular architecture greatly simplified later package development because individual functions could be improved without affecting the rest of the workflow.

------------------------------------------------------------------------

</div>

<div id="software-engineering-reflection" class="section level1" number="15">

# <span class="header-section-number">15</span> Software Engineering Reflection

Developing the analytical functions highlighted an important software engineering principle:

> Small, specialised functions are easier to understand, test, document and debug than one large function that performs many unrelated tasks.

This philosophy influenced the design of every remaining function in the package.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-3" class="section level1" number="16">

# <span class="header-section-number">16</span> Chapter Summary

At this stage, the package was capable of importing ecological datasets, summarising community composition, visualising ecological patterns, calculating ecological dissimilarities, clustering sampling sites, and performing ordination analyses using NMDS.

These exploratory analyses provide the foundation for the next stage of the workflow, where observed ecological patterns are transformed into hypotheses and evidence-based ecological inference.

</div>

<div id="chapter-7-from-ecological-patterns-to-ecological-inference" class="section level1" number="17">

# <span class="header-section-number">17</span> Chapter 7 — From Ecological Patterns to Ecological Inference

<div id="introduction-2" class="section level2" number="17.1">

## <span class="header-section-number">17.1</span> Introduction

Exploratory analyses such as clustering and NMDS reveal patterns within ecological datasets, but they do not explain why those patterns occur. Interpretation remains the responsibility of the ecologist.

The primary objective of the **ecoevidence** package is to bridge this gap by providing a structured workflow that guides users from observed ecological patterns to evidence-based ecological inference.

To achieve this, a series of functions were developed that progressively transform statistical outputs into ecological reasoning.

The key functions developed during this stage were:

- `eco_detect_patterns()`
- `eco_generate_hypotheses()`
- `eco_evaluate()`
- `eco_evidence_score()`
- `eco_compare_mechanisms()`
- `eco_decision_tree()`
- `eco_infer()`
- `eco_recommend()`

------------------------------------------------------------------------

</div>

</div>

<div id="detecting-ecological-patterns" class="section level1" number="18">

# <span class="header-section-number">18</span> Detecting Ecological Patterns

The first stage of ecological reasoning is recognising meaningful patterns within the data.

The function

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_detect_patterns(study)
```

</div>

</div>

was designed to examine the outputs of previous analyses and identify observations that warrant further ecological investigation.

Examples of detectable patterns include:

- distinct community clusters;
- environmental gradients;
- species turnover;
- unusually diverse sampling sites;
- potential ecological outliers.

Rather than providing conclusions, this function highlights observations that may require explanation.

------------------------------------------------------------------------

</div>

<div id="generating-ecological-hypotheses" class="section level1" number="19">

# <span class="header-section-number">19</span> Generating Ecological Hypotheses

Once patterns have been identified, the next step is to propose plausible ecological explanations.

The package provides:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_generate_hypotheses(study)
```

</div>

</div>

Possible hypotheses might include:

- environmental filtering;
- habitat heterogeneity;
- disturbance;
- dispersal limitation;
- competition;
- anthropogenic impacts.

The purpose of this function is not to determine which hypothesis is correct but to encourage systematic ecological reasoning based on the observed evidence.

------------------------------------------------------------------------

</div>

<div id="evaluating-available-evidence" class="section level1" number="20">

# <span class="header-section-number">20</span> Evaluating Available Evidence

Ecological hypotheses should not be accepted solely because they appear plausible.

Instead, they should be evaluated against available evidence.

This process is implemented using

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_evaluate(study)
```

</div>

</div>

Evidence considered may include:

- species composition;
- environmental variables;
- spatial structure;
- ordination results;
- ecological theory.

The evaluation stage encourages users to distinguish between observations and interpretations.

------------------------------------------------------------------------

</div>

<div id="scoring-the-strength-of-evidence" class="section level1" number="21">

# <span class="header-section-number">21</span> Scoring the Strength of Evidence

One of the novel features of the package is the inclusion of an evidence-scoring system.

This stage is performed using

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_evidence_score(study)
```

</div>

</div>

Rather than treating every hypothesis equally, hypotheses are ranked according to the amount of supporting ecological evidence.

An illustrative scoring framework is shown below.

| Evidence Level | Interpretation |
|----|----|
| High | Strong support from multiple analyses |
| Moderate | Supported by some evidence but requires further investigation |
| Low | Weak or inconclusive support |

This scoring system provides a transparent approach to ecological interpretation.

------------------------------------------------------------------------

</div>

<div id="comparing-alternative-mechanisms" class="section level1" number="22">

# <span class="header-section-number">22</span> Comparing Alternative Mechanisms

Ecological systems are rarely explained by a single mechanism.

Several competing hypotheses may explain the same observed pattern.

To assist with this process, the package includes:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_compare_mechanisms(study)
```

</div>

</div>

Possible competing mechanisms may include:

- habitat filtering;
- disturbance;
- dispersal limitation;
- stochastic processes;
- species interactions.

Comparing multiple mechanisms encourages balanced ecological interpretation rather than premature conclusions.

------------------------------------------------------------------------

</div>

<div id="decision-support" class="section level1" number="23">

# <span class="header-section-number">23</span> Decision Support

Following evidence evaluation, the package guides users through a structured decision-making process.

This stage is represented by

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_decision_tree(study)
```

</div>

</div>

Rather than replacing ecological expertise, the decision tree provides a consistent framework for interpreting analytical results.

The objective is to reduce subjective interpretation while maintaining scientific flexibility.

------------------------------------------------------------------------

</div>

<div id="ecological-inference" class="section level1" number="24">

# <span class="header-section-number">24</span> Ecological Inference

After evaluating the available evidence, the final inference is produced using

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_infer(study)
```

</div>

</div>

The function synthesises information generated throughout the workflow to produce an evidence-based ecological interpretation.

Possible outputs may include:

- likely environmental drivers;
- confidence in the proposed explanation;
- remaining uncertainties;
- suggested future analyses.

This stage represents the culmination of the analytical workflow.

------------------------------------------------------------------------

</div>

<div id="generating-recommendations" class="section level1" number="25">

# <span class="header-section-number">25</span> Generating Recommendations

The final analytical stage involves translating ecological inference into practical recommendations.

This is implemented using

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_recommend(study)
```

</div>

</div>

Recommendations may include:

- additional field sampling;
- improved environmental monitoring;
- habitat restoration;
- conservation management actions;
- further statistical analyses.

This final step ensures that the analytical workflow contributes directly to ecological decision-making rather than ending with statistical outputs alone.

------------------------------------------------------------------------

</div>

<div id="integrating-the-workflow" class="section level1" number="26">

# <span class="header-section-number">26</span> Integrating the Workflow

By this stage, the complete analytical framework had evolved into the following sequence.

    eco_import()

    ↓

    eco_summary()

    ↓

    eco_plot()

    ↓

    eco_distance()

    ↓

    eco_cluster()

    ↓

    eco_nmds()

    ↓

    eco_detect_patterns()

    ↓

    eco_generate_hypotheses()

    ↓

    eco_evaluate()

    ↓

    eco_evidence_score()

    ↓

    eco_compare_mechanisms()

    ↓

    eco_decision_tree()

    ↓

    eco_infer()

    ↓

    eco_recommend()

This workflow demonstrates the progression from raw ecological observations to structured ecological inference and practical conservation recommendations.

------------------------------------------------------------------------

</div>

<div id="development-reflection-3" class="section level1" number="27">

# <span class="header-section-number">27</span> Development Reflection

Developing these functions represented a major shift in the project.

Rather than creating another package that performs statistical analyses, the focus became supporting ecological reasoning. The package was designed to encourage users to move beyond interpreting p-values and ordination plots, and instead develop transparent, evidence-based ecological explanations.

This philosophy distinguishes **ecoevidence** from many existing ecological analysis packages.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-4" class="section level1" number="28">

# <span class="header-section-number">28</span> Chapter Summary

At the conclusion of this stage, the package had evolved into a complete ecological decision-support framework.

Users could import ecological datasets, perform exploratory analyses, identify ecological patterns, generate and evaluate competing hypotheses, assess the strength of evidence, draw ecological inferences, and formulate practical recommendations for biodiversity conservation and ecosystem management.

The next chapter will describe how these individual functions were integrated into the master workflow function, `eco_workflow()`, allowing the entire analytical process to be completed through a single function call.

</div>

<div id="chapter-8-integrating-the-package-with-eco_workflow" class="section level1" number="29">

# <span class="header-section-number">29</span> Chapter 8 — Integrating the Package with `eco_workflow()`

<div id="introduction-3" class="section level2" number="29.1">

## <span class="header-section-number">29.1</span> Introduction

As the **ecoevidence** package expanded, the number of individual functions increased substantially. Although each function performed a specific task, users would have been required to execute every function manually and in the correct order.

For example, a complete analysis would require:

1.  importing the data;
2.  summarising the dataset;
3.  visualising ecological patterns;
4.  calculating ecological distances;
5.  performing cluster analysis;
6.  conducting NMDS ordination;
7.  detecting ecological patterns;
8.  generating ecological hypotheses;
9.  evaluating competing explanations;
10. assigning evidence scores;
11. comparing ecological mechanisms;
12. drawing ecological inferences; and
13. producing management recommendations.

While this modular design is beneficial for flexibility, it can be time-consuming for users unfamiliar with the workflow.

To simplify the analysis process, a master function called **`eco_workflow()`** was developed.

------------------------------------------------------------------------

</div>

</div>

<div id="purpose-of-eco_workflow" class="section level1" number="30">

# <span class="header-section-number">30</span> Purpose of `eco_workflow()`

The objective of `eco_workflow()` is to automate the complete ecological workflow while maintaining reproducibility and transparency.

Instead of executing numerous individual functions, the user only needs to call one function.

A typical analysis becomes:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
results <- eco_workflow(
  community = varespec,
  environment = varechem
)
```

</div>

</div>

This design greatly reduces the complexity of using the package.

------------------------------------------------------------------------

</div>

<div id="workflow-architecture" class="section level1" number="31">

# <span class="header-section-number">31</span> Workflow Architecture

Internally, the workflow function executes a sequence of analytical steps.

    Community Data
            │
    Environmental Data
            │
    Coordinates
            │
            ▼
     eco_import()
            │
            ▼
     eco_summary()
            │
            ▼
     eco_plot()
            │
            ▼
     eco_distance()
            │
            ▼
     eco_cluster()
            │
            ▼
     eco_nmds()
            │
            ▼
     eco_detect_patterns()
            │
            ▼
     eco_generate_hypotheses()
            │
            ▼
     eco_evaluate()
            │
            ▼
     eco_evidence_score()
            │
            ▼
     eco_compare_mechanisms()
            │
            ▼
     eco_decision_tree()
            │
            ▼
     eco_infer()
            │
            ▼
     eco_recommend()
            │
            ▼
         Final Results

Each stage receives the output from the previous stage, ensuring that the analytical process remains organised and reproducible.

------------------------------------------------------------------------

</div>

<div id="advantages-of-a-workflow-function" class="section level1" number="32">

# <span class="header-section-number">32</span> Advantages of a Workflow Function

Developing a workflow function provides several important advantages.

<div id="reproducibility" class="section level2" number="32.1">

## <span class="header-section-number">32.1</span> Reproducibility

Every analysis follows exactly the same sequence of analytical steps.

This reduces variation between users and improves scientific reproducibility.

------------------------------------------------------------------------

</div>

<div id="ease-of-use" class="section level2" number="32.2">

## <span class="header-section-number">32.2</span> Ease of Use

New users can perform sophisticated ecological analyses without needing to understand every internal function immediately.

Advanced users remain free to use individual functions independently.

------------------------------------------------------------------------

</div>

<div id="reduced-coding-errors" class="section level2" number="32.3">

## <span class="header-section-number">32.3</span> Reduced Coding Errors

Because functions are executed automatically in the correct order, users are less likely to omit important analytical steps.

------------------------------------------------------------------------

</div>

<div id="consistency-1" class="section level2" number="32.4">

## <span class="header-section-number">32.4</span> Consistency

Every analysis follows the same evidence-based decision framework.

This consistency is particularly valuable in ecological monitoring programmes where analyses are repeated over multiple years.

------------------------------------------------------------------------

</div>

</div>

<div id="modular-design" class="section level1" number="33">

# <span class="header-section-number">33</span> Modular Design

Although `eco_workflow()` automates the complete analysis, it does not replace the individual functions.

Instead, it acts as a coordinator.

Each analytical component remains independent.

For example,

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_summary(study)

eco_nmds(study)

eco_infer(study)
```

</div>

</div>

can still be used separately whenever more detailed investigation is required.

This modular architecture provides both simplicity and flexibility.

------------------------------------------------------------------------

</div>

<div id="error-handling" class="section level1" number="34">

# <span class="header-section-number">34</span> Error Handling

An important consideration during development was ensuring that errors were detected as early as possible.

Examples include:

- missing community data;
- incompatible environmental datasets;
- unsupported distance measures;
- missing package dependencies;
- incorrectly formatted objects.

Rather than allowing errors to propagate through the workflow, each function performs input validation before continuing.

This makes debugging considerably easier.

------------------------------------------------------------------------

</div>

<div id="software-engineering-considerations" class="section level1" number="35">

# <span class="header-section-number">35</span> Software Engineering Considerations

Several software engineering principles guided the development of `eco_workflow()`.

<div id="modularity" class="section level3" number="35.0.1">

### <span class="header-section-number">35.0.1</span> Modularity

Each function performs one clearly defined task.

</div>

<div id="reusability-1" class="section level3" number="35.0.2">

### <span class="header-section-number">35.0.2</span> Reusability

Functions can be used individually or within the workflow.

</div>

<div id="maintainability" class="section level3" number="35.0.3">

### <span class="header-section-number">35.0.3</span> Maintainability

Improvements to one function do not require rewriting the entire package.

</div>

<div id="extensibility" class="section level3" number="35.0.4">

### <span class="header-section-number">35.0.4</span> Extensibility

New analytical methods can be inserted into the workflow with minimal modification to existing code.

These principles improve the long-term sustainability of the package.

------------------------------------------------------------------------

</div>

</div>

<div id="development-reflection-4" class="section level1" number="36">

# <span class="header-section-number">36</span> Development Reflection

Designing `eco_workflow()` represented the transition from a collection of independent R functions to a cohesive analytical framework.

Rather than viewing ecological analyses as isolated statistical procedures, the package treats them as successive stages within a structured evidence-based workflow.

This approach reflects the broader philosophy of the **ecoevidence** package: guiding users from ecological observations to scientifically supported ecological inference and conservation recommendations.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-5" class="section level1" number="37">

# <span class="header-section-number">37</span> Chapter Summary

The introduction of `eco_workflow()` unified the package into a single analytical framework.

Users can now execute a complete ecological workflow using one function while still retaining access to every individual analytical component when greater flexibility is required.

In the next chapter, the focus shifts from software architecture to software quality assurance, where the package is tested, documented, checked, and refined before release.

</div>

<div id="chapter-9-testing-debugging-and-quality-assurance" class="section level1" number="38">

# <span class="header-section-number">38</span> Chapter 9 — Testing, Debugging and Quality Assurance

<div id="introduction-4" class="section level2" number="38.1">

## <span class="header-section-number">38.1</span> Introduction

Developing package functions is only one aspect of software development. Before a package can be shared or used in research, it must undergo thorough testing and quality assurance to ensure that it is reliable, reproducible, and free from major errors.

Throughout the development of **ecoevidence**, each newly implemented function was tested individually before being integrated into the complete analytical workflow. As development progressed, package-wide checks were performed using the **devtools** package to identify problems related to documentation, dependencies, exported functions, and package structure.

------------------------------------------------------------------------

</div>

</div>

<div id="incremental-development" class="section level1" number="39">

# <span class="header-section-number">39</span> Incremental Development

Rather than writing the entire package before testing, development followed an incremental approach.

The typical workflow was:

    Write a function

    ↓

    Test the function

    ↓

    Document the function

    ↓

    Run devtools::document()

    ↓

    Load the package

    ↓

    Test again

    ↓

    Fix errors

    ↓

    Repeat

This approach made it easier to identify the source of errors before they affected multiple components of the package.

------------------------------------------------------------------------

</div>

<div id="loading-the-development-version" class="section level1" number="40">

# <span class="header-section-number">40</span> Loading the Development Version

During development, the package was loaded directly from the source directory rather than being installed after every modification.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::load_all()
```

</div>

</div>

Using `load_all()` allowed changes to be tested immediately without rebuilding and reinstalling the package each time.

------------------------------------------------------------------------

</div>

<div id="checking-documentation" class="section level1" number="41">

# <span class="header-section-number">41</span> Checking Documentation

Whenever a function was modified, the documentation was regenerated.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::document()
```

</div>

</div>

This command automatically:

- generated help files;
- updated the **NAMESPACE** file; and
- ensured exported functions were available to users.

Running this command frequently reduced documentation-related errors.

------------------------------------------------------------------------

</div>

<div id="running-package-checks" class="section level1" number="42">

# <span class="header-section-number">42</span> Running Package Checks

The package was routinely checked using

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::check()
```

</div>

</div>

This command performs a comprehensive assessment of package quality.

Typical checks include:

- syntax errors;
- undocumented objects;
- missing exports;
- undocumented parameters;
- dependency issues;
- example execution;
- package installation;
- namespace consistency.

Regular package checks helped identify problems early in the development process.

------------------------------------------------------------------------

</div>

<div id="common-problems-encountered" class="section level1" number="43">

# <span class="header-section-number">43</span> Common Problems Encountered

Several issues arose during package development.

<div id="missing-function-exports" class="section level2" number="43.1">

## <span class="header-section-number">43.1</span> Missing Function Exports

Some functions were initially unavailable after loading the package.

Typical error:

    could not find function "eco_import"

Cause:

The function had not been exported.

Solution:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
#' @export
```

</div>

</div>

followed by

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::document()
```

</div>

</div>

------------------------------------------------------------------------

</div>

<div id="missing-documentation" class="section level2" number="43.2">

## <span class="header-section-number">43.2</span> Missing Documentation

Occasionally, package checks reported undocumented functions or parameters.

These issues were resolved by ensuring that every exported function contained complete roxygen documentation including:

- `@param`
- `@return`
- `@examples`
- `@export`

------------------------------------------------------------------------

</div>

<div id="namespace-problems" class="section level2" number="43.3">

## <span class="header-section-number">43.3</span> Namespace Problems

Some package functions relied on external packages.

Whenever a new dependency was introduced, the DESCRIPTION file was updated accordingly.

After modifying dependencies, the package documentation was regenerated to maintain namespace consistency.

------------------------------------------------------------------------

</div>

<div id="external-data-files" class="section level2" number="43.4">

## <span class="header-section-number">43.4</span> External Data Files

The package also required supporting files stored in

    inst/extdata/

Using

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
system.file()
```

</div>

</div>

allowed these files to be accessed regardless of where the package was installed.

This approach improved portability and reproducibility.

------------------------------------------------------------------------

</div>

</div>

<div id="why-continuous-testing-matters" class="section level1" number="44">

# <span class="header-section-number">44</span> Why Continuous Testing Matters

Testing was performed throughout development rather than only at the end of the project.

Continuous testing provided several benefits.

- Bugs were detected early.
- Errors were easier to trace.
- Documentation remained synchronised with the code.
- New functions integrated more smoothly with the existing workflow.

This reduced the time required to resolve complex software issues later in development.

------------------------------------------------------------------------

</div>

<div id="development-reflection-5" class="section level1" number="45">

# <span class="header-section-number">45</span> Development Reflection

One of the most valuable lessons learned during package development was that software quality depends as much on testing and documentation as it does on writing code.

Functions that appear to work correctly may still fail package checks if documentation is incomplete, dependencies are missing, or exported functions are not properly registered.

Developing **ecoevidence** therefore required continual refinement of both the analytical code and the surrounding package infrastructure.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-6" class="section level1" number="46">

# <span class="header-section-number">46</span> Chapter Summary

By the conclusion of this stage, the package had undergone repeated testing, documentation updates, and package-wide quality checks.

These quality assurance procedures ensured that the package was organised, reproducible, and suitable for continued development and future public release.

The next chapter presents a detailed debugging diary documenting the major problems encountered during development and the solutions implemented to resolve them.

</div>

<div id="chapter-10-debugging-diary-lessons-learned-during-development" class="section level1" number="47">

# <span class="header-section-number">47</span> Chapter 10 — Debugging Diary: Lessons Learned During Development

<div id="introduction-5" class="section level2" number="47.1">

## <span class="header-section-number">47.1</span> Introduction

Software development is rarely a linear process. Throughout the development of the **ecoevidence** package, numerous challenges were encountered, ranging from package structure and documentation issues to namespace conflicts and package checking warnings.

Rather than viewing these problems as setbacks, each debugging session contributed to a better understanding of R package development and ultimately improved the quality of the package.

This chapter documents the major issues encountered during development, explains why they occurred, and describes the solutions implemented to resolve them.

------------------------------------------------------------------------

</div>

</div>

<div id="debugging-strategy" class="section level1" number="48">

# <span class="header-section-number">48</span> Debugging Strategy

Whenever an error occurred, the same systematic approach was followed.

    Read the error message

    ↓

    Identify the source

    ↓

    Understand why the error occurred

    ↓

    Implement a solution

    ↓

    Test the solution

    ↓

    Run package checks again

Following a consistent debugging strategy prevented small issues from becoming larger software problems.

------------------------------------------------------------------------

</div>

<div id="problem-1-empty-testing-infrastructure" class="section level1" number="49">

# <span class="header-section-number">49</span> Problem 1 — Empty Testing Infrastructure

<div id="error" class="section level2" number="49.1">

## <span class="header-section-number">49.1</span> Error

During package checking, R reported that the package contained testing infrastructure but no actual test files.

The package included a `tests/` directory and supporting files, but no unit tests had yet been written.

</div>

<div id="cause" class="section level2" number="49.2">

## <span class="header-section-number">49.2</span> Cause

The testing framework had been created during package setup, but development was still focused on implementing package functionality.

As a result, the package contained an empty testing structure.

</div>

<div id="solution" class="section level2" number="49.3">

## <span class="header-section-number">49.3</span> Solution

Rather than keeping incomplete testing infrastructure, it was removed temporarily.

The intention was to reintroduce formal unit testing once the package reached a stable stage of development.

</div>

<div id="lesson-learned" class="section level2" number="49.4">

## <span class="header-section-number">49.4</span> Lesson Learned

Package components should only be included when they are actively being used. An incomplete testing framework may introduce unnecessary warnings and make package checks harder to interpret.

------------------------------------------------------------------------

</div>

</div>

<div id="problem-2-missing-exported-functions" class="section level1" number="50">

# <span class="header-section-number">50</span> Problem 2 — Missing Exported Functions

<div id="error-1" class="section level2" number="50.1">

## <span class="header-section-number">50.1</span> Error

After loading the package, some functions could not be found.

For example:

    could not find function "eco_import"

</div>

<div id="cause-1" class="section level2" number="50.2">

## <span class="header-section-number">50.2</span> Cause

The functions existed in the source code but had not been exported through **roxygen2**.

Without the `@export` tag, users cannot access package functions after loading the package.

</div>

<div id="solution-1" class="section level2" number="50.3">

## <span class="header-section-number">50.3</span> Solution

Each user-facing function was updated to include:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
#' @export
```

</div>

</div>

The package documentation was then regenerated.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::document()
```

</div>

</div>

</div>

<div id="lesson-learned-1" class="section level2" number="50.4">

## <span class="header-section-number">50.4</span> Lesson Learned

Writing a function is not sufficient for package development. Public functions must also be exported and documented.

------------------------------------------------------------------------

</div>

</div>

<div id="problem-3-documentation-not-updating" class="section level1" number="51">

# <span class="header-section-number">51</span> Problem 3 — Documentation Not Updating

<div id="error-2" class="section level2" number="51.1">

## <span class="header-section-number">51.1</span> Error

Changes made to function documentation were not reflected in the help files.

</div>

<div id="cause-2" class="section level2" number="51.2">

## <span class="header-section-number">51.2</span> Cause

The documentation had been edited, but the package documentation had not been regenerated.

</div>

<div id="solution-2" class="section level2" number="51.3">

## <span class="header-section-number">51.3</span> Solution

Running

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
devtools::document()
```

</div>

</div>

updated both the help files and the NAMESPACE.

</div>

<div id="lesson-learned-2" class="section level2" number="51.4">

## <span class="header-section-number">51.4</span> Lesson Learned

Documentation should be regenerated whenever function signatures or roxygen comments are modified.

------------------------------------------------------------------------

</div>

</div>

<div id="problem-4-external-data-files" class="section level1" number="52">

# <span class="header-section-number">52</span> Problem 4 — External Data Files

<div id="error-3" class="section level2" number="52.1">

## <span class="header-section-number">52.1</span> Error

Supporting files stored in `inst/extdata` were not found when the package was loaded.

</div>

<div id="cause-3" class="section level2" number="52.2">

## <span class="header-section-number">52.2</span> Cause

Package resources should not be accessed using hard-coded file paths.

Such paths depend on the user’s computer and are not portable.

</div>

<div id="solution-3" class="section level2" number="52.3">

## <span class="header-section-number">52.3</span> Solution

External resources were accessed using:

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
system.file(
  "extdata",
  "mechanisms.csv",
  package = "ecoevidence"
)
```

</div>

</div>

This ensures that files can be located regardless of where the package is installed.

</div>

<div id="lesson-learned-3" class="section level2" number="52.4">

## <span class="header-section-number">52.4</span> Lesson Learned

Always use package-aware file paths when accessing bundled resources.

------------------------------------------------------------------------

</div>

</div>

<div id="problem-5-package-check-warnings" class="section level1" number="53">

# <span class="header-section-number">53</span> Problem 5 — Package Check Warnings

Throughout development, repeated package checks revealed several warnings and notes.

Examples included:

- undocumented parameters;
- unused imports;
- incomplete documentation;
- namespace inconsistencies.

Rather than ignoring these warnings, each one was investigated individually.

Addressing small warnings early prevented more serious issues later in development.

------------------------------------------------------------------------

</div>

<div id="problem-6-dependency-management" class="section level1" number="54">

# <span class="header-section-number">54</span> Problem 6 — Dependency Management

As additional functionality was added, new package dependencies became necessary.

Whenever a new package was used, the DESCRIPTION file was updated accordingly.

Keeping dependencies organised ensured that the package remained portable and easy to install.

------------------------------------------------------------------------

</div>

<div id="general-debugging-principles" class="section level1" number="55">

# <span class="header-section-number">55</span> General Debugging Principles

Several important software engineering principles emerged during development.

<div id="read-the-error-carefully" class="section level2" number="55.1">

## <span class="header-section-number">55.1</span> Read the Error Carefully

Error messages often identify the exact location of a problem.

Understanding the message is usually more valuable than immediately searching for a solution online.

------------------------------------------------------------------------

</div>

<div id="make-one-change-at-a-time" class="section level2" number="55.2">

## <span class="header-section-number">55.2</span> Make One Change at a Time

Changing multiple parts of the code simultaneously makes it difficult to identify which modification resolved the issue.

Small, incremental changes are easier to test and verify.

------------------------------------------------------------------------

</div>

<div id="test-frequently" class="section level2" number="55.3">

## <span class="header-section-number">55.3</span> Test Frequently

Running package checks regularly reduced the number of accumulated errors and simplified debugging.

------------------------------------------------------------------------

</div>

<div id="keep-documentation-current" class="section level2" number="55.4">

## <span class="header-section-number">55.4</span> Keep Documentation Current

Synchronising documentation with the source code prevented inconsistencies between the package implementation and the generated help files.

------------------------------------------------------------------------

</div>

</div>

<div id="reflection" class="section level1" number="56">

# <span class="header-section-number">56</span> Reflection

The debugging process became one of the most educational aspects of developing **ecoevidence**.

Each problem encountered improved understanding of package architecture, documentation, dependency management, and software quality assurance.

Rather than being obstacles, these debugging sessions contributed directly to producing a more robust and maintainable package.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-7" class="section level1" number="57">

# <span class="header-section-number">57</span> Chapter Summary

Debugging formed an integral part of the package development process.

By systematically identifying problems, understanding their causes, and implementing targeted solutions, the package gradually evolved into a stable and reproducible analytical framework.

The experience gained through debugging was as valuable as writing the package functions themselves and reinforced the importance of careful software engineering practices in ecological research.

In the next chapter, the complete package workflow will be demonstrated using an example ecological dataset, illustrating how the individual components of **ecoevidence** operate together within a reproducible analysis.

</div>

<div id="chapter-11-demonstrating-the-complete-ecoevidence-workflow" class="section level1" number="58">

# <span class="header-section-number">58</span> Chapter 11 — Demonstrating the Complete ecoevidence Workflow

<div id="introduction-6" class="section level2" number="58.1">

## <span class="header-section-number">58.1</span> Introduction

Following the successful development, documentation, and testing of the **ecoevidence** package, the final stage was to demonstrate its use on a complete ecological dataset.

This chapter illustrates how the individual functions developed throughout the package interact to form a reproducible ecological analysis workflow.

The objective is not only to demonstrate the software, but also to show how statistical analyses can be translated into ecological interpretation and evidence-based recommendations.

------------------------------------------------------------------------

</div>

</div>

<div id="loading-the-required-packages" class="section level1" number="59">

# <span class="header-section-number">59</span> Loading the Required Packages

Before beginning an analysis, the package and its dependencies must be loaded.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
library(ecoevidence)
library(vegan)
```

</div>

</div>

The examples presented in this chapter use the **vegan** example datasets because they are widely used in ecological statistics and provide an excellent demonstration of community ecology workflows.

------------------------------------------------------------------------

</div>

<div id="loading-example-data" class="section level1" number="60">

# <span class="header-section-number">60</span> Loading Example Data

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
data(varespec)
data(varechem)
```

</div>

</div>

The datasets consist of:

- **varespec** – species abundance data
- **varechem** – environmental variables measured at the same sampling sites

These datasets are commonly used for demonstrating multivariate ecological analyses.

------------------------------------------------------------------------

</div>

<div id="running-the-complete-workflow" class="section level1" number="61">

# <span class="header-section-number">61</span> Running the Complete Workflow

The entire analysis can be completed using a single function.

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
results <- eco_workflow(
  community = varespec,
  environment = varechem
)
```

</div>

</div>

The workflow automatically performs the following stages:

1.  Data import
2.  Data validation
3.  Summary statistics
4.  Data visualisation
5.  Distance calculations
6.  Cluster analysis
7.  NMDS ordination
8.  Pattern detection
9.  Hypothesis generation
10. Evidence evaluation
11. Evidence scoring
12. Ecological inference
13. Management recommendations

This greatly simplifies the analytical process while maintaining transparency and reproducibility.

------------------------------------------------------------------------

</div>

<div id="using-individual-functions" class="section level1" number="62">

# <span class="header-section-number">62</span> Using Individual Functions

Although the package provides a complete workflow, each analytical function can also be used independently.

For example,

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
study <- eco_import(
  community = varespec,
  environment = varechem
)
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_summary(study)
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_plot(study)
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_distance(study)
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_nmds(study)
```

</div>

</div>

<div class="cell">

<div class="code-copy-outer-scaffold">

``` r
eco_infer(study)
```

</div>

</div>

This modular design allows users to perform only the analyses that are relevant to their research questions.

------------------------------------------------------------------------

</div>

<div id="expected-workflow" class="section level1" number="63">

# <span class="header-section-number">63</span> Expected Workflow

The analytical process can be summarised as follows.

    Import data
          │
          ▼
    Validate inputs
          │
          ▼
    Summarise dataset
          │
          ▼
    Visualise patterns
          │
          ▼
    Calculate distances
          │
          ▼
    Cluster analysis
          │
          ▼
    NMDS ordination
          │
          ▼
    Detect ecological patterns
          │
          ▼
    Generate hypotheses
          │
          ▼
    Evaluate evidence
          │
          ▼
    Assign evidence scores
          │
          ▼
    Ecological inference
          │
          ▼
    Generate recommendations

Each stage contributes additional ecological understanding while preserving the reproducibility of the workflow.

------------------------------------------------------------------------

</div>

<div id="advantages-of-the-workflow" class="section level1" number="64">

# <span class="header-section-number">64</span> Advantages of the Workflow

The integrated workflow provides several benefits.

- Reduces repetitive coding.
- Standardises ecological analyses.
- Encourages reproducible research.
- Guides users through ecological reasoning.
- Links statistical analyses with ecological interpretation.
- Produces evidence-based recommendations for conservation and ecosystem management.

------------------------------------------------------------------------

</div>

<div id="example-applications" class="section level1" number="65">

# <span class="header-section-number">65</span> Example Applications

Although developed as part of this Honours project, the package could be applied to a wide range of ecological studies.

Potential applications include:

- vegetation ecology;
- freshwater ecology;
- marine ecology;
- biodiversity monitoring;
- conservation planning;
- ecological impact assessments;
- long-term ecological monitoring programmes.

The modular design also allows additional analytical methods to be incorporated in future versions.

------------------------------------------------------------------------

</div>

<div id="development-reflection-6" class="section level1" number="66">

# <span class="header-section-number">66</span> Development Reflection

Developing a complete workflow function demonstrated the importance of integrating statistical analyses with ecological reasoning.

Rather than requiring users to manually coordinate multiple analytical steps, the package provides a structured framework that guides analyses from raw ecological observations to evidence-based ecological interpretation.

This workflow reflects the overall objective of **ecoevidence**: supporting transparent, reproducible, and scientifically defensible ecological decision-making.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-8" class="section level1" number="67">

# <span class="header-section-number">67</span> Chapter Summary

This chapter demonstrated how the functions developed throughout the package combine to produce a complete ecological workflow.

The modular architecture allows users to perform individual analyses where required while also providing the convenience of a fully automated workflow through `eco_workflow()`.

The next chapter presents the major lessons learned during package development and reflects on the broader implications of software engineering for ecological research.

</div>

<div id="chapter-12-lessons-learned-during-package-development" class="section level1" number="68">

# <span class="header-section-number">68</span> Chapter 12 — Lessons Learned During Package Development

<div id="introduction-7" class="section level2" number="68.1">

## <span class="header-section-number">68.1</span> Introduction

Developing the **ecoevidence** package was significantly different from writing ordinary R scripts for data analysis. While previous coursework focused primarily on producing statistical results, package development required consideration of software architecture, documentation, reproducibility, usability, and long-term maintenance.

Throughout this project, both programming skills and ecological reasoning developed simultaneously. Every stage of the development process provided valuable insight into how scientific software should be designed to support reproducible research.

------------------------------------------------------------------------

</div>

</div>

<div id="lesson-1-planning-before-programming" class="section level1" number="69">

# <span class="header-section-number">69</span> Lesson 1 — Planning Before Programming

One of the earliest lessons learned was the importance of planning the overall structure of a package before writing code.

Initially, it was tempting to begin implementing functions immediately. However, as additional functions were added, it became clear that having a well-defined workflow greatly simplified development.

Designing the package around a single analytical pipeline ensured that every function had a clearly defined role.

This planning phase ultimately reduced duplication, improved consistency, and simplified debugging.

------------------------------------------------------------------------

</div>

<div id="lesson-2-small-functions-are-better-than-large-functions" class="section level1" number="70">

# <span class="header-section-number">70</span> Lesson 2 — Small Functions Are Better Than Large Functions

Rather than creating one large function to perform every analytical task, the package was deliberately divided into smaller, specialised functions.

Examples include:

- `eco_import()`
- `eco_summary()`
- `eco_plot()`
- `eco_distance()`
- `eco_cluster()`
- `eco_nmds()`
- `eco_detect_patterns()`
- `eco_generate_hypotheses()`
- `eco_evaluate()`
- `eco_infer()`

Each function performs one clearly defined task.

This modular design improved readability, simplified testing, and made future expansion considerably easier.

------------------------------------------------------------------------

</div>

<div id="lesson-3-documentation-is-part-of-programming" class="section level1" number="71">

# <span class="header-section-number">71</span> Lesson 3 — Documentation Is Part of Programming

Initially, documentation appeared to be an additional task separate from coding.

However, throughout development it became clear that documentation is an essential component of software development.

Writing clear documentation required careful consideration of:

- function inputs;
- expected outputs;
- assumptions;
- intended users.

The use of **roxygen2** greatly simplified this process by generating consistent help files directly from the source code.

------------------------------------------------------------------------

</div>

<div id="lesson-4-debugging-improves-understanding" class="section level1" number="72">

# <span class="header-section-number">72</span> Lesson 4 — Debugging Improves Understanding

Many of the most valuable learning experiences occurred while resolving errors rather than while writing new code.

Examples included:

- package structure issues;
- namespace problems;
- missing exports;
- documentation inconsistencies;
- dependency management;
- package checking warnings.

Each debugging session improved understanding of both R package development and software engineering principles.

------------------------------------------------------------------------

</div>

<div id="lesson-5-reproducibility-is-essential" class="section level1" number="73">

# <span class="header-section-number">73</span> Lesson 5 — Reproducibility Is Essential

One of the primary objectives of package development was to ensure that ecological analyses could be reproduced by other researchers.

The package therefore encourages users to follow a structured workflow in which analyses are performed consistently.

This improves scientific transparency and reduces the likelihood of analytical errors.

------------------------------------------------------------------------

</div>

<div id="lesson-6-software-engineering-supports-ecological-research" class="section level1" number="74">

# <span class="header-section-number">74</span> Lesson 6 — Software Engineering Supports Ecological Research

Developing the package demonstrated that ecological software involves more than implementing statistical methods.

Good software should also be:

- organised;
- well documented;
- reproducible;
- maintainable;
- easy to use.

These software engineering principles are directly applicable to ecological research because they improve the reliability and transparency of scientific analyses.

------------------------------------------------------------------------

</div>

<div id="personal-reflection" class="section level1" number="75">

# <span class="header-section-number">75</span> Personal Reflection

Developing **ecoevidence** has been one of the most technically demanding components of this Honours project.

The project required learning concepts that extended beyond ecological statistics, including package architecture, object-oriented programming, documentation systems, debugging strategies, and reproducible software development.

The experience has strengthened both programming skills and confidence in developing scientific software for ecological applications.

Perhaps the most important lesson learned is that software development is an iterative process. Initial versions are rarely perfect, and continual refinement is an expected and valuable part of creating robust analytical tools.

------------------------------------------------------------------------

</div>

<div id="contribution-to-the-honours-project" class="section level1" number="76">

# <span class="header-section-number">76</span> Contribution to the Honours Project

Although the package was developed to support the Honours research project, its value extends beyond the immediate study.

The package provides:

- a reproducible ecological workflow;
- a structured approach to ecological reasoning;
- transparent evidence evaluation;
- reusable analytical functions; and
- a foundation for future software development.

These features make the package a valuable complement to the ecological analyses presented in the dissertation.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-9" class="section level1" number="77">

# <span class="header-section-number">77</span> Chapter Summary

Developing **ecoevidence** provided valuable experience in software engineering, reproducible research, and ecological analysis.

The project demonstrated that successful scientific software depends not only on correct statistical methods but also on careful planning, documentation, testing, debugging, and long-term maintainability.

These lessons will inform future research projects and provide a strong foundation for continued development of ecological software.

</div>

<div id="chapter-13-future-development-of-the-ecoevidence-package" class="section level1" number="78">

# <span class="header-section-number">78</span> Chapter 13 — Future Development of the ecoevidence Package

<div id="introduction-8" class="section level2" number="78.1">

## <span class="header-section-number">78.1</span> Introduction

The current version of **ecoevidence** provides a reproducible framework for ecological data analysis, evidence evaluation, and ecological inference. However, software development is an iterative process, and there are numerous opportunities to extend the package in future releases.

The modular architecture of the package was intentionally designed to facilitate the addition of new analytical methods without requiring substantial changes to the existing workflow.

------------------------------------------------------------------------

</div>

</div>

<div id="expanding-ecological-analyses" class="section level1" number="79">

# <span class="header-section-number">79</span> Expanding Ecological Analyses

Future versions of the package could incorporate additional multivariate statistical techniques that are widely used in community ecology.

Potential additions include:

- Permutational Multivariate Analysis of Variance (PERMANOVA)
- Distance-based Redundancy Analysis (dbRDA)
- Canonical Correspondence Analysis (CCA)
- Redundancy Analysis (RDA)
- Indicator Species Analysis
- Variation Partitioning
- Beta Diversity Analysis

These methods would broaden the analytical capabilities of the package while maintaining the existing workflow.

------------------------------------------------------------------------

</div>

<div id="improving-data-visualisation" class="section level1" number="80">

# <span class="header-section-number">80</span> Improving Data Visualisation

Although the current package provides basic graphical outputs, future versions could include more advanced visualisation tools.

Possible improvements include:

- publication-quality figures using **ggplot2**;
- interactive ordination plots;
- environmental vector overlays;
- heatmaps of species abundance;
- species accumulation curves;
- biodiversity dashboards.

Improved visualisation would make ecological patterns easier to interpret and communicate.

------------------------------------------------------------------------

</div>

<div id="interactive-user-interface" class="section level1" number="81">

# <span class="header-section-number">81</span> Interactive User Interface

At present, the package is operated entirely through R code.

A future development goal is to create a **Shiny** application that provides a graphical user interface.

Such an interface would enable users with limited programming experience to:

- upload ecological datasets;
- select analytical methods;
- visualise results interactively; and
- export figures and reports.

This would increase the accessibility of the package for students, practitioners, and conservation managers.

------------------------------------------------------------------------

</div>

<div id="comprehensive-unit-testing" class="section level1" number="82">

# <span class="header-section-number">82</span> Comprehensive Unit Testing

Although functions were tested throughout development, a future version should include a comprehensive suite of automated unit tests.

Using the **testthat** package would allow developers to verify that functions continue to operate correctly as new features are introduced.

Examples of future tests include:

- input validation;
- expected output formats;
- error handling;
- edge cases;
- regression tests for previously fixed bugs.

Automated testing would improve the long-term reliability of the package.

------------------------------------------------------------------------

</div>

<div id="creating-a-pkgdown-website" class="section level1" number="83">

# <span class="header-section-number">83</span> Creating a pkgdown Website

A dedicated **pkgdown** website would improve the accessibility of the package documentation.

Such a website could include:

- installation instructions;
- function reference pages;
- tutorials;
- worked examples;
- frequently asked questions.

This would allow users to access documentation without opening R.

------------------------------------------------------------------------

</div>

<div id="cran-readiness" class="section level1" number="84">

# <span class="header-section-number">84</span> CRAN Readiness

A long-term objective is to prepare the package for submission to the Comprehensive R Archive Network (CRAN).

Achieving this would require:

- complete documentation;
- comprehensive testing;
- clean package checks with no errors or warnings;
- consistent coding standards;
- version control and release management.

CRAN submission would make the package publicly available to the wider ecological research community.

------------------------------------------------------------------------

</div>

<div id="applications-beyond-this-honours-project" class="section level1" number="85">

# <span class="header-section-number">85</span> Applications Beyond This Honours Project

Although **ecoevidence** was developed within the context of this Honours research project, the underlying framework has broader applications.

Potential application areas include:

- terrestrial ecology;
- freshwater ecology;
- marine ecology;
- conservation planning;
- ecological restoration;
- biodiversity monitoring;
- environmental impact assessment;
- protected area management.

The flexible workflow allows researchers to adapt the package to a wide range of ecological questions.

------------------------------------------------------------------------

</div>

<div id="long-term-vision" class="section level1" number="86">

# <span class="header-section-number">86</span> Long-Term Vision

The long-term vision for **ecoevidence** is to develop it into a comprehensive ecological decision-support package that integrates statistical analysis with ecological reasoning.

Rather than functioning solely as a collection of analytical tools, the package aims to provide a structured framework for evidence-based ecological interpretation and conservation decision-making.

Future versions may also incorporate machine learning methods, Bayesian approaches to evidence evaluation, and spatial analysis tools to further enhance its capabilities.

------------------------------------------------------------------------

</div>

<div id="development-reflection-7" class="section level1" number="87">

# <span class="header-section-number">87</span> Development Reflection

Developing the first version of **ecoevidence** established a strong foundation for future work.

Although additional functionality can be added over time, the current architecture provides a stable, modular, and extensible framework upon which future developments can be built.

The experience gained during this project has also provided valuable insight into software engineering practices that will support continued package development beyond the completion of the Honours degree.

------------------------------------------------------------------------

</div>

<div id="chapter-summary-10" class="section level1" number="88">

# <span class="header-section-number">88</span> Chapter Summary

The current version of **ecoevidence** represents the first stage of a broader software development project.

Future enhancements will focus on expanding analytical functionality, improving user accessibility, strengthening software quality assurance, and increasing the package’s impact within ecological research and conservation practice.

</div>

<div id="chapter-14-conclusion" class="section level1" number="89">

# <span class="header-section-number">89</span> Chapter 14 — Conclusion

<div id="overview-1" class="section level2" number="89.1">

## <span class="header-section-number">89.1</span> Overview

The development of the **ecoevidence** package represented an opportunity to combine ecological knowledge with modern software engineering practices to produce a reproducible analytical framework for community ecology.

Rather than functioning as a collection of independent statistical functions, the package was designed as a complete ecological workflow that guides users from raw ecological observations to evidence-based ecological interpretation and conservation recommendations.

Throughout the development process, emphasis was placed on reproducibility, modular programming, documentation, maintainability, and transparency. These principles are essential for producing scientific software that can be confidently used, evaluated, and extended by other researchers.

------------------------------------------------------------------------

</div>

</div>

<div id="project-outcomes" class="section level1" number="90">

# <span class="header-section-number">90</span> Project Outcomes

The primary objective of this project was successfully achieved through the development of a modular R package capable of supporting ecological analyses and structured ecological reasoning.

Major achievements include:

- development of a complete R package from the ground up;
- implementation of reusable analytical functions;
- creation of a structured ecological workflow;
- integration of documentation using **roxygen2**;
- implementation of custom S3 classes;
- development of a workflow function that automates the analytical process;
- establishment of reproducible package architecture; and
- creation of a framework that links statistical analyses with ecological interpretation.

Collectively, these achievements demonstrate both technical and ecological competence.

------------------------------------------------------------------------

</div>

<div id="contribution-to-ecological-research" class="section level1" number="91">

# <span class="header-section-number">91</span> Contribution to Ecological Research

One of the distinguishing characteristics of **ecoevidence** is its emphasis on ecological reasoning rather than statistical output alone.

Many existing software packages provide sophisticated statistical methods but leave the interpretation entirely to the user.

The design philosophy of **ecoevidence** is different.

The package encourages users to progress systematically through:

1.  data exploration;
2.  identification of ecological patterns;
3.  generation of ecological hypotheses;
4.  evaluation of supporting evidence;
5.  ecological inference; and
6.  formulation of practical recommendations.

This structured workflow promotes transparent and reproducible ecological decision-making.

------------------------------------------------------------------------

</div>

<div id="software-engineering-contributions" class="section level1" number="92">

# <span class="header-section-number">92</span> Software Engineering Contributions

Beyond its ecological applications, the project also demonstrates several important software engineering principles.

These include:

- modular programming;
- object-oriented design using S3 classes;
- automatic documentation generation;
- reproducible workflows;
- systematic debugging;
- continuous quality assurance; and
- extensible package architecture.

These practices improve software reliability and facilitate future development.

------------------------------------------------------------------------

</div>

<div id="personal-development" class="section level1" number="93">

# <span class="header-section-number">93</span> Personal Development

Developing **ecoevidence** significantly strengthened my programming skills and deepened my understanding of reproducible scientific software development.

The project required learning concepts that extended well beyond routine statistical analysis, including:

- package architecture;
- documentation systems;
- namespace management;
- dependency management;
- software testing;
- debugging strategies; and
- workflow design.

These experiences have provided valuable skills that will support future research and software development projects.

------------------------------------------------------------------------

</div>

<div id="broader-significance" class="section level1" number="94">

# <span class="header-section-number">94</span> Broader Significance

Reproducible analytical tools are becoming increasingly important as ecological datasets continue to grow in complexity.

Software packages such as **ecoevidence** contribute to reproducible research by reducing analytical inconsistencies, improving transparency, and encouraging standardised workflows.

Although developed within the context of this Honours project, the concepts presented here are broadly applicable to ecological monitoring, biodiversity assessment, conservation planning, and environmental management.

------------------------------------------------------------------------

</div>

<div id="final-reflection" class="section level1" number="95">

# <span class="header-section-number">95</span> Final Reflection

Developing **ecoevidence** demonstrated that scientific software development is an iterative process requiring continual refinement, testing, and documentation.

The project progressed from an initial concept into a structured analytical framework capable of supporting ecological analyses in a transparent and reproducible manner.

Perhaps the most valuable outcome was recognising that robust ecological software depends not only on statistical methods but also on thoughtful design, careful documentation, and rigorous quality assurance.

These lessons will continue to inform future research and software development throughout my academic and professional career.

------------------------------------------------------------------------

</div>

<div id="closing-statement" class="section level1" number="96">

# <span class="header-section-number">96</span> Closing Statement

The **ecoevidence** package represents the successful integration of ecological theory, statistical analysis, and reproducible software engineering into a single analytical framework.

It provides a strong foundation for future development while demonstrating how carefully designed software can support transparent ecological research and evidence-based conservation decision-making.

The knowledge gained throughout this project extends beyond the package itself and reflects the broader importance of reproducible computational methods in modern ecological science.

------------------------------------------------------------------------

</div>

<div id="references" class="section level1" number="97">

# <span class="header-section-number">97</span> References

The package development process was informed by standard R package development practices and ecological analytical methods, including documentation generated using **roxygen2**, package development tools provided by **devtools** and **usethis**, and multivariate ecological analyses implemented through the **vegan** package. \# Acknowledgement of Artificial Intelligence (AI) Assistance

The development of the **ecoevidence** package and the preparation of this software development tutorial were supported through the responsible use of OpenAI’s ChatGPT (GPT-5.5) as an academic writing and programming assistant.

AI was used to assist with tasks including:

- improving the clarity and organisation of written explanations;
- refining technical descriptions of R package development;
- suggesting software engineering best practices;
- explaining R programming concepts and package development workflows;
- assisting with debugging strategies and interpretation of error messages;
- improving the structure and readability of documentation; and
- providing guidance on the presentation of software development processes.

All package design decisions, analytical methods, software implementation, ecological concepts, function development, testing, debugging, and final editorial decisions were undertaken and verified by the author. Every code example included in this document was reviewed, adapted where necessary, and evaluated for correctness before inclusion.

AI assistance was used as a supplementary learning and writing aid rather than a substitute for independent software development or scientific reasoning. The author remained responsible for the design, implementation, validation, interpretation, and final presentation of all work contained within this document.

The use of AI in this project aligns with principles of transparency, academic integrity, and responsible research practice.

</div>

</div>
