// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)
#import "@preview/fontawesome:0.5.0": *

#show: doc => article(
  title: [BCB744 (BioStatistics): Final Integrative Assessment],
  authors: (
    ( name: [Smit, A. J.],
      affiliation: [University of the Western Cape],
      email: [] ),
    ),
  date: [2025-08-03],
  sectionnumbering: "1.1.a",
  pagenumbering: "1",
  toc: true,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#block[
#heading(
level: 
2
, 
numbering: 
none
, 
[
Honesty Pledge
]
)
]
#strong[This assignment requires that you work as an individual and not share your code, results, or discussion with your peers. Penalties and disciplinary action will apply if you are found cheating.]

#block[
#callout(
body: 
[
Copy the statement, below, into your document and replace the underscores with your name acknowledging adherence to the UWC's Honesty Pledge.

#strong[I, \_\_\_\_\_\_\_\_\_\_\_\_, hereby state that I have not communicated with or gained information in any way from my peers and that all work is my own.]

]
, 
title: 
[
Acknowledgement of the Pledge
]
, 
background_color: 
rgb("#dae6fb")
, 
icon_color: 
rgb("#0758E5")
, 
icon: 
fa-info()
, 
body_background_color: 
white
)
]
#block[
#heading(
level: 
2
, 
numbering: 
none
, 
[
Instructions
]
)
]
Please carefully adhere to the following guidelines. Non-compliance may result in deductions.

- #strong[Convert Quarto to HTML:] Submit your assignment as an HTML file, derived from a Quarto document. Ensure your submission is a #emph[thoroughly annotated report];, complete with meta-information (name, date, purpose, etc.) at the beginning. Each section/test should be accompanied by detailed explanations of its purpose.

- #strong[Testing Assumptions:] For all questions necessitating formal inferential statistics, conduct and document the appropriate preliminary tests to check statistical assumptions. This includes stating the assumptions, detailing the procedures for testing these assumptions, and specifying the null hypotheses ($H_0$). If assumptions are tested graphically, elucidate the rationale behind the graphical method. Discuss the outcomes of these assumption tests and provide a rationale for the chosen inferential statistical tests (e.g., #emph[t];-test, ANOVA).

- #strong[State Hypotheses:] When inferential statistics are employed, clearly articulate the null ($H_0$) and alternative ($H_A$) hypotheses. Later, in the results section, remember to state whether the $H_0$ or $H_A$ is accepted or rejected.

- #strong[Graphical Support:] Support all descriptive and inferential statistical analyses with appropriate graphical representations of the data.

- #strong[Presentation Format:] Structure each answer as a concise mini-paper, including the sections Introduction, Methods, Results, Discussion, and References. Though each answer is expected to span 2-3 pages, there are no strict page limits. \[Does not apply to questions marked with an \*\]

  - Incorporate a #strong[Preamble] section before the Introduction to detail preliminary analyses, figures, tables, and other relevant background information that doesn't fit into the main narrative of your paper. This section provides insight into the preparatory work and will not be considered part of the main evaluation.

  - The #strong[Introduction] should set the stage by offering background information, establishing the relevance of the study, and clearly stating the research question or hypothesis.

  - The #strong[Methods] section must specify the statistical methodologies applied, including how assumptions were tested and any additional data analyses performed. Emphasise the inferential statistics without delving into exploratory data analysis (EDA).

  - In the #strong[Results] section, focus solely on the findings pertinent to the hypotheses introduced in the Introduction. While assumption tests are part of the statistical analysis, they need not be highlighted in this section (unless they necessitated the decision to use a non-parametric test or a data transformation). Ensure that figure and/or table captions are informative and self-explanatory.

  - The #strong[Discussion] section is for interpreting the results, considering their significance, limitations, and implications, and suggesting avenues for future research. You may reference up to five pertinent studies in the Methods and Discussion sections.

  - End with a consolidated #strong[References] section, listing all sources cited across the questions.

- #strong[Formatting:] Presentation matters. Marks are allocated for the visual quality of the submission. This includes the neatness of the document, proper use of headings, and adherence to coding conventions (e.g., spacing).

- #strong[MARK ALLOCATION] Please see the #link("https://tangledbank.netlify.app/bcb744/bcb744_index#summative-tasks")[Introduction Page] for an explanation of the assessment approach that will be applied to these questions.

Submit the .html file wherein you provide answers to Questions 1--7 by no later than 21:00, Saturday, 19 July 2025. Label the script as follows:

BCB744\_\<Name\>\_\<Surname\>\_BioStats\_Exam\_rewrite\_2025, e.g.

BCB744\_AJ\_Smit\_BioStats\_Exam\_rewrite\_2025.html.

Email your answers to AJ Smit by no later than 21:00 on 19 July 2025.

= Question 1: Effects of Mercury-Contaminated Fish Consumption on Chromosomes
<question-1-effects-of-mercury-contaminated-fish-consumption-on-chromosomes>
== Dataset Overview
<dataset-overview>
The dataset `mercuryfish`, available in the R package #strong[coin];, comprises measurements of mercury levels in blood, and proportions of cells exhibiting abnormalities and chromosome aberrations. This data is collected from individuals who consume mercury-contaminated fish and a control group with no such exposure. For detailed attributes and dataset structure, refer to the dataset's documentation within the package.

== Objectives
<objectives>
Your analysis should aim to address the following research questions:

#block[
#set enum(numbering: "a.", start: 1)
+ #strong[Impact of Methyl-Mercury:] Is the consumption of fish containing methyl-mercury associated with an increased proportion of cellular abnormalities?

+ #strong[Mercury Concentration and Cellular Abnormalities:] How does the concentration of mercury in the blood affect the proportion of cells with abnormalities? Moreover, is there a difference in this relationship between the control group and those exposed to mercury?

+ #strong[Relationship Between Variables:] Does a relationship exist between the proportion of abnormal cells (`abnormal`) and the proportion of cells with chromosome aberrations (`ccells`)? This analysis should be conducted separately for the control and exposed groups to identify any disparities.
]

= Question 2: Malignant Glioma Pilot Study
<question-2-malignant-glioma-pilot-study>
== Dataset Introduction
<dataset-introduction>
The `glioma` dataset, found within the #strong[coin] R package, originates from a pilot study focusing on patients with malignant glioma who underwent pretargeted adjuvant radioimmunotherapy using yttrium-90-biotin. This dataset includes variables such as patient sex, treatment group, age, histology (tissue study), and survival time.

== Objectives
<objectives-1>
This analysis aims to investigate the following aspects:

#block[
#set enum(numbering: "a.", start: 1)
+ #strong[Sex and Group Interaction on Survival Time:] Determine whether there is an interaction between patient sex and treatment group that significantly impacts the survival time (`time`).

+ #strong[Age and Histology Interaction on Survival Time:] Assess if age and histology interact in a way that influences the survival time of patients.

+ #strong[Comprehensive Data Exploration:] Conduct an exhaustive graphical examination of the dataset to uncover any additional patterns or relationships that merit statistical investigation. Identify the most compelling and insightful observation, formulate a relevant hypothesis, and perform the appropriate statistical analysis.
]

= Question 3: Risk factors associated with low infant birth weight
<question-3-risk-factors-associated-with-low-infant-birth-weight>
== Dataset Introduction
<dataset-introduction-1>
Package #strong[MASS];, dataset `birthwt`: This dataframe has 189 rows and 10 columns. The data were collected at Baystate Medical Center, Springfield, Mass. during 1986.

== Objectives
<objectives-2>
State three hypotheses and test them. Make sure one of the tests makes use of the 95% confidence interval approach rather than a formal inferential methodology.

= Question 4: The lung capacity data
<question-4-the-lung-capacity-data>
== Objectives
<objectives-3>
#block[
#set enum(numbering: "a.", start: 1)
+ Using the #link("https://github.com/ajsmit/R_courses/raw/main/static/data/LungCapData.csv")[Lung Capacity] data provided, please calculate the 95% CIs for the `LungCap` variable as a function of:

  #block[
  #set enum(numbering: "i.", start: 1)
  + `Gender`

  + `Smoke`

  + `Caesarean`
  ]

+ Create a graph of the mean ± 95% CIs and determine if there are statistical differences in `LungCap` between the levels of `Gender`, `Smoke`, and `Caesarean`. Do the same using a #emph[t];-test. Are your findings the same using these two approaches?

+ Produce all the associated tests for assumptions -- i.e.~the assumptions to be met when deciding whether to use a #emph[t];-test or its non-parametric counterpart.

+ Create a combined tidy dataframe (observe tidy principles) with the estimates for the 95% CI for the `LungCap` data (`LungCap` as a function of `Gender`), estimated using both the traditional and bootstrapping approaches. Create a plot comprising two panels (one for the traditional estimates, one for the bootstrapped estimates) of the mean, median, scatter of raw data points, and the upper and lower 95% CI.

+ Undertake a statistical analysis that factors in the effect of `Age` together with one of the categorical variables on `LungCap`. What new insight does this provide?
]

= Question 5: Piglet data
<question-5-piglet-data>
== Objectives
<objectives-4>
Here are some fictitious data for pigs raised on different diets (make up an equally fictitious justification for the data and develop hypotheses around that):

#block[
```r
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5, 110.3)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

bacon <- data.frame(cbind(feed_1, feed_2, feed_3, feed_4))
```

]
= Question 6: Investigating the Impact of Biochar on Crop Growth and Nutritional Value
<question-6-investigating-the-impact-of-biochar-on-crop-growth-and-nutritional-value>
== Overview of Dataset
<overview-of-dataset>
In this analysis, we will explore the effects of biochar application on the growth and elemental composition of four key crops: carrot, lettuce, soybean, and sweetcorn. The dataset for this study is sourced from the US Environmental Protection Agency (EPA) and is available at #link("https://catalog.data.gov/dataset/biochar-4-crops")[EPA's Biochar Dataset];. To gain a comprehensive understanding of the dataset and its implications, it is highly recommended to review two pertinent research papers linked on the dataset page. These papers not only provide valuable background information on the studies conducted but also offer critical insights and methodologies for data analysis that may be beneficial for this project.

== Research Goals
<research-goals>
The primary aim of this project is to analyse the impact of biochar on plant yield and identify the three most significant nutrients that influence human health. Your task is to:

+ Determine whether biochar treatments vary in effectiveness across the different crops.
+ Provide evidence-based recommendations on how to tailor biochar application for each specific crop to optimise the production of nutrients beneficial to human health and achieve the best possible yield.

In the Introduction section, it is crucial to justify the selection of the three nutrients you will focus on, explaining their importance to human nutrition. Through detailed data analysis, this project seeks to offer actionable insights on biochar application strategies that enhance both the nutritional value and the biomass of the crops by the end of their growth period.

= Question 7\*
<question-7>
== Objectives
<objectives-5>
#block[
#set enum(numbering: "a.", start: 1)
+ For each line of the script, below, write an English explanation for what the code does.
]

#block[
```r
ggplot(points, aes(x = group, y = count)) +
  geom_boxplot(aes(colour = group), size = 1, outlier.colour = NA) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3) +
  facet_grid(group ~ ., scales = "free") +
    labs(x = "", y = "Number of data points") +
  theme(legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank())
```

]
#block[
#set enum(numbering: "a.", start: 2)
+ Using the `rnorm()` function, generate some fictitious data that can be plotted using the code, above. Make sure to assemble these data into a dataframe suitable for plotting, complete with correct column titles.

+ Apply the code #emph[exactly as stated] to the data to demonstate your understanding of the code and convince the examiner of your understanding of the correct data structure.
]

== The end. Thank you for playing along, and have a happy weekend…
<the-end.-thank-you-for-playing-along-and-have-a-happy-weekend>


 
  
#set bibliography(style: "../marine-biology.csl") 


#bibliography("../references.bib")

