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
  title: [Ordination],
  authors: (
    ( name: [Smit, A. J.],
      affiliation: [University of the Western Cape],
      email: [] ),
    ),
  date: [2021-01-01],
  sectionnumbering: "1.1.a",
  pagenumbering: "1",
  toc: true,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#block[
#callout(
body: 
[
#table(
  columns: (20.83%, 26.39%, 52.78%),
  align: (auto,auto,auto,),
  table.header([Type], [Name], [Link],),
  table.hline(),
  [#strong[Slides];], [Ordination lecture slides], [#link("../slides/BCB743_07_ordination.pdf")[💾 `BCB743_07_ordination.pdf`];],
  [#strong[Reading];], [Vegan--An Introduction to Ordination], [#link("../docs/Oksanen_intro-vegan.pdf")[💾 `Oksanen_intro-vegan.pdf`];],
)
]
, 
title: 
[
#strong[Material required for this chapter]
]
, 
background_color: 
rgb("#ccf1e3")
, 
icon_color: 
rgb("#00A047")
, 
icon: 
fa-lightbulb()
, 
body_background_color: 
white
)
]
The following methods are covered in the lecture slides. You are expected to be familiar with how to select the appropriate method, and how to execute each. Supplement your studying by accessing these sources: Numerical Ecology with R, GUSTA ME (see links immediately below), and Analysis of Community Ecology Data in R:

- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:pca")[Principal Component Analysis (PCA)]
- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:ca_dca")[Correspondence Analysis (CA)]
- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:ca_dca")[Detrended Correspondence Analysis (DCA)]
- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_nmds")[Principal Coordinate Analysis (PCoA)]
- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_nmds")[non-Metric Multidimensional Scaling (nMDS)]
- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:rda_cca")[Redundancy Analysis (RDA)]
- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:rda_cca")[Canonical Correspondence Analysis (CCA)]
- #link("https://www.davidzeleny.net/anadat-r/doku.php/en:rda_cca")[Distance-based Redundancy Analysis Analysis (CCA)]

Ordination comes from the Latin word #emph[ordinatio];, which means placing things in order @legendre2012numerical. In ecology and some other sciences, it refers to a suite of multivariate statistical techniques used to analyse and visualise complex, high-dimensional data, such as ecological community data. In other words, high-dimensional data are ordered along some 'reduced axes' that explain patterns seen in nature. While #link("cluster_analysis.qmd")[clustering methods] focus on identifying discontinuities or groups within the data, ordination aims to highlight and interpret gradients, which are ubiquitous in ecological communities.

Ordination is well-suited for handling multivariate ecological data, which can represent:

- A spatial context (e.g., a landscape) comprised of many sites (rows), each one characterised by multiple variables (columns), such as species abundances or environmental factors.
- A time series (e.g., repeated sampling) comprised of many samples (rows), each one containing multiple variables (columns), such as species or environmental variables.
- Multidimensional or multivariate data, where the number of dimensions (columns with information about species or environmental variables) approaches the number of samples (sites or times).

In such complex, high-dimensional data, analysing each variable separately using a series of univariate or bivariate analyses would be inefficient and unlikely to reveal the underlying patterns accurately. For example, in the Doubs River dataset, a univariate approach would require (27 × 26) / 2 = 351 separate analyses, which is impractical and prone to misinterpretation.

The multivariate data about environmental properties or species composition, which we present to the analyses as tables of species or environmental variables, can be prepared in different ways. The most common workflows involve the following steps (#ref(<fig-data-tables>, supplement: [Figure])):

- #strong[Species data];: A table where each row represents a site or sample, and each column represents a species. The values in the table are species abundances, presences, or other species-related data.
- #strong[Environmental data];: A table where each row represents a site or sample, and each column represents an environmental variable. The values in the table are environmental measurements, such as temperature, pH, or nutrient concentrations.

From here, we can derive the following types of matrices:

- #strong[Species × species association matrix];: A matrix that quantifies the similarity or dissimilarity between species based on their co-occurrence patterns across sites.
- #strong[Site × site matrix of species dissimilarities];: A matrix that quantifies the ecological differences between sites based on the species composition.
- #strong[Site × variable table of standardised environmental data];: A table with standardised environmental conditions at each site.
- #strong[Site × site matrix of environmental distances];: A matrix that quantifies the environmental differences between sites based on the environmental variables.
- #strong[Variable × variable correlation matrix];: A matrix that quantifies the relationships between environmental variables.

#figure([
#box(image("../images/spp_env_data.png"))
], caption: figure.caption(
position: bottom, 
[
Species and environmental tables and what to do with them.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-data-tables>


Some of these newly-calculated matrices are then used as starting points for the ordination analyses.

= Dimension Reduction
<dimension-reduction>
Ordination is a dimension reduction method. It:

- Takes high-dimensional data (many columns).
- Applies scaling and rotation.
- Reduces the complexity to a low-dimensional space (orthogonal axes).

Ordination represents the complex data along a reduced number of orthogonal axes (linearly independent and uncorrelated), constructed in such a way that they capture the main trends or gradients in the data in decreasing order of importance. Each orthogonal axis captures a portion of the variation attributed to the original variables (columns). Interpretation of these axes is aided by visualisations (biplots), regressions, and clustering techniques.

Essentially, ordination geometrically arranges (projects) sites or species into a simplified dataset, where distances between them in the Cartesian 2D or 3D space represent their ecological or species dissimilarities. In this simplified representation, the further apart the shapes representing sites or species are on the graph, the larger the ecological differences between them.

#block[
#callout(
body: 
[
Imagine you have a 3D pear and a strong beam of light that casts the pear's shadow onto a flat surface. When you place the pear in the beam of light, the shadow that forms on the surface represents a 2D projection of the 3D object. Depending on how you rotate the pear, the shadow can appear in different shapes. Sometimes, it looks like the characteristic pear shape, while other times, it might resemble a round disc or an elongated ellipse.

'Projection' in ordination works in a similar way. Consider the original data as the 3D pear, existing in a high-dimensional space where each dimension represents a different variable. The goal of ordination is to find new axes (principal components) that capture the most insightful variations in the data. These axes are akin to the rotation of the pear in the beam light to cast the shadow.

When you 'project' the data onto these new axes, you are essentially rotating the pear in the light beam to create a 2D (or lower-dimensional) shadow on a plane. This shadow, or projection, represents the data in a reduced form. Just like rotating the pear reveals different shapes of shadows, rotating the data (changing the axes) in ordination can reveal different structures and patterns within the data. Some rotations will clearly show the underlying structure (e.g., the pear shape), while others might obscure it (e.g., the round disc).

This process of projection helps in visualising complex, high-dimensional data in a simpler form and makes it easier to identify patterns, clusters, and relationships between variables.

]
, 
title: 
[
Analogy of what an ordination does
]
, 
background_color: 
rgb("#ccf1e3")
, 
icon_color: 
rgb("#00A047")
, 
icon: 
fa-lightbulb()
, 
body_background_color: 
white
)
]
The reduced axes are ordered by the amount of variation they capture, with the first axis capturing the most variation, the second axis capturing the second most, and so on. The axes are orthogonal, so they are uncorrelated. They are linear combinations of the original variables, making them interpretable.

"Ordination primarily endeavours to represent sample and species relationships as faithfully as possible in a low-dimensional space" (Gauch, 1982). This is necessary because visualising multiple dimensions (species or variables) simultaneously in community data is extremely challenging, if not impossible. Ordination compromises between the number of dimensions and the amount of information retained. Ecologists are frequently confronted by 10s, if not 100s, of variables, species, and samples. A single multivariate analysis also saves time compared to conducting separate univariate analyses for each species or variable. What we really want is for the dimensions of this 'low-dimensional space' to represent important and interpretable environmental gradients.

= Benefits of Ordination
<benefits-of-ordination>
An ecological reason for preferring ordination over multiple univariate analyses is that species do not occur in isolation but in communities. Species in a community are interdependent and influenced by the same environmental factors. As such, community patterns may differ from population patterns. Some ordination methods can also offer insights into β diversity, which is the variation in species composition among sites.

A statistical reason for avoiding multiple univariate analyses is the increased probability of making a Type I error (rejecting a true null hypothesis) with numerous tests, known as the problem of multiple comparisons. In contrast, multivariate analysis has a single test, enhancing statistical power by considering species in aggregate due to redundancy in the data.

Ordination focuses on "important dimensions," avoiding the interpretation of noise, thus acting as a "noise reduction technique" (Gauch, 1982). It allows determining the relative importance of different gradients, which is virtually impossible with univariate techniques. For example, one can assess whether the first axis represents a stronger gradient than the second axis.

A major benefit of ordination is that its numeric output lends itself to graphical representation, often leading to intuitive interpretations of species-environment relationships. This is useful for communicating results to non-specialists.

= Types of Ordinations
<types-of-ordinations>
The first group of ordination techniques includes #strong[eigen-analysis methods];, which use linear algebra for dimensionality reduction. The second group includes #strong[non-eigen-analysis methods];, which use iterative algorithms for dimensionality reduction. I will cover both classes in this lecture, with non-Metric Multidimensional Scaling being the only example of the second group.

The eigen-analysis methods produce outputs called eigenvectors and eigenvalues, which are then used to determine the most important patterns or gradients in the data. These properties and applications of eigenvectors and eigenvalues will be covered in subsequent sections. The non-eigen approach instead uses numerical optimisation to find the best representation of the data in a lower-dimensional space.

Below, I prefer a classification of the ordination methods into constrained and unconstrained methods. This classification is based on the type of information used to construct the ordination axes, and how they are used. Constrained methods use environmental data to construct the axes, while unconstrained methods do not. The main difference between these two classes is that constrained methods are hypothesis-driven, while unconstrained methods are exploratory.

== Unconstrained Ordination (Indirect Gradient Analysis)
<unconstrained-ordination-indirect-gradient-analysis>
These are not statistical techniques (no inference testing); they are purely descriptive. Sometimes they are called indirect gradient analysis. These analyses are based on either the environment × sites matrix or the species × sites matrix, each analysed and interpreted in isolation. The main goal is to find the main gradients in the data. We apply indirect gradient analysis when the gradients are unknown #emph[a priori];, and we do not have environmental data related to the species. Gradients or other influences that structure species in space are therefore inferred from the species composition data only. The communities thus reveal the presence (or absence) of gradients, but may not offer insight into the identity of the structuring gradients. The most common methods are:

- #strong[#link("PCA.qmd")[Principal Component Analysis (PCA)];:] The main eigenvector-based method, working on raw, quantitative data. It preserves the Euclidean (linear) distances among sites, mainly used for environmental data but also applicable to species dissimilarities.
- #strong[#link("CA.qmd")[Correspondence Analysis (CA)];:] Works on data that must be frequencies or frequency-like, dimensionally homogeneous, and non-negative. It preserves the $chi^2$ distances among rows or columns, mainly used in ecology to analyse species data tables.
- #strong[#link("DCA.qmd")[Detrended Correspondence Analysis (DCA)];:] A variant of CA that is more suitable for species data tables with long environmental gradients which creates an interesting visual effect in the ordination diagram, called the #emph[arch-effect];. Detrending linearises the species response to environmental gradients.
- #strong[#link("PCoA.qmd")[Principal Coordinate Analysis (PCoA)];:] Devoted to the ordination of dissimilarity or distance matrices, often in the Q mode instead of site-by-variables tables, offering great flexibility in the choice of association measures.
- #strong[#link("nMDS.qmd")[non-Metric Multidimensional Scaling (nMDS)];:] A non-eigen-analysis method that works on dissimilarity or rank-order distance matrices to study the relationship between sites or species. nMDS represents objects along a predetermined number of axes while preserving the ordering relationships among them.

== Constrained Ordination (Direct Gradient Analysis)
<constrained-ordination-direct-gradient-analysis>
#link("constrained_ordination.qmd")[Constrained ordination] adds a level of statistical testing and is also called direct gradient analysis or canonical ordination. It typically uses explanatory variables (in the environmental matrix) to explain the patterns seen in the species matrix. The main goal is to find the main gradients in the data and test the significance of these gradients. So, we use constrained ordination when important gradients are hypothesised. Likely evidence for the existence of gradients is measured and captured in a complementary environmental dataset that has the same spatial structure (rows) as the species dataset. Direct gradient analysis is performed using linear or non-linear regression methods that relate the ordination performed on the species to its matching environmental variables. The most common methods are:

- #strong[Redundancy Analysis (RDA)];: A constrained form of PCA, where ordination is constrained by environmental variables, used to study the relationship between species and environmental variables.
- #strong[Canonical Correspondence Analysis (CCA)];: A constrained form of CA, where ordination is constrained by environmental variables, used to study the relationship between species and environmental variables.
- #strong[Detrended Canonical Correspondence Analysis (DCCA)];: A constrained form of CA, used to study the relationship between species and environmental variables.
- #strong[#link("constrained_ordination.qmd")[Distance-Based Redundancy Analysis (db-RDA)];:] A constrained form of PCoA, where ordination is constrained by environmental variables, used to study the relationship between species and environmental variables.

PCoA and nMDS can produce ordinations from any square #link("dis-metrics.qmd")[dissimilarity or distance matrix];, offering more flexibility than PCA and CA, which require site-by-species tables. PCoA and nMDS are also more robust to outliers and missing data than PCA and CA.

= Ordination Diagrams
<ordination-diagrams>
Ordination analyses are typically presented through graphical representations called ordination diagrams, which provide a simplified visual summary of the relationships between samples (the rows), species (columns), and environmental variables (also columns) in multivariate ecological data.

== Basic Elements of Ordination Diagrams
<basic-elements-of-ordination-diagrams>
- Sample Representation:
  - Individual samples or plots (rows) are displayed as points or symbols.
  - The relative positions of these points reflect the similarity (points plotting closer together) or dissimilarity (points spread further apart) between samples based on their species composition.
- Species Representation:
  - In linear methods (e.g., PCA, RDA): Species are represented by arrows, with direction indicating increasing abundance and length suggesting rate of change.
  - In weighted averaging methods (e.g., CA, CCA): Species are shown as points, representing their optimal position (often suggesting a unimodal distribution).
- Environmental Variable Representation:
  - Quantitative Variables: Displayed as vectors, with the arrows' direction showing the gradient of increasing values and length indicating correlation strength with ordination axes.
  - Qualitative Variables: Represented by centroids (average positions) for each category.
- Default plot options use base graphics, but more advanced visualisations can be created using #strong[ggplot2];.

== Construction of the Ordination Space
<construction-of-the-ordination-space>
- The coordinates given by the eigenvectors (species and site scores) are displayed on a 2D plane, typically using PC1 and PC2 (or PC1 and PC3, etc.) as axes.
- This creates a biplot, simultaneously plotting sites as points and environmental variables as vectors.
- The loadings (coefficients of original variables) define the reduced-space 'landscape' across which sites are scattered.
- Different scaling options (e.g., site scaling vs.~species scaling) can emphasise different aspects of the data.

== Interpretation of the Diagram
<interpretation-of-the-diagram>
- Sample Relationships:
  - Proximity between sample points indicates similarity in species composition.
  - The spread of sites along environmental arrows represents their position along that gradient.
- Species-Environment Relationships:
  - The angle between species arrows or their distance from sample points reflects association or abundance patterns.
  - The arrangement of sites in the reduced ordination space represents their relative positions in the original multidimensional space.
- Environmental Gradients:
  - Arrow length indicates the strength of the relationship between the variable and the principal component.
  - The cosine of the angle between arrows represents the correlation between environmental variables.
  - Parallel arrows suggest positive correlation, opposite arrows indicate negative correlation, and perpendicular arrows suggest uncorrelated variables.
- Biplots are heuristic tools and patterns should be further tested for statistical significance if necessary.
- Outliers can greatly influence the ordination and should be carefully examined.

 
  
#set bibliography(style: "../marine-biology.csl") 


#bibliography("../references.bib")

