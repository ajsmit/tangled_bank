<div id="quarto-document-content" class="content" role="main">

<div id="title-block-header" class="quarto-title-block default">

<div class="quarto-title">

# Linking Communities Across Landscapes: A Quantitative Introduction to Metacommunity Ecology

BCB743 Quantitative Ecology — Chapter Contribution (Assignment 2)

</div>

<div class="quarto-title-meta">

<div>

<div class="quarto-title-meta-heading">

Authors

</div>

<div class="quarto-title-meta-contents">

B. Effendi

A. Waglay

C. E. McIntyre

</div>

</div>

<div>

<div class="quarto-title-meta-heading">

Published

</div>

<div class="quarto-title-meta-contents">

July 24, 2026

</div>

</div>

</div>

</div>

<div class="callout callout-style-default callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>**Material Required for This Chapter**

</div>

</div>

<div class="callout-body-container callout-body">

| Type | Name | Link |
|:---|:---|:---|
| **Slides** | Metacommunity Ecology lecture slides | [💾 `BCB743_Metacommunity_Ecology.pdf`](file:///C:/Users/27848/Desktop/university%20stuff/honours%20UWC/Quant/MetacommunityChapter/slides/BCB743_Metacommunity_Ecology.pdf) |
| **Reading** | Leibold et al. (2004) | [💾 `Leibold_et_al_2004.pdf`](file:///C:/Users/27848/Desktop/university%20stuff/honours%20UWC/Quant/MetacommunityChapter/docs/Leibold_et_al_2004.pdf) |
|  | Vellend (2010) | [💾 `Vellend_2010.pdf`](file:///C:/Users/27848/Desktop/university%20stuff/honours%20UWC/Quant/MetacommunityChapter/docs/Vellend_2010.pdf) |
|  | Logue et al. (2011) | [💾 `Logue_et_al_2011.pdf`](file:///C:/Users/27848/Desktop/university%20stuff/honours%20UWC/Quant/MetacommunityChapter/docs/Logue_et_al_2011.pdf) |
| **Worked Example Data** | Villeger et al. (2012) metacommunity dataset | [💾 `Villeger2012e_AJ.xlsx`](file:///C:/Users/27848/Desktop/university%20stuff/honours%20UWC/Quant/MetacommunityChapter/data/BCB743/metacommunity/Villeger2012e_AJ.xlsx) |
| **Practice Task Data** | Palozzi et al. (2017) peatland plant community dataset | [💾 `Palozzi2017_AJ.xlsx`](file:///C:/Users/27848/Desktop/university%20stuff/honours%20UWC/Quant/MetacommunityChapter/data/BCB743/metacommunity/Palozzi2017_AJ.xlsx) |

</div>

</div>

<div class="callout callout-style-simple callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Tasks to Complete in This Chapter

</div>

</div>

<div class="callout-body-container callout-body">

Throughout this chapter you will work through examples using the **Villeger et al. (2012)** metacommunity dataset from the CESTES Database to learn the quantitative methods used to investigate community assembly.

At the end of the chapter, complete the **Metacommunity Practice Task**, which applies the same analytical workflow to the independent **Palozzi et al. (2017)** dataset from the CESTES Database.

- [📄 Metacommunity Practice Task](file:///C:/Users/27848/Desktop/university%20stuff/honours%20UWC/Quant/MetacommunityChapter/tasks/Metacommunity_Practice_Task.qmd)

</div>

</div>

<div id="introduction" class="section level1" number="1">

# <span class="header-section-number">1</span> Introduction

**Overview**

One of the central questions in community ecology is:

*Why do ecological communities differ in their composition, even when they appear to occupy similar environments?*

Across forests, rivers, grasslands and marine ecosystems, neighbouring communities often contain different combinations of species despite experiencing comparable environmental conditions. Conversely, communities separated by large distances may exhibit striking similarities. Explaining these patterns has become one of the fundamental goals of modern ecology because the processes that determine which species establish, persist and coexist also shape biodiversity, ecosystem functioning and responses to environmental change (Leibold et al., 2004; Chase et al., 2020).

Traditional community ecology has largely explained these patterns through local ecological processes such as environmental filtering, competition and predation. While these processes remain fundamental, they cannot fully explain biodiversity patterns across landscapes because communities are rarely isolated. Species continually disperse among habitats, local extinctions are followed by recolonisation, and landscape connectivity influences which species are able to reach suitable environments (Holyoak, Leibold & Holt, 2005; Loreau, Mouquet & Holt, 2003).

Metacommunity ecology was developed to address this challenge by viewing local communities as components of larger, interconnected systems. Rather than asking only why species occur together within a single community, metacommunity ecology asks how interactions between local ecological processes and regional spatial processes shape biodiversity across multiple communities simultaneously (Leibold et al., 2004).

Throughout this chapter we explore how quantitative ecological methods can be used to distinguish the processes responsible for community assembly across landscapes, providing the conceptual framework that links many of the multivariate techniques introduced throughout BCB743.

<div id="ecological-background" class="section level2" number="1.1">

## <span class="header-section-number">1.1</span> Ecological Background

**Central Ecological Question**

*Why do different communities contain different combinations of species?*

Every forest, estuary, rocky shore or grassland contains a unique combination of species. Some communities are remarkably similar despite being separated by large distances, whereas others differ substantially even when they experience comparable environmental conditions. Understanding why these differences occur is one of the central objectives of community ecology because the processes that determine which species establish and persist also shape biodiversity, ecosystem functioning and responses to environmental change (Leibold et al., 2004; Shipley, Paine & Baraloto, 2012).

Early ecological theory largely sought explanations within individual communities. Modern ecology, however, recognises that community composition emerges from interactions occurring across multiple spatial scales, requiring both local ecological processes and regional spatial processes to be considered simultaneously (Vellend, 2010; Chase et al., 2020).

Throughout this section we introduce the ecological concepts that underpin metacommunity ecology before examining how quantitative methods can be used to investigate these processes.

<div class="callout callout-style-simple callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Main Idea

</div>

</div>

<div class="callout-body-container callout-body">

Community composition is not determined by a single ecological process. Instead, it emerges from interactions among environmental conditions, species interactions, dispersal and stochastic events operating across multiple spatial scales.

</div>

</div>

</div>

<div id="community-assembly" class="section level2" number="1.2">

## <span class="header-section-number">1.2</span> Community Assembly

Community assembly describes the processes through which species colonise habitats, interact with one another and either persist or disappear through time. Every ecological community therefore represents the outcome of numerous ecological processes acting simultaneously rather than a single deterministic mechanism (Shipley et al., 2012).

Rather than asking **which theory is correct**, modern ecology asks:

*Which ecological processes best explain the observed pattern?*

This shift from competing theories to competing processes forms the foundation of metacommunity ecology and underpins the quantitative methods introduced later in this chapter.

<div id="tbl-local-regional" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-local-regional-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 22%" />
<col style="width: 42%" />
<col style="width: 35%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Process</strong></th>
<th style="text-align: left;"><strong>Ecological question</strong></th>
<th style="text-align: left;"><strong>Typical outcome</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Environmental selection</td>
<td style="text-align: left;">Which species are favoured by local environmental conditions?</td>
<td style="text-align: left;">Species occur where conditions suit them.</td>
</tr>
<tr class="even">
<td style="text-align: left;">Dispersal</td>
<td style="text-align: left;">Can species reach suitable habitats?</td>
<td style="text-align: left;">Communities become connected through immigration and colonisation.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ecological drift</td>
<td style="text-align: left;">How important are random demographic events?</td>
<td style="text-align: left;">Community composition changes through chance.</td>
</tr>
<tr class="even">
<td style="text-align: left;">Species interactions</td>
<td style="text-align: left;">How do organisms influence one another?</td>
<td style="text-align: left;">Competition, predation and facilitation alter community composition.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 1: <strong>Major processes influencing community assembly.</strong></figcaption>
</figure>

</div>

After introducing these processes, students should recognise that no single mechanism completely explains biodiversity patterns. The challenge is therefore to determine which processes dominate within a particular ecological system. Modern metacommunity theory increasingly interprets these processes as interacting mechanisms that jointly determine species coexistence across landscapes (Shoemaker & Melbourne, 2016).

</div>

<div id="local-and-regional-processes" class="section level2" number="1.3">

## <span class="header-section-number">1.3</span> Local and Regional Processes

One of the most important developments in community ecology has been recognising that communities are influenced by processes operating at different spatial scales.

Local processes determine whether species can establish and persist within individual habitats, whereas regional processes determine which species are able to reach those habitats in the first place (Leibold et al., 2004; Holyoak, Leibold & Holt, 2005).

<div id="tbl-local-regional" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-local-regional-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 19%" />
<col style="width: 28%" />
<col style="width: 22%" />
<col style="width: 28%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Local processes</strong></th>
<th style="text-align: left;"><strong>Role in community assembly</strong></th>
<th style="text-align: left;"><strong>Regional processes</strong></th>
<th style="text-align: left;"><strong>Role in community assembly</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>Environmental filtering</strong></td>
<td style="text-align: left;">Determines which species can establish under local environmental conditions.</td>
<td style="text-align: left;"><strong>Dispersal</strong></td>
<td style="text-align: left;">Determines which species are able to reach suitable habitats.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Competition</strong></td>
<td style="text-align: left;">Influences species coexistence through interactions for limited resources.</td>
<td style="text-align: left;"><strong>Connectivity</strong></td>
<td style="text-align: left;">Facilitates movement of organisms among habitat patches.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Predation</strong></td>
<td style="text-align: left;">Alters species abundance and community structure through trophic interactions.</td>
<td style="text-align: left;"><strong>Colonisation</strong></td>
<td style="text-align: left;">Introduces new species into local communities following dispersal.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Facilitation</strong></td>
<td style="text-align: left;">Positive interactions increase establishment or persistence of other species.</td>
<td style="text-align: left;"><strong>Regional species pool</strong></td>
<td style="text-align: left;">Represents the set of species available to colonise local communities.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Demographic processes</strong></td>
<td style="text-align: left;">Birth, death and reproduction influence local population persistence.</td>
<td style="text-align: left;"><strong>Landscape structure</strong></td>
<td style="text-align: left;">Determines how easily species move among habitats by influencing habitat configuration and isolation.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 2: <strong>Comparison of local and regional ecological processes that influence community assembly.</strong></figcaption>
</figure>

</div>

Neither set of processes operates independently. Instead, community composition reflects the interaction between local ecological conditions and regional spatial dynamics.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

A[&quot;Regional Species Pool&quot;]

subgraph Regional Processes
B[&quot;Dispersal&quot;]
end

subgraph Local Community
C[&quot;Environmental Filtering&quot;]
D[&quot;Species Interactions&quot;]
end

E[&quot;Community Composition&quot;]

A --&gt; B
B --&gt; C
B --&gt; D
C --&gt; E
D --&gt; E</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 1.1.** Community assembly results from interactions between regional and local ecological processes. The regional species pool supplies potential colonists through dispersal, while local environmental filtering and species interactions determine which species establish and persist within communities. The resulting community composition forms the basis for understanding metacommunity structure.

</div>

</div>

<div id="from-communities-to-metacommunities" class="section level1" number="2">

# <span class="header-section-number">2</span> From Communities to Metacommunities

**Central Ecological Question**

*If communities are connected by the movement of organisms, should they still be studied as isolated systems?*

Traditional community ecology has provided important insights into how environmental conditions and species interactions influence local communities. However, many ecological patterns cannot be explained by local processes alone. Species disperse among habitats, populations become connected across landscapes, and local extinctions may be followed by recolonisation from neighbouring communities. As a result, biodiversity often reflects interactions occurring beyond the boundaries of individual communities (Holyoak, Leibold & Holt, 2005; Loreau, Mouquet & Holt, 2003).

Recognising these limitations led to the development of metacommunity ecology, which extends community ecology by explicitly considering the spatial connections among communities.

<div id="what-is-a-metacommunity" class="section level2" number="2.1">

## <span class="header-section-number">2.1</span> What is a Metacommunity?

Leibold et al. (2004) define a metacommunity as:

*“A set of local communities that are linked by the dispersal of multiple potentially interacting species.”*

This definition highlights two important ideas.

Communities are **not isolated**. Species movement links communities across landscapes.

Rather than studying individual communities independently, metacommunity ecology views biodiversity as the outcome of ecological processes operating simultaneously at **local and regional** spatial scales (Leibold et al., 2004; Chase et al., 2020).

<div class="callout callout-style-simple callout-important">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Key Definition**

A metacommunity is a collection of local communities connected by the dispersal of multiple interacting species.

Understanding biodiversity therefore requires considering both within-community processes and among-community connections.

</div>

</div>

</div>

</div>

<div id="components-of-a-metacommunity" class="section level2" number="2.2">

## <span class="header-section-number">2.2</span> Components of a Metacommunity

Every metacommunity consists of four fundamental components.

<div id="tbl-components" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-components-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 34%" />
<col style="width: 65%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Component</strong></th>
<th style="text-align: left;"><strong>Role in a metacommunity</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>Regional species pool</strong></td>
<td style="text-align: left;">The complete set of species capable of colonising local communities within a landscape.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Habitat patches</strong></td>
<td style="text-align: left;">Individual locations occupied by local communities that differ in environmental conditions or spatial position.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Dispersal</strong></td>
<td style="text-align: left;">Movement of organisms among habitat patches, linking otherwise separate communities.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Local communities</strong></td>
<td style="text-align: left;">Assemblages of interacting species occupying individual habitat patches.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 3: <strong>Fundamental components of a metacommunity.</strong></figcaption>
</figure>

</div>

Together, these components determine how species move through landscapes and how biodiversity patterns emerge across multiple communities.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

A[&quot;Regional Species Pool&quot;]

B[&quot;Habitat Patches&quot;]

C[&quot;Dispersal&quot;]

D[&quot;Local Community 1&quot;]
E[&quot;Local Community 2&quot;]
F[&quot;Local Community 3&quot;]

G[&quot;Metacommunity&quot;]

A --&gt; C
C --&gt; B

B --&gt; D
B --&gt; E
B --&gt; F

D --&gt; G
E --&gt; G
F --&gt; G</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 2.1.** Conceptual representation of a metacommunity. Species originating from a regional species pool disperse among habitat patches, creating a network of connected local communities. Together these communities form a metacommunity, where biodiversity is influenced by both local ecological processes and regional spatial dynamics.

</div>

<div id="why-was-the-metacommunity-concept-developed" class="section level2" number="2.3">

## <span class="header-section-number">2.3</span> Why was the Metacommunity Concept Developed?

Community ecology traditionally focused on ecological interactions occurring within individual habitats, such as competition, predation and environmental filtering. While these processes remain fundamental, they cannot fully explain why similar habitats often support different communities or why distant communities may exhibit similar species composition.

Metacommunity ecology addresses this limitation by recognising that dispersal, habitat connectivity and regional landscape structure influence which species are able to colonise local habitats before local ecological processes determine whether those species persist (Leibold et al., 2004; Holyoak, Leibold & Holt, 2005).

This broader perspective allows ecologists to investigate biodiversity patterns across landscapes rather than treating communities as isolated ecological units.

<div id="tbl-community-comparison" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-community-comparison-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 56%" />
<col style="width: 43%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Traditional community ecology</strong></th>
<th style="text-align: left;"><strong>Metacommunity ecology</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Focuses on individual local communities.</td>
<td style="text-align: left;">Focuses on networks of interconnected local communities.</td>
</tr>
<tr class="even">
<td style="text-align: left;">Emphasises ecological processes operating within a single habitat.</td>
<td style="text-align: left;">Integrates ecological processes operating across both local and regional spatial scales.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Community composition is explained primarily by local environmental conditions and species interactions.</td>
<td style="text-align: left;">Community composition reflects interactions among environmental filtering, dispersal, ecological drift and connectivity.</td>
</tr>
<tr class="even">
<td style="text-align: left;">Communities are often considered independently of one another.</td>
<td style="text-align: left;">Communities are linked through the dispersal of multiple interacting species.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Explains biodiversity within individual communities (α-diversity).</td>
<td style="text-align: left;">Explains patterns of biodiversity among communities (β-diversity) and across landscapes.</td>
</tr>
<tr class="even">
<td style="text-align: left;">Primarily addresses local community assembly.</td>
<td style="text-align: left;">Investigates how local and regional processes interact to shape metacommunity structure.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 4: <strong>Comparison between traditional community ecology and metacommunity ecology.</strong></figcaption>
</figure>

</div>

</div>

<div id="why-does-this-matter" class="section level2" number="2.4">

## <span class="header-section-number">2.4</span> Why Does This Matter?

Viewing communities as components of larger spatial networks has transformed modern ecology.

A metacommunity perspective allows ecologists to:

- explain patterns of biodiversity across landscapes;

- investigate how dispersal influences community assembly;

- evaluate the effects of habitat fragmentation and connectivity;

- predict ecological responses to climate change;

- inform biodiversity conservation and ecosystem management (Chase et al., 2020; Correa Ayram et al., 2016; Rudnick et al., 2012; Grimm et al., 2013).

Perhaps most importantly for BCB743, metacommunity ecology provides the ecological framework needed to interpret many of the quantitative methods introduced throughout this course. Rather than simply describing patterns in species composition, these methods can be used to investigate the ecological processes responsible for generating those patterns.

<div class="callout callout-style-simple callout-tip">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Main Idea:**

Metacommunity ecology extends traditional community ecology by recognising that local communities are connected through dispersal. Biodiversity therefore emerges from interactions between local ecological processes and regional spatial processes, making metacommunities the natural framework for studying community assembly across landscapes.

</div>

</div>

</div>

**Looking Ahead**

Understanding what a metacommunity is naturally leads to a second question:

*Which ecological processes generate the patterns observed within metacommunities?*

The next section introduces the modern process-based framework for answering this question by examining the roles of selection, dispersal, ecological drift and speciation in community assembly.

</div>

</div>

<div id="ecological-processes-shaping-metacommunities" class="section level1" number="3">

# <span class="header-section-number">3</span> Ecological Processes Shaping Metacommunities

**Central Ecological Question**

*Which ecological processes determine why communities differ in species composition across landscapes?*

Although metacommunities are defined by the dispersal of organisms among connected communities, dispersal alone does not determine community composition. Ecological communities are assembled through the interaction of multiple processes operating simultaneously across local and regional spatial scales. Some processes determine whether species can survive within a habitat, whereas others influence whether species are able to reach that habitat in the first place. Together, these processes generate the patterns of biodiversity observed across landscapes (Leibold et al., 2004; Holyoak, Leibold & Holt, 2005).

Rather than searching for a single explanation, modern ecology recognises that community assembly results from the combined influence of several interacting ecological processes. Consequently, ecologists ask not which theory is correct, but rather:

*Which ecological processes best explain the observed community pattern?*

This process-based perspective forms the foundation of modern metacommunity ecology and provides the ecological framework for interpreting the quantitative analyses introduced later in this chapter.

<div id="a-process-based-view-of-community-assembly" class="section level2" number="3.1">

## <span class="header-section-number">3.1</span> A Process-Based View of Community Assembly

Early ecological research often attempted to explain biodiversity patterns using individual theories, such as niche theory or neutral theory. While these theories remain important, contemporary ecology has shifted towards understanding the fundamental processes responsible for community assembly (Vellend, 2010).

Vellend (2010) proposed that most patterns of biodiversity can be understood through four fundamental ecological processes:

- Selection

- Dispersal

- Ecological drift

- Speciation

Rather than competing with one another, these processes interact continuously to determine which species occur within communities, how abundant they become and how biodiversity changes across space and time (Vellend, 2010; Logue et al., 2011; Chase et al., 2020).

Understanding these processes provides a common language for interpreting community assembly and forms the conceptual basis for the metacommunity framework.

<div class="callout callout-style-simple callout-tip">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Main Idea**

Modern metacommunity ecology is organised around ecological processes, not competing theories. Community composition emerges from interactions among selection, dispersal, ecological drift and speciation, all operating across multiple spatial scales.

</div>

</div>

</div>

<div id="tbl-four-processes" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-four-processes-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:98%;">
<colgroup>
<col style="width: 26%" />
<col style="width: 37%" />
<col style="width: 35%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Ecological process</strong></th>
<th style="text-align: left;"><strong>Central ecological question</strong></th>
<th style="text-align: left;"><strong>Role in community assembly</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>Selection</strong></td>
<td style="text-align: left;">Which species are favoured under local environmental conditions?</td>
<td style="text-align: left;">Determines which species establish and persist through environmental filtering and biotic interactions.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Dispersal</strong></td>
<td style="text-align: left;">Which species are able to reach suitable habitats?</td>
<td style="text-align: left;">Connects local communities through immigration, colonisation and landscape connectivity.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Ecological drift</strong></td>
<td style="text-align: left;">What role does chance play in community assembly?</td>
<td style="text-align: left;">Random demographic events alter species abundances and community composition independently of environmental conditions.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Speciation</strong></td>
<td style="text-align: left;">How are new species added to the regional species pool?</td>
<td style="text-align: left;">Generates new biodiversity over evolutionary timescales, expanding the pool of species available to colonise communities.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 5: <strong>The four fundamental ecological processes that shape community assembly and metacommunity structure.</strong></figcaption>
</figure>

</div>

As shown, each process addresses a different ecological question. In natural systems these processes rarely operate independently; instead, they interact to determine patterns of biodiversity across local communities and entire landscapes.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

A[&quot;Regional Species Pool&quot;]

B[&quot;Selection&quot;]

C[&quot;Dispersal&quot;]

D[&quot;Ecological Drift&quot;]

E[&quot;Speciation&quot;]

F[&quot;Community Assembly&quot;]

G[&quot;Metacommunity Structure&quot;]

A --&gt; F
B --&gt; F
C --&gt; F
D --&gt; F
E --&gt; F

F --&gt; G</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 3.1.** Modern community assembly is governed by four interacting ecological processes. Selection, dispersal, ecological drift and speciation influence community assembly simultaneously, producing the biodiversity patterns observed within metacommunities. Adapted from the process-based synthesis proposed by Vellend (2010).

Although these four processes provide the modern foundation for understanding community assembly, they influence biodiversity in different ways. Some processes act primarily within local communities, whereas others operate across larger spatial and evolutionary scales. Understanding each process individually allows ecologists to determine how their interactions generate metacommunity structure and biodiversity patterns.

**In the following sections, we examine each of these four ecological processes in turn, beginning with selection, the process through which environmental conditions and species interactions determine which species are able to persist within local communities.**

</div>

<div id="selection-who-can-survive" class="section level2" number="3.2">

## <span class="header-section-number">3.2</span> Selection: Who Can Survive?

Selection is the process through which local environmental conditions and species interactions determine which species are able to establish and persist within a community. Species differ in their environmental tolerances and ecological requirements, meaning that habitats “select” species whose traits are best suited to local conditions (Vellend, 2010; Shipley, Paine & Baraloto, 2012).

Selection is often referred to as **environmental filtering** because the environment filters species according to their ecological characteristics. Factors such as temperature, rainfall, salinity, nutrient availability and habitat complexity determine which species are capable of surviving in a particular location. At the same time, interactions among species, including competition, predation and facilitation, further influence community composition.

For example, two wetlands may be connected by dispersal, yet differ in nutrient availability or hydroperiod. Although many species are able to reach both wetlands, only those adapted to the local environmental conditions are likely to establish successfully.

<div class="callout callout-style-simple callout-note">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

Selection in Practice

Selection answers one of the most important questions in community ecology:

*Which species are able to survive under the environmental conditions present at a particular site?*

Species that are poorly adapted are filtered out, while well-adapted species become established and persist.

</div>

</div>

</div>

</div>

<div id="dispersal-who-can-arrive" class="section level2" number="3.3">

## <span class="header-section-number">3.3</span> Dispersal: Who Can Arrive?

Even when environmental conditions are suitable, species cannot become part of a community unless they first arrive there. **Dispersal** is the movement of organisms among habitat patches and provides the spatial link that distinguishes metacommunity ecology from traditional community ecology (Leibold et al., 2004; Holyoak, Leibold & Holt, 2005).

Dispersal allows species to colonise new habitats, recolonise disturbed areas and maintain populations through immigration. Consequently, the composition of local communities depends not only on local environmental conditions but also on the movement of individuals across the landscape.

The effectiveness of dispersal is influenced by **landscape connectivity**. Highly connected landscapes facilitate movement among communities, whereas fragmented landscapes reduce connectivity and may prevent species from reaching otherwise suitable habitats.

When dispersal is limited, species may be absent from habitats where they could otherwise survive. Conversely, high dispersal rates can allow species to persist temporarily in less suitable environments through continual immigration from neighbouring communities, a phenomenon known as **source-sink dynamics** (Leibold et al., 2004).

<div class="callout callout-style-simple callout-note">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

Dispersal in Practice

Dispersal answers a different ecological question:

*Can species reach suitable habitats?*

Community composition therefore depends not only on environmental suitability, but also on the movement of organisms among connected habitats.

</div>

</div>

</div>

<div id="tbl-selection-dispersal" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-selection-dispersal-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table">
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Selection</strong></th>
<th style="text-align: left;"><strong>Dispersal</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Determines <strong>which species can survive</strong> under local environmental conditions.</td>
<td style="text-align: left;">Determines <strong>which species can reach</strong> suitable habitats.</td>
</tr>
<tr class="even">
<td style="text-align: left;">Driven by environmental filtering and species interactions.</td>
<td style="text-align: left;">Driven by movement among habitat patches and landscape connectivity.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Operates primarily at the local scale.</td>
<td style="text-align: left;">Operates across local and regional spatial scales.</td>
</tr>
<tr class="even">
<td style="text-align: left;">Produces communities adapted to local conditions.</td>
<td style="text-align: left;">Links communities through immigration and colonisation.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Investigated using relationships between community composition and environmental variables.</td>
<td style="text-align: left;">Investigated using spatial analyses, connectivity measures and distance-based approaches.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 6: <strong>Selection and dispersal represent complementary ecological processes within metacommunities.</strong></figcaption>
</figure>

</div>

As shown in <a href="#tbl-selection-dispersal" class="quarto-xref">Table 6</a>, selection and dispersal answer different ecological questions but operate simultaneously during community assembly. Dispersal determines which species are able to reach suitable habitats, whereas selection determines which of those species establish and persist under local environmental conditions. Together, these complementary processes underpin the spatial organisation of metacommunities.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart LR

A[&quot;Regional Species Pool&quot;]

B[&quot;Dispersal&lt;br/&gt;(Who can arrive?)&quot;]

C[&quot;Local Community&quot;]

D[&quot;Selection&lt;br/&gt;(Who can survive?)&quot;]

E[&quot;Community Composition&quot;]

A --&gt; B
B --&gt; C
C --&gt; D
D --&gt; E

style B fill:#D6EAF8,stroke:#2E86C1,stroke-width:2px
style D fill:#D5F5E3,stroke:#239B56,stroke-width:2px
style E fill:#FCF3CF,stroke:#B7950B,stroke-width:2px</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 3.2.** Selection and dispersal operate sequentially during community assembly. Dispersal determines which species are able to reach local communities, while selection determines which of those species are able to establish and persist under local environmental conditions. Together these processes generate patterns of biodiversity across metacommunities.

<div class="callout callout-style-simple callout-tip">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Main Idea**

Selection and dispersal are complementary processes that together shape community assembly.

- Dispersal determines who can arrive.

- Selection determines who can survive.

Understanding the interaction between these processes is central to explaining patterns of biodiversity within metacommunities.

</div>

</div>

</div>

Although selection and dispersal explain much of the variation observed among ecological communities, community assembly is not entirely deterministic. Random demographic events can also influence which species persist within communities, particularly when populations are small or disturbances are frequent. These stochastic influences are collectively described as ecological drift, the next fundamental process shaping metacommunity structure (Vellend, 2010).

</div>

<div id="ecological-drift-what-role-does-chance-play" class="section level2" number="3.4">

## <span class="header-section-number">3.4</span> Ecological Drift: What Role Does Chance Play?

Not all differences among ecological communities arise because of environmental conditions or dispersal. Even when habitats are environmentally similar and equally connected, communities may still differ simply because of **random demographic events**. This process is known as **ecological drift** (Vellend, 2010).

Ecological drift refers to stochastic changes in community composition caused by chance events such as random births, deaths, colonisations and local extinctions. Unlike selection, which favours species best adapted to local conditions, drift occurs independently of species’ ecological characteristics.

Drift is most influential when populations are small, disturbances are frequent or environmental differences among habitats are weak. Under these conditions, chance events can substantially alter species abundances and community composition, even among otherwise similar communities (Vellend, 2010; Chase et al., 2020).

Although ecological drift is inherently unpredictable for individual communities, its effects can often be detected statistically by comparing patterns across many communities. Consequently, recognising the role of stochasticity has become an important component of modern metacommunity ecology.

<div class="callout callout-style-simple callout-note">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Ecological Drift in Practice**

Ecological drift addresses the question:

*How much of community assembly is determined by chance rather than deterministic ecological processes?*

Drift becomes increasingly important when populations are small, disturbances are frequent or environmental selection is weak.

</div>

</div>

</div>

</div>

<div id="speciation-where-does-biodiversity-come-from" class="section level2" number="3.5">

## <span class="header-section-number">3.5</span> Speciation: Where Does Biodiversity Come From?

Selection, dispersal and ecological drift determine how existing species are distributed among communities. **Speciation**, however, introduces entirely new species into the regional species pool over evolutionary timescales (Vellend, 2010).

Speciation is the evolutionary process through which new species arise. Although it typically operates over much longer timescales than ecological processes, it provides the ultimate source of biodiversity upon which community assembly depends.

Within metacommunity ecology, speciation is generally viewed as a background process that continually replenishes the regional species pool. Newly evolved species may subsequently disperse, establish within suitable habitats and interact with existing communities, thereby contributing to long-term changes in biodiversity (Leibold et al., 2004; Vellend, 2010).

Because most ecological studies examine relatively short time periods, speciation is often inferred rather than measured directly. Nevertheless, without speciation there would be no continual generation of biodiversity to sustain ecological communities over evolutionary time.

<div class="callout callout-style-simple callout-note">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Speciation in Practice**

Speciation answers a different ecological question:

*How are new species added to ecological systems over time?*

Although it operates on evolutionary timescales, speciation provides the biodiversity upon which all ecological communities are assembled.

</div>

</div>

</div>

<div id="tbl-process-comparison" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-process-comparison-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 34%" />
<col style="width: 23%" />
<col style="width: 42%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Ecological process</strong></th>
<th style="text-align: left;"><strong>Acts on…</strong></th>
<th style="text-align: left;"><strong>Primary ecological role</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>Selection</strong></td>
<td style="text-align: left;">Local communities</td>
<td style="text-align: left;">Determines which species establish and persist under local environmental conditions.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Dispersal</strong></td>
<td style="text-align: left;">Communities across landscapes</td>
<td style="text-align: left;">Connects communities through the movement of organisms.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Ecological drift</strong></td>
<td style="text-align: left;">Local populations</td>
<td style="text-align: left;">Alters community composition through random demographic events.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Speciation</strong></td>
<td style="text-align: left;">Regional species pool</td>
<td style="text-align: left;">Generates new biodiversity over evolutionary timescales.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 7: <strong>Comparison of the four fundamental ecological processes shaping community assembly.</strong></figcaption>
</figure>

</div>

As summarised in <a href="#tbl-process-comparison" class="quarto-xref">Table 7</a>, the four ecological processes operate at different spatial and temporal scales but collectively determine how biodiversity is generated, distributed and maintained across metacommunities.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

A[&quot;Regional Species Pool&quot;]

B[&quot;Speciation&quot;]

C[&quot;Dispersal&quot;]

D[&quot;Local Community&quot;]

E[&quot;Selection&quot;]

F[&quot;Ecological Drift&quot;]

G[&quot;Community Composition&quot;]

B --&gt; A
A --&gt; C
C --&gt; D
D --&gt; E
D --&gt; F
E --&gt; G
F --&gt; G</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 3.3.** The four ecological processes operate across different spatial and temporal scales. Speciation contributes new species to the regional species pool, dispersal moves species among communities, while selection and ecological drift determine community composition within local habitats.

<div class="callout callout-style-simple callout-tip">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Main Idea**

Community assembly results from the interaction of four fundamental ecological processes.

- **Selection** determines which species can survive.

- **Dispersal** determines which species can arrive.

- **Ecological drift** introduces random variation in community composition.

- **Speciation** generates the biodiversity upon which ecological communities are assembled.

Together, these interacting processes provide the modern framework for understanding metacommunity dynamics (Vellend, 2010).

</div>

</div>

</div>

The four processes described above provide a modern, process-based explanation for community assembly. Historically, however, metacommunity ecology was developed through a series of conceptual models that emphasised different combinations of these processes. Understanding these metacommunity paradigms provides valuable historical context and helps explain how the field has evolved into the process-based framework used today (Leibold et al., 2004; Logue et al., 2011).

</div>

<div id="where-do-the-four-metacommunity-paradigms-fit" class="section level2" number="3.6">

## <span class="header-section-number">3.6</span> Where Do the Four Metacommunity Paradigms Fit?

**Central Ecological Question**

*If community assembly is governed by four interacting ecological processes, why were four metacommunity paradigms originally developed?*

The four ecological processes introduced in the previous sections provide a modern framework for understanding community assembly. Before this process-based synthesis became widely accepted, however, ecologists developed a series of conceptual models to explain how local and regional processes interact to shape biodiversity across landscapes.

These models, known as the **four metacommunity paradigms**, were formalised by Leibold et al. (2004). Each paradigm emphasises a different combination of ecological processes and generates distinct predictions about how communities should respond to environmental variation, dispersal and spatial structure.

Today, these paradigms are best viewed as **conceptual tools** rather than competing theories. They remain valuable because they provide simple models for understanding how different ecological processes influence metacommunity structure, even though empirical studies increasingly show that most natural systems exhibit characteristics of more than one paradigm (Logue et al., 2011; Winegardner et al., 2012).

<div class="callout callout-style-simple callout-important">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Historical Perspective**

The four metacommunity paradigms were developed to simplify the complexity of community assembly.

Modern ecology recognises that natural metacommunities rarely fit neatly into a single paradigm. Instead, they usually reflect interactions among **selection, dispersal, ecological drift and speciation** operating simultaneously.

</div>

</div>

</div>

**The Four Metacommunity Paradigms:**

Rather than memorising four independent theories, students should think of each paradigm as highlighting a different ecological mechanism.

**Species Sorting:**

Species Sorting emphasises **selection**.

Communities differ because environmental conditions vary among habitats, and species occupy locations where their ecological requirements are best met. Dispersal is assumed to be sufficient for species to reach suitable habitats but not so extensive that it overrides environmental filtering (Leibold et al., 2004).

**Mass Effects:**

Mass Effects emphasises the interaction between **selection and high dispersal**.

Although environmental conditions remain important, continual immigration allows some species to persist in habitats where they would otherwise be unable to survive. Community composition therefore reflects both environmental filtering and ongoing dispersal among neighbouring communities (Leibold et al., 2004).

**Patch Dynamics:**

Patch Dynamics focuses primarily on **dispersal and colonisation–extinction** dynamics.

Habitats are assumed to be broadly similar, and differences among communities arise because species continually colonise newly available patches while becoming locally extinct elsewhere. Trade-offs between competitive ability and dispersal determine long-term coexistence.

**Neutral Dynamics:**

Neutral Dynamics places greatest emphasis on **ecological drift**.

Species are assumed to have similar ecological characteristics, so differences among communities arise primarily through random dispersal, demographic stochasticity and chance colonisation events rather than deterministic environmental filtering (Hubbell, 2001; Leibold et al., 2004).

<div id="tbl-paradigms" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-paradigms-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:97%;">
<colgroup>
<col style="width: 11%" />
<col style="width: 31%" />
<col style="width: 26%" />
<col style="width: 28%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Paradigm</strong></th>
<th style="text-align: left;"><strong>Dominant ecological process(es)</strong></th>
<th style="text-align: left;"><strong>Key ecological prediction</strong></th>
<th style="text-align: left;"><strong>Typical analytical approaches</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>Species Sorting</strong></td>
<td style="text-align: left;">Selection</td>
<td style="text-align: left;">Community composition reflects environmental variation.</td>
<td style="text-align: left;">Ordination, constrained ordination, environmental modelling</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Mass Effects</strong></td>
<td style="text-align: left;">Selection + high dispersal</td>
<td style="text-align: left;">Immigration allows species to persist outside their optimal habitats.</td>
<td style="text-align: left;">Variation partitioning, spatial analyses</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Patch Dynamics</strong></td>
<td style="text-align: left;">Dispersal and colonisation</td>
<td style="text-align: left;">Colonisation–extinction dynamics maintain biodiversity.</td>
<td style="text-align: left;">Connectivity analyses, occupancy models</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Neutral Dynamics</strong></td>
<td style="text-align: left;">Ecological drift</td>
<td style="text-align: left;">Community differences arise through stochastic processes.</td>
<td style="text-align: left;">Neutral models, spatial autocorrelation, abundance distributions</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 8: <strong>The four metacommunity paradigms emphasise different combinations of ecological processes.</strong></figcaption>
</figure>

</div>

As summarised in <a href="#tbl-paradigms" class="quarto-xref">Table 8</a>, each paradigm highlights a different combination of ecological processes rather than representing an entirely separate explanation for community assembly. Together they provide a conceptual framework for interpreting biodiversity patterns across landscapes.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

A[&quot;Selection&quot;]
B[&quot;Dispersal&quot;]
C[&quot;Ecological Drift&quot;]

D[&quot;Species Sorting&quot;]
E[&quot;Mass Effects&quot;]
F[&quot;Patch Dynamics&quot;]
G[&quot;Neutral Dynamics&quot;]

A --&gt; D
A --&gt; E

B --&gt; E
B --&gt; F

C --&gt; G

style D fill:#D5F5E3
style E fill:#FCF3CF
style F fill:#D6EAF8
style G fill:#FADBD8</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 3.4.** The four metacommunity paradigms emphasise different combinations of the fundamental ecological processes. Species Sorting primarily reflects environmental selection, Mass Effects integrate selection and dispersal, Patch Dynamics focuses on dispersal and colonisation, and Neutral Dynamics emphasises ecological drift.

<div class="callout callout-style-simple callout-tip">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Main Idea**

The four metacommunity paradigms remain important because they provide simple conceptual models for understanding community assembly. However, modern metacommunity ecology is no longer centred on these paradigms. Instead, ecologists interpret biodiversity patterns by evaluating how **selection, dispersal, ecological drift and speciation** interact within natural systems.

Rather than asking which paradigm is correct, modern ecology asks:

*Which combination of ecological processes best explains the observed metacommunity?*

</div>

</div>

</div>

</div>

</div>

<div id="from-ecological-questions-to-quantitative-analysis" class="section level1" number="4">

# <span class="header-section-number">4</span> From Ecological Questions to Quantitative Analysis

**Central Ecological Question:**

*How can quantitative analyses be used to identify the ecological processes shaping metacommunities?*

The ecological processes responsible for community assembly cannot usually be observed directly. Instead, ecologists use quantitative analyses to determine whether patterns of community composition are best explained by environmental selection, dispersal, ecological drift or the evolutionary legacy of historical speciation. Quantitative methods therefore provide the evidence needed to link ecological theory with real ecological data.

In this chapter, we use the CESTES (Community Ecology: Species, Traits, Environment and Space) database, a global collection of metacommunity datasets that combines four core types of ecological information: species composition, functional traits, environmental variables and spatial coordinates. Together, these datasets provide the information needed to investigate the processes responsible for community assembly.

Rather than introducing statistical methods individually, the analyses in this chapter are organised around the four fundamental metacommunity processes. Each section begins with an ecological question, followed by quantitative analyses that test whether the available evidence supports the role of that process in structuring the metacommunity. This process-based approach demonstrates how quantitative methods are used to answer ecological questions and interpret patterns of biodiversity.

<div class="callout callout-style-simple callout-tip">

<div class="callout-body d-flex">

<div class="callout-icon-container">

</div>

<div class="callout-body-container">

**Key Idea**

Quantitative analyses are not performed to produce statistics—they are used to test ecological hypotheses about the processes responsible for community assembly.

</div>

</div>

</div>

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart LR

A[&quot;Metacommunity Dataset&lt;br/&gt;(Species, Traits, Environment &amp; Space)&quot;]
--&gt; B[&quot;Quantitative Analyses&quot;]

B
--&gt; C[&quot;Environmental Selection&quot;]

B
--&gt; D[&quot;Dispersal&quot;]

B
--&gt; E[&quot;Ecological Drift&quot;]

B
--&gt; F[&quot;Historical Speciation&quot;]

C --&gt; G[&quot;Interpret community assembly&quot;]
D --&gt; G
E --&gt; G
F --&gt; G</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 4.1.** The analytical framework used throughout this chapter. The CESTES database provides complementary information on species, traits, environmental conditions and spatial location. Quantitative analyses are then used to investigate the four fundamental processes of metacommunity ecology and determine how they contribute to community assembly.

<div id="quantifying-historical-speciation-and-functional-trait-diversity" class="section level2" number="4.1">

## <span class="header-section-number">4.1</span> Quantifying Historical Speciation and Functional Trait Diversity

**Ecological Question**

*How has historical evolutionary diversification shaped the functional diversity available for community assembly?*

Directly measuring speciation requires genetic, phylogenetic or fossil evidence that is rarely available in ecological community datasets. Instead, ecologists infer the long-term influence of historical speciation by examining the diversity of functional traits expressed by species within a regional species pool. Functional traits, such as body size, feeding morphology and locomotory characteristics, represent evolutionary adaptations that have accumulated through diversification over time. These traits provide the ecological strategies upon which contemporary metacommunity processes, including environmental filtering, dispersal and ecological drift, subsequently act.

In this worked example, we use the Villeger et al. (2012) a fish metacommunity dataset from the CESTES metacommunity database to investigate functional trait diversity as an indirect measure of the evolutionary legacy of historical speciation.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

    A[&quot;Historical Speciation&lt;br/&gt;(Evolutionary diversification)&quot;]

    B[&quot;Functional Trait Diversity&lt;br/&gt;(Morphology, life-history and ecological traits)&quot;]

    C[&quot;Regional Species Pool&quot;]

    D[&quot;Local Environmental Conditions&quot;]

    E[&quot;Environmental Filtering&quot;]

    F[&quot;Dispersal&quot;]

    G[&quot;Ecological Drift&quot;]

    H[&quot;Local Community Assembly&quot;]

    I[&quot;Observed Metacommunity Structure&quot;]

    A --&gt; B
    B --&gt; C

    C --&gt; H

    D --&gt; E
    E --&gt; H

    F --&gt; H
    G --&gt; H

    H --&gt; I</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 4.2.** Conceptual framework illustrating the role of historical speciation in metacommunity ecology. Speciation generates evolutionary diversification, producing a regional pool of species with diverse functional traits. This evolutionary legacy forms the raw material upon which ecological processes, environmental filtering, dispersal and ecological drift, act to determine local community assembly and the resulting metacommunity structure.

<div id="worked-example-loading-the-functional-trait-dataset" class="section level3" number="4.1.1">

### <span class="header-section-number">4.1.1</span> Worked Example: Loading the Functional Trait Dataset

The first step is to import the functional trait data describing each fish species. Unlike community abundance matrices, where rows represent sampling sites and columns represent species, the trait matrix contains one row per species and multiple columns describing morphological and ecological characteristics. These traits represent the evolutionary diversity available within the regional species pool and form the foundation for the analyses that follow.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# STEP 1
# Load the packages required for data import
############################################################

library(readxl)
library(dplyr)

############################################################
# STEP 2
# Import the Villeger et al. (2012) dataset
############################################################

villeger_data <- read_excel(
  "data/Villeger2012e_AJ.xlsx",
  sheet = "traits"
)

############################################################
# STEP 3
# Inspect the functional trait dataset
############################################################

head(villeger_data)

str(villeger_data)

summary(villeger_data)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    # A tibble: 6 × 17
      Sp     logM  OgSf  OgSh  OgPo  EySz  GrLg  GtLg  EyPo  BdSh  BdSf  PfPo  PfSh
      <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    1 sp2   2.19  0.072 0.947 1     0.151 0     1.78  1     0.143  2.17 0      0   
    2 sp3   2.52  0.285 1.63  0.317 0.49  0.309 0.647 0.556 2.08   1.82 0.822  2.15
    3 sp4   0.706 0.283 2.05  0.508 0.474 0.381 0.688 0.584 3.29   3.97 0.773  2.71
    4 sp6   3.33  0.056 0.648 0.273 0.294 0.032 3.47  0.647 3.10   1.60 0.638  6.87
    5 sp7   3.11  0.173 0.513 0.346 0.263 0.128 2.08  0.647 1.02   1.66 0.806  3.48
    6 sp8   2.17  0.248 0.519 0.489 0.357 0.142 2.15  0.613 1.02   2.09 0.662  3.67
    # ℹ 4 more variables: CpHt <dbl>, CfSh <dbl>, FsRt <dbl>, FsSf <dbl>

</div>

<div class="cell-output cell-output-stdout">

    tibble [46 × 17] (S3: tbl_df/tbl/data.frame)
     $ Sp  : chr [1:46] "sp2" "sp3" "sp4" "sp6" ...
     $ logM: num [1:46] 2.187 2.522 0.706 3.327 3.11 ...
     $ OgSf: num [1:46] 0.072 0.285 0.283 0.056 0.173 0.248 0.151 0.101 0.134 0.186 ...
     $ OgSh: num [1:46] 0.947 1.63 2.054 0.648 0.513 ...
     $ OgPo: num [1:46] 1 0.317 0.508 0.273 0.346 0.489 0.471 0.344 0.272 0.265 ...
     $ EySz: num [1:46] 0.151 0.49 0.474 0.294 0.263 0.357 0.437 0.409 0.368 0.493 ...
     $ GrLg: num [1:46] 0 0.309 0.381 0.032 0.128 0.142 0.208 0.162 0.185 0.631 ...
     $ GtLg: num [1:46] 1.782 0.647 0.688 3.467 2.078 ...
     $ EyPo: num [1:46] 1 0.556 0.584 0.647 0.647 0.613 0.638 0.668 0.638 0.541 ...
     $ BdSh: num [1:46] 0.143 2.083 3.292 3.102 1.021 ...
     $ BdSf: num [1:46] 2.17 1.82 3.97 1.6 1.66 ...
     $ PfPo: num [1:46] 0 0.822 0.773 0.638 0.806 0.662 0.617 0.582 0.777 0.84 ...
     $ PfSh: num [1:46] 0 2.15 2.71 6.87 3.48 ...
     $ CpHt: num [1:46] 1.12 2.29 2.49 2.91 3.59 ...
     $ CfSh: num [1:46] 0.767 2.775 3.108 2.684 4.052 ...
     $ FsRt: num [1:46] 0 0.449 0.504 1.06 0.781 0.625 0.558 0.737 0.655 0.439 ...
     $ FsSf: num [1:46] 1.16 1.81 2.62 1.5 1.65 ...

</div>

<div class="cell-output cell-output-stdout">

          Sp                 logM            OgSf              OgSh       
     Length:46          Min.   :0.706   Min.   :0.04000   Min.   :0.1560  
     Class :character   1st Qu.:2.238   1st Qu.:0.06525   1st Qu.:0.6613  
     Mode  :character   Median :2.761   Median :0.10950   Median :0.9335  
                        Mean   :2.988   Mean   :0.15337   Mean   :1.0565  
                        3rd Qu.:3.506   3rd Qu.:0.19175   3rd Qu.:1.2893  
                        Max.   :7.324   Max.   :0.58900   Max.   :2.7510  
          OgPo             EySz             GrLg             GtLg       
     Min.   :0.0000   Min.   :0.1510   Min.   :0.0000   Min.   :0.3330  
     1st Qu.:0.2667   1st Qu.:0.2777   1st Qu.:0.0155   1st Qu.:0.9103  
     Median :0.4190   Median :0.3615   Median :0.0595   Median :1.0685  
     Mean   :0.4133   Mean   :0.3589   Mean   :0.1122   Mean   :1.4264  
     3rd Qu.:0.5018   3rd Qu.:0.4457   3rd Qu.:0.1760   3rd Qu.:1.6565  
     Max.   :1.0000   Max.   :0.6280   Max.   :0.6310   Max.   :5.4240  
          EyPo             BdSh            BdSf            PfPo       
     Min.   :0.4180   Min.   :0.064   Min.   :1.277   Min.   :0.0000  
     1st Qu.:0.6140   1st Qu.:1.020   1st Qu.:1.642   1st Qu.:0.4575  
     Median :0.6735   Median :1.905   Median :1.927   Median :0.5915  
     Mean   :0.7150   Mean   :2.125   Mean   :2.019   Mean   :0.4989  
     3rd Qu.:0.7790   3rd Qu.:3.031   3rd Qu.:2.216   3rd Qu.:0.6495  
     Max.   :1.0000   Max.   :8.862   Max.   :3.974   Max.   :0.8400  
          PfSh            CpHt            CfSh             FsRt       
     Min.   :0.000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000  
     1st Qu.:2.418   1st Qu.:1.643   1st Qu.:0.9395   1st Qu.:0.4415  
     Median :3.482   Median :2.280   Median :1.2240   Median :0.7045  
     Mean   :3.274   Mean   :2.358   Mean   :1.6884   Mean   :1.0158  
     3rd Qu.:4.467   3rd Qu.:2.878   3rd Qu.:2.6793   3rd Qu.:0.9377  
     Max.   :6.868   Max.   :6.515   Max.   :4.0520   Max.   :8.3080  
          FsSf      
     Min.   :0.000  
     1st Qu.:1.407  
     Median :1.618  
     Mean   :1.960  
     3rd Qu.:1.820  
     Max.   :5.841  

</div>

</div>

**Interpretation:**

The Villeger et al. (2012) functional trait dataset contains measurements for 46 fish species described by 16 continuous functional traits, in addition to a species identifier. Unlike the community matrix used in previous analyses, where rows represented sampling sites and columns represented species abundances, each row in this dataset represents an individual species and each column describes a morphological or ecological characteristic of that species.

The traits capture multiple aspects of fish functional ecology, including body size (logM), oral gape morphology (OgSf, OgSh, OgPo), eye size and position (EySz, EyPo), gill raker length (GrLg), gut length (GtLg), body shape (BdSh), fin morphology (PfPo, PfSh, CfSh) and swimming characteristics (FsRt, FsSf). Together, these variables describe the functional strategies that species have evolved over evolutionary time.

The summary statistics demonstrate considerable variation across nearly all measured traits. For example, log-transformed body mass ranges from 0.706 to 7.324, gut length varies from 0.333 to 5.424, and body shape ranges from 0.064 to 8.862. This broad range of values suggests substantial functional differentiation among species within the regional species pool.

Importantly, these measurements do not quantify speciation directly. Rather, they represent the evolutionary outcomes of historical diversification. Functional trait diversity therefore provides an indirect measure of the evolutionary legacy of speciation and forms the basis for understanding how different ecological strategies have evolved before contemporary metacommunity processes act upon them.

Although the summary statistics demonstrate substantial variation among species, they do not reveal how traits covary or whether groups of traits describe similar ecological strategies. A useful first step is therefore to examine the relationships among functional traits using a correlation matrix. This allows us to identify suites of traits that may have evolved together and provides an initial overview of the functional diversity present within the regional species pool.

</div>

<div id="exploring-relationships-among-functional-traits" class="section level3" number="4.1.2">

### <span class="header-section-number">4.1.2</span> Exploring Relationships Among Functional Traits

**Ecological Question**

*Do functional traits vary independently, or do some traits covary because they reflect similar ecological strategies that have evolved through historical diversification?*

Functional traits rarely evolve in isolation. Instead, multiple traits often change together in response to similar ecological pressures, producing characteristic combinations of morphology and life-history strategies. Examining the relationships among traits provides insight into the functional organisation of the regional species pool and helps identify groups of traits that may have evolved together over evolutionary time.

To investigate these relationships, we calculate the Pearson correlation coefficients among all functional traits and visualise them as a correlation heatmap. Positive correlations indicate traits that tend to increase together across species, whereas negative correlations indicate trade-offs between different ecological strategies.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# STEP 1
# Load packages required for correlation analysis
############################################################

library(corrplot)

############################################################
# STEP 2
# Remove the species identifier and retain only
# the numeric functional traits
############################################################

trait_matrix <- villeger_data[, -1]

############################################################
# STEP 3
# Calculate the Pearson correlation matrix
############################################################

trait_cor <- cor(
  trait_matrix,
  method = "pearson"
)

############################################################
# STEP 4
# Visualise the correlation matrix
############################################################

corrplot(
  trait_cor,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.8,
  addCoef.col = "black",
  number.cex = 0.55,
  diag = FALSE,
  mar = c(0, 0, 2, 0),
  title = "Correlation among Functional Traits"
)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.3.** Correlation matrix showing Pearson correlation coefficients among the sixteen functional traits measured for the 46 fish species in the Villeger et al. (2012) dataset. Blue cells indicate positive correlations, red cells indicate negative correlations, and colour intensity reflects the strength of the relationship. Strong correlations among traits suggest that multiple morphological characteristics have evolved together as integrated functional strategies, providing evidence of the evolutionary diversification that underpins contemporary metacommunity assembly. The correlation matrix serves as an initial exploration of functional trait relationships before visualising multidimensional trait space using Principal Component Analysis (PCA).

**Interpretation:**

The correlation matrix provides an overview of how functional traits are related across the regional species pool. Strong positive correlations suggest that certain traits frequently occur together, indicating coordinated morphological adaptations associated with similar ecological strategies. Conversely, weak or negative correlations indicate that traits vary independently or represent alternative functional strategies.

These patterns reflect the cumulative effects of evolutionary diversification rather than contemporary ecological processes. Over long evolutionary timescales, natural selection acting within diverging lineages has produced species with distinct combinations of functional traits. The resulting variation in trait combinations represents the functional diversity generated by historical speciation and provides the foundation upon which metacommunity processes such as environmental filtering, dispersal and ecological drift subsequently act.

While the correlation matrix reveals relationships among individual traits, it does not provide an overall picture of how species are distributed within multidimensional functional trait space. The next step is therefore to use Principal Component Analysis (PCA) to reduce the dimensionality of the trait dataset and visualise the functional diversity of the regional species pool. PCA allows species with similar ecological strategies to be identified while illustrating the breadth of evolutionary diversification represented within the metacommunity.

</div>

<div id="visualising-functional-trait-diversity-using-principal-component-analysis-pca" class="section level3" number="4.1.3">

### <span class="header-section-number">4.1.3</span> Visualising Functional Trait Diversity Using Principal Component Analysis (PCA)

**Ecological Question**

*How functionally different are species within the regional species pool, and what does this reveal about the evolutionary legacy of historical speciation?*

While the correlation matrix identifies relationships among individual traits, it does not provide an overall picture of how species differ across multiple traits simultaneously. Functional traits represent different aspects of species ecology, including feeding, locomotion and body morphology, all of which have evolved over long evolutionary timescales. Principal Component Analysis (PCA) is a multivariate ordination technique that reduces these multiple trait dimensions into a small number of principal axes while retaining as much of the original variation as possible. Functional diversity therefore complements species richness by describing ecological differences among species within communities (Petchey & Gaston, 2006).

Unlike previous ordination analyses in this chapter, which described differences among sampling sites or communities, this PCA focuses on species. Each point on the ordination represents a single fish species positioned according to its functional trait values. Species located close together share similar ecological strategies, whereas species that are widely separated occupy distinct regions of functional trait space. Consequently, the distribution of species within the ordination provides an indirect representation of the evolutionary diversification generated through historical speciation.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# STEP 1
# Load the package required for PCA visualisation
############################################################

library(ggplot2)

############################################################
# STEP 2
# Create a matrix containing only functional traits
############################################################

trait_matrix <- villeger_data[, -1]

############################################################
# STEP 3
# Perform Principal Component Analysis (PCA)
############################################################

trait_pca <- prcomp(
  trait_matrix,
  center = TRUE,
  scale. = TRUE
)

############################################################
# STEP 4
# Extract PCA scores for each species
############################################################

pca_scores <- as.data.frame(trait_pca$x)

pca_scores$Species <- villeger_data$Sp

############################################################
# STEP 5
# Calculate the percentage of variation explained
############################################################

variance <- summary(trait_pca)$importance[2, ] * 100

############################################################
# STEP 6
# Calculate the convex hull
############################################################

hull <- pca_scores[chull(pca_scores$PC1, pca_scores$PC2), ]

############################################################
# STEP 7
# Produce a publication-quality PCA plot
############################################################

ggplot(pca_scores, aes(PC1, PC2)) +

  # Functional trait space
  geom_polygon(
    data = hull,
    aes(PC1, PC2),
    fill = "skyblue",
    alpha = 0.20,
    colour = "steelblue",
    linewidth = 0.8
  ) +

  # Species
  geom_point(
    size = 3,
    colour = "black",
    fill = "steelblue",
    shape = 21,
    stroke = 0.4
  ) +

  # Species labels
  geom_text(
    aes(label = Species),
    size = 3,
    vjust = -0.7,
    check_overlap = TRUE
  ) +

  labs(
    title = "Functional Trait Space of the Regional 
    Species Pool",
    subtitle = "Principal Component Analysis of Functional Traits",
    x = paste0(
      "Principal Component 1 (",
      round(variance[1],1),
      "%)"
    ),
    y = paste0(
      "Principal Component 2 (",
      round(variance[2],1),
      "%)"
    )
  ) +

  coord_equal() +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90")
  )
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.4.** Principal Component Analysis (PCA) of the functional traits of 46 fish species from the regional species pool. Each point represents a species positioned according to its multidimensional functional trait values. Species located close together possess similar ecological strategies, whereas widely separated species occupy distinct regions of functional trait space. The shaded convex hull represents the total functional trait space occupied by the regional species pool and provides an indirect representation of the evolutionary legacy of historical speciation.

**Interpretation:**

The PCA ordination summarises the functional trait dataset into two principal components that together explain 46.6% of the total variation in species traits (PC1 = 31.6%, PC2 = 15.0%). Each point represents a fish species positioned according to its multidimensional functional trait values, with the distance between species reflecting their degree of functional similarity.

Most species are concentrated near the centre of the ordination, indicating that they share relatively similar combinations of morphological and ecological traits. This clustering suggests that many species occupy comparable functional niches despite belonging to different taxa. However, several species occur at the outer edges of the ordination, including sp55, sp70, sp25, sp2, sp19 and sp58. These species are functionally distinct from the main cluster and represent unique combinations of traits that expand the overall functional trait space occupied by the regional species pool.

The convex hull highlights the full extent of this functional trait space and demonstrates that the regional species pool encompasses a broad range of ecological strategies rather than a single dominant trait combination. The presence of both a dense central cluster and several functionally distinctive species suggests that historical evolutionary diversification has produced species with varying ecological roles, from common functional strategies to more specialised adaptations.

Importantly, this analysis does not measure speciation directly. Instead, it illustrates the evolutionary legacy of historical speciation through the diversity of functional traits observed among species. The wide distribution of species across multidimensional trait space indicates that historical diversification has generated a functionally diverse regional species pool. This functional diversity provides the evolutionary foundation upon which contemporary metacommunity processes, such as environmental filtering, dispersal and ecological drift, subsequently act to determine patterns of community assembly.

</div>

<div id="quantifying-functional-richness" class="section level3" number="4.1.4">

### <span class="header-section-number">4.1.4</span> Quantifying Functional Richness

**Ecological Question**

*How much of the regional functional trait space is occupied by local communities, and what does this reveal about the evolutionary legacy of historical speciation?*

While Principal Component Analysis provides a visual representation of functional trait diversity, it does not quantify the amount of functional space occupied by individual communities. Functional Richness (FRic) addresses this by measuring the volume of multidimensional trait space filled by the species present within a community. Communities with high functional richness contain species spanning a wide range of ecological strategies, whereas communities with low functional richness occupy only a small portion of the available functional trait space.

Functional Richness does not measure speciation directly. Instead, it quantifies the extent to which the functional diversity generated through historical evolutionary diversification is represented within contemporary communities. As a result, FRic provides an indirect measure of the evolutionary opportunities available for ecological assembly.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# STEP 1
# Load packages required for functional diversity analysis
############################################################

library(FD)
library(ggplot2)
library(dplyr)
library(readxl)

############################################################
# STEP 2
# Import the community matrix
############################################################

community_data <- read_excel(
  "data/Villeger2012e_AJ.xlsx",
  sheet = "comm"
)

############################################################
# STEP 3
# Prepare the community matrix
############################################################

community_matrix <- as.data.frame(community_data)

rownames(community_matrix) <- community_matrix[[1]]

community_matrix <- community_matrix[, -1]

############################################################
# STEP 4
# Prepare the functional trait matrix
############################################################

trait_matrix <- as.data.frame(villeger_data)

rownames(trait_matrix) <- trait_matrix$Sp

trait_matrix <- trait_matrix[, -1]

############################################################
# STEP 5
# Calculate Functional Richness
############################################################

fd_results <- dbFD(
  x = trait_matrix,
  a = community_matrix,
  calc.FRic = TRUE
)

############################################################
# STEP 6
# Create a dataframe for plotting
############################################################

fric_data <- data.frame(
  Site = names(fd_results$FRic),
  FRic = fd_results$FRic
)

############################################################
# STEP 7
# Visualise Functional Richness
############################################################

ggplot(fric_data,
       aes(x = reorder(Site, FRic),
           y = FRic)) +

  geom_col(fill = "steelblue") +

  coord_flip() +

  labs(
    title = "Functional Richness Across Sampling Sites",
    x = "Sampling Site",
    y = "Functional Richness (FRic)"
  ) +

  theme_minimal(base_size = 12)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    FEVe: Could not be calculated for communities with <3 functionally singular species. 
    FRic: To respect s > t, FRic could not be calculated for communities with <3 functionally singular species. 
    FRic: Dimensionality reduction was required. The last 14 PCoA axes (out of 16 in total) were removed. 
    FRic: Quality of the reduced-space representation = 0.4658204 
    FDiv: Could not be calculated for communities with <3 functionally singular species. 

</div>

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.5.** Functional Richness (FRic) for the 35 sampling sites in the Villeger et al. (2012) metacommunity dataset. FRic quantifies the volume of multidimensional functional trait space occupied by each local community. Higher FRic values indicate that communities contain species representing a broader range of ecological strategies, whereas lower values indicate that communities occupy a smaller proportion of the regional functional trait space generated through historical evolutionary diversification.

**Interpretation:**

Functional Richness (FRic) varies considerably among the 35 sampling sites, indicating that local communities differ in the amount of functional trait space they occupy. Sites 17 and 5 exhibit the highest Functional Richness, suggesting that these communities contain species representing a broad range of ecological strategies. In contrast, sites such as 34, 36 and 22 have comparatively low FRic values, indicating that they occupy only a small proportion of the regional functional trait space.

These differences do not indicate varying rates of speciation among communities. Rather, they reflect how much of the evolutionary diversity generated through historical speciation is represented within each local assemblage. Functional Richness therefore provides a quantitative measure of the extent to which the evolutionary legacy of speciation is expressed across the metacommunity.

While Functional Richness measures the volume of functional trait space occupied by each community, it does not describe how species are distributed within that space. The next step therefore examines Functional Dispersion (FDis), which quantifies how functionally similar or dissimilar species are within communities and provides additional insight into the organisation of functional diversity across the metacommunity.

</div>

<div id="quantifying-functional-dispersion" class="section level3" number="4.1.5">

### <span class="header-section-number">4.1.5</span> Quantifying Functional Dispersion

**Ecological Question**

*How functionally similar or dissimilar are species within local communities, and what does this reveal about the evolutionary legacy of historical speciation?*

While Functional Richness measures the volume of functional trait space occupied by a community, it does not describe how species are distributed within that space. Communities may occupy a similar volume of trait space but differ substantially in the degree of functional similarity among their constituent species. Functional Dispersion (FDis) addresses this limitation by measuring the average distance of species from the community centroid in multidimensional trait space. Communities with high Functional Dispersion contain species that are functionally distinct from one another, whereas communities with low Functional Dispersion are composed of species with more similar ecological strategies.

As with the previous analyses, Functional Dispersion does not quantify speciation directly. Instead, it measures how the functional diversity generated through historical evolutionary diversification is expressed within local communities. Consequently, FDis provides an additional perspective on the evolutionary legacy of speciation by quantifying the distribution of ecological strategies represented across the metacommunity.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# STEP 1
# Load the required packages
############################################################

library(FD)
library(ggplot2)

############################################################
# STEP 2
# Calculate Functional Diversity indices
############################################################

fd_results <- dbFD(
  x = trait_matrix,
  a = community_matrix,
  calc.FRic = TRUE
)

############################################################
# STEP 3
# Extract Functional Dispersion
############################################################

fdis_data <- data.frame(
  Site = names(fd_results$FDis),
  FDis = fd_results$FDis
)

############################################################
# STEP 4
# Order sites from highest to lowest Functional Dispersion
############################################################

fdis_data <- fdis_data[
  order(fdis_data$FDis, decreasing = TRUE),
]

############################################################
# STEP 5
# Visualise Functional Dispersion
############################################################

ggplot(
  fdis_data,
  aes(
    x = reorder(Site, FDis),
    y = FDis
  )
) +

  geom_col(
    fill = "steelblue"
  ) +

  coord_flip() +

  labs(
    title = "Functional Dispersion Across Sampling Sites",
    x = "Sampling Site",
    y = "Functional Dispersion (FDis)"
  ) +

  theme_minimal(base_size = 12)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    FEVe: Could not be calculated for communities with <3 functionally singular species. 
    FRic: To respect s > t, FRic could not be calculated for communities with <3 functionally singular species. 
    FRic: Dimensionality reduction was required. The last 14 PCoA axes (out of 16 in total) were removed. 
    FRic: Quality of the reduced-space representation = 0.4658204 
    FDiv: Could not be calculated for communities with <3 functionally singular species. 

</div>

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.6.** Functional Dispersion (FDis) for the 35 sampling sites in the Villeger et al. (2012) metacommunity dataset. FDis measures the average distance of species from the community centroid in multidimensional functional trait space. Higher values indicate communities containing functionally distinct species that represent a wide range of ecological strategies, whereas lower values indicate greater functional similarity among species. Functional Dispersion provides an indication of how the evolutionary diversity generated through historical speciation is distributed within local communities.

**Interpretation:**

Functional Dispersion (FDis) varies across the 35 sampling sites, indicating differences in how species are distributed within functional trait space. Sites 29, 5 and 6 exhibit the highest FDis values, suggesting that these communities contain species with a wide range of ecological strategies. In contrast, sites such as 17, 22 and 30 have the lowest FDis values, indicating that species within these communities are functionally more similar.

Unlike Functional Richness, which measures the volume of trait space occupied, Functional Dispersion describes how species are arranged within that space. These differences reflect variation in the expression of functional diversity among local communities and provide further evidence of how the evolutionary legacy of historical speciation is represented across the metacommunity.

Together, the PCA, Functional Richness and Functional Dispersion analyses provide complementary perspectives on functional trait diversity. The final section integrates these results to demonstrate how historical speciation has generated the regional pool of ecological strategies that underpins contemporary metacommunity assembly.

</div>

<div id="conceptual-synthesis-inferring-the-evolutionary-legacy-of-historical-speciation" class="section level3" number="4.1.6">

### <span class="header-section-number">4.1.6</span> Conceptual Synthesis: Inferring the Evolutionary Legacy of Historical Speciation

*How can functional trait analyses be used to infer the role of historical speciation in shaping metacommunity structure?*

Historical speciation is one of the four fundamental processes that underpin community assembly, alongside environmental filtering, dispersal and ecological drift (Vellend, 2010). Unlike the latter three processes, however, speciation operates over evolutionary rather than ecological timescales. Consequently, direct measurements of speciation require phylogenetic, genetic or fossil evidence that is generally unavailable in ecological community datasets. Instead, empirical metacommunity studies infer the legacy of historical speciation by examining the diversity of functional traits expressed within the regional species pool (Leibold et al., 2004; Logue et al., 2011).

The Villeger et al. (2012) dataset provides an excellent example of this approach by combining community composition with sixteen morphological and ecological traits describing forty-six fish species. These functional traits represent evolutionary adaptations that have accumulated through diversification over long periods of evolutionary time. As argued by McGill et al. (2006) and Violle et al. (2007), functional traits provide the link between evolutionary history and contemporary ecological processes because they describe the ecological strategies through which species interact with their environment.

The correlation analysis demonstrated that functional traits do not vary independently. Several traits exhibited strong positive correlations, while others showed clear negative relationships, indicating that suites of morphological characteristics have evolved together rather than in isolation. Such coordinated trait variation reflects the integration of feeding, locomotory and body morphology into distinct ecological strategies. Rather than representing random combinations of traits, these correlations suggest that historical diversification has produced species occupying different regions of functional niche space, providing evidence of the functional differentiation expected from long-term evolutionary processes.


Functional Richness (FRic) complemented the ordination by quantifying how much of this regional functional trait space was represented within individual communities. Considerable variation among the thirty-five sampling sites was observed, with sites 17 and 5 occupying the greatest proportion of functional trait space and sites such as 34, 36 and 22 occupying substantially smaller volumes. Introduced by Villéger et al. (2008), Functional Richness measures the multidimensional volume of ecological strategies represented within a community rather than simply counting species. Communities with high FRic therefore incorporate a larger proportion of the evolutionary diversity available within the regional species pool, whereas communities with low FRic represent only a subset of that diversity.

Functional Dispersion (FDis) provided a complementary measure by describing how species were distributed within occupied trait space. Although some communities occupied similar volumes of functional space, they differed markedly in the arrangement of species within that space. Sites 29, 5 and 6 displayed the highest Functional Dispersion, indicating communities composed of functionally distinct species representing diverse ecological strategies. Conversely, sites such as 17, 22 and 30 exhibited much lower Functional Dispersion, suggesting greater functional similarity among species. Laliberté and Legendre (2010) demonstrated that Functional Dispersion captures the distribution of species within multidimensional trait space independently of species richness, providing a robust measure of functional organisation within ecological communities.

Collectively, these analyses demonstrate why functional trait approaches have become central to modern metacommunity ecology. The correlation matrix revealed coordinated evolutionary relationships among traits, the PCA visualised the multidimensional functional trait space generated through diversification, Functional Richness quantified how much of that evolutionary diversity was represented within local communities, and Functional Dispersion described how species were distributed within that functional space. Together, these complementary analyses provide a quantitative framework for inferring the evolutionary legacy of historical speciation despite the absence of phylogenetic or genetic data.

Importantly, these results should not be interpreted as direct measurements of speciation. Rather, they represent quantitative evidence of the functional consequences of historical evolutionary diversification. Speciation generates the regional species pool, diversification produces functional trait variation, and contemporary ecological processes subsequently determine how that variation is assembled into local communities. This conceptual sequence reflects the modern view of metacommunity ecology proposed by Leibold et al. (2004) and synthesised by Vellend (2010), in which speciation establishes the evolutionary template upon which environmental filtering, dispersal and ecological drift operate. Consequently, functional trait diversity provides an effective empirical proxy for quantifying the ecological legacy of historical speciation and offers a practical framework for integrating evolutionary and ecological processes within metacommunity studies.

<div class="callout callout-style-default callout-important callout-titled" title="Key Take-home Message">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Key Take-home Message

</div>

</div>

<div class="callout-body-container callout-body">

Historical speciation cannot usually be measured directly using ecological community datasets because evolutionary processes occur over timescales that extend beyond most ecological surveys. Instead, ecologists infer its influence by quantifying functional trait diversity within the regional species pool. In this example, the correlation analysis, Principal Component Analysis (PCA), Functional Richness (FRic) and Functional Dispersion (FDis) collectively demonstrate that historical evolutionary diversification has generated a broad range of ecological strategies. These strategies form the evolutionary foundation upon which contemporary metacommunity processes—environmental filtering, dispersal and ecological drift—subsequently act to shape community composition and metacommunity structure.

</div>

</div>

</div>

</div>

<div id="quantifying-environmental-filtering" class="section level2" number="4.2">

## <span class="header-section-number">4.2</span> Quantifying Environmental Filtering

**Ecological Question**

*To what extent do environmental conditions determine patterns of species composition and functional diversity across local communities within a metacommunity?*

Environmental filtering is one of the four fundamental processes underlying metacommunity dynamics (Vellend, 2010). It describes the process by which abiotic conditions, such as temperature, salinity, water clarity and oxygen availability, act as ecological filters that favour species possessing functional traits suited to local environmental conditions while excluding species that are poorly adapted. As a consequence, communities occupying similar environments are expected to contain similar species and ecological strategies, whereas communities experiencing different environmental conditions are expected to differ in both species composition and functional diversity.

Unlike historical speciation, which operates over evolutionary timescales to generate the regional species pool, environmental filtering operates at ecological timescales by determining which members of that species pool are able to persist within particular habitats. Consequently, environmental filtering links species’ functional traits with local abiotic conditions and plays a central role in shaping patterns of community assembly across landscapes.

In this worked example, environmental filtering is investigated using a series of complementary multivariate analyses. Environmental variables are first explored to identify the major abiotic gradients present within the metacommunity. Distance-based Redundancy Analysis (dbRDA) is then used to determine whether variation in community composition is associated with these environmental gradients. Functional Diversity analyses assess whether environmental conditions influence the diversity of ecological strategies represented within local communities, while RLQ and Fourth-corner analyses integrate environmental variables, community composition and functional traits to evaluate whether environmental gradients act through species’ functional characteristics. Together, these analyses provide a quantitative framework for assessing the importance of environmental filtering in structuring fish metacommunities.

**Conceptual Framework:**

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

    A[&quot;Regional Species Pool&lt;br/&gt;(Functional diversity generated through historical speciation)&quot;]

    B[&quot;Environmental Gradients&lt;br/&gt;(Depth, salinity, oxygen, water clarity)&quot;]

    C[&quot;Environmental Filtering&lt;br/&gt;(Species sorting based on functional traits)&quot;]

    D[&quot;Local Community Assembly&lt;br/&gt;(Species composition and functional diversity)&quot;]

    A --&gt; C
    B --&gt; C
    C --&gt; D

    style A fill:#D6EAF8,stroke:#2E86C1
    style B fill:#FCF3CF,stroke:#B7950B
    style C fill:#FADBD8,stroke:#CB4335
    style D fill:#D5F5E3,stroke:#239B56</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 4.7.** Conceptual representation of environmental filtering within a metacommunity. Historical speciation generates the regional pool of species and functional traits, while environmental gradients act as ecological filters that favour species possessing suitable ecological characteristics. Communities assembled under similar environmental conditions are therefore expected to exhibit similar species composition and functional trait distributions.

The following analyses investigate whether environmental conditions contribute to patterns of community assembly within the fish metacommunity. The workflow begins by exploring the measured environmental variables and identifying the principal abiotic gradients present across the sampling sites. These gradients are then related to patterns of community composition using constrained ordination, followed by analyses of functional diversity and trait–environment relationships to determine whether environmental filtering operates through species’ ecological characteristics.

Collectively, these analyses allow environmental filtering to be evaluated from complementary perspectives. Rather than relying on a single statistical method, the combined approach examines how environmental gradients influence species composition, functional diversity and functional trait distributions, providing multiple lines of evidence for assessing the role of environmental selection in structuring the metacommunity.

<div id="importing-and-preparing-the-data" class="section level3" number="4.2.1">

### <span class="header-section-number">4.2.1</span> Importing and Preparing the Data

Before investigating the role of environmental filtering in metacommunity assembly, the different components of the dataset must be imported and organised into compatible data structures. Metacommunity analyses integrate multiple sources of ecological information, including species abundances, environmental conditions, spatial locations and functional traits. Each dataset represents a different aspect of the metacommunity and contributes unique information about the processes shaping community composition.

The community matrix describes the abundance of each fish species across sampling sites and provides the foundation for analyses of community composition. The environmental matrix contains the abiotic variables that may act as ecological filters, while the coordinate matrix records the geographic position of each sampling site, allowing spatial processes to be investigated later in the chapter. Finally, the functional trait matrix describes the ecological and morphological characteristics of each species, providing the link between environmental conditions and species responses.

Before these datasets can be analysed together, they must be imported, checked for consistency and converted into compatible formats. This preparation ensures that species names, sampling sites and environmental variables correspond correctly across all analyses, providing a reliable foundation for investigating metacommunity assembly.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 1. Load Required Packages
############################################################

library(vegan)
library(tidyverse)
library(ade4)
library(adespatial)
library(FD)
library(betapart)
library(corrplot)

############################################################
# Step 2. Define the Data Directory
############################################################

# Path to the folder containing all CSV files
data_dir <- "C:/Users/27848/Desktop/university stuff/honours UWC/Quant/MetacommunityChapter/data"

############################################################
# Step 3. Import the Data
############################################################

# Community composition matrix
comm <- read.csv(
  file.path(data_dir, "comm.csv"),
  row.names = 1
)

# Environmental variables
envir <- read.csv(
  file.path(data_dir, "envir.csv"),
  row.names = 1
)

# Spatial coordinates
coord <- read.csv(
  file.path(data_dir, "coord.csv"),
  row.names = 1
)

# Functional trait matrix
traits1 <- read.csv(
  file.path(data_dir, "Traits1.csv"),
  stringsAsFactors = FALSE
)

############################################################
# Step 4. Prepare the Trait Matrix
############################################################

# Use species names as row names
rownames(traits1) <- traits1$Sp

# Remove the species name column
traits1$Sp <- NULL

############################################################
# Step 5. Convert Variables to Numeric
############################################################

comm[] <- lapply(comm, as.numeric)
envir[] <- lapply(envir, as.numeric)
coord[] <- lapply(coord, as.numeric)
traits1[] <- lapply(traits1, as.numeric)

############################################################
# Step 6.a. Inspect the Data
############################################################

cat("Community matrix:", dim(comm), "\n")
cat("Environmental matrix:", dim(envir), "\n")
cat("Coordinate matrix:", dim(coord), "\n")
cat("Trait matrix:", dim(traits1), "\n")

head(comm)
head(envir)
head(coord)
head(traits1)

############################################################
# Step 6.b. Hellinger Transformation
############################################################

comm.hel <- decostand(
  comm,
  method = "hellinger"
)

head(comm.hel)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    Community matrix: 35 46 

</div>

<div class="cell-output cell-output-stdout">

    Environmental matrix: 35 4 

</div>

<div class="cell-output cell-output-stdout">

    Coordinate matrix: 35 2 

</div>

<div class="cell-output cell-output-stdout">

    Trait matrix: 46 16 

</div>

<div class="cell-output cell-output-stdout">

       sp2 sp3  sp4 sp6    sp7   sp8 sp9  sp10    sp12 sp15   sp16 sp17 sp18 sp19
    1  9.2   0 0.00   0 324.60 12.60   0 601.1 4064.80    0   0.00    0    0    0
    2  0.0   0 0.00   0   0.00  0.00   0   0.0    0.00    0   0.00    0    0    0
    3  0.0   0 1.77   0 130.98 86.32   0   0.0  916.24    0  13.76    0    0    0
    4  0.0   0 0.00   0   0.00  0.00   0   0.0    0.00    0 102.05    0    0    0
    5  0.0   0 0.00   0  62.90  0.00   0   0.0  279.70    0   0.00    0    0    0
    6 17.5   0 0.00   0   0.00  0.00   0   0.0    0.00    0   0.00    0    0    0
        sp20 sp21 sp22  sp23 sp25 sp27 sp28 sp30 sp31 sp32 sp34 sp35  sp42 sp43
    1 209.79 5.45    0  0.00    0    0    0    0    0    0 0.00    0  0.00    0
    2   0.00 0.00    0 89.40    0    0    0    0    0    0 0.00    0  0.00    0
    3  10.42 1.74    0  0.00    0    0    0    0    0    0 0.00    0  0.00    0
    4   0.00 0.00    0 98.66    0    0    0    0    0    0 9.65    0 15.19    0
    5   0.00 0.00    0  0.00    0    0    0    0    0    0 0.00    0  0.00    0
    6   0.00 0.00    0  0.00    0    0    0    0    0    0 0.00    0  0.00    0
       sp44 sp46   sp49  sp51 sp53 sp55  sp56 sp57 sp58 sp59 sp61 sp62   sp63
    1  0.00    0 213.86  0.00    0    0   0.0  0.0 0.00    0    0    0 189.95
    2 15.02    0   0.00  0.00    0    0   0.0  0.0 0.00    0    0    0 148.12
    3  0.00    0 160.00  0.00    0    0   0.0  0.0 1.57    0    0    0 112.93
    4 17.54    0   0.00 43.51    0    0   0.0  0.0 0.00    0    0    0 396.19
    5  0.00    0  58.00  0.00    0    0 980.2  1.3 0.00    0    0    0   0.00
    6  0.00    0  33.45 17.12    0    0   0.0  0.0 0.00    0    0    0   9.58
        sp64 sp65 sp66  sp68 sp70
    1   0.00    0    0  0.00    0
    2 137.28    0    0 20.15    0
    3  38.44    0    0  0.00    0
    4 189.73    0    0 14.57    0
    5 227.48    0    0 65.80    0
    6  12.98    0    0 21.76    0

</div>

<div class="cell-output cell-output-stdout">

      Depth Secchi Salinity Oxygen
    1   5.1    0.1    32.13   2.83
    2   7.2    0.7    37.51   2.35
    3   3.5    0.4    36.64   4.23
    4   7.8    0.7    36.86   4.84
    5   2.9    0.3    36.38   5.29
    6   8.6    0.6    37.03   3.11

</div>

<div class="cell-output cell-output-stdout">

             X       Y
    1 531885.4 2058773
    2 536502.7 2063189
    3 540517.6 2059175
    4 543930.6 2064193
    5 548547.6 2060580
    6 551558.8 2066000

</div>

<div class="cell-output cell-output-stdout">

         logM  OgSf  OgSh  OgPo  EySz  GrLg  GtLg  EyPo  BdSh  BdSf  PfPo  PfSh
    sp2 2.187 0.072 0.947 1.000 0.151 0.000 1.782 1.000 0.143 2.168 0.000 0.000
    sp3 2.522 0.285 1.630 0.317 0.490 0.309 0.647 0.556 2.083 1.817 0.822 2.153
    sp4 0.706 0.283 2.054 0.508 0.474 0.381 0.688 0.584 3.292 3.974 0.773 2.708
    sp6 3.327 0.056 0.648 0.273 0.294 0.032 3.467 0.647 3.102 1.596 0.638 6.868
    sp7 3.110 0.173 0.513 0.346 0.263 0.128 2.078 0.647 1.021 1.658 0.806 3.480
    sp8 2.170 0.248 0.519 0.489 0.357 0.142 2.154 0.613 1.016 2.087 0.662 3.674
         CpHt  CfSh  FsRt  FsSf
    sp2 1.121 0.767 0.000 1.158
    sp3 2.288 2.775 0.449 1.806
    sp4 2.493 3.108 0.504 2.618
    sp6 2.906 2.684 1.060 1.502
    sp7 3.592 4.052 0.781 1.648
    sp8 4.205 3.460 0.625 1.678

</div>

<div class="cell-output cell-output-stdout">

             sp2 sp3        sp4 sp6       sp7        sp8 sp9      sp10      sp12
    1 0.04041919   0 0.00000000   0 0.2400866 0.04730195   0 0.3267135 0.8495977
    2 0.00000000   0 0.00000000   0 0.0000000 0.00000000   0 0.0000000 0.0000000
    3 0.00000000   0 0.03465077   0 0.2980772 0.24198137   0 0.0000000 0.7883714
    4 0.00000000   0 0.00000000   0 0.0000000 0.00000000   0 0.0000000 0.0000000
    5 0.00000000   0 0.00000000   0 0.1937620 0.00000000   0 0.0000000 0.4085917
    6 0.39459828   0 0.00000000   0 0.0000000 0.00000000   0 0.0000000 0.0000000
      sp15       sp16 sp17 sp18 sp19       sp20       sp21 sp22     sp23 sp25 sp27
    1    0 0.00000000    0    0    0 0.19301281 0.03110943    0 0.000000    0    0
    2    0 0.00000000    0    0    0 0.00000000 0.00000000    0 0.466974    0    0
    3    0 0.09661297    0    0    0 0.08407368 0.03435586    0 0.000000    0    0
    4    0 0.33917409    0    0    0 0.00000000 0.00000000    0 0.333493    0    0
    5    0 0.00000000    0    0    0 0.00000000 0.00000000    0 0.000000    0    0
    6    0 0.00000000    0    0    0 0.00000000 0.00000000    0 0.000000    0    0
      sp28 sp30 sp31 sp32      sp34 sp35      sp42 sp43      sp44 sp46      sp49
    1    0    0    0    0 0.0000000    0 0.0000000    0 0.0000000    0 0.1948761
    2    0    0    0    0 0.0000000    0 0.0000000    0 0.1914075    0 0.0000000
    3    0    0    0    0 0.0000000    0 0.0000000    0 0.0000000    0 0.3294475
    4    0    0    0    0 0.1042989    0 0.1308564    0 0.1406148    0 0.0000000
    5    0    0    0    0 0.0000000    0 0.0000000    0 0.0000000    0 0.1860618
    6    0    0    0    0 0.0000000    0 0.0000000    0 0.0000000    0 0.5455496
           sp51 sp53 sp55     sp56       sp57       sp58 sp59 sp61 sp62      sp63
    1 0.0000000    0    0 0.000000 0.00000000 0.00000000    0    0    0 0.1836595
    2 0.0000000    0    0 0.000000 0.00000000 0.00000000    0    0    0 0.6010780
    3 0.0000000    0    0 0.000000 0.00000000 0.03263443    0    0    0 0.2767776
    4 0.2214679    0    0 0.000000 0.00000000 0.00000000    0    0    0 0.6682945
    5 0.0000000    0    0 0.764893 0.02785576 0.00000000    0    0    0 0.0000000
    6 0.3902906    0    0 0.000000 0.00000000 0.00000000    0    0    0 0.2919570
           sp64 sp65 sp66      sp68 sp70
    1 0.0000000    0    0 0.0000000    0
    2 0.5786655    0    0 0.2216979    0
    3 0.1614797    0    0 0.0000000    0
    4 0.4624706    0    0 0.1281581    0
    5 0.3684809    0    0 0.1981784    0
    6 0.3398392    0    0 0.4400131    0

</div>

</div>

The datasets are now ready for analysis. The community matrix contains species abundances, the environmental matrix describes abiotic conditions, the coordinate matrix records the spatial arrangement of sampling sites, and the trait matrix contains the functional characteristics of each species. Together, these datasets provide the information needed to investigate environmental filtering, dispersal and ecological drift.

Before testing ecological hypotheses, it is useful to explore the structure and contents of each dataset. This provides an overview of the variables available and confirms that the data have been imported correctly.

</div>

<div id="exploring-environmental-gradients" class="section level3" number="4.2.2">

### <span class="header-section-number">4.2.2</span> Exploring Environmental Gradients

*Which environmental gradients may act as ecological filters influencing community assembly?*

Environmental variables often interact to shape local habitat conditions. Exploring these relationships provides an overview of the major environmental gradients within the metacommunity and identifies potential abiotic filters before formal multivariate analyses are performed.

**Ecological Purpose:**

Environmental variables are first standardised so that each variable contributes equally to the analyses, regardless of its original units of measurement. A correlation matrix is then used to examine relationships among the environmental variables and identify the principal environmental gradients within the study system.

**Standardising Environmental Variables:**

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Standardise environmental variables
############################################################

env.std <- as.data.frame(scale(envir))
```

</div>

Standardising the environmental variables converts each variable to a common scale, preventing variables with larger numerical values from having a disproportionate influence on subsequent multivariate analyses.

**Correlation Matrix:**

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Correlation matrix
############################################################

cor.matrix <- cor(env.std)

corrplot(
  cor.matrix,
  method = "circle"
)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.8.** Correlation matrix showing relationships among the measured environmental variables. Positive correlations indicate variables that increase together, while negative correlations indicate inverse relationships. Circle size and colour intensity represent the strength of each correlation.

**Interpretation:**

The correlation matrix reveals several moderate relationships among the environmental variables, indicating that multiple abiotic gradients characterise the metacommunity. Depth and salinity are positively correlated, while both show negative relationships with oxygen concentration. In contrast, Secchi depth exhibits weaker correlations, suggesting that water clarity represents a relatively independent environmental gradient.

These environmental gradients represent potential ecological filters that may influence which species and functional traits occur at different sampling sites. The next analysis tests this directly by relating community composition to the measured environmental variables using Distance-based Redundancy Analysis (dbRDA).

</div>

<div id="distance-based-redundancy-analysis-dbrda" class="section level3" number="4.2.3">

### <span class="header-section-number">4.2.3</span> Distance-based Redundancy Analysis (dbRDA)

*Do environmental gradients explain differences in community composition among sampling sites?*

Distance-based Redundancy Analysis (dbRDA) is a constrained ordination technique that relates variation in community composition to measured environmental variables. It determines whether differences among communities are associated with environmental gradients and identifies which variables contribute most strongly to community assembly.

dbRDA tests the environmental filtering hypothesis by assessing whether variation in species composition can be explained by the measured environmental variables. If community composition changes along environmental gradients, this provides evidence that environmental conditions influence community assembly.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 4. Distance-based Redundancy Analysis (dbRDA)
############################################################

dbRDA <- capscale(
  comm.hel ~ .,
  data = env.std,
  distance = "bray"
)

anova(dbRDA)

anova(
  dbRDA,
  by = "margin"
)

plot(dbRDA)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    Permutation test for capscale under reduced model
    Permutation: free
    Number of permutations: 999

    Model: capscale(formula = comm.hel ~ Depth + Secchi + Salinity + Oxygen, data = env.std, distance = "bray")
             Df SumOfSqs      F Pr(>F)    
    Model     4   3.3685 2.7803  0.001 ***
    Residual 30   9.0870                  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

</div>

<div class="cell-output cell-output-stdout">

    Permutation test for capscale under reduced model
    Marginal effects of terms
    Permutation: free
    Number of permutations: 999

    Model: capscale(formula = comm.hel ~ Depth + Secchi + Salinity + Oxygen, data = env.std, distance = "bray")
             Df SumOfSqs      F Pr(>F)    
    Depth     1   0.6175 2.0386  0.019 *  
    Secchi    1   0.7568 2.4985  0.002 ** 
    Salinity  1   0.8825 2.9136  0.001 ***
    Oxygen    1   0.3711 1.2251  0.237    
    Residual 30   9.0870                  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

</div>

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.9** Distance-based Redundancy Analysis (dbRDA) ordination showing the relationship between fish community composition and the measured environmental variables. Environmental vectors indicate the direction and relative strength of the environmental gradients influencing community composition.

**Interpretation:**

The dbRDA ordination indicates that environmental gradients contribute to differences in fish community composition across the metacommunity. Water clarity (Secchi depth) appears to be the strongest environmental gradient, while depth, salinity and oxygen concentration also influence community structure. Sites located in the direction of an environmental vector are associated with higher values of that variable, and species positioned away from the centre of the ordination show stronger associations with particular environmental conditions.

The overall significance of these relationships should be confirmed using the permutation tests (anova(dbRDA) and anova(dbRDA, by = “margin”)). Together, these results provide evidence that environmental filtering contributes to metacommunity assembly by influencing the distribution of species across environmental gradients.

While dbRDA demonstrates whether environmental gradients influence species composition, it does not indicate whether they also affect the diversity of ecological strategies within communities. The next analysis examines this using functional diversity indices, which quantify how environmental filtering shapes the functional characteristics of local communities.

</div>

<div id="functional-diversity" class="section level3" number="4.2.4">

### <span class="header-section-number">4.2.4</span> Functional Diversity

*Do environmental gradients influence the functional diversity of local communities?*

Species respond to environmental conditions according to their functional traits rather than their taxonomic identities. If environmental filtering is important, communities occupying different habitats should differ not only in species composition but also in the diversity of ecological strategies they contain.

Functional diversity indices quantify the range and distribution of functional traits within communities. These metrics help determine whether environmental conditions influence the ecological strategies represented across the metacommunity.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 5. Functional Diversity
############################################################

# Ensure species order matches
traits.fd <- traits1[colnames(comm), ]

# Check
all(colnames(comm) == rownames(traits.fd))

# Functional diversity
fd <- dbFD(
  x = traits.fd,
  a = comm,
  stand.x = FALSE
)

fd
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    [1] TRUE

</div>

<div class="cell-output cell-output-stdout">

    FEVe: Could not be calculated for communities with <3 functionally singular species. 
    FRic: To respect s > t, FRic could not be calculated for communities with <3 functionally singular species. 
    FRic: Dimensionality reduction was required. The last 14 PCoA axes (out of 16 in total) were removed. 
    FRic: Quality of the reduced-space representation = 0.6119752 
    FDiv: Could not be calculated for communities with <3 functionally singular species. 

</div>

<div class="cell-output cell-output-stdout">

    $nbsp
     1  2  3  4  5  6  7  8  9 10 11 12 13 14 16 17 18 20 21 22 23 24 25 26 27 28 
     9  5 11  9  7  6 10  4  6  5  6  5  4  7  6  2  8  5  3  3 11 13  8 11 16 10 
    29 30 31 32 33 34 35 36 37 
     9  7 12 12  7  4  3  5  7 

    $sing.sp
     1  2  3  4  5  6  7  8  9 10 11 12 13 14 16 17 18 20 21 22 23 24 25 26 27 28 
     9  5 11  9  7  6 10  4  6  5  6  5  4  7  6  2  8  5  3  3 11 13  8 11 16 10 
    29 30 31 32 33 34 35 36 37 
     9  7 12 12  7  4  3  5  7 

    $FRic
            1         2         3         4         5         6         7         8 
     4.493650  1.958308 21.353634 16.651815 32.932164  9.052075 21.090361  2.346520 
            9        10        11        12        13        14        16        17 
    16.917969  7.799632  4.321345  2.430536  4.483290  7.799632 11.734882        NA 
           18        20        21        22        23        24        25        26 
    16.959693  7.946650  1.389125  3.293531 22.843096 19.757686 20.881983 22.197212 
           27        28        29        30        31        32        33        34 
    12.984694 16.040397 23.032068  2.126259 21.672285  6.786826  3.229593  1.476258 
           35        36        37 
     2.006966  3.429543 13.411156 

    $qual.FRic
    [1] 0.6119752

    $FEve
            1         2         3         4         5         6         7         8 
    0.5527696 0.6205414 0.1835346 0.5392607 0.4797614 0.7112545 0.5008017 0.5147676 
            9        10        11        12        13        14        16        17 
    0.4697397 0.5858825 0.6019819 0.4561421 0.5871915 0.4197760 0.7158875        NA 
           18        20        21        22        23        24        25        26 
    0.5558750 0.6656591 0.3717836 0.6803818 0.4776834 0.3612547 0.5688332 0.3282627 
           27        28        29        30        31        32        33        34 
    0.5297058 0.6105605 0.3056597 0.1767246 0.3715590 0.4955154 0.4435384 0.7577238 
           35        36        37 
    0.8493609 0.7437116 0.5843374 

    $FDiv
            1         2         3         4         5         6         7         8 
    0.4275734 0.7576472 0.3403071 0.6708474 0.7576817 0.8009163 0.6830569 0.8566349 
            9        10        11        12        13        14        16        17 
    0.8219154 0.5626252 0.4542448 0.9476869 0.4391252 0.7977192 0.6720104        NA 
           18        20        21        22        23        24        25        26 
    0.4578325 0.6339615 0.8572203 0.3455668 0.4789090 0.8120384 0.9365833 0.7368477 
           27        28        29        30        31        32        33        34 
    0.6719905 0.5730075 0.7658393 0.9231344 0.5800060 0.5287667 0.7797352 0.8443041 
           35        36        37 
    0.6475469 0.5662178 0.8739552 

    $FDis
            1         2         3         4         5         6         7         8 
    1.2047296 2.4991438 1.4605374 2.7948561 3.5681467 3.4161316 2.5651408 1.3673036 
            9        10        11        12        13        14        16        17 
    3.0212500 1.7150340 2.0765955 1.5598478 1.6862304 3.3763863 2.8282395 0.2359687 
           18        20        21        22        23        24        25        26 
    2.4396054 2.7552969 1.3509216 0.2553377 2.6459076 3.2409993 3.9082336 2.9856868 
           27        28        29        30        31        32        33        34 
    2.2487713 3.1009571 3.8050133 0.7210464 2.9318649 2.0905113 2.4191609 2.4326357 
           35        36        37 
    1.7708547 1.8250795 3.8445610 

    $RaoQ
             1          2          3          4          5          6          7 
     2.2168440  6.8803560  3.6691363 10.0572766 14.1620384 12.2411568  8.7644183 
             8          9         10         11         12         13         14 
     2.1486340 12.3215766  4.5779525  4.5480960  4.4785042  4.2110505 11.8313314 
            16         17         18         20         21         22         23 
     9.1650995  0.8225926  7.9223692  9.5698090  3.2384694  0.7195039  8.7954228 
            24         25         26         27         28         29         30 
    11.2403957 16.7692616 10.1525211  5.9080066 11.3147043 14.6719179  1.3419059 
            31         32         33         34         35         36         37 
    12.0803689  4.5515991  6.1779375  6.1601513  3.8709996  3.5311548 15.2981826 

    $CWM
           logM       OgSf      OgSh      OgPo      EySz       GrLg      GtLg
    1  2.810922 0.13480252 0.6771707 0.3006968 0.3706319 0.18877182 1.7995287
    2  2.250608 0.13211969 1.0202038 0.6867851 0.3229639 0.15587811 0.9820053
    3  2.698593 0.14722998 0.6840523 0.3246634 0.3623041 0.19898030 1.8068631
    4  2.419843 0.13200987 0.9457032 0.5755633 0.3131709 0.16274987 1.5292621
    5  5.371259 0.07746477 0.4324446 0.2219573 0.2517830 0.05185683 1.1508160
    6  2.543740 0.19471083 1.0972964 0.5070951 0.3584135 0.17776493 0.9347627
    7  2.757889 0.11972182 0.6790955 0.4163527 0.3330323 0.14328431 1.6403955
    8  2.817801 0.18012179 1.0584006 0.3700577 0.4365737 0.32991987 0.8447564
    9  3.066823 0.07576066 0.6780465 0.6890067 0.2435946 0.03346611 1.5154305
    10 2.531606 0.16109737 1.0297711 0.4947949 0.3885412 0.27151836 0.9275734
    11 2.989857 0.15116934 0.8226517 0.3566820 0.3203624 0.13447674 1.3649907
    12 2.292542 0.07242156 0.8214267 0.9144299 0.2385234 0.03827063 1.0539688
    13 3.805176 0.07073099 0.5815865 0.4372698 0.2255552 0.06674627 2.2130539
    14 2.566774 0.11150094 0.7758124 0.7310652 0.2668763 0.08837339 1.3106097
    16 2.611088 0.18548148 1.0938652 0.5334202 0.4025985 0.22688593 0.8780894
    17 3.095674 0.17095094 0.5176147 0.3572612 0.2621563 0.12579597 2.0601439
    18 2.334988 0.13648262 0.8981299 0.5457173 0.3212885 0.16936756 1.2277075
    20 2.485493 0.16282775 1.1985232 0.6233908 0.3909599 0.23672175 1.4073014
    21 3.110047 0.09745020 0.8600665 0.2896437 0.3323186 0.06238626 1.2141750
    22 4.277756 0.04716312 0.5177283 0.4286876 0.1895265 0.01423027 2.4350981
    23 3.453950 0.13988065 0.6239349 0.3585067 0.2842775 0.11374332 2.1109530
    24 5.436597 0.06749048 0.3702459 0.2121384 0.2173431 0.03214832 1.5215119
    25 3.991279 0.06277989 0.5845203 0.2656957 0.3301850 0.02518071 3.1275059
    26 3.770207 0.08435024 0.7449825 0.3722980 0.3127523 0.07832240 1.6376703
    27 2.966331 0.12615617 0.8459596 0.3962025 0.3429643 0.10751176 1.4950539
    28 3.127626 0.11824390 0.8933740 0.2966925 0.3957595 0.15475109 1.6028267
    29 4.345225 0.08375400 0.5446897 0.1482614 0.4375632 0.07262769 1.1419237
    30 2.754517 0.13222369 0.6534612 0.2786159 0.3701943 0.17864422 1.8921965
    31 3.387559 0.10473574 0.6572379 0.2562680 0.3673408 0.12326673 2.4252159
    32 2.845532 0.16584533 0.8019931 0.3551773 0.3361509 0.13825008 1.5533088
    33 3.101101 0.11559227 0.6771191 0.3202907 0.2967508 0.08904778 2.6395409
    34 2.912819 0.11450943 0.8022714 0.3718820 0.3475071 0.12261743 2.4788837
    35 2.262551 0.15740120 1.1909760 0.4678982 0.3904731 0.19092216 1.1696707
    36 2.593510 0.14917617 0.9098400 0.3793908 0.4044929 0.20208608 1.4775952
    37 3.840524 0.14002084 0.5816246 0.3303813 0.3043598 0.03951693 2.6229533
            EyPo      BdSh     BdSf      PfPo      PfSh      CpHt      CfSh
    1  0.6454785 1.3837435 1.846477 0.7405726 3.0871447 3.1922172 3.1902255
    2  0.7839915 1.4736104 1.980224 0.3959136 2.7901043 1.1039222 0.6002910
    3  0.6439617 1.3890381 1.903757 0.7258417 3.2065427 3.1916856 3.2196030
    4  0.7423551 1.8204611 2.054353 0.4695810 3.0312044 1.5076053 0.9306047
    5  0.5587585 0.5241182 1.511827 0.2119654 2.8238894 0.8085665 0.8527548
    6  0.7538012 1.7436864 2.019001 0.4823319 3.0262828 1.3631166 1.1298615
    7  0.7200227 1.1387664 1.870306 0.5887982 2.5538089 2.5733370 2.5136715
    8  0.6179936 2.3718686 1.983426 0.6591474 4.5008365 2.1342019 2.0299551
    9  0.9016618 0.6890811 1.837743 0.2360987 1.1726146 1.2017128 0.6955081
    10 0.6758079 2.0221061 1.989794 0.5690833 3.9904058 1.7960099 1.4205145
    11 0.6741250 1.5979443 1.812692 0.6736275 3.3683035 2.4205958 2.2788482
    12 0.9434942 0.4892232 1.960132 0.1049149 0.7039291 0.3439969 0.2273322
    13 0.7947879 1.2269381 1.736283 0.5006076 1.6280340 2.0599637 1.3509113
    14 0.8412389 0.7995143 1.883121 0.3255257 1.7613270 1.3516882 1.3980859
    16 0.7138489 1.6984805 1.952132 0.5167852 2.9586835 1.6864405 1.5747111
    17 0.6530783 1.0067083 1.663131 0.7921215 3.4200780 3.5301494 3.9822287
    18 0.7195386 1.5650543 2.026435 0.5263937 3.5867782 1.8547288 1.4402817
    20 0.7272624 1.8455082 1.917701 0.4799087 3.3456808 1.5703015 1.2383094
    21 0.6901709 1.9451611 1.920790 0.5493814 3.9827680 1.9387501 0.9379259
    22 0.8370289 1.1521497 1.653076 0.4459159 1.0227461 1.9058315 1.0697171
    23 0.6945001 1.4940887 1.720549 0.6542887 2.9443193 2.9281595 2.7686658
    24 0.6018347 0.5962164 1.503629 0.2689786 2.5737263 1.1600159 0.9790514
    25 0.7255901 2.3148095 1.612107 0.4939625 4.5918055 2.2760689 1.8904349
    26 0.7673004 1.6090436 1.767354 0.4724929 2.8602544 1.8927069 1.2594450
    27 0.6912626 1.7869508 1.906887 0.5726479 3.5078299 2.3121792 1.5727252
    28 0.7002863 1.6502292 1.908398 0.5733304 3.5527159 2.2597188 1.9637468
    29 0.8459403 0.6302191 1.635418 0.3355124 2.9972904 1.5087971 1.4769007
    30 0.6419205 1.3006261 1.855143 0.7531558 2.8954447 3.3431530 3.3549514
    31 0.6901948 1.6586167 1.810164 0.6288749 2.7448331 2.7802355 2.8005014
    32 0.6442252 1.6627702 1.867536 0.7048930 3.9328188 2.9080820 2.7793807
    33 0.6461884 2.2467639 1.724622 0.6994508 5.2712006 3.0650363 3.0513320
    34 0.6518743 2.4673474 1.744200 0.6527772 5.3544811 2.7600694 2.4484116
    35 0.6515449 2.2163353 2.285713 0.6134671 3.0217844 2.1723892 1.5619341
    36 0.6355993 1.8116954 1.909275 0.7010598 3.2784308 2.7471617 2.5075713
    37 0.7397113 1.9040668 1.650157 0.4963649 4.1764485 2.3017962 1.7472864
            FsRt      FsSf
    1  0.6786081 1.4706756
    2  0.3989552 0.8717226
    3  0.6451029 1.5045288
    4  0.7210032 1.1480575
    5  0.1574197 2.2189475
    6  0.8367683 1.8678763
    7  0.6923076 1.2779086
    8  0.5227244 2.0672436
    9  0.9692431 0.9162313
    10 0.5306229 1.5812523
    11 0.8666612 1.7109690
    12 0.1102597 0.2392800
    13 1.0484811 1.4215858
    14 0.2893354 0.8056580
    16 0.5374301 1.6262904
    17 0.7675520 1.6196231
    18 0.6740301 1.2436875
    20 0.3114627 1.2186098
    21 0.6223611 1.6503378
    22 1.1626070 1.4786859
    23 1.1391629 1.7330846
    24 0.4612856 2.2494376
    25 1.1299265 2.0968494
    26 0.8054348 2.1040984
    27 0.6609251 1.5962673
    28 0.5581453 2.1710778
    29 0.3447777 3.7454438
    30 0.6575553 1.4304117
    31 0.5530585 1.9774011
    32 0.6836482 1.6398036
    33 0.8871569 1.5958215
    34 0.8009446 1.4433974
    35 0.7175509 1.8118982
    36 0.6041335 1.5782757
    37 1.8181802 1.9045159

</div>

</div>

**Interpretation:**

The functional diversity indices show that communities differ in the range and distribution of ecological strategies they support. Communities with higher Functional Richness (FRic), Functional Dispersion (FDis) and Rao’s Quadratic Entropy (RaoQ) contain a wider variety of functional traits, whereas communities with lower values are functionally more similar.

These differences suggest that environmental conditions influence not only which species occur within communities but also the functional traits they possess. This provides further evidence for environmental filtering, indicating that local habitats select for particular combinations of ecological characteristics rather than species identities alone.

Although functional diversity demonstrates that communities differ in their ecological strategies, it does not directly relate environmental gradients to species traits. The next analysis uses RLQ analysis to integrate environmental variables, community composition and functional traits, allowing these relationships to be examined simultaneously.

</div>

<div id="rlq-analysis" class="section level3" number="4.2.5">

### <span class="header-section-number">4.2.5</span> RLQ Analysis

*How are environmental gradients linked to species functional traits through community composition?*

While functional diversity summarises variation in ecological strategies within communities, it does not directly relate environmental conditions to species traits. RLQ analysis integrates environmental variables, community composition and functional traits to investigate whether changes in environmental conditions are associated with predictable changes in species’ functional characteristics.

RLQ analysis combines three datasets: the environmental variables (R table), the community matrix (L table) and the functional trait matrix (Q table). This allows environmental filtering to be assessed by determining whether environmental gradients are associated with differences in species traits across the metacommunity.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 6. RLQ analysis
############################################################

# Ensure traits match species order
traits.rlq <- traits1[colnames(comm), ]

# L table (community)
L <- dudi.coa(
  comm,
  scannf = FALSE,
  nf = 2
)

# R table (environment)
R <- dudi.pca(
  env.std,
  row.w = L$lw,
  scannf = FALSE,
  nf = 2
)

# Q table (traits)
Q <- dudi.hillsmith(
  traits.rlq,
  row.w = L$cw,
  scannf = FALSE,
  nf = 2
)

# RLQ
rlq.result <- rlq(
  R,
  L,
  Q,
  scannf = FALSE,
  nf = 2
)

summary(rlq.result)

plot(rlq.result)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    RLQ analysis

    Class: rlq dudi
    Call: rlq(dudiR = R, dudiL = L, dudiQ = Q, scannf = FALSE, nf = 2)

    Total inertia: 1.812

    Eigenvalues:
        Ax1     Ax2     Ax3     Ax4 
    0.87803 0.82926 0.09406 0.01109 

    Projected inertia (%):
        Ax1     Ax2     Ax3     Ax4 
    48.4445 45.7540  5.1897  0.6118 

    Cumulative projected inertia (%):
        Ax1   Ax1:2   Ax1:3   Ax1:4 
      48.44   94.20   99.39  100.00 


    Eigenvalues decomposition:
            eig     covar      sdR      sdQ      corr
    1 0.8780255 0.9370301 1.355373 1.940718 0.3562314
    2 0.8292602 0.9106372 1.229214 1.682902 0.4402090

    Inertia & coinertia R (R):
        inertia      max     ratio
    1  1.837036 2.427672 0.7567067
    12 3.348003 3.537379 0.9464644

    Inertia & coinertia Q (Q):
        inertia      max     ratio
    1  3.766388 6.011989 0.6264796
    12 6.598549 8.869437 0.7439648

    Correlation L (L):
           corr       max     ratio
    1 0.3562314 0.8786425 0.4054338
    2 0.4402090 0.8631605 0.5099967

</div>

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.10.** RLQ ordination illustrating the relationships among environmental variables, community composition and functional traits. The analysis links environmental gradients to the functional characteristics of species through their occurrence within local communities.

**Interpretation:**

The RLQ analysis revealed a strong association between environmental conditions, community composition and species functional traits. The first two RLQ axes explained most of the joint variation in the dataset, indicating that the primary environmental gradients are closely linked to differences in functional trait composition across the metacommunity.

These results support the environmental filtering hypothesis by showing that species are distributed according to suites of functional traits associated with particular environmental conditions. The RLQ analysis therefore complements the dbRDA and functional diversity analyses by demonstrating that environmental gradients influence community assembly through species’ ecological characteristics.

Although RLQ demonstrates an overall relationship between environmental conditions and functional traits, it does not identify which specific trait–environment relationships are responsible for these patterns. The next analysis uses Fourth-corner analysis to test the association between individual functional traits and environmental variables.

</div>

<div id="fourth-corner-analysis" class="section level3" number="4.2.6">

### <span class="header-section-number">4.2.6</span> Fourth-corner Analysis

*Which functional traits are directly associated with environmental gradients?*

While RLQ analysis identifies overall relationships between environmental conditions and functional traits, it does not determine which individual trait–environment relationships drive these patterns. Fourth-corner analysis directly tests whether specific functional traits are significantly associated with particular environmental variables.

Fourth-corner analysis links the environmental matrix, community matrix and functional trait matrix to identify significant trait–environment relationships. This provides a more detailed assessment of how environmental filtering acts on species’ functional characteristics.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 7. Fourth-corner analysis
############################################################

# Ensure trait matrix matches community matrix
traits.rlq <- traits1[colnames(comm), ]

# Fourth-corner analysis
fourth <- fourthcorner(
  tabR = env.std,
  tabL = comm,
  tabQ = traits.rlq,
  modeltype = 6,
  p.adjust.method.G = "holm",
  p.adjust.method.D = "holm",
  nrepet = 999
)

# Summary
summary(fourth)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    Fourth-corner Statistics
    ------------------------
    Permutation method  Comb. 2 and 4  ( 999  permutations)

    Adjustment method for multiple comparisons:   holm 
                  Test Stat          Obs     Std.Obs     Alter Pvalue Pvalue.adj  
    1     Depth / logM    r -0.216789193 -1.16447523 two-sided  0.276      1.000  
    2    Secchi / logM    r  0.097028434  0.54449233 two-sided  0.619      1.000  
    3  Salinity / logM    r  0.063410219  0.31403908 two-sided  0.772      1.000  
    4    Oxygen / logM    r  0.271150461  1.43614806 two-sided  0.154      1.000  
    5     Depth / OgSf    r  0.177299675  1.38927246 two-sided  0.181      1.000  
    6    Secchi / OgSf    r -0.144725629 -1.18717942 two-sided  0.267      1.000  
    7  Salinity / OgSf    r -0.014177511 -0.07536426 two-sided  0.944      1.000  
    8    Oxygen / OgSf    r -0.234436226 -1.86458482 two-sided  0.051      1.000  
    9     Depth / OgSh    r  0.262507596  2.02279343 two-sided  0.028      1.000  
    10   Secchi / OgSh    r  0.050730826  0.34205056 two-sided  0.731      1.000  
    11 Salinity / OgSh    r  0.028313474  0.30535014 two-sided  0.807      1.000  
    12   Oxygen / OgSh    r -0.179949110 -1.34876931 two-sided  0.195      1.000  
    13    Depth / OgPo    r  0.328057573  2.30353433 two-sided  0.005      0.315  
    14   Secchi / OgPo    r  0.163655434  1.47374081 two-sided  0.148      1.000  
    15 Salinity / OgPo    r  0.250775797  1.12937682 two-sided  0.299      1.000  
    16   Oxygen / OgPo    r -0.229859882 -2.05709950 two-sided  0.041      1.000  
    17    Depth / EySz    r -0.093205539 -0.56094872 two-sided  0.615      1.000  
    18   Secchi / EySz    r -0.294365163 -1.77297030 two-sided  0.076      1.000  
    19 Salinity / EySz    r -0.310152359 -1.95160997 two-sided  0.048      1.000  
    20   Oxygen / EySz    r -0.027716040 -0.15478336 two-sided  0.890      1.000  
    21    Depth / GrLg    r  0.165380369  0.98977626 two-sided  0.362      1.000  
    22   Secchi / GrLg    r -0.270724139 -1.61957579 two-sided  0.108      1.000  
    23 Salinity / GrLg    r -0.089266439 -0.54962107 two-sided  0.616      1.000  
    24   Oxygen / GrLg    r -0.296328832 -1.75445862 two-sided  0.077      1.000  
    25    Depth / GtLg    r -0.184969287 -1.58701126 two-sided  0.097      1.000  
    26   Secchi / GtLg    r  0.106754514  0.89400794 two-sided  0.394      1.000  
    27 Salinity / GtLg    r -0.186868734 -1.76241572 two-sided  0.079      1.000  
    28   Oxygen / GtLg    r  0.162843718  1.40893586 two-sided  0.166      1.000  
    29    Depth / EyPo    r  0.055002121  0.41706696 two-sided  0.702      1.000  
    30   Secchi / EyPo    r  0.118651720  0.94819888 two-sided  0.360      1.000  
    31 Salinity / EyPo    r  0.016007272  0.17133236 two-sided  0.873      1.000  
    32   Oxygen / EyPo    r  0.016189106  0.19510542 two-sided  0.855      1.000  
    33    Depth / BdSh    r  0.034176850  0.25607824 two-sided  0.802      1.000  
    34   Secchi / BdSh    r  0.155156481  1.22811406 two-sided  0.240      1.000  
    35 Salinity / BdSh    r -0.088546350 -0.75314733 two-sided  0.484      1.000  
    36   Oxygen / BdSh    r  0.031145591  0.29650996 two-sided  0.783      1.000  
    37    Depth / BdSf    r  0.221057468  1.52243164 two-sided  0.137      1.000  
    38   Secchi / BdSf    r -0.085614730 -0.58866589 two-sided  0.586      1.000  
    39 Salinity / BdSf    r -0.049802556 -0.30527993 two-sided  0.794      1.000  
    40   Oxygen / BdSf    r -0.214653222 -1.44080954 two-sided  0.155      1.000  
    41    Depth / PfPo    r  0.006944119  0.05324392 two-sided  0.956      1.000  
    42   Secchi / PfPo    r -0.151480406 -0.86312022 two-sided  0.421      1.000  
    43 Salinity / PfPo    r -0.210538859 -1.27181349 two-sided  0.223      1.000  
    44   Oxygen / PfPo    r -0.148471142 -0.85627832 two-sided  0.443      1.000  
    45    Depth / PfSh    r -0.019617088 -0.27128989 two-sided  0.791      1.000  
    46   Secchi / PfSh    r  0.078906631  0.78732312 two-sided  0.426      1.000  
    47 Salinity / PfSh    r -0.004511844 -0.01897986 two-sided  0.977      1.000  
    48   Oxygen / PfSh    r  0.049854948  0.54801275 two-sided  0.586      1.000  
    49    Depth / CpHt    r -0.069480883 -0.38395384 two-sided  0.730      1.000  
    50   Secchi / CpHt    r -0.179493241 -1.02819261 two-sided  0.329      1.000  
    51 Salinity / CpHt    r -0.251917803 -1.53899826 two-sided  0.126      1.000  
    52   Oxygen / CpHt    r -0.108012021 -0.63643345 two-sided  0.583      1.000  
    53    Depth / CfSh    r -0.114372812 -0.63788369 two-sided  0.548      1.000  
    54   Secchi / CfSh    r -0.282499892 -1.62530483 two-sided  0.112      1.000  
    55 Salinity / CfSh    r -0.281187163 -1.71941314 two-sided  0.083      1.000  
    56   Oxygen / CfSh    r -0.099526081 -0.59330692 two-sided  0.595      1.000  
    57    Depth / FsRt    r  0.055014982  0.60857611 two-sided  0.567      1.000  
    58   Secchi / FsRt    r  0.199737980  2.16307443 two-sided  0.026      1.000  
    59 Salinity / FsRt    r  0.058166011  0.72228029 two-sided  0.478      1.000  
    60   Oxygen / FsRt    r -0.020588076 -0.21188380 two-sided  0.848      1.000  
    61    Depth / FsSf    r -0.223249167 -2.02633016 two-sided  0.032      1.000  
    62   Secchi / FsSf    r -0.066578100 -0.56846116 two-sided  0.607      1.000  
    63 Salinity / FsSf    r -0.131070675 -1.25796632 two-sided  0.192      1.000  
    64   Oxygen / FsSf    r  0.230752458  2.04378140 two-sided  0.041      1.000  

    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

</div>

</div>

**Interpretation:**

The fourth-corner analysis identified several apparent trait–environment relationships before correcting for multiple comparisons. However, after applying the Holm correction, none of these relationships remained statistically significant. This suggests that no single functional trait is consistently associated with the measured environmental gradients across the metacommunity.

Rather than acting on individual traits, environmental filtering is likely to influence combinations of interacting functional traits. This interpretation is consistent with the RLQ and dbRDA analyses, which demonstrated significant relationships between environmental conditions, community composition and overall functional trait structure.

</div>

<div id="conceptual-synthesis-environmental-filtering" class="section level3" number="4.2.7">

### <span class="header-section-number">4.2.7</span> Conceptual Synthesis: Environmental Filtering

Environmental filtering explains how local abiotic conditions determine which species from the regional pool are able to establish and persist within local communities. The analyses presented in this section provide multiple lines of evidence supporting this process. The correlation matrix identified the major environmental gradients, dbRDA showed that these gradients contribute to variation in community composition, functional diversity demonstrated differences in ecological strategies among communities, and RLQ linked these environmental gradients to functional trait composition. Although the fourth-corner analysis did not identify significant individual trait–environment relationships after multiple-testing correction, the combined results indicate that environmental filtering acts through suites of interacting traits rather than isolated characteristics.

Together, these analyses demonstrate that environmental conditions play an important role in structuring the fish metacommunity by influencing both species composition and functional diversity.

<div class="callout callout-style-default callout-important callout-titled" title="Key Take-home Message">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Key Take-home Message

</div>

</div>

<div class="callout-body-container callout-body">

Environmental filtering influences community assembly by selecting species with functional traits suited to local environmental conditions. In this metacommunity, environmental gradients explain variation in both species composition and functional trait structure, indicating that communities are assembled through deterministic ecological processes rather than by chance alone.

</div>

</div>

</div>

</div>

<div id="quantifying-dispersal" class="section level2" number="4.3">

## <span class="header-section-number">4.3</span> Quantifying Dispersal

**Ecological Question**

*How does the spatial arrangement of communities influence patterns of species composition across the metacommunity?*

Unlike environmental filtering, which focuses on whether species can survive under local environmental conditions, dispersal examines whether species are able to reach suitable habitats. Even when habitats are environmentally favourable, limited movement among sites can restrict colonisation, resulting in spatial patterns in community composition. Analysing the spatial relationships among sampling locations therefore allows ecologists to determine whether dispersal contributes to metacommunity assembly independently of environmental filtering.

*How can spatial information be prepared for analysing dispersal processes?*

To investigate dispersal, the geographic location of each sampling site must first be incorporated into the analysis. Spatial coordinates provide the basis for calculating distances among communities and identifying spatial patterns that may influence species distributions.

The spatial coordinates are imported and checked to ensure they correspond to the community matrix. These coordinates will be used throughout the dispersal analyses to calculate geographic distances, generate spatial variables and evaluate the influence of dispersal on community assembly.

<div id="geographic-distance" class="section level3" number="4.3.1">

### <span class="header-section-number">4.3.1</span> Geographic Distance

*How far apart are the local communities, and what does this imply for dispersal?*

Before investigating the influence of dispersal on metacommunity structure, it is first necessary to quantify the spatial relationships among sampling sites. Geographic distance provides a measure of the physical separation between communities and forms the foundation for analysing spatial processes. In general, dispersal is expected to occur more readily between nearby communities than between those separated by greater distances. Consequently, if dispersal limitation is an important assembly process, geographically proximate communities should exhibit greater ecological similarity than communities located farther apart. Consequently, ecologists frequently rely on indirect spatial analyses to infer dispersal processes within metacommunities (Jacobson & Peres-Neto, 2010).

The purpose of this analysis is to calculate the pairwise geographic distances among all sampling sites. Although the distance matrix does not directly test for dispersal limitation, it provides the spatial framework required to generate spatial predictors in subsequent analyses. These predictors will later be used to determine whether spatial structure contributes to patterns of community composition independently of environmental filtering.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 8. Geographic distances
############################################################

geo.dist <- dist(coord)

geo.dist
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

                1          2          3          4          5          6          7
    2    6389.001                                                                  
    3    8641.520   5677.693                                                       
    4   13208.510   7495.544   6069.268                                            
    5   16759.794  12324.289   8151.971   5862.875                                 
    6   20958.737  15316.280  12980.518   7839.203   6200.485                      
    7   24653.674  19674.369  16116.238  12304.476   7999.768   5512.365           
    8   28825.395  23270.454  20587.234  15787.540  12810.196   7954.302   5460.940
    9   33003.429  27930.034  24452.606  20477.044  16311.345  12947.884   8349.778
    10  37502.900  32014.900  29141.734  24528.036  21140.630  16699.576  13197.588
    11  42476.205  37404.478  33900.342  29935.482  25749.065  22291.217  17829.352
    12  48077.883  42756.181  39584.617  35260.660  31463.750  27467.349  23470.399
    13  53146.421  48086.305  44553.945  40610.991  36402.505  32925.198  28510.519
    14  58466.062  53210.892  49928.681  45715.746  41785.097  37932.960  33819.406
    16  70119.849  64843.984  61579.725  57348.463  53434.162  49550.721  45472.509
    17  74700.272  69654.826  66092.288  62174.078  57945.400  54456.140  50079.793
    18  78682.124  73454.519  70119.856  65959.265  61969.194  58171.351  54028.565
    20  85458.497  80388.952  76852.051  72903.477  68704.667  65166.301  60832.408
    21  95394.383  90379.203  86775.819  82898.864  78635.238  75176.743  70792.505
    22 101295.255  96427.006  92658.842  88969.574  84543.704  81307.095  76780.708
    23  90337.028  85533.927  81697.466  78093.192  73594.773  70473.004  65870.273
    24  82636.708  77890.660  73995.617  70467.005  65905.442  62888.442  58218.434
    25  95160.063  90601.829  86524.676  83226.844  78496.747  75737.527  70936.956
    26  89159.671  84774.304  80542.421  77463.661  72581.323  70096.868  65159.187
    27  86040.801  81962.598  77483.956  74786.267  69672.143  67652.524  62522.744
    28  90009.219  85882.976  81441.790  78680.057  73604.354  71496.330  66402.535
    29  91283.185  87408.292  82786.288  80325.707  75089.851  73333.665  68118.835
    30  95792.576  91934.153  87302.495  84854.603  79614.917  77859.866  72647.640
    31  98685.386  94845.216  90202.200  87771.810  82524.948  80782.237  75567.859
    32 103087.805  99140.592  94572.119  92009.332  86831.530  84923.948  79766.122
    33 107800.433 104015.503  99338.639  96961.437  91692.826  89989.286  84767.442
    34 113370.291 109475.766 104873.344 102361.758  97161.628  95292.104  90126.219
    35 118621.115 114842.235 110164.083 107782.206 102519.776 100789.320  95580.297
    36 123894.503 120051.015 115416.394 112954.767 107732.923 105902.757 100727.724
    37 124811.317 120803.972 116282.578 113629.205 108507.091 106456.514 101359.085
                8          9         10         11         12         13         14
    2                                                                              
    3                                                                              
    4                                                                              
    5                                                                              
    6                                                                              
    7                                                                              
    8                                                                              
    9    5852.768                                                                  
    10   8745.822   5706.344                                                       
    11  14592.632   9488.420   6748.248                                            
    12  19538.714  15173.449  10842.269   6184.223                                 
    13  25113.627  20174.974  16636.703  10686.911   6197.248                      
    14  30008.369  25476.078  21302.990  16076.176  10469.759   5904.185           
    16  41611.139  37127.611  32879.256  27704.011  22087.803  17204.736  11653.789
    17  46584.455  41748.439  37943.574  32260.593  27107.731  21573.684  16721.271
    18  50237.681  45678.991  41511.750  36220.498  30704.020  25610.972  20243.772
    20  57272.799  52496.394  48596.049  43007.983  37754.055  32322.369  27302.528
    21  67293.862  62467.615  58627.133  52981.237  47784.877  42295.078  37339.210
    22  73476.408  68497.604  64872.818  59036.777  54046.471  48382.046  43666.370
    23  62687.866  57616.209  54150.380  48183.394  43366.315  37574.297  33104.633
    24  55151.417  49996.447  46688.581  40600.850  35974.329  30062.710  25902.778
    25  68080.962  62803.476  59707.391  53494.472  49056.615  43071.186  39052.669
    26  62575.908  57149.886  54393.938  47995.679  43956.122  37837.223  34374.327
    27  60391.592  54775.274  52570.794  45963.941  42581.863  36384.786  33797.950
    28  64171.716  58594.484  56250.412  49690.007  46113.192  39925.226  37027.614
    29  66215.657  60530.577  58563.865  51902.312  48749.236  42555.359  40146.681
    30  70728.439  65050.554  63042.038  56392.808  53157.415  46960.547  44389.015
    31  73650.338  67973.443  65953.730  59308.855  56041.909  49844.672  47201.507
    32  77681.608  72059.467  69830.449  63243.725  59719.542  53531.744  50547.563
    33  82859.516  77183.802  75140.277  68505.832  65164.445  58968.467  56155.768
    34  88051.717  82430.412  80181.101  73605.732  70018.348  63838.325  60711.061
    35  93622.416  87965.401  85833.986  79226.745  75749.231  69561.002  66528.956
    36  98668.634  93045.561  90788.471  84219.662  80593.195  74419.486  71198.944
    37  99097.786  93547.925  91062.825  84572.542  80704.129  74567.176  71075.446
               16         17         18         20         21         22         23
    2                                                                              
    3                                                                              
    4                                                                              
    5                                                                              
    6                                                                              
    7                                                                              
    8                                                                              
    9                                                                              
    10                                                                             
    11                                                                             
    12                                                                             
    13                                                                             
    14                                                                             
    16                                                                             
    17   6348.222                                                                  
    18   8641.522   5269.405                                                       
    20  15985.682  10760.193   7617.864                                            
    21  25990.530  20724.785  17460.493  10039.427                                 
    22  32541.078  26948.197  24150.798  16574.825   6913.450                      
    23  22473.075  16461.379  14769.682   7524.722   6819.537  11024.732           
    24  16151.216   9812.071  10334.158   6983.046  13828.343  18781.521   7756.790
    25  29009.371  22777.657  21726.321  14621.600  10045.447   9124.680   7097.528
    26  25495.848  19156.500  19674.366  14156.676  14392.408  15655.806   8291.658
    27  26681.425  20657.592  22628.536  18696.563  20559.810  21718.909  14156.676
    28  29102.992  22865.070  24031.188  18944.954  18751.452  18790.107  13054.826
    29  33010.768  26903.296  28461.709  23614.194  23072.970  22271.321  17670.422
    30  36765.478  30535.551  31569.241  25990.524  23689.161  21536.297  19281.253
    31  39351.311  33077.237  33846.210  27927.855  24675.732  21718.911  20913.518
    32  41996.751  35654.759  35776.048  29229.413  24313.776  20123.973  21826.256
    33  47784.883  41447.634  41593.699  34988.445  29576.181  24736.906  27542.803
    34  51733.160  45389.584  44959.137  37922.336  31182.641  25407.177  30398.646
    35  57631.594  51286.814  50852.394  43779.733  36780.818  30748.607  36259.968
    36  61950.668  55629.067  54848.796  47577.810  39890.393  33467.245  40116.052
    37  61410.991  55144.480  53986.032  46559.798  38324.036  31670.577  39217.948
               24         25         26         27         28         29         30
    2                                                                              
    3                                                                              
    4                                                                              
    5                                                                              
    6                                                                              
    7                                                                              
    8                                                                              
    9                                                                              
    10                                                                             
    11                                                                             
    12                                                                             
    13                                                                             
    14                                                                             
    16                                                                             
    17                                                                             
    18                                                                             
    20                                                                             
    21                                                                             
    22                                                                             
    23                                                                             
    24                                                                             
    25  13153.234                                                                  
    26   9511.744   6825.442                                                       
    27  12465.808  12653.523   6236.144                                            
    28  13703.929   9688.065   4855.464   4034.994                                 
    29  18135.261  13463.619   9539.252   6401.963   4686.517                      
    30  21283.115  13652.366  11952.541  10607.425   7670.583   4529.085           
    31  23647.450  14687.601  14173.754  13466.621  10283.339   7449.360   2922.945
    32  25934.643  14916.297  16501.715  17290.025  13606.537  11850.953   7452.065
    33  31746.827  20545.111  22291.215  22592.010  19130.179  16656.069  12133.249
    34  35582.344  23307.581  26404.339  27660.347  23932.891  22087.803  17580.115
    35  41481.640  29177.665  32270.585  33264.937  29636.075  27461.474  22932.822
    36  45821.686  33128.364  36833.377  38277.215  34538.073  32643.080  28119.124
    37  45393.128  32410.156  36756.709  38844.703  34961.372  33585.041  29125.831
               31         32         33         34         35         36
    2                                                                   
    3                                                                   
    4                                                                   
    5                                                                   
    6                                                                   
    7                                                                   
    8                                                                   
    9                                                                   
    10                                                                  
    11                                                                  
    12                                                                  
    13                                                                  
    14                                                                  
    16                                                                  
    17                                                                  
    18                                                                  
    20                                                                  
    21                                                                  
    22                                                                  
    23                                                                  
    24                                                                  
    25                                                                  
    26                                                                  
    27                                                                  
    28                                                                  
    29                                                                  
    30                                                                  
    31                                                                  
    32   4826.316                                                       
    33   9210.393   5821.708                                            
    34  14704.056  10371.147   5988.907                                 
    35  20012.516  16029.738  10827.398   5904.185                      
    36  25214.519  20987.583  16121.242  10616.917   5453.565           
    37  26298.816  21740.240  17630.474  11686.594   8129.692   4064.846

</div>

</div>

**Interpretation:**

The geographic distance matrix quantifies the spatial separation among every pair of sampling sites within the fish metacommunity. These distances describe the spatial configuration of local communities and provide the basis for modelling potential dispersal pathways across the landscape. While the matrix itself does not indicate whether dispersal influences community composition, it establishes the spatial relationships necessary for subsequent analyses. If dispersal limitation is operating within the metacommunity, communities located closer together would be expected to exchange individuals more readily and therefore become more similar in species composition than communities separated by larger geographic distances. The geographic distance matrix therefore serves as the foundation for constructing spatial variables that represent dispersal processes across multiple spatial scales.

Geographic distance alone cannot distinguish whether spatial patterns influence community composition. To model spatial structure explicitly, the geographic distances are next decomposed into Moran’s Eigenvector Maps (MEMs). These spatial eigenvectors capture broad- and fine-scale spatial patterns and provide the variables needed to test whether dispersal contributes to metacommunity assembly independently of environmental selection.

</div>

<div id="morans-eigenvector-maps-mem" class="section level3" number="4.3.2">

### <span class="header-section-number">4.3.2</span> Moran’s Eigenvector Maps (MEM)

*Is there spatial structure within the metacommunity that may reflect dispersal limitation?*

While geographic distance describes the physical separation among sampling sites, it does not directly identify the spatial patterns that may influence community assembly. Species distributions often exhibit spatial autocorrelation because nearby communities are more likely to exchange individuals than distant communities. Moran’s Eigenvector Maps (MEM) provide a means of modelling these spatial patterns by decomposing the geographic arrangement of sampling sites into a series of orthogonal spatial variables representing both broad- and fine-scale spatial structure.

The purpose of this analysis is to generate Moran’s Eigenvector Maps (MEM) from the geographic coordinates of the sampling sites. These spatial eigenvectors summarise patterns of spatial autocorrelation at multiple scales and serve as predictor variables in subsequent analyses. By incorporating these spatial variables into multivariate models, ecologists can determine whether spatial structure—and therefore dispersal—contributes to community assembly independently of environmental selection.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 9. Moran's Eigenvector Maps (MEM)
############################################################

mem <- dbmem(as.matrix(coord))

mem
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    Orthobasis with 35 rows and 11 columns
    Only 6 rows and 4 columns are shown
           MEM1       MEM2       MEM3      MEM4
    1 0.5204991 0.08574082 -0.3535453 1.0827140
    2 0.7309936 0.12578200 -0.6058684 1.5296376
    3 0.9569370 0.16862296 -0.8798429 1.8883700
    4 1.1003021 0.19616948 -1.0777275 1.8548245
    5 1.2066911 0.21429518 -1.1920590 1.5284371
    6 1.2718382 0.22105065 -1.2016512 0.9275792

</div>

</div>

**Interpretation:**


The MEM variables describe the spatial structure of the study area but do not determine whether this structure influences community composition. The next analysis incorporates the MEM variables into a Spatial Distance-based Redundancy Analysis (Spatial dbRDA) to test whether spatial processes explain a significant proportion of the variation in fish community composition, providing direct evidence for the role of dispersal in metacommunity assembly.

</div>

<div id="spatial-distance-based-redundancy-analysis-spatial-dbrda" class="section level3" number="4.3.3">

### <span class="header-section-number">4.3.3</span> Spatial Distance-based Redundancy Analysis (Spatial dbRDA)

*Does spatial structure explain variation in community composition?*

While Moran’s Eigenvector Maps (MEM) identify patterns of spatial structure across the landscape, they do not determine whether these spatial patterns influence the composition of ecological communities. Spatial distance-based redundancy analysis (dbRDA) addresses this question by relating community composition directly to the spatial eigenvectors generated by the MEM analysis. If the model is statistically significant, it indicates that spatial structure contributes to community assembly and provides evidence consistent with dispersal-related processes.

The purpose of this analysis is to determine whether spatial variables derived from the MEM analysis explain variation in fish community composition. By constraining community composition using the MEM variables, the spatial dbRDA evaluates whether dispersal contributes to metacommunity structure independently of the measured environmental gradients.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 10. Spatial dbRDA
############################################################

space.dbRDA <- capscale(
  comm.hel ~ .,
  data = as.data.frame(mem),
  distance = "bray"
)

anova(space.dbRDA)

plot(space.dbRDA)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    Permutation test for capscale under reduced model
    Permutation: free
    Number of permutations: 999

    Model: capscale(formula = comm.hel ~ MEM1 + MEM2 + MEM3 + MEM4 + MEM5 + MEM6 + MEM7 + MEM8 + MEM9 + MEM10 + MEM11, data = as.data.frame(mem), distance = "bray")
             Df SumOfSqs      F Pr(>F)    
    Model    11   5.7983 1.8211  0.001 ***
    Residual 23   6.6572                  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

</div>

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.11.** Spatial distance-based redundancy analysis (dbRDA) illustrating the relationship between fish community composition and the Moran’s Eigenvector Maps (MEM). The ordination evaluates whether spatial structure contributes significantly to variation in community composition independently of measured environmental variables.

**Interpretation:**

The spatial distance-based redundancy analysis demonstrated that spatial structure explained a significant proportion of the variation in fish community composition across the metacommunity. The overall model was highly significant (Pseudo-F = 1.821, P = 0.001), indicating that the spatial eigenvectors collectively capture meaningful ecological patterns in species composition. The 11 MEM variables explained a substantial component of the observed variation, while the remaining variation was attributed to unexplained residual differences among communities.

From a metacommunity perspective, these findings provide strong evidence that dispersal-related processes contribute to community assembly. Communities located closer together are more likely to exchange individuals and therefore tend to be more similar than geographically distant communities. When considered alongside the significant environmental analyses presented previously, these results indicate that both environmental filtering and dispersal act simultaneously to structure the fish metacommunity. Environmental conditions determine which species are able to persist locally, whereas dispersal influences where those species occur across the landscape.

Although the spatial dbRDA demonstrates that dispersal contributes significantly to community composition, it does not quantify how much variation is explained uniquely by environmental selection, uniquely by dispersal, or jointly by both processes. Variation partitioning addresses this question by separating these components, providing a quantitative assessment of the relative importance of deterministic environmental filtering and spatial processes within the metacommunity.

</div>

<div id="variation-partitioning" class="section level3" number="4.3.4">

### <span class="header-section-number">4.3.4</span> Variation Partitioning

*How much variation in community composition is explained uniquely by environmental selection and dispersal?*

Environmental conditions and dispersal frequently influence ecological communities simultaneously, making it difficult to distinguish their individual contributions to community assembly. Variation partitioning provides a framework for separating these influences by dividing the explained variation into unique environmental, unique spatial and shared components, while also identifying the proportion of variation that remains unexplained.

The purpose of this analysis is to partition the variation in fish community composition into fractions explained uniquely by environmental variables, uniquely by spatial variables derived from the MEM analysis, jointly by both predictor sets, and by residual unexplained variation. This allows the relative importance of environmental filtering and dispersal to be quantified before considering the contribution of stochastic ecological drift.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 11. Variation partitioning
############################################################

vp <- varpart(
  comm.hel,
  env.std,
  mem
)

plot(vp)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output-display">

<div class="quarto-figure quarto-figure-center">

<figure class="figure">
</figure>

</div>

</div>

</div>

**Figure 4.12.** Variation partitioning diagram showing the proportions of variation in community composition uniquely explained by environmental variables, uniquely explained by spatial variables, jointly explained by both predictor sets, and the unexplained residual variation.

**Interpretation:**


Together, the environmental and spatial predictors explained 24.5% of the total variation in community composition, leaving 75.5% as unexplained residual variation. These results indicate that both environmental selection and dispersal are important drivers of metacommunity organisation, but they do not account for all observed ecological variation. The substantial residual component suggests that additional environmental variables, biotic interactions, historical factors and stochastic ecological processes also contribute to community assembly.

The variation partitioning analysis demonstrates that deterministic environmental filtering and dispersal together explain an important proportion of community variation, yet most of the variation remains unexplained. This residual variation provides the basis for investigating the final metacommunity process, ecological drift, which considers how stochastic demographic processes, local extinctions and random colonisation events contribute to patterns of community composition

</div>

<div id="conceptual-synthesis-dispersal" class="section level3" number="4.3.5">

### <span class="header-section-number">4.3.5</span> Conceptual Synthesis: Dispersal

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart LR

A[&quot;Geographic Coordinates&quot;]
--&gt; B[&quot;Geographic Distance Matrix&quot;]

B
--&gt; C[&quot;Moran&#39;s Eigenvector Maps&lt;br/&gt;(MEM)&quot;]

C
--&gt; D[&quot;Spatial dbRDA&quot;]

D
--&gt; E[&quot;Significant Spatial Structure&lt;br/&gt;Pseudo-F = 1.821&lt;br/&gt;P = 0.001&quot;]

C
--&gt; F[&quot;Variation Partitioning&quot;]

G[&quot;Environmental Variables&quot;]
--&gt; F

F
--&gt; H[&quot;Unique Spatial Fraction&lt;br/&gt;10.8%&quot;]

F
--&gt; I[&quot;Shared Fraction&lt;br/&gt;6.7%&quot;]

F
--&gt; J[&quot;Unique Environmental Fraction&lt;br/&gt;7.0%&quot;]

H
--&gt; K[&quot;Evidence for Dispersal&quot;]

I
--&gt; K

E
--&gt; K

K
--&gt; L[&quot;Dispersal contributes to&lt;br/&gt;metacommunity assembly&quot;]</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 4.13.** Conceptual synthesis of the dispersal analyses. Geographic distances were converted into Moran’s Eigenvector Maps (MEM), which captured spatial structure across the landscape. These spatial variables significantly explained community composition in the spatial dbRDA and accounted for an independent proportion of variation in the variation partitioning analysis, demonstrating that dispersal contributes to metacommunity assembly.

<div class="callout callout-style-default callout-important callout-titled" title="Key Take-home Message">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Key Take-home Message

</div>

</div>

<div class="callout-body-container callout-body">

Dispersal influences metacommunity assembly by determining how species move among local communities. The MEM analysis identified spatial structure across the landscape, the spatial dbRDA showed that this structure significantly explained community composition (Pseudo-F = 1.821, P = 0.001), and variation partitioning demonstrated that spatial processes uniquely explained 10.8% of community variation. Together, these analyses provide strong evidence that dispersal operates alongside environmental filtering to structure the fish metacommunity.

</div>

</div>

</div>

</div>

<div id="quantifying-ecological-drift" class="section level2" number="4.4">

## <span class="header-section-number">4.4</span> Quantifying Ecological Drift

**Ecological Question**

*To what extent do stochastic processes contribute to variation in community composition across the metacommunity?*

The previous sections demonstrated that both environmental filtering and dispersal contribute significantly to patterns of fish community assembly. Environmental gradients determine which species are able to persist under local habitat conditions, while dispersal influences the movement of species among communities. However, these deterministic processes rarely explain all observed variation in community composition. Even after accounting for environmental and spatial effects, communities often differ because of random ecological events.

Ecological drift represents the stochastic component of metacommunity assembly. It arises through random fluctuations in species abundances, demographic stochasticity, local extinctions and colonisation events that occur independently of species’ ecological traits or environmental conditions. Drift is expected to be particularly important when species possess similar ecological characteristics or when environmental gradients are relatively weak, allowing chance events to influence which species persist within local communities.

In this section, ecological drift is investigated using two complementary analyses. First, **beta diversit** quantifies the extent of variation in species composition among local communities, providing an overall measure of community heterogeneity across the metacommunity. Second, the **residual variation** identified through variation partitioning is examined to determine how much community variation remains unexplained after accounting for environmental filtering and dispersal. Together, these analyses provide evidence for the contribution of stochastic processes to metacommunity assembly.

<div id="beta-diversity" class="section level3" number="4.4.1">

### <span class="header-section-number">4.4.1</span> Beta Diversity

*How much variation exists among local communities, and what does this reveal about community turnover?*

Beta diversity describes the extent to which species composition differs among local communities within a metacommunity. Communities exhibiting high beta diversity contain different combinations of species, whereas communities with low beta diversity are more compositionally similar. Although beta diversity itself does not distinguish between deterministic and stochastic assembly processes, it provides an important measure of community heterogeneity upon which these processes operate.

The purpose of this analysis is to quantify abundance-based beta diversity among the fish communities. Unlike presence–absence measures, abundance-based beta diversity incorporates differences in both species identities and their relative abundances, providing a more comprehensive assessment of variation in community composition. The results establish the degree of community turnover across the metacommunity before evaluating the relative importance of deterministic and stochastic assembly processes.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 12. Beta diversity
############################################################

beta.core <- betapart.core.abund(comm)

beta <- beta.multi.abund(beta.core)

beta

beta.pair.abund(beta.core)
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">

    $beta.BRAY.BAL
    [1] 0.891661

    $beta.BRAY.GRA
    [1] 0.0690839

    $beta.BRAY
    [1] 0.9607449

</div>

<div class="cell-output cell-output-stdout">

    $beta.bray.bal
                1          2          3          4          5          6          7
    2  0.63870527                                                                  
    3  0.08768324 0.63077786                                                       
    4  0.78587291 0.01361075 0.81385203                                            
    5  0.76089007 0.61599629 0.70217817 0.76969642                                 
    6  0.53527894 0.61998398 0.50164605 0.51730581 0.39327342                      
    7  0.28498800 0.47713247 0.14159154 0.69923007 0.62511191 0.50164605           
    8  0.28525641 0.62500000 0.26634615 0.62500000 0.40064103 0.54027778 0.28525641
    9  0.87755026 0.54318609 0.83426599 0.70122263 0.66314792 0.79927040 0.28810898
    10 0.36826250 0.32871615 0.27260108 0.32015122 0.73687941 0.50164605 0.39197850
    11 0.53725886 0.80380784 0.52389358 0.75592611 0.72405750 0.73805499 0.52389358
    12 0.90279409 0.51213545 0.80874437 0.38380799 0.42405559 0.79927040 0.07415835
    13 0.74971341 0.85594192 0.72411158 0.85594192 0.84256783 0.85514726 0.48001528
    14 0.61136960 0.64574568 0.54642875 0.51593823 0.24325595 0.44728179 0.12208583
    16 0.69135802 0.80395062 0.53195062 0.80395062 0.54913580 0.61698765 0.54913580
    17 0.01721897 0.98278103 0.45086095 0.98278103 0.72733918 0.96227422 0.43835283
    18 0.35603215 0.38293876 0.20040185 0.31794357 0.65934306 0.78103034 0.32266096
    20 0.84661383 0.70271035 0.72372797 0.70271035 0.61177553 0.58688495 0.61177553
    21 1.00000000 0.96336317 1.00000000 0.97613605 1.00000000 1.00000000 0.91308844
    22 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 0.92326727
    23 0.50158368 1.00000000 0.72907640 0.93576587 0.84682630 0.70237566 0.63076143
    24 0.90223701 0.54045418 0.82775392 0.76536766 0.26648880 0.50164605 0.79803349
    25 1.00000000 1.00000000 0.99066593 0.88496094 1.00000000 1.00000000 0.96673177
    26 0.88616746 0.95506988 0.89287872 0.95481226 0.89792836 0.70237566 0.76007769
    27 0.78341007 0.96099715 0.80839384 0.97268461 0.90044862 0.85843936 0.71141019
    28 0.64096114 0.91806718 0.64552230 0.91510122 0.69743493 0.91476110 0.57078034
    29 0.61738895 0.97736420 0.49016056 0.98953883 0.83305280 0.88691165 0.59273006
    30 0.10804418 0.93506842 0.37155145 0.96715102 0.83305280 0.98042530 0.42987281
    31 0.37198185 0.94094690 0.35583413 0.85482871 0.83197842 0.96787970 0.46262119
    32 0.38717398 0.87091738 0.59146583 0.89065698 0.76411357 0.74392740 0.59366127
    33 0.61213041 0.95141108 0.81125924 0.97038015 0.91697466 0.95640182 0.81182682
    34 0.74353196 0.95145986 0.74972459 0.96678351 0.78584543 0.91476110 0.75262894
    35 1.00000000 1.00000000 0.89401198 1.00000000 1.00000000 1.00000000 1.00000000
    36 0.52146300 1.00000000 0.51569828 1.00000000 0.52146300 1.00000000 0.52146300
    37 1.00000000 1.00000000 1.00000000 0.98737445 1.00000000 1.00000000 0.98621374
                8          9         10         11         12         13         14
    2                                                                              
    3                                                                              
    4                                                                              
    5                                                                              
    6                                                                              
    7                                                                              
    8                                                                              
    9  0.88461538                                                                  
    10 0.01175214 0.76050681                                                       
    11 0.47115385 0.83659059 0.73950322                                            
    12 0.63643162 0.07415835 0.74562155 0.87331358                                 
    13 0.88461538 0.13183034 0.85594192 0.96545663 0.85594192                      
    14 0.60373932 0.31096867 0.79863552 0.57306771 0.44035539 0.95200611           
    16 0.72435897 0.63209877 0.54913580 0.75664198 0.80395062 0.93382716 0.31802469
    17 1.00000000 0.80896686 0.98278103 0.48895387 0.95061728 0.98278103 0.55230669
    18 0.87500000 0.36494278 0.41198567 0.73259369 0.65768324 0.83532803 0.55425002
    20 0.59508547 0.68431109 0.75589130 0.91281580 0.72082655 0.97439817 0.51100418
    21 1.00000000 0.84993197 1.00000000 1.00000000 0.98267763 0.57852503 1.00000000
    22 1.00000000 0.68281466 1.00000000 1.00000000 1.00000000 0.27588842 1.00000000
    23 0.39743590 0.68997846 0.79591837 0.55238936 0.98062243 0.32288880 0.66138692
    24 0.34188034 0.41621696 0.66422517 0.35903417 0.53413095 0.13183034 0.30285106
    25 1.00000000 0.88889166 1.00000000 1.00000000 1.00000000 0.65934276 1.00000000
    26 0.45192308 0.47993095 0.84848932 0.92497793 0.97435897 0.27588842 0.83423254
    27 0.96399573 0.81955020 0.99329573 0.99694238 0.98030436 0.46740543 0.87551354
    28 0.85897436 0.83243599 0.94131546 0.98486950 0.93724310 0.66232327 0.89885166
    29 0.99316239 0.79986789 0.99810981 1.00000000 0.98267763 0.29247230 0.94240954
    30 0.87606838 0.99725812 0.96574028 0.96343462 0.95429634 0.85976309 0.99455526
    31 0.87617521 0.99624861 0.96901858 0.96535746 0.95578880 0.86354605 0.98485373
    32 0.59188034 0.84763948 0.82752001 0.49155214 0.87128107 0.74971341 0.58513587
    33 0.92200855 0.91407075 0.98168877 0.60156349 0.95131141 0.98127627 0.66215414
    34 0.88461538 0.93773994 0.94122685 0.98486950 0.95131141 0.79212839 0.89674801
    35 0.85628743 1.00000000 0.92215569 1.00000000 1.00000000 1.00000000 0.95748503
    36 0.97435897 1.00000000 0.99576602 1.00000000 1.00000000 0.86816966 0.99768760
    37 1.00000000 0.98442115 1.00000000 0.96469550 1.00000000 0.95223538 1.00000000
               16         17         18         20         21         22         23
    2                                                                              
    3                                                                              
    4                                                                              
    5                                                                              
    6                                                                              
    7                                                                              
    8                                                                              
    9                                                                              
    10                                                                             
    11                                                                             
    12                                                                             
    13                                                                             
    14                                                                             
    16                                                                             
    17 0.95812346                                                                  
    18 0.63120988 0.80960077                                                       
    20 0.31802469 0.98278103 0.75482659                                            
    21 0.94666667 1.00000000 1.00000000 0.95683250                                 
    22 1.00000000 1.00000000 0.95850441 1.00000000 0.84993197                      
    23 0.68296296 0.01721897 0.82681052 0.86589767 0.84993197 0.77549728           
    24 0.54913580 0.00000000 0.46440115 0.56860802 0.77918367 0.01141141 0.22209553
    25 0.93777778 1.00000000 0.93426225 1.00000000 0.87870748 0.88150150 0.80381351
    26 0.51407407 1.00000000 0.85524592 0.69322766 0.65580492 0.49697885 0.51488994
    27 0.71555556 1.00000000 0.89962436 0.63930366 0.49802255 0.84131692 0.84621628
    28 0.76888889 1.00000000 0.83856032 0.73168919 0.74542120 0.91700562 0.90400395
    29 0.90202469 1.00000000 0.95387438 0.92774751 0.83730612 0.81902703 0.80874192
    30 1.00000000 1.00000000 0.99039050 0.95683250 0.77959184 1.00000000 1.00000000
    31 1.00000000 0.99269006 0.98292129 0.95683250 0.96092517 1.00000000 0.93310528
    32 0.57925926 0.01721897 0.56434000 0.74736395 0.81743150 1.00000000 0.54754044
    33 0.76888889 0.01721897 0.72639119 0.86872833 0.94721088 1.00000000 0.63014784
    34 0.76888889 1.00000000 0.83668210 0.54709504 1.00000000 1.00000000 1.00000000
    35 1.00000000 1.00000000 0.94610778 1.00000000 1.00000000 1.00000000 0.37125749
    36 1.00000000 1.00000000 0.99606884 1.00000000 1.00000000 1.00000000 0.88698541
    37 0.93777778 1.00000000 0.95107889 1.00000000 0.98299320 0.98498498 0.95400988
               24         25         26         27         28         29         30
    2                                                                              
    3                                                                              
    4                                                                              
    5                                                                              
    6                                                                              
    7                                                                              
    8                                                                              
    9                                                                              
    10                                                                             
    11                                                                             
    12                                                                             
    13                                                                             
    14                                                                             
    16                                                                             
    17                                                                             
    18                                                                             
    20                                                                             
    21                                                                             
    22                                                                             
    23                                                                             
    24                                                                             
    25 0.95357134                                                                  
    26 0.30362538 0.76722054                                                       
    27 0.74915885 0.86309840 0.56985326                                            
    28 0.75108739 0.90470928 0.58667388 0.42569299                                 
    29 0.91988740 0.95357134 0.51044454 0.68310607 0.39376484                      
    30 0.98453858 1.00000000 0.88649115 0.56187651 0.52067806 0.62007874           
    31 0.98990826 0.85938599 0.81933535 0.81652500 0.46568547 0.18067660 0.28155832
    32 0.38775173 0.99851436 0.75312905 0.60234401 0.47673571 0.85103995 0.70386266
    33 0.56810982 0.49485216 0.86124299 0.89544596 0.81609574 0.97381204 0.94112988
    34 0.96678351 0.48489401 0.92101856 0.74388249 0.57404839 0.76928726 0.78217326
    35 1.00000000 1.00000000 0.74251497 0.84550898 0.83233533 0.96167665 1.00000000
    36 1.00000000 1.00000000 0.98309666 0.75693721 0.53458833 0.50442939 0.52146300
    37 0.97386126 0.27149002 0.86296936 0.98155363 0.84176992 0.92577479 1.00000000
               31         32         33         34         35         36
    2                                                                   
    3                                                                   
    4                                                                   
    5                                                                   
    6                                                                   
    7                                                                   
    8                                                                   
    9                                                                   
    10                                                                  
    11                                                                  
    12                                                                  
    13                                                                  
    14                                                                  
    16                                                                  
    17                                                                  
    18                                                                  
    20                                                                  
    21                                                                  
    22                                                                  
    23                                                                  
    24                                                                  
    25                                                                  
    26                                                                  
    27                                                                  
    28                                                                  
    29                                                                  
    30                                                                  
    31                                                                  
    32 0.82722020                                                       
    33 0.94618532 0.45510069                                            
    34 0.78382574 0.79853113 0.43515273                                 
    35 0.83233533 0.83233533 0.83233533 1.00000000                      
    36 0.50374544 0.76162715 0.95440334 0.58213913 0.22155689           
    37 0.93956105 0.98365797 0.49485216 0.48489401 1.00000000 1.00000000

    $beta.bray.gra
                  1            2            3            4            5
    2  0.3122590877                                                    
    3  0.5337631860 0.2085440570                                       
    4  0.1558462740 0.3628406072 0.0462819632                          
    5  0.1294575991 0.2330170627 0.0190264417 0.0708480515             
    6  0.4465343042 0.2164889462 0.4277483592 0.3741377405 0.5304415964
    7  0.3243767363 0.3531721884 0.1535477264 0.1231056515 0.0436065369
    8  0.6913722066 0.2355953492 0.6460517106 0.3034177467 0.5359325931
    9  0.0919075369 0.1478576798 0.0489076010 0.0149825500 0.1186864012
    10 0.5600784880 0.0640112235 0.4556696223 0.3042368794 0.1746485654
    11 0.4133849084 0.0250173935 0.3074794704 0.1154878685 0.1880782851
    12 0.0840504345 0.0007448860 0.1082236924 0.2274776192 0.3500435433
    13 0.2280570122 0.0318005744 0.1927024690 0.0784238057 0.1148939208
    14 0.3365941686 0.0025719482 0.2584194333 0.1810915527 0.4626552548
    16 0.2977394180 0.1183920141 0.4078876481 0.1558810196 0.3994747692
    17 0.9004343505 0.0042962809 0.3919439641 0.0097365984 0.2027822787
    18 0.5936529357 0.1748393296 0.5846267134 0.4022252467 0.2587484379
    20 0.1387259694 0.0546641397 0.1873826312 0.1536301073 0.2761509266
    21 0.0000000000 0.0104003318 0.0000000000 0.0022375253 0.0000000000
    22 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.0000000000
    23 0.3758742080 0.0000000000 0.0819747768 0.0037470134 0.0550676560
    24 0.0144089455 0.3775361482 0.0825023518 0.1525533499 0.3140872795
    25 0.0000000000 0.0000000000 0.0012264037 0.0423341972 0.0000000000
    26 0.0965225622 0.0027486817 0.0558818384 0.0141767764 0.0578408212
    27 0.1700626726 0.0095989315 0.0709263628 0.0036563549 0.0422123051
    28 0.3086118974 0.0015066302 0.1957306610 0.0298706685 0.1800445860
    29 0.1823276104 0.0149230687 0.0767224902 0.0040246156 0.0145976840
    30 0.0539748314 0.0550698922 0.3417523925 0.0229303119 0.0830046303
    31 0.0050286323 0.0511571158 0.3802544580 0.1062022541 0.0919162306
    32 0.4937812592 0.0248857996 0.1705595835 0.0206023869 0.1106003950
    33 0.2958826420 0.0144667700 0.0606003157 0.0023320350 0.0313192050
    34 0.2071457553 0.0090978784 0.1056343389 0.0064366046 0.1013375696
    35 0.0000000000 0.0000000000 0.1036135715 0.0000000000 0.0000000000
    36 0.4290522054 0.0000000000 0.3173365667 0.0000000000 0.3303040306
    37 0.0000000000 0.0000000000 0.0000000000 0.0001380245 0.0000000000
                  6            7            8            9           10
    2                                                                  
    3                                                                  
    4                                                                  
    5                                                                  
    6                                                                  
    7  0.4480943929                                                    
    8  0.0419349510 0.6542016991                                       
    9  0.1514051893 0.3204986403 0.0912766653                          
    10 0.2499615562 0.4403086828 0.5601953863 0.0973497342             
    11 0.1248968552 0.3519816356 0.2878764333 0.0708057316 0.0084802561
    12 0.1141451959 0.6261299505 0.2280770558 0.3009335114 0.0238717009
    13 0.0578148663 0.4055423360 0.0545909199 0.4411300855 0.0184521247
    14 0.3121528150 0.5964382735 0.2472030942 0.2274877696 0.0177517439
    16 0.0199717166 0.4096952334 0.0108219340 0.2854550736 0.2432887155
    17 0.0140802363 0.4445767281 0.0000000000 0.1013143678 0.0027190332
    18 0.0747690323 0.5451046054 0.0524508588 0.3531051399 0.1136076876
    20 0.1780323897 0.2967553123 0.2034366712 0.1512265656 0.0219937924
    21 0.0000000000 0.0421061330 0.0000000000 0.0065762198 0.0000000000
    22 0.0000000000 0.0334086909 0.0000000000 0.0058455989 0.0000000000
    23 0.2234303303 0.1686425042 0.4748033143 0.0025457422 0.0815519533
    24 0.4722845954 0.0662907097 0.6293225677 0.3959298731 0.2855051104
    25 0.0000000000 0.0016177570 0.0000000000 0.0456175462 0.0000000000
    26 0.1814361502 0.1537309216 0.3638758459 0.1392731756 0.0235789069
    27 0.1012836856 0.1486081362 0.0272651002 0.0152076696 0.0022367810
    28 0.0496069729 0.2855717344 0.0901517274 0.0514607266 0.0066633907
    29 0.1010342757 0.0118827665 0.0062251784 0.0853833404 0.0013420188
    30 0.0187121345 0.2304744926 0.1193667616 0.0019820867 0.0299047895
    31 0.0308828604 0.2471954847 0.1198391333 0.0028287288 0.0275198623
    32 0.1759266819 0.2254876618 0.2988831697 0.0212683884 0.0488022727
    33 0.0323336876 0.0889678786 0.0608391974 0.0024661067 0.0069993243
    34 0.0583094986 0.1382214952 0.0842022854 0.0090302338 0.0163284163
    35 0.0000000000 0.0000000000 0.1001948958 0.0000000000 0.0705263458
    36 0.0000000000 0.3572816136 0.0136601950 0.0000000000 0.0002069017
    37 0.0000000000 0.0055167103 0.0000000000 0.0009510091 0.0000000000
                 11           12           13           14           16
    2                                                                  
    3                                                                  
    4                                                                  
    5                                                                  
    6                                                                  
    7                                                                  
    8                                                                  
    9                                                                  
    10                                                                 
    11                                                                 
    12 0.0159640665                                                    
    13 0.0033138819 0.0315912699                                       
    14 0.0513881346 0.0032086713 0.0102625715                          
    16 0.1256013280 0.1182016662 0.0292531536 0.4086777289             
    17 0.0643931914 0.0122506775 0.0005240881 0.1086497890 0.0174729633
    18 0.0432311270 0.0965117898 0.0109951362 0.1233171188 0.1426173139
    20 0.0050317027 0.0509210690 0.0009839623 0.0864793688 0.3222180992
    21 0.0000000000 0.0049417197 0.2001445811 0.0000000000 0.0404185351
    22 0.0000000000 0.0000000000 0.3777397997 0.0000000000 0.0000000000
    23 0.1909547595 0.0061557530 0.3399085319 0.1093109388 0.2449467558
    24 0.5506296352 0.3829618827 0.7659725571 0.5743726218 0.4295615885
    25 0.0000000000 0.0000000000 0.2589375995 0.0000000000 0.0559889186
    26 0.0140464556 0.0016076357 0.2014250521 0.0113395904 0.3116586807
    27 0.0011076381 0.0048755019 0.2358330180 0.0314847059 0.2104930521
    28 0.0022024264 0.0012497957 0.0804244985 0.0025939851 0.1422357266
    29 0.0000000000 0.0114350301 0.5435343284 0.0382028293 0.0885172858
    30 0.0321929183 0.0387818346 0.1262570252 0.0046288272 0.0000000000
    31 0.0310031678 0.0383165429 0.1245187147 0.0131483286 0.0000000000
    32 0.1589505496 0.0250048557 0.0992779422 0.0828775171 0.3002400689
    33 0.1632387181 0.0145641632 0.0091092882 0.1028201254 0.1766196400
    34 0.0046540399 0.0091974098 0.0814775567 0.0200748321 0.1642873801
    35 0.0000000000 0.0000000000 0.0000000000 0.0391401245 0.0000000000
    36 0.0000000000 0.0000000000 0.0105095257 0.0003154961 0.0000000000
    37 0.0170029506 0.0000000000 0.0263678340 0.0000000000 0.0497216022
                 17           18           20           21           22
    2                                                                  
    3                                                                  
    4                                                                  
    5                                                                  
    6                                                                  
    7                                                                  
    8                                                                  
    9                                                                  
    10                                                                 
    11                                                                 
    12                                                                 
    13                                                                 
    14                                                                 
    16                                                                 
    17                                                                 
    18 0.0069319136                                                    
    20 0.0011844841 0.0257270086                                       
    21 0.0000000000 0.0000000000 0.0191899879                          
    22 0.0000000000 0.0235953773 0.0000000000 0.0093343749             
    23 0.5153920315 0.0953100036 0.0633880958 0.0053458597 0.0059800948
    24 0.8888520261 0.4800335989 0.3768078144 0.1548354866 0.6605112692
    25 0.0000000000 0.0517318198 0.0000000000 0.0541400021 0.0468221139
    26 0.0000000000 0.0490207738 0.0743388977 0.0780068288 0.1432711673
    27 0.0000000000 0.0496796219 0.1483788440 0.0203827066 0.0162724391
    28 0.0000000000 0.0484587991 0.0540866377 0.0679422374 0.0268659692
    29 0.0000000000 0.0366351085 0.0543328259 0.0751356665 0.0744595793
    30 0.0000000000 0.0087662115 0.0385386824 0.1638014229 0.0000000000
    31 0.0067067749 0.0157646899 0.0391041423 0.0301794161 0.0000000000
    32 0.4147325782 0.1966873840 0.0919012608 0.0175923708 0.0000000000
    33 0.5006324771 0.1466195302 0.0599406076 0.0007992992 0.0000000000
    34 0.0000000000 0.0730082530 0.1625633557 0.0000000000 0.0000000000
    35 0.0000000000 0.0465644188 0.0000000000 0.0000000000 0.0000000000
    36 0.0000000000 0.0005728267 0.0000000000 0.0000000000 0.0000000000
    37 0.0000000000 0.0291965541 0.0000000000 0.0017786855 0.0006405900
                 23           24           25           26           27
    2                                                                  
    3                                                                  
    4                                                                  
    5                                                                  
    6                                                                  
    7                                                                  
    8                                                                  
    9                                                                  
    10                                                                 
    11                                                                 
    12                                                                 
    13                                                                 
    14                                                                 
    16                                                                 
    17                                                                 
    18                                                                 
    20                                                                 
    21                                                                 
    22                                                                 
    23                                                                 
    24 0.5310162094                                                    
    25 0.0818828534 0.0172219470                                       
    26 0.1262052904 0.5575194994 0.1422674048                          
    27 0.0117056527 0.1809229069 0.0654793523 0.0807637225             
    28 0.0287657001 0.2029814255 0.0607306254 0.0177054060 0.1313756506
    29 0.0828775388 0.0283608754 0.0009043691 0.3051044384 0.1562854176
    30 0.0000000000 0.0013552764 0.0000000000 0.0942135302 0.3333327406
    31 0.0506774049 0.0015663439 0.0699556751 0.1535955928 0.1446212694
    32 0.0595128154 0.4573972206 0.0007730356 0.0328791843 0.0222586225
    33 0.0075792318 0.2994797801 0.2193108453 0.0334335081 0.0058249081
    34 0.0000000000 0.0248965891 0.2701124427 0.0100873239 0.0157553372
    35 0.6026879244 0.0000000000 0.0000000000 0.2395720951 0.1470595003
    36 0.0497130604 0.0000000000 0.0000000000 0.0034305143 0.0914805532
    37 0.0031835141 0.0168287638 0.2611770842 0.0443366791 0.0026669314
                 28           29           30           31           32
    2                                                                  
    3                                                                  
    4                                                                  
    5                                                                  
    6                                                                  
    7                                                                  
    8                                                                  
    9                                                                  
    10                                                                 
    11                                                                 
    12                                                                 
    13                                                                 
    14                                                                 
    16                                                                 
    17                                                                 
    18                                                                 
    20                                                                 
    21                                                                 
    22                                                                 
    23                                                                 
    24                                                                 
    25                                                                 
    26                                                                 
    27                                                                 
    28                                                                 
    29 0.3932917329                                                    
    30 0.4040102019 0.1627488052                                       
    31 0.4603798684 0.3954877757 0.0492038134                          
    32 0.0915825417 0.0796044208 0.2320029213 0.1396985666             
    33 0.0516563696 0.0117800376 0.0433470839 0.0412310727 0.0606687067
    34 0.0722524838 0.1242069605 0.1711178386 0.1751989017 0.0011202933
    35 0.1549958473 0.0376875306 0.0000000000 0.1666888841 0.1586686867
    36 0.0751717667 0.3634574592 0.4230471141 0.4457113789 0.0780161614
    37 0.0571813050 0.0278616198 0.0000000000 0.0439054409 0.0032511180
                 33           34           35           36
    2                                                     
    3                                                     
    4                                                     
    5                                                     
    6                                                     
    7                                                     
    8                                                     
    9                                                     
    10                                                    
    11                                                    
    12                                                    
    13                                                    
    14                                                    
    16                                                    
    17                                                    
    18                                                    
    20                                                    
    21                                                    
    22                                                    
    23                                                    
    24                                                    
    25                                                    
    26                                                    
    27                                                    
    28                                                    
    29                                                    
    30                                                    
    31                                                    
    32                                                    
    33                                                    
    34 0.0659897490                                       
    35 0.1604323318 0.0000000000                          
    36 0.0192966950 0.1346816654 0.6981317528             
    37 0.0452547871 0.1052242019 0.0000000000 0.0000000000

    $beta.bray
               1         2         3         4         5         6         7
    2  0.9509644                                                            
    3  0.6214464 0.8393219                                                  
    4  0.9417192 0.3764514 0.8601340                                        
    5  0.8903477 0.8490134 0.7212046 0.8405445                              
    6  0.9818132 0.8364729 0.9293944 0.8914436 0.9237150                    
    7  0.6093647 0.8303047 0.2951393 0.8223357 0.6687185 0.9497404          
    8  0.9766286 0.8605953 0.9123979 0.9284177 0.9365736 0.5822127 0.9394581
    9  0.9694578 0.6910438 0.8831736 0.7162052 0.7818343 0.9506756 0.6086076
    10 0.9283410 0.3927274 0.7282707 0.6243881 0.9115280 0.7516076 0.8322872
    11 0.9506438 0.8288252 0.8313731 0.8714140 0.9121358 0.8629518 0.8758752
    12 0.9868445 0.5128803 0.9169681 0.6112856 0.7740991 0.9134156 0.7002883
    13 0.9777704 0.8877425 0.9168140 0.9343657 0.9574617 0.9129621 0.8855576
    14 0.9479638 0.6483176 0.8048482 0.6970298 0.7059112 0.7594346 0.7185241
    16 0.9890974 0.9223426 0.9398383 0.9598316 0.9486106 0.6369594 0.9588310
    17 0.9176533 0.9870773 0.8428049 0.9925176 0.9301215 0.9763545 0.8829296
    18 0.9496851 0.5577781 0.7850286 0.7201688 0.9180915 0.8557994 0.8677656
    20 0.9853398 0.7573745 0.9111106 0.8563405 0.8879265 0.7649173 0.9085308
    21 1.0000000 0.9737635 1.0000000 0.9783736 1.0000000 1.0000000 0.9551946
    22 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.9566760
    23 0.8774579 1.0000000 0.8110512 0.9395129 0.9018940 0.9258060 0.7994039
    24 0.9166460 0.9179903 0.9102563 0.9179210 0.5805761 0.9739306 0.8643242
    25 1.0000000 1.0000000 0.9918923 0.9272951 1.0000000 1.0000000 0.9683495
    26 0.9826900 0.9578186 0.9487606 0.9689890 0.9557692 0.8838118 0.9138086
    27 0.9534727 0.9705961 0.8793202 0.9763410 0.9426609 0.9597230 0.8600183
    28 0.9495730 0.9195738 0.8412530 0.9449719 0.8774795 0.9643681 0.8563521
    29 0.7997166 0.9922873 0.5668831 0.9935634 0.8476505 0.9879459 0.6046128
    30 0.1620190 0.9901383 0.7133038 0.9900813 0.9160574 0.9991374 0.6603473
    31 0.3770105 0.9921040 0.7360886 0.9610310 0.9238946 0.9987626 0.7098167
    32 0.8809552 0.8958032 0.7620254 0.9112594 0.8747140 0.9198541 0.8191489
    33 0.9080131 0.9658778 0.8718596 0.9727122 0.9482939 0.9887355 0.9007947
    34 0.9506777 0.9605577 0.8553589 0.9732201 0.8871830 0.9730706 0.8908504
    35 1.0000000 1.0000000 0.9976255 1.0000000 1.0000000 1.0000000 1.0000000
    36 0.9505152 1.0000000 0.8330348 1.0000000 0.8517670 1.0000000 0.8787446
    37 1.0000000 1.0000000 1.0000000 0.9875125 1.0000000 1.0000000 0.9917305
               8         9        10        11        12        13        14
    2                                                                       
    3                                                                       
    4                                                                       
    5                                                                       
    6                                                                       
    7                                                                       
    8                                                                       
    9  0.9758920                                                            
    10 0.5719475 0.8578565                                                  
    11 0.7590303 0.9073963 0.7479835                                        
    12 0.8645087 0.3750919 0.7694932 0.8892776                              
    13 0.9392063 0.5729604 0.8743940 0.9687705 0.8875332                    
    14 0.8509424 0.5384564 0.8163873 0.6244558 0.4435641 0.9622687          
    16 0.7351809 0.9175538 0.7924245 0.8822433 0.9221523 0.9630803 0.7267024
    17 1.0000000 0.9102812 0.9855001 0.5533471 0.9628680 0.9833051 0.6609565
    18 0.9274509 0.7180479 0.5255934 0.7758248 0.7541950 0.8463232 0.6775671
    20 0.7985221 0.8355377 0.7778851 0.9178475 0.7717476 0.9753821 0.5974835
    21 1.0000000 0.8565082 1.0000000 1.0000000 0.9876193 0.7786696 1.0000000
    22 1.0000000 0.6886603 1.0000000 1.0000000 1.0000000 0.6536282 1.0000000
    23 0.8722392 0.6925242 0.8774703 0.7433441 0.9867782 0.6627973 0.7706979
    24 0.9712029 0.8121468 0.9497303 0.9096638 0.9170928 0.8978029 0.8772237
    25 1.0000000 0.9345092 1.0000000 1.0000000 1.0000000 0.9182804 1.0000000
    26 0.8157989 0.6192041 0.8720682 0.9390244 0.9759666 0.4773135 0.8455721
    27 0.9912608 0.8347579 0.9955325 0.9980500 0.9851799 0.7032384 0.9069982
    28 0.9491261 0.8838967 0.9479788 0.9870719 0.9384929 0.7427478 0.9014456
    29 0.9993876 0.8852512 0.9994518 1.0000000 0.9941127 0.8360066 0.9806124
    30 0.9954351 0.9992402 0.9956451 0.9956275 0.9930782 0.9860201 0.9991841
    31 0.9960143 0.9990773 0.9965384 0.9963606 0.9941053 0.9880648 0.9980021
    32 0.8907635 0.8689079 0.8763223 0.6505027 0.8962859 0.8489914 0.6680134
    33 0.9828477 0.9165369 0.9886881 0.7648022 0.9658756 0.9903856 0.7649743
    34 0.9688177 0.9467702 0.9575553 0.9895235 0.9605088 0.8736059 0.9168228
    35 0.9564823 1.0000000 0.9926820 1.0000000 1.0000000 1.0000000 0.9966252
    36 0.9880192 1.0000000 0.9959729 1.0000000 1.0000000 0.8786792 0.9980031
    37 1.0000000 0.9853722 1.0000000 0.9816984 1.0000000 0.9786032 1.0000000
              16        17        18        20        21        22        23
    2                                                                       
    3                                                                       
    4                                                                       
    5                                                                       
    6                                                                       
    7                                                                       
    8                                                                       
    9                                                                       
    10                                                                      
    11                                                                      
    12                                                                      
    13                                                                      
    14                                                                      
    16                                                                      
    17 0.9755964                                                            
    18 0.7738272 0.8165327                                                  
    20 0.6402428 0.9839655 0.7805536                                        
    21 0.9870852 1.0000000 1.0000000 0.9760225                              
    22 1.0000000 1.0000000 0.9820998 1.0000000 0.8592663                    
    23 0.9279097 0.5326110 0.9221205 0.9292858 0.8552778 0.7814774          
    24 0.9786974 0.8888520 0.9444348 0.9454158 0.9340192 0.6719227 0.7531117
    25 0.9937667 1.0000000 0.9859941 1.0000000 0.9328475 0.9283236 0.8856964
    26 0.8257328 1.0000000 0.9042667 0.7675666 0.7338117 0.6402500 0.6410952
    27 0.9260486 1.0000000 0.9493040 0.7876825 0.5184053 0.8575894 0.8579219
    28 0.9111246 1.0000000 0.8870191 0.7857758 0.8133634 0.9438716 0.9327697
    29 0.9905420 1.0000000 0.9905095 0.9820803 0.9124418 0.8934866 0.8916195
    30 1.0000000 1.0000000 0.9991567 0.9953712 0.9433933 1.0000000 1.0000000
    31 1.0000000 0.9993968 0.9986860 0.9959366 0.9911046 1.0000000 0.9837827
    32 0.8794993 0.4319516 0.7610274 0.8392652 0.8350239 1.0000000 0.6070533
    33 0.9455085 0.5178515 0.8730107 0.9286689 0.9480102 1.0000000 0.6377271
    34 0.9331763 1.0000000 0.9096904 0.7096584 1.0000000 1.0000000 1.0000000
    35 1.0000000 1.0000000 0.9926722 1.0000000 1.0000000 1.0000000 0.9739454
    36 1.0000000 1.0000000 0.9966417 1.0000000 1.0000000 1.0000000 0.9366985
    37 0.9874994 1.0000000 0.9802754 1.0000000 0.9847719 0.9856256 0.9571934
              24        25        26        27        28        29        30
    2                                                                       
    3                                                                       
    4                                                                       
    5                                                                       
    6                                                                       
    7                                                                       
    8                                                                       
    9                                                                       
    10                                                                      
    11                                                                      
    12                                                                      
    13                                                                      
    14                                                                      
    16                                                                      
    17                                                                      
    18                                                                      
    20                                                                      
    21                                                                      
    22                                                                      
    23                                                                      
    24                                                                      
    25 0.9707933                                                            
    26 0.8611449 0.9094879                                                  
    27 0.9300818 0.9285778 0.6506170                                        
    28 0.9540688 0.9654399 0.6043793 0.5570686                              
    29 0.9482483 0.9544757 0.8155490 0.8393915 0.7870566                    
    30 0.9858939 1.0000000 0.9807047 0.8952093 0.9246883 0.7828275          
    31 0.9914746 0.9293417 0.9729309 0.9611463 0.9260653 0.5761644 0.3307621
    32 0.8451490 0.9992874 0.7860082 0.6246026 0.5683183 0.9306444 0.9358656
    33 0.8675896 0.7141630 0.8946765 0.9012709 0.8677521 0.9855921 0.9844770
    34 0.9916801 0.7550065 0.9311059 0.7596378 0.6463009 0.8934942 0.9532911
    35 1.0000000 1.0000000 0.9820871 0.9925685 0.9873312 0.9993642 1.0000000
    36 1.0000000 1.0000000 0.9865272 0.8484178 0.6097601 0.8678868 0.9445101
    37 0.9906900 0.5326671 0.9073060 0.9842206 0.8989512 0.9536364 1.0000000
              31        32        33        34        35        36
    2                                                             
    3                                                             
    4                                                             
    5                                                             
    6                                                             
    7                                                             
    8                                                             
    9                                                             
    10                                                            
    11                                                            
    12                                                            
    13                                                            
    14                                                            
    16                                                            
    17                                                            
    18                                                            
    20                                                            
    21                                                            
    22                                                            
    23                                                            
    24                                                            
    25                                                            
    26                                                            
    27                                                            
    28                                                            
    29                                                            
    30                                                            
    31                                                            
    32 0.9669188                                                  
    33 0.9874164 0.5157694                                        
    34 0.9590246 0.7996514 0.5011425                              
    35 0.9990242 0.9910040 0.9927677 1.0000000                    
    36 0.9494568 0.8396433 0.9737000 0.7168208 0.9196886          
    37 0.9834665 0.9869091 0.5401070 0.5901182 1.0000000 1.0000000

</div>

</div>

**Interpretation:**

The abundance-based beta diversity analysis revealed a high overall Bray–Curtis dissimilarity among the fish communities (βBRAY = 0.961), indicating substantial variation in community composition across the metacommunity. Most of this dissimilarity was attributed to balanced variation in species abundances (βBRAY.BAL = 0.892), whereas abundance gradients contributed relatively little (βBRAY.GRA = 0.069). These results suggest that differences among communities primarily reflect changes in species composition and relative abundances rather than the progressive loss or gain of individuals. From a metacommunity perspective, this high community turnover is consistent with the combined influence of environmental filtering, dispersal and stochastic ecological processes. However, beta diversity alone cannot distinguish the relative importance of these assembly mechanisms and therefore provides the foundation for examining the contribution of ecological drift in the following analysis.

Although beta diversity demonstrates that communities differ substantially in their species composition, it does not identify the processes responsible for this variation. To investigate the contribution of ecological drift, the next section examines the residual variation remaining after environmental filtering and dispersal have been accounted for using variation partitioning.

</div>

<div id="residual-variation" class="section level3" number="4.4.2">

### <span class="header-section-number">4.4.2</span> Residual Variation

*How much community variation remains unexplained after accounting for environmental selection and dispersal?*

Variation partitioning separates the variation in community composition into components explained by environmental variables, spatial variables and their shared effects. The remaining unexplained variation, referred to as the residual fraction, cannot be attributed to the measured deterministic processes and is commonly interpreted as representing the influence of stochastic ecological processes together with other unmeasured ecological mechanisms.

The purpose of this analysis is to examine the results of the variation partitioning model and evaluate the proportion of community variation that remains unexplained after accounting for environmental filtering and dispersal. This residual component provides indirect evidence for the contribution of ecological drift within the metacommunity.

**Show the R code used for this analysis**

<div class="code-copy-outer-scaffold">

``` r
############################################################
# Step 13. Residual variation
############################################################

vp
```

</div>

<div class="cell" layout-align="center">

<div class="cell-output cell-output-stdout">


    Partition of variance in RDA 

    Call: varpart(Y = comm.hel, X = env.std, mem)

    Explanatory tables:
    X1:  env.std
    X2:  mem 

    No. of explanatory tables: 2 
    Total variation (SS): 26.026 
                Variance: 0.76548 
    No. of observations: 35 

    Partition table:
                         Df R.squared Adj.R.squared Testable
    [a+c] = X1            4   0.23854       0.13702     TRUE
    [b+c] = X2           11   0.44134       0.17416     TRUE
    [a+b+c] = X1+X2      15   0.57788       0.24463     TRUE
    Individual fractions                                    
    [a] = X1|X2           4                 0.07047     TRUE
    [b] = X2|X1          11                 0.10762     TRUE
    [c]                   0                 0.06654    FALSE
    [d] = Residuals                         0.75537    FALSE
    ---
    Use function 'rda' to test significance of fractions of interest

</div>

</div>

**Interpretation:**

The variation partitioning analysis showed that the measured environmental and spatial variables together explained 24.5% of the variation in fish community composition, while the remaining 75.5% was unexplained. Of the explained variation, environmental variables uniquely accounted for 7.0%, spatial variables 10.8%, and 6.7% was shared between the two. The large residual fraction indicates that most community variation was not captured by the measured deterministic processes. From a metacommunity perspective, this unexplained variation is consistent with the influence of ecological drift through random demographic fluctuations, colonisation and local extinction events. However, the residual fraction should not be interpreted as ecological drift alone, as it may also reflect unmeasured environmental variables, species interactions and historical processes.

Having examined the individual analyses, the next section integrates the evidence from beta diversity and residual variation to evaluate the overall contribution of ecological drift to metacommunity assembly and how stochastic processes complement environmental filtering and dispersal in structuring fish communities.

</div>

<div id="conceptual-synthesis-ecological-drift" class="section level3" number="4.4.3">

### <span class="header-section-number">4.4.3</span> Conceptual Synthesis: Ecological Drift

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

A[&quot;Community Matrix&quot;]
    --&gt; B[&quot;Abundance-based&lt;br/&gt;Beta Diversity&quot;]

B --&gt; C[&quot;Communities differ in&lt;br/&gt;species composition&quot;]

C --&gt; D[&quot;Variation Partitioning&quot;]

D --&gt; E[&quot;Explained Variation&quot;]

D --&gt; F[&quot;Residual Variation&lt;br/&gt;(75.5%)&quot;]

F --&gt; G[&quot;Evidence consistent with&lt;br/&gt;stochastic ecological drift&lt;br/&gt;and other unmeasured processes&quot;]

G --&gt; H[&quot;Ecological drift contributes&lt;br/&gt;to metacommunity assembly&quot;]</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 4.14.** Conceptual synthesis of the quantitative evidence supporting ecological drift. Beta diversity analyses demonstrate differences in species composition among communities, while variation partitioning shows that a large proportion of community variation remains unexplained. This residual variation is consistent with ecological drift and other unmeasured processes, indicating that stochasticity contributes to metacommunity assembly alongside historical speciation, environmental selection and dispersal.

<div class="callout callout-style-default callout-important callout-titled" title="Key Take-home Message">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Key Take-home Message

</div>

</div>

<div class="callout-body-container callout-body">

Ecological drift represents the stochastic component of metacommunity assembly. In this worked example, beta diversity demonstrated substantial differences among local fish communities, while variation partitioning revealed that 75.5% of community variation remained unexplained after accounting for environmental filtering and dispersal. Although this residual variation cannot be attributed exclusively to ecological drift, it is consistent with the influence of stochastic demographic processes together with unmeasured environmental variables, species interactions and historical contingencies. These findings highlight that community assembly is shaped not only by deterministic mechanisms but also by random ecological processes.

</div>

</div>

</div>

</div>

<div id="interpreting-the-results-through-the-lens-of-metacommunity-ecology" class="section level2" number="4.5">

## <span class="header-section-number">4.5</span> Interpreting the Results Through the Lens of Metacommunity Ecology

The analyses completed in this chapter reveal much more than environmental gradients or patterns of species similarity. When interpreted together, they provide evidence for several ecological processes operating simultaneously within a metacommunity. This distinction is important because metacommunity ecology seeks to explain why communities differ across landscapes, rather than simply describing that they do.

<div id="what-evidence-do-our-results-provide-for-environmental-selection" class="section level3" number="4.5.1">

### <span class="header-section-number">4.5.1</span> What evidence do our results provide for environmental selection?

Of the four ecological processes discussed in this chapter, our quantitative analyses provide the strongest evidence for environmental selection. The dbRDA showed that measured environmental variables explained a substantial proportion of the variation in community composition, while the envfit analysis identified several environmental variables that were significantly associated with the observed community patterns. Rather than occurring randomly, differences among communities were closely linked to differences in their local environments.

This relationship suggests that species are responding to environmental conditions in predictable ways. Communities occupying similar environments tended to support more similar species assemblages, whereas communities exposed to different environmental conditions became increasingly distinct. These results are consistent with environmental filtering, where local environmental conditions favour species possessing traits that allow them to survive and reproduce under those conditions (Keddy, 1992; Weiher & Keddy, 1995). They also support the concept of species sorting, where differences in habitat conditions influence which species successfully establish within different communities (Leibold et al., 2004; Cottenie, 2005).

Importantly, our analyses move beyond simply describing an environmental gradient. While an environmental gradient tells us that conditions change across space, the dbRDA and envfit analyses help explain why community composition changes along that gradient. By linking environmental variation to biological variation, these analyses provide evidence that environmental selection is actively contributing to community assembly (Legendre & Legendre, 2012; Borcard et al., 2018; Vellend, 2010).

Although environmental selection appears to be the dominant process in this worked example, it is unlikely to act alone. Community assembly is typically influenced by multiple ecological processes operating simultaneously, including dispersal, ecological drift and evolutionary change (Logue et al., 2011; Leibold & Chase, 2018). The strength of quantitative metacommunity analyses lies in their ability to identify which of these processes is most strongly supported by the available evidence.

<div class="callout callout-style-default callout-important callout-titled" title="Interpreting Evidence for Environmental Selection">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Interpreting Evidence for Environmental Selection

</div>

</div>

<div class="callout-body-container callout-body">

The dbRDA and *envfit* analyses provide evidence that differences in community composition are closely associated with differences in environmental conditions. This suggests that environmental selection is influencing which species are able to establish and persist at different sites. Quantitative analyses therefore help ecologists move beyond describing environmental gradients by identifying the ecological processes that are likely to generate those patterns. Strong ecological inference comes from combining these statistical results with ecological theory and complementary lines of evidence.

</div>

</div>

</div>

<div id="what-evidence-do-our-results-provide-for-dispersal" class="section level3" number="4.5.2">

### <span class="header-section-number">4.5.2</span> What evidence do our results provide for dispersal?

Compared with environmental selection, the evidence for dispersal is less direct. None of the analyses performed in this chapter measure the movement of organisms between communities. However, they do provide patterns that are consistent with dispersal contributing to the structure of the metacommunity.

The NMDS ordination showed that several communities shared similar species compositions despite occurring at different locations. One possible explanation is that individuals are dispersing among sites, allowing connected communities to exchange species and maintain similar assemblages over time (Leibold et al., 2004; Holyoak et al., 2005). At the same time, other communities remained compositionally distinct, suggesting that movement among all sites is not unrestricted.

These observations highlight an important feature of metacommunity ecology. Similar communities are not always the result of dispersal alone. They may also occur because sites experience comparable environmental conditions or because species respond similarly to those conditions through environmental selection (Cottenie, 2005; Logue et al., 2011). Likewise, differences among communities may arise because dispersal is limited, because environments differ, or because stochastic processes influence community assembly. Our results therefore suggest that dispersal may contribute to the observed patterns, but they do not allow us to separate dispersal from these other processes with confidence.

Additional datasets would allow us to investigate dispersal much more effectively. Geographic coordinates could be used to calculate distances among communities, while habitat connectivity, landscape resistance, river networks or ocean currents could help estimate the likelihood of organism movement between sites (Leibold & Chase, 2018). These data could then be analysed using approaches such as variation partitioning, Moran’s Eigenvector Maps (MEMs), Mantel tests or other spatial modelling techniques to distinguish the relative contributions of environmental selection and dispersal (Borcard et al., 2018; Dray et al., 2006; Peres-Neto et al., 2006). Incorporating temporal datasets collected over multiple years could further reveal how dispersal influences colonisation, extinction and community turnover through time (Logue et al., 2011).

Rather than viewing dispersal as a process that can be identified from a single statistical output, metacommunity ecology treats dispersal as a hypothesis that is evaluated by integrating quantitative analyses with spatial and ecological information. This combination of evidence allows ecologists to develop a more complete understanding of how organisms move across landscapes and how that movement contributes to biodiversity patterns.

<div class="callout callout-style-default callout-tip callout-titled" title="Interpreting Evidence for Dispersal">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Interpreting Evidence for Dispersal

</div>

</div>

<div class="callout-body-container callout-body">

The analyses presented in this chapter suggest that dispersal may contribute to similarities among communities, but they do not measure dispersal directly. Similar community composition may result from organism movement, shared environmental conditions or the combined influence of multiple ecological processes. By integrating spatial datasets, connectivity information and additional spatial analyses, ecologists can distinguish whether community similarity is primarily driven by dispersal, environmental selection or an interaction between both processes. This illustrates that quantitative metacommunity analyses are most powerful when multiple datasets and complementary analytical approaches are interpreted together.

</div>

</div>

</div>

<div id="what-evidence-do-our-results-provide-for-ecological-drift" class="section level3" number="4.5.3">

### <span class="header-section-number">4.5.3</span> What evidence do our results provide for ecological drift?

Compared with environmental selection, the evidence for ecological drift in our analyses is less direct. The dbRDA showed that the measured environmental variables explained a substantial proportion of the variation in community composition. However, not all of the variation among communities could be attributed to the environmental variables included in the analysis. This remaining unexplained variation suggests that additional ecological processes may also be influencing community assembly (Vellend, 2010; Chase & Myers, 2011).

One possible explanation is ecological drift. Ecological drift describes random changes in species abundances and community composition that arise through stochastic processes such as demographic fluctuations, random colonisation and local extinctions (Hubbell, 2001; Vellend, 2010). If communities experience similar environmental conditions but still differ in species composition, these random processes may contribute to the observed differences. Our results therefore suggest that ecological drift could be influencing the metacommunity alongside environmental selection rather than acting as an independent or dominant process.

Recognising the potential role of ecological drift is important because it reminds us that community assembly is not always entirely deterministic. Even when environmental selection explains a large proportion of community variation, stochastic processes can still influence which species establish, persist or disappear from local communities. Considering both deterministic and stochastic processes therefore provides a more complete understanding of how biodiversity is structured across landscapes (Leibold & Chase, 2018; Shoemaker et al., 2020).

However, the unexplained variation identified in our analyses should not automatically be interpreted as evidence of ecological drift. Some of this variation may instead reflect environmental variables that were not measured, historical disturbances, priority effects, biotic interactions or limitations of the sampling design (Chase & Myers, 2011; Borcard et al., 2018; Shoemaker et al., 2020). Consequently, our analyses provide evidence that additional processes are likely operating within the metacommunity, but they cannot determine whether ecological drift is responsible for all of the unexplained variation.

Future studies could investigate the role of ecological drift more directly by incorporating repeated sampling through time, allowing researchers to determine whether communities diverge because of random demographic fluctuations. Long-term monitoring would also help distinguish persistent environmental effects from stochastic changes in community composition. Additional information on disturbance history, species interactions, functional traits and temporal community dynamics could further separate the influence of ecological drift from environmental selection and other ecological processes (Logue et al., 2011; Vellend et al., 2014; Shoemaker et al., 2020). By integrating these complementary datasets with quantitative analyses, ecologists can develop a more robust understanding of the role that stochastic processes play in shaping metacommunity dynamics.

<div class="callout callout-style-default callout-tip callout-titled" title="Interpreting Evidence for Ecological Drift">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Interpreting Evidence for Ecological Drift

</div>

</div>

<div class="callout-body-container callout-body">

The unexplained variation remaining after the dbRDA suggests that community assembly is influenced by processes beyond the measured environmental variables. Ecological drift is one possible explanation, but it is not the only one. Unmeasured environmental factors, disturbance history, species interactions and sampling limitations may also contribute to the observed patterns. By combining long-term monitoring, additional ecological datasets and complementary analytical approaches, ecologists can better distinguish the influence of stochastic ecological drift from deterministic processes such as environmental selection. This illustrates an important principle of metacommunity ecology: robust ecological inference is built by integrating multiple sources of evidence rather than relying on a single statistical result.

</div>

</div>

</div>

<div id="what-evidence-do-our-results-provide-for-speciation" class="section level3" number="4.5.4">

### <span class="header-section-number">4.5.4</span> What evidence do our results provide for speciation?

The quantitative analyses presented in this chapter can contribute to our understanding of speciation, even though they cannot demonstrate that new species have formed. Our Bray-Curtis dissimilarity analysis, NMDS ordination and dbRDA revealed that communities differed in species composition and that many of these differences were associated with environmental conditions. These results suggest that populations occupying different habitats may experience different selective pressures, potentially leading to evolutionary divergence over long periods of time (Vellend, 2010; Urban et al., 2008).

The distinct community groupings identified in the NMDS, together with the significant environmental relationships detected by the dbRDA and envfit, can therefore help ecologists identify locations where divergent selection or long-term ecological separation may be occurring. These patterns generate hypotheses about where speciation may be taking place and provide a valuable starting point for future evolutionary investigations (Leibold et al., 2004; Logue et al., 2011; Urban et al., 2008).

However, community analyses alone cannot confirm that speciation has occurred. Speciation is an evolutionary process that develops over many generations and is influenced by genetic divergence, reproductive isolation and historical evolutionary events (Ricklefs, 2008; Vellend, 2016). Demonstrating speciation therefore requires evidence that extends beyond community composition. Taxonomic revisions, phylogenetic analyses, population genetics, genomic sequencing, fossil evidence and historical biogeography all provide complementary information that helps determine whether populations have diverged into distinct species (Cavender-Bares et al., 2009; Wiens, 2011; Vellend, 2016).

Future studies could strengthen evidence for speciation by combining quantitative community analyses with evolutionary datasets. For example, integrating species abundance data with phylogenetic trees would allow researchers to determine whether closely related species occupy similar environmental conditions or whether evolutionary divergence has resulted in distinct ecological niches. Genetic data could reveal whether geographically separated populations continue to exchange genes or have become reproductively isolated. Long-term ecological monitoring could further identify whether persistent environmental differences continue to drive divergence through time. By integrating these complementary datasets, ecologists can move from identifying patterns that are consistent with evolutionary divergence to testing hypotheses about the mechanisms responsible for speciation.

<div class="callout callout-style-default callout-tip callout-titled" title="Interpreting Evidence for Speciation">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Interpreting Evidence for Speciation

</div>

</div>

<div class="callout-body-container callout-body">

Quantitative analyses provide an important first step in investigating speciation by identifying communities that are compositionally distinct or associated with contrasting environmental conditions. These patterns can suggest where evolutionary divergence may be occurring, but they cannot confirm that new species have formed. Demonstrating speciation requires integrating quantitative community analyses with taxonomy, phylogenetics, population genetics, historical biogeography and the broader evolutionary literature. This combination of statistical evidence and conceptual synthesis allows ecologists to investigate how biodiversity is generated and maintained across landscapes over evolutionary time.

</div>

</div>

</div>

<div id="so-why-is-this-a-metacommunity-and-not-simply-an-environmental-gradient" class="section level3" number="4.5.5">

### <span class="header-section-number">4.5.5</span> So why is this a metacommunity and not simply an environmental gradient?

The analyses completed throughout this chapter demonstrate why metacommunity ecology extends beyond simply describing environmental gradients. If we had only observed that community composition changed across different environmental conditions, we would have identified a pattern of species turnover. While this is an important ecological observation, it does not explain the processes responsible for generating that pattern (Leibold et al., 2004; Vellend, 2010).

Instead, each quantitative analysis contributed a different piece of ecological evidence. The Bray-Curtis dissimilarity analysis demonstrated that communities differed in species composition. The NMDS ordination showed that these differences followed structured patterns rather than occurring randomly. The dbRDA and envfit analyses then demonstrated that a substantial proportion of this variation was associated with measured environmental variables, providing strong evidence that environmental selection influences community assembly (Legendre & Legendre, 2012; Borcard et al., 2018). At the same time, the remaining unexplained variation reminded us that other ecological processes may also contribute to community structure.

When these results are interpreted together, they suggest that no single process completely explains the observed biodiversity patterns. Environmental selection appears to play the strongest role in structuring this metacommunity, but dispersal among sites may influence species distributions, stochastic ecological drift may contribute to unexplained variation, and long-term evolutionary divergence may ultimately lead to speciation (Leibold et al., 2004; Logue et al., 2011; Vellend, 2010, 2016). Modern metacommunity ecology recognises that these processes operate simultaneously and often interact across spatial and temporal scales rather than acting independently (Shoemaker et al., 2020; Leibold & Chase, 2018).

This integrated interpretation is what distinguishes metacommunity ecology from traditional studies of environmental gradients. Rather than asking whether communities differ across space, metacommunity ecology asks why they differ, which processes are responsible for those differences, and how those processes interact to shape biodiversity across landscapes. Quantitative analyses therefore become tools for evaluating competing ecological hypotheses rather than simply describing statistical relationships (Logue et al., 2011; Leibold & Chase, 2018).

Importantly, no individual analysis performed in this chapter provides definitive evidence for any single ecological process. Instead, each analysis contributes one line of evidence that must be interpreted alongside ecological theory, previous research, conceptual understanding and, where appropriate, additional datasets such as spatial information, functional traits, phylogenetic relationships or long-term monitoring data. This process of integrating multiple sources of evidence is fundamental to ecological inference and reflects how metacommunity research is conducted in practice (Borcard et al., 2018; Vellend, 2016; Shoemaker et al., 2020).

<div class="callout callout-style-default callout-important callout-titled" title="From Environmental Gradients to Metacommunity Ecology">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>From Environmental Gradients to Metacommunity Ecology

</div>

</div>

<div class="callout-body-container callout-body">

Environmental gradients describe **where** communities differ, but they do not explain **why** those differences occur. By combining multiple quantitative analyses, ecologists can evaluate evidence for environmental selection, dispersal, ecological drift and speciation. These statistical results should then be interpreted alongside ecological theory, taxonomy, phylogenetics, spatial information and previous research to develop a comprehensive understanding of the processes shaping biodiversity. This ability to integrate multiple sources of evidence is one of the defining characteristics of modern metacommunity ecology.

</div>

</div>

</div>

</div>

</div>

<div id="beyond-the-worked-example" class="section level1" number="5">

# <span class="header-section-number">5</span> Beyond the Worked Example

The analyses presented in this chapter demonstrated how quantitative methods can be used to investigate the four fundamental processes shaping metacommunities: historical speciation, environmental selection, dispersal and ecological drift.Joint species distribution models represent one example of these developments by modelling multiple species simultaneously while accounting for shared environmental responses (Pollock et al., 2014). Although these processes provide the foundation of modern metacommunity ecology, current research continues to expand the framework to address increasingly complex ecological questions (Leibold & Chase, 2018; Chase et al., 2020).

Modern studies now investigate how ecosystems are connected through the movement of organisms, energy and nutrients, how communities change through time, how functional traits influence community assembly, and how metacommunity theory can be applied to biodiversity conservation.Recent developments have focused on linking meta-ecosystem theory more closely with empirical ecological studies (Gounand et al., 2018). Seasonal variation, disturbance and dormancy all contribute to temporal metacommunity dynamics (Holyoak, Caspi & Redosh, 2020). These developments extend the framework introduced in this chapter while retaining the same process-based approach to understanding biodiversity (Chase et al., 2020; Holyoak et al., 2020).

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart TD

A[&quot;Metacommunity Ecology&quot;]

A --&gt; B[&quot;Meta-ecosystems&quot;]

A --&gt; C[&quot;Temporal Dynamics&quot;]

A --&gt; D[&quot;Functional Traits&quot;]

A --&gt; E[&quot;Conservation&quot;]

B --&gt; F[&quot;Movement of organisms,&lt;br/&gt;energy and nutrients&quot;]

C --&gt; G[&quot;Communities change&lt;br/&gt;through time&quot;]

D --&gt; H[&quot;Species characteristics&lt;br/&gt;influence community assembly&quot;]

E --&gt; I[&quot;Managing connected&lt;br/&gt;landscapes&quot;]</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 5.1.** Modern developments in metacommunity ecology. The process-based framework introduced in this chapter has been extended to investigate ecosystem connectivity, temporal community dynamics, functional trait ecology and biodiversity conservation.

<div id="tbl-modern-developments" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-modern-developments-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 28%" />
<col style="width: 71%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Development</strong></th>
<th style="text-align: left;"><strong>How it extends metacommunity ecology</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>Meta-ecosystems</strong></td>
<td style="text-align: left;">Recognises that ecosystems are connected by the movement of organisms, nutrients and energy, rather than species alone.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Temporal metacommunities</strong></td>
<td style="text-align: left;">Investigates how community composition changes through time as a result of succession, disturbance and environmental change.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Functional trait ecology</strong></td>
<td style="text-align: left;">Uses species traits to understand why organisms respond differently to environmental conditions and contribute differently to community assembly.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Conservation ecology</strong></td>
<td style="text-align: left;">Applies metacommunity principles to improve habitat connectivity, biodiversity conservation and ecological restoration.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 9: <strong>Major developments building on the metacommunity framework.</strong></figcaption>
</figure>

</div>

<div class="callout callout-style-simple callout-tip callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Tip</span>Looking Ahead

</div>

</div>

<div class="callout-body-container callout-body">

The quantitative analyses presented in this chapter provide the foundation for understanding modern metacommunity ecology. As new datasets and analytical approaches become available, the same process-based framework can be applied to increasingly complex ecological questions across a wide range of ecosystems.

</div>

</div>

</div>

<div id="strengths-and-limitations-of-the-metacommunity-framework" class="section level1" number="6">

# <span class="header-section-number">6</span> Strengths and Limitations of the Metacommunity Framework

The metacommunity framework has become one of the most widely used approaches in community ecology because it links local ecological processes with regional spatial dynamics. Rather than describing communities as isolated units, it provides a process-based framework for explaining patterns of biodiversity across heterogeneous landscapes (Leibold et al., 2004; Leibold & Chase, 2018). Throughout this chapter, quantitative analyses were used to investigate how historical speciation, environmental selection, dispersal and ecological drift contribute to community assembly.

Like all ecological frameworks, however, metacommunity ecology has limitations. The four processes rarely operate independently, making it difficult to determine their individual contributions in natural systems. In addition, ecological patterns often depend on the spatial and temporal scale of a study, while unmeasured environmental variables and species interactions may also influence community structure (Chase et al., 2020). For these reasons, metacommunity studies typically combine multiple complementary analyses to strengthen ecological inference rather than relying on a single statistical method.

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart LR

A[&quot;Metacommunity Framework&quot;]

A --&gt; B[&quot;Strengths&quot;]
A --&gt; C[&quot;Limitations&quot;]

B --&gt; D[&quot;Links local and regional processes&quot;]
B --&gt; E[&quot;Supports hypothesis testing&quot;]
B --&gt; F[&quot;Applicable across ecosystems&quot;]

C --&gt; G[&quot;Processes overlap&quot;]
C --&gt; H[&quot;Scale dependent&quot;]
C --&gt; I[&quot;Requires multiple datasets&quot;]</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 6.1.** Summary of the strengths and limitations of the metacommunity framework. Although the framework provides a powerful process-based approach for understanding biodiversity, ecological inference is strengthened by combining multiple sources of evidence.

<div id="tbl-framework-evaluation" class="quarto-float quarto-figure quarto-figure-center anchored">

<figure class="quarto-float quarto-float-tbl figure">
<div aria-describedby="tbl-framework-evaluation-caption-0ceaefa1-69ba-4598-a22c-09a6ac19f8ca">
<table class="caption-top table" style="width:99%;">
<colgroup>
<col style="width: 41%" />
<col style="width: 57%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"><strong>Aspect</strong></th>
<th style="text-align: left;"><strong>Description</strong></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>Strength</strong></td>
<td style="text-align: left;">Integrates local and regional ecological processes within a single conceptual framework.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Strength</strong></td>
<td style="text-align: left;">Supports quantitative hypothesis testing using community, environmental and spatial data.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Strength</strong></td>
<td style="text-align: left;">Applicable across terrestrial, freshwater and marine ecosystems.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Limitation</strong></td>
<td style="text-align: left;">Multiple ecological processes often operate simultaneously, making them difficult to separate.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>Limitation</strong></td>
<td style="text-align: left;">Ecological patterns depend on the spatial and temporal scale of the study.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>Limitation</strong></td>
<td style="text-align: left;">Reliable analyses require comprehensive species, environmental and spatial datasets.</td>
</tr>
</tbody>
</table>
</div>
<figcaption>Table 10: <strong>Strengths and limitations of the metacommunity framework.</strong></figcaption>
</figure>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Critical Thinking

</div>

</div>

<div class="callout-body-container callout-body">

No single ecological framework explains every pattern observed in nature. The strength of the metacommunity framework lies in its ability to generate and test hypotheses about the ecological processes responsible for community assembly, while recognising that multiple processes often interact simultaneously.

</div>

</div>

</div>

<div id="chapter-synthesis" class="section level1" number="7">

# <span class="header-section-number">7</span> Chapter Synthesis

Throughout this chapter, we explored how metacommunity ecology provides a process-based framework for understanding patterns of biodiversity across landscapes. Rather than describing communities in isolation, the metacommunity framework recognises that community assembly results from the interaction of **historical speciation, environmental selection, dispersal and ecological drift** (Leibold et al., 2004; Leibold & Chase, 2018).

Using the CESTES database, we demonstrated how quantitative analyses can be used to investigate each of these processes. Beginning with species, environmental and spatial data, the analyses provided evidence that allowed ecological patterns to be interpreted in terms of the processes responsible for community assembly. Although no single analysis can fully explain biodiversity patterns, combining multiple complementary approaches strengthens ecological inference and improves our understanding of metacommunity dynamics (Chase et al., 2020). Metacommunity ecology continues to develop as an active research field. Increasingly, researchers are integrating ecological networks and evolutionary processes into metacommunity theory to better understand biodiversity patterns (Toju et al., 2017). Ultimately, metacommunity ecology also provides an important framework for understanding how biodiversity supports ecosystem functioning across spatial scales (Gonzalez et al., 2020).

<div class="cell" layout-align="default">

<div class="cell-output-display">

<div>

<figure class="figure">
<div>
<pre class="mermaid mermaid-js"><code>flowchart LR

A[&quot;Metacommunity Dataset&quot;]

A --&gt; B[&quot;Quantitative Analyses&quot;]

B --&gt; C[&quot;Historical Speciation&quot;]
B --&gt; D[&quot;Environmental Selection&quot;]
B --&gt; E[&quot;Dispersal&quot;]
B --&gt; F[&quot;Ecological Drift&quot;]

C --&gt; G[&quot;Process-based understanding&lt;br/&gt;of community assembly&quot;]
D --&gt; G
E --&gt; G
F --&gt; G</code></pre>
</div>
</figure>

</div>

</div>

</div>

**Figure 7.1.** Summary of the quantitative workflow presented throughout this chapter. Ecological data are analysed to investigate the four fundamental processes of metacommunity ecology, providing a process-based understanding of community assembly.

<div class="callout callout-style-simple callout-important callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Important</span>Key Take-home Messages

</div>

</div>

<div class="callout-body-container callout-body">

- Metacommunities are networks of local communities connected by the movement of organisms across landscapes.

- Community assembly is best understood through the interaction of **historical speciation, environmental selection, dispersal and ecological drift**.

- Quantitative analyses provide evidence that links observed biodiversity patterns to these ecological processes.

- No single analysis can fully explain community structure; combining multiple complementary approaches produces stronger ecological inference.

- The metacommunity framework provides an important foundation for understanding biodiversity, guiding conservation, and addressing ecological questions across terrestrial, freshwater and marine ecosystems.

</div>

</div>

<div class="callout callout-style-simple callout-note callout-titled">

<div class="callout-header d-flex align-content-center">

<div class="callout-icon-container">

</div>

<div class="callout-title-container flex-fill">

<span class="screen-reader-only">Note</span>Final Reflection

</div>

</div>

<div class="callout-body-container callout-body">

Metacommunity ecology demonstrates that quantitative analyses are more than statistical tools—they provide a means of testing ecological hypotheses and understanding the processes that shape biodiversity. By integrating ecological theory with quantitative evidence, the metacommunity framework enables ecologists to investigate not only **how** communities differ, but **why** they differ across space and time.

</div>

</div>

</div>

<div id="references" class="section level1" number="8">

# <span class="header-section-number">8</span> References

Chase, J.M. & Leibold, M.A., 2003. Ecological niches: Linking classical and contemporary approaches. Chicago: University of Chicago Press.

Chase, J.M., Jeliazkov, A., Ladouceur, E. & Viana, D.S., 2020. Biodiversity conservation through the lens of metacommunity ecology. Annals of the New York Academy of Sciences, 1469(1), pp.86–104.

Correa Ayram, C.A., Mendoza, M.E., Etter, A. & Pérez Salicrup, D.R., 2016. Habitat connectivity in biodiversity conservation: a review of recent studies and applications. Progress in Physical Geography, 40(1), pp.7–37.

Gonzalez, A., Germain, R.M., Srivastava, D.S., Filotas, E., Dee, L.E., Gravel, D., Thompson, P.L., Isbell, F., Wang, S., Kéfi, S., Montoya, J., Zelnik, Y.R. & Loreau, M., 2020. Scaling-up biodiversity–ecosystem functioning research. Ecology Letters, 23(4), pp.757–776.

Gounand, I., Harvey, E., Little, C.J. & Altermatt, F., 2018. Meta-ecosystems 2.0: rooting the theory into the field. Trends in Ecology & Evolution, 33(1), pp.36–46.

Grimm, N.B., Chapin, F.S. III, Bierwagen, B., Gonzalez, P., Groffman, P.M., Luo, Y., Melton, F., Nadelhoffer, K., Pairis, A., Raymond, P.A., Schimel, J. & Williamson, C.E., 2013. The impacts of climate change on ecosystem structure and function. Frontiers in Ecology and the Environment, 11(9), pp.474–482.

Holyoak, M., Caspi, T. & Redosh, L.W., 2020. Integrating disturbance, seasonality, multi-year temporal dynamics, and dormancy into the dynamics and conservation of metacommunities. Frontiers in Ecology and Evolution, 8, Article 571130.

Holyoak, M., Leibold, M.A. & Holt, R.D. (eds.), 2005. Metacommunities: Spatial dynamics and ecological communities. Chicago: University of Chicago Press.

Hubbell, S.P., 2001. The unified neutral theory of biodiversity and biogeography. Princeton, NJ: Princeton University Press.

Hubbell, S.P., 2005. Neutral theory in community ecology and the hypothesis of functional equivalence. Functional Ecology, 19(1), pp.166–172.

Jacobson, B. & Peres-Neto, P.R., 2010. Quantifying and disentangling dispersal in metacommunities: how close have we come? How far is there to go? Landscape Ecology, 25(4), pp.495–507.

Jeliazkov, A. & the CESTES Consortium, 2019. A global database for metaCommunity Ecology: Species, Traits, Environment and Space – Version 1.0 (CESTES v1.0). Available at: https://icestes.github.io/

Leibold, M.A., Holyoak, M., Mouquet, N., Amarasekare, P., Chase, J.M., Hoopes, M.F., Holt, R.D., Shurin, J.B., Law, R., Tilman, D., Loreau, M. & Gonzalez, A., 2004. The metacommunity concept: a framework for multi-scale community ecology. Ecology Letters, 7(7), pp.601–613.

Logue, J.B., Mouquet, N., Peter, H., Hillebrand, H. & The Metacommunity Working Group, 2011. Empirical approaches to metacommunities: a review and comparison with theory. Trends in Ecology & Evolution, 26(9), pp.482–491.

Loreau, M., Mouquet, N. & Holt, R.D., 2003. Meta-ecosystems: a theoretical framework for a spatial ecosystem ecology. Ecology Letters, 6(8), pp.673–679.

Meynard, C.N., Lavergne, S., Boulangeat, I., Garraud, L., Van Es, J., Mouquet, N. & Thuiller, W., 2013. Disentangling the drivers of metacommunity structure across spatial scales. Journal of Biogeography, 40(8), pp.1560–1571.

Petchey, O.L. & Gaston, K.J., 2006. Functional diversity: back to basics and looking forward. Ecology Letters, 9(6), pp.741–758.

Pollock, L.J., Tingley, R., Morris, W.K., Golding, N., O’Hara, R.B., Parris, K.M., Vesk, P.A. & McCarthy, M.A., 2014. Understanding co-occurrence by modelling species simultaneously with a joint species distribution model (JSDM). Methods in Ecology and Evolution, 5(5), pp.397–406.

Rudnick, D.A., Ryan, S.J., Beier, P., Cushman, S.A., Dieffenbach, F., Epps, C.W., Gerber, L.R., Hartter, J., Jenness, J., Kintsch, J., Merenlender, A.M., Perkl, R.M., Preziosi, D.V. & Trombulak, S.C., 2012. The role of landscape connectivity in planning and implementing conservation and restoration priorities. Issues in Ecology, 16, pp.1–20.

Shipley, B., Paine, C.E.T. & Baraloto, C., 2012. Quantifying the importance of local niche-based and stochastic processes to tropical tree community assembly. Ecology, 93(4), pp.760–769.

Shoemaker, L.G. & Melbourne, B.A., 2016. Linking metacommunity paradigms to spatial coexistence mechanisms. Ecology, 97(9), pp.2436–2449.

Toju, H., Yamamichi, M., Guimarães, P.R. Jr., Olesen, J.M., Mougi, A., Yoshida, T. & Thompson, J.N., 2017. Species-rich networks and eco-evolutionary synthesis at the metacommunity level. Nature Ecology & Evolution, 1, Article 0024.

Vellend, M., 2010. Conceptual synthesis in community ecology. The Quarterly Review of Biology, 85(2), pp.183–206.

Winegardner, A.K., Jones, B.K., Ng, I.S.Y., Siqueira, T. & Cottenie, K., 2012. The terminology of metacommunity ecology. Trends in Ecology & Evolution, 27(5), pp.253–254.

</div>

</div>
