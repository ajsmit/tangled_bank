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
  title: [Lecture 1: Worldmapper],
  authors: (
    ( name: [Smit, A. J.],
      affiliation: [University of the Western Cape],
      email: [] ),
    ),
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
- Limits to life in solar system.
- Earth is the only planet with life as far as we know.
- Life evolved and diversified.
- Our ancestors.
- Human societies developed during the Holocene.
- Modifcations to all life on Earth due to people's impact.
- Exceeding planetary boundaries.
- Consequences for all life on Earth, including that of plants.

]
, 
title: 
[
#strong[Content]
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
#block[
#callout(
body: 
[
This lecture will provide students with an understanding of the concept of planetary boundaries and the significant role humans play in altering Earth's environmental systems. I will offer a broad overview of how life on Earth has evolved over billions of years, and highlight the immense timescale involved in the diversification of life. I emphasise how humans, despite their relatively recent appearance, have profoundly affected the planet. By examining previous planetary-scale events, such as the Great Oxidation Event, we will explore how certain forms of life have historically altered Earth's systems and contrast these with the current impact of human activity. This will lead to a discussion on the consequences of exceeding planetary boundaries, particularly focusing on how these changes affect plant life.

]
, 
title: 
[
#strong[Aims]
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
#block[
#callout(
body: 
[
By the end of this lecture, you will be able to:

+ Explain the concept of planetary boundaries and understand why they are critical for maintaining the stability of Earth's systems, particularly those that support life.
+ Describe the evolutionary timeline of life on Earth, highlighting the immense time it took for life to diversify and comparing it to the brief period in which humans have existed.
+ Identify key planetary-scale events, such as the Great Oxidation Event, that were caused by the super-abundance of certain life forms, and explain their impact on Earth's atmosphere and ecosystems.
+ Understand how human activity differs from natural events in terms of its rapid and wide-reaching impact on planetary systems, especially through the capacity for knowledge, learning, and technology.
+ Evaluate the impact of exceeding planetary boundaries on all life, with particular emphasis on the implications for plant ecophysiology and the survival of diverse ecosystems.
+ Reflect on the role of human societies in shaping Earth's current environment, particularly during and after the Holocene, and recognise the critical need for sustainability to prevent further ecological damage.

]
, 
title: 
[
#strong[Learning Outcomes]
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
= Lecture Transcript: The Limits to Life
<lecture-transcript-the-limits-to-life>
== Introduction: Contextualising Plant Ecophysiology
<introduction-contextualising-plant-ecophysiology>
Right, today we'll get into the actual, the first portion of our real lecture content. Yesterday was an introduction to the module; today, we need to talk a bit about setting the scene within which we'll contextualise the plant ecophysiology component of your ecophysiology module.

Generally, the way I like to do this is to start by giving you a brief overview of the state of the world and the place of people in it. It's because of people that the various stresses that plants experience exist. People, because they are so numerous, exert a whole range of different influences on the planet, and I'd like to give you an overview of how that came to be.

I'll call this set of lectures or these slides 'The Limits to Life', because there are certain boundaries within which life can operate smoothly. As we move outside those boundaries, or if we exceed some of these limits, life becomes increasingly difficult for all of us, including the plants we're mostly interested in throughout this module. The exceeding of limits is brought about by human influence on the planet.

== Earth's Place in the Solar System
<earths-place-in-the-solar-system>
As you know, Earth is the third planet from the Sun, and that's of major significance. The relevance of our position in the solar system, in between Venus and Mars, is that it creates a perfect set of conditions where everything is just right for life to exist. This is not true for Venus, which is the second planet, nor Mars, the fourth planet from the Sun \[attention: Mars is the fourth planet, not the third; Earth is the third\]. Venus is closer to the Sun so it's too hot; Mars is further away and is too cold. So, just like Goldilocks, Earth is just right.

It's just right because water can exist in the three phases necessary for the existence of life: as a liquid, as ice, and as vapour --- clouds. Without water present in all three phases, the hydrological cycle as we know it could not operate. This sets a series of limits within which life exists easily, with the range of temperatures on the planet being an important parameter. Even though water exists as a liquid between $0 thin^circle.stroked.tiny upright(C)$ and $100 thin^circle.stroked.tiny upright(C)$, life is constrained to a smaller subset of that temperature range.

== Shifting Limits: Anthropogenic Change
<shifting-limits-anthropogenic-change>
This particular set of limits is shifting; it's changing and is not constant. It hasn't been constant forever, but now, in more recent times --- at least since the Industrial Revolution in the 1700s --- the rate at which those limits are changing is accelerating. That's called climate change. I'll talk a bit more about this later on in the module, and also on Thursday, when I see you again, as there's an entire module focused on climate change, if I remember correctly.

== The Earth from Space
<the-earth-from-space>
Around 1976 \[attention: The referenced "Apollo mission" was earlier; for example, Apollo 14 was in 1971\], during the Apollo mission, Carl Sagan decided to turn the Apollo spacecraft around and look back at Earth. That was the first time in human history that people could actually see Earth entirely, from outside the planet itself. It became evident that Earth is quite unique, as far as our knowledge of the solar system was concerned. Looking back at Earth, in this case across the Moon's surface, it became obvious that Earth is the only place we know of where life is able to exist.

As I've explained, that's due to water existing as a gas, liquid, and ice, and you can see that in the image: gas in the clouds, liquid water, and at the polar regions, ice (though not always visible in the image). Australia is visible on the left, the large expanse of blue is the Pacific Ocean.

That moment made people realise that Earth is the only place we know of that harbours life and, because there's only one such place, it's actually quite fragile. We need to take care of it, and be aware that this is the only place where people can live. There is a beautiful video --- I'll send you the link later --- where Carl Sagan poetically discusses the fragility of life on this planet.

== Viewing Earth as a System
<viewing-earth-as-a-system>
This module will focus on looking at Earth as a whole system, rather than on individual plants or animals. We'll discuss how the whole Earth system is able to sustain life, and the necessary conditions for life to persist. For instance, if you look down onto South Africa, with most of Africa visible on the top left, and Antarctica below, you'll see the swirling clouds of the large atmospheric gyres, which move water vapour and air around and constitute our climate systems.

One critically important aspect of the Earth is the oceans. Without the oceans, life would not exist as we know it. $70.8 thin %$ of Earth's surface is covered in ocean water, and it's the presence of these oceans that creates the conditions allowing life to persist on the rest of the planet. The oceans produce about $50 thin %$ of the global supply of oxygen, and are crucial in maintaining an even temperature gradient across the planet. Without oceans, Earth would be far hotter and daily temperature fluctuations would be much more extreme.

Ideally, we shouldn't call the planet "Earth" but rather "planet ocean", as it is predominantly covered by ocean water.

== Demographic Changes and Global Population
<demographic-changes-and-global-population>
I want to show you some figures from a website called World Mapper, which creates interesting images of world maps distorted according to the density of certain processes --- for example, population density, education levels, or carbon emissions per capita.

If we look back 2,000 years ago, in year one (according to our calendar), most people were in Southeast Asia and Europe. Some distortion in South America is due to Inca and Maya populations, but mostly, the bulk of the population was in Asia and Northern Europe. Very few people were in North America, and New Zealand was unpopulated.

Moving forward 1,500 years, we see Asia and Northern Europe remain large, with Asia expanding, but North America now growing in size, meaning the population began to rise there. Later, during the last 500 years or so, Africa saw rapid population expansion.

This is very important, as continual population growth has a huge effect on biodiversity, both regionally and globally. Human impact is thus largely a consequence of there being very large numbers of people.

At the start of the previous century, in $1900$, there were $1.56$ billion people on the planet. Let's roll forward $30$ years from now, and we expect to see $9.8$ billion people. Most of these will be in Africa. Comparisons of the maps show that Africa, previously relatively smaller, is now significantly bloated in population. As of two years ago, Africa was already very large. In $30$ years, it's even larger, with $411$ million people \[attention: The actual projected population for Africa in $2050$ is much higher, on the order of $2.5$ billion\].

Why has Africa become so overpopulated? And can we explain this pattern?

Extending to the year $2100$, Africa's population is projected to increase further, while Europe and North America are decreasing in size. Many countries, such as Canada, Australia, and New Zealand, now have negative population growth, while South Africa and some Southeast Asian countries experience continued growth.

Examining demographics, Southeast Asia and Africa --- regions experiencing rapid population growth --- also have high proportions of very young people (aged $0$--$4$ years). These populations have fewer older people, while Northern Europe and North America have more elderly than young.

== Patterns of Poverty, Urbanisation, and Education
<patterns-of-poverty-urbanisation-and-education>
In Africa and Southeast Asia, most people still live in rural areas; urban development has not proceeded at the same pace as in Europe or North America. South Africa is an exception, with more people making their lives in cities, a trend that continues to generate environmental challenges.

However, for much of Africa, people remain in rural settings. Industrial and urban development lags behind, and many areas remain undeveloped. In Africa and Southeast Asia --- the regions where population is growing most rapidly --- the majority of people live in absolute poverty. The World Bank defines absolute poverty as income less than $1.9$ US dollars per day. Most people in these regions fall below this line.

Thus, the fastest-growing populations are also the poorest. This seems counterintuitive. If people are poor, why do they have more children?

== Education and Gender Disparity
<education-and-gender-disparity>
Another graph shows that the regions with high absolute poverty and high population growth also have the lowest education levels for women, compared to men. This is crucial --- where women are uneducated, they tend to have more children. As education increases, women are empowered, and family size decreases.

But why is female education so much lower in these regions? This requires a deeper look.

== The Role of Religion and Historical Structure
<the-role-of-religion-and-historical-structure>
It's not the sole explanation, but religion plays a significant role. In large parts of Africa (particularly southern, sub-Saharan, and North Africa), Christianity is dominant, whilst in other areas, Islam has a major influence. Both Christianity and Islam, to varying degrees and in differing ways, have historically been associated with reduced access for women to education, sometimes through direct discouragement or outright prevention of female education. This disparity in education is, in my view, a central reason why population growth rates remain high in these regions.

If you disagree or want to explore further, I encourage you to consult additional sources. As I always say, never take anything I say at face value --- research and seek secondary sources.

Throughout history, many conflicts and ongoing strife in these regions have their roots in religious or cultural institutions, which more often than not have been sources of conflict rather than peace, prosperity, equality, or growth for all.

== Human Impact and the Industrial Revolution
<human-impact-and-the-industrial-revolution>
The reason for focusing so much on people is not simply to criticise religion, but to illustrate how having too many people on the planet has created wide-ranging impacts on life on Earth.

Much of this began with the Industrial Revolution in North America and Europe, which led to the excessive combustion of fossil fuels. Coal, gas, and oil were consumed in vast quantities to grow the economies of these industrialised countries. The result: excessive carbon dioxide was emitted into the atmosphere. Since the 1900s, the developed nations in the global north have contributed the most, per capita, to climate change.

South Africa, although developing, is the most industrialised country in Africa, and it ranks among the world's top contributors to carbon emissions.

Countries that are now industrialising rapidly, such as those in Africa, North Africa, Saudi Arabia, the Middle East, China, Japan, and Southeast Asia, are increasing their carbon emissions at a faster rate than the global north presently, as they attempt to close economic gaps.

== Trends in Carbon Emissions and Renewable Energy
<trends-in-carbon-emissions-and-renewable-energy>
There has been a decline in Europe's carbon emissions, particularly among Scandinavian countries which have shifted much of their energy supply to renewables. The United States has not decreased emissions as much, for various political reasons, but still shows a downward trend.

== Healthcare and Environmental Disparities
<healthcare-and-environmental-disparities>
Poorer regions with the highest population growth have the lowest levels of healthcare, highest child mortality rates, and highest rates of HIV/AIDS.

Greenhouse gas emissions remain highest in the most industrialised countries. Most carbon emissions originate from transportation, the generation of heat and electricity, industry, and other fuels. Land use changes --- such as deforestation for agriculture, afforestation, reforestation, harvest management --- can alter the flux of carbon dioxide.

Deforestation, in particular, leads to higher CO$""_2$ emissions, as forests are important carbon sinks. Agriculture is also a significant contributor to emissions, not just via CO$""_2$, but also due to methane ($upright(C H)_4$) and nitrous oxide ($upright(N)_2 upright(O)$), both potent greenhouse gases.

In developing countries, rapid population growth has meant that agriculture has expanded, leading to increased emissions of methane and nitrous oxide, often exacerbated by changes in land use. Waste recycling infrastructure is generally lacking, and more pollutants flow directly into the environment.

== Methane and Nitrous Oxide: Regional Patterns
<methane-and-nitrous-oxide-regional-patterns>
Looking at global patterns, Southeast Asia, South America, and China stand out for methane and nitrous oxide emissions. Livestock, mainly cattle, are the major methane source in South America. In Southeast Asia and China, the main sources are extensive rice farming (which under anaerobic conditions releases methane) and fertiliser use.

== Waste Management and Pollution
<waste-management-and-pollution>
In the developing world --- Africa and South America --- most sewage is not collected, but instead flows directly into the environment, aggravating pollution. In contrast, Europe and North America have substantial investment in infrastructure to treat and recycle waste, reducing environmental pollution.

== Undernourishment and Deforestation
<undernourishment-and-deforestation>
Undernourishment is highest in regions with the fastest population growth, due to a lack of resources to support large families. Again, this links back to poverty and education.

Deforestation is occurring rapidly in Southeast Asia, China, South Africa, North Africa, Brazil, Colombia, Panama, and elsewhere. Forests are cleared primarily for agriculture, agronomy, expanding residential areas and cities, and --- particularly in Africa --- to supply fuel wood for heating and cooking.

== Science Research and GDP
<science-research-and-gdp>
Scientific research is most intense in countries with the highest gross domestic product (GDP). South Africa is almost unique on the continent for its scientific output, but remains far behind countries in the global north.

== Setting the Scene for the Module
<setting-the-scene-for-the-module>
To prepare for Thursday's lecture, please read the paper 'A Safe Operating Space for Humanity' by Johan Rockström and colleagues. I've posted the paper on ICAMVA; please have a look, as it discusses the boundaries humanity must not exceed to maintain a habitable planet.

We've already described climate change in some detail, and its origins. The nitrogen and phosphorus cycles are also affected by waste management practices, and the safe limits for the nitrogen cycle have already been exceeded, moving us into dangerous territory there. We're also approaching a dangerous level of climate change, and the greatest ongoing threat to the planet is biodiversity loss, which itself is a direct result of too many people, lack of education, and the other factors I've described.

== Looking Ahead
<looking-ahead>
For the rest of the module, I'll talk about climate change, ocean acidification, the nitrogen cycle, the phosphorus cycle, global freshwater use, and biodiversity loss. Each of these issues has major consequences for how stressed plants are within the environment, as human impacts on planetary boundaries indirectly produce the environmental stresses plants experience.

Thus, we will examine how plants perceive stress, the physiological and ecological effects of stress, and how plants cope with these challenges. That will be our focus for the rest of the module.

 
  
#set bibliography(style: "../marine-biology.csl") 


#bibliography("../references.bib")

