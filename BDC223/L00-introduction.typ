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

#show: doc => article(
  title: [Lecture 0: Introduction],
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

= Basic House Keeping
<basic-house-keeping>
So before we start with this module, let me give you some background on how I'd like to proceed.

Lectures will be held on Monday, early Tuesday, and early Thursday. Fridays have an allocated practical time slot, which we may use if necessary. However, my intention is to use that slot mainly for question and answer sessions, but you'll have to plan ahead and schedule my presence during this time.

On Mondays, Tuesdays, and Thursdays, we'll have in-person lectures in the class. All of the lecture material that I'll be presenting in the class will be based on the content of various lecture slides, which I'll be displaying as I talk. These slides are available as PDFs for you to download on iKamva. During the COVID period, I also presented these lectures as a series of online pre-recorded lectures, which can also be downloaded on iKamva. They correspond to the slides you have access to. Additionally, in 2025, I have created transcripts -- I have converted all the content of the pre-recorded lectures to text and made those available on my website, Tangled Bank, under the various lectures allocated to BDC 223. Please navigate to that website for the transcripts of the pre-recorded lecture materials.

So, in total, you have lecture slides, in-person lectures, pre-recorded lectures that correspond more or less to what I'm saying in class, and textual transcripts of all of the material. All of this is available to you and should help you understand the content of BDC 223.

If there's something you don't understand, please make an appointment (as a class) to see me on Friday after 2 pm. This will give you a chance to discuss any issues that came up during the week's lectures. This setup will allow us to revisit earlier material if there are any unresolved questions.

It's largely in your hands how you want to use the Friday afternoon allocation. By default, I won't interact unless you make an appointment, and when you do so, please make sure that you're with a group of at least four or five people. I won't hold individual meetings, since often questions are shared and it's more efficient to address them together. That's why the WhatsApp group exists. Use it to coordinate which topics are unclear, post your questions there, and I can respond either as a voice note or, if needed, we can use Friday afternoons for more detailed explanations.

I hope this format works for everyone. If not, let me know and we can look at alternatives. I'll be available as much as possible on WhatsApp, so please use that. I'll definitely be available during the three lecture periods each week and, by appointment, on Friday afternoons.

= The Fourth Term BDC 223
<the-fourth-term-bdc-223>
In this fourth term, we'll mostly be discussing plants -- photosynthetic organisms, whether terrestrial or marine. Our focus will be the plant-related content equivalent to what Prof Maritz covered, but specifically on photosynthetic organisms.

Before we dive in, let's have a basic overview of the module. I've already shared the slides; you can review them. At the beginning, I've included some quotes that I find amusing or thought-provoking; you're welcome to read through those. If you'd like me to elaborate on any of them, let me know, but the goal is for them to inspire or provide some insight into the scientific mindset. For example, Richard Feynman, a physicist who died in the 1980s, believed that we're all born knowing nothing, and lifelong learning gives life meaning. That's also my view: there's always more to learn, and science is about empowering you to answer questions that haven't yet received enough thought.

There's still plenty to discover in the world, and in this module, we'll aim to generate new knowledge regarding plant biology. There are many exciting developments out there; Prof Maritz has probably pointed out some, and I share that enthusiasm, especially since my research is in the ocean as a marine biologist. My perspective and approach will focus on ocean processes, while Prof Maritz's emphasis is more on terrestrial ecosystems. Both perspectives are valuable and interconnected.

My main expectation is for you to read widely around the topics I make available. Some details will be in textbooks or other readings that I might not directly cover in lectures. Remember, exam questions won't be limited to what I've said in class; your responsibility as science students is to explore and verify information on your own.

If you master all my lecture material, it will probably get you about 70% in the exam; the rest comes from your broader reading and learning. Teaching is about directing you, but learning is your personal process. Integrate the information, connect concepts, and aim for deep understanding. That's not something I can give you. You create it for yourselves. To do that, read, interact with your peers, and engage with me (use WhatsApp for questions or alternative perspectives).

Science advances through scepticism and questioning, not authority. Always question, including me, your family, community leaders, and so on. Don't accept things as fact simply because someone says so. Develop your own thinking and remain open-minded, sceptical, and inquisitive.

One of my slides talks about the difference between knowing and understanding. Listing names of snakes doesn't mean you understand their behaviour. Go beyond memorisation to understanding why and how things happen. That's the key to deep learning.

As I said, I am a marine biologist. I work in the ocean, especially around South Africa (but also elsewhere), and my research is often ocean-centric. That doesn't mean it's irrelevant to land-based biology. I encourage you to draw general conclusions and connections across different contexts---integrate everything you learn.

Tests and assessments will focus on integration and synthesis, rather than regurgitation. You'll need to demonstrate that you can apply what you've learned to new problems.

Content for this module includes:

+ Planetary boundaries (tomorrow's topic).
+ Climate change (starting Thursday) -- its relevance to this module and biology as a whole.
+ Plant stress -- how plants experience and respond to stress.
+ The role of light in the environment, critical to plant life.
+ Heat stress and plant adaptation.
+ Plant nutrition -- their uptake of inorganic nitrogen and phosphorus, tying into global biogeochemical cycles and the carbon cycle.

There will be three practical labs dealing mainly with data analysis and calculations about plant ecophysiology: surface area/volume ratios, nutrient uptake, and light measurements. You'll get lab assignments on Mondays, due the following Monday at midnight, with calculations to be shown in spreadsheets and conclusions in a MS Word document.

You'll also write a short personal essay, due roughly two from now.

The mark allocation is similar to Prof Martitz's section: random quizzes, two class tests (typically on the Thursdays ot Fridays), and all work up to those points will be covered.

The learning outcomes for my section:

- Understand how environmental conditions (light, temperature, nutrients, etc.) affect plant distribution and interactions.
- Learn physiological mechanisms for water, nutrient, and carbon uptake in plants.
- Grasp the role of plants in the Earth system, integrating their function across contexts.
- Discuss ecophysiological processes involved in nutrient and water transport and loss.
- Examine the implications of global change and the limits of life on Earth.

Tomorrow we'll focus on planetary boundaries, starting with people and their impact as the most destructive organism on the planet, then look at how plants adapt to environmental changes.

Good luck! Let's get started.

 
  
#set bibliography(style: "../marine-biology.csl") 


#bibliography("../references.bib")

