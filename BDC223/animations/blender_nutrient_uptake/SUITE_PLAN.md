# Nutrients: from seawater to growth

This production plan now records the completed nine-film Blender companion to BDC223 Lectures 8a and 8b. All nine films, editable projects, teaching notes and the lecture companion are available locally. The existing Manim film remains available. See the [film index](README.md) and [completion report](SUITE_REVIEW.md).

The suite is ready for the separate deployment step. Nothing in this production run has been published.

## Sequence

| Film | The explanation made visible | Student intervention or prediction | Lecture connection |
|---|---|---|---|
| **1. From the flask to the surface** | A nitrate pulse enters seawater; the view moves to the thallus. Random molecular movement and removal at an absorbing surface produce a concentration gradient. Distinguish bulk concentration from surface concentration. | Does a molecule need to know where the seaweed is? What maintains the gradient? | 8a: pathways, external phase, concentration gradients and boundary layer |
| **2. Moving water and diffusion limitation** | Compare weak and stronger water movement at matched bulk concentration and unchanged uptake machinery. Show a velocity profile separately from the concentration profile. Follow downstream depletion, then reduce capacity in both treatments through a sequence of steady comparisons. | Predict the effect of stirring when delivery restricts uptake, then when uptake capacity is restrictive. | 8a: water movement, diffusion limitation and apparent kinetic parameters; [film and guide](CONCEPT_02.md) |
| **3. Crossing the membrane** | Separate delivery, lipid permeability and electrochemical favourability. Show alternating access, an ATP-driven pump and a separate symporter. Add the energies, then stop pumping and let the gradient dissipate. | Does uptake have to stop instantly when pumping stops? Does a saturating curve establish that transport is active? | 8a: active, passive and facilitated transport; [film and guide](CONCEPT_03.md) |
| **4. Capacity and concentration** | Compare 12 uptake sites at two maintained surface concentrations; derive the curve from steady occupancy. Double concentration, double identical carriers per gram, then lower Ks at equal capacity. | Compare low Ks at equal and unequal Vmax; calculate the effect of concentration doubling; distinguish abundance from turnover. | 8a: Michaelis–Menten kinetics and affinity; [film and guide](CONCEPT_04.md) |
| **5. Measuring a changing uptake rate** | Follow a conserved nitrate pulse through water and tissue; connect S(t), accumulated uptake, tangents and V–S. Compare multiple flasks, restore nitrate, double biomass and isolate sample withdrawal. | Calculate an interval rate; predict a second pulse; distinguish concentration, amount and rate; account for sampled N. | 8b: experiments, normalisation and kinetic interpretation; [film and guide](CONCEPT_05.md) |
| **6. Form and nutrient supply** | Compare equal-volume sheets and cylindrical axes; unfold their surfaces; distinguish tissue depth from external delivery and similar-shape scaling. Model canopy renewal at fixed area, then an area sweep under fixed input. | Which comparisons isolate surface area, delivery and uptake activity? What do hairs establish about affinity? | 8a: surface area to volume, functional form and morphology; [film and guide](CONCEPT_06.md) |
| **7. Uptake, storage and growth** | Follow N into internal pools and assimilation, then into new tissue. Compare depleted and replete tissue; distinguish transient surge uptake from steady capacity and later external limitation. | Can uptake continue without immediate growth? Can growth continue after external N falls? | 8a: luxury consumption, nutritional history, surge phases, uptake–growth coupling ; [film and guide](CONCEPT_07.md) |
| **8. A changing physiological environment** | Light and temperature change demand and processing capacity within species-specific limits. Compare nitrate and ammonium responses and possible interactions. | Design a matched experiment to distinguish delivery from a change in physiology. | 8a: light, photoperiod, temperature, nutrient type, biphasic responses ; [film and guide](CONCEPT_08.md) |
| **9. From limitation to eutrophication** | Link nutrient enrichment to growth, changing N:P supply and demand, organic matter, respiration and oxygen loss. Connect river inputs, upwelling and recycling to the pulse already understood. | Test N, P and combined addition; explain why nutrient supply alone does not guarantee a bloom. | 8a: limiting nutrients, stoichiometry, nitrogen cycle, enrichment and eutrophication ; [film and guide](CONCEPT_09.md) |

## Shared visual conventions

- Gold parcels represent dissolved nitrate-N; green represents the seaweed and retained N. Parcels are enlarged visual representatives, not ions drawn to scale.
- Use a generic thin green thallus, not a claimed reconstruction of *Gracilaria gracilis*. The latter provides an empirical comparison, not the anatomy in the film.
- Begin at the organism or experiment, then move to the mechanism and finally to a measurable response.
- Distinguish elapsed experimental time from viewing time. Numerical examples are labelled illustrative and are derived from a stated model.
- Each film ends with a prediction that the next film or an accompanying exercise can test.
- Use short captions and provide a narration script. This first version has no audio so it can accompany the lecturer's own explanation.

## Scientific distinctions that the suite must preserve

1. Molecular motion continues in water with no imposed flow. A diffusion gradient describes a population-level imbalance in flux, not a force guiding individual molecules.
2. The concentration boundary layer is a region in the water. It is not a solid skin. Its effective thickness depends on transport and geometry; it is distinct from the hydrodynamic boundary layer.
3. External diffusion is driven by the difference between bulk and surface dissolved concentration. It must not be explained using total tissue N as the other endpoint.
4. Diffusive delivery to a surface and transport across a membrane are separate processes. Osmosis is water movement. For ions, membrane transport depends on electrochemical potential, not concentration alone.
5. Water movement can improve delivery without changing intrinsic uptake capacity in a short, controlled comparison. Fitted whole-organism parameters can nevertheless depend on experimental conditions; flow does not prove a universal invariant Vmax.
6. A hyperbola or a linear segment does not by itself identify the molecular transport mechanism. A high-S linear response is not a universal property of ammonium, and a saturating response does not prove active transport.
7. Uptake, assimilation and growth are distinct. Internal reserves can uncouple them; N:P ratios and Q10 are context-dependent descriptions, not universal exact constants.
8. The scope and measured results of Smit (2002) should remain distinct from broader hypotheses. For example, ammonium suppressed nitrate uptake in those experiments; the result was not a universal complete shutdown until ammonium was absent.

## Sources and evidence

- [Lecture 8a](../../L08a-nutrient_uptake.qmd), especially “Pathways and barriers”, “The boundary layer concept”, “Water movement”, and the uptake mechanisms and modifiers.
- [Lecture 8b](../../L08b-nutrients_michaelis_menten.qmd): the perturbation and multiple-flask experiments, worked units and Michaelis–Menten model.
- [Smit (2002), *Nitrogen uptake by Gracilaria gracilis*](../../../docs/Smit_2002.pdf): the course's experimental source, including the interaction between water movement and low nutrient availability. [Public copy](https://tangledbank.netlify.app/docs/Smit_2002.pdf).
- [Lindemann et al. (2016), *Scaling laws in phytoplankton nutrient uptake affinity*](https://www.frontiersin.org/journals/marine-science/articles/10.3389/fmars.2016.00026/full): explicit separation of diffusive supply, uptake sites and affinity.
- [Blender rendering documentation](https://docs.blender.org/manual/en/latest/advanced/command_line/render.html) and [EEVEE technical documentation](https://developer.blender.org/docs/features/eevee/).

## Production acceptance

Deliver an editable Blender project, its Python builder, a playable captioned film, a poster, a transcript and teaching notes. Verify the concentration calculation against an independent solver and its steady-state solution. Inspect representative frames and transitions from the actual rendered film. All nine films now meet the local production checks; see the per-film records and suite report.
