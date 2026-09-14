# From enrichment to oxygen loss

Film 9 completes another part of the nine-film nutrient-uptake suite.

- [Film](concept_09_enrichment_and_oxygen.mp4): 3:20, 1920 × 1080, 30 fps, H.264; written explanations, no audio.
- [Native Blender project](concept_09_enrichment_and_oxygen.blend), [captions](concept_09.vtt), [poster](concept_09_poster.jpg).
- [Lecture explanation and eight practice questions with answers](../../L08c-nutrients_visualised.qmd#sec-enrichment-oxygen).
- [Model table](concept_09_model.json), [model checks](concept_09_model_checks.json), [native checks](concept_09_asset_checks.json), [production verification](concept_09_verification.json).

## Sequence and narration

| Film time | Teaching point | Narration / discussion |
|---|---|---|
| 0:00–0:16 | Bring the pulse back to the coast | River inputs, upwelling and remineralisation can supply nutrients. A nutrient pulse supports additional production only if organisms can use it under the prevailing light, temperature, delivery and loss conditions. |
| 0:16–0:30 | More growth can mean more organic matter | Biomass can accumulate when production exceeds losses. Organic matter may settle and be respired by organisms below. The coastal scene is a conceptual sequence, not a forecast or the timing of the numerical experiments. |
| 0:30–0:44 | Predict four nutrient additions | Compare a control, nitrogen addition, phosphorus addition, and both nutrients. All start with the same biomass, light, carbon and other conditions. First predict the growth response. |
| 0:44–1:24 | Follow the factorial experiment | Each additional model biomass unit requires ten micromoles of nitrogen and one of phosphorus. Nitrogen alone increases the yield; phosphorus alone does not. Adding both increases it further as phosphorus restricts the nitrogen-enriched treatment. |
| 1:24–1:52 | A ratio is a clue, not a diagnosis | Maximum additional biomass is the smaller of nitrogen divided by ten and phosphorus divided by one in this fixed-composition model. Real composition and nutrient stores vary. Redfield and benthic-plant ratios are empirical summaries, not universal optimal requirements. |
| 1:52–2:08 | Keep organic loading the same | Both dark bottom-water examples start with six milligrams of organic carbon per litre and eight milligrams of oxygen per litre. Change oxygen renewal alone. There is no photosynthesis in these bottom-water compartments. |
| 2:08–2:52 | Respiration competes with oxygen renewal | Aerobic decomposition consumes oxygen and regenerates ammonium nitrogen. Oxygen declines much further under weak renewal. Decomposition itself slows when oxygen becomes scarce. The model accounts for initial oxygen, added oxygen and consumed oxygen. |
| 2:52–3:20 | Test the chain from enrichment to oxygen loss | Measure nutrient additions, biomass production and losses, organic matter, respiration, light and oxygen renewal. Nitrogen uptake is not the same as permanent nitrogen removal. Recycling can return acquired nitrogen to the water; harvesting or export would cross the chosen system boundary. |

## A conceptual coast, followed by two controlled models

The coastal cutaway connects river supply, upwelling, floating algal biomass, settling and decomposition. It is not a mapped bay, a fluid simulation or a forecast. River and upwelling particles indicate possible inputs; their sizes, travel times and counts do not give a nutrient flux. Settling and bloom timing are schematic. This 24-second native sequence plays over the opening 30 seconds of film.

The factorial and oxygen models are separate experiments. Organic loading in the oxygen model is specified directly, not computed from the factorial model. Their units, clocks and assumptions differ. Do not infer an environmental conversion from the arbitrary biomass units to mg organic C/L.

## N and P additions

Initial biomass B = 1 model unit in every treatment. Available nutrient amounts (µmol) are (N,P) = (5,1), (20,1), (5,2), (20,2). Every additional unit requires 10 µmol N and 1 µmol P. Carbon, light and other requirements remain sufficient; mortality, grazing, export and internal storage are omitted.

```text
Growth g = 0.9 B min[N/(1+N), P/(0.1+P)]
dN/dt = -10g
dP/dt = -g
dB/dt = g
```

Time is days. The half-saturation constants are amounts in the fixed-volume culture (1 and 0.1 µmol); the rate coefficient is 0.9 per day. At every time N + 10B = N₀ + 10 and P + B = P₀ + 1. The analytical additional-biomass ceilings are min(N₀/10,P₀) = 0.5, 1, 0.5 and 2. Numerical values at day 12 agree with these ceilings within 10⁻⁵ units.

The +N response supports initial N restriction in this model. A further response to +N+P exposes P restriction after enrichment. This is a sequential pattern; do not label every multi-nutrient effect simultaneous co-limitation. Real experiments require replication, timing and uncertainty, and may have more complex responses. [Elser et al. (2007)](https://doi.org/10.1111/j.1461-0248.2007.01113.x).

The Redfield and benthic-plant ratios are contextual empirical summaries, **not model parameters or universal optimal recipes**. The film uses a chosen N:P demand of 10:1. [Atkinson and Smith (1983)](https://doi.org/10.4319/lo.1983.28.3.0568).

Native scene 2 lasts 24 seconds, mapping to 12 model days. Plant scale cubed follows B/B₀, preserving shape while changing the schematic biomass. The final film holds the starting cultures through second 44 and maps seconds 44–78 to the experiment, then holds the final state to second 84.

## Dark bottom-water oxygen model

Each constant-volume compartment begins with D = 6 mg organic C/L and O = 8 mg O₂/L. Oxygen source concentration O* remains 8 mg/L. The oxygen-renewal coefficient k is 0.08 or 1.2 per day. D is the organic carbon remaining, not total organic matter mass. Organic C:N is set to 8 mol/mol. Released N is expressed as µmol ammonium-N/L.

```text
Aerobic carbon decomposition r = 0.22 D O/(0.5 + O)    [mg C/L/day]
dD/dt = -r
dO/dt = k(8 - O) - (32/12)r                         [mg O₂/L/day]
dN_regen/dt = (1000/96)r                            [µmol N/L/day]
dO_used/dt = (32/12)r
dO_added/dt = k(8 - O)
```

The oxygen-demand factor assumes 1 mol O₂ per mol organic C oxidised; thus the initial carbon has an ultimate demand of 16 mg O₂/L. The conversion to regenerated N uses 12 g C/mol and the chosen C:N ratio of 8. These choices are illustrative stoichiometry, not fitted sediment chemistry. Oxygen scarcity reduces aerobic decomposition; no hard clipping creates negative O₂ or destroys the oxygen budget.

The invariants are O + O_used − O_added = 8 mg/L and (1000/96)D + N_regen = 62.5 µmol N/L. Minima over 12 days are about 0.48874 and 6.10990 mg O₂/L for weak and stronger renewal. Realised decomposition differs because O differs, despite identical initial loading and rate constants.

This model isolates **oxygen exchange**. It does not separately advect D or regenerated N with replacement water. Do not interpret k as a measured bulk flushing rate, velocity, or predicted current. It is an idealised ventilation/oxygen-transfer term that makes the supply-versus-consumption comparison explicit. There is no photosynthesis, nitrification, anaerobic metabolism, variable stratification or changing solubility. Such processes would be needed for a complete coastal forecast. [Diaz and Rosenberg (2008)](https://doi.org/10.1126/science.1156401).

Native scene 3 lasts 24 seconds, mapping to 12 model days. Total organic-marker volume follows remaining D; dissolved-O₂ marker volume follows O. Blue spheres represent dissolved O₂, not gas bubbles. Film seconds 112–128 hold the initial comparison for prediction, then 128–164 map to 0–12 days. The graph and native values share this mapping, followed by a final hold.

## Recycling and removal

Uptake moves N into biomass. Organic N can return to ammonium through remineralisation; nitrification changes inorganic form; nitrogen fixation introduces fixed N from N₂. Denitrification and anammox can return fixed N to N₂. These are conceptually distinguished in the lecture; only aerobic remineralisation is calculated in the oxygen model. Harvesting, export and burial need separate boundary and flux accounts. Neither a fall in water DIN nor high seaweed uptake proves permanent ecosystem removal.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.
