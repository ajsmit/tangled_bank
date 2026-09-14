# Form and nutrient supply

The sixth film compares different forms at equal tissue volume, unfolds their surfaces, and connects area to uptake. A second comparison keeps surface area constant while restricting water renewal. Students can then distinguish geometric, delivery and physiological explanations.

## Watch and teach

- [Film](concept_06_form_and_supply.mp4): 3:10, 1920 × 1080, 30 fps, H.264; written explanations and no audio.
- [Editable Blender project](concept_06_form_and_supply.blend): four named scenes with ordinary keyframes.
- [Lecture companion and eight practice questions with answers](../../L08c-nutrients_visualised.qmd#sec-form-and-supply).
- [Captions](concept_06.vtt), [poster](concept_06_poster.jpg), [geometry table](concept_06_geometry.csv).
- Model (`form_model.py`, private source), [model data](concept_06_model.json), [independent model checks](concept_06_model_checks.json), [native geometry checks](concept_06_asset_checks.json), [production verification](concept_06_verification.json).

| Film time | Explanation | Pause or prediction |
|---|---|---|
| 0:00–0:18 | Three geometries contain the same tissue volume and assumed fresh mass. | Predict the area ranking. What must be matched for equal volume to imply equal mass? |
| 0:18–0:36 | Count all surfaces; at 0:24 unfold the faces, edges, walls and ends at one area scale. | What is missing if we count only one sheet face? |
| 0:36–0:56 | Hold areal flux constant and multiply by area to obtain total uptake. | Does higher uptake per gram imply faster transporters? |
| 0:56–1:14 | Compare maximum interior-to-surface distances at a common thickness scale. | Which distance is inside tissue? Where is the external concentration layer? |
| 1:14–1:26 | Double all dimensions of a similar shape. | Explain area ×4, volume ×8 and SA:V ×0.5. |
| 1:26–1:40 | The same eight axes experience different prescribed water renewal. | Predict local concentration and uptake before revealing them. |
| 1:40–2:06 | Compare steady concentrations and uptake, then close both N budgets. | Why does identical area give different uptake? |
| 2:06–2:26 | Add surface under a fixed incoming supply. | Can uptake exceed 0.300 µmol N/h at steady state? |
| 2:26–2:46 | Introduce hyaline hairs and separate candidate mechanisms. | What observations would distinguish area from delivery? |
| 2:46–3:02 | Compare uptake per surface and per gram under matched conditions. | If Q/A differs at matched local concentration, is area alone sufficient? |
| 3:02–3:10 | Connect acquired N to internal pools and new tissue. | Can storage separate uptake from immediate growth? |

## Geometry and its limits

The shapes have equal nominal tissue volume, 1 cm³. They are idealised pieces, not reconstructions of named species. The eight fine cylinders are separate branch segments: there are no junctions, overlaps or holdfasts in the area calculation. All cylinder ends and all six sheet faces count as exchange surface. In living thalli, contact, attachment, inactive tissue and anatomical specialisation can change that accounting.

| Shape | Dimensions in cm | Volume, cm³ | Area, cm² | Maximum interior distance, cm |
|---|---|---:|---:|---:|
| Sheet | 2.5 × 4.0 × 0.1 | 1 | 21.3 | 0.05 |
| Eight fine axes | r = 0.1; L = 1/(8πr²) each | 1 | 20.5026548 | 0.1 |
| Thick axis | r = 0.5; L = 1/(πr²) | 1 | 5.5707963 | 0.5 |

The rectangular surface formula is A = 2(lw + lh + wh). Cylinders use A = n(2πrL + 2πr²), with tissue volume nπr²L. The unrounded cylinder lengths produce exactly equal analytical volumes. The Blender cylinders use 128 sides: independent checks of their closed meshes show area and volume differ from the smooth-cylinder values by less than 0.05%. Rotations preserve both quantities. Geometry checks cover three frames in each quantitative scene and 24 group comparisons.

The unfolded patches use 90 pixels per centimetre in both directions. Green rectangles show broad faces or lateral cylinder walls; gold shows edges or circular ends. Patch gaps and their rearrangement are for counting, not an anatomical dissection. The internal-distance diagram uses 400 pixels per centimetre; the sheet section is cropped laterally. No internal transport coefficient, diffusion time or cellular arrangement is inferred from these distances.

The similar-shape enlargement is a separate comparison that does not conserve tissue volume. Uniformly scaling lengths by two multiplies area by four and volume by eight. This is not a rule that any increase in one measured length must halve SA:V.

## Uptake and the two normalisations

At a uniform areal flux j, total uptake is Q = Aj and uptake per gram is V = Q/M. For common density ρ, A/M = (A/tissue volume)/ρ. We assume ρ = 1 g fresh mass/cm³, so each opening example contains 1 g fresh mass. Equal volumes need not have equal masses if their densities differ.

The illustrative areal uptake law is:

```text
j(C) = jmax C / (K + C)
jmax = 0.10 µmol N cm⁻² h⁻¹
K = 5 µmol N/L
```

With maintained surface concentration C = 10, j = 0.0666667. Total rates are 1.420, 1.3668437 and 0.3713864 µmol N/h. Per-gram rates have the same numbers only because the examples each contain 1 g. The capacities from earlier films are separate examples, with different normalisations; do not substitute this areal jmax into a per-gram equation.

Holding activity per cm² constant does not hold capacity per gram constant when area per gram changes. Gold particles illustrate equal arrival density per surface area, with positions sampled over mesh triangle areas. They are not a molecular simulation, measured uptake events or a conserved finite pulse. Counts, paths and animation speed do not define the numerical flux.

## Canopy renewal and nitrogen conservation

The second model has one locally well-mixed water compartment, W = 0.05 L. Internal mixing and exchange with the external reservoir are distinct: a compartment can be internally mixed but renewed slowly. Incoming concentration Cb is maintained at 10 µmol/L. F is the exchanged water volume per hour; it is not a current velocity or a diffusion coefficient.

```text
Q = A j(C)
d(W C)/dt = F Cb − F C − Q
At steady state: F(Cb − C) = Q
```

Both arrangements contain the same eight axes, A = 20.5026548 cm², with unchanged areal uptake parameters. The local-delivery assumption sets the concentration experienced by active surfaces equal to the mixed-compartment concentration. There is no separate spatial surface boundary-layer calculation.

| Treatment | F, L/h | C, µmol/L | Input, µmol/h | Output, µmol/h | Uptake, µmol/h |
|---|---:|---:|---:|---:|---:|
| Freely renewed | 1 | 8.698110 | 10 | 8.698110 | 1.301890 |
| Sheltered | 0.03 | 0.779764 | 0.300 | 0.023393 | 0.276607 |

Spacing illustrates a possible sheltering mechanism. F is prescribed, not calculated from geometry. We do not solve fluid dynamics, turbulence, boundary-layer overlap or nutrient gradients between individual axes. Particle paths are schematic throughflow positioned in front of the axes; particle counts are not quantitative concentrations. There is no fitted mapping from the displayed speed to F. The numbers above come exclusively from the compartment balance.

The area sweep changes A from 0 to 50 cm² at F = 0.03 L/h. These are separate steady states, not a growth trajectory. It neither holds biomass constant nor calculates a per-gram response. Q approaches the incoming supply FCb = 0.300 µmol/h from below. This ceiling applies to the steady model without another source; transient uptake can also draw down N already present in the water.

The positive quadratic solution is checked against independent root finding for 25 area/renewal combinations. A separate BDF integration advances water N, acquired N and exported N and checks both the final concentration and total introduced N. Maximum steady budget error is about 1.5 × 10⁻¹⁴ µmol/h; final concentrations agree within 2.1 × 10⁻¹¹ µmol/L.

## Narration notes

**0:00–0:36.** Begin with equal tissue volume. Count every exposed face, then unfold the surfaces at one scale. Thinness and radius explain the numerical areas. A shape name does not supply a number by itself.

**0:36–0:56.** Each square centimetre has the same uptake activity and nutrient concentration. Multiplying that flux by surface area gives total uptake. Dividing by mass answers a different reporting question.

**0:56–1:26.** The pink distance lies inside tissue. Delivery through surrounding water is separate. Now enlarge a similar shape: the tissue volume changes, so this is not the opening equal-volume experiment.

**1:26–2:06.** Keep the eight axes, area and uptake activity. Restrict renewal and predict what happens. At steady state, incoming N has two destinations: it either leaves in the outgoing water or is taken up. Check the subtraction in each treatment.

**2:06–2:26.** Add more active area under fixed renewal. Local concentration falls and total uptake approaches the incoming supply. Extra surface cannot produce nitrogen.

**2:26–2:46.** Hairs invite several explanations. Decide which observations would distinguish a change in surface area from a change in local supply or chemistry. Producing hairs does not itself establish altered transporter affinity.

**2:46–3:10.** Compare uptake per surface as well as per gram, with local concentrations and tissue condition matched. Next, follow the acquired nitrogen into internal pools and growth.

These are prompts for live narration, not an audio track.

## Sources and lecture integration

[Wallentinus (1984)](https://link.springer.com/article/10.1007/BF02180189) reports nutrient-uptake differences among Baltic macroalgae associated with morphology. These species comparisons motivate the exercise; they do not supply this model's parameter values or isolate geometry from physiology.

[Lichtenberg, Nørregaard and Kühl (2017)](https://pmc.ncbi.nlm.nih.gov/articles/PMC5378137/) measured oxygen boundary-layer structure around *Fucus* hair tufts, including local thickening. The film uses this to qualify a universal hair-thinning explanation, not to infer nitrate kinetic parameters.

Lecture 8a now links the form comparison, distinguishes area from areal activity and replaces the hairline-hair/intrinsic-affinity claim. The companion gives eight practice questions and answers. All existing slide-image references remain. Lecture 2 and the other films are retained.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.
