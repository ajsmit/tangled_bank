# Capacity and concentration

The fourth BDC223 film connects individual uptake-site occupancy to the Michaelis–Menten curve. It then changes concentration, carrier abundance and binding kinetics separately, before asking students to compare populations with unequal capacities.

## Watch and teach

- [Film](concept_04_capacity_and_concentration.mp4): 2:28, 1920 × 1080, 30 fps, H.264. Written explanations; no audio.
- [Editable Blender project](concept_04_capacity_and_concentration.blend): two scenes, each with 12 independently animated uptake sites and a maintained surface concentration.
- [Lecture explanation and seven practice questions with answers](../../L08c-nutrients_visualised.qmd#sec-capacity-concentration).
- [Captions](concept_04.vtt), [poster](concept_04_poster.jpg) and narration notes below.
- Numerical model (`capacity_model.py`, private source), [generated data and event ledger](concept_04_model.json), [model checks](concept_04_model_checks.json), [native animation checks](concept_04_asset_checks.json) and [production verification](concept_04_verification.json).

The Blender project contains the membrane patches, carrier geometry, nitrate parcels and event animation. Python composes these views with text, graphs and worked comparisons. The full film therefore requires both the native project and compositor (`finish_concept_04.py`, private source) to reproduce. All artwork is original and the geometry is schematic.

| Time | What changes | Suggested intervention |
|---|---|---|
| 0:00–0:32 | Compare S = 1.25 and 20 µmol/L, holding carrier abundance and per-site constants fixed. | Count occupied sites; distinguish a fluctuating sample from a steady expectation. |
| 0:32–0:50 | Steady occupied fraction generates the uptake curve. | Explain why more nitrate helps less as occupancy rises. |
| 0:50–1:04 | Identify half-saturation and the asymptotic capacity. | At S = Ks, predict V/Vmax before reading the value. |
| 1:04–1:22 | Double low and high concentrations. | Predict whether either rate doubles. |
| 1:22–1:40 | Double identical carriers per gram. | Predict both Vmax and Ks; distinguish abundance from per-carrier turnover. |
| 1:40–1:56 | Lower Ks at equal capacity by changing the binding rate constant. | Compare rates at the same S. |
| 1:56–2:04 | Present A (Vmax 4, Ks 1) and B (Vmax 12, Ks 2). | Pause: which has greater uptake at S = 0.5? |
| 2:04–2:20 | Reveal initial slopes and exact uptake rates. | Explain why Ks alone gives an incorrect ranking. |
| 2:20–2:28 | Move towards a closed-flask experiment. | Predict the observations needed to infer a changing uptake rate. |

The final two low-concentration segments share one MP4 chapter so that seeking to the question does not skip the prediction.

## Model, units and limits

For a vacant site E and an occupied site ES:

```text
E + S -> ES         transition rate kon S
ES -> E + S        transition rate koff
ES -> E + Nin      transition rate kcat

df/dt = kon S (1 − f) − (koff + kcat) f
Ks = (koff + kcat) / kon
fsteady = S / (Ks + S)
V = Vmax fsteady
alpha = Vmax / Ks
```

The constants are kon = 0.04 L µmol⁻¹ s⁻¹, koff = 0.08 s⁻¹ and kcat = 0.12 s⁻¹, giving Ks = 5 µmol/L. Residence and waiting times are exponentially distributed. Their means are 5 seconds bound, and 20 versus 1.25 seconds waiting for binding in the two opening treatments. The probability of transport rather than release at a bound-state exit is 0.6.

This is a coarse two-state model, with inward transport energetically supported and reverse transport from the cell neglected. It does not resolve the alternating-access steps shown in concept 3. Binding, release and transport constants are selected for visible teaching events, not fitted molecular kinetics.

The opening starts at statistical steady state with constant surface nitrate. Independent random sequences drive the 12 sites in each treatment. Gold occupancy, green transport pulses and the displayed occupancy count use the same event ledger. Every binding event adds one nitrate to the bound pool; return or transport removes one. The ledger accounts for this pool exactly. The surrounding reservoir particles illustrate the 16-fold concentration contrast in equal volumes; their trajectories do not solve diffusion.

The plotted population capacity is Vmax = 10 µmol N g⁻¹ dry mass h⁻¹. With the chosen kcat, this corresponds to a carrier pool of 10/(3600 × 0.12) = 0.023148 µmol sites per gram dry mass. The 12 visible sites are a sample, not this population per gram. Site icons in the abundance comparison represent relative abundance, increasing from 12 to 24.

The film uses **dry mass**, whereas Lecture 8b's worked Manim example uses **fresh mass**. Do not compare their numerical rates without a measured conversion. Alpha has units L g⁻¹ dry mass h⁻¹.

In this model the binding dissociation constant Kd = koff/kon = 2 µmol/L differs from uptake half-saturation Ks = 5 µmol/L. Increasing kon to 0.10 lowers Ks to 2 while holding capacity fixed. Doubling site abundance instead doubles capacity while holding Ks fixed. Doubling kcat changes both: Vmax becomes 20 and Ks becomes 8. These separate interventions support the lecture's parameter interpretation.

The model has no external diffusion layer, changing energy supply, internal nutrient pools, acclimation or growth. It cannot diagnose those processes from curve shape. Surface concentration is maintained, whereas flask experiments usually sample bulk concentration. The next film will follow the changing nitrogen budget of a finite reservoir.

## Narration notes

**0:00–0:32.** Each patch has the same number of uptake sites. We maintain more nitrate at one membrane. An empty site there binds nitrate more frequently, but once occupied it has the same transport and release rate constants. Follow one site through several events, then compare the numbers occupied. Individual counts fluctuate around different steady expectations.

**0:32–0:50.** Occupied sites can transport nitrate. At steady state, the fraction occupied is S divided by Ks plus S. Multiply this fraction by the population's capacity to obtain uptake. Raising concentration fills more sites, but it does not create an unlimited capacity.

**0:50–1:04.** At S equal to Ks, half the sites are occupied on average and uptake is half the limiting capacity. The curve approaches Vmax asymptotically. All 12 sites can be occupied in a particular frame without the steady average being one.

**1:04–1:22.** Double nitrate from 1 to 2, then from 20 to 40. Uptake increases by about 71% in the first case and 11% in the second. At high concentration most sites are already occupied, so more nitrate makes less difference.

**1:22–1:40.** Now double the abundance of identical carriers per gram. Keep all per-carrier constants fixed. Capacity and uptake double, while half-saturation stays unchanged. This intervention changes abundance, not the speed of each carrier.

**1:40–1:56.** Restore the original capacity. Increase the binding rate constant while holding release and transport constants fixed. Half-saturation falls. At the same positive concentration, uptake is now greater, while the limiting capacity remains the same.

**1:56–2:04.** Which population takes up more at S equal to 0.5? A has the lower Ks. B has the higher capacity. Predict before calculating.

**2:04–2:20.** Compare Vmax divided by Ks: the initial slopes are 4 and 6. B is more responsive near zero concentration. The exact equation gives 1.33 for A and 2.40 for B at S equal to 0.5. Lowest Ks does not always mean highest uptake.

**2:20–2:28.** We have held surface concentration constant. Next, let the nitrate in a closed flask decline. We will measure concentration through time, calculate uptake from its slope, and follow the trajectory on the uptake curve.

These notes support live teaching or later narration; they are not an audio track.

## Lecture integration

The companion includes the film, derivation, controlled comparisons and seven practice questions. Lecture 8a now explains the low-concentration response without automatically attributing it to external diffusion limitation. It distinguishes half-saturation from binding affinity and limiting capacity from a rate reached at a finite concentration. Lecture 8b links the maintained-supply model to its flask measurements. Original slide images are retained.

Continue to [concept 5: Measuring a changing uptake rate](CONCEPT_05.md). Concepts 1–9 are complete locally; the complete suite is linked in [README.md](README.md).

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.

## Sources

- [IUBMB kinetic recommendations](https://iubmb.qmul.ac.uk/kinetics/ek4t6.html): definitions of limiting rate, half-saturation and the initial slope. The course uses Ks for uptake half-saturation; the recommendations use Km for the Michaelis constant.
- [Lindemann et al. (2016), Scaling laws in phytoplankton nutrient uptake affinity](https://www.frontiersin.org/journals/marine-science/articles/10.3389/fmars.2016.00026/full): context for distinguishing nutrient supply, uptake sites and affinity.

The two-state simulation and numerical comparisons are constructed for this lesson; they do not reproduce species-specific measurements from these sources.
