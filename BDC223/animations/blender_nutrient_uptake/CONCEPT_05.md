# Measuring a changing uptake rate

The fifth BDC223 film follows nitrate from a finite water reservoir into the seaweed. One model supplies the nitrogen budget, depletion curve, accumulated uptake, interval calculations and movement along the Michaelis–Menten curve. A second pulse, a biomass comparison and a sampling counterexample test students' explanations.

## Watch and teach

- [Film](concept_05_measuring_uptake.mp4): 3:24, 1920 × 1080, 30 fps, H.264. Written explanations; no audio.
- [Editable Blender project](concept_05_measuring_uptake.blend): an animated flask and a separate row of five matched flasks.
- [Lecture explanation and eight practice questions with answers](../../L08c-nutrients_visualised.qmd#sec-measuring-uptake).
- [Captions](concept_05.vtt), [poster](concept_05_poster.jpg) and narration notes below.
- [Illustrative five-minute intervals](concept_05_illustrative_samples.csv), [model and event data](concept_05_model.json), [model checks](concept_05_model_checks.json), [native checks](concept_05_asset_checks.json) and [production verification](concept_05_verification.json).

This uses the existing Lecture 8b Manim example's parameters, including its fresh-mass basis. It can be used alongside that film. Concept 4's separate dry-mass capacity example is not numerically interchangeable with this one.

| Viewing time | Teaching point | Suggested pause |
|---|---|---|
| 0:00–0:14 | Add nitrate, mix, then define experimental time zero. | How much N is present in 0.50 L at 25 µmol/L? |
| 0:14–0:46 | Follow 60 experimental minutes. Water N falls and acquired N rises. | Where does the disappearing nitrate go? |
| 0:46–1:06 | Replay concentration and accumulated uptake with moving tangents. | Can accumulated uptake be high while current uptake is slow? |
| 1:06–1:28 | Calculate the first five-minute interval step by step. | Which operation changes concentration into amount? |
| 1:28–1:52 | Connect time, concentration and uptake rate on two different graphs. | Why does advancing time move the V–S point leftwards? |
| 1:52–2:12 | Compare five starting concentrations in matched flasks. | How does this differ from successive samples from one flask? |
| 2:12–2:24 | Predict the effect of restoring nitrate at 60 experimental minutes. | Predict the slope, V–S position and accumulated amount. |
| 2:24–2:46 | Restore S to 25 and follow 20 further experimental minutes. | Does the acquired nitrogen reset when a second pulse arrives? |
| 2:46–3:02 | Double biomass while holding the per-gram uptake response fixed. | Compare initial per-gram and whole-flask rates. |
| 3:02–3:16 | Withdraw a water sample in a separate example with no uptake. | Can water N decrease while concentration stays constant? |
| 3:16–3:24 | Introduce the next comparison: thallus form. | How much surface is exposed by a given amount of tissue? |

## Model and units

The initial conditions are S0 = 25 µmol N/L, water volume = 0.50 L, fresh mass = 4.5 g, Vmax = 6 µmol N g⁻¹ fresh mass h⁻¹ and Ks = 5 µmol N/L. Time in the model is minutes.

```text
V(S) = Vmax S / (Ks + S)
dS/dt = −M V(S) / (60 volume)
dU/dt = M V(S) / 60
Water N = volume S
Water N + U = total added N
```

Between pulses the integrated equation is:

```text
S − S0 + Ks ln(S/S0) = −M Vmax t / (60 volume)
```

The Python model evaluates its positive solution using Lambert W. Independent integration advances both S and U, rather than defining the second compartment as a residual. The checks compare these solutions, the integrated balance, nitrogen conservation, pulse continuity and the doubled-biomass response.

Initially there are 12.50 µmol N in the water. At 60 minutes S = 0.07456844 µmol/L, acquired N is 12.46271578 µmol and current uptake is approximately 0.0882 µmol N g⁻¹ fresh mass h⁻¹. The added amount needed to restore S to 25 is 12.46271578 µmol. The total introduced then becomes 24.96271578 µmol; U remains continuous through the addition and V returns to 5.00.

The model assumes negligible volume change from stock addition and no measurable withdrawal in the main depletion run. It is well mixed, with adequate delivery for the specified uptake response. Light, temperature, fresh mass, volume and uptake parameters remain fixed. Reverse flux, microbial removal, adsorption, internal feedback, growth and transient binding kinetics are omitted. Restored uptake is therefore an instantaneous model prediction, not a universal response time for real tissue.

The original seaweed nitrogen is outside the budget of newly added N. Green markers indicate acquired nitrogen, without specifying storage, assimilation or growth.

### Native parcels and the quantitative budget

The first pulse contains 125 representative parcels of 0.1 µmol each. The second activates another 124 of the same weight and one smaller final parcel. When cumulative uptake passes a parcel's midpoint, its gold water representation switches to a green tissue representation. The sum of active parcel weights equals introduced N; the represented water and tissue amounts each approximate their continuous values to within 0.05 µmol.

Small residual dissolved amounts can therefore be present even after the final gold parcel of a pulse disappears. Exact numerical labels and graphs use the continuous model. Parcel paths illustrate transfer and mixing; they do not solve fluid flow, diffusion or atomic transport. The opening addition and mixing sequence has no quantitative uptake clock.

The saved Blender scene contains the model time, concentration, rate and three budget quantities as animated custom properties on **Nitrogen budget**. All 1,800 post-mixing native frames have been checked against the source data, covering 450,000 parcel-frame states. The retained markers do not reset when the second pulse arrives.

### Intervals, independent flasks and sampling

The first five-minute interval gives a mean rate of 4.9324222 µmol N g⁻¹ fresh mass h⁻¹. The plotted interval concentrations use the mean of the two endpoints. These paired averages need not lie exactly on the instantaneous curve, especially over long intervals. Concentrations, amounts and rates are rounded only for display.

The multiple-flask illustration uses initial concentrations 1, 3, 5, 10 and 25 µmol/L. Initial model rates are 1.00, 2.25, 3.00, 4.00 and 5.00 in the same per-gram hourly units. These are illustrative starting slopes, not measured endpoint rates from a long incubation or independent experimental data. Dot counts in this row show relative concentration at a lower display density than in the main flask sequence; use the concentration labels and volume to calculate amounts.

For the biomass intervention, increasing mass to 9 g doubles the initial whole-flask uptake from 22.5 to 45.0 µmol N/h. Initial V per gram stays 5.00, and initial available N stays 12.50 µmol. At matched S, per-gram rates remain equal; at matched elapsed time, the flasks have different S. The time to reach Ks falls from 31.16354 to 15.58177 minutes.

The sampling example is a separate inventory at S = 10 µmol/L. Removing 0.010 L from 0.50 L exports 0.10 µmol N. The remaining 4.90 µmol in 0.49 L still gives S = 10. No uptake occurs during this illustrative withdrawal. In experiments with appreciable sample losses, include remaining water N, exported sample N and acquired N in the balance, and record any inputs or replacement water.

## Narration notes

**0:00–0:14.** The nitrate addition is small in volume but known in amount. After mixing, we define time zero. Concentration multiplied by water volume gives 12.50 micromoles of newly added N. We are not measuring all the nitrogen already in the seaweed.

**0:14–0:46.** Watch gold disappear from the water and green appear in the tissue. The concentration curve becomes shallower while the amount acquired increases. The numerical inventory closes throughout. The no-seaweed control stays constant in this ideal model.

**0:46–1:06.** Compare the moving tangents. A negative slope in water concentration corresponds to a positive slope in accumulated uptake. Late in the experiment the seaweed has acquired much of the pulse, but it is taking up little at that instant.

**1:06–1:28.** Subtract the two concentrations. Multiply by litres to get an amount, divide by grams to standardise biomass, and divide by elapsed hours to get the hourly rate. A five-minute interval is five divided by sixty hours.

**1:28–1:52.** The two graphs have different horizontal axes. Time advances while concentration falls. On the uptake graph this carries us left and down. Blue circles are estimates from intervals; the coral point follows the instantaneous model.

**1:52–2:12.** These flasks begin at different concentrations. Their initial slopes provide different positions on the uptake curve. In the depletion experiment, we instead visit concentrations through time in the same flask. Tissue differences and changes in tissue condition need attention in either design.

**2:12–2:24.** Restore the concentration after uptake has slowed. Predict the slope, the uptake-curve position and the acquired amount before continuing.

**2:24–2:46.** The rate returns to its initial value because we have restored concentration without changing uptake properties. The blue jump records an input, not uptake. Acquired N remains in the tissue, while total added N increases. Measure depletion slopes between additions.

**2:46–3:02.** Twice the biomass means twice the initial whole-flask uptake, with the same initial rate per gram. It takes less time to deplete the same initial nitrogen supply.

**3:02–3:16.** Removing a water sample exports nitrogen. In this well-mixed example it also removes water in the same proportion, so concentration stays unchanged. Account for both the exported nitrogen and the remaining volume.

**3:16–3:24.** We have standardised uptake per gram. Now ask how much surface that gram exposes to moving water. The next film compares thallus forms under matched conditions.

These notes support live explanation or later narration. They are not an audio track.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.

## Lecture integration and sources

The companion adds eight practice questions and their answers. Lecture 8a now distinguishes the two control treatments, corrects the claim that a raw concentration slope is already V, and links sampling losses to the nutrient inventory. Lecture 8b links this film and explains when its constant-volume interval calculation applies. Original slide images and the existing Manim film are retained.

The experimental context is [Smit (2002), Nitrogen uptake by Gracilaria gracilis](https://doi.org/10.1515/BOT.2002.019). Its nitrate response and perturbation experiments motivate the lesson; the film's parameters are illustrative. All artwork and calculations were constructed for this teaching suite.

Continue to [concept 6: Form and nutrient supply](CONCEPT_06.md). Concepts 1–9 are available locally. The complete suite is linked in [README.md](README.md).
