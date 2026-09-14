# Concept 3: Crossing the membrane

[Watch the film](concept_03_crossing_membrane.mp4) · [Editable Blender project](concept_03_crossing_membrane.blend) · [Lecture and practice](../../L08c-nutrients_visualised.qmd#sec-crossing-membrane)

The film is 2:16, 1920 × 1080, 30 fps, H.264, with written explanations, a [caption track](concept_03.vtt) and no audio. It continues from external delivery to the next boundary: the plasma membrane. [Production verification](concept_03_verification.json) records the model, native animation, rendered video and local lecture checks.

## Teaching sequence

| Time | What students see | Explanation or prediction |
|---|---|---|
| 0:00–0:12 | Nitrate above a porous cell wall and lipid bilayer. | Reaching the membrane is different from crossing it. |
| 0:12–0:26 | The wall moves out of the close-up. Neutral CO2 markers cross lipid; nitrate remains outside. | Permeability depends on charge, size and chemical properties. Paths are schematic and include both directions. |
| 0:26–0:46 | Concentration and electrical energy contributions add as voltage changes. | Hold nitrate concentrations constant. A negative interior opposes entry of negative nitrate. |
| 0:46–1:04 | A generic carrier binds solute, encloses it, opens inside and releases it. | Alternating access can mediate passive transport. The protein stays embedded in the membrane. |
| 1:04–1:28 | An ATP-driven pump exports protons; a separate symporter brings protons and nitrate inward. | Distinguish primary and secondary active transport. Define symport within one protein. |
| 1:28–1:44 | The energy changes of one nitrate and two protons are added. | Uphill nitrate movement can be supported by downhill proton movement when the processes are coupled. |
| 1:44–2:00 | Pumping stops; an imposed decay of voltage and pH difference changes the cycle energy. | Does the driving gradient disappear immediately? Do not read an energy graph as an uptake-rate graph. |
| 2:00–2:16 | Two matching saturating curves and a prediction before the answer appears. | A plateau does not distinguish a passive carrier from an energy-coupled carrier. |

The central questions are: **What route is available? What favours movement? What establishes the rate?** These questions concern different properties. The last leads into concept 4, finite capacity and the Michaelis–Menten response.

## Thermodynamic model

The Python model (`membrane_model.py`, private source) calculates the free-energy change for outside-to-inside transport:

```text
ΔG = RT ln(Cinside / Coutside) + z F (ψinside − ψoutside)
```

Temperature is 298.15 K; R and F come from SciPy's physical constants. Voltages are converted from mV to V and energies reported in kJ/mol. Concentration ratios approximate activity ratios. The nitrate concentrations mean free dissolved nitrate on the two sides of the membrane, not total tissue N or pooled vacuolar and cytosolic concentrations.

Negative ΔG favours inward movement; positive ΔG opposes uncoupled inward movement. This is a thermodynamic calculation, not a flux law. Permeability, transporter abundance, binding, turnover and downstream processing are not calculated from the energy sign.

### Change only voltage

Outside nitrate is 100 µmol/L and inside nitrate is 10 µmol/L. For z = −1, the concentration contribution is −5.708 kJ/mol. Sweeping the voltage from 0 to −80 mV raises the electrical contribution to +7.719 kJ/mol. The total then becomes +2.011 kJ/mol. At −59.159 mV the total is zero, despite unequal concentrations.

The independently rearranged Nernst equation checks this zero. Further checks reverse the charge sign and confirm that the voltage term vanishes for a neutral solute.

### Couple two protons to nitrate

The second example uses outside nitrate 10 µmol/L, inside nitrate 1000 µmol/L, pH 6 outside and 7 inside, and voltage −80 mV. These are **generic proton-coupled teaching conditions, not seawater values or a species fit**. The exact 2 H+:1 nitrate ratio is an illustrative choice, not a universal seaweed stoichiometry.

| Inward movement | Chemical (kJ/mol) | Electrical (kJ/mol) | Total (kJ/mol of cycles) |
|---|---:|---:|---:|
| One nitrate | +11.416 | +7.719 | +19.135 |
| Two protons | −11.416 | −15.438 | −26.854 |
| Combined cycle | 0.000 | −7.719 | −7.719 |

The two concentration contributions happen to cancel for these chosen ratios. The cycle carries one net positive charge inward, giving a second, independent way to check its energy. The nitrate movement remains uphill on its own. Coupling through the transport mechanism is what allows proton movement to support it.

### Stop the pump

The counterfactual holds nitrate reservoirs fixed and prescribes:

```text
f(t) = exp(−t/5 s)
voltage(t) = −80 f(t) mV
pHoutside(t) = 7 − f(t)
pHinside(t) = 7
```

Cycle energy starts at −7.719 kJ/mol, reaches zero at 2.582 model seconds, and is +8.826 kJ/mol at 10 seconds. With the proton gradient fully dissipated, the remaining uphill nitrate concentration term is +11.416 kJ/mol. An algebraic solution independently checks the numerical zero crossing.

This relaxation is imposed for teaching. The model does not solve membrane capacitance, ion balances, buffering, leaks or transport kinetics. A 5-second timescale is not a measured pump-inhibition response. Initial persistence of a driving gradient is the qualitative lesson; the precise time and subsequent reverse-favouring state depend on the stated assumptions. Actual reversal is not animated or claimed as a measured flux.

The [model data](concept_03_model.json) and [independent checks](concept_03_model_checks.json) retain the parameters and calculated values.

## Visual interpretation

The Blender file contains three native scenes: `01 | Wall and lipid membrane` (26 s), `02 | Passive carrier cycle` (18 s), and `03 | Pump and symporter` (12 s). The last plate is repeated twice in the completed film. The membrane is a cutaway strip; the proteins are original schematic geometry, not atomic structures. No external artwork or protein models were downloaded.

The carrier has two access gates. Their baked states never open both sides together, and transported markers pass each gate while it is open. The generic passive carrier is shown in inward cycles to explain net uptake, but a passive carrier can work outward when the gradient favours that direction. The 6-second cycle is chosen for visibility, not as a molecular turnover measurement. The pump marker and proton paths illustrate energy use and direction; they do not measure ATP demand for a seaweed.

Gold marks nitrate and coral marks protons. Purple marks a generic solute in the passive sequence and ATP near the pump, in separate labelled scenes. A neutral CO2 marker is pale. Marker sizes and paths must not be used to infer molecular dimensions, concentrations or fluxes. The wall is moved out of view for clarity, not biologically removed by an experiment.

All motion is baked as ordinary keyframes; opening the project does not require script auto-run. Teaching text, calculated graphs and questions are composed with Python and Pillow (`finish_concept_03.py`, private source) after rendering. The .blend file contains the editable three-dimensional scenes, while the MP4 contains the complete lesson.

## Narration notes

**0:00–0:26.** We have delivered nitrate to the thallus. It still has to cross the plasma membrane. The porous wall and the lipid membrane present different barriers. Small non-polar molecules can cross the lipid directly. Ions usually require a protein route.

**0:26–0:46.** Nitrate is more concentrated outside. Now change only the voltage. A negative interior increasingly opposes entry of this negative ion. Add the concentration and electrical contributions before deciding which direction is favourable. A favourable direction still needs a permeable route.

**0:46–1:04.** Watch the binding site. It opens outside, closes, then opens inside. The carrier changes shape but remains in the membrane. If the solute moves downhill without energy coupling, this is passive facilitated transport.

**1:04–1:28.** ATP hydrolysis drives the proton pump. Another protein allows protons to return with nitrate. Follow the energy from ATP to the ion gradient, then to the coupled transport cycle. Protons and nitrate enter together through the symporter. The two separate proteins are not an antiporter.

**1:28–1:44.** Nitrate entry costs free energy under these conditions. Proton entry releases more than enough to compensate. Add the movements belonging to one coupled cycle. These pH values and the coupling ratio are chosen to explain the mechanism, not to represent every seaweed.

**1:44–2:00.** Stop pumping. The existing gradient remains initially, then dissipates. The graph follows the energy of a possible inward cycle. It does not give the rate of uptake. What measurements would establish how a real cell responds?

**2:00–2:16.** Both carriers have finite capacity and can saturate. Does the curve prove active transport? No. We need evidence about the gradients and coupling. The next concept asks how capacity produces the concentration response.

These notes support live teaching; they are not an audio track or a guarantee of narration timing. The companion provides six prediction questions with suggested answers.

## Lecture integration

Lecture 8a's membrane sections now classify facilitated diffusion within passive transport, distinguish free nitrate from total tissue N, include membrane voltage, correct the definition of antiport, and describe alternating access without suggesting that the protein turns over in the bilayer. Repeated claims that ammonium must be passive and linear, or that a plateau proves active uptake, have been corrected. The adjoining growth discussion also stops assigning storage capacity or instantaneous growth from the membrane mechanism. Original slide images are retained; the revised prose and new visualisation supply the mechanism explanations.

This is the third stage of the suite. Continue to the completed [capacity and concentration film](CONCEPT_04.md) and [uptake measurement film](CONCEPT_05.md). Form, storage, physiology and enrichment are also complete; see the [nine-film index](README.md). These materials are available locally.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.

## Primary sources

- [Parker and Newstead (2014), Molecular basis of nitrate uptake by the plant nitrate transporter NRT1.1](https://pmc.ncbi.nlm.nih.gov/articles/PMC3982047/): proton coupling and alternating access in an Arabidopsis transporter. This supports the mechanism, not the schematic geometry or universal seaweed stoichiometry.
- [García-Sánchez et al. (2000), Sodium-dependent nitrate transport at the plasma membrane of leaf cells of the marine higher plant Zostera marina](https://pmc.ncbi.nlm.nih.gov/articles/PMC58924/): a seagrass example showing why the coupling ion should be established experimentally.
- [Loqué et al. (2009), Pore mutations in ammonium transporter AMT1 with increased electrogenic ammonium transport activity](https://pmc.ncbi.nlm.nih.gov/articles/PMC2757203/): experimental protein-mediated ammonium transport, including saturating kinetics; a counterexample to the blanket passive/linear classification.

The energy calculations, counterfactual and visual narrative are constructed for this lesson. They do not reproduce the measurements from these papers.
