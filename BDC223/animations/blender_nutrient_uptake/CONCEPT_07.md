# Uptake, storage and growth

Film 7 completes another part of the nine-film nutrient-uptake suite.

- [Film](concept_07_uptake_storage_growth.mp4): 3:00, 1920 × 1080, 30 fps, H.264; written explanations, no audio.
- [Native Blender project](concept_07_uptake_storage_growth.blend), [captions](concept_07.vtt), [poster](concept_07_poster.jpg).
- [Lecture explanation and eight practice questions with answers](../../L08c-nutrients_visualised.qmd#sec-storage-growth).
- [Model table](concept_07_model.json), [model checks](concept_07_model_checks.json), [native checks](concept_07_asset_checks.json), [production verification](concept_07_verification.json).

## Sequence and narration

| Film time | Teaching point | Narration / discussion |
|---|---|---|
| 0:00–0:20 | One nitrogen atom, several destinations | Uptake transfers nitrogen from water into tissue. Internal storage, assimilation and structural growth are different processes. The three containers are compartments in a budget, not three cells. |
| 0:20–0:44 | Begin with a finite pulse | The one litre vessel contains 20 micromoles of dissolved nitrogen. Initially the tissue contains one micromole of reserve nitrogen and ten of organic nitrogen. Light, carbon and other nutrients are sufficient. |
| 0:44–1:04 | Uptake can exceed immediate assimilation | The reserve increases when uptake exceeds assimilation. This illustrative uptake law responds both to the external concentration and to internal reserve status. Neither the curve nor reserve filling proves passive diffusion. |
| 1:04–1:24 | Replace the water at hour six | The experimenter replaces the water with nitrogen-free medium. The removed nitrogen is recorded as exported. External uptake becomes zero; reserve nitrogen is still available inside the tissue. |
| 1:24–1:44 | Growth continues using stored nitrogen | Assimilation moves nitrogen from the reserve into organic tissue. Structural biomass continues to increase after uptake has stopped. This model supplies the carbon, light and other requirements for growth. |
| 1:44–2:12 | Follow the rates and close the budget | Reserve nitrogen first rises, then falls after water replacement. Structural biomass increases by about 0.81 biomass units after the wash. The model keeps ten micromoles of structural organic nitrogen per biomass unit; reserve mass is separate. |
| 2:12–2:34 | Match concentration, vary nutritional history | Compare the same initial structural biomass and external nitrogen concentration, with one or eight micromoles of internal reserve. The model imposes reserve feedback on uptake. This is a conditional explanation, not a claim that more severe starvation always increases uptake. |
| 2:34–3:00 | Test storage rather than assume it | Measure external disappearance, internal nitrogen fractions and biomass separately. A labelled nitrogen pulse followed by a wash can trace earlier uptake into later organic tissue. Control light, carbon, phosphorus, water delivery and tissue condition. |

## A conservative N model

The main example starts with E = 20 µmol external N in 1 L, R = 1 µmol internal reserve N and G = 10 µmol structural organic N. Export X starts at zero. Structural biomass B = G/q, with q = 10 µmol organic N per **model biomass unit**. B is a reference quantity, not a measurement in grams. The model does not attempt a realistic species calibration or a fixed total tissue quota. Reserve N is additional to structural organic N.

```text
S = E / (1 L)
r = R / B
U = 4 B S/(2 + S) / [1 + (r/4)²]
A = 0.7 B r/(2 + r)
dE/dt = -U
dR/dt = U - A
dG/dt = A
dX/dt = 0 between interventions
dB/dt = A/q
```

Time is hours, U and A are µmol N/h, and r is µmol reserve N per biomass unit. Constants in denominators have the corresponding concentration or reserve-quota units; 4 and 0.7 multiplying B are capacities in µmol N per biomass unit per hour. The external Michaelis term is evaluated at the well-mixed concentration: there is no separate spatial transport field here. The reserve-feedback factor is imposed as an illustrative regulatory mechanism. It does not establish passive membrane transport or identify a molecular pathway.

At hour 6 the remaining water N is transferred to X and E becomes zero. The solver restarts with that state. Duplicate rows at hour 6 deliberately retain the before/after values. Uptake becomes zero immediately; positive reserves continue supporting assimilation. E + R + G + X remains 31 µmol. Carbon, light and other nutrients are sufficient; excretion, mortality and N losses through other pathways are omitted.

Just after washing: R = 8.10902, G = 13.33264, X = 9.55834 µmol. At hour 30: R = 0.01953, G = 21.42213 µmol. Structural biomass increases by 0.80895 units after washout. The reserve first increases, then supports later structural assimilation. A real reserve can contain several chemical forms in several locations; this model lumps them together and is not a vacuole reconstruction.

The second treatment begins with R = 8, the same external pulse and the same structural biomass. Its total initial N is 38 µmol, so total N is **not matched across the two treatments**. Initial uptake is 3.42246 versus 0.72727 µmol/h. The film compares starting rates under the imposed feedback law. Nutritional deprivation can also impair tissue; the comparison does not mean the most damaged or most severely starved organism must take up N fastest.

## Native geometry and time mapping

Scene 1 is 30 seconds (900 frames); native frame f corresponds to model time 30(f−1)/899 hours. The film holds the initial state for 20 seconds, follows hours 0–6 until film second 64, then follows hours 6–30 until second 104. This deliberate change in time compression is visible in the experimental clock. Frame selection is rounded to the nearest native frame. At the wash the representation changes over the nearest native samples; exact values and the discontinuity are in the budget and CSV.

External and reserve marker volume indicate their respective pool amounts, with different marker-to-amount factors. **Do not compare raw dot counts between the two containers.** The labelled bars share one quantitative scale. Structural plant scale cubed follows B/B₀. Morphology and location are schematic; the plant represents the organic N compartment, not an assertion that all growth happens in one cell. Scene 2 uses equal starting plant geometry with an eightfold reserve-marker difference and subtle motion. The native loop repeats during the comparison.

## Evidence and teaching limits

The model demonstrates a possible separation of uptake, internal storage and growth. Smit (2002) directly tested uptake and nutritional history, but explicitly did not relate N uptake and cellular N status to growth rate. Do not attribute these growth curves or parameters to that paper. The broader distinction between external nutrient availability and internal nutrient status has a long experimental history; see [Droop (1974)](https://doi.org/10.1017/S002531540005760X), which studied phosphorus and vitamin B12 in Monochrysis, not the N model shown here.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.
