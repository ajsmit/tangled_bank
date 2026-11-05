# Nine-film suite: completion and teaching review

## Revision of 5 November 2026

The nine films and the Lecture 9c companion have been revised to use connected sentences in their explanations. The older Lecture 9b animation received the same prose review. Short axis labels, symbols and section names remain concise.

The surface comparison now includes a 1 cm² reference square, complete cylinder dimensions and SA:V units of cm⁻¹. The production code calculates areas and internal distances from the specified dimensions, fits long text within the frame and uses a font that supports scientific superscripts. In the storage film, the water-replacement transition now preserves the external nitrogen inventory until the intervention; interpolation previously began removing it early.

The companion distinguishes an unfavourable net passive flux from the possibility of individual ions crossing a membrane. It also corrects the reserve-feedback question: matching bulk concentration and water movement does not establish identical local delivery or rule out other physiological effects of pre-treatment. The oxygen model's assumed C:N ratio and oxygen demand are now explicit in the explanation and code.

Current revision checks are recorded in [REVISION_2026-11-05.json](REVISION_2026-11-05.json). The production account below and the older individual verification files describe the earlier build. This revision remains local. Safari checks verified backward seeking, direct slider seeking, restart, captions and full-screen controls, including the complete-file fallback on a preview server without byte-range support. All 13 BDC223 animations now have a text-free control band and one-second keyframes. The lesson player places its controls below the video. A margin note beside the first animation distinguishes random particle motion, net diffusive flux and directional drift.


All nine movies are available locally, totalling **23 minutes 42 seconds**. The [index](README.md) links each MP4, editable Blender project and teaching guide. The [lecture companion](../../L09c-nutrients_visualised.qmd) brings them together with explanations, calculations, prediction pauses and practice answers. The original Manim material in Lecture 9b remains available.

The final three films add:

- **Uptake, storage and growth (3:00).** Account for water N, internal reserves, structural organic N and N removed during water replacement. Growth continues from stored N after external uptake stops. Compare different starting reserves at matched external concentration and structural biomass.
- **A changing physiological environment (3:20).** Separate nitrate reduction and assimilation, follow day/night uptake and reserve dynamics, examine bounded light and temperature responses, and distinguish the measured ammonium interaction from illustrative kinetic components.
- **From enrichment to oxygen loss (3:20).** Connect a conceptual coast to a factorial N/P experiment and a separate dark bottom-water oxygen budget. Compare oxygen renewal at the same initial organic loading, then distinguish recycling from removal.

Each has an MP4, native `.blend`, poster, WebVTT captions, narration notes, model tables, CSV data and reproducible Python. Production scripts and reproduction instructions are kept locally in `_private/animations/BDC223/blender_nutrient_uptake/`, excluded from Git and the website. This version has written explanations and **no audio**. The instructor can narrate or pause for student predictions. The original teaching canvases use 1920 × 1080; the current exports add a 200-pixel control band and use 1920 × 1280, 30 fps, H.264 with progressive-download metadata.

## Changes to the lecture

Lecture 9a now links the complete sequence and qualifies shorthand in its original figures. Its **94 original slide-image references are retained in the same order**. The images themselves were not redrawn; students are directed to read them alongside the qualified explanations. Prose changes address:

- Uptake, storage, assimilation and structural growth as distinct quantities and processes.
- Surge and internal feedback as hypotheses requiring measurements, with external delivery and membrane gradients kept separate.
- Nutritional history as a conditional influence; severe deprivation can also impair tissue.
- Nitrate reductase producing nitrite, followed by nitrite reductase producing ammonium.
- Photosynthetic support for assimilation without claiming that every organic molecule contains N and P.
- Continued uptake in darkness and light/temperature responses that can saturate or decline.
- Ammonium-related nitrate suppression in Smit (2002) as a partial, condition-specific effect, rather than universal complete inhibition.
- Biphasic kinetics as a description that does not identify transport proteins, energy costs or passive transport by itself.
- Redfield and benthic-plant composition ratios as empirical summaries, and factorial additions as a test of nutrient restriction.
- The distinction between N₂ dissolution and fixation, remineralisation and nitrification, recycling and export.
- Biomass accumulation as production minus losses, and oxygen decline as consumption exceeding supply.

The companion adds **24 practice questions and 24 worked answers**, eight per new concept. Questions ask students to calculate budgets, make contrasting predictions and design measurements that can reject an explanation. They are not assessment marks or material about individual students.

## Scientific scope

All numerical examples in films 7–9 are labelled illustrative. They are not species calibrations or forecasts for a South African bay. Film 7 uses reference structural biomass units and an imposed reserve-feedback law. Film 8 prescribes light/dark capacities and response curves; its displayed ammonium-inhibition percentage is separately attributed to the actual Gracilaria study. Film 9 uses a chosen fixed N:P requirement, and its oxygen model isolates oxygen exchange rather than complete flushing of every nutrient and organic pool.

The coastal animation, factorial experiment and oxygen experiment are distinct. The oxygen loading is not derived from the arbitrary factorial biomass units. A growth response does not prove a bloom; a bloom does not guarantee oxygen depletion; and uptake does not prove permanent ecosystem N removal.

[Smit (2002)](../../../docs/Smit_2002.pdf) was checked in the original PDF, including the plotted µM units and Figure 7 on printed page 203. It anchors the course's uptake evidence but does not supply the new storage-to-growth model. Supporting primary literature and the exact assumptions are linked in the concept guides and lecture.

## Verification

The three new models were checked against separate Radau integrations of the same stated equations, conservation identities and, for the N/P experiment, analytical yield ceilings. Maximum independent numerical differences were below 3 × 10⁻¹⁰ in their corresponding model units. Pool and oxygen-budget residuals were below 6 × 10⁻¹⁴.

Saved native projects were opened independently of the builders. Checks reconstructed amounts from marker volumes and biomass scales, verified photoperiod lamp states and counted atoms in the molecular icons. Maximum checked quantity differences were below 10⁻⁶. These checks establish implementation consistency, not the biological validity of illustrative parameters.

All 17,400 new movie frames were decoded without errors. Codec, dimensions, frame counts, duration, chapters, caption timing and fast-start atom order were checked. Representative frames from every chapter of each encoded film were inspected, including washout, light/dark states, chemical glyphs, factorial yields and low-oxygen comparisons. A separate text-bounds check sampled every second of films 7–9 and found no clipped text or overlapping text boxes.

Both Quarto lecture pages rendered locally. The companion contains nine properly closed video elements and three new practice sections with eight questions and eight answers apiece. Every linked film, poster and caption was served successfully from the local preview; HTTP byte-range requests returned 206. Source media and local rendered copies matched byte-for-byte. All nine films played and sought successfully through the browser controls. The three new answer panels opened correctly, and film 8's English captions were checked on screen. Browser observations and final hashes are in [SUITE_VERIFICATION.json](SUITE_VERIFICATION.json) and the individual verification records.

## Deployment

At the end of production verification, the complete suite had been checked locally. This report records those production checks; publication is verified separately. Private PNG caches, draft stills and QA logs remain under `_private/animations/BDC223_blender/concept_07` through `concept_09`.


### Axis placement revision, 5 November 2026

All y-axis titles in the nine nutrient animations now run vertically beside their axes, outside the tick labels. Titles and units wrap where needed to retain readable type. The earlier Manim nutrient animation follows the same convention; neighbouring arrows and small panels have been moved to leave room. The photosynthesis animations were inspected and already use vertical y-axis titles.

The paired velocity and concentration profiles in concept 2 now both use distance from the surface on x. Velocity and nitrate-N concentration are on y. The surface markers are at the left edge, and the moving scan lines are vertical. Lecture 9c explains the revised orientation.

### Clarity revision for films 7 and 8, 5 November 2026

The current films use quantitative diagrams instead of the earlier 3D vessel and molecular scenes. Film 7 explicitly encloses both internal pools within one tissue boundary, separates N amounts from transfer rates, pauses for eight seconds at water replacement, and accounts for exported N in a conserved 31 µmol bar. Matched comparisons use common bar scales for reserves and uptake.

Film 8 places membrane transport before the labelled chemical conversions. Aligned day/night graphs connect uptake and assimilation rates to reserve N, with matching time cursors and shaded dark intervals. The response curves identify their chosen peaks and explain the direction of change. The ammonium comparison outlines the reference extent behind the reduced bar, and the two-component example reports the numerical sum at the current concentration. Model parameters, underlying data, durations and chapter boundaries are unchanged; captions and Lecture 9c describe the revised visuals.

The native Blender verification above applies to the earlier scenes. The current diagrams are generated by `finish_final_concepts.py`. Revision evidence, backups and encoded-frame checks are retained locally in `_private/lecture-development/BDC223/2026-11-05-concepts-7-8/`.
