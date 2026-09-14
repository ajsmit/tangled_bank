# Concept 2: Moving water and diffusion limitation

[Watch the film](concept_02_moving_water.mp4) · [Editable Blender project](concept_02_moving_water.blend) · [Lecture and practice](../../L08c-nutrients_visualised.qmd#sec-moving-water)

The film is 1:36, 1920 × 1080, 30 fps, H.264, with written explanations, a [caption track](concept_02.vtt) and no audio. It continues from concept 1's prediction: what happens when we move the water, keeping the biology the same? The [production verification](concept_02_verification.json) records the mathematical, native-project, video and lecture checks.

## What the film explains

| Time | What students see | Explanation or prediction |
|---|---|---|
| 0:00–0:12 | Two Blender views of the same thallus patch, with weak and stronger water movement. | Hold bulk nitrate, temperature, area and uptake capacity constant. Blue marks water movement; gold parcels add illustrative random displacements. |
| 0:12–0:26 | Separate velocity and concentration profiles, with markers scanning the same distance from the surface. | Water velocity and nitrate concentration are different quantities. Zero fluid velocity at the stationary surface does not stop molecular motion. |
| 0:26–0:46 | Calculated concentration fields along two patches; a line selects the same downstream position. | Uptake depletes water downstream. Stronger flow keeps the depleted region closer to the surface. The line selects position, not elapsed time. |
| 0:46–1:04 | Concentration profiles, surface tangents, surface concentrations and fluxes. | A smaller bulk-to-surface difference can support greater flux when the surface gradient is steeper. In this example uptake increases by 94.2%. |
| 1:04–1:20 | Uptake capacity is reduced equally in both treatments through 181 steady comparisons. | With low capacity, improved delivery yields only a 2.1% uptake increase. This is a parameter comparison, not a simulated physiological time course. |
| 1:20–1:36 | A prediction and its qualified explanation. | Almost unchanged uptake under stronger flow is consistent with a capacity restriction, but does not identify the membrane mechanism. |

The lesson is the conditional explanation. Neither “stirring always greatly increases uptake” nor “a small response proves nutrient sufficiency” follows from these comparisons.

## Numerical model

The quantitative part solves a steady advection–diffusion approximation over a flat surface:

```text
u(y) ∂C/∂x = D ∂²C/∂y²
u(y) = γ y
C(0,y) = Cb                    [inlet, away from the surface corner]
C(x,H) = Cb                    [maintained outer water]
D ∂C/∂y at y=0 = Jmax Cs/(K+Cs) [inward uptake flux]
```

The x coordinate is distance downstream; y is distance above the thallus. The no-slip surface has u(0) = 0. Molecular diffusion supplies the surface. Axial diffusion is neglected, as in a local boundary-layer approximation. The flow is prescribed linear shear, not a full Navier–Stokes or turbulence calculation. The inlet corner is not intended as a detailed physical reconstruction.

Parameters: D = 1500 µm²/s, Cb = 10 µmol N/L, K = 2 µmol N/L; shear rates 0.25 and 2.0 s⁻¹; outer boundary H = 1500 µm; comparison station x = 2000 µm. These illustrative values are not species fits or calibrations to shaker settings.

| Surface capacity Jmax (µmol N m⁻² s⁻¹) | Flow | Cs (µmol N/L) | Local J (µmol N m⁻² s⁻¹) | Effective diffusion distance (µm) |
|---:|---|---:|---:|---:|
| 0.20 | weak | 0.4160 | 0.034435 | 417.5 |
| 0.20 | stronger | 1.0047 | 0.066875 | 201.8 |
| 0.01 | weak | 8.1100 | 0.008022 | 353.4 |
| 0.01 | stronger | 9.0382 | 0.008188 | 176.2 |

The effective distance is D(Cb−Cs)/J, with consistent units. It is found by extending the surface tangent to Cb. It is not a sharp physical edge: the actual calculated concentration profiles are curved. The model obtains it from the coupled solution; it is not prescribed as an arbitrary layer thickness.

Jmax and K describe a saturating sink at the **surface concentration**. They should not be equated to parameters fitted to biomass-normalised whole-organism uptake against **bulk concentration**. A saturating response does not distinguish active from facilitated transport. Assimilation, storage and growth are not represented.

The model uses 160 finite-volume cells across y and an adaptive BDF integration downstream. The nonlinear surface condition includes the first half-cell diffusion resistance. Concentrations in µmol/L and D in µm²/s require the factor 0.001 to obtain J in µmol m⁻² s⁻¹. The model preserves this conversion explicitly.

[Independent checks](concept_02_model_checks.json) compare against Radau integration, 320 cells, a farther outer boundary, a bracketed surface-root calculation, local flux conservation and an integrated downstream nitrogen budget. A zero-uptake comparison remains uniform. The 181 capacity comparisons keep the two treatments matched, respect physical bounds and reproduce both endpoint cases.

## Visual interpretation and limits

The Blender project contains two editable motion plates, each with a 16-second cycle. The film uses the first 12 seconds side by side. Geometry, materials, lights, camera and motion are editable, with ordinary baked keyframes and no script auto-run requirement. The green object is a flat, stylised thallus patch, not a reconstruction of a species or membrane.

Blue tracers illustrate flow along the surface, increasing with height. The stronger-flow plate uses eight times the imposed motion rate. Gold parcels add equal-scale random displacements in both views. The displayed dimensions, speeds, particle counts and trajectories do not provide quantitative uptake data. Surface absorption is represented by the **subsequent calculated fields and fluxes**, rather than by counting particle disappearances in these opening views.

The numerical graphs, colour maps, captions and questions are composed in Python after Blender rendering. They are not text or graph objects in the .blend file. The complete production sources are supplied. The moving profile markers select distance; the moving map line selects downstream position; the changing capacity selects different steady solutions. None of these is an elapsed-experiment clock.

The model omits waves, turbulent fluctuations, moving thalli, complex geometry, physiological acclimation and axial diffusion. The outer concentration and upstream supply are maintained. It is not a closed-flask depletion calculation and does not replace the existing Manim experiment.

## Narration notes

**0:00–0:12.** Let us change only the water movement. The thallus area, bulk nitrate, temperature and uptake machinery stay the same. Water carries nitrate along the surface, while molecules also move relative to that water.

**0:12–0:26.** Read these profiles separately. The first shows water velocity; the second shows nitrate concentration. At the stationary surface, water velocity is zero. That does not mean that the molecules have stopped moving. Diffusion still supplies the surface.

**0:26–0:46.** Follow the water downstream. Uptake removes nitrate as the water passes the thallus. The depleted region extends farther from the surface in weak flow. Now compare the same position under stronger flow: the region is narrower. The arrows distinguish delivery along the thallus from diffusion towards it.

**0:46–1:04.** The surface concentration rises under stronger flow. This makes the bulk-to-surface difference smaller. How can uptake increase? Look at the surface tangent. The difference acts over a shorter effective distance, so the gradient at the surface is steeper. The diffusion coefficient has not changed.

**1:04–1:20.** Now compare a different uptake capacity. Reduce it equally in both treatments. As removal becomes slower, the surface is less depleted. With low capacity, stronger flow still improves delivery, but uptake changes very little. The uptake system has little capacity to use the extra supply.

**1:20–1:36.** If stronger flow produces almost unchanged uptake, what might restrict the rate? Membrane transport or internal processing is one possibility. It is a testable explanation, not proof of a particular mechanism or of nutrient-sufficient growth.

These are live-teaching notes, not an audio track or a speech-duration guarantee. The companion lecture contains five prediction questions with suggested answers, including an alternative explanation involving whether shaker treatments actually changed flow at the thallus.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.

## Sources

The narrative follows [Lecture 8a](../../L08a-nutrient_uptake.qmd), particularly its boundary-layer and water-movement discussion. [Smit (2002)](../../../docs/Smit_2002.pdf) supplies the course's experimental context for water movement and nutrient uptake. [Lindemann et al. (2016)](https://www.frontiersin.org/journals/marine-science/articles/10.3389/fmars.2016.00026/full) supports distinguishing diffusive supply from biological constraints. The planar model and illustrative parameter comparisons here are constructed for teaching; they are not a reproduction of either paper's measurements or geometry.

Concepts 1–9 are available locally; continue to [Crossing the membrane](CONCEPT_03.md), [Capacity and concentration](CONCEPT_04.md) and [Measuring a changing uptake rate](CONCEPT_05.md). All nine films are linked in the [suite index](README.md).
