# Nutrients: from seawater to growth

The Blender companion to BDC223 Lectures 8a and 8b contains all nine films, with editable native projects, captions and teaching notes. Production Python is kept in `_private`. [Open the lecture companion and practice](../../L08c-nutrients_visualised.qmd). All films have written explanations and no audio.

| Film | Watch | Duration | Edit | Teach |
|---|---|---|---|---|
| 1 | [From the flask to the surface](concept_01_supply_to_surface.mp4) | 1:08 | [Blender](concept_01_supply_to_surface.blend) | [Guide](README.md#watch-and-teach) |
| 2 | [Moving water and diffusion limitation](concept_02_moving_water.mp4) | 1:36 | [Blender](concept_02_moving_water.blend) | [Guide](CONCEPT_02.md) |
| 3 | [Crossing the membrane](concept_03_crossing_membrane.mp4) | 2:16 | [Blender](concept_03_crossing_membrane.blend) | [Guide](CONCEPT_03.md) |
| 4 | [Capacity and concentration](concept_04_capacity_and_concentration.mp4) | 2:28 | [Blender](concept_04_capacity_and_concentration.blend) | [Guide](CONCEPT_04.md) |
| 5 | [Measuring a changing uptake rate](concept_05_measuring_uptake.mp4) | 3:24 | [Blender](concept_05_measuring_uptake.blend) | [Guide](CONCEPT_05.md) |
| 6 | [Form and nutrient supply](concept_06_form_and_supply.mp4) | 3:10 | [Blender](concept_06_form_and_supply.blend) | [Guide](CONCEPT_06.md) |
| 7 | [Uptake, storage and growth](concept_07_uptake_storage_growth.mp4) | 3:00 | [Blender](concept_07_uptake_storage_growth.blend) | [Guide](CONCEPT_07.md) |
| 8 | [A changing physiological environment](concept_08_physiological_environment.mp4) | 3:20 | [Blender](concept_08_physiological_environment.blend) | [Guide](CONCEPT_08.md) |
| 9 | [From enrichment to oxygen loss](concept_09_enrichment_and_oxygen.mp4) | 3:20 | [Blender](concept_09_enrichment_and_oxygen.blend) | [Guide](CONCEPT_09.md) |

Total viewing time: **23:42**. The [suite plan](SUITE_PLAN.md) maps the films to the lecture, and the [completion report](SUITE_REVIEW.md) records the teaching changes and verification. The remainder of this page documents concept 1.

## Watch and teach

- [Film: From the flask to the surface](concept_01_supply_to_surface.mp4), 1:08, 1920 × 1080, 30 fps, H.264. Written explanations; no audio.
- [Editable Blender project](concept_01_supply_to_surface.blend), containing three named scenes. The three-dimensional objects, materials, cameras and particle paths are editable. Ordinary keyframes are baked into the project, so playback does not require enabling script execution.
- [Lecture companion and practice](../../L08c-nutrients_visualised.qmd).
- [Captions](concept_01.vtt) and [narration notes](#narration-notes).
- Scientific model (`gradient_model.py`, private source), [generated profiles](concept_01_model.json), [independent checks](model_checks.json), and [production verification](verification.json).

The laboratory scene uses a stylised glass flask and a generic green thallus. The view moves towards the seaweed, then cuts to an enlarged surface patch. The final section combines the Blender scene with a calculated concentration profile. The teaching labels, graph and final question are composed from Python with Pillow; they are not baked text objects in the Blender file. The Blender project is included here; production Python is kept in `_private`.

| Time | Teaching point | Suggested pause |
|---|---|---|
| 0:00–0:16 | Add nitrate, then move from the flask towards the thallus. | What must happen before a nutrient can cross the membrane? |
| 0:16–0:32 | Random motion continues in all directions. Uptake removes nitrate at the surface. | Does an ion need to detect the seaweed? |
| 0:32–0:44 | A concentration gradient develops in a local model with constant bulk concentration. | Identify the axes: concentration against distance. |
| 0:44–0:50 | Near-steady delivery balances removal, but surface concentration remains below bulk concentration. | Explain why abundant bulk nitrate does not guarantee rapid delivery. |
| 0:50–1:00 | Surface removal is switched off. Diffusion refills the depleted region. | Predict the next profile before revealing it. |
| 1:00–1:08 | Predict what stronger water movement will do at matched bulk concentration and unchanged uptake machinery. | Ask students to state a mechanism and the condition under which it should matter. |

## Model and limits

The quantitative close-up is a one-dimensional planar diffusion model, not a continuation of the whole-flask nitrogen budget. It treats the water at x = 300 µm as a maintained reservoir. In a closed perturbation experiment the bulk concentration also changes; the existing Manim film treats that case.

```text
∂C/∂t = D ∂²C/∂x²
C(L,t) = Cb
D ∂C/∂x at x=0 = k Cs       [inward surface uptake is positive]
```

The illustrative parameters are D = 1500 µm²/s, L = 300 µm, Cb = 10 µmol N/L and k = 20 µm/s. No species-specific parameter fit is claimed. This simple surface sink isolates external delivery; it does not model Michaelis–Menten saturation, cell-wall structure, membrane energetics, assimilation, storage or growth.

The finite-volume discretisation has 100 cells. It includes the resistance between the surface and the centre of the first cell, so the boundary flux obeys the stated surface condition. A symmetric matrix exponential advances the concentration field. An independent adaptive BDF integration verifies the solution, and the numerical steady state is compared with the analytical result:

```text
Cs = Cb / (1 + k L / D) = 2 µmol N/L
Jin = D (Cb − Cs) / L = k Cs = 0.040 µmol N m⁻² s⁻¹
```

Uptake is turned off after the model has run for 120 s and nearly reached steady state. With k = 0, the surface becomes a no-flux boundary, and the maintained reservoir refills the water. After 60 further model seconds, Cs is about 9.45 µmol N/L. The bulk concentration remains 10 throughout. The film accelerates the concentration calculation and labels model time separately.

The gold particles are **illustrative paths**, not the numerical discretisation and not a literal molecular count. They move with random, unbiased steps, reflect at the surface when not removed, and can transfer into the tissue while uptake is active. New representative parcels enter from the distant reservoir. The thin line records one selected random path. Surface removal stops in the final experiment. Particle size, density and speed are chosen for visibility and must not be used to estimate D, flux or molecular dimensions. The continuum profile, rather than a dot count, supplies the quantitative result.

There is no computational fluid dynamics in this first concept. Flow, shear and the distinction between concentration and velocity boundary layers belong in concept 2, where they can be introduced through a controlled comparison. The surface patch is not a detailed membrane reconstruction; membrane transport is concept 3.

## Narration notes

**0:00–0:16.** We add nitrate to seawater containing a seaweed. Before nitrate can cross a membrane, it must reach the thallus surface. Let us move closer and examine this delivery step.

**0:16–0:32.** Individual ions move randomly, both towards and away from the surface. Uptake removes nitrate at the surface. When replacement is slow, the water immediately beside the seaweed becomes depleted. Random movement then gives a net supply towards that region.

**0:32–0:44.** Here we hold the bulk concentration constant and let uptake operate. Read the horizontal axis carefully: it is distance from the surface. The concentration at the surface falls even though the concentration farther away remains high.

**0:44–0:50.** Delivery now balances removal, but the surface is still depleted. For this steady profile, flux depends on the concentration difference divided by the diffusion distance.

**0:50–1:00.** Switch uptake off. Diffusion continues, refilling the depleted water. The profile becomes flatter. Uptake was maintaining the sink; diffusion redistributes the nitrate.

**1:00–1:08.** Restore uptake, then predict the effect of stronger water movement. Keep bulk nitrate and uptake capacity unchanged. What should happen to delivery, and why?

These notes are intended for live explanation or a later recorded narration. They are not an audio track, and they have not been timed to a particular speaker.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.

## Lecture changes

The new companion page contains the film, a written explanation, model assumptions, and four prediction questions with answers. Lecture 8a links to it. Its directly related passages now distinguish surface from bulk and tissue concentrations, diffusion from osmosis and membrane transport, a concentration boundary layer from a solid skin, and molecular diffusion from imposed water movement. The water-movement discussion now separates intrinsic capacity from fitted parameters and avoids treating a shaker setting as a direct water-velocity measurement.

The existing Manim film is retained; its production source is kept in `_private`.

## Sources

The visual narrative follows [Lecture 8a](../../L08a-nutrient_uptake.qmd) and [Lecture 8b](../../L08b-nutrients_michaelis_menten.qmd). The experimental context is [Smit (2002)](../../../docs/Smit_2002.pdf); the distinction between diffusive supply and uptake capacity is also developed by [Lindemann et al. (2016)](https://www.frontiersin.org/journals/marine-science/articles/10.3389/fmars.2016.00026/full). All rendered artwork and code in this concept were created for this lesson.
