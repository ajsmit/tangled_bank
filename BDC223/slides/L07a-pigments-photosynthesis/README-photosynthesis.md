# Lecture 7a: P–I curves and spectra

Six replacement slides in the Fira Sans, dark-header, teal-and-blue Beamer/TikZ
format used for the other recreated BDC223 lectures. All text, tables and curves
are editable in `L07a-photosynthesis-slides.tex`.

Build the six-page PDF and its six 1920-pixel PNGs with:

```sh
sh build-photosynthesis.sh
```

The lecture uses the PNGs in `../../images/L07a/` and links to the PDF. The
original JPEGs remain available.

## Content and sources

- The six supplied slides, formerly `BDC223_L06a_slide09.jpeg` through
  `BDC223_L06a_slide14.jpeg`, determine the topics and order. The replacements
  simplify the language and redraw the diagrams as vectors.
- Slide 1 uses the illustrative gross model
  `Pg = Pmax * tanh(alpha * I / Pmax)` and net model `Pn = Pg - R`.
  The drawing has `Pmax = 1`, `alpha = 0.005`, `R = 0.15` in relative rate units,
  so `Ik = 200` and `Ic = 200 * atanh(0.15)`. Its x-axis is labelled in multiples
  of `Ik`; no measured irradiances are implied. The orange tangent meets the
  **gross** asymptote at `Ik`, and `Ic` is the net curve's x-intercept. The vertical
  separation between curves is respiration. Photoinhibition is illustrated
  separately on slide 4, rather than being attributed to the tanh model.
- Slide 2 states the parameter conventions used throughout the lecture.
  `tanh(1) = 0.761594...`, hence gross photosynthesis at `Ik` is about 76% of
  `Pmax`. Source: [Jassby and Platt (1976)](https://doi.org/10.4319/lo.1976.21.4.0540).
- Slide 3 replaces the old absolute claims of temperature independence and a
  positive linear temperature response with qualified descriptions of metabolic
  limitation, temperature optima and light acclimation. A reduction in `Pmax`
  needs to be interpreted with the species and measurement conditions.
- Slide 4 retains the original course's indicative seaweed ranges and
  ice-diatom example, explicitly labelled as examples rather than universal
  thresholds. No new empirical data have been introduced. The high-light
  decline is a schematic drawing, not a fit or a species-specific threshold.
  The subsequent lecture section develops the photoinhibition model of Platt,
  Gallegos and Harrison (1980).
- Slide 5 distinguishes absorption from biological response, with an explicit
  requirement for equal incident photon flux at wavelengths compared in the
  action spectrum, at irradiances below saturation.
- Slide 6 draws original Gaussian-band schematics, based on the teaching
  comparison in the supplied slide. They are not traced or digitised data.
  Both spectra are normalised to their red maxima; the carotenoid contribution
  to the illustrative action curve is lower than its absorption contribution.
  The plotted shaded band is 450–510 nm. No measured transfer efficiencies or
  universal spectral mismatch are implied. Pigment- and organism-dependent
  differences are stated explicitly. Related primary sources include
  [Haxo and Blinks (1950)](https://pmc.ncbi.nlm.nih.gov/articles/PMC2147193/) and
  [Croce et al. (2003), carotenoid-to-chlorophyll transfer in CP29](https://pmc.ncbi.nlm.nih.gov/articles/PMC1302818/).

## Image mapping

| Former JPEG | Replacement PNG |
| --- | --- |
| BDC223_L06a_slide09.jpeg | L07a-photosynthesis-slide-1.png |
| BDC223_L06a_slide10.jpeg | L07a-photosynthesis-slide-2.png |
| BDC223_L06a_slide11.jpeg | L07a-photosynthesis-slide-3.png |
| BDC223_L06a_slide12.jpeg | L07a-photosynthesis-slide-4.png |
| BDC223_L06a_slide13.jpeg | L07a-photosynthesis-slide-5.png |
| BDC223_L06a_slide14.jpeg | L07a-photosynthesis-slide-6.png |
