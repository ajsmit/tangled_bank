# Follow the nitrogen

A 2 minute 41 second Manim animation for BDC223. It starts with a nitrate pulse into a seaweed flask, derives uptake rates from the depletion curve, and follows the same process along the Michaelis–Menten curve. The video is 1920 × 1080 at 30 frames per second, with on-screen explanations and no audio.

- [Play or download the video](nutrient_uptake.mp4)
- Manim source (`nutrient_uptake.py`, private source)
- [Lecture 8b and written walk-through](../../L08b-nutrients_michaelis_menten.qmd#sec-follow-nitrogen)
- [Illustrative five-minute measurements](illustrative_samples.csv)
- [Numerical checks](model_checks.json)

## Use it in class

| Start | What students see | Useful pause |
|:--|:--|:--|
| 0:00 | A nitrate pulse, seaweed uptake and a falling concentration curve. The no-seaweed control stays level. | Ask where the disappearing N has gone. |
| 0:35 | Early and late five-minute intervals become rates per gram per hour. | Let students identify why volume, biomass and time appear in the calculation. |
| 0:54 | Each interval becomes a point on the V–S graph. Early points are high and right. | Ask why the points appear from right to left. |
| 1:11 | The flask, S(t), accumulated U(t), and V(S) move together. Tangents change with the rate. | At about 1:29, S = Ks and V = Vmax/2. Distinguish half the rate from half the amount. |
| 1:46 | A schematic uptake system compares scarce and abundant nitrate. | Ask whether doubling nitrate should double uptake at high S. |
| 2:21 | A second pulse restores the external concentration. The existing seaweed N is retained. | Predict the direction of movement on V–S and the next depletion slope before playing the answer. |

The chapter markers are embedded in the MP4. Players that support chapters can jump between sections. The lecture contains a written explanation and four questions with expandable answers. Because the film carries the explanation on screen, it also works with the sound off in a lecture theatre. Pause for calculations and discussion.

## The experiment represented

The flask contains 0.50 L of seawater and 4.5 g of seaweed fresh mass. A small-volume nitrate addition brings dissolved nitrate-N to S₀ = 25 µmol N L⁻¹. The illustrative uptake parameters are Vmax = 6 µmol N g fresh mass⁻¹ h⁻¹ and Ks = 5 µmol N L⁻¹. The green seaweed is a generic drawing. These values are not presented as estimates for a particular species.

At each instant,

```text
V(S) = Vmax S / (Ks + S)
dS/dt = -(M / Ω) V(S) / 60          [t in minutes]
U(t) = Ω [S₀ - S(t)]                [µmol N taken up by the whole piece]
Ω S(t) + U(t) = Ω S₀ = 12.5 µmol N
```

The analytic solution uses the principal branch of the Lambert W function. An independent numerical integration checks it to within 10⁻⁷ µmol L⁻¹. The checks also verify positivity, monotonic depletion, increasing cumulative uptake, the nitrogen balance, the integrated uptake flux, and V(Ks) = Vmax/2. The concentration remains positive at finite times rather than being clipped to zero.

At five minutes, S = 21.30068 µmol L⁻¹, closely matching the lecture's 21.3 example. The first interval gives an average V of 4.93242 µmol N g⁻¹ h⁻¹, approximately 69.1 µg N g⁻¹ h⁻¹. The 30–35 minute interval gives 2.80856 µmol N g⁻¹ h⁻¹. S reaches Ks after 31.16354 minutes. At 60 minutes, S ≈ 0.07457 and V ≈ 0.08817 in their respective units.

In the rate-construction scene, each point uses the mean of its two endpoint concentrations and the mean uptake rate over five minutes. These points need not lie exactly on the instantaneous model curve. In the replay, the moving point and tangents use instantaneous values. The supplied CSV includes both.

The model assumes constant water volume, biomass and kinetic parameters, stable light, temperature and mixing, negligible sampling withdrawal, and no other sources or sinks of N between pulses. It illustrates uptake without simulating growth, changing internal quotas, acclimation or transporter regulation. The coloured parcels in the flask follow the modelled allocation between water and seaweed. They are not a molecular simulation, and the displayed numbers, rather than a literal count of dots, give the quantitative balance.

The membrane scene is a separate comparison of concentrations. A finite handling time and shorter waits at higher S illustrate saturation. Its four sites are conceptual, not a claim that a cell has four transporters or that whole-alga kinetics identify a particular molecular mechanism. The second pulse restores S to 25 while leaving Vmax and Ks unchanged. It adds new nitrogen without removing what the seaweed took up previously.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/nutrient_uptake/`. They are excluded from Git and the website.

## Existing demonstrations and the design choices

I searched for enzyme-kinetics animations, substrate-depletion simulations and algal perturbation demonstrations on 12 September 2026. The following are useful precedents. This is a design review, not a measured comparison of learning outcomes.

| Demonstration | What I could verify | What this BDC223 treatment develops |
|:--|:--|:--|
| [SIMENKIN, Universidad de Alcalá, module 2](https://biomodel.uah.es/metab/enzimas/simenzkin/modulo-2_xls.htm) | Its documented panels connect substrate loss, product formation and rate plots, with quantitative problems. The page credits Torres and Santos' teaching work. | An explicit physical nitrogen balance, the actual seaweed-flask calculation, and a staged conversion of each measured interval into a V–S point. |
| [MySimulator: enzyme kinetics](https://www.mysimulator.uk/molecular-biology/enzyme-kinetics/) | I ran and paused the public standalone simulation and inspected its linked substrate/product time graph and moving V–S point. | A guided sequence with fewer competing plots, fixed units, a visible organism, and shared time across the flask and three graph views. |
| [Labster: enzyme kinetics](https://www.labster.com/simulations/enzyme-kinetics) | The public description connects laboratory measurements, molecular animations, progress curves and student construction of Michaelis–Menten graphs. I did not access the full paid lab. | An openly editable, locally playable film focused on the BDC223 perturbation experiment, with pauses and a second-pulse prediction. |
| [PhysiologyWeb interactive graph](https://www.physiologyweb.com/calculators/michaelis_menten_equation_interactive_graph.html) | The page describes changing Vmax and Km to explore a saturation graph. | A temporal explanation of how the data arise, plus a distinction between amount, concentration and rate. |

The aim is to make the causal links visible. The additions are the transfer of N into tissue, the no-seaweed control, the unit conversion, points appearing in experimental order, accumulated uptake beside depletion, instantaneous tangents, an exact half-saturation pause, a capacity sketch, and an intervention that tests the explanation. No third-party graphics or animation code were copied. A claim of a numerical improvement in teaching effectiveness would require testing with students.

The course sources are Lecture 8a, Lecture 8b and Lab 4. The local ecological context is supported by [Smit (2002), Nitrogen uptake by Gracilaria gracilis](../../../docs/Smit_2002.pdf): nitrate uptake saturated, and water motion and nutritional history affected uptake behaviour. The distinction between low-concentration affinity and half-saturation is also consistent with [Lindemann et al. (2016), Scaling laws in phytoplankton nutrient uptake affinity](https://doi.org/10.3389/fmars.2016.00026).

## Changes to the lectures

Lecture 8b embeds the film and adds a written walk-through, the numerical correspondence with the existing example, the nitrogen balance, assumptions, and prediction questions. It also distinguishes the amount taken up per gram from a rate, includes water volume in the condensed mass-conversion equations, corrects the first five-minute interval's duration, and explains the Ks/Km notation.

Lecture 8a now links to that walk-through. The directly related passages use rectangular hyperbola, include the sign and volume/mass normalisation when deriving uptake from depletion, describe Vmax as an asymptotic limit, and interpret low-S uptake using Vmax/Ks. The existing lecture structure and unrelated topics are retained.
