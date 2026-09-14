# A changing physiological environment

Film 8 completes another part of the nine-film nutrient-uptake suite.

- [Film](concept_08_physiological_environment.mp4): 3:20, 1920 × 1080, 30 fps, H.264; written explanations, no audio.
- [Native Blender project](concept_08_physiological_environment.blend), [captions](concept_08.vtt), [poster](concept_08_poster.jpg).
- [Lecture explanation and eight practice questions with answers](../../L08c-nutrients_visualised.qmd#sec-physiological-environment).
- [Model table](concept_08_model.json), [model checks](concept_08_model_checks.json), [native checks](concept_08_asset_checks.json), [production verification](concept_08_verification.json).

## Sequence and narration

| Film time | Teaching point | Narration / discussion |
|---|---|---|
| 0:00–0:22 | Uptake is not the whole nitrogen pathway | Nitrate is reduced to nitrite by nitrate reductase, then to ammonium by nitrite reductase. Ammonium nitrogen can enter amino acids. This is a pathway outline; it omits reductant, protons, water and full reaction stoichiometry. |
| 0:22–0:42 | A day-night experiment | External nutrient supply is maintained. The reference tissue experiences twelve hours of light, then twelve of darkness, repeated twice. The light regime changes the prescribed uptake and assimilation capacities. |
| 0:42–1:12 | Darkness does not mean zero uptake | Uptake and assimilation continue at lower rates in the dark in this example. Internal nitrogen reserves change because the two rates differ. Stored carbon and energy are assumed sufficient; neither growth nor carbon exhaustion is modelled. |
| 1:12–1:48 | Light responses have a useful range | An illustrative processing response rises to a maximum and declines at high irradiance. The peak is chosen for teaching, not measured for a species. Increasing light can help in one range and harm in another. |
| 1:48–2:16 | Temperature is a response curve | The example peaks at twenty degrees Celsius. A Q10 value describes a rate ratio over a stated temperature interval. It does not justify indefinite doubling or identify an optimum from two temperatures. |
| 2:16–2:42 | Ammonium can suppress nitrate uptake | In Smit 2002, Gracilaria gracilis showed approximately thirty-eight percent suppression of nitrate uptake with ammonium above five micromoles per litre. The bars are a normalised summary, not raw paired measurements or a fitted inhibition function. |
| 2:42–3:04 | A second kinetic component | The total curve here is the sum of two saturating components with different concentration scales. Its additional rise is an illustrative biphasic response. Curve shape alone does not establish transporter identity, a switch to passive transport or energetic cost. |
| 3:04–3:20 | Separate delivery and physiology | Measure local nutrient concentration and uptake per unit tissue or active surface under matched conditions. Cross a light or temperature treatment with water movement, include replicates and state what outcomes distinguish the proposed mechanisms. |

## Three different kinds of evidence

The opening molecular icons show nitrate (one N, three O), nitrite (one N, two O) and ammonium (one N, four H). Nitrogen is blue, oxygen red and hydrogen pale. Ion charges appear in labels. Bond order, resonance, precise bond angles, enzyme structure and balanced redox chemistry are omitted. Nitrate reductase produces nitrite; nitrite reductase produces ammonium; amino-acid synthesis is a subsequent step. The seaweed icon represents organic N, not a specific amino-acid molecule.

The day/night experiment, response curves and summed kinetic components are **illustrative models**. The ammonium-inhibition panel instead reports a **bounded experimental result**. Keep these categories distinct when teaching.

## Photoperiod model

A fixed 1 g reference tissue experiences 12 hours light / 12 hours dark, twice. The external N reservoir is maintained; no water depletion or growth is calculated. R is an internal reserve in µmol N, A_cum is cumulative assimilated N and I_cum is cumulative uptake from the external reservoir. Initial R = 2 µmol; both cumulative pools begin at zero.

```text
In light: U = 1.8/[1+(R/4)²]; A = 1.5 R/(1+R)
In dark:  U = 0.65/[1+(R/4)²]; A = 0.35 R/(1+R)
dR/dt = U - A
dA_cum/dt = A
dI_cum/dt = U
R + A_cum = 2 + I_cum
```

Rates are µmol N/h for the reference tissue. Denominator constants have units of µmol N. Capacities change at the prescribed light transitions; the solver restarts at each discontinuity, with duplicate boundary timestamps retaining both rate states. Stored carbon and energy are assumed sufficient for dark processing. Their exhaustion, circadian regulation, nitrate-reductase dynamics and structural growth are not simulated. The claim is that non-zero dark uptake is possible, not that these particular rates apply to all algae.

Native scene 1 lasts 24 seconds: frame f maps to 48(f−1)/719 model hours. Its lamp changes at the four 6-second native intervals. The final film maps seconds 26–66 to 0–48 model hours, with initial and final holds. Reserve-marker volume is baked from the same table. The camera's reference tissue and exposed reserve container are two views of the same model organism, not separate treatments. Lighting itself is illustrative.

## Light, temperature and biphasic examples

The light-processing response is f(I) = (I/250) exp(1−I/250), shown for 0–800 µmol photons/m²/s. Its peak is 1 at I = 250 by construction. The temperature response is f(T) = exp(−[(T−20)/12]²), shown only for 5–35°C, with its chosen peak at 20°C. They are relative processing curves, not fitted uptake responses or universal optima. They do not imply a mechanistic stress model. A Q10 ratio needs a specified process and temperature interval.

The two-component response is V = 2S/(1+S) + 6S/(30+S), S in µmol/L and V in illustrative uptake units. Both components act simultaneously. The sum is concave and increasing over the shown domain: there is **no abrupt transporter switch or acceleration in slope**. The second component contributes over a wider concentration range. This decomposition does not identify actual proteins, passive transport or energetic costs; other descriptions can fit limited data.

## The Smit result

In [Smit (2002)](../../../docs/Smit_2002.pdf), Gracilaria gracilis showed approximately 38% suppression of nitrate uptake with ammonium-N above 5 µmol/L (abstract; results and Figures 6–7). PDF text extraction can misread the µ symbol as m; the original figure on printed page 203 was visually checked. The film's 100 versus 62 bars are a normalised summary, without invented error bars or a fitted dose-response curve. They are not raw paired measurements. Nitrate uptake was not universally absent while ammonium remained.

The native molecules rotate gently in scene 2; the 12-second loop repeats through the 22-second pathway explanation. Chemistry labels and all quantitative plots are composed outside Blender. Chemical glyphs use Arial Unicode because the suite's Avenir font lacks some subscripts and superscripts.

## Production source

The local production scripts, dependencies and reproduction instructions are kept in `_private/animations/BDC223/blender_nutrient_uptake/`. They are excluded from Git and the website.
