> **TIP:**
>
> - [Lab 4: Nutrient Uptake](../BDC223/Lab4_nitrogen_uptake.llms.md)

> **TIP:**
>
> - Measuring nutrient uptake from the disappearance of a nutrient from the water.
> - The multiple flask method: set-up, data and the calculation of an uptake rate.
> - The perturbation method: depletion curves and interval uptake rates.
> - From a depletion curve to the $V$–$[S]$ graph; amounts versus rates; controls and sampling losses.
> - The Michaelis–Menten model and the meaning of $V_{max}$, $K_m$ (or $K_s$) and the initial slope $\alpha$.
> - An outline of fitting the model by nonlinear regression.

> **TIP:**
>
> [Lecture 9a](../BDC223/L09a-nutrient_uptake.llms.md) explained how nutrients reach a seaweed and cross its membranes. This lecture shows how we measure the rate at which that happens. It works through the two standard experimental designs, the multiple flask and the perturbation experiment, and converts the raw concentration data into biomass-specific uptake rates step by step. It then fits these rates to the Michaelis–Menten model and interprets its parameters ecophysiologically. The calculations prepare you for [Lab 4](../BDC223/Lab4_nitrogen_uptake.llms.md), and the [Blender visualisations](../BDC223/L09c-nutrients_visualised.llms.md) show the same processes in animated form.

> **TIP:**
>
> By the end of this lecture, you will be able to:
>
> 1.  Describe the multiple flask and perturbation experiments, and distinguish them by their set-up, the independence of their data and the way they are analysed.
> 2.  Calculate a biomass-specific nutrient uptake rate from start and end concentrations, water volume, algal mass and incubation time, and express it in µmol N or µg N.
> 3.  Construct $V$–$[S]$ data from a depletion curve by pairing each interval rate with a representative concentration.
> 4.  Distinguish the amount of nutrient taken up from the rate of uptake.
> 5.  Calculate uptake rates with the Michaelis–Menten equation, and interpret $V_{max}$, $K_m$ and $\alpha$.
> 6.  Explain why controls, sampling losses and the condition of the alga must be considered before a loss of nutrient from the water is attributed to uptake.

> **NOTE:**
>
> - [Lecture 9a: Nutrient Uptake](../BDC223/L09a-nutrient_uptake.llms.md)
> - [Smit, A. J. (2002). Nitrogen uptake by *Gracilaria gracilis* (Rhodophyta): adaptations to a temporally variable nitrogen environment. Botanica Marina, 45(2), 196–209.](../docs/Smit_2002.pdf)

## 1 Algal nutrient uptake experiments

> **TIP:**
>
> The animation below follows one nitrate pulse from the flask, through a depletion curve, to the Michaelis–Menten relationship. Pause at the prediction questions. The [walk-through below](#sec-follow-nitrogen) explains the calculations and connects the moving points to the uptake process.

Your browser cannot play this video. [Download the animation](animations/nutrient_uptake/nutrient_uptake.mp4) and use the written walk-through below.

[Download the animation](animations/nutrient_uptake/nutrient_uptake.mp4) · [Illustrative measurements](animations/nutrient_uptake/illustrative_samples.csv)

The animation has on-screen explanations and no audio. Its numbers are illustrative, chosen to match the scale of the worked example in this lecture. They are not measurements from a particular species.

The uptake process of nitrogen by algae can be measured *in situ* or under controlled conditions in the laboratory. Nutrient uptake is often determined by measuring the disappearance of a nutrient from the culture medium over a time interval after the addition of the alga ([Smit 2002](#ref-smit2002nitrogen)). These experiments allow the calculation of *depletion curves*, and from the depletion curve the *uptake kinetics* can be determined.

To do this, we use two types of experiments: **multiple flask** experiments and **perturbation** experiments. In both, a known amount of nutrient (the substrate) is introduced into the water. We measure the decrease in its concentration over a timed interval, convert that decrease into an amount using the water volume, and divide by the algal mass and elapsed time. This gives a positive uptake rate, $V$, for a corresponding substrate concentration, $[S]$. The primary difference between the methods lies in the experimental setup and the data analysis.

The [Blender film, Measuring a changing uptake rate](../BDC223/L09c-nutrients_visualised.llms.md#sec-measuring-uptake), develops the same numerical example as the Manim animation above. It adds a comparison of the two experimental designs, a biomass intervention and a separate demonstration of sampling losses. Use its worked questions to check the distinction between concentration, amount and rate.

## 2 Multiple flask method

The multiple flask method is a widely used experimental technique for measuring the nutrient uptake kinetics of macroalgae. This method is used to determine how macroalgae absorb nutrients (such as nitrogen or phosphorus) from their environment under different substrate concentrations. The goal is to generate a relationship between the rate of nutrient uptake and the nutrient concentration available to the macroalgae. The experimental data can then be used to characterise the nutrient uptake kinetics, such as maximum uptake rate ($V_{max}$) and half-saturation constant ($K_m$). These model parameters tell us about the mechanisms that algae use to remove nitrogen or other nutrients from their environment to be incorporated into biomass.

### 2.1 Experimental setup

In the multiple flask method, a series of flasks are prepared, each containing a different initial concentration of the nutrient (substrate) to span the range of nutrient levels typically encountered by the macroalgae in its natural habitat. This allows for measurements of nutrient uptake rates across a spectrum of substrate concentrations, from low to high.

### 2.2 Steps of the multiple flask experiment

1.  **Substrate Preparation:** Prepare several flasks, each with a known initial concentration of the nutrient (*e.g.*, nitrogen) in solution. These concentrations should cover a range of interest, often from nutrient-limiting to saturating levels.
2.  **Algal Introduction:** Introduce a known biomass of macroalgae into each flask. The biomass should be standardised across all flasks (*e.g.*, 4.5 g of fresh macroalgal tissue per flask).
3.  **Incubation:** The flasks are incubated for a defined time period, typically 20–30 minutes, under controlled environmental conditions such as light and temperature.
4.  **Sampling:** At the beginning of the incubation ($t=0$) and at the end of the incubation period (*e.g.*, $t=20$ minutes), water samples are taken from each flask to measure the concentration of the nutrient in the water.
5.  **Nutrient Analysis:** The concentration of the nutrient in each water sample is analysed using chemical methods (*e.g.*, colorimetric analysis or ion chromatography).

The difference in nutrient concentration between the start and end of the incubation reflects the amount of nutrient taken up by the macroalgae during the experiment.

### 2.3 Data collected

- **Initial substrate concentrations** ($[S_{\text{initial}}]$) in each flask.
- **Final substrate concentrations** ($[S_{\text{final}}]$) after the incubation period.
- **Time** of incubation ($\Delta t$).
- **Algal biomass** in each flask (usually standardised, *e.g.*, 4.5 g fresh mass).

### 2.4 Calculations for determining nutrient uptake rate ($V$)

The following steps outline how to calculate the nutrient uptake rate ($V$) from the experimental data obtained using the multiple flask method (apply to data obtained from each flask). These steps convert changes in nutrient concentration into actual uptake rates, adjusted for algal biomass and incubation time.

#### Step 1: calculate the change in nutrient concentration ($\Delta[S]$)

To determine how much nutrient was taken up during the incubation, subtract the final nutrient concentration from the initial nutrient concentration:

$$\Delta [S] = [S_{\text{initial}}] - [S_{\text{final}}]$$

For example: $$\Delta [S] = 25 \, \mu M - 9.9 \, \mu M = 15.1 \, \mu M$$

This gives the reduction in nutrient concentration over the time period but does not yet account for the volume of the flask or the biomass of algae.

#### Step 2: convert concentrations to mass of nutrient present per flask

Convert the concentration of the nutrient (in μmol.L⁻¹) into the actual mass of nutrient (in μg) present in the flask. To do this, use the molecular mass (MM) of the nutrient (*e.g.*, nitrogen), which is 14.0067 g.mol⁻¹ for N.

For example: $$25 \, \mu M = 25 \, \mu mol/L \times 14.0067 \, \frac{g}{mol} = 350.17 \, \mu g \, N \, \text{(per liter)}$$ $$9.9 \, \mu M = 9.9 \, \mu mol/L \times 14.0067 \, \frac{g}{mol} = 138.67 \, \mu g \, N \, \text{(per liter)}$$

Next, account for the volume of the flask (*e.g.*, 500 mL). Since the above values are for 1 liter, divide by 2 to find the mass in 500 mL:

$$\text{Mass of N at the start} = 350.17 \, \mu g / 2 = 175.09 \, \mu g$$ $$\text{Mass of N at the end} = 138.67 \, \mu g / 2 = 69.34 \, \mu g$$

#### Step 3: calculate the amount of nutrient taken up by the alga

Now, calculate how much nutrient was taken up by the algae during the incubation:

$$\Delta \text{Mass of N} = 175.09 \, \mu g - 69.34 \, \mu g = 105.75 \, \mu g \, N$$

This represents the total amount of nitrogen removed from the water by the algal biomass during the 20-minute incubation.

#### Step 4: normalise nutrient uptake by algal biomass

To determine how much nutrient was taken up per unit mass of algae, divide the total nutrient uptake by the biomass of algae in the flask (*e.g.*, 4.5 g):

$$\text{N taken up per gram} = \frac{105.75 \, \mu g \, N}{4.5 \, g} = 23.5 \, \mu g \, N/g$$

This is the amount taken up per gram during the 20-minute incubation. Divide by elapsed time to obtain a rate.

#### Step 5: calculate the nutrient uptake rate per hour

If the experiment lasted 20 minutes, but the uptake rate needs to be expressed on an hourly basis, multiply this amount per gram by 3 h⁻¹ (because 20 minutes is one third of an hour):

$$\text{Nutrient uptake rate per hour} = 23.5 \, \mu g \, N/g \times 3\,\mathrm{h^{-1}} = 70.50 \, \mu g \, N/g/hr$$

### 2.5 Final workflow for calculating nutrient uptake rate

1.  **Determine the change in nutrient concentration** between the start and end of the experiment for each flask: $$\Delta [S] = [S_{\text{initial}}] - [S_{\text{final}}]$$
2.  **Convert concentrations to mass of nutrient** (*e.g.*, μg N) using the molecular mass and flask volume: $$\text{Mass of nutrient} = [S] \times \Omega \times \text{MM of nutrient}$$
3.  **Calculate the amount of nutrient taken up** by the algae: $$\Delta \text{Mass of nutrient} = \text{Mass of nutrient (initial)} - \text{Mass of nutrient (final)}$$
4.  **Normalise the nutrient uptake by the algal biomass**: $$\text{N taken up per gram} = \frac{\Delta \text{Mass of nutrient}}{\text{Algal biomass}}$$
5.  **Divide the amount per gram by elapsed time**, using $\Delta t$ in minutes to obtain an hourly rate: $$\text{Nutrient uptake rate (hourly)} = \text{N taken up per gram} \times \frac{60}{\Delta t}$$

## 3 Perturbation method

The perturbation method is an alternative approach for measuring nutrient uptake kinetics in macroalgae. This method involves a single flask where a relatively high initial concentration of a substrate nutrient is introduced, typically set at a level that is ecologically relevant to the system being studied. Rather than using multiple flasks with different initial nutrient concentrations, the perturbation method measures the decrease in nutrient concentration over time within the same flask. By sampling the substrate concentration at regular time intervals (*e.g.*, every 5 or 10 minutes), we can calculate nutrient uptake rates for each time period.

### 3.1 Experimental setup

In the perturbation method, a single flask is prepared with a high initial concentration of the nutrient (*e.g.*, nitrogen), and a known amount of macroalgal biomass is introduced. The flask is incubated, and water samples are taken at regular intervals to track the decrease in nutrient concentration. The resultant data allow us to calculate the nutrient uptake rate for each time interval.

#### Steps of the perturbation experiment

1.  **Substrate Preparation:** Add a known and high concentration of the nutrient (*e.g.*, 25 μM nitrogen) to the flask. The concentration should be high enough to ensure measurable changes over the course of the experiment but ecologically relevant.
2.  **Algal Introduction:** Introduce a known biomass of a macroalga into the flask (*e.g.*, 4.5 g of fresh macroalgal tissue).
3.  **Incubation and Sampling:** The flask is incubated, and water samples are taken at regular intervals (*e.g.*, every 5 or 10 minutes) to measure the nutrient concentration at each time point.
4.  **Nutrient Analysis:** The concentration of the nutrient in each water sample is analysed to determine how much nutrient remains at each time point.
5.  **Data Collection:** The change in nutrient concentration between each successive time point is used to calculate the nutrient uptake rate over the interval.

The resulting data from the perturbation method consist of a time series of substrate concentrations paired with calculated nutrient uptake rates over specific time intervals.

### 3.2 Data collected

- **Initial substrate concentration** ($[S_{initial}]$) and substrate concentrations at subsequent time points ($[S_{t1}]$, $[S_{t2}]$, …).
- **Time intervals** ($\Delta t$, *e.g.*, every 5 or 10 minutes).
- **Algal biomass** in the flask (*e.g.*, 4.5 g of fresh mass).

By plotting the remaining substrate concentration against each time point at which we sampled the water for nutrient measurement, we can construct a **nutrient depletion curve**. From this, we can observe how the nutrient is taken up by the macroalga and calculate the nutrient uptake rate at different stages of the experiment.

### 3.3 Calculations for determining nutrient uptake rate ($V$)

Once the experimental data have been collected, the next step is to calculate the nutrient uptake rate for each time interval based on the reduction in nutrient concentration between successive time points. The following steps outline how to perform these calculations.

#### Step 1: calculate the change in nutrient concentration ($\Delta [S]$)

To determine how much nutrient has been taken up during a specific time interval (*e.g.*, the first 5 minutes, for which $\Delta t = 5\;\mathrm{min}$), subtract the nutrient concentration at the end of the interval from the concentration at the start:

$$\Delta [S] = [S_{\text{start}}] - [S_{\text{end}}]$$

For example: $$\Delta [S] = 25 \, \mu M - 21.3 \, \mu M = 3.7 \, \mu M$$

This gives the reduction in nutrient concentration over the 5-minute interval.

#### Step 2: convert concentrations to mass of nutrient present per flask

Convert the nutrient concentration (in μmol.L⁻¹) into the mass of nutrient (in μg) present in the flask. To do this, use the molecular mass (MM) of the nutrient, which is 14.0067 g.mol⁻¹ for nitrogen.

For example: $$25 \, \mu M = 25 \, \mu mol/L \times 14.0067 \, \frac{g}{mol} = 350.17 \, \mu g \, N \, \text{(per liter)}$$ $$21.3 \, \mu M = 21.3 \, \mu mol/L \times 14.0067 \, \frac{g}{mol} = 298.34 \, \mu g \, N \, \text{(per liter)}$$

Since the flask contains 500 mL (0.5 L) of solution, divide the values by 2 to get the mass of nitrogen in the 500 mL flask:

$$\text{Mass of N at the start} = 350.17 \, \mu g / 2 = 175.09 \, \mu g$$ $$\text{Mass of N at the end} = 298.34 \, \mu g / 2 = 149.17 \, \mu g$$

#### Step 3: calculate the amount of nutrient taken up by the alga

Next, calculate the amount of nitrogen taken up by the algae during the 5-minute interval:

$$\Delta \text{Mass of N} = 175.09 \, \mu g - 149.17 \, \mu g = 25.92 \, \mu g \, N$$

This represents the total amount of nitrogen removed from the water by the algal biomass in the 5-minute period.

#### Step 4: normalise nutrient uptake by algal biomass

To determine how much nitrogen was taken up per unit mass of algae, divide the total nitrogen uptake by the algal biomass (*e.g.*, 4.5 g):

$$\text{N taken up per gram} = \frac{25.92 \, \mu g \, N}{4.5 \, g} = 5.76 \, \mu g \, N/g$$

This is the amount taken up per gram during the five-minute interval. Divide by elapsed time to obtain a rate.

#### Step 5: calculate the nutrient uptake rate per hour

Divide the amount taken up per gram by $5/60\;\mathrm{h}$ to obtain an hourly rate. This is equivalent to multiplying by $12\;\mathrm{h^{-1}}$:

$$\text{Nutrient uptake rate per hour} = 5.76 \, \mu g \, N/g \times 12\,\mathrm{h^{-1}} = 69.12 \, \mu g \, N/g/hr$$

This uptake rate relates to the specific time interval and can be used to track changes in $V$ over time. In this example, this uptake rate relates to the first 5 minutes of the experiment. The average $[S]$ during this interval was $(25 \, \mu M + 21.3 \, \mu M)/2 = 23.15 \, \mu M$.

Repeat these steps for each remaining interval and express $V$ relative to the mean $[S]$ for each interval (some authors use the $[S]$ at the start of the interval instead of the mean for the interval).

### 3.4 Final workflow for calculating nutrient uptake rate

1.  **Determine the change in nutrient concentration** between successive time points: $$\Delta [S] = [S_{\text{start}}] - [S_{\text{end}}]$$
2.  **Convert concentrations to mass of nutrient** using the molecular mass and flask volume: $$\text{Mass of nutrient} = [S] \times \Omega \times \text{MM of nutrient}$$
3.  **Calculate the amount of nutrient taken up** by the algae during the time interval: $$\Delta \text{Mass of nutrient} = \text{Mass of nutrient (start)} - \text{Mass of nutrient (end)}$$
4.  **Normalise the nutrient uptake by the algal biomass**: $$\text{N taken up per gram} = \frac{\Delta \text{Mass of nutrient}}{\text{Algal biomass}}$$
5.  **Divide the amount per gram by elapsed time**, using $\Delta t$ in minutes to obtain an hourly rate: $$\text{Nutrient uptake rate (hourly)} = \text{N taken up per gram} \times \frac{60}{\Delta t}$$

The important differences between the multiple flask and perturbation experiments are summarised in [Table 1](#tbl-diffs).

| Feature | Multiple Flask Experiments | Perturbation Experiments |
|:---|:---|:---|
| Experimental Setup | Multiple flasks, each with different $[S]$ | Single flask with initial high $[S]$ |
| Data Independence | Data points are independent | Data points are correlated (repeated measures) |
| Analysis | Nonlinear least squares regression (NLS) | Nonlinear mixed model (NLMM) |
| R Function | [`nls()`](https://rdrr.io/r/stats/nls.html) | [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html) |

Table 1: Key differences between multiple flask and perturbation experiments.

Our choice between multiple flask and perturbation experiments depends on our research questions and experimental constraints. In both methods, we must consider all sources of error and variability, such as measurement error, the type of nutrient, the physiological state of the alga, the light intensity, the experimental temperature, and other variables that might affect the uptake response.

## 4 Follow the nitrogen: reading the animation

### 4.1 First, keep track of where the nitrogen is

Imagine a piece of seaweed in $0.50\;\mathrm{L}$ of well-mixed seawater. Its fresh mass is $4.5\;\mathrm{g}$. Add a small volume of concentrated nitrate solution to bring the external concentration to $25\;\mu\mathrm{mol\,N\,L^{-1}}$. This is the perturbation: a sudden change in the supply of nitrogen.

Follow the gold dots, which represent parcels of nitrate-N, from the water into the seaweed. Their disappearance from the water is accompanied by an increase in the nitrogen held inside the tissue, where it may be stored or assimilated. Uptake does not require an immediately visible increase in the size of the seaweed.

There are initially $25\times0.50=12.5\;\mu\mathrm{mol}$ of added N in the flask. We keep a balance of this added nitrogen:

$$N_{\mathrm{water}}(t)+U(t)=12.5\;\mu\mathrm{mol\,N},
\qquad U(t)=\Omega\,[S_0-S(t)].$$

Here, $U(t)$ is the **amount taken up since the pulse**, and $\Omega$ is the water volume in litres. The seaweed already contained nitrogen before the experiment. That pre-existing nitrogen is outside this balance of the newly added pulse.

### 4.2 Then read the depletion curve

The horizontal axis is time. The vertical axis is the concentration still in the water, $S$. Each new measurement extends the depletion curve. A steep decline means that a large amount of nitrogen has left the water during that interval. A shallow decline means that less has left over the same amount of time.

In the animation, the no-seaweed control remains at $25\;\mu\mathrm{mol\,L^{-1}}$. This is what we would expect if the experimental conditions cause no other removal of dissolved N. In a real experiment, compare that control with the seaweed flask before attributing all disappearance to the seaweed. Microbial uptake, adsorption, nutrient release and sampling losses can complicate the balance.

### 4.3 Turn a slope into an uptake rate

The depletion slope is negative because $S$ falls with time. We report uptake as a positive quantity by using $S_{\mathrm{start}}-S_{\mathrm{end}}$. For a five-minute interval, with concentrations in $\mu\mathrm{mol\,N\,L^{-1}}$:

$$\overline V
=\frac{(S_{\mathrm{start}}-S_{\mathrm{end}})\,\Omega}{M\,(5/60)}
\quad\left[\mu\mathrm{mol\,N\,g^{-1}\,h^{-1}}\right].$$

The volume converts concentration into an amount, $M$ standardises that amount per gram of seaweed, and $5/60$ expresses the interval in hours. Keep those three operations in mind. They tell you what each part of the calculation is doing.

This expression assumes that the water volume is effectively constant over the interval and that the concentration change has been attributed appropriately to uptake. If sample withdrawals appreciably reduce volume, use the remaining volume for the following interval and account for the exported nutrient in the overall inventory. The [sampling example](../BDC223/L09c-nutrients_visualised.llms.md#change-biomass-then-account-for-sampling) shows why loss of nitrogen from the flask is not always uptake.

| Interval | Start $S$ | End $S$ | Mean of endpoint concentrations | Mean uptake rate $\overline V$ |
|:---|---:|---:|---:|---:|
| 0–5 min | 25.00 | 21.30 | 23.15 | 4.93 |
| 30–35 min | 5.54 | 3.43 | 4.48 | 2.81 |

Concentrations are in $\mu\mathrm{mol\,N\,L^{-1}}$. Rates are in $\mu\mathrm{mol\,N\,g^{-1}\,h^{-1}}$, using fresh mass. These are rounded values from the animation’s model. The first rate is about $69.1\;\mu\mathrm{g\,N\,g^{-1}\,h^{-1}}$ after multiplying by $14.0067\;\mu\mathrm{g\,N}$ per $\mu\mathrm{mol\,N}$, consistent with the worked example above.

### 4.4 Move each interval onto the $V$–$S$ graph

Plot the first pair, approximately $(23.15,\;4.93)$, with concentration on the horizontal axis and uptake rate on the vertical axis, then add the pairs from successive intervals. This constructs the uptake–concentration relationship from the depletion experiment.

Watch the order in which the points appear. The early intervals lie towards the upper right, where both concentration and rate are high. Later intervals appear further left and lower down. **Time carries us from right to left along this curve during depletion.** Although we conventionally read an increasing concentration axis from left to right, the flask is losing nitrate as the experiment proceeds.

The midpoint of the two endpoint concentrations is a convenient approximation to the concentration experienced over a short interval. The calculated rate is also an interval average. It therefore sits close to, rather than necessarily exactly on, the instantaneous Michaelis–Menten curve. Very long intervals can hide substantial changes in rate.

### 4.5 Separate an amount from a rate

During the replay, the green curve shows accumulated uptake, $U(t)$. It rises as nitrogen enters the seaweed. Its slope becomes shallower as uptake slows. Near the end of the experiment, the seaweed has acquired almost all of the added nitrogen, while its **current uptake rate** is very low.

The moving tangents connect these observations. With time expressed in hours,

$$V(t)=-\frac{\Omega}{M}\frac{\mathrm dS}{\mathrm dt}
=\frac{1}{M}\frac{\mathrm dU}{\mathrm dt}.$$

If time is expressed in minutes, multiply the right-hand expressions by 60 to report an hourly rate. A high point on the accumulated-uptake curve means that much N has already been taken up. A steep tangent means that uptake is currently fast.

### 4.6 Explain the bend, then test your explanation

The animation uses $V_{\max}=6\;\mu\mathrm{mol\,N\,g^{-1}\,h^{-1}}$ and $K_s=5\;\mu\mathrm{mol\,N\,L^{-1}}$. At $S=K_s$, the uptake rate is $3$, half of $V_{\max}$. This occurs about $31.2$ minutes into this particular experiment. $K_s$ is a concentration, so it does not itself tell us how many minutes an experiment will take.

At low external concentration, nutrient arrival restricts uptake. The membrane sketch shows sites spending time waiting. When nitrate is abundant, the finite capacity of transport and subsequent processing becomes increasingly important. Doubling $S$ from $25$ to $50$ raises the modelled rate from $5.00$ to only $5.45$. The rate approaches $V_{\max}$ asymptotically. A saturating curve alone does not identify the detailed transport mechanism.

At very low $S$, $V\approx\alpha S$, where $\alpha=V_{\max}/K_s$. This initial slope describes uptake responsiveness when nitrate is scarce. Compare $V_{\max}$ as well as $K_s$ when comparing algae: a smaller $K_s$ alone does not guarantee a larger absolute uptake rate if their capacities differ.

Now predict the second pulse. If we restore the nitrate concentration to $25$, while keeping the uptake system unchanged, the point returns up and right along the same $V$–$S$ curve. The next depletion segment becomes steep again. The accumulated amount already taken up remains in the seaweed. This intervention tests the explanation that the first slowdown resulted from declining external supply.

> **NOTE:**
>
> This illustration assumes fixed water volume, algal fresh mass, mixing, temperature, light, $V_{\max}$ and $K_s$, with no other N inputs or losses between pulses. Sampling removes a negligible volume. In real experiments, account for the water and nutrients removed in samples, and measure controls and replicate flasks. Internal storage, nutritional history and regulation can also change uptake capacity during a run. A failure to recover after the second pulse would give us a reason to investigate those processes.
>
> The nitrate example is consistent with the rate-saturating response described for *Gracilaria gracilis* by Smit ([2002](#ref-smit2002nitrogen)), which also shows why water movement and the seaweed’s nutritional history matter. The animation’s parameter values are illustrative.

> **TIP:**
>
> 1.  A depletion curve becomes almost horizontal. What has happened to the external concentration and to the uptake rate? What additional evidence would you need before concluding that the seaweed had stopped functioning?
> 2.  Late in the experiment, accumulated uptake is high but uptake rate is low. Explain how both statements can be true.
> 3.  Double the seaweed mass while holding water volume, initial $S$, and uptake parameters per gram constant. Predict the initial depletion slope, the initial rate **per gram**, and the total amount of pulse N eventually available for uptake.
> 4.  After a second nitrate pulse, uptake remains slow. Suggest an explanation and an experiment that would distinguish it from low external nutrient supply.

Check your reasoning

1.  In this model, external N is nearly depleted and uptake is slow. A no-seaweed control helps identify other losses. A second pulse tests whether uptake can recover when supply is restored.
2.  The accumulated amount records the history of uptake. Its slope gives the current rate, which can be small after most available N has already entered the seaweed.
3.  The initial concentration decline doubles in magnitude because twice as much seaweed removes N. The initial rate per gram remains the same at the same $S$. The flask still contains only $12.5\;\mu\mathrm{mol}$ of added N, so doubling biomass does not double this available amount.
4.  For example, internal N stores may now suppress further uptake, or the incubation conditions may have changed. Compare the response with fresh tissue under matched light, temperature and mixing, and measure internal N if storage is your explanation. State the result you would expect under each explanation.

## 5 The Michaelis-Menten model

We apply the Michaelis–Menten model ([Equation 1](#eq-mm)) to data from multiple flask and perturbation experiments when a saturating concentration response describes nutrient uptake under the experimental conditions. It estimates limiting capacity and half-saturation. The fitted curve does not, by itself, establish irreversibility or identify the membrane’s energy-coupling mechanism.

The Michaelis-Menten equation is given by:

$$V_i = \frac{V_{max} \cdot [S_i]}{K_m + [S_i]} + \epsilon_i \tag{1}$$

Here $K_m$ denotes the half-saturation concentration, written $K_s$ in Lecture 9a and the animation. We use $K_s$ for the whole-alga uptake response. It is an operational half-saturation parameter, and need not be a direct measure of molecular binding affinity.

The [Blender film on capacity and concentration](../BDC223/L09c-nutrients_visualised.llms.md#sec-capacity-concentration) shows how uptake-site occupancy can produce this relationship. That film maintains concentration at the membrane; here we measure changing concentration in the flask. Surface and bulk concentrations need not be equal when external delivery is restrictive.

Where:

- $V_i$ is the uptake rate at the $i$-th observation,
- $V_{max}$ is the limiting uptake rate approached at high substrate concentration,
- $[S_i]$ is the substrate concentration at the $i$-th observation,
- $K_m$ is the Michaelis constant, which represents the substrate concentration at which the uptake rate is half of $V_{max}$, and
- $\epsilon_i$ is the error term at the $i$-th observation.

Both parameters have ecophysiological interpretations. $V_{max}$ represents the uptake capacity per unit algal mass under the experimental conditions. The half-saturation concentration tells us the external concentration needed to attain half of that capacity. For whole-alga uptake, this response can reflect nutrient delivery to the surface as well as transport and processing within the tissue. The initial slope, $\alpha=V_{max}/K_s$, is the useful comparison of absolute uptake responsiveness at low nutrient concentration ([Smit 2002](#ref-smit2002nitrogen)).

## 6 Data analysis: fitting the Michaelis-Menten model with nonlinear regression

> **TIP:**
>
> - [Biostatistics, Chapter 22: Nonlinear Regression](../BCB744/basic_stats/22-nonlinear-regression.llms.md)

I provide this information only as a matter of interest to BDC223 students, as this is an advanced topic that will only be covered in your BSc (Hons) degrees. It is intended to provide a glimpse into the type of analysis that can be performed on nutrient uptake data. For the purpose of this module, you will not be required to perform this analysis and can instead rely on fitting the $V$ vs $[S]$ curve by hand.

To formally model the Michaelis-Menten relationship, we use the [`nls()`](https://rdrr.io/r/stats/nls.html) function in R to apply a non-linear regression the data from multiple flask experiments. For the perturbation experiment, things are a bit more complicated. This method includes dependent data points because the measurements are taken from the same flask at different times, introducing a correlation between observations. This violates the independence assumption required for standard regression models. To accurately analyse these data, I recommend a *nonlinear mixed-effects model* implemented in the `nlme()` function. Mixed-effects models account for fixed effects (overall trends across all observations) and random effects (variation specific to individual experimental units, here each flask, within which the time points are repeated measures). This helps handle the correlation between repeated measures and produces reliable estimates of the uptake dynamics within the flask.

## 7 Summary

- Nutrient uptake is measured from the disappearance of a nutrient from the water surrounding a known mass of alga over a timed interval.
- An uptake rate is obtained by converting the fall in concentration into an amount using the water volume, dividing by algal mass and dividing by elapsed time; the normalisation basis (fresh or dry mass) must always be stated.
- In a multiple flask experiment, each flask starts at a different concentration and gives one independent estimate of $V$ at its $[S]$.
- In a perturbation experiment, one flask is sampled repeatedly as the nutrient is depleted, and each interval gives a rate that is paired with the mean concentration of that interval; these data are repeated measures and are not independent.
- The amount taken up records the history of uptake, whereas the uptake rate is the current slope; late in a depletion run the amount can be high while the rate is low.
- The Michaelis–Menten model describes a saturating response with two parameters: $V_{max}$, the limiting uptake capacity, and $K_m$ (or $K_s$), the concentration at which $V = V_{max}/2$; the initial slope $\alpha = V_{max}/K_m$ describes uptake at low concentration.
- A saturating curve does not by itself identify the transport mechanism, and controls, sampling losses and the nutritional history of the alga must be considered before a loss of nutrient is attributed to uptake.

## 8 Self-test questions

> **NOTE:**
>
> $\Delta[S] = 25 - 9.9 = 15.1\;\mu\mathrm{mol\,L^{-1}}$. In 0.5 L this is $7.55\;\mu\mathrm{mol}$ N, or $7.55 \times 14.0067 = 105.75\;\mu\mathrm{g}$ N. Per gram, $105.75/4.5 = 23.5\;\mu\mathrm{g\,N\,g^{-1}}$ over 20 minutes. Multiplying by $60/20 = 3\;\mathrm{h^{-1}}$ gives $70.5\;\mu\mathrm{g\,N\,g^{-1}\,h^{-1}}$ (about $5.03\;\mu\mathrm{mol\,N\,g^{-1}\,h^{-1}}$).

> **NOTE:**
>
> $V = 6 \times 25/(5 + 25) = 5.00$ and $V = 6 \times 50/(5 + 50) = 5.45\;\mu\mathrm{mol\,N\,g^{-1}\,h^{-1}}$, an increase of only about 9%. At concentrations well above $K_s$, the uptake system is close to its limiting capacity, so extra nitrate adds progressively less to the rate. The rate approaches $V_{max}$ asymptotically and never reaches it at a finite concentration.

> **NOTE:**
>
> $\alpha = V_{max}/K_s = 6/5 = 1.2\;\mathrm{L\,g^{-1}\,h^{-1}}$ (µmol N g⁻¹ h⁻¹ divided by µmol N L⁻¹). At very low concentrations $V \approx \alpha S$, so $\alpha$ describes how responsive uptake is when the nutrient is scarce. A lower $K_s$ alone does not guarantee faster uptake at low concentration if the two algae have different $V_{max}$; $\alpha$ combines both parameters.

> **NOTE:**
>
> $\overline V = (5.54 - 3.43) \times 0.50/[4.5 \times (5/60)] = 1.055/0.375 \approx 2.81\;\mu\mathrm{mol\,N\,g^{-1}\,h^{-1}}$. It is paired with the mean of the endpoint concentrations, $(5.54 + 3.43)/2 \approx 4.48\;\mu\mathrm{mol\,L^{-1}}$. The first interval gave about (23.15, 4.93), so this later point lies to the left and lower down: as the flask is depleted, time carries the points from right to left along the curve.

> **NOTE:**
>
> The multiple flask method uses several flasks, each starting at a different concentration, and measures one rate per flask. The perturbation method adds a high concentration to a single flask and measures successive rates as the nutrient is depleted. Flask data are independent and can be fitted by nonlinear least squares ([`nls()`](https://rdrr.io/r/stats/nls.html)). Perturbation data come from the same flask and alga and are correlated repeated measures, so a nonlinear mixed-effects model (`nlme()`) is needed.

> **NOTE:**
>
> Compare the flask with a no-seaweed control at the same concentration to detect background changes such as microbial uptake or adsorption. Account for the nitrogen and water removed in samples, because a withdrawn sample exports nitrogen without any uptake. Also consider nutrient release by the seaweed, and whether light, temperature, mixing or the alga’s internal nutrient status changed during the run.

## 9 Multiple-choice questions

Click an option to check your answer. A green tick marks the correct option, and its explanation then appears below the options.

**1.** Which statement about the half-saturation concentration, Kₛ, of a whole-alga uptake response is most correct?

- It is the concentration at which V equals Vmax, and it directly measures the binding affinity of one transporter.
- It is the time at which V falls to Vmax/2 in a depletion run, and it measures how quickly the flask is depleted.
- It is the concentration where V equals Vmax/2, and it need not measure the binding affinity of one transporter.
- It is the concentration at which V equals Vmax/2, and it is fixed for a species regardless of delivery or history.

Kₛ is a concentration, defined where V = Vmax/2. For a whole alga it is an operational parameter that can include delivery to the surface and processing in the tissue, so it need not equal a molecular binding constant, and it can change with conditions.

**2.** A depletion curve shows S against time, and a second curve shows accumulated uptake U(t) against time for the same flask. What does the slope of U(t), divided by algal mass, give?

- The current biomass-specific uptake rate at that moment in the experiment.
- The half-saturation concentration of the uptake system for that alga.
- The concentration of nitrate remaining in the water at that moment.
- The total amount of added nitrogen taken up per gram since the pulse began.

U(t) is an amount; its height records the history of uptake. Its slope, dU/dt divided by M, is the current rate. Late in a run U can be high while its slope, and therefore the rate, is low.

**3.** In a perturbation experiment, why do successive V–S points move from right to left on the V–S graph as the experiment proceeds?

- The horizontal axis of the V–S graph is time, and time increases towards the left.
- The alga’s Vmax declines with each interval, so points must move to the left.
- Later samples are smaller in volume, so their concentrations are underestimated.
- Nitrate is depleted, so each later interval is paired with a lower concentration.

The horizontal axis of the V–S graph is concentration, not time. As the flask loses nitrate, each successive interval has a lower mean S, so the points move left and, because uptake slows, also down.

**4.** Four replicate perturbation flasks are each sampled at eight times, giving 28 interval rates. A student fits one Michaelis–Menten curve to all 28 rates with ordinary nonlinear least squares. Which criticism is most correct?

- Rates from the same flask must be fitted with a straight line, because a curve requires flasks at different starting S.
- Rates from the same flask cannot be paired with concentrations, so only the first interval of each flask can be used.
- Rates from the same flask are independent replicates, so the 28 rates should be averaged before any curve is fitted.
- Rates from the same flask are repeated measures, so a mixed model with flask as a random effect is more appropriate.

Each flask contributes a series of correlated measurements from the same alga, which violates the independence assumed by nls(). A nonlinear mixed-effects model (nlme()) treats flask as a random effect and accounts for that correlation.

**5.** A perturbation run uses 30-minute sampling intervals, and the nitrate concentration halves within some intervals. Which statement about the resulting V–S pairs is most correct?

- Each rate is an instantaneous value paired with a start value, so the pairs must all lie exactly on the fitted curve.
- Each rate is an interval mean paired with an endpoint mean, so the pairs must lie exactly on the fitted curve.
- Each rate is an interval mean paired with an endpoint mean, so the pairs need not lie on the instantaneous curve.
- Each rate is an instantaneous value paired with an endpoint mean, so the pairs cannot be used to estimate Vmax.

The calculated rate averages uptake through time, whereas the paired concentration averages two endpoints. Because the model is nonlinear and S changes a lot over a long interval, the pair sits near, but not necessarily on, the curve. Shorter intervals reduce the mismatch.

**6.** One study reports Vmax per gram fresh mass and another reports Vmax per gram dry mass for the same species. Which conclusion is best supported?

- The values can be compared directly, because Vmax depends only on the transporters and not on mass.
- The values cannot be compared directly unless a measured fresh-to-dry-mass conversion is available.
- The dry-mass value must be smaller, because dry tissue contains fewer uptake sites than fresh tissue.
- The fresh-mass value must be the more accurate, because water forms part of the living tissue volume.

Normalisation basis must always be stated. A gram of dry mass represents more tissue than a gram of fresh mass, so the two rates differ numerically for the same alga and need a measured conversion before comparison.

**7.** A Michaelis–Menten curve fits a set of nitrate uptake rates very closely. Which conclusion does the good fit, on its own, support?

- Uptake rises with concentration and approaches a limiting rate under the experimental conditions.
- Uptake is restricted by diffusion through the boundary layer at all of the measured concentrations.
- Uptake is irreversible, and none of the nitrate taken up can return to the water during the run.
- Uptake occurs by secondary active transport coupled to the proton gradient across the membrane.

The model describes a saturating response and estimates Vmax and Kₛ. A saturating curve does not by itself identify the membrane mechanism, establish irreversibility, or show which step restricts the rate.

**8.** A perturbation experiment follows nitrate depletion until the uptake rate is very low. The researcher then adds nitrate to restore the initial concentration. Temperature, light, mixing and algal mass are unchanged, and samples remove negligible volume. After the second pulse the uptake rate stays low, well below the rate measured at the same concentration during the first pulse. Which interpretation is most correct?

- Falling external supply alone cannot explain the slowdown; the second pulse proves that the transporters were permanently destroyed.
- Falling external supply alone cannot explain the slowdown; internal N status or tissue condition may have reduced uptake capacity.
- Falling external supply alone explains the slowdown; the low rate shows that the added nitrate was not mixed into the flask water.
- Falling external supply alone explains the slowdown; the second pulse confirms that the uptake system was unchanged throughout.

If declining supply were the only cause, restoring S should return the rate to its earlier value at that concentration. Failure to recover points to a change in the alga, such as internal N stores suppressing uptake. Measuring internal N and comparing fresh tissue would test this; the result does not prove permanent damage.

**9.** Two flasks contain the same nitrate concentration at the start and the same tissue from one thallus. Flask 1 holds 2.0 g fresh mass in 1.0 L; flask 2 holds 4.0 g in 1.0 L. Light, temperature and mixing are matched. Flask 2 reaches S = 5 µmol L⁻¹ after 20 minutes and flask 1 after about 40 minutes. A student concludes that the alga in flask 2 has a lower Kₛ. Which evaluation is most correct?

- The conclusion is right; Kₛ is a concentration, and more biomass per litre lowers it because the uptake sites compete for nitrate.
- The conclusion is wrong; Kₛ is a concentration, and more biomass per litre depletes the water faster at the same rate per gram.
- The conclusion is wrong; Kₛ is a time, and more biomass per litre lengthens it because each gram receives less nitrate per minute.
- The conclusion is right; Kₛ is a time, and more biomass per litre shortens it because the uptake sites per gram are more abundant.

Kₛ is a concentration at which V = Vmax/2. The time needed to reach any concentration depends on the biomass-to-volume ratio: doubling M in the same volume doubles the rate of concentration decline while the rate per gram at a given S is unchanged.

**10.** Two seaweeds are compared in multiple flask experiments under matched light, temperature and mixing. Alga A has Vmax = 8 µmol N g⁻¹ h⁻¹ and Kₛ = 2 µmol L⁻¹. Alga B has Vmax = 20 µmol N g⁻¹ h⁻¹ and Kₛ = 4 µmol L⁻¹. Rates are on the same mass basis. A manager wants the species with faster uptake when nitrate stays near 0.4 µmol L⁻¹. Which recommendation is best supported?

- Alga B, because its higher Vmax (20 µmol N g⁻¹ h⁻¹) applies at every S, giving about 20 against 8 µmol N g⁻¹ h⁻¹.
- Alga A, because its lower Kₛ (2 µmol L⁻¹) means a higher affinity, giving about 1.8 against 1.3 µmol N g⁻¹ h⁻¹.
- Alga A, because its Kₛ/Vmax ratio (0.25 g h L⁻¹) exceeds B’s (0.20 g h L⁻¹), giving about 1.8 against 1.3 µmol N g⁻¹ h⁻¹.
- Alga B, because its initial slope (5 L g⁻¹ h⁻¹) exceeds A’s (4 L g⁻¹ h⁻¹), giving about 1.8 against 1.3 µmol N g⁻¹ h⁻¹.

α = Vmax/Kₛ: A = 8/2 = 4 and B = 20/4 = 5 L g⁻¹ h⁻¹. At S = 0.4, V_A = 8(0.4)/2.4 = 1.33 and V_B = 20(0.4)/4.4 = 1.82 µmol N g⁻¹ h⁻¹. Ranking by Kₛ alone gives the wrong answer when capacities differ.

**11.** A seaweed flask is sampled every 10 minutes, and each 25 mL sample is removed from an initial 0.30 L of water. After six samples, the student calculates every interval rate using 0.30 L and finds that the summed uptake exceeds the added N minus the N left in the water and the N removed in samples. No stock was added after the start. Which evaluation is most correct?

- The sampled water diluted the N and reduced the volume; later intervals need the remaining volume, and sampled N counts as uptake.
- The sampled water concentrated the N and raised the volume; later intervals need the initial volume, and sampled N counts as uptake.
- The sampled water exported N and reduced the volume; later intervals need the remaining volume, and sampled N is counted separately.
- The sampled water exported N but left the volume unchanged; later intervals need the initial volume, and sampled N counts as uptake.

Six 25 mL samples remove 0.15 L, half the water. A withdrawn sample removes N without any uptake and does not change concentration. Using the original volume overstates the amount lost in later intervals, so each interval should use the actual remaining volume and sampled N should enter the inventory separately.

**12.** A researcher plots interval uptake rates against the mean S of each interval for one perturbation run. Early points sit near V = 6.8 at S = 23, and late points near V = 0.7 at S = 0.4. The researcher fits a Michaelis–Menten curve and reports Vmax = 8 µmol N g⁻¹ h⁻¹ and Kₛ = 4 µmol L⁻¹. A second student says the data show uptake had stopped by the end of the run. Which response is most correct?

- Uptake had stopped at low S; the late points sit on the rising part of the curve, where V is effectively equal to zero.
- Uptake had stopped at low S; the late points sit on the flat part of the curve, where V is roughly equal to Vmax.
- Uptake was still positive at low S; the late points sit on the rising part of the curve, where V is roughly αS.
- Uptake was still positive at low S; the late points sit on the flat part of the curve, where V is roughly equal to Vmax.

At S = 0.4 the model gives V = 8(0.4)/4.4 ≈ 0.73, which is small but positive. At S well below Kₛ the curve is near-linear, V ≈ αS with α = 8/4 = 2 L g⁻¹ h⁻¹. A low rate late in the run reflects low external supply, not a stopped uptake system.

**13.** In a multiple flask experiment, thallus pieces for the low-concentration flasks were collected in the morning and kept in nitrate-free seawater, whereas those for the high-concentration flasks were collected in the afternoon from a nitrate-rich tank. All flasks were incubated for 20 minutes at matched light, temperature and mixing. The fitted Vmax is lower than in an earlier study. Which evaluation is most correct?

- The fitted curve isolates concentration from nutritional history, so the low Vmax must reflect the species’ capacity.
- The fitted curve confounds concentration with incubation time, so the low Vmax must reflect a short incubation period.
- The fitted curve isolates concentration from incubation time, so the low Vmax need not reflect any difference in nutritional history.
- The fitted curve confounds concentration with nutritional history, so the low Vmax need not reflect the species’ capacity.

Independent flasks can differ in tissue condition. Here the high-S flasks contain tissue with a different N history, which may suppress uptake. Concentration and history change together, so the curve cannot separate their effects; matched tissue should be allocated at random among concentrations.

**14.** A seaweed has Vmax = 15 µmol N g⁻¹ h⁻¹ and Kₛ = 3 µmol L⁻¹. Using V = Vmax S/(Kₛ + S), what is the uptake rate at S = 6 µmol L⁻¹?

- 7.5 µmol N g⁻¹ h⁻¹, one half of the limiting capacity.
- 10.0 µmol N g⁻¹ h⁻¹, two thirds of the limiting capacity.
- 5.0 µmol N g⁻¹ h⁻¹, one third of the limiting capacity.
- 30.0 µmol N g⁻¹ h⁻¹, twice the value of the limiting capacity.

V = 15 × 6/(3 + 6) = 90/9 = 10.0 µmol N g⁻¹ h⁻¹. The value 5.0 comes from swapping S and Kₛ in the numerator, 7.5 from assuming S = Kₛ, and 30.0 from using the low-S approximation αS = (15/3) × 6 at a concentration that is not low.

**15.** For an alga with Kₛ = 2.5 µmol L⁻¹, at what external concentration does the uptake rate reach 80% of Vmax? Rearranging V = Vmax S/(Kₛ + S) gives S = Kₛ f/(1 − f), where f = V/Vmax.

- S = 12.5 µmol L⁻¹, five times the half-saturation concentration.
- S = 2.0 µmol L⁻¹, eight tenths of the half-saturation concentration.
- S = 0.625 µmol L⁻¹, a quarter of the half-saturation concentration.
- S = 10 µmol L⁻¹, four times the half-saturation concentration.

S = 2.5 × 0.8/0.2 = 10.0 µmol L⁻¹. The distractors come from multiplying Kₛ by f (2.0), inverting the ratio (0.625) and dividing Kₛ by (1 − f) (12.5).

**16.** Uptake by a seaweed levels off near 12 µmol N g⁻¹ h⁻¹, which you take as Vmax. In a flask at S = 3 µmol L⁻¹ the rate is 4 µmol N g⁻¹ h⁻¹. Assuming Michaelis–Menten kinetics, Kₛ = S(Vmax/V − 1). What is Kₛ?

- 3.0 µmol L⁻¹, so the measured flask lies at half-saturation.
- 6.0 µmol L⁻¹, so the measured flask lies below half-saturation.
- 1.5 µmol L⁻¹, so the measured flask lies above half-saturation.
- 9.0 µmol L⁻¹, so the measured flask lies below half-saturation.

Kₛ = 3 × (12/4 − 1) = 3 × 2 = 6.0 µmol L⁻¹. Check: V = 12 × 3/(6 + 3) = 4. Omitting the −1 gives 9.0; inverting the bracket gives 1.5; assuming S = Kₛ ignores that V is only one third of Vmax.

**17.** An alga has Vmax = 18 µmol N g⁻¹ dry mass h⁻¹ and Kₛ = 4 µmol L⁻¹. Calculate the initial slope α and use it to estimate the uptake rate at S = 0.2 µmol L⁻¹.

- α = 4.5 L g⁻¹ h⁻¹; V ≈ 0.90 µmol N g⁻¹ h⁻¹
- α = 4.5 µmol L⁻¹; V ≈ 0.90 µmol N g⁻¹ h⁻¹
- α = 0.22 L g⁻¹ h⁻¹; V ≈ 0.04 µmol N g⁻¹ h⁻¹
- α = 72 L g⁻¹ h⁻¹; V ≈ 14.4 µmol N g⁻¹ h⁻¹

α = Vmax/Kₛ = 18/4 = 4.5 L g⁻¹ h⁻¹ (µmol N g⁻¹ h⁻¹ divided by µmol L⁻¹). V ≈ αS = 4.5 × 0.2 = 0.90; the full equation gives 0.86, close because S ≪ Kₛ. Kₛ/Vmax inverts the ratio, and α is not a concentration.

**18.** In a perturbation experiment, 3.0 g fresh mass of seaweed is held in 0.75 L of seawater. Between 20 and 30 minutes, nitrate falls from 18.4 to 15.2 µmol L⁻¹. Sampling volume is negligible. What is the interval uptake rate, and with which concentration is it paired?

- 4.8 µmol N g⁻¹ h⁻¹, paired with S = 16.8 µmol L⁻¹
- 0.8 µmol N g⁻¹ h⁻¹, paired with S = 16.8 µmol L⁻¹
- 4.8 µmol N g⁻¹ h⁻¹, paired with ΔS = 3.2 µmol L⁻¹
- 6.4 µmol N g⁻¹ h⁻¹, paired with S = 16.8 µmol L⁻¹

ΔS = 3.2 µmol L⁻¹; × 0.75 L = 2.4 µmol N; ÷ 3.0 g = 0.8 µmol N g⁻¹ per 10 min; × 6 h⁻¹ = 4.8 µmol N g⁻¹ h⁻¹. It is paired with the mean of the endpoints, (18.4 + 15.2)/2 = 16.8. Omitting volume gives 6.4; omitting the time conversion gives 0.8; ΔS is not the concentration experienced.

**19.** In a multiple flask experiment, a flask contains 250 mL of seawater and 1.5 g fresh mass of seaweed. Over a 30-minute incubation, nitrate falls from 40 to 28 µmol L⁻¹. Using 14.0067 µg N per µmol N, what is the uptake rate in µg N g⁻¹ h⁻¹?

- 224.1 µg N g⁻¹ h⁻¹
- 28.0 µg N g⁻¹ h⁻¹
- 56.0 µg N g⁻¹ h⁻¹
- 4.0 µg N g⁻¹ h⁻¹

ΔS = 12 µmol L⁻¹; × 0.25 L = 3.0 µmol = 42.02 µg N; ÷ 1.5 g = 28.01 µg N g⁻¹ per 30 min; × 2 h⁻¹ = 56.0 µg N g⁻¹ h⁻¹. Omitting volume gives 224.1, omitting the time conversion gives 28.0, and 4.0 is the rate in µmol, not µg.

**20.** A flask held 0.40 L until a 20 mL sample was withdrawn at 10 minutes, leaving 0.38 L. Between 10 and 20 minutes nitrate falls from 12.0 to 9.5 µmol L⁻¹, with 2.0 g fresh mass of seaweed present and no further sampling during the interval. What is the uptake rate for this interval?

- 2.85 µmol N g⁻¹ h⁻¹, using the remaining 0.38 L
- 5.70 µmol N g⁻¹ h⁻¹, using the remaining 0.38 L
- 3.00 µmol N g⁻¹ h⁻¹, using the original 0.40 L volume
- 0.48 µmol N g⁻¹ h⁻¹, using the remaining 0.38 L

2.5 µmol L⁻¹ × 0.38 L = 0.95 µmol N; ÷ 2.0 g = 0.475 µmol N g⁻¹ per 10 min; × 6 h⁻¹ = 2.85 µmol N g⁻¹ h⁻¹. Using 0.40 L overstates the amount; 0.48 omits the hourly conversion; 5.70 is the whole-flask rate in µmol N h⁻¹, not divided by mass.

## 10 References

Smit AJ (2002) Nitrogen uptake by *gracilaria gracilis* (Rhodophyta): Adaptations to a temporally variable nitrogen environment. Botanica Marina 45:196–209. doi: [10.1515/BOT.2002.019](https://doi.org/10.1515/BOT.2002.019)

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit2026,
  author = {Smit, A. J. and J. Smit, A.},
  title = {Lecture 9b: {Nutrient} Uptake Kinetics},
  date = {2026-11-06},
  url = {https://tangledbank.netlify.app/BDC223/L09b-nutrients_michaelis_menten.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ, J. Smit A (2026) Lecture 9b: Nutrient uptake kinetics. [https://tangledbank.netlify.app/BDC223/L09b-nutrients_michaelis_menten.html.](https://tangledbank.netlify.app/BDC223/L09b-nutrients_michaelis_menten.html)
