# BDC223 climate figure data

The source snapshots were retrieved on **15 October 2026**. Their URLs and
SHA-256 checksums are in `sources.json`. NOAA's atmospheric files in this snapshot
were created on 9 October 2026. Recent observations may be revised by the providers.

| File | Provider and use |
|---|---|
| `co2_mm_mlo.txt` | NOAA GML / Scripps monthly Mauna Loa CO2, March 1958–September 2026; monthly means and seasonally adjusted values |
| `co2_annmean_gl.txt` | NOAA GML global marine boundary layer annual means, 1979–2025 |
| `co2_gr_gl.txt` | NOAA GML within-year global CO2 growth; the figure uses 1980–2025, excluding the earlier two-station approximation |
| `GLB.Ts+dSST.csv` | NASA GISTEMP v4 land–ocean temperature index, annual means through 2025 and monthly anomalies through September 2026; reference period 1951–1980 |
| `antarctica2015co2composite.txt` | Bereiter et al. (2015) revised Antarctic composite, archived by NOAA NCEI, study 17975; gas ages in calendar years before 1950 |
| `african-timeline.csv` | A schematic summary of the African chronology in Lecture 4; dates are approximate and rows are not spaced to scale |

## Regenerating the figures

From the repository root, run:

```sh
Rscript BDC223/scripts/update_climate_figures.R
```

The script needs R packages `ggplot2`, `svglite` and `jsonlite`. It reads the local
snapshots without network access, writes eight SVG figures to `BDC223/images/climate/`,
and records their numerical summaries in `figure_values.json`.

For a later update, download new snapshots from the URLs in `sources.json`, update
the retrieval date and SHA-256 checksums, then run the script. Reconcile the lecture's
numbers and captions against `figure_values.json` before rendering the two lectures.

The 50-year temperature rate is an ordinary least-squares fit to 1976–2025 NASA
annual anomalies, multiplied by ten to express degrees Celsius per decade. It is
a descriptive observed trend, not the separately assessed human-induced rate from
Forster et al. (2026). The seasonal CO2 plot uses monthly mean minus NOAA's
seasonally adjusted value, followed by a calendar-month average over 2021–2025.

The ice-core plots keep pre-1950 samples separate from the 2025 global observation.
The modern point is located at −0.075 ka on the years-before-1950 axis. Different
sampling resolutions and age uncertainty prevent interpretation as a uniformly
sampled continuous series. The monthly temperature plot shows anomalies against
each calendar month's 1951–1980 mean; it does not add an absolute seasonal cycle.

The African companion is a chronology, not a reconstruction of a single African
temperature or rainfall series. Its sources and interpretation are given beside
the figure in Lecture 4. Earlier than 22 ka, the longer African narrative continues
to provide the Miocene, Pliocene and Pleistocene context.
