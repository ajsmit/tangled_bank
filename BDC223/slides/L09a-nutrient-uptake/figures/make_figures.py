"""Graphs for the Lecture 9a slides (nutrient uptake). Run: uv run --with matplotlib --with numpy python make_figures.py

Data sources (all values typed from the cited tables; nothing is digitised from figures):
  EPSTEIN   tissue concentrations adequate for growth of higher plants, mmol kg-1 dry matter
            (Epstein 1972; as tabulated in Taiz & Zeiger, Table 4.4).
  DEBOER    mean seawater and seaweed dry-matter concentrations (DeBoer 1981, in Lobban & Harrison 1997, Table 5.2).
  SMIT_*    Gracilaria gracilis uptake parameters from Smit (2002) Bot. Mar. 45: 196-209, Tables I-III.
Curves drawn from these parameters are model curves; no measured points are shown. Curves labelled
"schematic" use illustrative parameters chosen to show a shape, and say so on the slide.
"""
from pathlib import Path

import matplotlib
import numpy as np

matplotlib.use("pdf")
import matplotlib.pyplot as plt
from matplotlib import font_manager

HERE = Path(__file__).parent
FIRA = Path("/usr/local/texlive/2026/texmf-dist/fonts/opentype/public/fira")
for f in ("FiraSans-Book.otf", "FiraSans-Regular.otf", "FiraSans-SemiBold.otf", "FiraSans-BookItalic.otf"):
    if (FIRA / f).exists():
        font_manager.fontManager.addfont(str(FIRA / f))

INK, SEA, ACCENT, GREY = "#1F2A37", "#1D4E89", "#0F766E", "#6B7280"
LEAF, WARM, HOT, SAND = "#3F8F2F", "#B45309", "#B91C1C", "#F2B84B"
plt.rcParams.update({
    "font.family": "Fira Sans", "font.size": 8, "axes.titlesize": 8.5, "axes.labelsize": 8,
    "axes.edgecolor": INK, "axes.labelcolor": INK, "xtick.color": INK, "ytick.color": INK,
    "text.color": INK, "axes.spines.top": False, "axes.spines.right": False, "axes.linewidth": 0.8,
    "legend.frameon": False, "legend.fontsize": 7, "lines.linewidth": 1.8, "mathtext.fontset": "custom",
    "mathtext.rm": "Fira Sans", "mathtext.it": "Fira Sans:italic", "pdf.fonttype": 3,
})
W1, H1 = 3.3, 2.35   # one-column graph (inches)
W2, H2 = 6.3, 2.45   # full-width graph

V_LAB = r"$V$ (µmol N g$^{-1}$ dry h$^{-1}$)"

# Smit (2002) Table II: NO3- uptake, Michaelis-Menten (Vmax µmol N g-1 dry h-1, Ks µM)
SMIT_NO3 = {("replete", 15): (34.6, 6.9), ("replete", 20): (35.0, 5.6),
            ("limited", 15): (15.5, 6.5), ("limited", 20): (20.0, 4.2)}
# Smit (2002) Table I: NH4+ uptake, linear model slopes (V = slope * S), µmol N g-1 dry h-1 per µM
SMIT_NH4 = {("replete", 15): 1.8, ("replete", 20): 2.1, ("limited", 15): 2.5, ("limited", 20): 3.2}
# Smit (2002) Table III: NO3- uptake at three rates of water motion (oscillations min-1)
SMIT_WATER = {0: (13.7, 16.8), 250: (11.8, 4.8), 500: (15.3, 5.4)}

EPSTEIN = [("H", 60000), ("C", 40000), ("O", 30000), ("N", 1000), ("K", 250), ("Ca", 125), ("Mg", 80),
           ("P", 60), ("S", 30), ("Cl", 3.0), ("B", 2.0), ("Fe", 2.0), ("Mn", 1.0), ("Zn", 0.3),
           ("Cu", 0.1), ("Ni", 0.05), ("Mo", 0.001)]
# DeBoer (1981): seawater mmol kg-1; mean dry-matter µg g-1; atomic mass g mol-1
DEBOER = [("C", 2.3, 274000, 12.011), ("N", 0.03, 23000, 14.007), ("P", 0.002, 2800, 30.974),
          ("K", 10.2, 41100, 39.098), ("Ca", 10.3, 14300, 40.078), ("Mg", 53.2, 7300, 24.305),
          ("S", 28.2, 19400, 32.06), ("Fe", 1e-6, 300, 55.845), ("Zn", 6e-6, 90, 65.38),
          ("Cu", 4e-6, 15, 63.546), ("Mn", 0.5e-6, 50, 54.938)]


def mm(s, vmax, ks):
    return vmax * s / (ks + s)


def save(fig, name):
    fig.savefig(HERE / f"{name}.pdf", bbox_inches="tight", pad_inches=0.03)
    plt.close(fig)


def deplete(vmax, ks, s0=50.0, mass=0.5, vol=0.5, t_end=180, dt=0.1):
    """Concentration (µM) vs time (min) for a flask: dS/dt = -V(S) * M / Omega (M g dry, Omega L)."""
    t = np.arange(0, t_end + dt, dt)
    s = np.empty_like(t)
    s[0] = s0
    for i in range(1, len(t)):
        s[i] = max(s[i - 1] - mm(s[i - 1], vmax, ks) * mass / vol * dt / 60, 0)
    return t, s


def fig_plant_tissue():
    fig, ax = plt.subplots(figsize=(W2, H1))
    names, vals = zip(*EPSTEIN)
    cols = [SEA if v >= 30 else WARM for v in vals]
    ax.bar(names, vals, color=cols, width=0.7)
    ax.set_yscale("log")
    ax.set_ylim(5e-4, 2e5)
    ax.set_ylabel("mmol kg$^{-1}$ dry matter")
    ax.text(4, 3e4 * 3, "macronutrients", color=SEA, ha="center")
    ax.text(13, 30, "micronutrients", color=WARM, ha="center")
    save(fig, "fig_plant_tissue")


def fig_concentration_factor():
    fig, ax = plt.subplots(figsize=(W1 + 0.4, H1 + 0.2))
    names = [d[0] for d in DEBOER]
    factor = [(d[2] / d[3]) / d[1] for d in DEBOER]   # tissue mmol kg-1 dry / seawater mmol kg-1
    order = np.argsort(factor)[::-1]
    ax.barh([names[i] for i in order], [factor[i] for i in order],
            color=[HOT if names[i] in ("N", "P") else SEA for i in order], height=0.65)
    ax.set_xscale("log")
    ax.invert_yaxis()
    ax.set_xlabel("concentration in dry tissue ÷ in seawater")
    save(fig, "fig_concentration_factor")


def fig_liebig():
    """Attainable new biomass = min(N/10, P/1) for the worked example in the notes (10 µmol N, 1 µmol P per unit)."""
    p = np.linspace(0, 4, 400)
    fig, ax = plt.subplots(figsize=(W1, H1))
    for n, c in ((20, SEA), (30, ACCENT)):
        ax.plot(p, np.minimum(n / 10, p), color=c, label=f"{n} µmol N available")
    ax.plot([1, 2], [1, 2], "o", color=INK, ms=4, zorder=5)
    ax.annotate("P-limited", (0.6, 0.6), (0.15, 1.6), color=WARM, arrowprops=dict(arrowstyle="->", color=WARM))
    ax.annotate("N-limited at 20 µmol N", (3.2, 2), (2.4, 1.25), color=SEA, ha="left", arrowprops=dict(arrowstyle="->", color=SEA))
    ax.set_xlabel("P available (µmol)")
    ax.set_ylabel("new biomass units")
    ax.set_ylim(0, 3.4)
    ax.legend(loc="upper left")
    save(fig, "fig_liebig")


def fig_boundary_layer(name, cases, xlab="distance from thallus surface (µm)"):
    fig, ax = plt.subplots(figsize=(W1, H1))
    x = np.linspace(0, 400, 400)
    for cb, cs, delta, col, lab in cases:
        c = np.where(x < delta, cs + (cb - cs) * x / delta, cb)
        ax.plot(x, c, color=col, label=lab)
        ax.axvline(delta, color=col, lw=0.8, ls=":")
    ax.axvspan(-30, 0, color="#3F8F2F", alpha=0.25, lw=0)
    ax.text(-15, 2, "thallus", rotation=90, ha="center", va="bottom", color=LEAF, fontsize=7)
    ax.set_xlim(-30, 400)
    ax.set_ylim(0, 32)
    ax.set_xlabel(xlab)
    ax.set_ylabel("dissolved N (µM)")
    ax.legend(loc="lower right")
    save(fig, name)


def fig_depletion():
    fig, ax = plt.subplots(figsize=(W1, H1))
    for (state, col) in (("replete", SEA), ("limited", WARM)):
        t, s = deplete(*SMIT_NO3[(state, 15)])
        ax.plot(t, s, color=col, label=f"N-{state} parameters")
        ts = np.arange(0, 181, 15)
        ax.plot(ts, np.interp(ts, t, s), "o", color=col, ms=3.5)
    ax.set_xlabel("time (min)")
    ax.set_ylabel(r"NO$_3^-$-N in flask (µM)")
    ax.set_ylim(0, 55)
    ax.legend(loc="upper right")
    save(fig, "fig_depletion")


def fig_slope_to_v():
    vmax, ks = SMIT_NO3[("replete", 15)]
    t, s = deplete(vmax, ks)
    ts = np.array([0, 20, 40, 60, 80, 100])
    ss = np.interp(ts, t, s)
    fig, (a1, a2) = plt.subplots(1, 2, figsize=(W2, H2))
    a1.plot(t, s, color=GREY, lw=1)
    cols = (HOT, WARM, LEAF, ACCENT, SEA)   # one colour per interval, shared by both panels
    a1.plot(ts, ss, "o", color=INK, ms=4)
    for i, c in enumerate(cols):
        a1.annotate("", (ts[i + 1], ss[i + 1]), (ts[i], ss[i]), arrowprops=dict(arrowstyle="->", color=c, lw=1.8))
    a1.set_xlabel("time (min)")
    a1.set_ylabel("S (µM)")
    a1.set_title(r"1. slope $\Delta S/\Delta t$ for each interval", loc="left")
    v = -(np.diff(ss) / (np.diff(ts) / 60)) * 0.5 / 0.5   # Omega = 0.5 L, M = 0.5 g dry
    smid = (ss[:-1] + ss[1:]) / 2
    sx = np.linspace(0, 55, 200)
    a2.plot(sx, mm(sx, vmax, ks), color=GREY, lw=1, ls="--", label="Michaelis–Menten curve")
    a2.scatter(smid, v, c=cols, s=30, zorder=3, label="interval rates")
    a2.set_xlabel("mean S in interval (µM)")
    a2.set_ylabel(V_LAB)
    a2.set_title(r"2. $V = -(\Omega/M)\,\Delta S/\Delta t$, plotted against S", loc="left")
    a2.set_xlim(0, 55)
    a2.set_ylim(0, 40)
    a2.legend(loc="lower right").legend_handles[1].set_color(GREY)
    a2.text(0.98, 0.32, "colours match the intervals in panel 1", transform=a2.transAxes, ha="right", color=GREY, fontsize=7)
    fig.tight_layout(w_pad=2.5)
    save(fig, "fig_slope_to_v")


def fig_mm_nitrate():
    s = np.linspace(0, 60, 300)
    fig, ax = plt.subplots(figsize=(W1, H1))
    for (state, temp), (vmax, ks) in SMIT_NO3.items():
        if state == "replete":
            ax.plot(s, mm(s, vmax, ks), color=SEA if temp == 15 else HOT, ls="-" if temp == 15 else "--",
                    label=f"{temp} °C: $V_{{max}}$ = {vmax}, $K_s$ = {ks} µM")
    ax.set_xlabel(r"S (µM NO$_3^-$-N)")
    ax.set_ylabel(V_LAB)
    ax.set_ylim(0, 40)
    ax.legend(loc="lower right")
    save(fig, "fig_mm_nitrate")


def fig_mm_params():
    vmax, ks = SMIT_NO3[("replete", 15)]
    s = np.linspace(0, 60, 300)
    fig, ax = plt.subplots(figsize=(W1, H1))
    ax.plot(s, mm(s, vmax, ks), color=SEA)
    ax.axhline(vmax, color=HOT, lw=1, ls="--")
    ax.text(58, vmax + 1, r"$V_{max}$", color=HOT, ha="right")
    ax.plot([0, ks, ks], [vmax / 2, vmax / 2, 0], color=WARM, lw=1, ls=":")
    ax.text(ks + 1, 2, r"$K_s$", color=WARM)
    ax.text(ks + 1, vmax / 2 - 3.5, r"$V_{max}/2$", color=WARM)
    ax.plot([0, 8], [0, 8 * vmax / ks], color=LEAF, lw=1.2)
    ax.text(9, 39.5, r"$\alpha = V_{max}/K_s$ (initial slope)", color=LEAF, va="center")
    ax.set_xlabel("S (µM)")
    ax.set_ylabel(V_LAB)
    ax.set_xlim(0, 60)
    ax.set_ylim(0, 42)
    save(fig, "fig_mm_params")


def fig_alpha_ab():
    s = np.linspace(0, 10, 300)
    fig, ax = plt.subplots(figsize=(W1, H1))
    for (vmax, ks, lab, col) in ((4, 1, "A", SEA), (12, 2, "B", WARM)):
        ax.plot(s, mm(s, vmax, ks), color=col, label=f"{lab}: $V_{{max}}$ = {vmax}, $K_s$ = {ks}, α = {vmax / ks:g}")
        ax.plot([0.5], [mm(0.5, vmax, ks)], "o", color=col, ms=4)
    ax.axvline(0.5, color=GREY, lw=0.8, ls=":")
    ax.text(0.6, 10.5, "S = 0.5", color=GREY)
    ax.set_xlabel("S (same units)")
    ax.set_ylabel("V (same units)")
    ax.set_ylim(0, 12)
    ax.legend(loc="lower right")
    save(fig, "fig_alpha_ab")


def fig_phases():
    vmax, ks = SMIT_NO3[("replete", 15)]
    s = np.linspace(0, 60, 300)
    regions = {"surge": (44, 60, LEAF), "internal": (20, 60, SEA), "external": (0, 12, HOT)}
    for key, (lo, hi, col) in regions.items():
        fig, ax = plt.subplots(figsize=(W1, H1))
        ax.axvspan(lo, hi, color=col, alpha=0.13, lw=0)
        ax.plot(s, mm(s, vmax, ks), color=INK, lw=1.4)
        ax.set_xlabel("S (µM)")
        ax.set_ylabel(V_LAB)
        ax.set_xlim(0, 60)
        ax.set_ylim(0, 40)
        sec = ax.secondary_xaxis("top")
        sec.set_xticks([4, 56], [r"later ($t_{end}$)", r"start ($t_0$)"])
        sec.tick_params(length=0, labelsize=7, colors=GREY)
        save(fig, f"fig_phase_{key}")


def fig_nh4_linear():
    s = np.linspace(0, 40, 100)
    fig, ax = plt.subplots(figsize=(W1, H1))
    for (state, temp), slope in SMIT_NH4.items():
        ax.plot(s, slope * s, color=SEA if state == "replete" else WARM, ls="-" if temp == 15 else "--",
                label=f"N-{state}, {temp} °C: slope {slope}")
    ax.set_xlabel(r"S (µM NH$_4^+$-N)")
    ax.set_ylabel(V_LAB)
    ax.set_ylim(0, 130)
    ax.legend(loc="upper left")
    save(fig, "fig_nh4_linear")


def fig_linear_vs_mm():
    s = np.linspace(0, 40, 200)
    fig, ax = plt.subplots(figsize=(W1, H1))
    ax.plot(s, SMIT_NH4[("replete", 15)] * s, color=WARM, label=r"NH$_4^+$: linear, slope = α = 1.8")
    vmax, ks = SMIT_NO3[("replete", 15)]
    ax.plot(s, mm(s, vmax, ks), color=SEA, label=r"NO$_3^-$: Michaelis–Menten")
    ax.set_xlabel("S (µM N)")
    ax.set_ylabel(V_LAB)
    ax.set_ylim(0, 75)
    ax.legend(loc="upper left")
    save(fig, "fig_linear_vs_mm")


def fig_biphasic():
    s = np.linspace(0, 100, 400)
    high = mm(s, 10, 2)
    low = 0.25 * s
    fig, ax = plt.subplots(figsize=(W1, H1))
    ax.plot(s, high + low, color=INK, label="total uptake")
    ax.plot(s, high, color=SEA, ls="--", lw=1.2, label="high-affinity, saturating")
    ax.plot(s, low, color=WARM, ls="--", lw=1.2, label="low-affinity, unsaturated in range")
    ax.set_xlabel("S")
    ax.set_ylabel("V")
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_ylim(0, 45)
    ax.legend(loc="upper left", bbox_to_anchor=(0, 1.02))
    save(fig, "fig_biphasic")


def fig_water_motion():
    s = np.linspace(0, 60, 300)
    fig, ax = plt.subplots(figsize=(W1, H1))
    for (osc, (vmax, ks)), col, ls in zip(SMIT_WATER.items(), (GREY, SEA, ACCENT), ("-", "--", "-")):
        ax.plot(s, mm(s, vmax, ks), color=col, ls=ls,
                label=f"{osc} osc min$^{{-1}}$: $V_{{max}}$ {vmax}, $K_s$ {ks} µM")
    ax.set_xlabel(r"S (µM NO$_3^-$-N)")
    ax.set_ylabel(V_LAB)
    ax.set_ylim(0, 22)
    ax.legend(loc="upper right")
    save(fig, "fig_water_motion")


def fig_nutritional_history():
    s = np.linspace(0, 60, 300)
    fig, axes = plt.subplots(1, 2, figsize=(W2, H2), sharey=True)
    for ax, state in zip(axes, ("replete", "limited")):
        for temp, col, ls in ((15, SEA, "-"), (20, HOT, "--")):
            vmax, ks = SMIT_NO3[(state, temp)]
            ax.plot(s, mm(s, vmax, ks), color=col, ls=ls,
                    label=f"{temp} °C: $V_{{max}}$ {vmax}, $K_s$ {ks}")
        ax.set_title(f"N-{state}", loc="left")
        ax.set_xlabel(r"S (µM NO$_3^-$-N)")
        ax.set_ylim(0, 40)
        ax.legend(loc="lower right")
    axes[0].set_ylabel(V_LAB)
    fig.tight_layout(w_pad=2)
    save(fig, "fig_nutritional_history")


def fig_q10():
    t = np.linspace(5, 30, 200)
    fig, ax = plt.subplots(figsize=(W1, H1))
    for q, col, lab in ((2.0, HOT, "$Q_{10}$ = 2 (enzyme-driven)"), (1.2, SEA, "$Q_{10}$ = 1.2 (diffusion)")):
        ax.plot(t, q ** ((t - 15) / 10), color=col, label=lab)
    ax.axhline(1, color=GREY, lw=0.6, ls=":")
    ax.set_xlabel("temperature (°C)")
    ax.set_ylabel("rate relative to 15 °C")
    ax.legend(loc="upper left")
    save(fig, "fig_q10")


if __name__ == "__main__":
    fig_plant_tissue()
    fig_concentration_factor()
    fig_liebig()
    fig_boundary_layer("fig_bl_flow", [(30, 10, 300, GREY, "slow flow: thick layer"),
                                       (30, 22, 80, SEA, "fast flow: thin layer")])
    fig_boundary_layer("fig_bl_conc", [(30, 12, 200, SEA, r"high bulk N ($C_b$ = 30 µM)"),
                                       (10, 4, 200, WARM, r"low bulk N ($C_b$ = 10 µM)")])
    fig_depletion()
    fig_slope_to_v()
    fig_mm_nitrate()
    fig_mm_params()
    fig_alpha_ab()
    fig_phases()
    fig_nh4_linear()
    fig_linear_vs_mm()
    fig_biphasic()
    fig_water_motion()
    fig_nutritional_history()
    fig_q10()
    # self-check: the worked alpha example in the notes (1.33 and 2.40 at S = 0.5)
    assert round(mm(0.5, 4, 1), 2) == 1.33 and round(mm(0.5, 12, 2), 2) == 2.40
