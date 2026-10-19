"""Illustrative datasets for BDC223 Lecture 7b (Measuring Photosynthesis).

Every number shown in the lecture text, slides, figures and animations comes from this
module. The data are simulated from stated models with a fixed random seed; they are
teaching examples, not measurements of a particular species.

Run `uv run --with scipy --with numpy python models.py` to write data/*.dat (pgfplots tables) and values.json.
"""
import json
import math
import random
from pathlib import Path

HERE = Path(__file__).parent
rng = random.Random(7)


def jp(I, Pmax, alpha, R=0.0):
    """Jassby and Platt (1976) tanh model with dark respiration (net rate)."""
    return Pmax * math.tanh(alpha * I / Pmax) - R


def linfit(x, y):
    n = len(x); mx = sum(x) / n; my = sum(y) / n
    b = sum((a - mx) * (c - my) for a, c in zip(x, y)) / sum((a - mx) ** 2 for a in x)
    return b, my - b * mx


def fit_jp(I, P, p0):
    """Non-linear least-squares fit of the tanh model (SciPy); returns Pmax, alpha, R."""
    from scipy.optimize import curve_fit
    import numpy as np
    f = lambda x, Pm, a, R: Pm * np.tanh(a * x / Pm) - R
    p, _ = curve_fit(f, np.array(I, float), np.array(P, float), p0=p0)
    return tuple(float(v) for v in p)


def fit_jp0(I, P, p0):
    """As fit_jp, without a respiration term (e.g. electron transport rates)."""
    from scipy.optimize import curve_fit
    import numpy as np
    f = lambda x, Pm, a: Pm * np.tanh(a * x / Pm)
    p, _ = curve_fit(f, np.array(I, float), np.array(P, float), p0=p0)
    return tuple(float(v) for v in p)


# ---------------------------------------------------------------- 1. O2 chamber (seaweed)
O2 = dict(volume_L=0.20, mass_gFW=2.0, Pmax=30.0, alpha=0.25, R=5.0, step_min=5.0, start_uM=250.0,
          levels=[0, 25, 50, 100, 200, 400, 600, 800], noise_uM=0.35)


def o2_experiment():
    t, c, steps = [], [], []
    conc = O2['start_uM']; minute = 0.0; dt = 1 / 6        # a reading every 10 s
    for I in O2['levels']:
        P = jp(I, O2['Pmax'], O2['alpha'], O2['R'])        # µmol O2 g-1 h-1
        slope_true = P * O2['mass_gFW'] / O2['volume_L'] / 60   # µM min-1
        seg_t, seg_c = [], []
        for k in range(int(O2['step_min'] / dt) + 1):
            tt = minute + k * dt
            cc = conc + slope_true * k * dt
            seg_t.append(tt); seg_c.append(cc + rng.gauss(0, O2['noise_uM']))
        # measured slope uses minutes 1-5 of each step (the first minute is discarded as an acclimation period)
        use = [(a, b) for a, b in zip(seg_t, seg_c) if a - minute >= 1.0]
        b, a = linfit([u[0] for u in use], [u[1] for u in use])
        rate = b * 60 * O2['volume_L'] / O2['mass_gFW']        # µmol O2 g-1 h-1
        steps.append(dict(I=I, t0=minute, t1=minute + O2['step_min'], slope=b, intercept=a, rate=rate, true=P))
        t += seg_t; c += seg_c
        conc = conc + slope_true * O2['step_min']; minute += O2['step_min']
    return t, c, steps


# ---------------------------------------------------------------- 2. Open gas exchange (leaf)
IRGA = dict(flow_umol_s=500.0, area_cm2=6.0, Cr=420.0, Amax=22.0, alpha=0.060, Rd=1.2,
            levels=[2000, 1500, 1000, 600, 400, 200, 100, 50, 25, 0], noise_ppm=0.15)


def irga_curve():
    rows = []
    for I in IRGA['levels']:
        A_true = jp(I, IRGA['Amax'], IRGA['alpha'], IRGA['Rd'])          # µmol CO2 m-2 s-1
        dC = A_true * (IRGA['area_cm2'] * 1e-4) / (IRGA['flow_umol_s'] * 1e-6)   # µmol mol-1
        Cs = IRGA['Cr'] - dC + rng.gauss(0, IRGA['noise_ppm'])
        A = (IRGA['flow_umol_s'] * 1e-6) * (IRGA['Cr'] - Cs) / (IRGA['area_cm2'] * 1e-4)
        rows.append(dict(I=I, Cs=Cs, dC=IRGA['Cr'] - Cs, A=A, true=A_true))
    return rows


# ---------------------------------------------------------------- 3. Light and dark bottles (Winkler)
WINKLER = dict(initial=7.80, light=8.45, dark=7.55, hours=6.0, PQ=1.2)


def winkler():
    w = WINKLER
    NP = (w['light'] - w['initial']) / w['hours']; R = (w['initial'] - w['dark']) / w['hours']; GP = NP + R
    to_C = lambda x: x * 12 / 32 / w['PQ'] * 1000        # mg O2 L-1 h-1 -> mg C m-3 h-1
    return dict(NP=NP, R=R, GP=GP, NP_C=to_C(NP), GP_C=to_C(GP))


# ---------------------------------------------------------------- 4. 14C incubation
C14 = dict(uCi_added=5.0, dpm_per_uCi=2.22e6, DIC_mmol_L=2.0, hours=2.0, dpm_light=3350.0, dpm_dark=150.0, isotope_factor=1.05)


def c14():
    d = C14
    dpm_added = d['uCi_added'] * d['dpm_per_uCi']
    DIC_mgC_m3 = d['DIC_mmol_L'] * 12 * 1000
    frac = (d['dpm_light'] - d['dpm_dark']) / dpm_added
    P = frac * DIC_mgC_m3 * d['isotope_factor'] / d['hours']
    return dict(dpm_added=dpm_added, DIC_mgC_m3=DIC_mgC_m3, frac=frac, P=P)


# ---------------------------------------------------------------- 5. PAM fluorometry
PAM = dict(F0=300.0, Fm=1500.0, absorptance=0.84, PSII_share=0.5, ETRmax=110.0,
           levels=[0, 15, 45, 90, 150, 250, 400, 600, 900, 1200])


def pam_rlc():
    Fv_Fm = (PAM['Fm'] - PAM['F0']) / PAM['Fm']
    alpha = Fv_Fm * PAM['PSII_share'] * PAM['absorptance']        # e- per incident photon at low light
    rows = []
    for I in PAM['levels']:
        etr = PAM['ETRmax'] * math.tanh(alpha * I / PAM['ETRmax']) if I else 0.0
        Y = etr / (I * PAM['PSII_share'] * PAM['absorptance']) if I else Fv_Fm
        Y *= 1 + rng.gauss(0, 0.012)
        rows.append(dict(I=I, Y=Y, ETR=Y * I * PAM['PSII_share'] * PAM['absorptance']))
    return Fv_Fm, alpha, rows


def pam_trace():
    """Schematic saturation-pulse trace (relative fluorescence) for figures and animation."""
    pts = []
    F, Fm = PAM['F0'], PAM['Fm']
    def add(t0, t1, f):
        n = max(2, int((t1 - t0) * 4))
        for k in range(n + 1):
            tt = t0 + (t1 - t0) * k / n; pts.append((tt, f(tt)))
    add(0, 20, lambda t: F)                                          # measuring light only: F0
    add(20, 20.8, lambda t: Fm)                                      # saturating pulse: Fm
    add(20.8, 30, lambda t: F)
    # actinic light on at 30 s: rise then decline to steady state F (~600)
    add(30, 120, lambda t: 600 + 420 * math.exp(-(t - 30) / 12) * (1 - math.exp(-(t - 30) / 1.5)))
    # saturating pulses in the light at 60 and 90 s give Fm' = 950
    out = []
    for tt, f in pts:
        if any(tp <= tt <= tp + 0.8 for tp in (60, 90)):
            f = 950
        out.append((tt, f))
    return out


# ---------------------------------------------------------------- write outputs
def write():
    (HERE / 'data').mkdir(exist_ok=True)
    t, c, steps = o2_experiment()
    with open(HERE / 'data' / 'o2_trace.dat', 'w') as f:
        f.write('t c\n'); f.writelines(f'{a:.4f} {b:.3f}\n' for a, b in zip(t, c))
    Pm, a, R = fit_jp([s['I'] for s in steps], [s['rate'] for s in steps], (28, 0.24, 5))
    with open(HERE / 'data' / 'o2_rates.dat', 'w') as f:
        f.write('I rate\n'); f.writelines(f"{s['I']} {s['rate']:.3f}\n" for s in steps)
    irga = irga_curve()
    Am, aa, Rd = fit_jp([r['I'] for r in irga], [r['A'] for r in irga], (21, 0.055, 1.2))
    with open(HERE / 'data' / 'irga.dat', 'w') as f:
        f.write('I A dC\n'); f.writelines(f"{r['I']} {r['A']:.3f} {r['dC']:.3f}\n" for r in irga)
    Fv_Fm, alpha_etr, rlc = pam_rlc()
    E_m, E_a = fit_jp0([r['I'] for r in rlc[1:]], [r['ETR'] for r in rlc[1:]], (105, 0.33))
    with open(HERE / 'data' / 'rlc.dat', 'w') as f:
        f.write('I Y ETR\n'); f.writelines(f"{r['I']} {r['Y']:.4f} {r['ETR']:.2f}\n" for r in rlc)
    with open(HERE / 'data' / 'pam_trace.dat', 'w') as f:
        f.write('t F\n'); f.writelines(f'{a:.3f} {b:.1f}\n' for a, b in pam_trace())
    vals = dict(
        o2=dict(settings=O2, steps=steps, fit=dict(Pmax=Pm, alpha=a, R=R, Ik=Pm / a, Ic=Pm / a * math.atanh(R / Pm))),
        irga=dict(settings=IRGA, rows=irga, fit=dict(Amax=Am, alpha=aa, Rd=Rd, Ik=Am / aa, Ic=Am / aa * math.atanh(Rd / Am))),
        winkler=dict(settings=WINKLER, **winkler()),
        c14=dict(settings=C14, **c14()),
        pam=dict(settings=PAM, Fv_Fm=Fv_Fm, alpha_incident=alpha_etr, rows=rlc,
                 fit=dict(ETRmax=E_m, alpha=E_a, Ek=E_m / E_a),
                 example=dict(I=400, F=600, Fm_prime=950, Y=(950 - 600) / 950, ETR=(950 - 600) / 950 * 400 * 0.5 * 0.84)))
    (HERE / 'values.json').write_text(json.dumps(vals, indent=1))
    return vals


if __name__ == '__main__':
    v = write()
    print(json.dumps({k: v[k].get('fit', {k2: v[k][k2] for k2 in v[k] if k2 != 'settings'}) for k in v}, indent=1, default=str)[:2500])
