"""Generate the TikZ sources for the Lecture 6 text figures.

fig-spectrum-sea.tex  : EM spectrum, solar spectrum and its attenuation with depth in clear ocean water
fig-ocean-zones.tex   : (a) ocean biozones, (b) light zones with depth reached by each colour
fig-beer-lambert.tex  : Beer-Lambert profiles (depth on x) with linear and logarithmic light axes

Spectral attenuation uses approximate diffuse attenuation coefficients (Kd) for very clear ocean
water (KD below). They are illustrative values chosen to reproduce the familiar pattern (blue
deepest, red and infrared lost within metres); they are not a measured data set. The surface
spectrum is a 5,778 K black body (no atmospheric absorption). Run: python3 make_figures.py
"""
import math
from pathlib import Path

HERE = Path(__file__).parent

# Approximate Kd (m^-1) for very clear ocean water, by wavelength (nm). Illustrative.
KD = [(350, 0.045), (375, 0.035), (400, 0.028), (425, 0.022), (450, 0.019), (475, 0.019),
      (500, 0.027), (525, 0.050), (550, 0.065), (575, 0.090), (600, 0.24), (625, 0.30),
      (650, 0.35), (675, 0.43), (700, 0.65), (725, 1.6), (750, 2.5), (800, 2.0)]


def kd(w):
    for (w0, k0), (w1, k1) in zip(KD, KD[1:]):
        if w0 <= w <= w1:
            return k0 + (k1 - k0) * (w - w0) / (w1 - w0)
    raise ValueError(w)


def planck(w):  # relative, 5,778 K; w in nm
    return (500 / w) ** 5 / (math.exp(14388000 / (w * 5778)) - 1) * (math.exp(14388000 / (500 * 5778)) - 1)


def par_fraction(z):
    ws = range(400, 701)
    return sum(planck(w) * math.exp(-kd(w) * z) for w in ws) / sum(planck(w) for w in ws)


def z_frac(w, f):  # depth where light at wavelength w falls to fraction f
    return math.log(1 / f) / kd(w)


BANDS = [('violet', 400, 450, 420), ('blue', 450, 495, 470), ('green', 495, 570, 530),
         ('yellow', 570, 590, 580), ('orange', 590, 620, 605), ('red', 620, 700, 660)]

PRE = r"""\documentclass[tikz,border=6pt]{standalone}
\usepackage[sfdefault,lining]{FiraSans}
\usepackage{amsmath}
\usepackage{firamath-otf}
\setmathrm{Fira Sans}
\usepackage{pgfplots}
\pgfplotsset{compat=1.18}
\usetikzlibrary{arrows.meta,calc,patterns}
\definecolor{ink}{HTML}{1F2A37}\definecolor{grey}{HTML}{6B7280}\definecolor{sea}{HTML}{1D4E89}
\definecolor{accent}{HTML}{0F766E}
\newcommand{\wavecol}[2]{\definecolor{tmpwave}{wave}{#2}\colorlet{#1}[rgb]{tmpwave}}
\begin{document}
"""

# ---------------------------------------------------------------- figure 1
def fig_spectrum_sea():
    X = lambda w: (w - 300) * 0.017          # cm per nm, shared by spectrum and sea block
    y0 = 1.8
    yd = {0: y0, 1: y0 - 2.6, 10: y0 - 5.4, 100: y0 - 8.2}
    def Y(z):                                # depth axis: linear to 1 m, logarithmic below
        return y0 - 2.6 * z if z <= 1 else y0 - 2.6 - 2.8 * math.log10(z)
    out = [PRE, r"\begin{tikzpicture}[font=\small]"]
    for name, w0, w1, wc in BANDS:
        out.append(rf"\wavecol{{c{name}}}{{{wc}}}")
    # --- EM spectrum bar (top)
    top = 10.4
    types = [('Gamma\\\\rays', 1.2), ('X-rays', 1.2), ('Ultra-\\\\violet', 1.5), ('', 0.45), ('Infrared', 1.6),
             ('Micro-\\\\waves', 1.9), ('Radio', 3.4)]
    x = 0.4
    out.append(rf"\node[anchor=east,font=\footnotesize,align=right] at (0.3,{top+0.6}) {{Type of\\radiation}};")
    for lab, wdt in types:
        if lab:
            out.append(rf"\fill[grey!22] ({x},{top}) rectangle ({x+wdt},{top+1.2}); \draw ({x},{top}) rectangle ({x+wdt},{top+1.2});")
            out.append(rf"\node[font=\scriptsize,align=center,text width={wdt-0.1}cm] at ({x+wdt/2},{top+0.6}) {{{lab}}};")
        else:
            vis0, vis1 = x, x + wdt
            out.append(rf"\shade[left color=violet,right color=red] ({x},{top}) rectangle ({x+wdt},{top+1.2}); \draw ({x},{top}) rectangle ({x+wdt},{top+1.2});")
        x += wdt
    for xx, lab in [(0.4, r'$10^{-12}$'), (1.6, r'$10^{-9}$'), (2.8, r'$10^{-8}$'), (vis0, r'$4\times10^{-7}$'),
                    (vis1, r'$7\times10^{-7}$'), (5.95, r'$10^{-3}$'), (7.85, r'$1$'), (11.25, r'$10^{3}$')]:
        out.append(rf"\draw ({xx},{top+1.2}) -- ({xx},{top+1.35}) node[above,rotate=90,anchor=west,font=\scriptsize] {{{lab}}};")
    out.append(rf"\node[anchor=east,font=\footnotesize] at (0.3,{top+1.9}) {{Wavelength (m)}};")
    # --- solar spectrum
    base, H = 4.6, 3.0
    out.append(rf"\draw[dashed,grey] ({vis0},{top}) -- ({X(400)},{base+H+1.0}) -- ({X(400)},{base+H*planck(400)}); \draw[dashed,grey] ({vis1},{top}) -- ({X(700)},{base+H+1.0}) -- ({X(700)},{base+H*planck(700)});")
    out.append(rf"\node[font=\footnotesize] at ({X(345)},{base+H+0.6}) {{Ultraviolet}};")
    out.append(rf"\node[font=\footnotesize] at ({X(550)},{base+H+0.6}) {{Visible}};")
    out.append(rf"\node[font=\footnotesize] at ({X(800)},{base+H+0.6}) {{Infrared}};")
    # --- sea block
    L, R, B = -0.9, 14.0, y0 - 9.2
    s = yd[0] + 1.0          # back edge of the sea surface
    out.append(rf"\fill[sea!18] ({L},{yd[0]}) -- ({R},{yd[0]}) -- ({R+1.0},{s}) -- ({L+1.0},{s}) -- cycle;")  # top face
    out.append(rf"\fill[sea!30] ({R},{yd[0]}) -- ({R+1.0},{s}) -- ({R+1.0},{B+2.6}) -- ({R},{B}) -- cycle;")   # side face
    out.append(rf"\fill[sea!10] ({L},{yd[0]}) rectangle ({R},{B});")
    out.append(rf"\draw ({L},{yd[0]}) -- ({R},{yd[0]}) -- ({R+1.0},{s}) -- ({L+1.0},{s}) -- cycle; \draw ({L},{yd[0]}) rectangle ({R},{B}); \draw ({R},{B}) -- ({R+1.0},{B+2.6}) -- ({R+1.0},{s});")
    out.append(rf"\node[anchor=east,font=\footnotesize] at ({L-0.1},{yd[0]}) {{sea surface}};")
    for name, w0, w1, wc in BANDS:
        pts = ' -- '.join(f"({X(w):.3f},{base + H*planck(w):.3f})" for w in range(w0, w1 + 1, 2))
        out.append(rf"\fill[c{name}!75] ({X(w0)},{base}) -- {pts} -- ({X(w1)},{base}) -- cycle;")
        out.append(rf"\fill[c{name}!75] ({X(w0)},{yd[0]}) rectangle ({X(w1)},{base});")
        out.append(rf"\node[rotate=90,font=\scriptsize] at ({(X(w0)+X(w1))/2},{base-1.0}) {{{name}}};")
    curve = ' -- '.join(f"({X(w):.3f},{base + H*planck(w):.3f})" for w in range(300, 1101, 5))
    out.append(rf"\draw[very thick] {curve};")
    out.append(rf"\draw[->] ({X(290)},{base}) -- ({X(1120)},{base}) node[right,font=\footnotesize] {{nm}};")
    for w in (300, 400, 500, 600, 700, 800, 900, 1000, 1100):
        out.append(rf"\draw ({X(w)},{base}) -- ({X(w)},{base-0.08}) node[below,font=\scriptsize,fill=white,inner sep=1pt] {{{w}}};")
    out.append(rf"\node[anchor=west,font=\footnotesize,align=left] at ({X(830)},{base+2.7}) {{Solar spectrum at the sea surface\\(5,778 K black body, schematic)}};")
    for name, w0, w1, wc in BANDS:   # bars fade with depth below the surface
        zf = z_frac(wc, 0.01)
        yb = max(Y(zf), B)
        out.append(rf"\shade[top color=c{name}!75,bottom color=sea!10] ({X(w0)},{yd[0]}) rectangle ({X(w1)},{yb:.3f});")
    for z in (1, 10, 100):
        yl = yd[z]
        out.append(rf"\draw[grey] ({L},{yl}) -- ({R},{yl}); \node[anchor=east,font=\footnotesize] at ({L-0.1},{yl}) {{{z} m}};")
        pts = ' -- '.join(f"({X(w):.3f},{yl + 1.8*planck(w)*math.exp(-kd(w)*z):.3f})" for w in range(350, 751, 5))
        out.append(rf"\draw[very thick] {pts};")
        out.append(rf"\node[anchor=west,font=\footnotesize,align=left] at ({X(810)},{yl+0.7}) {{{100*par_fraction(z):.0f}\% of surface PAR\\remains at {z} m}};")
    out.append(rf"\draw[-{{Stealth}},thick] ({L+0.6},{y0-3.4}) -- ({L+0.6},{y0-7.4}) node[midway,left,rotate=90,anchor=south] {{depth}};")
    out.append(rf"\node[font=\scriptsize,grey,anchor=south west] at ({L+0.1},{B+0.1}) {{Depth axis: linear to 1 m, logarithmic below. Colour bars fade out where each colour falls to 1\% of its surface value.}};")
    out.append(r"\end{tikzpicture}\end{document}")
    (HERE / 'fig-spectrum-sea.tex').write_text('\n'.join(out))
    return {z: round(100 * par_fraction(z), 1) for z in (1, 10, 100)}


# ---------------------------------------------------------------- figure 2
def fig_ocean_zones():
    out = [PRE, r"\begin{tikzpicture}[font=\small]"]
    # (a) biozones: depth axis stretched near the surface (not linear)
    def D(z):
        for z0, z1, y0, y1 in [(0, 200, 0, -0.9), (200, 1000, -0.9, -2.1), (1000, 2000, -2.1, -3.1), (2000, 7000, -3.1, -7.1)]:
            if z <= z1:
                return y0 + (y1 - y0) * (z - z0) / (z1 - z0)
        return -7.1
    bed = [(0.8, 0), (3.2, 200), (4.3, 1000), (5.2, 2000), (6.4, 4000), (8.4, 4300), (9.6, 6000), (10.6, 6900),
           (11.4, 6000), (12.6, 4000), (15, 4000)]
    bedpath = ' -- '.join(f"({x},{D(z):.3f})" for x, z in bed)
    out.append(rf"\fill[sea!45] {bedpath} -- (15,0) -- cycle;")
    out.append(rf"\fill[orange!20] (0,0.6) -- {bedpath} -- (15,-7.4) -- (0,-7.4) -- cycle;")
    out.append(rf"\draw[thick,brown!60!black] (0,0.6) -- {bedpath};")
    out.append(r"\draw[sea,thick,decorate,decoration={snake,amplitude=1.2pt,segment length=8pt}] (0.8,0) -- (15,0);")
    out.append(rf"\begin{{scope}}\clip {bedpath} -- (15,0) -- cycle;")
    for z in (200, 1000, 2000, 6000):
        out.append(rf"\draw[ink!70] (0,{D(z):.3f}) -- (15,{D(z):.3f});")
    out.append(r"\end{scope}")
    for z, lab in [(100, 'Epipelagic'), (600, 'Mesopelagic'), (1500, 'Bathypelagic'), (3300, 'Abyssopelagic')]:
        out.append(rf"\node[font=\footnotesize,white] at (9.6,{D(z):.3f}) {{{lab}}};")
    out.append(rf"\draw[ink] (10.6,{D(6350):.3f}) -- (12.4,{D(5200):.3f}) node[right,font=\footnotesize,ink] {{Hadopelagic}};")
    for z in (0, 200, 1000, 2000, 3000, 4000, 5000, 6000, 7000):
        out.append(rf"\draw (15,{D(z):.3f}) -- (15.15,{D(z):.3f}) node[right,font=\scriptsize] {{{z:,} m}};")
    out.append(r"\node[font=\scriptsize,grey,anchor=north east] at (15.9,-7.5) {depth axis not linear};")
    for x, z, a, lab in [(2.0, 100, -5, 'Sublittoral'), (4.75, 1500, -50, 'Bathyal'), (7.4, 4150, -6, 'Abyssal'), (10.6, 6900, 0, 'Hadal')]:
        out.append(rf"\node[rotate={a},font=\scriptsize,brown!50!black,anchor=north] at ({x},{D(z)-0.05:.3f}) {{{lab}}};")
    out.append(r"\node[rotate=-37,font=\scriptsize,brown!50!black,anchor=south] at (0.42,0.32) {Littoral};")
    out.append(r"\draw[{Stealth}-{Stealth}] (0.8,0.55) -- node[above,font=\footnotesize] {Neritic} (3.2,0.55);")
    out.append(r"\draw[{Stealth}-{Stealth}] (3.2,0.55) -- node[above,font=\footnotesize] {Oceanic} (15,0.55);")
    out.append(r"\draw[{Stealth}-{Stealth}] (0.8,1.25) -- node[above,font=\footnotesize] {Pelagic (open water)} (15,1.25);")
    out.append(r"\node[anchor=north west,font=\scriptsize,align=left] at (0.1,-1.6) {\textbf{Pelagic zones}\\Epipelagic\quad 0--200 m\\Mesopelagic\quad 200--1,000 m\\Bathypelagic\quad 1,000--2,000 m\\Abyssopelagic\quad 2,000--6,000 m\\Hadopelagic\quad $>$6,000 m};")
    out.append(r"\node[anchor=north west,font=\scriptsize,align=left] at (0.1,-4.6) {\textbf{Benthic zones}\\Littoral\quad intertidal\\Sublittoral\quad 0--200 m\\Bathyal\quad 200--2,000 m\\Abyssal\quad 2,000--6,000 m\\Hadal\quad $>$6,000 m};")
    out.append(r"\node[anchor=north west,font=\small\bfseries] at (0,-8.4) {(a) Biological zones};")
    # (b) light zones: depth 0..1200 m, y = -z/1200*7
    ox = 18.4
    Y = lambda z: -z / 1200 * 7.0
    out.append(rf"\shade[top color=sea!25,bottom color=sea!60] ({ox},0) rectangle ({ox+5.2},{Y(200)});")
    out.append(rf"\shade[top color=sea!60,bottom color=ink] ({ox},{Y(200)}) rectangle ({ox+5.2},{Y(1000)});")
    out.append(rf"\fill[ink] ({ox},{Y(1000)}) rectangle ({ox+5.2},{Y(1200)});")
    out.append(rf"\draw ({ox},0) rectangle ({ox+5.2},{Y(1200)});")
    for z in (200, 1000):
        out.append(rf"\draw[dashed,white,thick] ({ox},{Y(z)}) -- ({ox+5.2},{Y(z)});")
    for z in range(0, 1201, 200):
        out.append(rf"\draw ({ox},{Y(z)}) -- ({ox-0.12},{Y(z)}) node[left,font=\scriptsize] {{{z:,}}};")
    out.append(rf"\node[rotate=90,font=\footnotesize] at ({ox-1.2},{Y(600)}) {{Water depth (m)}};")
    for z0, z1, lab in [(0, 200, 'Euphotic'), (200, 1000, 'Dysphotic\\\\(twilight)'), (1000, 1200, 'Aphotic')]:
        out.append(rf"\node[white,font=\footnotesize\bfseries,align=center] at ({ox+0.9},{Y((z0+z1)/2)}) {{{lab}}};")
    cols = [('UV-A', 380), ('red', 660), ('yellow', 580), ('green', 530), ('blue', 470)]
    stats = {}
    for i, (lab, w) in enumerate(cols):
        xx = ox + 2.4 + i * 0.62
        out.append(rf"\wavecol{{lc{i}}}{{{max(w,385)}}}")
        z1, z2 = z_frac(w, 1e-2), z_frac(w, 1e-4)
        stats[lab] = (round(z1), round(z2))
        out.append(rf"\draw[lc{i},line width=2.6pt] ({xx},0) -- ({xx},{Y(z1):.3f});")
        out.append(rf"\draw[lc{i},line width=2.6pt,dash pattern=on 2pt off 2pt] ({xx},{Y(z1):.3f}) -- ({xx},{Y(z2):.3f});")
        out.append(rf"\node[rotate=90,anchor=west,font=\footnotesize] at ({xx},0.1) {{{lab}}};")
    out.append(rf"\node[anchor=north west,font=\scriptsize,align=left,text width=6.3cm] at ({ox-1.4},{Y(1200)-0.15}) {{Solid: depth at which each colour falls to 1\% of its surface value; dotted: to 0.01\%. Clear ocean water, illustrative $K_d$ values.}};")
    out.append(rf"\node[anchor=north west,font=\small\bfseries] at ({ox-1.4},-8.4) {{(b) Light zones}};")
    out.append(r"\end{tikzpicture}\end{document}")
    (HERE / 'fig-ocean-zones.tex').write_text('\n'.join(out))
    return stats


# ---------------------------------------------------------------- figure 3
def fig_beer_lambert():
    curves = [(0.04, 'accent'), (0.15, 'sea'), (0.5, 'brown!70!black')]
    def panel(log):
        opts = ("ymode=log,ymin=0.01,ymax=100,ylabel={Light remaining (\\% of surface, logarithmic axis)}" if log
                else "ymin=0,ymax=100,ylabel={Light remaining (\\% of surface, linear axis)}")
        s = [rf"\begin{{axis}}[width=7.6cm,height=6.4cm,{opts},xmin=0,xmax=130,xlabel={{Depth (m)}},"
             r"tick label style={font=\footnotesize},label style={font=\small},title style={font=\small\bfseries},"
             + ("title={(b) Logarithmic light axis}" if log else "title={(a) Linear light axis}") + "]"]
        for k, col in curves:
            zz = min(130, math.log(1e4) / k) if log else 130
            s.append(rf"\addplot[very thick,{col},domain=0:{zz:.2f},samples=120] {{100*exp(-{k}*x)}};")
        s.append(r"\draw[dashed,grey] (axis cs:0,1) -- (axis cs:130,1);")
        s.append(r"\addplot[only marks,mark=*,mark size=1.8pt,ink] coordinates {(115.1,1)(30.7,1)(9.2,1)};")
        if log:
            s += [r"\node[anchor=south west,font=\footnotesize\bfseries,accent] at (axis cs:60,12) {$K_d = 0.04$};",
                  r"\node[anchor=west,font=\footnotesize\bfseries,sea] at (axis cs:42,0.05) {$K_d = 0.15$};",
                  r"\node[anchor=west,font=\footnotesize\bfseries,brown!70!black] at (axis cs:14,0.02) {$K_d = 0.5$};",
                  r"\node[anchor=south,font=\scriptsize,grey] at (axis cs:75,1) {1\% of surface};"]
        else:
            s += [r"\node[anchor=north east,font=\footnotesize,align=left,draw=grey!50,fill=white,inner sep=4pt] at (axis cs:127,95) {{\color{accent}\rule[2pt]{12pt}{1.2pt}} $K_d = 0.04$\\ {\color{sea}\rule[2pt]{12pt}{1.2pt}} $K_d = 0.15$\\ {\color{brown!70!black}\rule[2pt]{12pt}{1.2pt}} $K_d = 0.5$};"]
        s.append(r"\end{axis}")
        return s
    out = [PRE, r"\begin{tikzpicture}"] + panel(False) + [r"\begin{scope}[xshift=8.6cm]"] + panel(True) + [r"\end{scope}", r"\end{tikzpicture}\end{document}"]
    (HERE / 'fig-beer-lambert.tex').write_text('\n'.join(out))

if __name__ == '__main__':
    print('PAR remaining (%):', fig_spectrum_sea())
    print('depth to 1% and 0.01% (m):', fig_ocean_zones())
    fig_beer_lambert()
    print('PAR 1% depth (m):', next(z for z in range(1, 400) if par_fraction(z) < 0.01))
