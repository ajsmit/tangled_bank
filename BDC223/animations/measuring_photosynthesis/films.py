"""Manim films for BDC223 Lecture 7b (Measuring Photosynthesis).

Film 1, OxygenToPI: a stepped-light oxygen incubation becomes rates and a P-I curve.
Film 2, SaturatingPulse: a PAM saturation-pulse trace becomes yields, ETR and a rapid light curve.

All numbers are read from ../../slides/L07b-measuring-photosynthesis/values.json and data/
(illustrative simulations from models.py). Render:
  uv run --python 3.12 --with manim manim -qh films.py OxygenToPI SaturatingPulse
"""
import json
import math
from pathlib import Path
from manim import *

SRC = Path(__file__).resolve().parents[2] / 'slides' / 'L07b-measuring-photosynthesis'
V = json.loads((SRC / 'values.json').read_text())
INK = '#1F2A37'; ACCENT = '#0F766E'; SEA = '#1D4E89'; WARM = '#B45309'; LEAF = '#3F8F2F'; RED = '#A61B1B'; GREY = '#6B7280'
config.background_color = '#FFFFFF'
Text.set_default(color=INK, font='Helvetica Neue')
MathTex.set_default(color=INK)
Tex.set_default(color=INK)


def table(name):
    rows = (SRC / 'data' / name).read_text().split('\n')[1:]
    return [[float(x) for x in r.split()] for r in rows if r.strip()]


def caption(scene, text, wait=0.0, old=None, size=30):
    t = Text(text, font_size=size)
    if t.width > 13.2:
        t.scale_to_fit_width(13.2)
    t.to_edge(DOWN, buff=0.35)
    if old is not None:
        scene.play(FadeOut(old, run_time=0.3))
    scene.play(FadeIn(t, run_time=0.4))
    if wait:
        scene.wait(wait)
    return t


def title_card(scene, title, sub):
    t = Text(title, font_size=52, weight=BOLD); s = Text(sub, font_size=28, color=GREY).next_to(t, DOWN)
    note = Text('Illustrative data simulated from stated models', font_size=22, color=GREY).to_edge(DOWN)
    scene.play(FadeIn(t), FadeIn(s), FadeIn(note)); scene.wait(2.2)
    scene.play(FadeOut(t), FadeOut(s), FadeOut(note))


class OxygenToPI(Scene):
    def construct(self):
        o2 = V['o2']; st = o2['settings']; steps = o2['steps']; fit = o2['fit']
        trace = table('o2_trace.dat')
        title_card(self, 'From an oxygen trace to a P–I curve', 'BDC223 Lecture 7b: Measuring Photosynthesis')

        # ---- chamber (left)
        lamp = Rectangle(width=2.6, height=0.35, fill_color=GREY, fill_opacity=0.5, stroke_color=INK).move_to([-5.0, 2.6, 0])
        glow = Rectangle(width=2.6, height=1.0, fill_color=YELLOW, fill_opacity=0.0, stroke_width=0).next_to(lamp, DOWN, buff=0.05)
        chamber = Rectangle(width=2.6, height=2.6, stroke_color=INK, fill_color='#E6F0FA', fill_opacity=1).move_to([-5.0, 0.2, 0])
        weed = VGroup(*[CubicBezier([-5.6 + d, -0.8, 0], [-5.3 + d, 0.0, 0], [-5.8 + d, 0.6, 0], [-5.4 + d, 1.2, 0], color=LEAF, stroke_width=8) for d in (0, 0.5)])
        sensor = Rectangle(width=0.18, height=2.0, fill_color=WARM, fill_opacity=0.7, stroke_color=INK).move_to([-4.0, 0.5, 0])
        lbl = VGroup(Text('O₂ sensor', font_size=20).next_to(sensor, RIGHT, buff=0.1).shift(UP * 0.6),
                     Text(f"{st['mass_gFW']:.1f} g seaweed in {st['volume_L']:.2f} L", font_size=20).next_to(chamber, DOWN, buff=0.15))
        par_val = ValueTracker(0)
        par_txt = always_redraw(lambda: Text(f'PAR {int(round(par_val.get_value()))} µmol m⁻² s⁻¹', font_size=22).next_to(lamp, UP, buff=0.1))
        self.play(FadeIn(lamp), FadeIn(glow), FadeIn(chamber), Create(weed), FadeIn(sensor), FadeIn(lbl), FadeIn(par_txt))

        # ---- O2 trace axes (right)
        ax = Axes(x_range=[0, 40, 5], y_range=[230, 360, 20], x_length=7.2, y_length=4.6, tips=False,
                  axis_config=dict(color=INK, include_numbers=True, font_size=22, decimal_number_config=dict(color=INK, num_decimal_places=0))).move_to([2.6, 0.3, 0])
        xl = Text('Time (min)', font_size=22).next_to(ax.x_axis, DOWN, buff=0.45)
        yl = Text('O₂ (µmol L⁻¹)', font_size=22).rotate(PI / 2).next_to(ax.y_axis, LEFT, buff=0.55)
        self.play(Create(ax), FadeIn(xl), FadeIn(yl))
        cap = caption(self, 'Light is raised in 5-minute steps while the sensor logs dissolved O₂.')

        # draw trace step by step, brightening the lamp
        for k, s in enumerate(steps):
            pts = [ax.c2p(t, c) for t, c in trace if s['t0'] <= t <= s['t1'] + 1e-9]
            seg = VMobject(color=SEA, stroke_width=3).set_points_as_corners(pts)
            self.play(par_val.animate.set_value(s['I']), glow.animate.set_fill(opacity=0.08 + 0.6 * s['I'] / 800),
                      Create(seg), run_time=1.1, rate_func=linear)
        cap = caption(self, 'In darkness O₂ falls (respiration); in light it rises (net photosynthesis).', 2.5, cap)

        # slopes for two example steps
        lines = VGroup()
        for s in steps:
            x0, x1 = s['t0'] + 1, s['t1']
            lines.add(Line(ax.c2p(x0, s['slope'] * x0 + s['intercept']), ax.c2p(x1, s['slope'] * x1 + s['intercept']), color=WARM, stroke_width=5))
        cap = caption(self, 'Discard the first minute of each step; fit a straight line to the rest.', 0, cap)
        self.play(LaggedStart(*[Create(l) for l in lines], lag_ratio=0.15), run_time=3)
        ex = steps[4]
        eq = MathTex(r'P = \text{slope}\times 60 \times \frac{V}{m}', font_size=36).move_to([2.6, 3.25, 0])
        num = MathTex(rf'= {ex["slope"]:.3f}\times 60\times\frac{{{st["volume_L"]:.2f}}}{{{st["mass_gFW"]:.1f}}} = {ex["rate"]:.1f}\ \mu\text{{mol O}}_2\,\text{{g}}^{{-1}}\,\text{{h}}^{{-1}}',
                      font_size=30).next_to(eq, DOWN, buff=0.15).set_x(2.6)
        self.play(Indicate(lines[4], color=RED), Write(eq))
        cap = caption(self, f"At {ex['I']} µmol m⁻² s⁻¹ the slope is {ex['slope']:.2f} µM min⁻¹.", 0, cap)
        self.play(Write(num)); self.wait(3)

        # ---- to the P-I curve
        self.play(*[FadeOut(m) for m in (lamp, glow, chamber, weed, sensor, lbl, par_txt, eq, num)], run_time=0.6)
        trace_group = VGroup(ax, xl, yl, lines, *[m for m in self.mobjects if isinstance(m, VMobject) and m.get_color() == ManimColor(SEA)])
        self.play(trace_group.animate.scale(0.42).to_corner(UL, buff=0.3), run_time=1.2)
        pax = Axes(x_range=[0, 850, 200], y_range=[-8, 32, 8], x_length=6.6, y_length=4.6, tips=False,
                   axis_config=dict(color=INK, include_numbers=True, font_size=22, decimal_number_config=dict(color=INK, num_decimal_places=0))).move_to([1.4, -0.2, 0])
        pxl = Text('PAR (µmol photons m⁻² s⁻¹)', font_size=22).next_to(pax.x_axis, DOWN, buff=0.45)
        pyl = Text('Rate (µmol O₂ g⁻¹ h⁻¹)', font_size=22).rotate(PI / 2).next_to(pax.y_axis, LEFT, buff=0.55)
        zero = DashedLine(pax.c2p(0, 0), pax.c2p(850, 0), color=GREY, stroke_width=2)
        self.play(Create(pax), FadeIn(pxl), FadeIn(pyl), Create(zero))
        cap = caption(self, 'Each slope becomes one rate, plotted against its irradiance.', 0, cap)
        dots = VGroup()
        for k, s in enumerate(steps):
            d = Dot(pax.c2p(s['I'], s['rate']), color=SEA, radius=0.08)
            self.play(TransformFromCopy(lines[k], d), run_time=0.45); dots.add(d)
        Pm, a, R = fit['Pmax'], fit['alpha'], fit['R']
        curve = pax.plot(lambda x: Pm * math.tanh(a * x / Pm) - R, x_range=[0, 840], color=ACCENT, stroke_width=5)
        cap = caption(self, 'Fit P = Pmax·tanh(αI/Pmax) − R by least squares.', 0, cap)
        self.play(Create(curve), run_time=2)
        slope_line = pax.plot(lambda x: a * x - R, x_range=[0, 150], color=WARM, stroke_width=3)
        ikline = DashedLine(pax.c2p(fit['Ik'], -8), pax.c2p(fit['Ik'], 32), color=GREY)
        icdot = Dot(pax.c2p(fit['Ic'], 0), color=RED, radius=0.09)
        params = VGroup(
            MathTex(rf'P_{{max}} = {Pm:.1f}', font_size=30), MathTex(rf'\alpha = {a:.3f}', font_size=30),
            MathTex(rf'R = {R:.1f}', font_size=30), MathTex(rf'I_k = {fit["Ik"]:.0f}', font_size=30),
            MathTex(rf'I_c = {fit["Ic"]:.0f}', font_size=30)).arrange(DOWN, aligned_edge=LEFT, buff=0.18).next_to(pax, RIGHT, buff=0.3).shift(DOWN * 0.3)
        self.play(Create(slope_line), FadeIn(params[1])); self.wait(0.8)
        self.play(FadeIn(params[0]), FadeIn(params[2])); self.wait(0.8)
        self.play(Create(ikline), FadeIn(params[3])); self.wait(0.8)
        self.play(FadeIn(icdot, scale=1.6), FadeIn(params[4]))
        cap = caption(self, 'α: low-light efficiency · Pmax: capacity · Ik: onset of saturation · Ic: compensation point', 5, cap, size=26)
        self.play(*[FadeOut(m) for m in self.mobjects])


class SaturatingPulse(Scene):
    def construct(self):
        pam = V['pam']; ex = pam['example']; fit = pam['fit']; rows = pam['rows']; s = pam['settings']
        title_card(self, 'The saturating pulse', 'From fluorescence to electron transport and a rapid light curve')

        ax = Axes(x_range=[0, 120, 20], y_range=[0, 1700, 500], x_length=9, y_length=4.4, tips=False,
                  axis_config=dict(color=INK, include_numbers=True, font_size=22, decimal_number_config=dict(color=INK, num_decimal_places=0))).move_to([0, 0.4, 0])
        xl = Text('Time (s)', font_size=22).next_to(ax.x_axis, DOWN, buff=0.45)
        yl = Text('Fluorescence (relative)', font_size=22).rotate(PI / 2).next_to(ax.y_axis, LEFT, buff=0.55)
        self.play(Create(ax), FadeIn(xl), FadeIn(yl))
        tr = table('pam_trace.dat')

        def seg(t0, t1):
            pts = [ax.c2p(t, f) for t, f in tr if t0 <= t <= t1]
            return VMobject(color=RED, stroke_width=4).set_points_as_corners(pts)

        cap = caption(self, 'Dark-adapted leaf, weak measuring light only: minimum fluorescence F₀.')
        self.play(Create(seg(0, 19.9)), run_time=2, rate_func=linear)
        f0 = MathTex('F_0 = 300', font_size=30).next_to(ax.c2p(10, 300), DOWN)
        self.play(FadeIn(f0))
        cap = caption(self, 'A strong saturating pulse closes all PSII reaction centres: maximum fluorescence Fm.', 0, cap)
        flash = Rectangle(width=14, height=8, fill_color=YELLOW, fill_opacity=0.25, stroke_width=0)
        self.play(FadeIn(flash, run_time=0.15)); self.play(Create(seg(20, 30)), FadeOut(flash), run_time=1.2)
        fm = MathTex('F_m = 1500', font_size=30).next_to(ax.c2p(20.4, 1500), RIGHT)
        self.play(FadeIn(fm))
        fvfm = MathTex(r'F_v/F_m = \frac{F_m - F_0}{F_m} = \frac{1500-300}{1500} = ' + f'{pam["Fv_Fm"]:.2f}', font_size=34).to_corner(UR, buff=0.5)
        self.play(Write(fvfm)); cap = caption(self, 'Fv/Fm: the maximum quantum yield of PSII (about 0.8 in healthy leaves).', 3, cap)

        shade = Rectangle(width=ax.c2p(120, 0)[0] - ax.c2p(30, 0)[0], height=ax.c2p(0, 1700)[1] - ax.c2p(0, 0)[1],
                          fill_color=YELLOW, fill_opacity=0.15, stroke_width=0).move_to(ax.c2p(75, 850))
        cap = caption(self, 'Actinic light on: fluorescence rises, then falls to a steady level F as photochemistry starts.', 0, cap)
        self.play(FadeIn(shade), Create(seg(30, 59.9)), run_time=2.5, rate_func=linear)
        self.play(Create(seg(60, 120)), run_time=2.5, rate_func=linear)
        fmp = MathTex(r"F_m' = 950", font_size=30).next_to(ax.c2p(60.4, 950), UP)
        ff = MathTex('F = 600', font_size=30).next_to(ax.c2p(110, 600), UP)
        self.play(FadeIn(fmp), FadeIn(ff))
        phi = MathTex(r"\Phi_{PSII} = \frac{F_m' - F}{F_m'} = \frac{950-600}{950} = " + f"{ex['Y']:.2f}", font_size=34).next_to(fvfm, DOWN, aligned_edge=RIGHT)
        self.play(Write(phi))
        cap = caption(self, 'ΦPSII: the fraction of absorbed light used in photochemistry at this irradiance.', 3, cap)
        etr = MathTex(r'\text{ETR} = \Phi_{PSII}\times \text{PAR}\times 0.5\times 0.84 = ' + f"{ex['Y']:.2f}" + r'\times 400\times 0.5\times 0.84 = ' + f"{ex['ETR']:.0f}",
                      font_size=30).to_edge(UP, buff=0.25)
        self.play(FadeOut(fvfm), phi.animate.next_to(etr, DOWN), Write(etr))
        cap = caption(self, 'ETR in µmol electrons m⁻² s⁻¹: half the photons go to PSII; the leaf absorbs about 84%.', 4, cap)
        self.play(*[FadeOut(m) for m in self.mobjects])

        # ---- rapid light curve
        rax = Axes(x_range=[0, 1250, 200], y_range=[0, 125, 25], x_length=8, y_length=4.6, tips=False,
                   axis_config=dict(color=INK, include_numbers=True, font_size=22, decimal_number_config=dict(color=INK, num_decimal_places=0))).move_to([-0.6, 0.2, 0])
        rxl = Text('PAR (µmol photons m⁻² s⁻¹)', font_size=22).next_to(rax.x_axis, DOWN, buff=0.45)
        ryl = Text('ETR (µmol e⁻ m⁻² s⁻¹)', font_size=22).rotate(PI / 2).next_to(rax.y_axis, LEFT, buff=0.55)
        self.play(Create(rax), FadeIn(rxl), FadeIn(ryl))
        cap = caption(self, 'A rapid light curve: 10-s steps of increasing light, with a saturating pulse at the end of each.')
        yt = always_redraw(lambda: VMobject())
        for r in rows[1:]:
            d = Triangle(color=RED, fill_opacity=1).scale(0.09).move_to(rax.c2p(r['I'], r['ETR']))
            lab = Text(f"Y = {r['Y']:.2f}", font_size=20, color=GREY).next_to(d, UP, buff=0.08)
            self.play(FadeIn(d, scale=1.5), FadeIn(lab), run_time=0.35); self.play(FadeOut(lab), run_time=0.15)
        curve = rax.plot(lambda x: fit['ETRmax'] * math.tanh(fit['alpha'] * x / fit['ETRmax']), x_range=[0, 1230], color=ACCENT, stroke_width=5)
        cap = caption(self, 'Fit the same tanh model used for P–I curves.', 0, cap)
        self.play(Create(curve), run_time=2)
        params = VGroup(MathTex(rf'\text{{ETR}}_{{max}} = {fit["ETRmax"]:.0f}', font_size=30), MathTex(rf'\alpha = {fit["alpha"]:.2f}', font_size=30),
                        MathTex(rf'E_k = {fit["Ek"]:.0f}', font_size=30)).arrange(DOWN, aligned_edge=LEFT).to_corner(UR, buff=0.6).shift(DOWN * 1.2)
        self.play(FadeIn(params))
        cap = caption(self, 'Electrons are not carbon: calibrate against O₂ or ¹⁴C when absolute rates are needed.', 5, cap)
        self.play(*[FadeOut(m) for m in self.mobjects])
