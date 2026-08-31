# STRATA theme

The site-wide Quarto foundation for The Tangled Bank. It replaces the
Bootswatch base entirely and is tuned for long-form reading and mathematics on
screen. The separate Tangled Bank Reader files are loaded after STRATA in
`_quarto.yml`; see `README-reader.md` for that focused interaction and teaching
layer.

## Files

| File | Purpose |
|---|---|
| `strata.scss` | Tokens, Bootstrap variable overrides, and all component rules (typography, navbar, sidebar, TOC, code, maths, figures, tables, callouts, references, page navigation, cookie banner, print). |
| `strata-landing.scss` | Home-page-only rules for the `.tb-*` hero, course cards, epigraph, and banner. |
| `strata-fonts.css` | `@font-face` declarations for the self-hosted variable fonts in `/fonts`. Loaded from `includes/head-custom.html`. |
| `site-typography.css` | Post-theme primary-branch font and rhythm rules shared by STRATA and pages that select a legacy or specialised theme. |
| `../fonts/*.woff2` | Latin and Latin-Extended subsets of each face. Listed under `project.resources` so Quarto copies them into `_site`. |

Older theme files in this directory (`tangled-bank*.scss`, `infrared.scss`,
`fonts.scss`, `landing.scss`, `styles.scss`, `base.css`,
`typography-current.css`, `biostats-callouts.css`, `bcb743-callouts.css`) are
no longer loaded anywhere.

## Type

| Role | Face | Notes |
|---|---|---|
| Body | Native system / Inter | The primary branch's 15px sans-serif body at 1.45 line height. |
| Headings | Roboto Slab | Variable slab serif, matching the primary branch. |
| UI | Native system / Inter | Navbar, sidebar, TOC, captions, tables, and labels. |
| Code | JetBrains Mono | Variable, self-hosted. |
| Editorial | STIX Two Text | Landing-page ledes and epigraphs. |
| Maths | STIX Two Math | Bundled with MathJax 4 (`mathjax-stix2`), configured in `includes/head-custom.html`. |

## Colour

Pure-white page ground (`#ffffff`) with a subtly distinct surface for code,
callouts, and cards; near-black ink; kelp green (`#0f6b62`) for links and
primary actions; coral (`#c9573f`) for section numbers, active markers, and
"important" callouts; ochre for warnings. All tokens are exposed as CSS custom
properties (`--st-*`) so JavaScript-injected markup and extensions can reuse
them. A paired dark palette is defined in `strata-dark.scss`; it changes only
colour and surface tokens, leaving the primary-branch typography unchanged.

## Regenerating the fonts

The woff2 files were fetched from the Google Fonts CSS API with a Chrome user
agent (so it serves variable woff2), keeping only the `latin` and `latin-ext`
subsets. To refresh:

```python
import re, urllib.request, os
UA = {"User-Agent": "Mozilla/5.0 (Macintosh) AppleWebKit/537.36 Chrome/124.0 Safari/537.36"}
fams = ["Inter:wght@400..700", "Roboto+Slab:wght@400..700",
        "JetBrains+Mono:wght@400..500",
        "STIX+Two+Text:ital,wght@0,400..700;1,400..700"]
out = []
for f in fams:
    css = urllib.request.urlopen(urllib.request.Request(
        f"https://fonts.googleapis.com/css2?family={f}&display=swap", headers=UA)).read().decode()
    for subset, block in re.findall(r"/\* (\S+) \*/\n(@font-face \{[^}]+\})", css):
        if subset not in ("latin", "latin-ext"): continue
        fam = re.search(r"font-family: '([^']+)'", block).group(1)
        style = re.search(r"font-style: (\w+)", block).group(1)
        url = re.search(r"url\((https://[^)]+)\)", block).group(1)
        name = f"{fam.replace(' ', '')}-{style}-{subset}.woff2"
        open(f"fonts/{name}", "wb").write(urllib.request.urlopen(urllib.request.Request(url, headers=UA)).read())
        out.append(f"/* {fam} {style} {subset} */\n" + block.replace(url, f"../fonts/{name}") + "\n")
open("styles/strata-fonts.css", "w").write("\n".join(out))
```

Run it from the repository root.

## Sass gotcha

Font stacks in the `:root { --st-font-* }` block are written literally rather
than interpolated from Sass variables. This preserves quotes around family
names containing spaces and keeps the custom properties valid in browsers.
