#!/usr/bin/env python3
"""Ensure shared browser features reach every published Quarto HTML page.

Pages rendered with ``embed-resources: true`` bypass the project's shared
header. The post-render pass therefore adds Hypothes.is and the site-wide
typography layer where Quarto could not include them itself.
"""

from pathlib import Path
import re
import shutil


PROJECT_ROOT = Path(__file__).resolve().parent.parent
SITE_DIR = PROJECT_ROOT / "_site"
BOOTSTRAP_SOURCE = PROJECT_ROOT / "scripts" / "hypothesis-comments.js"
BOOTSTRAP_TARGET = SITE_DIR / "scripts" / "hypothesis-comments.js"
QUARTO_JS = SITE_DIR / "site_libs" / "quarto-html" / "quarto.js"
BOOTSTRAP_IMPORT = 'import "/scripts/hypothesis-comments.js";'
HYPOTHESIS_EMBED = "https://hypothes.is/embed.js"
QUARTO_JS_REFERENCE = "quarto-html/quarto.js"
QUARTO_CONTENT_MARKER = 'id="quarto-document-content"'
FONT_STYLESHEET_REFERENCE = "styles/strata-fonts.css"
TYPOGRAPHY_STYLESHEET_REFERENCE = "styles/site-typography.css"
FONT_STYLESHEET = (
    '<link rel="stylesheet" href="/styles/strata-fonts.css?v=20260831-2">\n'
)
TYPOGRAPHY_STYLESHEET = (
    '<link rel="stylesheet" href="/styles/site-typography.css?v=20260831-3">\n'
)
STANDALONE_SNIPPET = """\
<script async src="https://hypothes.is/embed.js"></script>
<script>
document.addEventListener("DOMContentLoaded", function () {
  document.body.classList.add("hypothesis-enabled");
});
</script>
"""


def insert_before_head_end(html: str, snippet: str, html_path: Path) -> str:
    head_end = re.search(r"</head\s*>", html, flags=re.IGNORECASE)
    if head_end is None:
        raise RuntimeError(f"No closing head element in {html_path}")
    return html[: head_end.start()] + snippet + html[head_end.start() :]


def ensure_shared_bootstrap() -> None:
    BOOTSTRAP_TARGET.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(BOOTSTRAP_SOURCE, BOOTSTRAP_TARGET)

    quarto_js = QUARTO_JS.read_text(encoding="utf-8")
    if BOOTSTRAP_IMPORT not in quarto_js:
        QUARTO_JS.write_text(
            f"{BOOTSTRAP_IMPORT}\n{quarto_js}", encoding="utf-8"
        )


def ensure_standalone_pages() -> int:
    changed = 0
    for html_path in SITE_DIR.rglob("*.html"):
        html = html_path.read_text(encoding="utf-8")
        if HYPOTHESIS_EMBED in html or QUARTO_JS_REFERENCE in html:
            continue

        updated = insert_before_head_end(html, STANDALONE_SNIPPET, html_path)
        html_path.write_text(updated, encoding="utf-8")
        changed += 1
    return changed


def ensure_typography_pages() -> int:
    changed = 0
    for html_path in SITE_DIR.rglob("*.html"):
        html = html_path.read_text(encoding="utf-8")
        if QUARTO_CONTENT_MARKER not in html:
            continue
        stylesheets = ""
        if FONT_STYLESHEET_REFERENCE not in html:
            stylesheets += FONT_STYLESHEET
        if TYPOGRAPHY_STYLESHEET_REFERENCE not in html:
            stylesheets += TYPOGRAPHY_STYLESHEET
        if not stylesheets:
            continue

        updated = insert_before_head_end(html, stylesheets, html_path)
        html_path.write_text(updated, encoding="utf-8")
        changed += 1
    return changed


if __name__ == "__main__":
    ensure_shared_bootstrap()
    typography_count = ensure_typography_pages()
    hypothesis_count = ensure_standalone_pages()
    print(
        "Shared browser features enabled; "
        f"updated typography on {typography_count} page(s) and "
        f"Hypothes.is on {hypothesis_count} standalone page(s)."
    )
