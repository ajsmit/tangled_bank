#!/usr/bin/env python3
"""Ensure every published HTML page loads the Hypothes.is client."""

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
STANDALONE_SNIPPET = """\
<script async src="https://hypothes.is/embed.js"></script>
<script>
document.addEventListener("DOMContentLoaded", function () {
  document.body.classList.add("hypothesis-enabled");
});
</script>
"""


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

        head_end = re.search(r"</head\s*>", html, flags=re.IGNORECASE)
        if head_end is None:
            raise RuntimeError(f"No closing head element in {html_path}")

        updated = html[: head_end.start()] + STANDALONE_SNIPPET + html[head_end.start() :]
        html_path.write_text(updated, encoding="utf-8")
        changed += 1
    return changed


if __name__ == "__main__":
    ensure_shared_bootstrap()
    changed_count = ensure_standalone_pages()
    print(f"Hypothes.is enabled; updated {changed_count} standalone HTML page(s).")
