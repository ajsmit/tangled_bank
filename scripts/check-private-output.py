#!/usr/bin/env python3
"""Stop the render if anything from a _private folder reached _site."""

from pathlib import Path
import sys

SITE_DIR = Path(__file__).resolve().parent.parent / "_site"

leaks = [str(p.relative_to(SITE_DIR)) for p in SITE_DIR.rglob("_private")]
leaks += [name for name in ("search.json", "sitemap.xml")
          if (SITE_DIR / name).exists() and "/_private/" in (SITE_DIR / name).read_text(encoding="utf-8")]
if leaks:
    sys.exit("Private material found in _site; do not publish:\n  " + "\n  ".join(sorted(leaks)))
