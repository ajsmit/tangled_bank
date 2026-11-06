#!/usr/bin/env python3
"""Keep assessment pages out of the llms.txt output.

Quarto writes llms.txt and a .llms.md copy of every page (website: llms-txt: true)
and can exclude only drafts. This removes every page under an assessments/ folder
from the llms.txt index and deletes its .llms.md copy.
"""

import os
import re
from pathlib import Path

SITE_DIR = Path(os.environ.get("QUARTO_PROJECT_OUTPUT_DIR",
                               Path(__file__).resolve().parent.parent / "_site"))
EXCLUDE = re.compile(r"(^|/)assessments/")


def main() -> None:
    index = SITE_DIR / "llms.txt"
    if index.exists():
        lines = index.read_text(encoding="utf-8").splitlines(keepends=True)
        link = re.compile(r"\]\(([^)]+)\)")
        kept = [l for l in lines if not ((m := link.search(l)) and EXCLUDE.search(m.group(1)))]
        if len(kept) != len(lines):
            index.write_text("".join(kept), encoding="utf-8")
    for md in SITE_DIR.rglob("*.llms.md"):
        if EXCLUDE.search(md.relative_to(SITE_DIR).as_posix()):
            md.unlink()
    remaining = [l for l in (index.read_text(encoding="utf-8").splitlines() if index.exists() else [])
                 if EXCLUDE.search(l)]
    assert not remaining, f"assessment pages still listed in llms.txt: {remaining[:3]}"


if __name__ == "__main__":
    main()
