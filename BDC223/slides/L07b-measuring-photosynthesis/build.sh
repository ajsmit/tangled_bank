#!/bin/sh
# Build Lecture 7b data, slides and slide PNGs.
set -e
cd "$(dirname "$0")"
uv run -q --with scipy --with numpy python models.py >/dev/null
lualatex -interaction=nonstopmode -halt-on-error L07b-slides.tex >/dev/null
OUT=../../images/L07b
mkdir -p "$OUT"
rm -f "$OUT"/L07b-slide-*.png
pdftoppm -png -scale-to-x 1920 -scale-to-y -1 L07b-slides.pdf "$OUT/L07b-slide"
for e in aux log nav out snm toc vrb; do rm -f L07b-slides.$e; done
