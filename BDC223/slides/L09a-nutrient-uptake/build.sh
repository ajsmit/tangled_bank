#!/bin/sh
# Build the Lecture 9a graphs, slides and slide PNGs (images/L09a/L09a-slide-NN.png, one per frame in order).
set -e
cd "$(dirname "$0")"
(cd figures && uv run -q --with matplotlib --with numpy python make_figures.py)
lualatex -interaction=nonstopmode -halt-on-error L09a-slides.tex >/dev/null
lualatex -interaction=nonstopmode -halt-on-error L09a-slides.tex >/dev/null
OUT=../../images/L09a
mkdir -p "$OUT"
rm -f "$OUT"/L09a-slide-*.png
pdftoppm -png -scale-to-x 1920 -scale-to-y -1 L09a-slides.pdf "$OUT/L09a-slide"
for e in aux log nav out snm toc vrb; do rm -f L09a-slides.$e; done
