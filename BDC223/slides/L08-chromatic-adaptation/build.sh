#!/bin/sh
# Build the supplementary Lecture 8 slides and their PNGs.
set -e
cd "$(dirname "$0")"
lualatex -interaction=nonstopmode -halt-on-error L08-slides.tex >/dev/null
OUT=../../images/L08
mkdir -p "$OUT"
rm -f "$OUT"/L08-slide-*.png
pdftoppm -png -scale-to-x 1920 -scale-to-y -1 L08-slides.pdf "$OUT/L08-slide"
for e in aux log nav out snm toc vrb; do rm -f L08-slides.$e; done
