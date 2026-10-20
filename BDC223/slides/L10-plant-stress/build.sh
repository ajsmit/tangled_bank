#!/bin/sh
# Build the Lecture 10 slides and their PNGs.
set -e
cd "$(dirname "$0")"
lualatex -interaction=nonstopmode -halt-on-error L10-slides.tex >/dev/null
lualatex -interaction=nonstopmode -halt-on-error L10-slides.tex >/dev/null
OUT=../../images/L10
mkdir -p "$OUT"
rm -f "$OUT"/L10-slide-*.png
pdftoppm -png -scale-to-x 1920 -scale-to-y -1 L10-slides.pdf "$OUT/L10-slide"
for e in aux log nav out snm toc vrb; do rm -f L10-slides.$e; done
