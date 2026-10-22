#!/bin/sh
# Build the six replacement pigment slides and their lecture-page images.
set -eu
cd "$(dirname "$0")"
lualatex -interaction=nonstopmode -halt-on-error L07a-pigments-slides.tex > build.log
lualatex -interaction=nonstopmode -halt-on-error L07a-pigments-slides.tex >> build.log
OUT=../../images/L07a
mkdir -p "$OUT"
pdftoppm -png -scale-to-x 1920 -scale-to-y -1 L07a-pigments-slides.pdf "$OUT/L07a-pigments-slide"
for e in aux nav out snm toc vrb; do rm -f "L07a-pigments-slides.$e"; done
