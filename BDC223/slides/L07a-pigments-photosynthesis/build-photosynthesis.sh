#!/bin/sh
set -eu
cd "$(dirname "$0")"
lualatex -interaction=nonstopmode -halt-on-error L07a-photosynthesis-slides.tex > build-photosynthesis.log
lualatex -interaction=nonstopmode -halt-on-error L07a-photosynthesis-slides.tex >> build-photosynthesis.log
OUT=../../images/L07a
mkdir -p "$OUT"
pdftoppm -png -scale-to-x 1920 -scale-to-y -1 L07a-photosynthesis-slides.pdf "$OUT/L07a-photosynthesis-slide"
for e in aux nav out snm toc vrb; do rm -f "L07a-photosynthesis-slides.$e"; done
