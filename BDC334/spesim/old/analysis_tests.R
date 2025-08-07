library(tidyverse)

# Load the dataset
demo_spp <- read.csv("BDC334/spesim/demo_output_20250806_212011_abundances.csv") |>
  select(-site)

# Calculate a species association matrix
demo_spp |>
  cor()
