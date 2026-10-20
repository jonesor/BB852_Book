# Lecture figures

Lecture-specific figure generators and their outputs live here so that they
remain separate from the Bookdown source and rendered book files.

Run each generator from the repository root:

```r
source("lecture_figures/randomisation_tests/generate_figures.R")
source("lecture_figures/pimp_your_plots/generate_figures.R")
source("lecture_figures/statistics_roadmap/generate_figures.R")
source("lecture_figures/t-tests/generate_figures.R")
```

Each lecture directory contains:

- `generate_figures.R`: the reproducible source code.
- `output/full/`: figures prepared for full-width placement.
- `output/half/`: figures prepared for half-width placement.

The t-tests generator writes each figure once, at the size of its slide
placeholder, into `output/full/`, `output/half/`, `output/takeaway/` or
`output/cover/`.

All generators write PNG files only.
