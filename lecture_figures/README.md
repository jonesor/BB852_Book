# Lecture figures

Lecture-specific figure generators and their outputs live here so that they
remain separate from the Bookdown source and rendered book files.

Build every lecture figure set from the repository root with one command:

```sh
Rscript lecture_figures/generate_all.R
```

Or, from the RStudio console:

```r
source("lecture_figures/generate_all.R")
```

The aggregate build runs each generator in a separate R process, stops on the
first failure, and verifies that every lecture produced full- and half-width
PNG files. It also rejects unexpected PDF output.

Individual generators can still be run when working on one lecture:

```r
source("lecture_figures/data_wrangling_summary/generate_figures.R")
source("lecture_figures/summary_statistics/generate_figures.R")
source("lecture_figures/randomisation_tests/generate_figures.R")
source("lecture_figures/pimp_your_plots/generate_figures.R")
source("lecture_figures/statistics_roadmap/generate_figures.R")
source("lecture_figures/t-tests/generate_figures.R")
source("lecture_figures/anova/generate_figures.R")
source("lecture_figures/linear_regression/generate_figures.R")
source("lecture_figures/ancova/generate_figures.R")
source("lecture_figures/data_management/generate_figures.R")
source("lecture_figures/glm_count/generate_figures.R")
source("lecture_figures/glm_part2/generate_figures.R")
source("lecture_figures/evaluating_models/generate_figures.R")
source("lecture_figures/power_analysis/generate_figures.R")
```

Each lecture directory contains:

- `generate_figures.R`: the reproducible source code.
- `output/full/`: figures prepared for full-width placement.
- `output/half/`: figures prepared for half-width placement.

Each generator removes its existing output directory before rebuilding, so
deleted or renamed figures do not leave stale files. Content figures are
written in both full- and half-width layouts. Purpose-built cover graphics are
the only exception and remain in `output/cover/`. The linear_regression and ancova
generators also write `output/takeaway/` versions of the figures used on
"Title, Bullets & Photo" slides (figure plus takeaway box).

The anova generator reads `CourseData/espresso.csv`. The linear_regression
generator reads `CourseData/morphometry.csv` and also writes the R output shown
on the slides to `output/text/`. The ancova generator (two-way ANOVA, ANCOVA
and multiple regression lecture) reads `CourseData/insectDiet.csv` and
`CourseData/morphometry.csv`, and also writes its slide output to
`output/text/`.

The data_management generator reads `CourseData/heights.csv`; its
spreadsheet examples and nest record card are invented teaching data, not
real records. It also reads `source_images/bto_nest_record_card.jpg`, a completed
BTO Nest Record Scheme card used with permission for educational use
(source: BTO Nest Record Scheme Handbook, 2003; (c) BTO).

The glm_count generator (GLMs with count data lecture) reads
`CourseData/fox.csv`, `CourseData/elephantPoaching.csv` and
`CourseData/roundabouts.csv`, and writes its slide output to `output/text/`.

The glm_part2 generator (lecture 17, binomial GLMs) reads
`CourseData/NFLfieldgoal.csv` (yards converted to metres, as in the book) and
`CourseData/hawksbill.csv`. The mean-variance, logit-link and binomial
confidence-interval figures are analytical (no data). It writes its slide
output to `output/text/`, and also saves three figures at the takeaway size in
`output/takeaway/`.

The evaluating_models generator (lecture 15, evaluating and comparing
models) reads `CourseData/morphometry.csv` (converted from mm to cm as in the
book) and uses simulated data for the residual-pattern and R-squared panels
(seeds stated in the script). It writes its slide output to `output/text/`.

The summary_statistics generator (lecture 07, summary statistics,
distributions and probability) uses simulated data only: a hypothetical moth
light-trap survey with illustrative values (forewing length mean 28 mm, SD
2 mm). It also writes the numbers quoted on the slides to
`output/text/slide_numbers.txt`, and a title-slide graphic to `output/cover/`.

The power_analysis generator (lecture 18, power analysis by simulation) uses
no data files: everything is simulated or calculated, with the course book's
values and seeds (insect diet example, binomial GLM, regression). It writes
the code output shown on the slides to `output/text/`, a title-slide graphic
to `output/cover/`, and three figures at the takeaway size to
`output/takeaway/`.

The data_wrangling_summary generator (lecture 04b, data wrangling summary)
reads `CourseData/AarslevTemperature.csv`. It writes the R output and answer
keys quoted on the slides to `output/text/`, a title-slide graphic to
`output/cover/`, and one figure at the takeaway size to `output/takeaway/`.

All figures are written as PNG files only.
