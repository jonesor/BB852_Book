#!/bin/sh
set -e

Rscript -e "source('scripts/PrepareCourseData.R')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
