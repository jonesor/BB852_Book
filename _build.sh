#!/bin/sh
set -e

Rscript -e "source('PrepareCourseData.R')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
