#!/bin/sh
set -e

BUILD_DATE="${BUILD_DATE:-$(TZ=Europe/Copenhagen date +%F)}"
PDF_FILE="BB852_book_${BUILD_DATE}.pdf"
export PDF_FILE

Rscript -e "source('scripts/PrepareCourseData.R')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
mv docs/BB852.pdf "docs/$PDF_FILE"
Rscript -e "files <- list.files('docs', pattern = '[.]html$', full.names = TRUE); stopifnot(length(files) > 0); for (file in files) { html <- readLines(file, warn = FALSE); html <- gsub('BB852_book_BUILD_DATE.pdf', Sys.getenv('PDF_FILE'), html, fixed = TRUE); writeLines(html, file, useBytes = TRUE) }"

test -s "docs/$PDF_FILE"
