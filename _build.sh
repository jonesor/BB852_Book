#!/bin/sh
set -e

BUILD_DATE="${BUILD_DATE:-$(TZ=Europe/Copenhagen date +%F)}"
PDF_FILE="BB852_book_${BUILD_DATE}.pdf"
PDF_FILES_DIR="${PDF_FILE%.pdf}_files"
export PDF_FILE
trap 'rm -rf "$PDF_FILES_DIR"' EXIT

Rscript -e "source('scripts/PrepareCourseData.R')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book', output_file = Sys.getenv('PDF_FILE'))"
Rscript -e "files <- list.files('docs', pattern = '[.]html$', full.names = TRUE); stopifnot(length(files) > 0); for (file in files) { html <- readLines(file, warn = FALSE); html <- gsub('BB852_book_BUILD_DATE.pdf', Sys.getenv('PDF_FILE'), html, fixed = TRUE); writeLines(html, file, useBytes = TRUE) }"

test -s "docs/$PDF_FILE"
