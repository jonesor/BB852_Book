# Style all R and Rmd files
if (!requireNamespace("styler", quietly = TRUE)) {
  stop("Package 'styler' is required. Install with install.packages('styler').")
}

styler::style_dir(filetype = c("R", "Rmd"))
