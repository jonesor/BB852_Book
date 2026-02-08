# Lint R and Rmd files
if (!requireNamespace("lintr", quietly = TRUE)) {
  stop("Package 'lintr' is required. Install with install.packages('lintr').")
}

lintr::lint_dir(pattern = "\\.(R|Rmd)$")
