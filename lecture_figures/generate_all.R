# Build every BB852 lecture figure set from a clean output directory.

command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command_args, value = TRUE)

if (length(file_arg) == 1) {
  runner_path <- sub("^--file=", "", file_arg)
} else {
  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1]
    },
    character(1)
  )
  source_files <- source_files[!is.na(source_files)]

  if (length(source_files) == 0) {
    stop(
      "Run with Rscript lecture_figures/generate_all.R or ",
      "source(\"lecture_figures/generate_all.R\")"
    )
  }

  runner_path <- tail(source_files, 1)
}

runner_path <- normalizePath(runner_path)
lecture_root <- dirname(runner_path)
repository_root <- dirname(lecture_root)
setwd(repository_root)

lecture_names <- c(
  "pimp_your_plots",
  "randomisation_tests",
  "statistics_roadmap",
  "t-tests",
  "anova",
  "linear_regression",
  "ancova"
)

rscript <- file.path(R.home("bin"), "Rscript")

for (lecture_name in lecture_names) {
  generator <- file.path("lecture_figures", lecture_name, "generate_figures.R")
  if (!file.exists(generator)) {
    stop("Missing generator: ", generator)
  }

  message("\nBuilding ", lecture_name, "...")
  status <- system2(rscript, generator)
  if (!identical(status, 0L)) {
    stop("Figure generation failed for ", lecture_name)
  }
}

for (lecture_name in lecture_names) {
  output_root <- file.path("lecture_figures", lecture_name, "output")
  layout_stems <- list()
  for (layout in c("full", "half")) {
    layout_directory <- file.path(output_root, layout)
    png_files <- list.files(layout_directory, pattern = "[.]png$", full.names = TRUE)
    if (!dir.exists(layout_directory) || length(png_files) == 0) {
      stop("No ", layout, " PNG files were generated for ", lecture_name)
    }
    layout_stems[[layout]] <- tools::file_path_sans_ext(basename(png_files))
  }

  if (!setequal(layout_stems$full, layout_stems$half)) {
    stop("Full- and half-width figure names do not match for ", lecture_name)
  }
}

all_output_files <- list.files(
  "lecture_figures",
  recursive = TRUE,
  full.names = TRUE
)
all_output_files <- all_output_files[grepl("/output/", all_output_files)]
allowed_output <- grepl("[.]png$", all_output_files) |
  grepl("/output/text/[^/]+[.]txt$", all_output_files)
unexpected_output <- all_output_files[!allowed_output]
if (length(unexpected_output) > 0) {
  stop("Unexpected output file: ", paste(unexpected_output, collapse = ", "))
}

message("\nBuilt all lecture figures successfully. Outputs are PNG only.")
