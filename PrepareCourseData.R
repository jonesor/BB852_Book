# Prepare CourseData from DataSetLibrary and validate referenced datasets
# This script is intended to be run before rendering the book.

# Collect CSV references from all Rmd files
rmd_files <- list.files(pattern = "\\.Rmd$")

course_data_files <- NULL
for (i in seq_along(rmd_files)) {
  x1 <- readLines(rmd_files[i])
  csv_lines <- grep("[_A-Za-z0-9]+\\.csv", x1)
  course_data_files <- append(course_data_files, x1[csv_lines])
}

course_data_files <- unique(course_data_files)

library(stringr)
library(dplyr)
course_data_files <- unlist(str_extract_all(course_data_files,
                                            "([A-Za-z0-9_]+\\.csv)")) %>%
  sort() %>%
  unique()

course_data_files <- data.frame(file_name = c(course_data_files, "birds.csv")) %>%
  dplyr::filter(!file_name %in% c("write.csv", "read.csv", "myData.csv",
                                 "course_data_files.csv", "datasets_missing.csv"))

write.csv(x = course_data_files, file = "course_data_files.csv", row.names = FALSE)

# Delete all files in CourseData
unlink("CourseData/*", recursive = FALSE)

# Copy potential files over from the DataSetLibrary into CourseData.
# Later the files in CourseData should be put in a Dropbox folder for
# easy access by the students
source_dir <- "DataSetLibrary"
dest_dir <- "CourseData"

# Get a list of all available files in DataSetLibrary
# and filter the list to only those that are USED in the Rmd files
library_files <- list.files(path = "DataSetLibrary/", full.names = TRUE, recursive = TRUE)
course_files <- library_files[basename(library_files) %in% course_data_files$file_name]

# Copy only the files referenced in the Rmd files
file.copy(from = course_files, file.path(dest_dir, basename(course_files)))

# Missing files: mentioned in materials but not available
current_files <- list.files("CourseData/")

missingFiles <- course_data_files$file_name[!course_data_files$file_name %in% current_files]

datasets_missing <- data.frame(name = missingFiles) %>%
  dplyr::filter(!name %in% c("SDUweather.csv", "example.csv", "myData.csv"))

# Write out the missing datasets to a CSV
write.csv(x = datasets_missing, file = "datasets_missing.csv", row.names = FALSE)

if (nrow(datasets_missing) > 0) {
  stop("One or more datasets mentioned in the text are missing. \n       Check datasets_missing.csv")
}

# Optional: sync to Dropbox
source("CopyDataToDropbox.R")
