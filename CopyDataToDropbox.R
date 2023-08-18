#Copy course data to the dropbox

source_dir <- "CourseData"
destination_dir <- "/Users/jones/Dropbox/_SDU_Teaching/BB852/BB852_CourseData"


# Delete all files in the destination directory
suppressMessages(file.remove(list.files(destination_dir, full.names = TRUE)))

# Get the list of files in the source directory
file_list <- list.files(source_dir, full.names = TRUE)

# Copy each file to the destination directory
for (file in file_list) {
  suppressMessages(file.copy(from = file, to = destination_dir, overwrite = TRUE))
}