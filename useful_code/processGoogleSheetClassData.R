# Prepare class data
require(lubridate)
require(magrittr)
require(dplyr)
require(googledrive)
require(readxl)

# Download from Google Drive
file <- googledrive::drive_find(pattern = "Class", type = "spreadsheet")
file
googledrive::drive_download(
  file = googledrive::as_id(file$id),
  path = paste("tempData/", file$name, ".xlsx", sep = ""),
  type = "xlsx", overwrite = TRUE
)

# Import from xlsx
classData <- readxl::read_xlsx("tempData/BB852 Class Data (Responses).xlsx")

# Rename columns
names(classData) <- c(
  "Timestamp", "Gender", "Height", "HandWidth",
  "CountriesVisited", "Handedness", "Coffee", "Reaction",
  "Precision", "CatsOrDogs", "Entertainment"
)


# Process to add year column
classData <- classData %>%
  mutate(Year = year(Timestamp)) %>%
  select(Year, everything(), -Timestamp)

classData

summary(classData)
# Write to course data folder
write.csv(
  x = classData,
  file = "/Users/jones/Dropbox/_SDU_Teaching/BB852/DataSetLibrary/classData.csv",
  row.names = FALSE
)


ggplot(classData, aes(x = HandWidth, y = Height, colour = Gender)) +
  geom_point()
