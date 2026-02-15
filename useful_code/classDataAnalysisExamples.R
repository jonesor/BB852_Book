require(tidyverse)
# Set options:

options(dplyr.summarise.inform = FALSE)



# class data

classData <- read.csv("CourseData/classData.csv")
head(classData)
tail(classData)


ggplot(classData |> filter(Year == 2019), aes(x = HandWidth, y = Height)) +
  geom_point()

ggplot(classData, aes(x = Precision, fill = Gender)) +
  geom_histogram()

# Are males taller than females?

obsDiff <- classData |>
  group_by(Gender) |>
  summarise(meanVal = mean(Height)) |>
  pull(meanVal) |>
  diff()

obsDiff
df1 <- data.frame(shuffledDiffs = rep(NA, 1000))

df1$shuffledDiffs <- replicate(1000, classData |>
  mutate(Gender = sample(Gender)) |>
  group_by(Gender) |>
  summarise(meanVal = mean(Height)) |>
  pull(meanVal) |>
  diff())

table(df1$shuffledDiffs <= obsDiff) # So the p-value is <0.001

# p-value
sum(df1$shuffledDiffs <= obsDiff) / length(df1$shuffledDiffs)

ggplot(df1, aes(shuffledDiffs)) +
  geom_histogram() +
  geom_vline(xintercept = obsDiff) +
  ggtitle("M vs. F, Height")

# Are reaction times different for males vs. females?

obsDiff <- classData |>
  group_by(Gender) |>
  summarise(meanVal = mean(Reaction)) |>
  pull(meanVal) |>
  diff()

obsDiff
df1 <- data.frame(shuffledDiffs = NULL)
df1$shuffledDiffs <- replicate(1000, classData |>
  mutate(Gender = sample(Gender)) |>
  group_by(Gender) |>
  summarise(meanVal = mean(Reaction)) |>
  pull(meanVal) |>
  diff())

table(shuffledDiffs <= obsDiff) # So the p-value is <0.001
ggplot(df1, aes(shuffledDiffs)) +
  geom_histogram() +
  geom_vline(xintercept = obsDiff) +
  ggtitle("M vs. F, Reaction time")



# Are reaction times different for males vs. females?

obsDiff <- classData |>
  group_by(Gender) |>
  summarise(meanVal = mean(Precision)) |>
  pull(meanVal) |>
  diff()

obsDiff
df1 <- data.frame(shuffledDiffs = NULL)
df1$shuffledDiffs <- replicate(1000, classData |>
  mutate(Gender = sample(Gender)) |>
  group_by(Gender) |>
  summarise(meanVal = mean(Precision)) |>
  pull(meanVal) |>
  diff())

table(shuffledDiffs <= obsDiff) # So the p-value is <0.001
ggplot(df1, aes(shuffledDiffs)) +
  geom_histogram() +
  geom_vline(xintercept = obsDiff) +
  ggtitle("M vs. F, Precision")

# Are reaction times different for right vs. left handers?

table(classData$Handedness)

obsDiff <- classData |>
  filter(Handedness != "Both") |>
  group_by(Handedness) |>
  summarise(meanVal = mean(Reaction)) |>
  pull(meanVal) |>
  diff()

obsDiff

df1 <- data.frame(rep = 1:100, shuffledDiffs = NA)
df1$shuffledDiffs <- replicate(100, classData |>
  filter(Handedness != "Both") |>
  mutate(Handedness = sample(Handedness)) |>
  group_by(Handedness) |>
  summarise(meanVal = mean(Reaction)) |>
  pull(meanVal) |>
  diff())

table(shuffledDiffs <= obsDiff) # So the p-value is <0.001
ggplot(df1, aes(shuffledDiffs)) +
  geom_histogram() +
  geom_vline(xintercept = obsDiff) +
  ggtitle("Handedness, Reaction time")

# Some box plots
ggplot(classData, aes(x = Gender, y = Precision)) +
  geom_boxplot() +
  geom_jitter(width = 0.2)

names(classData)

ggplot(classData, aes(x = Gender, y = Coffee)) +
  geom_boxplot() +
  geom_jitter(width = 0.2)

table(classData$CatsOrDogs)

ggplot(classData, aes(x = CatsOrDogs, y = Reaction)) +
  geom_boxplot() +
  geom_jitter(width = 0.2)
