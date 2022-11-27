# Script to simulate data used in the book.

# Heights ----
set.seed(1234)
temp <- data.frame(
  place = rep(c("London", "Bristol"), each = 15),
  heightMale = round(rnorm(30, 175, 4)), heightFemale = round(rnorm(30, 160, 4))
)
write.csv(temp, file = "CourseData/heights.csv", row.names = FALSE)
rm(temp)

# Fish personality ----
set.seed(123)
meanSEM <- expand.grid(personality = c("Proactive", "Reactive"), colouration = c("Homogeneous", "Heterogeneous")) %>%
  mutate(spawningMean = c(270, 260, 240, 170), spawningSEM = rep(40, 4))

sampleSize <- 8
fishPersonality <- expand.grid(ID = 1:sampleSize, personality = c("Proactive", "Reactive"), colouration = c("Homogeneous", "Heterogeneous")) %>%
  left_join(meanSEM) %>%
  mutate(spawning = round(rnorm(sampleSize * 4, spawningMean, spawningSEM))) %>%
  select(personality, colouration, spawning)

write.csv(fishPersonality, "CourseData/fishPersonality.csv", row.names = FALSE)
rm(fishPersonality)


# Insect diet ----
set.seed(9724)
myData <- expand.grid(replicate = 1:12, diet = c("lowProtein", "highProtein"), genotype = c("gt1", "gt2")) %>%
  left_join(expand.grid(diet = c("lowProtein", "highProtein"), genotype = c("gt1", "gt2")) %>%
    mutate(meanSize = c(20, 26, 16, 17.5), semSize = c(2.5, 2.5, 2, 2))) %>%
  mutate(lengthMM = round(rnorm(48, meanSize, semSize)), 2) %>%
  select(diet, genotype, lengthMM)

write.csv(myData, "CourseData/insectDiet.csv", row.names = FALSE)
rm(myData)


# Maze  ----
set.seed(123)
maze <- data.frame(Age = rep(c("Child", "Adult"), each = 10)) %>%
  mutate(nErrors = c(rpois(10, 3), rpois(10, 1)))
write.csv(x = maze, file = "CourseData/maze.csv", row.names = FALSE)
rm(maze)

# Coral ----
set.seed(42)
coral <- data.frame(habitat = rep(c("mesophotic", "shallow"), each = 10), intensity = c(rnorm(10, 160, 50), rnorm(10, 90, 50)))
write.csv(coral, file = "CourseData/coral.csv", row.names = FALSE)
rm(coral)

# Oilseed ----
set.seed(123)
x <- data.frame(plotId = 1:12, yield = round(rnorm(12, 900, 300)))
write.csv(x, file = "CourseData/oilseed.csv", row.names = FALSE)
rm(x)

# Roundabouts ----
set.seed(1234)
sampleSize <- 20
x <- data.frame(area = sort(runif(n = sampleSize, 1, 50))) %>%
  mutate(nSpecies = 1 + rpois(rep(sampleSize, length(area)), lambda = (area / 5) - 0.99)) %>%
  mutate(habitatType = "complex") %>%
  na.omit()
sampleSize <- 17
x2 <- data.frame(area = sort(runif(n = sampleSize, 1, 50))) %>%
  mutate(nSpecies = 1 + rpois(rep(sampleSize, length(area)), lambda = 2)) %>%
  mutate(habitatType = "simple") %>%
  na.omit()
x3 <- rbind(x, x2)
write.csv(x3, file = "CourseData/roundabouts.csv", row.names = FALSE)
rm(x, x2, x3, sampleSize)

# Crabs -----
source("examPreparationCode/simulateCrabs.R")

# Niveoscincus Character displacement---- 

set.seed(44)
niveoscincus <- data.frame(area = rep(c("OverlapZone", "AllopatricZone"), each = 14), snoutVentLength = c(rnorm(14,  53.4, 7.811), rnorm(14,  56, 7.811))) %>% 
  mutate(snoutVentLength = round(snoutVentLength,1))

# ggplot(niveoscincus,aes(x = area,y = snoutVentLength)) + geom_boxplot()
# 
# 
# (obsDiff <- niveoscincus %>%
#     group_by(area) %>%
#     summarise(meanVal = mean(snoutVentLength)) %>%
#     pull(meanVal) %>%
#     diff())
# 
# shuffledData <- data.frame(rep = 1:1000) %>%
#   mutate(shuffledDiffs = replicate(
#     1000,
#     niveoscincus %>%
#       mutate(area = sample(area)) %>%
#       group_by(area) %>%
#       summarise(meanVal = mean(snoutVentLength)) %>%
#       pull(meanVal) %>%
#       diff()
#   ))
# table(shuffledData$shuffledDiffs <= obsDiff)


write.csv(niveoscincus, file = "CourseData/Niveoscincus.csv", row.names = FALSE)

rm(niveoscincus)