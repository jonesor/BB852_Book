cancer <- read.csv("CourseData/cancer.csv")
library(tidyverse)

ggplot(cancer, aes(x = Distance, y = Cancers)) +
  geom_point()


mod1 <- glm(Cancers ~ Distance, data = cancer, family = poisson)
summary(mod1)


# Vector to predict from
newData <- data.frame(Distance = seq(0, 100, 1))

# Predicted values (and SE)
predVals <- predict(mod1, newData,
  type = "response",
  se.fit = TRUE
)

# Create new data for the predicted fit line
newData <- newData %>%
  mutate(Cancers = predVals$fit) %>%
  mutate(ymin = predVals$fit - 1.96 * predVals$se.fit) %>%
  mutate(ymax = predVals$fit + 1.96 * predVals$se.fit)

head(newData)
ggplot(cancer, aes(x = Distance, y = Cancers)) +
  geom_ribbon(data = newData, aes(
    x = Distance, ymin = ymin,
    ymax = ymax
  ), fill = "grey75") +
  geom_point() +
  geom_smooth(data = newData, stat = "identity")
