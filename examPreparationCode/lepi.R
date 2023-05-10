allmoth <- read.csv("/Users/jones/Dropbox/Moths/GBIFmoths/allData.csv")
names(allmoth)

lepi <- allmoth %>%
  select(species, year, month, day, decimalLatitude, decimalLongitude) %>%
  mutate(dayOfYear = lubridate::yday(lubridate::dmy(paste(day, month, year, sep = "/")))) %>%
  filter(year %in% 2000:2010)

write.csv(lepi, file = "CourseData/lepi.csv", row.names = FALSE)

library(patchwork)

# ID the most common
top5 <- lepi %>%
  group_by(species) %>%
  summarise(number = n()) %>%
  arrange(-number) %>%
  slice(1:5) %>%
  pull(species)


lepi1 <- lepi %>%
  filter(species %in% top5) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(number = n())

A <- ggplot(lepi1, aes(x = year, y = number)) +
  geom_line() +
  geom_point()

lepi2 <- lepi %>%
  filter(species %in% top5) %>%
  group_by(species, year) %>%
  summarise(number = n())

B <- ggplot(lepi2, aes(x = year, y = number, colour = species)) +
  geom_line() +
  geom_point()

A + ggtitle("A") + B + ggtitle("B")


lepi2 %>%
  group_by(species) %>%
  summarise(min = min(number), max = max(number), mean = mean(number), sd = sd(number)) %>%
  arrange(-mean)
