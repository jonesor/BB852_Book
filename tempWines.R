# Load the data from a CSV file 'winesSpain.csv', treating empty strings as missing values (NA)
wines <- read.csv("DataSetLibrary/winesSpain.csv", na.strings = "")
# Display the column names of the wines dataset to understand its structure
names(wines)

# Create and display frequency tables for the 'year' and 'region' columns in the wines dataset
table(wines$year)
table(wines$region)

# Filter out non-vintage (N.V.) wines and include only wines from 1980 onwards
wines <- wines %>%
  filter(year != "N.V.") %>%
  mutate(year = as.numeric(year)) %>%  # Convert 'year' column to numeric for analysis
  filter(year >= 1980)

# Create a table showing sample size, mean and standard deviation of ratings for each region,
# then sort it by sample size in descending order and select the top 10 regions
wineTable1 <- wines %>%
  group_by(region) %>%
  summarise(sampleSize = n(), meanRating = mean(rating), sdRating = sd(rating)) %>%
  arrange(-sampleSize) %>%
  slice(1:10)

# Display the table of the top 10 regions by sample size
wineTable1

# Filter the dataset to include only wines from the top two regions
wines <- wines %>%
  filter(region %in% wineTable1$region[1:2])

# Summarize the dataset to compute the mean wine quality (rating) by year and region
wineSummary <- wines %>%
  group_by(year, region) %>%
  summarise(meanQuality = mean(rating))

# Plot the mean wine quality by year for each of the top two regions using ggplot2
ggplot(wineSummary, aes(x = year, y = meanQuality, color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +  # Add a linear model fit
  ggtitle("A - quality~temperature relationship") + 
  ylab("Year") + 
  xlab("Mean wine rating")

# Load temperature data for Spain from a CSV file
spainTemp <- read.csv("DataSetLibrary/SpainSurfaceTemp.csv")
# Display a summary of the Spain temperature dataset
summary(spainTemp)

# Merge the wine quality summary data for the Rioja region with the Spain temperature data
wineSummaryRioja <- wineSummary %>%
  filter(region == "Rioja") %>%
  left_join(spainTemp)

# Plot the relationship between mean wine quality and temperature for the Rioja region using ggplot2
ggplot(wineSummaryRioja, aes(x = tempC, y = meanQuality)) +
  geom_point() +
  geom_smooth(method = "lm") +  # Add a linear model fit
  ggtitle("B - quality~temperature relationship") + 
  xlab("Temperature (Celsius)") + 
  ylab("Mean wine rating")
