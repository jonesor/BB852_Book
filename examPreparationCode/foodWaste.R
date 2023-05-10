# Food waste

# grams per capita per week
gpd <- 245.4 # 49.08


n <- 37

gpd_se <- 9.1

# sd = se * sqrt(n)
(gpd_sd <- gpd_se * sqrt(n))

x <- rnorm(n = 1000, mean = gpd, sd = gpd_sd)
table(x < 0)

hist(x)


# Food waste

# grams per capita per day
gpd2 <- gpd * 0.75


# Set up a data frame for the simulation results
simulData <- data.frame(sampleSize = 5:20)

# Set basic values
(controlMean <- gpd)
(treatmentMean <- gpd2)
(sdValue <- gpd_sd)

# Function to do the t-test
pwr <- function(n) {
  sum(replicate(
    1000,
    t.test(
      rnorm(n, controlMean, sdValue),
      rnorm(n, treatmentMean, sdValue)
    )$p.value
  ) < 0.05) / 1000
}

# map_dbl applying the function for every value of
# simulData$sampleSize
simulData$Power <- purrr::map_dbl(simulData$sampleSize, pwr)


# Plot the output
ggplot(simulData, aes(x = sampleSize, y = Power)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed")
