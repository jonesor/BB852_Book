# Simulate crabs

library(tidyverse)
library(patchwork)


set.seed(124)
sampleSize <- 60
crabs <- data.frame(ID = 1:sampleSize, 
                    carapaceWidth = runif(n = sampleSize, min = 55, max = 80)) |> 
  mutate(chelaLength = -2.93 + 0.676 * 
           carapaceWidth + rnorm(n = sampleSize, mean = 0, sd = 6)) |>
  mutate(chelaLength = round(chelaLength, 2)) |>
  mutate(carapaceWidth = round(carapaceWidth, 2))

# plot(crabs$carapaceWidth, crabs$chelaLength)

set.seed(12)

fightOutcome <- data.frame(focalCrab = sample(x = crabs$ID, 
                                              size = sampleSize / 2)) |>
  mutate(opponentCrab = sample(x = crabs$ID[!crabs$ID %in% focalCrab], 
                               size = sampleSize / 2)) |>
  left_join(crabs, by = c("focalCrab" = "ID")) |>
  left_join(crabs, by = c("opponentCrab" = "ID")) |>
  rename(focal_carapaceWidth = carapaceWidth.x) |>
  rename(opponent_carapaceWidth = carapaceWidth.y) |>
  rename(focal_chelaLength = chelaLength.x) |>
  rename(opponent_chelaLength = chelaLength.y) |>
  mutate(relativeCarapaceWidth = focal_carapaceWidth / opponent_carapaceWidth) |>
  mutate(relativeChelaLength = focal_chelaLength / opponent_chelaLength)


set.seed(123)
fightOutcome_2 <- fightOutcome |>
  mutate(outcome = -8.5 + 8 * relativeChelaLength + 0.5 * relativeCarapaceWidth) |>
  mutate(pr = 1 / (1 + exp(-outcome))) |>
  mutate(y = rbinom(n = length(pr), size = 1, prob = pr)) |>
  mutate(fightOutcome = ifelse(y == 1, "won", "lost"))

# Save the data ----
crabFights <- fightOutcome_2 |>
  select(focalCrab, opponentCrab, focal_carapaceWidth, 
         focal_chelaLength, opponent_carapaceWidth, 
         opponent_chelaLength, fightOutcome)

write.csv(x = crabFights, file = "DataSetLibrary/crabFights.csv", 
          row.names = FALSE)
