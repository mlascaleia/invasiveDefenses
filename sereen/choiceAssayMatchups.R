# the goal of this script it to create matchups for the 
# rheumaptera choice assays, a few different ways

library(tidyverse)

# the first way is completely random
barb <- read.csv("sereen/Alkaloid Notes.csv") %>%
  select(Species) %>%
  filter(Species  != "Berberine")

# functionally the same
# barb <- read.csv("sereen/Alkaloid Notes.csv")
# barb <- select(barb, Species)
# barb <- filter(barb, Species != "Berberine")

generate_matchups <- function(seed_order,
                              matchups = 125,
                              seed = 1) {
  set.seed(seed)
  n   <- length(seed_order)
  cmb <- t(combn(n, 2))           # all pairs as seed positions (a < b)
  
  take    <- sample(nrow(cmb), matchups, replace = FALSE)
  chosen  <- cmb[take, , drop = FALSE]
  
  A <- seed_order[chosen[, 1]]
  B <- seed_order[chosen[, 2]]
  ord  <- sample(length(A))       # randomize running order
  data.frame(
    pla1   = A[ord],
    pla2   = B[ord]
  )
}

matches <- generate_matchups(seed_order = barb$Species, seed = 71) %>%
  mutate(matchupNo = 1:nrow(.)) %>%
  rename(yellow = pla1, green = pla2)

table(c(matches$yellow, matches$green))

write.csv(matches, file = "sereen/choiceAssayMatchups.csv", row.names = F)

