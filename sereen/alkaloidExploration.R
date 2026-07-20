library(tidyverse)

alk <- read.csv("michael/lcmsData/berberidaceae/berberidaceaeFinalized.csv") %>%
  filter(NPC.pathway == "Alkaloids")

sort(-table(alk$NPC.class))
