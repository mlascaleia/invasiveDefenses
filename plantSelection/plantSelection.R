rm(list = ls())

library(tidyverse)

# read data

allPlants <- read.csv("full_raw_plants.csv")
wantSpecies <- read.csv("cutDownList.csv")

# clean big data

targetPlants_all <- allPlants %>%
  select(Plant.ID, Family, Genus, Species = Specific.epithet,
         Cultivar, Sub = Infraspecific.Epithet, Garden.Location, 
         lat = Garden.Latitude, lon = Garden.Longitude,
         Condition) %>%
  right_join(wantSpecies, by = c("Family", "Genus", "Species")) %>%
  
  # it appears some plants have died. I will need to remove them from the list
  
  filter(!(is.na(Plant.ID) & AA > 0)) %>%
  filter(!(is.na(lat) & AA > 0))

# let's try to see if I can get just viburnums today

vibs <- targetPlants_all[targetPlants_all$Genus %in% "Viburnum", ]

length(unique(vibs$Species)) # 39 species
# how many of them are present in the block that consists of...
# 24, 25, 18, 19, 13, and 14?

length(unique(
  vibs$Species[str_detect(vibs$Garden.Location, "(24|25|18|19|13|14)")]
  ))

# 33 of 39 are present there

unique(
  vibs$Species[str_detect(vibs$Garden.Location, "(24|25|18|19|13|14)")]
)

# get vibs for FIRST DAY!

# set.seed(515)
# vibs515 <- vibs[str_detect(vibs$Garden.Location, "(24|25|18|19|13|14)\\-"), ] %>%
#  
#    # checked manually all cultivars can be removed
#   filter(Cultivar == "") %>%
#   
#   # and now randomly get one of each
#   
#   group_by(Species) %>%
#   slice_sample(n = 1)


# I'm gonna have to pick manually or semi-randomly

write.csv(vibs, "vibs.csv", row.names = F)


# plot 515 vibs

ggplot(data = vibs[str_detect(vibs$Garden.Location, "(25|24|18|19|13|14)\\-"), ], aes(x = lon, y = lat)) +
  geom_point() +
  geom_point(data = vibs515, aes(color = Species)) +
  # geom_label(data = vibs515, aes(label = Species), nudge_y = 0.0005) +
  coord_fixed()

write.csv(vibs515, "vibs515.csv", row.names = F)

write.table(vibs515$Plant.ID, "vibs515ID.txt", row.names = F)

