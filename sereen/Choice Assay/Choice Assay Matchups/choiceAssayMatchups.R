# the goal of this script it to create matchups for the 
# rheumaptera choice assays

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

# write.csv(matches, file = "sereen/choiceAssayMatchups.csv", row.names = F)

# following this up, I'm going to identify all matchups that were 
# not done between two berberis species

trueBarb <- barb[grepl("^(B|M)", barb$Species), ]
cmb <- t(combn(25, 2))
allMatches <- generate_matchups(seed_order = trueBarb, seed = 71, matchups = nrow(cmb))%>%
  rename(yellow = pla1, green = pla2)

# now bring in the completed first set of matchups
mu1 <- read.csv("sereen/matchups1.csv")

whosDone <- full_join(mu1, allMatches, by = c("yellow", "green"))

# so now we need to figure out whom to do/re-do
# I'll comment why each line exists

whosDone <- whosDone %>%
  mutate(done = ifelse(is.na(matchupNo), "no", "yes")) %>%
  # re-do matchups where the caterpillar was not removed
  mutate(done = ifelse(str_detect(Notes, "Re-do CAT")|
                         is.na(Notes), "no", done)) %>%
  # re-do matchups where the caterpillar ate all of one then moved to the next
  mutate(done = ifelse((Y.Eaten == 100 & G.Eaten > 0)|
                       (Y.Eaten > 0 & G.Eaten == 100) |
                         is.na(Y.Eaten), "no", done)) %>%
  # erase data from matchups that are being re-done
  mutate(across(ends_with("Eaten"), ~ ifelse(matchupNo <= 125 & done == "no", NA, .x))) %>%
  # now assign matchup numbers to the rest
  mutate(matchupNo = ifelse(is.na(matchupNo), (max(.$matchupNo, na.rm = T)+1):nrow(.), matchupNo)) %>%
  # arrange for printed data sheet
  arrange(done, matchupNo)

toDo <- whosDone %>% filter(done == "no")

# write.csv(toDo, "sereen/matchups2_empty.csv", row.names = F, na = "")

# We ultimately only worked until 206, so now I want to do 2 things
# 1, I want to gather more info on underrepresented individuals
# 2 I want to gather more information on species of interest

# And I think I currently have about 200 matchups to do it in, so let's set it up

done2 <- whosDone %>%
  filter(matchupNo <= 206)

# point number 2 - make 100 matchups that focus on a specific set of barberries

bOfInterest <- c("B. canadensis", "B. thunbergii", 
                 "B. vulgaris", "M. aquifolium", "B. gilgiana",
                 "B. amurensis", "B. virgetorum")

# pull x matchups without repeats, for each of these species

x <- 15
set.seed(71)
pla1 <- NULL; pla2 <- NULL
for(i in 1:length(bOfInterest)){
  pla1[(((i-1)*x)+1):(i*x)] <- rep(bOfInterest[i], x) 
  pla2[(((i-1)*x)+1):(i*x)] <- sample(trueBarb[trueBarb != bOfInterest[i]], x, replace = F)
}
matches3 <- data.frame(pla1, pla2)
for(i in 1:nrow(matches3)){
  if(runif(1) > .5){
    matches3[i,] <- matches3[i,c("pla2", "pla1")]
  }
}
m3 <- matches3 %>%
  rename(yellow = pla1, green = pla2) %>%
  mutate(Y.Eaten = "", G.Eaten = "", Notes = "", matchupNo = 301:(300 + nrow(.)))

write.csv(m3, "sereen/matchups3_empty.csv", row.names = F)

# okay then point 1 even out the matchup distribution
# not inlcuding things made in point 2

NumTestsSoFar <- sort(table(c(done2$green, done2$yellow)))
need <- NumTestsSoFar %>%
  as.data.frame() %>%
  filter(Var1 %in% trueBarb) %>%
  mutate(need = max(.$Freq) - Freq) %>%
  select(species = Var1, need = need) %>%
  filter(need > 0) %>%
  mutate(species = as.character(species))

# I'm not going to concern myself over repeats here,
# so there will be repeats, especially once part 2 is factored in 

repped <- NULL
for(i in 1:nrow(need)){
  t <- rep(need$species[i], need$need[i])
  repped <- c(repped, t)
}

# now do selection
set.seed(71)
rando <- sample(1:length(repped))
pla1 <- repped[rando[1:(length(repped)/2)]]
pla2 <- repped[rando[(length(repped)/2 + 1):(length(repped))]]
matches4 <- data.frame(pla1, pla2)
bads <- matches4[matches4$pla1 == matches4$pla2, ] # need 6 more matchups, 2 for each of these
matches4 <- matches4[matches4$pla1 != matches4$pla2, ]
extrapla <- rep(bads$pla1, 2)
m_for_extrapla <- sample(trueBarb, 6, replace = F) # (could be replace = T but this makes it more even)
matches4.01 <- data.frame(pla1 = extrapla, pla2 = m_for_extrapla)
# matches4.01[matches4.01$pla1 == matches4.01$pla2, ] #good
m4 <- bind_rows(matches4, matches4.01) %>%
  rename(yellow = pla1, green = pla2) %>%
  mutate(Y.Eaten = "", G.Eaten = "", Notes = "", matchupNo = 501:(500 + nrow(.)))
write.csv(m4, "sereen/matchups4_empty.csv", row.names = F)




