#The purpose of this script is to create a single data frame from the double data entry of the choice assay

#Initialize
rm(list=ls())
library(tidyverse)
library(arsenal)

#Load data
d1 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/choiceAssayMatchups_Entered_1.csv") %>%
  arrange(matchupNo)
d2 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/choiceAssayMatchups_Entered_2.csv") %>%
  arrange(matchupNo)

#Compare the 2 data frames
comparison <- comparedf(d1, d2, by="matchupNo")
diffs(comparison)

final <- d1

# final$Y.Eaten[final$matchupNo == 47] <- d1$Y.Eaten[d1$matchupNo == 47] # d1 is correct (this line is redundant)
final$Notes[final$matchupNo == 45] <- d2$Notes[d2$matchupNo == 45] # d2 is correct
final$Notes[final$matchupNo == 67] <- d2$Notes[d2$matchupNo == 67] #d2 is correct
final$Notes[final$matchupNo == 85] <- d2$Notes[d2$matchupNo == 85] #d2 is correct
final$Notes[final$matchupNo == 99] <- d1$Notes[d1$matchupNo == 99] #d1 is correct
final$Notes[final$matchupNo == 100] <- d1$Notes[d1$matchupNo == 100] #d1 is correct
final$Notes[final$matchupNo == 101] <- d1$Notes[d1$matchupNo == 101] #d1 is correct
final$Notes[final$matchupNo == 115] <- d1$Notes[d1$matchupNo == 115] #d1 is correct
final$Notes[final$matchupNo == 116] <- d1$Notes[d1$matchupNo == 116] #d1 is correct

# now fix the single known error
final$green[final$matchupNo == 61] <- "B. amurensis"

#Omit invalid matchups
final_corr <- final[-c(90, 103), ]
final_clean <- na.omit(final_corr)

# write the completed datasheet
#write.csv(final_clean, "sereen/matchups1clean.csv", row.names = F)

###Matchup 2
#Load data
d2.1 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/matchups2_dataentry1.csv") %>%
  arrange(matchupNo)
d2.2 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/matchups2_dataentry2.csv") %>%
  arrange(matchupNo)

#Compare the 2 data frames
comparison <- comparedf(d2.1, d2.2, by="matchupNo")
diffs(comparison)

final2 <- d2.2

#Omit invalid matchups
final2_clean <- na.omit(final2)

# write the completed datasheet
#write.csv(final_clean, "sereen/matchups2clean.csv", row.names = F)


###Matchup 3
#Load data
d3.1 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/matchups3_dataentry1.csv") %>%
  arrange(matchupNo)
d3.2 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/matchups3_dataentry2.csv") %>%
  arrange(matchupNo)

#Compare the 2 data frames
comparison <- comparedf(d3.1, d3.2, by="matchupNo")
diffs(comparison)

final3 <- d3.2

# final$Y.Eaten[final$matchupNo == 47] <- d1$Y.Eaten[d1$matchupNo == 47] # d1 is correct (this line is redundant)
final3$G.Eaten[final3$matchupNo == 321] <- final3$G.Eaten[d3.1$matchupNo == 321] # d3.1 is correct

#Omit invalid matchups
final3_clean <- na.omit(final3)

# write the completed datasheet
#write.csv(final_clean, "sereen/matchups3clean.csv", row.names = F)



###Matchup 4
#Load data
d4.1 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/matchups4_dataentry1.csv") %>%
  arrange(matchupNo)
d4.2 <- read.csv("sereen/Choice Assay/Choice Data/Raw Data/matchups4_dataentry2.csv") %>%
  arrange(matchupNo)

#Compare the 2 data frames
comparison <- comparedf(d4.1, d4.2, by="matchupNo")
diffs(comparison)

final4 <- d4.1

#Omit invalid matchups
final4_clean <- na.omit(final4)

# write the completed datasheet
#write.csv(final_clean, "sereen/matchups4clean.csv", row.names = F)
