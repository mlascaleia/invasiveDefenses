#The purpose of this script is to create a single data frame from the double data entry of the choice assay

#Initialize
rm(list=ls())
library(tidyverse)
library(arsenal)

#Load data
d1 <- read.csv("sereen/choiceAssayMatchups_Entered_1.csv") %>%
  arrange(matchupNo)
d2 <- read.csv("sereen/choiceAssayMatchups_Entered_2.csv") %>%
  arrange(matchupNo)

#Compare the 2 data frames
comparison <- comparedf(d1, d2, by="matchupNo")
diffs(comparison)

final <- d1

final$Y.Eaten[final$matchupNo == 47] <- d1$Y.Eaten[d1$matchupNo == 47] # d1 is correct (this line is redundant)
final$Notes[final$matchupNo == 45] <- d2$Notes[d2$matchupNo == 45] # d2 is correct
final$Notes[final$matchupNo == 67] <- d2$Notes[d2$matchupNo == 67] #d2 is correct
final$Notes[final$matchupNo == 85] <- d2$Notes[d2$matchupNo == 85] #d2 is correct
final$Notes[final$matchupNo == 99] <- d1$Notes[d1$matchupNo == 99] #d1 is correct
final$Notes[final$matchupNo == 100] <- d1$Notes[d1$matchupNo == 100] #d1 is correct
final$Notes[final$matchupNo == 101] <- d1$Notes[d1$matchupNo == 101] #d1 is correct
final$Notes[final$matchupNo == 115] <- d1$Notes[d1$matchupNo == 115] #d1 is correct
final$Notes[final$matchupNo == 116] <- d1$Notes[d1$matchupNo == 116] #d1 is correct

# now fix the single known error
final$yellow[final$matchupNo == 67] <- "B. amurensis"




