# the purpose of this script is to load in all the spectrophotometer data
# and organize nicely onto a master datasheet

# this is michael's version where I do it my way

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

# read in data
master <- read.csv("olivia/Scans.csv") %>%
  select(-Flavonoids, -Phenolics, -Saponins, -Terpenoids, -Tannins)

# create vector of file names
csv.names <- list.files(path = "olivia/chemData/", recursive = T, full.names = T)

# create a list of all csvs
csvs <- lapply(csv.names, read.csv)
#so you created a value that is basically the whole list of csv files from every chemical analysis? 
#I'm a little confused how this is different from the previous step

# name them all their file name
names(csvs) <- csv.names

# create the extractor

theExtractor <- function(filename, csvList = csvs){
  thiscsv <- csvList[[filename]]
  assay <- str_extract(filename, "(AlCl|FC|PA|Vanillin)")
  wavelength <- switch(assay,
                       AlCl = 416,
                       FC = 766,
                       PA = c(430, 600),
                       Vanillin = 500)
  absorbance <- thiscsv$Latest..Absorbance[thiscsv$Latest..Wavelength..nm. %in% wavelength]
  return(data.frame(assay = assay, wavelength = wavelength, absorbance = absorbance))
}

theExtractor(csv.names[1])

#ok so basically this is taking each csv file and first determining which assay it is by its filename.
#then for whichever assay it is, it is assigned a certain wavelength. it then finds the value and then returns the data

# for(i in 1:length(csv.names)){
#   theExtractor(names(csvs)[i])
# }


values <- lapply(names(csvs), theExtractor)
names(values) <- str_extract(names(csvs), "(?<=_).*(?=\\.)")
values.df <- bind_rows(values, .id = "Leaf.Number") %>%
  pivot_wider(id_cols = Leaf.Number, names_from = wavelength, values_from = absorbance) %>% #determining how to organize and pull info
  rename(Phenolics = `766`, 
         Flavonoids = `416`, 
         Saponins = `430`, 
         Tannins = `500`, 
         Terpenoids = `600`) %>%
  full_join(master)


write.csv(values.df, "olivia/Harvard Master2.csv", row.names = FALSE)



