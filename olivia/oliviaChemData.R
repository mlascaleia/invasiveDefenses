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
master <- read.csv("isha/Harvard Master.csv") %>%
  select(-Flavonoids, -Phenolics, -Saponins, -Terpenoids, -Tannins)

master$sampleID <- gsub("\\*", "x", master$sampleID)

# create vector of file names
csv.names <- list.files(path = "isha/chemData/", recursive = T, full.names = T)

# create a list of all csvs
csvs <- lapply(csv.names, read.csv)

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

values <- lapply(names(csvs), theExtractor)
names(values) <- str_extract(names(csvs), "(?<=_).*(?=\\.)")
values.df <- bind_rows(values, .id = "sampleID") %>%
  pivot_wider(id_cols = sampleID, names_from = wavelength, values_from = absorbance) %>% #determining how to organize and pull info
  rename(Phenolics = `766`, 
         Flavonoids = `416`, 
         Saponins = `430`, 
         Tannins = `500`, 
         Terpenoids = `600`) %>%
  full_join(master)


write.csv(values.df, "isha/Harvard Master2.csv", row.names = FALSE)



