
Alkaloid_Notes <- read.csv("sereen/Alkaloid Assay/Alkaloid Notes.csv")

library(tidyverse)
library(dplyr)
library(purrr)
library(readr)

#Import Alkaloid Notes

# Get a list of all CSV file paths in the folder
file_paths <- list.files(path = "sereen/Alkaloid Assay/Spec Data/", 
                         pattern = "*.csv", full.names = TRUE)



# Loop through files, pull row 29, and bind them together
compiled_data <- map_dfr(file_paths, function(file) {
  read_csv(file, show_col_types = FALSE) %>% 
    slice(29) %>%                                # Extract row number 29
    mutate(source_file = basename(file))        # Track which file it came from
})


# Remove .csv from species names
compiled_data$source_file <- sub(".csv", "", compiled_data$source_file, fixed = TRUE)


# Rename a single column
compiled_data <- rename(compiled_data, Species = source_file)


#Combine data frames
# Inner Join (Keeps only rows with matching IDs in both dataframes)
merged_df <- merge(compiled_data, Alkaloid_Notes, by = "Species")

####
#Make a new row with absorbance accounting for dilution factor
merged_df$absorbance_dilution <- merged_df$`Latest: Absorbance` / merged_df$Dilution

merged_df$absorbance_dilution_drymass <- merged_df$`Latest: Absorbance` / merged_df$`Dilution` / merged_df$Dry.Mass



###Pull invasive, non-invasive exotic, add this in manually to the csv and then run it to compare alkaloid concentration, by dilution and dry mass
model <- lm(absorbance_dilution_drymass ~ INV, data = merged_df)

##I need to take the means of each subset and then compare them with Anova? 
invasive_sub_df <- merged_df[!is.na(merged_df$INV) & merged_df$INV == "Invasive",]
inv_mean <- mean(invasive_sub_df$absorbance_dilution_drymass)

nie_sub_df <- merged_df[!is.na(merged_df$INV) & merged_df$INV == "Non-Invasive Exotic",]
nie_mean <- mean(nie_sub_df$absorbance_dilution_drymass)

native_sub_df <- merged_df[!is.na(merged_df$INV) & merged_df$INV == "Native",]
native_mean <- mean(native_sub_df$absorbance_dilution_drymass)


combined_df <- rbind(invasive_sub_df, nie_sub_df, native_sub_df)

save(combined_df, file = "sereen/Alkaloid Assay/alkaloidassay.rdata")

model <- lm(absorbance_dilution_drymass ~ as.factor(INV), data = combined_df)
summary(model)

boxplot(absorbance_dilution_drymass ~ INV, data = combined_df, 
        main = "Absorbance by Invasion Status",
        xlab = "Invasion Status",
        ylab = "Absorbance",
        col = c("lightblue", "lightgreen", "lightpink"))


####Now, effect size of cloudiness on the data????
