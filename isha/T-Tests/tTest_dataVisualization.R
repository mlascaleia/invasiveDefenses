# the purpose of this script is to run t tests for whole plant and leaf traits
# and organize nicely onto a master datasheet for each
# and then create box plots

# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(ggplot2)



# Read data
plant_data <- read.csv("isha/Harvard Master.csv", header = TRUE)

# Create binary variable: 1 = Invasive, 0 = Non-invasive (native + non-invasive exotic)
plant_data$isInvasive <- ifelse(plant_data$Type == "Invasive", 1, 0)

# List of traits to analyze
traits <- c("Plant.height..ft.", "Nitrogen.content", "C.N.ratio", 
            "Flavonoids", "Phenolics", "Terpenoids", "Tannins", 
            "Average.water.content....", "SLA..mm2.mg.")

# Create empty dataframe for results
results <- data.frame()

# Whole plant traits t-tests ----
for (trait in traits) {
  # T-test using binary variable
  test <- t.test(plant_data[[trait]] ~ plant_data$isInvasive)
  
  # Get means
  means <- tapply(plant_data[[trait]], plant_data$isInvasive, mean, na.rm = TRUE)
  
  # Store results
  results <- rbind(results, data.frame(
    Trait = trait,
    Mean_Invasive = means["1"],
    Mean_NonInvasive = means["0"],
    Mean_Difference = means["1"] - means["0"],
    t_statistic = test$statistic,
    df = test$parameter,
    p_value = test$p.value,
    Significant = ifelse(test$p.value < 0.05, "Yes", "No")
  ))
}

# View results
print(results)

# Save results
write.csv(results, "isha/T-Tests/t_test_whole_results.csv", row.names = FALSE)



# Leaf traits t-tests ----

leaf_data <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)

# Create binary variable: 1 = Invasive, 0 = Non-invasive (native + non-invasive exotic)
leaf_data$isInvasive <- ifelse(leaf_data$Type == "Invasive", 1, 0)

#### Eliminate pseudoreplication by averaging per species ####
leaf_data_sum <- leaf_data %>%
  group_by(Species.Name, Type, isInvasive) %>%
  summarise(
    Toughness..N. = mean(Toughness..N., na.rm = TRUE),
    Thickness..mm. = mean(Thickness..mm., na.rm = TRUE)
  )

# Traits to analyze
leaf_traits <- c("Toughness..N.", "Thickness..mm.")

# Create empty dataframe for results
results <- data.frame()

# Run t-tests using the averaged data
for (trait in leaf_traits) {
  # T-test using binary variable
  test <- t.test(leaf_data_sum[[trait]] ~ leaf_data_sum$isInvasive)
  
  # Get means
  means <- tapply(leaf_data_sum[[trait]], leaf_data_sum$isInvasive, mean, na.rm = TRUE)
  
  # Store results
  results <- rbind(results, data.frame(
    Trait = trait,
    Mean_Invasive = means["1"],
    Mean_NonInvasive = means["0"],
    Mean_Difference = means["1"] - means["0"],
    t_statistic = test$statistic,
    df = test$parameter,
    p_value = test$p.value,
    Significant = ifelse(test$p.value < 0.05, "Yes", "No")
  ))
}

# View results
print(results)

# Save results (directly in isha folder, not in subfolder)
write.csv(results, "isha/T-Tests/t_test_leaf_results.csv", row.names = FALSE)




# Data visualization ----


# Read data
plant_data <- read.csv("isha/Harvard Master.csv", header = TRUE)

# Create status column
plant_data$Status <- ifelse(plant_data$Type == "Invasive", "Invasive", "Non-Invasive")

# List of traits
traits <- c("Plant.height..ft.", "Nitrogen.content", "C.N.ratio", 
            "Flavonoids", "Phenolics", "Terpenoids", "Tannins", 
            "Average.water.content....", "SLA..mm2.mg.")

# Make box plots
for (trait in traits) {
  p <- ggplot(plant_data, aes(x = Status, y = .data[[trait]], fill = Status)) +
    geom_boxplot()
  
  ggsave(paste0("isha/Plots/", trait, ".png"), p)
}

# LEAF TRAITS
leaf_data <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)
leaf_data$Status <- ifelse(leaf_data$Type == "Invasive", "Invasive", "Non-Invasive")

# Average by species
leaf_sum <- aggregate(cbind(Toughness..N., Thickness..mm.) ~ Species.Name + Status, 
                      data = leaf_data, FUN = mean, na.rm = TRUE)

leaf_traits <- c("Toughness..N.", "Thickness..mm.")

for (trait in leaf_traits) {
  p <- ggplot(leaf_sum, aes(x = Status, y = .data[[trait]], fill = Status)) +
    geom_boxplot()
  
  ggsave(paste0("isha/Plots/", trait, ".png"), p)
}



