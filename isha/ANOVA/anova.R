# the purpose of this script is to run anova tests for whole plant and leaf traits
# and organize nicely onto a master datasheet

# clear environment
rm(list = ls())

# load packages
library(broom)
library(dplyr)



#whole plant data


# Read the CSV file
data <- read.csv("isha/Harvard Master.csv", header = TRUE)

# Create an empty list to store results
anova_results <- list()

# Columns to analyze (adjust these to match your column names)
columns_to_analyze <- c("Plant.height..ft.", "Dry.mass..mg.", "Nitrogen.content","Carbon.content", "C.N.ratio", "Flavonoids", "Phenolics", "Saponins", "Terpenoids", "Tannins")

# Loop through each column and run ANOVA
for (col in columns_to_analyze) {
  formula <- as.formula(paste(col, "~ Type"))
  anova_results[[col]] <- aov(formula, data = data)
}

detailed_results <- lapply(anova_results, function(x) {
  result <- tidy(x)
  # Add effect size (eta squared)
  result$eta_sq <- result$sumsq / sum(result$sumsq)
  return(result)
})

# Combine with variable names
detailed_combined <- do.call(rbind, Map(cbind, 
                                        Variable = names(detailed_results),
                                        detailed_results))

# Save detailed results
write.csv(detailed_combined, "isha/ANOVA/whole_plant_anova_results.csv", row.names = FALSE)

whole_plant_anova <- read.csv("isha/ANOVA/whole_plant_anova_results.csv")




# leaf data


# Read the CSV file
data_leaf <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)

# Ensure grouping variable is a factor
data_leaf$Type <- as.factor(data_leaf$Type)

# Columns to analyze
columns_to_analyze_leaf <- c("Mass..mg.", "Toughness..N.", "Thickness..mm.")

# Run ANOVAs and store results
anova_results_leaf <- lapply(columns_to_analyze_leaf, function(col) {
  aov(as.formula(paste(col, "~ Type")), data = data_leaf)
})
names(anova_results_leaf) <- columns_to_analyze_leaf

# Extract and format results
detailed_results <- lapply(names(anova_results_leaf), function(name) {
  model <- anova_results_leaf[[name]]
  result <- tidy(model)
  result %>%
    mutate(
      Variable = name,
      eta_sq = sumsq / sum(sumsq)
    ) %>%
    select(Variable, everything())
}) %>% bind_rows()

# Save results
write.csv(detailed_results, "isha/ANOVA/leaf_anova_results.csv", row.names = FALSE)

leaf_anova <- read.csv("isha/ANOVA/leaf_anova_results.csv")




