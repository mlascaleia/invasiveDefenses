# Load required packages
library(broom)
library(agricolae)
library(dplyr)

# Read the CSV file
data <- read.csv("isha/Harvard Master.csv", header = TRUE)

# Create empty lists to store results
anova_results <- list()
tukey_results <- list()
detailed_results <- list()

# Columns to analyze
columns_to_analyze <- c("Plant.height..ft.", "Dry.mass..mg.", "Nitrogen.content",
                        "Carbon.content", "C.N.ratio", "Flavonoids", "Phenolics", 
                        "Saponins", "Terpenoids", "Tannins")

# Loop through each column and run analyses
for (col in columns_to_analyze) {
  # Create formula
  formula <- as.formula(paste(col, "~ Type"))
  
  # Run ANOVA
  anova_model <- aov(formula, data = data)
  anova_results[[col]] <- anova_model
  
  # Store tidy ANOVA results with effect size
  anova_tidy <- broom::tidy(anova_model)
  anova_tidy$eta_sq <- anova_tidy$sumsq / sum(anova_tidy$sumsq)
  anova_tidy$Variable <- col  # Add variable name to each result
  detailed_results[[col]] <- anova_tidy
  
  # Perform Tukey HSD test
  tukey <- HSD.test(anova_model, "Type", group = TRUE, console = FALSE)
  
  # Store Tukey results in consistent format
  tukey_groups <- data.frame(
    Variable = col,
    Type = rownames(tukey$groups),
    Mean = tukey$groups[, 1],
    Groups = tukey$groups[, 2],
    stringsAsFactors = FALSE
  )
  tukey_results[[col]] <- tukey_groups
}

# Combine ANOVA results safely
detailed_combined <- bind_rows(detailed_results)

# Combine Tukey results safely
tukey_combined <- bind_rows(tukey_results)

# Save results
write.csv(detailed_combined, "isha/ANOVA/whole_plant_anova_results.csv", row.names = FALSE)
write.csv(tukey_combined, "isha/ANOVA/whole_plant_tukey_results.csv", row.names = FALSE)

# Print example output
cat("\nANOVA results structure:\n")
print(head(detailed_combined))

cat("\nTukey results structure:\n")
print(head(tukey_combined))