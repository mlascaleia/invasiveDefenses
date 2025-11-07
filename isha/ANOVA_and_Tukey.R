# the purpose of this script is to run anova tests and tukey tests for whole plant and leaf traits
# and organize nicely onto a master datasheet for each

# clear environment
rm(list = ls())

# load packages
library(broom)
library(agricolae)
library(dplyr)


#whole plant

# Read the CSV file
data <- read.csv("invasiveDefenses/isha/Harvard Master.csv", header = TRUE)
colnames(data)
# Create empty lists to store results
anova_results <- list()
tukey_results <- list()
detailed_results <- list()

# Columns to analyze
columns_to_analyze <- c("Plant.height..ft.", "Dry.mass..mg.", "Nitrogen.content",
                        "Carbon.content", "C.N.ratio", "Flavonoids", "Phenolics", 
                        "Saponins", "Terpenoids", "Tannins", "Average.water.content....", "SLA..mm2.mg.")

# Set significance threshold (typically 0.05)
alpha_level <- 0.05

# Loop through each column and run analyses
for (col in columns_to_analyze) {
  # Create formula
  formula <- as.formula(paste(col, "~ Type"))
  
  # Run ANOVA
  anova_model <- aov(formula, data = data)
  anova_results[[col]] <- anova_model
  
  # Store tidy ANOVA results with effect size AND F-statistic
  anova_tidy <- broom::tidy(anova_model)
  anova_tidy$eta_sq <- anova_tidy$sumsq / sum(anova_tidy$sumsq)
  anova_tidy$Variable <- col  # Add variable name to each result
  
  # The F-statistic is already in the tidy output as 'statistic'
  # Let's rename it to be more explicit
  anova_tidy <- anova_tidy %>%
    rename(F_statistic = statistic)
  
  detailed_results[[col]] <- anova_tidy
  
  # Check if ANOVA is significant (p < alpha_level)
  p_value <- anova_tidy$p.value[anova_tidy$term == "Type"]
  
  if (!is.na(p_value) && p_value < alpha_level) {
    # Only run Tukey if ANOVA is significant
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
    
    cat("Significant ANOVA found for", col, "(p =", p_value, "), running Tukey HSD\n")
  } else {
    tukey_results[[col]] <- data.frame(
      Variable = col,
      Type = NA,
      Mean = NA,
      Groups = NA,
      Note = ifelse(is.na(p_value), "Error in ANOVA", "Not significant (p > 0.05)"),
      stringsAsFactors = FALSE
    )
    cat("Non-significant ANOVA for", col, "(p =", p_value, "), skipping Tukey HSD\n")
  }
}

# Combine ANOVA results safely
detailed_combined <- bind_rows(detailed_results)

# Combine Tukey results safely (will include non-significant markers)
tukey_combined <- bind_rows(tukey_results) %>%
  arrange(Variable, Type)  # Sort by variable then type

# Save results
write.csv(detailed_combined, "invasiveDefenses/isha/ANOVA/whole_plant_anova_results.csv", row.names = FALSE)
write.csv(tukey_combined, "invasiveDefenses/isha/ANOVA/whole_plant_tukey_results.csv", row.names = FALSE)
#leaf

# Read the CSV file
data <- read.csv("invasiveDefenses/isha/Harvard MasterLeaf.csv", header = TRUE)

# Create empty lists to store results
anova_results <- list()
tukey_results <- list()
detailed_results <- list()

# Columns to analyze
columns_to_analyze <- c("Mass..mg.", "Toughness..N.", "Thickness..mm.")

# Set significance threshold (typically 0.05)
alpha_level <- 0.05

# Loop through each column and run analyses
for (col in columns_to_analyze) {
  # Create formula
  formula <- as.formula(paste(col, "~ Type"))
  
  # Run ANOVA
  anova_model <- aov(formula, data = data)
  anova_results[[col]] <- anova_model
  
  # Store tidy ANOVA results with effect size AND F-statistic
  anova_tidy <- broom::tidy(anova_model)
  anova_tidy$eta_sq <- anova_tidy$sumsq / sum(anova_tidy$sumsq)
  anova_tidy$Variable <- col  # Add variable name to each result
  
  # Rename the statistic column to F_statistic
  anova_tidy <- anova_tidy %>%
    rename(F_statistic = statistic)
  
  detailed_results[[col]] <- anova_tidy
  
  # Check if ANOVA is significant (p < alpha_level)
  p_value <- anova_tidy$p.value[anova_tidy$term == "Type"]
  
  if (!is.na(p_value) && p_value < alpha_level) {
    # Only run Tukey if ANOVA is significant
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
    
    cat("Significant ANOVA found for", col, "(p =", p_value, "), running Tukey HSD\n")
  } else {
    tukey_results[[col]] <- data.frame(
      Variable = col,
      Type = NA,
      Mean = NA,
      Groups = NA,
      Note = ifelse(is.na(p_value), "Error in ANOVA", "Not significant (p > 0.05)"),
      stringsAsFactors = FALSE
    )
    cat("Non-significant ANOVA for", col, "(p =", p_value, "), skipping Tukey HSD\n")
  }
}

# Combine ANOVA results safely
detailed_combined <- bind_rows(detailed_results)

# Combine Tukey results safely (will include non-significant markers)
tukey_combined <- bind_rows(tukey_results) %>%
  arrange(Variable, Type)  # Sort by variable then type

# Save results
write.csv(detailed_combined, "invasiveDefenses/isha/ANOVA/leaf_anova_results.csv", row.names = FALSE)
write.csv(tukey_combined, "invasiveDefenses/isha/ANOVA/leaf_tukey_results.csv", row.names = FALSE)

