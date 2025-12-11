# the purpose of this script is to run anova tests and tukey tests for whole plant and leaf traits
# and organize nicely onto a master datasheet for each
# and then create box plots that show the signifcance letters from the tukey tests

# clear environment
rm(list = ls())

# load packages
library(broom)
library(agricolae)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)  # Added for combining plots

#whole plant

# Read the CSV file
data <- read.csv("isha/Harvard Master.csv", header = TRUE)
colnames(data)
# Create empty lists to store results
anova_results <- list()
tukey_results <- list()
detailed_results <- list()

# Columns to analyze
columns_to_analyze <- c("Plant.height..ft.", "Dry.mass..mg.", "Nitrogen.content",
                        "C.N.ratio", "Flavonoids", "Phenolics", 
                        "Terpenoids", "Tannins", "Average.water.content....", "SLA..mm2.mg.")

# Set significance threshold (typically 0.05)
alpha_level <- 0.05

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


data2 <- data %>%
  mutate(isInv = ifelse(Type == "Invasive", 1, 0))


summary(lm(Plant.height..ft. ~ isInv, data = data2))

# Combine ANOVA results safely
detailed_combined <- bind_rows(detailed_results)

# Combine Tukey results safely (will include non-significant markers)
tukey_combined <- bind_rows(tukey_results) %>%
  arrange(Variable, Type)  # Sort by variable then type

# Save results
write.csv(detailed_combined, "isha/ANOVA/whole_plant_anova_results.csv", row.names = FALSE)
write.csv(tukey_combined, "isha/ANOVA/whole_plant_tukey_results.csv", row.names = FALSE)


#leaf

# Read the CSV file
data <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)

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
  
  # Store tidy ANOVA results with effect size
  anova_tidy <- broom::tidy(anova_model)
  anova_tidy$eta_sq <- anova_tidy$sumsq / sum(anova_tidy$sumsq)
  anova_tidy$Variable <- col  # Add variable name to each result
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
write.csv(detailed_combined, "isha/ANOVA/leaf_anova_results.csv", row.names = FALSE)
write.csv(tukey_combined, "isha/ANOVA/leaf_tukey_results.csv", row.names = FALSE)




# data visualization


# Load Tukey results (add this near the top after your ANOVA code)
whole_tukey <- read_csv("isha/ANOVA/whole_plant_tukey_results.csv")
leaf_tukey <- read_csv("isha/ANOVA/leaf_tukey_results.csv")

whole_tukey <- whole_tukey %>%
  mutate(Variable = case_when(
    Variable == "SLA..mm2.mg." ~ "SLA (mm2/mg)",
    Variable == "Average.water.content...." ~ "Average water content (%)",
    Variable == "Nitrogen.content" ~ "Nitrogen content",
    Variable == "C.N.ratio" ~ "C:N ratio",
    Variable == "Plant.height..ft." ~ "Plant height (ft)",
    TRUE ~ Variable
  ))

leaf_tukey <- leaf_tukey %>%
  mutate(Variable = case_when(
    Variable == "Mass..mg." ~ "Mass (mg)",
    Variable == "Toughness..N." ~ "Toughness (N)",
    Variable == "Thickness..mm." ~ "Thickness (mm)",
    TRUE ~ Variable
  ))

# Combine both Tukey results (optional, but helps with lookup)
all_tukey <- bind_rows(
  whole_tukey %>% mutate(Source = "Whole"),
  leaf_tukey %>% mutate(Source = "Leaf")
)

# Modified plotting function for single plots
create_boxplot <- function(data, y_var, y_label, file_suffix, tukey_data, plot_label = NULL) {
  # Convert y_var to string for lookup
  y_var_str <- as_label(enquo(y_var))
  
  # Get corresponding Tukey letters
  plot_letters <- tukey_data %>%
    filter(Variable == y_var_str)
  
  # Calculate y-axis limits
  y_max <- max(data[[y_var_str]], na.rm = TRUE) * 1.1
  
  # Create base plot
  p <- ggplot(data = data, mapping = aes(x = Type, y = {{ y_var }})) +
    geom_boxplot(aes(fill = Type)) +
    geom_point() +
    scale_fill_manual(values = c("thistle", "olivedrab3", "lemonchiffon")) +
    labs(x = "Species Type", y = y_label) +
    theme_tufte(18) +
    theme(legend.position = "none",  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
    scale_x_discrete(labels = c(
      "Invasive" = "I",
      "Native" = "N",
      "Non-invasive exotic" = "NIE"
    ))
  
  # Add significance letters if they exist
  if (nrow(plot_letters) > 0) {
    if (all(is.na(plot_letters$Groups))) {
      plot_letters$Groups <- "ns"  # Label non-significant
    }
    
    # Only add letters if we have valid Type values
    if (!all(is.na(plot_letters$Type))) {
      p <- p + geom_text(
        data = plot_letters,
        aes(x = Type, y = y_max, label = Groups),
        vjust = 1.5, 
        size = 7, 
        color = "black"
      )
    }
  } else {
    message("No Tukey data available for: ", y_var_str)
  }
  
  # Add plot label (A, B, C, D) if specified - LOWER POSITION
  if (!is.null(plot_label)) {
    # Position label lower by adjusting vjust to a higher value
    p <- p + 
      annotate("text", x = 0.5, y = Inf, label = plot_label, 
               hjust = 0, vjust = 2, size = 8, fontface = "bold")  # Changed vjust from 1 to 3
  }
  
  # Save individual plot if file_suffix is provided
  if (!is.null(file_suffix)) {
    ggsave(paste0("isha/Plots/", file_suffix, ".png"), 
           plot = p, width = 7, height = 8)
  }
  
  return(p)
}

# Modified processing function
process_columns <- function(data, columns_to_plot, tukey_data) {
  # Loop through each column specification
  for (col_spec in columns_to_plot) {
    col_name <- col_spec$column
    y_label <- col_spec$label
    file_suffix <- col_spec$file_suffix
    
    # Create the plot
    create_boxplot(
      data = data, 
      y_var = !!sym(col_name), 
      y_label = y_label, 
      file_suffix = file_suffix,
      tukey_data = tukey_data
    )
  }
}

# Function to create the grid of 4 plots
create_four_plot_grid <- function(master_whole, master_leaf, whole_tukey, leaf_tukey) {
  # Create the four individual plots with labels
  plot1 <- create_boxplot(
    data = master_whole,
    y_var = `SLA (mm2/mg)`,
    y_label = "Specific Leaf Area (mm²/mg)",
    file_suffix = NULL,  # Don't save individual plot
    tukey_data = whole_tukey,
    plot_label = "A"
  )
  
  plot2 <- create_boxplot(
    data = master_whole,
    y_var = Tannins,
    y_label = "Relative Tannins",
    file_suffix = NULL,
    tukey_data = whole_tukey,
    plot_label = "B"
  )
  
  plot3 <- create_boxplot(
    data = master_leaf,
    y_var = `Toughness (N)`,
    y_label = "Leaf Toughness (N)",
    file_suffix = NULL,
    tukey_data = leaf_tukey,
    plot_label = "C"
  )
  
  plot4 <- create_boxplot(
    data = master_leaf,
    y_var = `Thickness (mm)`,
    y_label = "Leaf Thickness (mm)",
    file_suffix = NULL,
    tukey_data = leaf_tukey,
    plot_label = "D"
  )
  
  # Combine plots into a 2x2 grid
  combined_plot <- (plot1 + plot2) / (plot3 + plot4) +
    plot_layout(guides = 'collect') &
    theme(legend.position = 'none')
  
  # Save the combined plot
  ggsave("isha/Plots/four_trait_grid.png", 
         plot = combined_plot, 
         width = 16,  # Wider for 2 plots side by side
         height = 16, # Taller for 2 rows
         dpi = 300)
  
  # Also save as PDF for publication quality
  ggsave("isha/Plots/four_trait_grid.pdf", 
         plot = combined_plot, 
         width = 16,
         height = 16)
  
  return(combined_plot)
}

# Load data
master_whole <- read_csv("isha/Harvard Master.csv")
master_leaf <- read_csv("isha/Harvard MasterLeaf.csv")

# Run standard individual plots (optional)
columns_to_plot <- list(
  list(column = "Plant height (ft)", label = "Plant Height (ft)", file_suffix = "PlantHeight"),
  list(column = "Nitrogen content", label = "Nitrogen Content", file_suffix = "NitrogenContent"),
  list(column = "C:N ratio", label = "C:N Ratio", file_suffix = "CNRatio"),
  list(column = "Flavonoids", label = "Flavonoids", file_suffix = "Flavonoids"),
  list(column = "Phenolics", label = "Phenolics", file_suffix = "Phenolics"),
  list(column = "Terpenoids", label = "Terpenoids", file_suffix = "Terpenoids"),
  list(column = "Tannins", label = "Tannins", file_suffix = "Tannins"),
  list(column = "Average water content (%)", label = "Average Water Content (%)", file_suffix = "WaterContent"),
  list(column = "SLA (mm2/mg)", label = "Specific Leaf Area (mm2/mg)", file_suffix = "SpecificLeafArea")
)

columns_to_plot_leaf <- list(
  list(column = "Toughness (N)", label = "Toughness (N)", file_suffix = "Toughness"),
  list(column = "Thickness (mm)", label = "Thickness (mm)", file_suffix = "Thickness")
)

# Run individual plotting (optional)
process_columns(master_whole, columns_to_plot, whole_tukey)
process_columns(master_leaf, columns_to_plot_leaf, leaf_tukey)

# Create and save the 4-plot grid
grid_plot <- create_four_plot_grid(master_whole, master_leaf, whole_tukey, leaf_tukey)

# Display the grid plot
print(grid_plot)

cat("Grid plot saved as 'isha/Plots/four_trait_grid.png' and '.pdf'\n")




