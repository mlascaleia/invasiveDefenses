# the purpose of this script is to create box plots of all the data
# and save them as png's in the folder "Plots"

# clear environment
rm(list = ls())


#load packages
library(tidyverse)
library(ggplot2)
library(ggthemes)


# Define your plotting function
create_boxplot <- function(data, y_var, y_label, file_suffix) {
  p <- ggplot(data = data, mapping = aes(x = Type, y = {{ y_var }})) +
    geom_boxplot(aes(fill = Type)) +
    geom_point() +
    scale_fill_manual(values = c("thistle", "olivedrab3", "lemonchiffon"), 
                      name = "Species Type") +
    xlab("Species Type") +
    ylab(y_label) +
    theme_tufte() +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5))
  
  # Save with dynamic filename
  ggsave(paste0("invasiveDefenses/isha/Plots/", file_suffix, ".png"),
         plot = p,
         width = 7,      # Adjust as needed
         height = 8,     # Adjust as needed
         units = "in",
         dpi = 300)
}


# Function to process all specified columns
process_columns <- function(data, columns_to_plot)
  
  # Loop through each column specification
  for (col_spec in columns_to_plot) {
    col_name <- col_spec$column
    y_label <- col_spec$label
    file_suffix <- col_spec$file_suffix
    
    # Create the plot
    create_boxplot(data = data, 
                   y_var = !!sym(col_name), 
                   y_label = y_label, 
                   file_suffix = file_suffix)
  }

#whole plant traits
master_whole <- read_csv("invasiveDefenses/isha/Harvard Master.csv")
colnames(master_whole)
# Define which columns to plot and their labels
columns_to_plot <- list(
  list(column = "Plant height (ft)", label = "Plant Height (ft)", file_suffix = "PlantHeight"),
  list(column = "Dry mass (mg)", label = "Dry Mass (mg)", file_suffix = "DryMass"),
  list(column = "Nitrogen content", label = "Nitrogen Content", file_suffix = "NitrogenContent"),
  list(column = "Carbon content", label = "Carbon Content", file_suffix = "CarbonContent"),
  list(column = "C:N ratio", label = "C:N Ratio", file_suffix = "CNRatio"),
  list(column = "Flavonoids", label = "Flavonoids", file_suffix = "Flavonoids"),
  list(column = "Phenolics", label = "Phenolics", file_suffix = "Phenolics"),
  list(column = "Saponins", label = "Saponins", file_suffix = "Saponins"),
  list(column = "Terpenoids", label = "Terpenoids", file_suffix = "Terpenoids"),
  list(column = "Tannins", label = "Tannins", file_suffix = "Tannins"),
  list(column = "Average water content (%)", label = "Average water content (%)", file_suffix = "WaterContent"),
  list(column = "SLA (mm2/mg)", label = "SLA (mm2/mg)", file_suffix = "SLA")
  # Add more columns as needed
)

# Run the plotting process
process_columns(master_whole, columns_to_plot)



# leaf traits
master_leaf <- read_csv("invasiveDefenses/isha/Harvard MasterLeaf.csv")
colnames(master_leaf)

# Define which columns to plot and their labels
columns_to_plot_leaf <- list(
  list(column = "Mass (mg)", label = "Mass (mg)", file_suffix = "Mass"),
  list(column = "Toughness (N)", label = "Toughness (N)", file_suffix = "Toughness"),
  list(column = "Thickness (mm)", label = "Thickness (mm)", file_suffix = "Thickness")
  # Add more columns as needed
)

# Run the plotting process
process_columns(master_leaf, columns_to_plot_leaf)
