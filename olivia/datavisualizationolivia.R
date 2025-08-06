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
  # Convert treatment to factor with specified order
  data$treatment <- factor(data$treatment, levels = c("LP", "LA", "AP", "AA"))
  
  ggplot(data = data, mapping = aes(x = treatment, y = {{ y_var }})) +
    geom_boxplot(aes(fill = treatment),
                 color = "white",               # Outline color
                 outlier.color = "white",       # Outlier points color
                 alpha = 0.7) +                 # Slightly transparent fill
    stat_boxplot(geom = "errorbar",             # Whiskers
                 color = "white",
                 width = 0.2) +
    geom_point(color = "white") +
    scale_fill_manual(values = c("darkolivegreen4", "olivedrab3", "lightsalmon", "peachpuff"), 
                      name = "Treatments",
                      labels = c("Native Intact", "Native Removed", "Exotic Intact", "Exotic Removed")) +
    xlab("Treatments") +
    ylab(y_label) +
    theme_tufte() +
    theme(
      legend.position = "none",
      text = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      axis.text.x = element_blank(),
      axis.title = element_text(color = "white"),
      axis.ticks = element_line(color = "white"),
      axis.ticks.x = element_blank(),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      plot.title = element_text(hjust = 0.5))
  
  # Save with dynamic filename
  ggsave(paste0("olivia/finalplots/", file_suffix, ".png"))
}

# Function to process all specified columns
process_columns <- function(data, columns_to_plot) {
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
}

#chemical analyses
all_data <- read_csv("olivia/Harvard Master2.csv")
all_data$treatment <- stringr::str_extract(all_data$Leaf.Number, "^..")
colnames(all_data)

# Define which columns to plot and their labels
columns_to_plot <- list(
  list(column = "Flavonoids", label = "Flavonoids", file_suffix = "Flavonoids"),
  list(column = "Phenolics", label = "Phenolics", file_suffix = "Phenolics"),
  list(column = "Saponins", label = "Saponins", file_suffix = "Saponins"),
  list(column = "Terpenoids", label = "Terpenoids", file_suffix = "Terpenoids"),
  list(column = "Tannins", label = "Tannins", file_suffix = "Tannins")
)

# Run the plotting process
process_columns(all_data, columns_to_plot)

#physical tests
all_data_physical <- read_csv("olivia/physicalData.csv")
all_data_physical$treatment <- stringr::str_extract(all_data_physical$"Leaf Number", "^..")
colnames(all_data_physical)

# Define which columns to plot and their labels
columns_to_plot_physical <- list(
  list(column = "Toughness (N)", label = "Toughness (N)", file_suffix = "Toughness"),
  list(column = "Thickness (mm)", label = "Thickness (mm)", file_suffix = "Thickness"),
  list(column = "Mass (mg)", label = "Mass (mg)", file_suffix = "Mass"),
  list(column = "Dried Mass (mg)", label = "Dry Mass (mg)", file_suffix = "DryMass"),
  list(column = "Caterpillar Mass Change", label = "Change in Mass (mg)", file_suffix = "CaterpillarMass")
)

# Run the plotting process
process_columns(all_data_physical, columns_to_plot_physical)




# # clear environment
# rm(list = ls())
# 
# 
# #load packages
# library(tidyverse)
# library(ggplot2)
# library(ggthemes)
# 
# 
# 
# # Define your plotting function
# create_boxplot <- function(data, y_var, y_label, file_suffix) {
#   ggplot(data = data, mapping = aes(x = treatment, y = {{ y_var }})) +
#     geom_boxplot(aes(fill = treatment)) +
#     geom_point() +
#     scale_fill_manual(values = c("darkolivegreen4","olivedrab3", "lightsalmon","peachpuff"), 
#                       name = "Treatments") +
#     xlab("Treatments") +
#     ylab(y_label) +
#     theme_tufte() +
#     theme(
#       legend.position = "none",
#       text = element_text(color = "white"),  # All text
#       axis.text = element_text(color = "white"),  # Axis numbers
#       axis.title = element_text(color = "white"),  # Axis labels
#       axis.ticks = element_line(color = "white"),  # Axis ticks
#       legend.text = element_text(color = "white"),  # Legend text
#       legend.title = element_text(color = "white"),
#       plot.title = element_text(hjust = 0.5))
#   
#   # Save with dynamic filename
#   ggsave(paste0("olivia/finalplots/", file_suffix, ".png"))
# }
# 
# 
# # Function to process all specified columns
# process_columns <- function(data, columns_to_plot)
#   
#   # Loop through each column specification
#   for (col_spec in columns_to_plot) {
#     col_name <- col_spec$column
#     y_label <- col_spec$label
#     file_suffix <- col_spec$file_suffix
#     
#     # Create the plot
#     create_boxplot(data = data, 
#                    y_var = !!sym(col_name), 
#                    y_label = y_label, 
#                    file_suffix = file_suffix)
#   }
# 
# #chemical analyses
# all_data <- read_csv("olivia/Harvard Master2.csv")
# all_data$treatment <- stringr::str_extract(all_data$Leaf.Number, "^..")
# colnames(all_data)
# 
# # Define which columns to plot and their labels
# columns_to_plot <- list(
#   list(column = "Flavonoids", label = "Flavonoids", file_suffix = "Flavonoids"),
#   list(column = "Phenolics", label = "Phenolics", file_suffix = "Phenolics"),
#   list(column = "Saponins", label = "Saponins", file_suffix = "Saponins"),
#   list(column = "Terpenoids", label = "Terpenoids", file_suffix = "Terpenoids"),
#   list(column = "Tannins", label = "Tannins", file_suffix = "Tannins")
#   # Add more columns as needed
# )
# 
# # Run the plotting process
# process_columns(all_data, columns_to_plot)
# 
# 
# 
# #physical tests
# all_data_physical <- read_csv("olivia/physicalData.csv") #rename based on file name
# all_data_physical$treatment <- stringr::str_extract(all_data_physical$"Leaf Number", "^..")
# colnames(all_data_physical)
# 
# # Define which columns to plot and their labels
# columns_to_plot_physical <- list(
#   list(column = "Toughness (N)", label = "Toughness (N)", file_suffix = "Toughness"),
#   list(column = "Thickness (mm)", label = "Thickness (mm)", file_suffix = "Thickness"),
#   list(column = "Mass (mg)", label = "Mass (mg)", file_suffix = "Mass"),
#   list(column = "Dried Mass (mg)", label = "Dry Mass (mg)", file_suffix = "DryMass"),
#   list(column = "Caterpillar Mass Change", label = "Change in Mass (mg)", file_suffix = "CaterpillarMass")
# )
# 
# # Run the plotting process
# process_columns(all_data_physical, columns_to_plot_physical)

