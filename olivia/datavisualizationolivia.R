# the purpose of this script is to create box plots of all the data
# and save them as png's in the folder "finalplots"



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
                 color = "black",               # Outline color
                 outlier.color = "black",       # Outlier points color
                 alpha = 0.7) +                 # Slightly transparent fill
    stat_boxplot(geom = "errorbar",             # Whiskers
                 color = "black",
                 width = 0.2) +
    geom_point(color = "black") +
    scale_fill_manual(values = c("darkgreen", "olivedrab2","darkmagenta", "plum"), 
                      name = "Treatments",
                      labels = c("Native Intact", "Native Removed", "Exotic Intact", "Exotic Removed")) +
    xlab("Treatments") +
    ylab(y_label) +
    theme_tufte(24) +
    theme(
      legend.position = "right",
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.text.x = element_blank(),
      axis.title = element_text(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.x = element_blank(),
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black"),
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










## grid of chemical defenses

# Load required packages
library(ggtext)
library(patchwork)

# Update your create_boxplot function
create_boxplot <- function(data, y_var, y_label) {
  # Convert treatment to factor with specified order
  data$treatment <- factor(data$treatment, levels = c("LP", "LA", "AP", "AA"))
  
  ggplot(data = data, mapping = aes(x = treatment, y = {{ y_var }})) +
    geom_boxplot(aes(fill = treatment),
                 color = "black",
                 outlier.color = "black",
                 alpha = 0.7) +
    stat_boxplot(geom = "errorbar",
                 color = "black",
                 width = 0.2) +
    geom_point(color = "black") +
    scale_fill_manual(
      values = c("darkgreen", "olivedrab2", "darkmagenta", "plum"), 
      name = NULL,  # Remove legend title
      labels = c(
        "<i>L. floridana</i> present", 
        "<i>L. floridana</i> absent", 
        "<i>A. altissima</i> present", 
        "<i>A. altissima</i> absent"
      )
    ) +
    xlab(NULL) +  # Remove x-axis label
    ylab(y_label) +
    theme_tufte(18, base_family = "Arial") +
    theme(
      legend.position = "none",
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title = element_text(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.x = element_blank(),
      plot.title = element_blank(),
      # Use element_markdown() for legend text to parse HTML tags
      legend.text = element_markdown(size = 12)
    )
}

# Create individual plots WITHOUT titles
flavonoids_plot <- create_boxplot(all_data, Flavonoids, "Relative [Flavonoids]")
phenolics_plot <- create_boxplot(all_data, Phenolics, "Relative [Phenolics]")
tannins_plot <- create_boxplot(all_data, Tannins, "Relative [Tannins]")
terpenoids_plot <- create_boxplot(all_data, Terpenoids, "Relative [Terpenoids]")

# OPTION 1: 2x2 grid
grid_4 <- (flavonoids_plot | phenolics_plot) / 
  (tannins_plot | terpenoids_plot)

# Add a common legend with ggtext parsing
grid_4_with_legend <- grid_4 + 
  plot_layout(guides = 'collect') &
  theme(
    legend.position = 'bottom',
    legend.text = element_markdown(size = 14)  # Use element_markdown for legend
  )

# Save the 2x2 grid
ggsave("olivia/finalplots/chemical_grid_2x2.png", 
       plot = grid_4_with_legend, 
       width = 12, height = 10, dpi = 300)







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

