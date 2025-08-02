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
  ggplot(data = data, mapping = aes(x = treatment, y = {{ y_var }})) +
    geom_boxplot(aes(fill = treatment)) +
    geom_point() +
    scale_fill_manual(values = c("lightsalmon","peachpuff","darkolivegreen4","darkolivegreen3"), 
                      name = "Treatments") +
    scale_x_discrete(label=c("AA Absent","AA Present","LF Absent", "LF Present"))+
    xlab("Treatments") +
    ylab(y_label) +
    theme_tufte() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  # Save with dynamic filename
  ggsave(paste0("olivia/finalplots/", file_suffix, ".png"))
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

#chemical analyses
all_data <- read_csv("olivia/Harvard Master2.csv")
all_data$treatment <- stringr::str_extract(all_data$Leaf.Number, "^..")

# Define which columns to plot and their labels
columns_to_plot <- list(
  list(column = "Flavonoids", label = "Flavonoids", file_suffix = "Flavonoids"),
  list(column = "Phenolics", label = "Phenolics", file_suffix = "Phenolics"),
  list(column = "Saponins", label = "Saponins", file_suffix = "Saponins"),
  list(column = "Terpenoids", label = "Terpenoids", file_suffix = "Terpenoids"),
  list(column = "Tannins", label = "Tannins", file_suffix = "Tannins")
  # Add more columns as needed
)

# Run the plotting process
process_columns(all_data, columns_to_plot)



#physical data



# Define your plotting function
create_boxplot <- function(data, y_var, y_label, file_suffix) {
  ggplot(data = data, mapping = aes(x = treatment, y = {{ y_var }})) +
    geom_boxplot(aes(fill = treatment)) +
    geom_point() +
    scale_fill_manual(values = c("lightsalmon","peachpuff","darkolivegreen4","darkolivegreen3"), 
                      name = "Treatments") +
    scale_x_discrete(label=c("AA Absent","AA Present","LF Absent", "LF Present"))+
    xlab("Treatments") +
    ylab(y_label) +
    theme_tufte() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  # Save with dynamic filename
  ggsave(paste0("olivia/finalplots/", file_suffix, ".png"))
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




all_data_p <- read_csv("olivia/physicaldata.csv")
colnames(all_data_p)
all_data_p$treatment <- stringr::str_extract(all_data_p$Leaf.Number, "^..")

# Define which columns to plot and their labels
columns_to_plot_p <- list(
  list(column = "Toughness (N)", label = "Toughness (N)", file_suffix = "Toughness (N)"),
  list(column = "Thickness (mm)", label = "Thickness (mm), file_suffix = Thickness (mm)"),
  list(column = "Mass (mg)", label = "Mass (mg)", file_suffix = "Mass (mg)"),
  list(column = "Dried Mass (mg)", label = "Dried Mass (mg)", file_suffix = "Dried Mass (mg)"),
  list(column = "Caterpillar Mass Before (mg)", label = "Caterpillar Mass Before (mg)", file_suffix = "Caterpillar Mass Before (mg)"),
  list(column = "Caterpillar Mass After (mg)", label = "Caterpillar Mass After (mg)", file_suffix = "Caterpillar Mass After (mg)"),
  # Add more columns as needed
)
# Run the plotting process
process_columns(all_data_p, columns_to_plot_p)


