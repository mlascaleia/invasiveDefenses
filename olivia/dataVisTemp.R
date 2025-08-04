#physical tests
all_data_physical <- read_csv("olivia/physicalData.csv") #rename based on file name
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