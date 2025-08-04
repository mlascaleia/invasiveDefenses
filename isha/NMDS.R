


#load packages
library(dplyr)
library(vegan)

# Read your CSV file
my_data <- read.csv("isha/NMDS data.csv", header = TRUE, row.names = 2)
selected_data <- my_data %>%
  mutate(Growth.Form = as.numeric(as.factor(my_data$Growth.Form))) %>%
  select(Growth.Form, Plant.height..ft., Nitrogen.content, C.N.ratio, Flavonoids, Phenolics, Terpenoids, Tannins, Average.toughness..N., Average.thickness..mm., Average.water.content....)

grouping_var <- as.factor(my_data$Type)

# Run NMDS on your subsetted data
nmds_result <- metaMDS(selected_data, 
                       distance = "bray",  # or "jaccard" for presence/absence
                       k = 2,             # number of dimensions
                       try = 20,          # number of random starts
                       trymax = 100,       # maximum number of iterations
                       autotransform = TRUE)  # automatic data transformation

# Check stress value (should be <0.2 ideally)
nmds_result$stress

# Define colors for groups (replace with your preferred colors)
my_point_colors <- c("thistle4", "olivedrab4", "lemonchiffon4")

my_ellipse_colors <- c("thistle", "olivedrab3", "lemonchiffon")



plot(nmds_result, type = "n", main = "NMDS Plot with Group Ellipses")

# Color points using custom palette
points(nmds_result, display = "sites", pch = 16, 
       col = my_point_colors[as.numeric(grouping_var)],  # Map colors to groups
       cex = 0.7)  # Adjust point size

# Add ellipses with custom colors
ordiellipse(
  nmds_result, 
  groups = grouping_var,
  kind = "sd",
  draw = "polygon",
  col = my_ellipse_colors,  # Use predefined colors
  border = "black",         # Ellipse border color
  lwd = 1,                 # Border line width
  alpha = 100              # Transparency (0-255)
)

# Add legend
legend(x = "topright",           # Position (try "topright", "bottomleft", etc.)
  legend = levels(grouping_var),
  fill = my_point_colors[1:length(levels(grouping_var))],
  col = my_point_colors[1:length(levels(grouping_var))],
  title = "Species Type",
  xpd = TRUE            # Allow plotting outside main plot area
)














