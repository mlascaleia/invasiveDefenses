# the purpose of this script is to run nmds and pca for all my data
# and create pngs of the data

# clear environment
rm(list = ls())



#load packages
library(dplyr)
library(vegan)
library(ggplot2)
library(ggfortify)


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



png("isha/Plots/nmds_plot.png", width = 8, height = 6, units = "in", res = 300)


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



## PCA Analysis -----------------------------------------------------------

# 1. Scale the data (important for PCA with different measurement units)
scaled_data <- scale(selected_data)

# 2. Run PCA
pca_result <- prcomp(scaled_data)

# 3. View summary
summary(pca_result)

# 4. View loadings (variable contributions)
print(pca_result$rotation)

# 5. View eigenvalues (variance explained)
pca_eigenvalues <- pca_result$sdev^2
print(pca_eigenvalues)

## PCA Visualization ------------------------------------------------------

# Scree plot (variance explained by each PC)
png("isha/Plots/pca_scree_plot.png", width = 8, height = 6, units = "in", res = 300)
plot(pca_eigenvalues, type = "b", pch = 19, 
     xlab = "Principal Component", ylab = "Eigenvalue",
     main = "Scree Plot of PCA")
dev.off()

# Biplot with groups
png("isha/Plots/pca_biplot.png", width = 8, height = 6, units = "in", res = 300)
biplot(pca_result, cex = c(0.7, 0.9), 
       col = c("gray50", "black"),
       xlab = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       ylab = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"))
dev.off()

# Enhanced ggplot version with ellipses
png("isha/Plots/pca_ggplot.png", width = 8, height = 6, units = "in", res = 300)
autoplot(pca_result, data = my_data, colour = 'Type',
         loadings = TRUE, loadings.label = TRUE,
         loadings.label.size = 3, loadings.label.colour = 'black',
         frame = TRUE, frame.type = 'norm') +
  scale_color_manual(values = my_point_colors) +
  theme_minimal() +
  ggtitle("PCA of Plant Traits by Species Type")
dev.off()









