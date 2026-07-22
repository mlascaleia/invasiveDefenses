# the purpose of this script is to run nmds and pca for all my data
# and create pngs of the data

# clear environment
rm(list = ls())


#load packages
library(dplyr)
library(vegan)
library(ggplot2)
library(ggfortify)

master <- read.csv("isha/Harvard Master.csv")

my_data <- read.csv("isha/NMDS data.csv", header = TRUE, row.names = 2)
my_data <- merge(master, my_data, all.x = TRUE)
selected_data <- my_data %>%
  mutate(Growth.Form = as.numeric(as.factor(my_data$Growth.Form))) %>%
  select(
    GrowthForm = Growth.Form,
    PlantHeight = Plant.height..ft.,
    Nitrogen = Nitrogen.content,
    CarbonNitrogenRatio = C.N.ratio,
    Flavonoids = Flavonoids,
    Phenolics = Phenolics,
    Terpenoids = Terpenoids,
    Tannins = Tannins,
    Toughness = Average.toughness..N.,
    Thickness = Average.thickness..mm.,
    WaterContent = Average.water.content....,
    SpecificLeafArea = SLA..mm2.mg.
    
  )
grouping_var <- as.factor(my_data$Type)

# run NMDS
nmds_result <- metaMDS(selected_data, 
                       distance = "bray",  # or "jaccard" for presence/absence
                       k = 2,             # number of dimensions
                       try = 20,          # number of random starts
                       trymax = 100,       # maximum number of iterations
                       autotransform = TRUE)  # automatic data transformation

# stress value
nmds_result$stress

# michael trying a few other NMDS things

# run NMDS
nmds_result2 <- metaMDS(selected_data, 
                       distance = "bray",  # or "jaccard" for presence/absence
                       k = 3,             # number of dimensions
                       try = 20,          # number of random starts
                       trymax = 100,       # maximum number of iterations
                       autotransform = TRUE)  # automatic data transformation

# stress value
nmds_result2$stress


# make plot ####

env_vectors <- envfit(nmds_result, selected_data, permutations = 999, na.rm = TRUE)


my_point_colors <- c("thistle3", "olivedrab4", "gold")
my_ellipse_colors <- c("thistle", "olivedrab3", "lemonchiffon")
my_point_shapes <- c(16, 17, 15)

png("isha/Plots/NMDS_plot.png", 
    width = 10, 
    height = 6, 
    units = "in", 
    res = 300,
    bg = "transparent")  

par(family = "serif", bg = NA)
plot(nmds_result, type = "n", main = "NMDS Plot of Species Type Data", bg = NA)


points(nmds_result, display = "sites", 
       pch = my_point_shapes[as.numeric(grouping_var)],
       col = my_point_colors[as.numeric(grouping_var)],
       cex = 1.5)

# add ellipses
ordiellipse(
  nmds_result,
  groups = grouping_var,
  kind = "sd",
  draw = "polygon",
  col = my_ellipse_colors,
  border = "black",       
  lwd = 1,              
  alpha = 100             
)

# add environmental vectors
plot(env_vectors, col = "black", lwd = 2, cex = 0.7)


# add legend
legend(x = "topright",
       legend = levels(grouping_var),
       pch = my_point_shapes[1:length(levels(grouping_var))],
       col = my_point_colors[1:length(levels(grouping_var))],
       pt.bg = my_point_colors[1:length(levels(grouping_var))],
       title = "Species Type",
       xpd = TRUE
)


dev.off()

# Clear environment
rm(list = ls())

# Load packages
library(dplyr)
library(vegan)
library(ggplot2)

# Read data
master <- read.csv("isha/Harvard Master.csv")
my_data <- read.csv("isha/NMDS data.csv", header = TRUE, row.names = 2)
my_data <- merge(master, my_data, all.x = TRUE)

# Prepare data
selected_data <- my_data %>%
  mutate(Growth.Form = as.numeric(as.factor(my_data$Growth.Form))) %>%
  select(
    GrowthForm = Growth.Form,
    PlantHeight = Plant.height..ft.,
    Nitrogen = Nitrogen.content,
    CarbonNitrogenRatio = C.N.ratio,
    Flavonoids = Flavonoids,
    Phenolics = Phenolics,
    Terpenoids = Terpenoids,
    Tannins = Tannins,
    Toughness = Average.toughness..N.,
    Thickness = Average.thickness..mm.,
    WaterContent = Average.water.content....,
    SpecificLeafArea = SLA..mm2.mg.
  )

grouping_var <- my_data$Type

# Define groupings to test
groupings <- list(
  "All 3 groups" = grouping_var,
  "Invasive vs (NIE+Native)" = ifelse(grouping_var == "Invasive", "Invasive", "Non-invasive"),
  "NIE vs (Native+Invasive)" = ifelse(grouping_var == "Non-invasive exotic", "NIE", "Others"),
  "Native vs (Invasive+NIE)" = ifelse(grouping_var == "Native", "Native", "Others")
)

# Run PERMANOVA for each grouping
results <- data.frame()

for (i in 1:length(groupings)) {
  group_name <- names(groupings)[i]
  group_vector <- groupings[[i]]
  
  # Run PERMANOVA
  result <- adonis2(selected_data ~ group_vector, method = "bray")
  
  # Store results
  results <- rbind(results, data.frame(
    Grouping = group_name,
    R2 = result$R2[1],
    P_value = result$`Pr(>F)`[1]
  ))
}

# Print results
print(results)

# Find best (highest R2)
best <- results[which.max(results$R2), ]
cat("\nBest grouping:", best$Grouping, "with R2 =", round(best$R2, 4))

# Save
write.csv(results, "isha/permanova_results.csv", row.names = FALSE)

# Simple plot
p <- ggplot(results, aes(x = Grouping, y = R2, fill = Grouping)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(R2, 4)), vjust = -0.5) +
  labs(title = "PERMANOVA Results") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

dir.create("isha/Plots", showWarnings = FALSE)
ggsave("isha/Plots/permanova_results.png", p, width = 8, height = 5)

cat("\nDone!")





----------------------------------------------------------------------

  # # try PCA ####
# 
# # drop categorical variable(s)
# pca_data <- selected_data %>%
#   select(-GrowthForm)
# 
# # run PCA on scaled correlation matrix
# pca_result <- prcomp(pca_data, center = TRUE, scale = TRUE)
# summary(pca_result)
# screeplot(pca_result, type = "lines", main = "Scree Plot")
# 
# 
# # put back in species info
# pca_result
# 
# # loadings (variable contributions to each PC)
# pca_result$rotation
# 
# # scores (sample positions in PC space)
# pca_result$x
# pca_scores <- as.data.frame(pca_result$x)
# pca_scores$Species.Name <- my_data$Species.Name
# pca_scores$Type <- grouping_var
# 
# # make pca plot ####
# 
# # extract loadings for biplot arrows
# pca_loadings <- as.data.frame(pca_result$rotation[, 1:2])
# pca_loadings$Variable <- rownames(pca_loadings)
# 
# # scale loadings for visual clarity (adjust multiplier as needed)
# loading_scale <- 3
# 
# # variance explained for axis labels
# var_explained <- summary(pca_result)$importance[2, 1:2] * 100
# 
# ggplot() +
#   # sample points, colored by Type
#   geom_point(data = pca_scores,
#              aes(x = PC1, y = PC2, color = Type),
#              size = 3, alpha = 0.8) +
#   # species name labels
#   geom_text(data = pca_scores,
#             aes(x = PC1, y = PC2, label = Species.Name, color = Type),
#             size = 2.8, vjust = -0.8, hjust = 0.5, show.legend = FALSE) +
#   # loading arrows
#   geom_segment(data = pca_loadings,
#                aes(x = 0, y = 0,
#                    xend = PC1 * loading_scale,
#                    yend = PC2 * loading_scale),
#                arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#                color = "gray40", linewidth = 0.5, alpha = 0.7) +
#   # loading labels
#   geom_text(data = pca_loadings,
#             aes(x = PC1 * loading_scale * 1.15,
#                 y = PC2 * loading_scale * 1.15,
#                 label = Variable),
#             size = 2.5, color = "gray30") +
#   # axis labels with variance explained
#   labs(x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
#        y = paste0("PC2 (", round(var_explained[2], 1), "%)"),
#        color = "Type") +
#   # origin crosshairs
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.4) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.4) +
#   theme_classic() +
#   theme(legend.position = "right",
#         axis.title = element_text(size = 11),
#         axis.text = element_text(size = 9),
#         legend.title = element_text(size = 10),
#         legend.text = element_text(size = 9))
# 
  
  
# # PCA Analysis ----
# 
# # 1. Scale the data (important for PCA with different measurement units)
# scaled_data <- scale(selected_data)
# 
# # 2. Run PCA
# pca_result <- prcomp(scaled_data)
# 
# # 3. View summary
# summary(pca_result)
# 
# # 4. View loadings (variable contributions)
# print(pca_result$rotation)
# 
# # 5. View eigenvalues (variance explained)
# pca_eigenvalues <- pca_result$sdev^2
# print(pca_eigenvalues)
# 
# 
# 
# # PCA Visualization ----
# 
# # Scree plot (variance explained by each PC)
# png("isha/Plots/pca_scree_plot.png", width = 8, height = 6, units = "in", res = 300)
# plot(pca_eigenvalues, type = "b", pch = 19, 
#      xlab = "Principal Component", ylab = "Eigenvalue",
#      main = "Scree Plot of PCA")
# dev.off()
# 
# # Biplot with groups
# png("isha/Plots/pca_biplot.png", width = 8, height = 6, units = "in", res = 300)
# biplot(pca_result, cex = c(0.7, 0.9), 
#        col = c("gray50", "black"),
#        xlab = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
#        ylab = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"))
# dev.off()
# 
# # Enhanced ggplot version with ellipses
# png("isha/Plots/pca_ggplot.png", width = 8, height = 6, units = "in", res = 300)
# autoplot(pca_result, data = my_data, colour = 'Type',
#          loadings = TRUE, loadings.label = TRUE,
#          loadings.label.size = 3, loadings.label.colour = 'black',
#          frame = TRUE, frame.type = 'norm') +
#   scale_color_manual(values = my_point_colors) +
#   theme_minimal() +
#   ggtitle("PCA of Plant Traits by Species Type")
# dev.off()









