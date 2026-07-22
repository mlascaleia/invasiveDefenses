# the purpose of this script is to compare different groupings for this study
# and run an nmds and permanova for the best model
# and then create pngs of the plots

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


# comparing nmds groupings ----
grouping_var <- my_data$Type

groupings <- list(
  "All 3 groups" = grouping_var,
  "Invasive vs (NIE+Native)" = ifelse(grouping_var == "Invasive", "Invasive", "Non-invasive"),
  "NIE vs (Native+Invasive)" = ifelse(grouping_var == "Non-invasive exotic", "NIE", "Others"),
  "Native vs (Invasive+NIE)" = ifelse(grouping_var == "Native", "Native", "Others"))

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

results


# nmds of invasive vs non-invasive --------

master <- read.csv("isha/Harvard Master.csv")

my_data <- read.csv("isha/NMDS data.csv", header = TRUE, row.names = 2)
my_data <- merge(master, my_data, all.x = TRUE)

# Create a new grouping variable with 2 groups
my_data$Group <- ifelse(my_data$Type == "Invasive", "Invasive", "Non-invasive")

# Convert to factor
grouping_var_2 <- as.factor(my_data$Group)

# Prepare selected data
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



# run NMDS ----
nmds_result <- metaMDS(selected_data, 
                       distance = "bray",  # or "jaccard" for presence/absence
                       k = 2,             # number of dimensions
                       try = 20,          # number of random starts
                       trymax = 100,       # maximum number of iterations
                       autotransform = TRUE)  # automatic data transformation

# stress value
nmds_result$stress


# make plot ----
env_vectors <- envfit(nmds_result, selected_data, permutations = 999, na.rm = TRUE)

# Update colors and shapes for 2 groups
my_point_colors <- c("plum", "olivedrab4")  # Invasive, Non-invasive
my_ellipse_colors <- c("thistle", "olivedrab3")  # Invasive, Non-invasive
my_point_shapes <- c(16, 17)  # Different shapes for each group

png("isha/Plots/NMDS.png", 
    width = 8, 
    height = 6, 
    units = "in", 
    res = 300,
    bg = "transparent")  

par(family = "serif", bg = NA)
plot(nmds_result, type = "n", main = "NMDS Plot: Invasive vs Non-Invasive Species", bg = NA)

points(nmds_result, display = "sites", 
       pch = my_point_shapes[as.numeric(grouping_var_2)],
       col = my_point_colors[as.numeric(grouping_var_2)],
       cex = 1.5)

# add ellipses
ordiellipse(
  nmds_result,
  groups = grouping_var_2,
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
       legend = levels(grouping_var_2),
       pch = my_point_shapes[1:length(levels(grouping_var_2))],
       col = my_point_colors[1:length(levels(grouping_var_2))],
       pt.bg = my_point_colors[1:length(levels(grouping_var_2))],
       title = "Species Type",
       xpd = TRUE
)

dev.off()

# permanova ----
adonis_result <- adonis2(selected_data ~ Group, data = my_data, method = "bray", permutations = 999)
print(adonis_result)






