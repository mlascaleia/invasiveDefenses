# the purpose of this script is to run t tests for whole plant and leaf traits
# and organize nicely onto a master datasheet for each
# and then create box plots

# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)



# Read data
plant_data <- read.csv("isha/Harvard Master.csv", header = TRUE)

# Create binary variable: 1 = Invasive, 0 = Non-invasive (native + non-invasive exotic)
plant_data$isInvasive <- ifelse(plant_data$Type == "Invasive", 1, 0)

# List of traits to analyze
traits <- c("Plant.height..ft.", "Nitrogen.content", "C.N.ratio", 
            "Flavonoids", "Phenolics", "Terpenoids", "Tannins", 
            "Average.water.content....", "SLA..mm2.mg.")

# Create empty dataframe for results
results <- data.frame()

# Whole plant traits t-tests ----
for (trait in traits) {
  # T-test using binary variable
  test <- t.test(plant_data[[trait]] ~ plant_data$isInvasive)
  
  # Get means
  means <- tapply(plant_data[[trait]], plant_data$isInvasive, mean, na.rm = TRUE)
  
  # Store results
  results <- rbind(results, data.frame(
    Trait = trait,
    Mean_Invasive = means["1"],
    Mean_NonInvasive = means["0"],
    Mean_Difference = means["1"] - means["0"],
    t_statistic = test$statistic,
    df = test$parameter,
    p_value = test$p.value,
    Significant = ifelse(test$p.value < 0.05, "Yes", "No")
  ))
}

# View results
print(results)

# Save results
write.csv(results, "isha/T-Tests/t_test_whole_results.csv", row.names = FALSE)



# Leaf traits t-tests ----

leaf_data <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)

# Create binary variable: 1 = Invasive, 0 = Non-invasive (native + non-invasive exotic)
leaf_data$isInvasive <- ifelse(leaf_data$Type == "Invasive", 1, 0)

#### Eliminate pseudoreplication by averaging per species ####
leaf_data_sum <- leaf_data %>%
  group_by(Species.Name, Type, isInvasive) %>%
  summarise(
    Toughness..N. = mean(Toughness..N., na.rm = TRUE),
    Thickness..mm. = mean(Thickness..mm., na.rm = TRUE)
  )

# Traits to analyze
leaf_traits <- c("Toughness..N.", "Thickness..mm.")

# Create empty dataframe for results
results <- data.frame()

# Run t-tests using the averaged data
for (trait in leaf_traits) {
  # T-test using binary variable
  test <- t.test(leaf_data_sum[[trait]] ~ leaf_data_sum$isInvasive)
  
  # Get means
  means <- tapply(leaf_data_sum[[trait]], leaf_data_sum$isInvasive, mean, na.rm = TRUE)
  
  # Store results
  results <- rbind(results, data.frame(
    Trait = trait,
    Mean_Invasive = means["1"],
    Mean_NonInvasive = means["0"],
    Mean_Difference = means["1"] - means["0"],
    t_statistic = test$statistic,
    df = test$parameter,
    p_value = test$p.value,
    Significant = ifelse(test$p.value < 0.05, "Yes", "No")
  ))
}

# View results
print(results)

# Save results (directly in isha folder, not in subfolder)
write.csv(results, "isha/T-Tests/t_test_leaf_results.csv", row.names = FALSE)





# Data visualization ----

# Read data
plant_data <- read.csv("isha/Harvard Master.csv", header = TRUE)

# Create status column
plant_data$Status <- ifelse(plant_data$Type == "Invasive", "Invasive", "Non-Invasive")

# List of traits
traits <- c("Plant.height..ft.", "Nitrogen.content", "C.N.ratio", 
            "Flavonoids", "Phenolics", "Terpenoids", "Tannins", 
            "Average.water.content....", "SLA..mm2.mg.")

# Create a list of expressions for all trait labels
trait_labels_exp <- list(
  "Plant.height..ft." = "Plant Height (ft)",
  "Nitrogen.content" = "Nitrogen Content",
  "C.N.ratio" = "C:N Ratio",
  "Flavonoids" = "Flavonoids",
  "Phenolics" = "Phenolics",
  "Terpenoids" = "Terpenoids",
  "Tannins" = "Tannins",
  "Average.water.content...." = "Average Water Content (%)",
  "SLA..mm2.mg." = expression(SLA~(mm^2/mg))
)

# Define colors
invasive_color <- "thistle"  # Purple
noninvasive_color <- "olivedrab3"  # Sea green

# Make box plots
for (i in 1:length(traits)) {
  trait <- traits[i]
  
  p <- ggplot() +
    # Boxplot layer using plant_data
    geom_boxplot(data = plant_data, 
                 aes(x = Status, y = .data[[trait]], fill = Status), 
                 width = 0.7, alpha = 0.7, outlier.shape = NA) +
    scale_fill_manual(values = c("Invasive" = invasive_color, "Non-Invasive" = noninvasive_color)) +
    theme_tufte() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "none"
    ) +
    labs(
      x = "Species Guild",
      y = trait_labels_exp[[trait]]
    )
  
  # Save plot
  ggsave(
    paste0("isha/Plots/", trait, ".png"), 
    p,
    width = 6,
    height = 5,
    dpi = 300
  )
}

# LEAF TRAITS
leaf_data <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)
leaf_data$Status <- ifelse(leaf_data$Type == "Invasive", "Invasive", "Non-Invasive")

# Average by species
leaf_sum <- aggregate(cbind(Toughness..N., Thickness..mm.) ~ Species.Name + Status, 
                      data = leaf_data, FUN = mean, na.rm = TRUE)

leaf_traits <- c("Toughness..N.", "Thickness..mm.")

# Create a list of expressions for leaf trait labels
leaf_labels_exp <- list(
  "Toughness..N." = "Toughness (N)",
  "Thickness..mm." = "Thickness (mm)"
)

# Create individual plots for the grid
plot_list <- list()

for (i in 1:length(leaf_traits)) {
  trait <- leaf_traits[i]
  
  p <- ggplot() +
    # Boxplot layer using leaf_sum
    geom_boxplot(data = leaf_sum, 
                 aes(x = Status, y = .data[[trait]], fill = Status), 
                 width = 0.7, alpha = 0.7, outlier.shape = NA) +
    scale_fill_manual(values = c("Invasive" = invasive_color, "Non-Invasive" = noninvasive_color),
                      name = "Species Guild") +
    theme_tufte() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "bottom"  # Put legend at bottom for the grid
    ) +
    labs(
      x = "Species Guild",
      y = leaf_labels_exp[[trait]]
    ) +
    annotate("text", x = -Inf, y = Inf, label = ifelse(trait == "Thickness..mm.", "A", "B"), 
             hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold")
  
  # Save individual plot
  ggsave(
    paste0("isha/Plots/", trait, ".png"), 
    p,
    width = 6,
    height = 5,
    dpi = 300
  )
  
  # Store in list for grid
  plot_list[[trait]] <- p
}

# Create grid with Thickness and SLA -----
# First, get the SLA plot from the plant_data loop
sla_plot <- ggplot() +
  geom_boxplot(data = plant_data, 
               aes(x = Status, y = .data[["SLA..mm2.mg."]], fill = Status), 
               width = 0.7, alpha = 0.7, outlier.shape = NA) +
  scale_fill_manual(values = c("Invasive" = invasive_color, "Non-Invasive" = noninvasive_color),
                    name = "Species Guild") +
  theme_tufte() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    x = "Species Guild",
    y = expression(SLA~(mm^2/mg))
  ) +
  annotate("text", x = -Inf, y = Inf, label = "B", 
           hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold")

# Extract the legend from one of the plots
get_legend <- function(p) {
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Get legend from the thickness plot
legend <- get_legend(plot_list[["Thickness..mm."]])

# Remove legends from individual plots
plot_list[["Thickness..mm."]] <- plot_list[["Thickness..mm."]] + theme(legend.position = "none")
sla_plot_no_legend <- sla_plot + theme(legend.position = "none")

# Arrange plots in a grid with shared legend
grid_plot <- grid.arrange(
  arrangeGrob(
    plot_list[["Thickness..mm."]], 
    sla_plot_no_legend, 
    ncol = 2,
    top = textGrob("Leaf Traits by Species Guild", 
                   gp = gpar(fontsize = 18, fontface = "bold"))
  ),
  legend,
  nrow = 2,
  heights = c(10, 1)
)

# Save the grid plot
ggsave(
  "isha/Plots/Thickness_SLA_Grid.png", 
  grid_plot,
  width = 12,
  height = 6,
  dpi = 300
)

