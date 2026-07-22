# the purpose of this script is to run t tests for whole plant and leaf traits
# and organize nicely onto a master datasheet for each
# and then create box plots

# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(ggplot2)

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

# Run t-tests
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

# Show only significant results
print(results[results$Significant == "Yes", ])

# Leaf traits ----

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

# Show only significant results
print(results[results$Significant == "Yes", ])


# Data visualization ----


# Read data
plant_data <- read.csv("isha/Harvard Master.csv", header = TRUE)

# Create status column
plant_data$Status <- ifelse(plant_data$Type == "Invasive", "Invasive", "Non-Invasive")

# List of traits
traits <- c("Plant.height..ft.", "Nitrogen.content", "C.N.ratio", 
            "Flavonoids", "Phenolics", "Terpenoids", "Tannins", 
            "Average.water.content....", "SLA..mm2.mg.")

# Make box plots
for (trait in traits) {
  p <- ggplot(plant_data, aes(x = Status, y = .data[[trait]], fill = Status)) +
    geom_boxplot()
  
  ggsave(paste0("isha/Plots/", trait, ".png"), p)
}

# LEAF TRAITS
leaf_data <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)
leaf_data$Status <- ifelse(leaf_data$Type == "Invasive", "Invasive", "Non-Invasive")

# Average by species
leaf_sum <- aggregate(cbind(Toughness..N., Thickness..mm.) ~ Species.Name + Status, 
                      data = leaf_data, FUN = mean, na.rm = TRUE)

leaf_traits <- c("Toughness..N.", "Thickness..mm.")

for (trait in leaf_traits) {
  p <- ggplot(leaf_sum, aes(x = Status, y = .data[[trait]], fill = Status)) +
    geom_boxplot()
  
  ggsave(paste0("isha/Plots/", trait, ".png"), p)
}









# whole_tukey <- read_csv("isha/ANOVA/whole_plant_tukey_results.csv")
# leaf_tukey <- read_csv("isha/ANOVA/leaf_tukey_results.csv")
# 
# whole_tukey <- whole_tukey %>%
#   mutate(Variable = case_when(
#     Variable == "SLA..mm2.mg." ~ "SLA (mm2/mg)",
#     Variable == "Average.water.content...." ~ "Average water content (%)",
#     Variable == "Nitrogen.content" ~ "Nitrogen content",
#     Variable == "C.N.ratio" ~ "C:N ratio",
#     Variable == "Plant.height..ft." ~ "Plant height (ft)",
#     TRUE ~ Variable
#   ))
# 
# leaf_tukey <- leaf_tukey %>%
#   mutate(Variable = case_when(
#     Variable == "Toughness..N." ~ "Toughness (N)",
#     Variable == "Thickness..mm." ~ "Thickness (mm)",
#     TRUE ~ Variable
#   ))
# 
# all_tukey <- bind_rows(
#   whole_tukey %>% mutate(Source = "Whole"),
#   leaf_tukey %>% mutate(Source = "Leaf")
# )
# 
# create_boxplot <- function(data, y_var, y_label, file_suffix, tukey_data, plot_label = NULL) {
#   y_var_str <- as_label(enquo(y_var))
#   plot_letters <- tukey_data %>%
#     filter(Variable == y_var_str)
# 
#   y_max <- max(data[[y_var_str]], na.rm = TRUE) * 1.1
#   
#   p <- ggplot(data = data, mapping = aes(x = Type, y = {{ y_var }})) +
#     geom_boxplot(aes(fill = Type)) +
#     geom_point() +
#     scale_fill_manual(values = c("thistle", "olivedrab3", "lemonchiffon")) +
#     labs(x = "Species Type", y = y_label) +
#     theme_tufte(18) +
#     theme(legend.position = "none",  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
#     scale_x_discrete(labels = c(
#       "Invasive" = "I",
#       "Native" = "N",
#       "Non-invasive exotic" = "NIE"
#     ))
# 
#   if (nrow(plot_letters) > 0) {
#     if (all(is.na(plot_letters$Groups))) {
#       plot_letters$Groups <- "ns"
#     }
# 
#     if (!all(is.na(plot_letters$Type))) {
#       p <- p + geom_text(
#         data = plot_letters,
#         aes(x = Type, y = y_max, label = Groups),
#         vjust = 1.5, 
#         size = 7, 
#         color = "black"
#       )
#     }
#   }
# 
#   if (!is.null(plot_label)) {
#     p <- p + 
#       annotate("text", x = 0.5, y = Inf, label = plot_label, 
#                hjust = 0, vjust = 2, size = 8, fontface = "bold")
#   }
#   
#   # Save individual plot if file_suffix is provided
#   if (!is.null(file_suffix)) {
#     ggsave(paste0("isha/Plots/", file_suffix, ".png"), 
#            plot = p, width = 7, height = 8)
#   }
#   
#   return(p)
# }
# 
# 
# process_columns <- function(data, columns_to_plot, tukey_data) {
#   for (col_spec in columns_to_plot) {
#     col_name <- col_spec$column
#     y_label <- col_spec$label
#     file_suffix <- col_spec$file_suffix
#     
#     create_boxplot(
#       data = data, 
#       y_var = !!sym(col_name), 
#       y_label = y_label, 
#       file_suffix = file_suffix,
#       tukey_data = tukey_data
#     )
#   }
# }
# 
# create_four_plot_grid <- function(master_whole, master_leaf, whole_tukey, leaf_tukey) {
#   plot1 <- create_boxplot(
#     data = master_whole,
#     y_var = `SLA (mm2/mg)`,
#     y_label = "Specific Leaf Area (mm²/mg)",
#     file_suffix = NULL,
#     tukey_data = whole_tukey,
#     plot_label = "A"
#   )
#   
#   plot2 <- create_boxplot(
#     data = master_whole,
#     y_var = Tannins,
#     y_label = "[Relative Tannins]",
#     file_suffix = NULL,
#     tukey_data = whole_tukey,
#     plot_label = "B"
#   )
#   
#   plot3 <- create_boxplot(
#     data = master_leaf,
#     y_var = `Toughness (N)`,
#     y_label = "Leaf Toughness (N)",
#     file_suffix = NULL,
#     tukey_data = leaf_tukey,
#     plot_label = "C"
#   )
#   
#   plot4 <- create_boxplot(
#     data = master_leaf,
#     y_var = `Thickness (mm)`,
#     y_label = "Leaf Thickness (mm)",
#     file_suffix = NULL,
#     tukey_data = leaf_tukey,
#     plot_label = "D"
#   )
# 
#   combined_plot <- (plot1 + plot2) / (plot3 + plot4) +
#     plot_layout(guides = 'collect') &
#     theme(legend.position = 'none')
#   
#   ggsave("isha/Plots/four_trait_grid.png", 
#          plot = combined_plot, 
#          width = 16,
#          height = 16,
#          dpi = 300)
#   
#   ggsave("isha/Plots/four_trait_grid.pdf", 
#          plot = combined_plot, 
#          width = 16,
#          height = 16)
#   
#   return(combined_plot)
# }
# 
# # Individual plots ----
# 
# master_whole <- read_csv("isha/Harvard Master.csv")
# master_leaf <- read_csv("isha/Harvard MasterLeaf.csv")
# 
# columns_to_plot <- list(
#   list(column = "Plant height (ft)", label = "Plant Height (ft)", file_suffix = "PlantHeight"),
#   list(column = "Nitrogen content", label = "Nitrogen Content", file_suffix = "NitrogenContent"),
#   list(column = "C:N ratio", label = "C:N Ratio", file_suffix = "CNRatio"),
#   list(column = "Flavonoids", label = "Flavonoids", file_suffix = "Flavonoids"),
#   list(column = "Phenolics", label = "Phenolics", file_suffix = "Phenolics"),
#   list(column = "Terpenoids", label = "Terpenoids", file_suffix = "Terpenoids"),
#   list(column = "Tannins", label = "Tannins", file_suffix = "Tannins"),
#   list(column = "Average water content (%)", label = "Average Water Content (%)", file_suffix = "WaterContent"),
#   list(column = "SLA (mm2/mg)", label = "Specific Leaf Area (mm2/mg)", file_suffix = "SpecificLeafArea")
# )
# 
# columns_to_plot_leaf <- list(
#   list(column = "Toughness (N)", label = "Toughness (N)", file_suffix = "Toughness"),
#   list(column = "Thickness (mm)", label = "Thickness (mm)", file_suffix = "Thickness")
# )
# 
# process_columns(master_whole, columns_to_plot, whole_tukey)
# process_columns(master_leaf, columns_to_plot_leaf, leaf_tukey)
# 
# 
# grid_plot <- create_four_plot_grid(master_whole, master_leaf, whole_tukey, leaf_tukey)
# 
# print(grid_plot)



