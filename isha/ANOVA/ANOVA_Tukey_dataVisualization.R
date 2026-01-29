# the purpose of this script is to run anova tests and tukey tests for whole plant and leaf traits
# and organize nicely onto a master datasheet for each
# and then create box plots that show the signifcance letters from the tukey tests

# clear environment
rm(list = ls())

# load libraries
library(broom)
library(agricolae)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)

# ANOVA and Tukey Tests ----

## Whole plant traits ----

plant_data <- read.csv("isha/Harvard Master.csv", header = TRUE)
anova_results <- list()
tukey_results <- list()
detailed_results <- list()

columns_to_analyze <- c("Plant.height..ft.", "Nitrogen.content",
                        "C.N.ratio", "Flavonoids", "Phenolics", 
                        "Terpenoids", "Tannins", "Average.water.content....", "SLA..mm2.mg.")

alpha_level <- 0.05

for (col in columns_to_analyze) {
  formula <- as.formula(paste(col, "~ Type"))

  anova_model <- aov(formula, data = plant_data)
  anova_results[[col]] <- anova_model

  anova_tidy <- broom::tidy(anova_model)
  anova_tidy$eta_sq <- anova_tidy$sumsq / sum(anova_tidy$sumsq)
  anova_tidy$Variable <- col  # Add variable name to each result
  detailed_results[[col]] <- anova_tidy

  p_value <- anova_tidy$p.value[anova_tidy$term == "Type"]
  
  if (!is.na(p_value) && p_value < alpha_level) {
    tukey <- HSD.test(anova_model, "Type", group = TRUE, console = FALSE)
    
    tukey_groups <- data.frame(
      Variable = col,
      Type = rownames(tukey$groups),
      Mean = tukey$groups[, 1],
      Groups = tukey$groups[, 2],
      stringsAsFactors = FALSE
    )
    tukey_results[[col]] <- tukey_groups
    
  } else {
    tukey_results[[col]] <- data.frame(
      Variable = col,
      Type = NA,
      Mean = NA,
      Groups = NA,
      Note = ifelse(is.na(p_value), "Error in ANOVA", "Not significant (p > 0.05)"),
      stringsAsFactors = FALSE
    )
  }
}


plant_data2 <- plant_data %>%
  mutate(isInv = ifelse(Type == "Invasive", 1, 0))


summary(lm(Plant.height..ft. ~ isInv, data = plant_data2))

detailed_combined <- bind_rows(detailed_results)

tukey_combined <- bind_rows(tukey_results) %>%
  arrange(Variable, Type)

write.csv(detailed_combined, "isha/ANOVA/whole_plant_anova_results.csv", row.names = FALSE)
write.csv(tukey_combined, "isha/ANOVA/whole_plant_tukey_results.csv", row.names = FALSE)


## Leaf traits ----

leaf_data <- read.csv("isha/Harvard MasterLeaf.csv", header = TRUE)

anova_results <- list()
tukey_results <- list()
detailed_results <- list()

leaf_columns_to_analyze <- c("Toughness..N.", "Thickness..mm.")

for (col in leaf_columns_to_analyze) {
  formula <- as.formula(paste(col, "~ Type"))
  
  anova_model <- aov(formula, data = leaf_data)
  anova_results[[col]] <- anova_model

  anova_tidy <- broom::tidy(anova_model)
  anova_tidy$eta_sq <- anova_tidy$sumsq / sum(anova_tidy$sumsq)
  anova_tidy$Variable <- col
  detailed_results[[col]] <- anova_tidy
  
  p_value <- anova_tidy$p.value[anova_tidy$term == "Type"]
  
  if (!is.na(p_value) && p_value < alpha_level) {
    tukey <- HSD.test(anova_model, "Type", group = TRUE, console = FALSE)
    
    tukey_groups <- data.frame(
      Variable = col,
      Type = rownames(tukey$groups),
      Mean = tukey$groups[, 1],
      Groups = tukey$groups[, 2],
      stringsAsFactors = FALSE
    )
    tukey_results[[col]] <- tukey_groups
    
  } else {
    tukey_results[[col]] <- data.frame(
      Variable = col,
      Type = NA,
      Mean = NA,
      Groups = NA,
      Note = ifelse(is.na(p_value), "Error in ANOVA", "Not significant (p > 0.05)"),
      stringsAsFactors = FALSE
    )
  }
}

detailed_combined <- bind_rows(detailed_results)

tukey_combined <- bind_rows(tukey_results) %>%
  arrange(Variable, Type)

write.csv(detailed_combined, "isha/ANOVA/leaf_anova_results.csv", row.names = FALSE)
write.csv(tukey_combined, "isha/ANOVA/leaf_tukey_results.csv", row.names = FALSE)




# Data visualization ----

whole_tukey <- read_csv("isha/ANOVA/whole_plant_tukey_results.csv")
leaf_tukey <- read_csv("isha/ANOVA/leaf_tukey_results.csv")

whole_tukey <- whole_tukey %>%
  mutate(Variable = case_when(
    Variable == "SLA..mm2.mg." ~ "SLA (mm2/mg)",
    Variable == "Average.water.content...." ~ "Average water content (%)",
    Variable == "Nitrogen.content" ~ "Nitrogen content",
    Variable == "C.N.ratio" ~ "C:N ratio",
    Variable == "Plant.height..ft." ~ "Plant height (ft)",
    TRUE ~ Variable
  ))

leaf_tukey <- leaf_tukey %>%
  mutate(Variable = case_when(
    Variable == "Toughness..N." ~ "Toughness (N)",
    Variable == "Thickness..mm." ~ "Thickness (mm)",
    TRUE ~ Variable
  ))

all_tukey <- bind_rows(
  whole_tukey %>% mutate(Source = "Whole"),
  leaf_tukey %>% mutate(Source = "Leaf")
)

create_boxplot <- function(data, y_var, y_label, file_suffix, tukey_data, plot_label = NULL) {
  y_var_str <- as_label(enquo(y_var))
  plot_letters <- tukey_data %>%
    filter(Variable == y_var_str)

  y_max <- max(data[[y_var_str]], na.rm = TRUE) * 1.1
  
  p <- ggplot(data = data, mapping = aes(x = Type, y = {{ y_var }})) +
    geom_boxplot(aes(fill = Type)) +
    geom_point() +
    scale_fill_manual(values = c("thistle", "olivedrab3", "lemonchiffon")) +
    labs(x = "Species Type", y = y_label) +
    theme_tufte(18) +
    theme(legend.position = "none",  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
    scale_x_discrete(labels = c(
      "Invasive" = "I",
      "Native" = "N",
      "Non-invasive exotic" = "NIE"
    ))

  if (nrow(plot_letters) > 0) {
    if (all(is.na(plot_letters$Groups))) {
      plot_letters$Groups <- "ns"
    }

    if (!all(is.na(plot_letters$Type))) {
      p <- p + geom_text(
        data = plot_letters,
        aes(x = Type, y = y_max, label = Groups),
        vjust = 1.5, 
        size = 7, 
        color = "black"
      )
    }
  }

  if (!is.null(plot_label)) {
    p <- p + 
      annotate("text", x = 0.5, y = Inf, label = plot_label, 
               hjust = 0, vjust = 2, size = 8, fontface = "bold")
  }
  
  # Save individual plot if file_suffix is provided
  if (!is.null(file_suffix)) {
    ggsave(paste0("isha/Plots/", file_suffix, ".png"), 
           plot = p, width = 7, height = 8)
  }
  
  return(p)
}


process_columns <- function(data, columns_to_plot, tukey_data) {
  for (col_spec in columns_to_plot) {
    col_name <- col_spec$column
    y_label <- col_spec$label
    file_suffix <- col_spec$file_suffix
    
    create_boxplot(
      data = data, 
      y_var = !!sym(col_name), 
      y_label = y_label, 
      file_suffix = file_suffix,
      tukey_data = tukey_data
    )
  }
}

create_four_plot_grid <- function(master_whole, master_leaf, whole_tukey, leaf_tukey) {
  plot1 <- create_boxplot(
    data = master_whole,
    y_var = `SLA (mm2/mg)`,
    y_label = "Specific Leaf Area (mm²/mg)",
    file_suffix = NULL,
    tukey_data = whole_tukey,
    plot_label = "A"
  )
  
  plot2 <- create_boxplot(
    data = master_whole,
    y_var = Tannins,
    y_label = "[Relative Tannins]",
    file_suffix = NULL,
    tukey_data = whole_tukey,
    plot_label = "B"
  )
  
  plot3 <- create_boxplot(
    data = master_leaf,
    y_var = `Toughness (N)`,
    y_label = "Leaf Toughness (N)",
    file_suffix = NULL,
    tukey_data = leaf_tukey,
    plot_label = "C"
  )
  
  plot4 <- create_boxplot(
    data = master_leaf,
    y_var = `Thickness (mm)`,
    y_label = "Leaf Thickness (mm)",
    file_suffix = NULL,
    tukey_data = leaf_tukey,
    plot_label = "D"
  )

  combined_plot <- (plot1 + plot2) / (plot3 + plot4) +
    plot_layout(guides = 'collect') &
    theme(legend.position = 'none')
  
  ggsave("isha/Plots/four_trait_grid.png", 
         plot = combined_plot, 
         width = 16,
         height = 16,
         dpi = 300)
  
  ggsave("isha/Plots/four_trait_grid.pdf", 
         plot = combined_plot, 
         width = 16,
         height = 16)
  
  return(combined_plot)
}

# Individual plots ----

master_whole <- read_csv("isha/Harvard Master.csv")
master_leaf <- read_csv("isha/Harvard MasterLeaf.csv")

columns_to_plot <- list(
  list(column = "Plant height (ft)", label = "Plant Height (ft)", file_suffix = "PlantHeight"),
  list(column = "Nitrogen content", label = "Nitrogen Content", file_suffix = "NitrogenContent"),
  list(column = "C:N ratio", label = "C:N Ratio", file_suffix = "CNRatio"),
  list(column = "Flavonoids", label = "Flavonoids", file_suffix = "Flavonoids"),
  list(column = "Phenolics", label = "Phenolics", file_suffix = "Phenolics"),
  list(column = "Terpenoids", label = "Terpenoids", file_suffix = "Terpenoids"),
  list(column = "Tannins", label = "Tannins", file_suffix = "Tannins"),
  list(column = "Average water content (%)", label = "Average Water Content (%)", file_suffix = "WaterContent"),
  list(column = "SLA (mm2/mg)", label = "Specific Leaf Area (mm2/mg)", file_suffix = "SpecificLeafArea")
)

columns_to_plot_leaf <- list(
  list(column = "Toughness (N)", label = "Toughness (N)", file_suffix = "Toughness"),
  list(column = "Thickness (mm)", label = "Thickness (mm)", file_suffix = "Thickness")
)

process_columns(master_whole, columns_to_plot, whole_tukey)
process_columns(master_leaf, columns_to_plot_leaf, leaf_tukey)


grid_plot <- create_four_plot_grid(master_whole, master_leaf, whole_tukey, leaf_tukey)

print(grid_plot)



