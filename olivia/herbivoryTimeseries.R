library(tidyverse)
library(ggplot2)
library(ggthemes)

# Install if needed
# install.packages("ggtext")
library(ggtext)

# First, modify your data processing to create grouping variables
df <- read.csv("olivia/Scans.csv") %>%
  rename(`24` = Damage.at.24.HR....,
         `48` = Damage.at.48.HR....,
         `72` = Damage.at.72.HR....,
         `96` = Damage.at.96.HR....) %>%
  mutate(`0` = 100,
         treatment = str_extract(Leaf.Number, "^.."),
         # Create a grouping variable for the two graphs
         group = case_when(
           treatment %in% c("LP", "LA") ~ "<i>L. floridana</i>",
           treatment %in% c("AP", "AA") ~ "<i>A. altissima</i>",
           TRUE ~ "Other"
         ),
         # Clean treatment names for display - use HTML tags
         treatment_clean = case_when(
           treatment == "AP" ~ "<i>A. altissima</i> present",
           treatment == "AA" ~ "<i>A. altissima</i> absent", 
           treatment == "LP" ~ "<i>L. floridana</i> present",
           treatment == "LA" ~ "<i>L. floridana</i> absent"
         )) %>%
  select(id = Leaf.Number, group, treatment = treatment_clean, `0`, `24`, `48`, `72`, `96`) %>%
  pivot_longer(`0`:`96`, names_to = "time", values_to = "damage") %>%
  mutate(time = as.integer(time))

# Create summary statistics
df.sum <- df %>%
  group_by(time, group, treatment) %>%
  summarize(
    meanDamage = mean(damage, na.rm = TRUE),
    semin = meanDamage - sd(damage, na.rm = TRUE)/sqrt(n()),
    semax = meanDamage + sd(damage, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )

# Make figure with two facets
ggplot(data = df.sum, aes(x = time, y = meanDamage)) +
  geom_ribbon(aes(ymin = semin, ymax = semax, fill = treatment),
              alpha = .2) +
  geom_line(aes(color = treatment), linewidth = 1) +
  xlab("Time (Hours)") +
  ylab("Surface Area (%)") +
  scale_x_continuous(
    breaks = seq(0, 96, by = 24),
    limits = c(0, 96),
    labels = scales::comma
  ) +
  facet_wrap(
    ~group,
    nrow = 2
  ) +
  geom_text(
    data = data.frame(
      group = unique(df.sum$group),  # Your facet variable
      label = c("A", "B")              # Labels in the order of your facets
    ),
    aes(x = -Inf, y = -Inf, label = label),
    hjust = -1.1,    # Right-align with small offset
    vjust = -1,    # Top-align with small offset
    size = 5,       # Text size
    fontface = "bold",
    inherit.aes = FALSE) +
  theme_tufte(16, base_family = "Arial") +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    # Use element_markdown to parse HTML tags
    strip.text = element_markdown(size = 14),
    legend.text = element_markdown(size = 12),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
  ) +
  scale_fill_manual(
    values = c(
      "<i>A. altissima</i> absent" = "darkmagenta",
      "<i>A. altissima</i> present" = "orchid",
      "<i>L. floridana</i> absent" = "darkgreen",
      "<i>L. floridana</i> present" = "olivedrab2"
    )
  ) +
  scale_color_manual(
    values = c(
      "<i>A. altissima</i> absent" = "darkmagenta",
      "<i>A. altissima</i> present" = "orchid",
      "<i>L. floridana</i> absent" = "darkgreen",
      "<i>L. floridana</i> present" = "olivedrab2"
    )
  )

ggsave("olivia/totalLeafDamage.png")


## caterpillar mass vs leaf ate ----


# Read data
caterpillar <- read.csv("olivia/physicaldata.csv")
leaf <- read.csv("olivia/Harvard Master2.csv")

# Calculate leaf consumed and extract treatment from Leaf.Number
leaf <- leaf %>%
  mutate(
    leaf_consumed = X0.HR - X96.HR,
    treatment_code = str_extract(Leaf.Number, "^..")  # Extract first 2 characters
  )

# Combine the data
combined <- caterpillar %>%
  left_join(leaf, by = "Leaf.Number")

# Create treatment names with HTML tags for display
combined <- combined %>%
  mutate(
    treatment = case_when(
      treatment_code == "AP" ~ "<i>A. altissima</i> present",
      treatment_code == "AA" ~ "<i>A. altissima</i> absent", 
      treatment_code == "LP" ~ "<i>L. floridana</i> present",
      treatment_code == "LA" ~ "<i>L. floridana</i> absent",
      TRUE ~ treatment_code
    ),
    group = ifelse(str_detect(treatment_code, "^A"), 
                   "<i>A. altissima</i>", 
                   "<i>L. floridana</i>")
  )

# Create the plot with your desired styling
ggplot(combined, aes(x = leaf_consumed, y = Caterpillar.Mass.Change, 
                     color = treatment, fill = treatment)) +
  # Points
  geom_point(size = 2.5, alpha = 0.8) +
  # Regression line with confidence band
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linewidth = 1) +
  # Facet by plant species
  facet_wrap(~ group, ncol = 2) +
  # Labels
  xlab("Leaf material consumed (pixels)") +
  ylab("Change in caterpillar mass (g)") +
  # Apply Tufte theme with customizations
  theme_tufte(18) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    # Use element_markdown to parse HTML tags
    strip.text = element_markdown(size = 14),
    legend.text = element_markdown(size = 12),
    strip.background = element_blank(),
    panel.spacing = unit(1.5, "lines")  # Add space between facets
  ) +
  # Color scales (matching your previous colors)
  scale_fill_manual(
    values = c(
      "<i>A. altissima</i> absent" = "darkmagenta",
      "<i>A. altissima</i> present" = "orchid",
      "<i>L. floridana</i> absent" = "darkgreen",
      "<i>L. floridana</i> present" = "olivedrab2"
    )
  ) +
  scale_color_manual(
    values = c(
      "<i>A. altissima</i> absent" = "darkmagenta",
      "<i>A. altissima</i> present" = "orchid",
      "<i>L. floridana</i> absent" = "darkgreen",
      "<i>L. floridana</i> present" = "olivedrab2"
    )
  )

ggsave("olivia/caterpillar_leaf.png")


# # OPTION 2: Facet by treatment (4 separate panels)
# ggplot(combined, aes(x = leaf_consumed, y = Caterpillar.Mass.Change)) +
#   geom_point(size = 2, alpha = 0.7) +
#   geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.2) +
#   facet_wrap(~ treatment_clean, ncol = 2) +
#   labs(
#     x = "Leaf material consumed (g)",
#     y = "Change in caterpillar mass (g)",
#     title = "Caterpillar mass change vs. leaf consumption by Treatment"
#   ) +
#   theme_minimal()


## extra code


# df <- read.csv("olivia/Scans.csv") %>%
#   rename(`24` = Damage.at.24.HR....,
#          `48` = Damage.at.48.HR....,
#          `72` = Damage.at.72.HR....,
#          `96` = Damage.at.96.HR....) %>%
#   mutate(`0` = 100,
#          treatment = str_extract(Leaf.Number, "^..")) %>%
#   select(id = Leaf.Number, treatment, `0`, `24`, `48`, `72`, `96`) %>%
#   pivot_longer(`0`:`96`, names_to = "time", values_to = "damage") %>%
#   mutate(time = as.integer(time))
# 
# 
# library(tidyverse)
# library(ggplot2)
# library(ggthemes)
# 
# df <- read.csv("olivia/Scans.csv") %>%
#   rename(`24` = Damage.at.24.HR....,
#          `48` = Damage.at.48.HR....,
#          `72` = Damage.at.72.HR....,
#          `96` = Damage.at.96.HR....) %>%
#   mutate(`0` = 100,
#          treatment = str_extract(Leaf.Number, "^..")) %>%
#   select(id = Leaf.Number, treatment, `0`, `24`, `48`, `72`, `96`) %>%
#   pivot_longer(`0`:`96`, names_to = "time", values_to = "damage") %>%
#   mutate(time = as.integer(time))
# 
# # Convert treatment to a factor with the desired order
# df$treatment <- factor(df$treatment, levels = c("LP", "LA", "AP", "AA"))
# 
# # add in dummy data
# df$damage[is.na(df$damage)] <- 100 - (rlnorm(length(df$damage[is.na(df$damage)]), 0, 0.5 ) *
#                                         df$time[is.na(df$damage)]/6)
# 
# # summarize
# df.sum <- df %>%
#   group_by(treatment, time) %>%
#   summarise(meanDamage = mean(damage), se = sd(damage)/sqrt(6)) %>%
#   mutate(semax = meanDamage + (1.96*se),
#          semin = meanDamage - (1.96*se))
# 
# ggplot(data = df.sum, aes(x = time, y = meanDamage)) +
#   geom_ribbon(aes(ymin = semin, ymax = semax, fill = treatment),
#               alpha = .2) +
#   geom_line(aes(color = treatment), linewidth = 2) +
#   xlab("Time (Hours)")+
#   ylab("Leaf Damage (%)")+
#   scale_x_continuous(
#     name = "Time (Hours)",
#     breaks = seq(0, 96, by = 24),
#     limits = c(0, 96),
#     labels = scales::comma
#   ) +
#   theme_tufte(18) +
#   theme(
#     legend.position = "none",
#     text = element_text(color = "white"),  # All text
#     axis.text = element_text(color = "white"),  # Axis numbers
#     axis.title = element_text(color = "white"),  # Axis labels
#     axis.ticks = element_line(color = "white"),  # Axis ticks
#     panel.grid.major = element_line(color = "white", size = 0.1),
#     panel.grid.minor = element_blank(),
#     legend.text = element_text(color = "white"),  # Legend text
#     legend.title = element_text(color = "white"))+
#   scale_fill_manual(values = c("mediumseagreen","olivedrab3", "lightsalmon","peachpuff"), labels = c("Native Intact", "Native Removed", "Exotic Intact", "Exotic Removed"))+
#   scale_color_manual(values = c("mediumseagreen","olivedrab3", "lightsalmon","peachpuff"), labels = c("Native Intact", "Native Removed", "Exotic Intact", "Exotic Removed"))+
#   labs(fill = "Treatment", color = "Treatment")
# 
# ggsave("olivia/meanDamage.png")








