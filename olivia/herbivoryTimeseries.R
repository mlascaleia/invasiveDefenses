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
  theme_tufte(18) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    # Use element_markdown to parse HTML tags
    strip.text = element_markdown(size = 14),
    legend.text = element_markdown(size = 12),
    strip.background = element_blank()
  ) +
  scale_fill_manual(
    values = c(
      "<i>A. altissima</i> absent" = "darkmagenta",
      "<i>A. altissima</i> present" = "plum",
      "<i>L. floridana</i> absent" = "darkgreen",
      "<i>L. floridana</i> present" = "olivedrab2"
    )
  ) +
  scale_color_manual(
    values = c(
      "<i>A. altissima</i> absent" = "darkmagenta",
      "<i>A. altissima</i> present" = "plum",
      "<i>L. floridana</i> absent" = "darkgreen",
      "<i>L. floridana</i> present" = "olivedrab2"
    )
  )

ggsave("olivia/totalLeafDamage.png")



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








