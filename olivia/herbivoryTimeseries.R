library(tidyverse)

df <- read.csv("olivia/Harvard Master2.csv") %>%
  rename(`24` = Damage.at.24.HR....,
         `48` = Damage.at.48.HR....,
         `72` = Damage.at.72.HR....,
         `96` = Damage.at.96.HR....) %>%
  mutate(`0` = 100,
         treatment = str_extract(Leaf.Number, "^..")) %>%
  select(id = Leaf.Number, treatment, `0`, `24`, `48`, `72`, `96`) %>%
  pivot_longer(`0`:`96`, names_to = "time", values_to = "damage") %>%
  mutate(time = as.integer(time))

# add in dummy data
# (things should usually go down but they may go up a little with the dummy data)

df$damage[is.na(df$damage)] <- 100 - (rlnorm(length(df$damage[is.na(df$damage)]), 0, 0.5 ) *
                                        df$time[is.na(df$damage)]/6)

# summarize

df.sum <- df %>%
  group_by(treatment, time) %>%
  summarise(meanDamage = mean(damage), se = sd(damage)/sqrt(6)) %>%
  mutate(semax = meanDamage + (1.96*se),
         semin = meanDamage - (1.96*se))

# make figure

ggplot(data = df.sum, aes(x = time, y = meanDamage)) +
  geom_ribbon(aes(ymin = semin, ymax = semax, fill = treatment),
              alpha = .2) +
  geom_line(aes(color = treatment), linewidth = 2) +
  geom_line(data = df, aes(y = damage, group = id, color = treatment), 
            linewidth = 0.5, alpha = 0.5) +
  ggthemes::theme_tufte() +
  facet_wrap(~treatment, nrow = 1)












