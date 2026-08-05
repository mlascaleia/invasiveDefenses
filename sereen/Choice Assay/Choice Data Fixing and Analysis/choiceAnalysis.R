# the purpose of this script is to rank the choices of plant for R. meadii

# initialize
rm(list = ls())
library(tidyverse)
#library(brms)

# load data

d <- read.csv("sereen/Choice Assay/Choice Data/Corrected Data/matchups1clean.csv", stringsAsFactors = FALSE) %>%
# The outgroups were never eaten so no information can be gained from them
  filter((str_detect(yellow, "^(B|M)"))&
         (str_detect(green, "^(B|M)")))
e <- read.csv("sereen/Choice Assay/Choice Data/Corrected Data/matchups2clean.csv") 
f <- read.csv("sereen/Choice Assay/Choice Data/Corrected Data/matchups3clean.csv")
g <- read.csv("sereen/Choice Assay/Choice Data/Corrected Data/matchups4clean.csv")

d <- bind_rows(d,e,f,g)

# fix any errors that snuck through
d$Y.Eaten[d$matchupNo == 361] <- 85


# convert to long

long <- bind_rows(
  d %>% transmute(trial = matchupNo, species = yellow,
                   color = "yellow", eaten = Y.Eaten, opponent = green),
  d %>% transmute(trial = matchupNo, species = green,
                   color = "green",  eaten = G.Eaten, opponent = yellow)
) %>%
  mutate(pair = paste(pmin(species, opponent), pmax(species, opponent), sep = "|"),
    # add in a censor depending on if it may have wanted >100 of what was given
    # or "less than 0" (which is just for math purposes)
    cens = case_when(eaten >= 97 ~ "right",
                     eaten <= 0   ~ "left",
                     TRUE         ~ "none"),
    species = factor(species),
    color   = factor(color, levels = c("yellow", "green"))
  ) %>%
  mutate(eaten = eaten/100)

# fit the data

# fit <- brm(
#   eaten | cens(cens) ~ 0 + species + (1 | trial),  # + (1 | pair) can be added with replicates
#   data   = long,
#   family = gaussian(),          # Tobit: latent normal clipped to [0, 100]
#   prior  = c(prior(normal(0.5, 0.4), class = "b"),
#              # prior(normal(0, 0.25),  class = "b", coef = "colorgreen"),
#              prior(exponential(2),   class = "sd"),
#              prior(exponential(2),   class = "sigma")),
#   chains = 4, cores = 4, iter = 4000, seed = 42,
#   control = list(adapt_delta = 0.95)
# )
# 
# summary(fit)

# # trying with ordbeta
# library(ordbetareg)
# fit2 <- ordbetareg(
#   eaten ~ 0 + species + (1 | trial),
#   data = long,
#   coef_prior_mean = 1.5,
#   coef_prior_SD = 1,
#   chains = 4, cores = 4, iter = 6000, seed = 42,
#   control = list(adapt_delta = 0.95)
# )
# 
# summary(fit2)
# save(fit2, file = "sereen/Choice Assay/choiceModel.rdata")
# load("sereen/Choice Assay/choiceModel.rdata")

# # extract the ranking
# 
# ab <- as.data.frame(fixef(fit2))
# ab$term <- rownames(ab)
# ab <- ab %>%
#   filter(grepl("^species", term)) %>%
#   mutate(species  = sub("^species", "", term),
#          Estimate = Estimate - mean(Estimate)) %>%
#   arrange(desc(Estimate)) %>% 
#   select(species, Estimate, Est.Error, Q2.5, Q97.5)
# ab
# 
# ggplot(ab, aes(x = reorder(species, Estimate), y = Estimate)) +
#   geom_pointrange(aes(ymax = Estimate + 1.96 * Est.Error, 
#                       ymin = Estimate - 1.96 * Est.Error)) +
#   theme_minimal() +
#   # theme(axis.text.x = element_text(angle = 90, vjust = .4)) +
#   coord_flip ()
# save(ab, file = "sereen/Choice Assay/choiceResults.rdata")
load("sereen/Choice Assay/choiceResults.rdata")
# bring it all together
rm(d,e,f,g,long)
library(fuzzyjoin)

load("sereen/Alkaloid Assay/alkaloidassay.rdata")
load("sereen/Alkaloid Assay/lcmsresults2.rdata")

alkData$Binomial <- str_replace(alkData$Binomial, "erberis", ".")
alkData$Binomial <- str_replace(alkData$Binomial, "ahonia", ".")

together <- stringdist_left_join(
  combined_df, alkData,
  by = c("Species" = "Binomial"),
  method = "lv", max_dist = 2,
  distance_col = "dist"
) %>%
  mutate(alk_lcms = scale(.$total_alkaloid_area, center = F)) %>%
  mutate(cl = Cloudy > 0)

summary(lm(absorbance_dilution_drymass ~ alk_lcms + cl, data = together))

newTog <- together %>%
  mutate(adm = ifelse(cl == T, absorbance_dilution_drymass - 0.06319, absorbance_dilution_drymass))

summary(lm(adm ~ alk_lcms, data = newTog))

allTog <- stringdist_left_join(
  newTog, ab,
  by = c("Species" = "species"),
  method = "lv", max_dist = 3,
  distance_col = "dist"
)

# neaten that nasty thing up....
allTog <- allTog %>%
  select(Species, adm, 
         tasty = Estimate,
         invasive = INV,
         stdE = Est.Error)

plot(allTog$adm, allTog$tasty)
summary(lm(tasty ~ poly(adm, 2), data = allTog))

allTog <- allTog %>%
  mutate(invades = ifelse(invasive == "Native", "Non-Invasive Exotic", invasive))

summary(lm(tasty ~ invades*adm, allTog))


##Sereen's attempt to plot the data onto the model
#Make plot with everything on it
library(ggplot2)

# 1. Fit the linear model
lin_model <- lm(tasty ~ invades*adm, data = allTog)

# 2. Plot the data points
plot(allTog$adm, allTog$tasty, col = "blue", pch = 16, 
     xlab = "Alkaloid Content", ylab = "Tastiness", main = "Tastiness By Alkaloid Content")

# 3. Overlay the model line
abline(lin_model, col = "red", lwd = 2)


######Plot for invasives
#Make a subset for invasives data points
invasive <- subset(allTog, invades == "Invasive")

#Plot the invasives data points
plot(invasive$adm, invasive$tasty, pch = 16, col = "darkorchid1",
     xlab = "Alkaloid Content", ylab = "Tastiness", main = "Tastiness By Alkaloid Content for Invasives")

#Plot the model onto the invasives data points
lines(invasive$adm, predict(lin_model, newdata = invasive), col = "darkorchid", lwd = 2)

library(ggplot2)
library(dplyr)

#Invasive plot with error
predictions <- predict(lin_model, newdata = invasive, se.fit = TRUE)
invasive$predicted_y <- predictions$fit
invasive$se_val <- predictions$se.fit

# 2. Plot with manual error ribbon
ggplot(data = invasive, aes(x = adm)) +
  # Draw the error ribbon FIRST so it sits behind your points and line
  geom_ribbon(aes(ymin = predicted_y - 1.96 * se_val, ymax = predicted_y + 1.96 * se_val), 
              fill = "grey80", alpha = 0.5) + # 1.96 multiplier gives the 95% Confidence Interval
  geom_point(aes(y = tasty), color = "darkorchid", size = 3) +
  geom_line(aes(y = predicted_y), color = "darkorchid2", linewidth = 1) + 
  labs(
    title = "Preference by Alkaloid Content for Invasives",
    x = "Alkaloid Content",
    y = "Rheumaptera Preference"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5),   
    axis.title.x = element_text(hjust = 0.5), 
    axis.title.y = element_text(hjust = 0.5)  
  )+
  coord_cartesian(xlim = c(-0.02, 0.24), ylim = c(-1.5, 1.75))+
  scale_y_continuous(breaks = seq(-1.5, 1.75, by = 0.5)) 




##Non-invasive graph with error
# 1. Get predictions AND standard errors (se.fit)
predictions <- predict(lin_model, newdata = noninvasive, se.fit = TRUE)
noninvasive$predicted_y <- predictions$fit
noninvasive$se_val <- predictions$se.fit

# 2. Plot with manual error ribbon
ggplot(data = noninvasive, aes(x = adm)) +
  # Draw the error ribbon FIRST so it sits behind your points and line
  geom_ribbon(aes(ymin = predicted_y - 1.96 * se_val, ymax = predicted_y + 1.96 * se_val), 
              fill = "grey80", alpha = 0.5) + # 1.96 multiplier gives the 95% Confidence Interval
  geom_point(aes(y = tasty), color = "green", size = 3) +
  geom_line(aes(y = predicted_y), color = "darkolivegreen", linewidth = 1) + 
  labs(
    title = "Preference by Alkaloid Content for Non-Invasives",
    x = "Alkaloid Content",
    y = "Rheumaptera Preference"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5),   
    axis.title.x = element_text(hjust = 0.5), 
    axis.title.y = element_text(hjust = 0.5)  
  )+
  coord_cartesian(xlim = c(-0.02, 0.24), ylim = c(-1.5, 1.75))+
  scale_y_continuous(breaks = seq(-1.5, 1.75, by = 0.5)) 




#Graph alkaloid content
allTog %>%
  arrange(adm)%>%
  ggplot(aes(x = reorder(Species, adm), y = adm, color = invasive)) + geom_point(cex=5)+ labs(
              title = "Alkaloid Content by Species",
              x = "Species",
              y = "Alkaloid Content")+
  scale_color_manual(values = c("Invasive" = "darkorchid", 
                                "Non-Invasive Exotic" = "gold1", 
                                "Native" = "blue"))+
  theme_classic()+
  theme(
    axis.text.y = element_text(face = "italic"),
    plot.title = element_text(hjust = 0.5),  # Centers the main title
  )+coord_flip()




#Graph Preferences
allTog %>%
  arrange(tasty)%>%
  ggplot(aes(x = reorder(Species, tasty), y = tasty, color = invasive)) + 
  geom_pointrange(cex=1, aes (ymin = tasty  - 1.96 * stdE, ymax = tasty + 1.96 * stdE))+ labs(
    title = "Barberry Preference of Rheumaptera",
    x = "Species",
    y = "Preference")+
  scale_color_manual(values = c("Invasive" = "darkorchid", 
                                "Non-Invasive Exotic" = "gold1", 
                                "Native" = "blue"))+
  theme_classic()+
   theme(
    axis.text.y = element_text(face = "italic"),
    plot.title = element_text(hjust = 0.5),  # Centers the main title
    axis.title.x = element_text(hjust = 0.5), # Centers the X-axis label
    axis.title.y = element_text(hjust = 0.5)  # Centers the Y-Axis label
  )+coord_flip()
