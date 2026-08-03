# the purpose of this script is to rank the choices of plant for R. meadii

# initialize
rm(list = ls())
library(dplyr)
library(brms)

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
  mutate(alk_lcms = scale(together$total_alkaloid_area, center = F)) %>%
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
         invasive = INV)

plot(allTog$adm, allTog$tasty)
summary(lm(tasty ~ poly(adm, 2), data = allTog))

allTog <- allTog %>%
  mutate(invades = ifelse(invasive == "Native", "Non-Invasive Exotic", invasive))

summary(lm(tasty ~ invades*poly(adm,2), allTog))





