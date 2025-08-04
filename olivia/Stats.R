source("olivia/CleanchemData_Olivia.R")

dif <- values.df %>%
  select(-Damage.at.48.HR....,
         -Damage.at.72.HR....,
         -Damage.at.24.HR....,
         -Damage.at.96.HR....) %>%
  mutate(percentRemaining24 = X24.HR/X0.HR,
         percentRemaining48 = X48.HR/X0.HR,
         percentRemaining72 = X72.HR/X0.HR,
         percentRemaining96 = X96.HR/X0.HR)

# lets error check 

# remove one point for now 

dif <- dif[!dif$Leaf.Number == "LA16", ]

dif2 <- dif %>%
  select(Leaf.Number,
         percentRemaining24,
         percentRemaining48,
         percentRemaining72,
         percentRemaining96) %>%
 
   mutate(percentRemaining0 = 1) %>%
  
  pivot_longer(starts_with("p"),
               names_to = "time", values_to = "damage") %>%
  
  mutate(time = as.numeric(gsub("percentRemaining",
                    "",time)))%>%
  group_by(Leaf.Number)%>%
  summarize(slope = coef(lm(damage ~ time))[2]) %>%
  
  full_join(dif)%>%
  
  mutate(species = str_extract(Leaf.Number,"^(A|L)"),
         trichomes = str_extract(Leaf.Number, "(?<=^.)."))

m1 <- lm(slope ~ trichomes, data = dif2)
summary(m1)

m2 <- lm (slope ~ species, data = dif2)
summary(m2)

m3 <- lm(slope ~ trichomes+species, data = dif2)
summary(m3)

m4 <- glm(slope ~ trichomes*species, data = dif2)
summary(m4)
null <- lm(slope ~ 1, data = dif2)

lapply(list(m1,m2,m3,m4,null),AIC)

m5 <- lm(Phenolics ~ trichomes+species, data = dif2)
summary(m1)

m6 <- lm (Phenolics ~ species, data = dif2)
summary(m2)

m7 <- lm(Phenolics ~ trichomes+species, data = dif2)
summary(m3)

m8 <- glm(Phenolics ~ trichomes*species, data = dif2)
summary(m4)
null <- lm(slope ~ 1, data = dif2)

lapply(list(m1,m2,m3,m4,null),AIC)


bigM <- lm(slope ~ Flavonoids + Phenolics + Saponins + Tannins, data = dif2)
summary(bigM)




