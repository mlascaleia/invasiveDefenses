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



#Model for Trichomes and species

m1 <- lm(slope ~ trichomes, data = dif2)
summary(m1)

m2 <- lm (slope ~ species, data = dif2)
summary(m2)

m3 <- lm(slope ~ trichomes+species, data = dif2)
summary(m3)

m4 <- glm(slope ~ trichomes*species, data = dif2)
summary(m4)
null <- lm(slope ~ 1, data = dif2)

#AIC Test
lapply(list(m1,m2,m3,m4,null),AIC)
#m4 the best model- report m3

#Model for Phenolics and trichomes
m5 <- lm(Phenolics ~ trichomes, data = dif2)
summary(m5)

m6 <- lm (Phenolics ~ species, data = dif2)
summary(m6)

m7 <- lm(Phenolics ~ trichomes+species, data = dif2)
summary(m7)

m8 <- glm(Phenolics ~ trichomes*species, data = dif2)
summary(m8)

null <- lm(Phenolics ~ 1, data = dif2)

#AIC Test
lapply(list(m5,m6,m7,m8,null),AIC)
#m6 is the best model



#Model for Flavonoids and trichomes
m9 <- lm(Flavonoids ~ trichomes, data = dif2)
summary(m9)

m10 <- lm (Flavonoids ~ species, data = dif2)
summary(m10)

m11 <- lm(Flavonoids ~ trichomes+species, data = dif2)
summary(m11)

m12 <- glm(Flavonoids ~ trichomes*species, data = dif2)
summary(m12)

null <- lm(Flavonoids ~ 1, data = dif2)

#AIC Test
lapply(list(m9,m10,m11,m12,null),AIC)
#m3


#Model for Saponins and trichomes
m13 <- lm(Saponins ~ trichomes, data = dif2)
summary(m13)

m14 <- lm (Saponins ~ species, data = dif2)
summary(m14)

m15 <- lm(Saponins ~ trichomes+species, data = dif2)
summary(m15)

m16 <- glm(Saponins ~ trichomes*species, data = dif2)
summary(m16)

null <- lm(Saponins ~ 1, data = dif2)

#AIC Test
lapply(list(m13,m14,m15,m16,null),AIC)
#m3




#Model for Terpenoids and trichomes
m17 <- lm(Terpenoids ~ trichomes, data = dif2)
summary(m17)

m18 <- lm (Terpenoids ~ species, data = dif2)
summary(m18)

m19 <- lm(Terpenoids ~ trichomes+species, data = dif2)
summary(m19)

m20 <- glm(Terpenoids ~ trichomes*species, data = dif2)
summary(m20)

null <- lm(Terpenoids ~ 1, data = dif2)

#AIC Test
lapply(list(m17,m18,m19,m20,null),AIC)
#models super super similar - m19 best

bigM <- lm(slope ~ Flavonoids + Phenolics + Saponins + Tannins, data = dif2)
summary(bigM)




