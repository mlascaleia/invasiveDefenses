
library(tidyverse)
library(vegan)

# read data

area <- read.csv("data/lcms/lcmsAreaNorm.csv") %>%
  column_to_rownames(var = "ref") %>%
  filter(!is.na(Norm..Area..SUB15963_KC18_H2OvsACNFA_SPL001_14_87_B.raw..F17.)) %>%
  as.matrix()
gap <- read.csv("data/lcms/lcmsGapFilling.csv") %>%
  column_to_rownames(var = "ref")
gapCode <- read.csv("data/lcms/lcmsGapFillingCode.csv") %>%
  column_to_rownames(var = "ref") %>%
  as.matrix()

gapCode[is.na(gapCode)] <- 1000

isGap <- as.matrix(gap == "Full gap")
isGapCoded <- as.matrix(gapCode >= 64)

# make the true (0 is truly the gap)
trueGap <- (isGap * isGapCoded) == 0

trueArea <- trueGap * area %>%
  as.data.frame() %>%
  filter(!is.na(Norm..Area..SUB15963_KC18_H2OvsACNFA_SPL001_14_87_B.raw..F17.))

write.csv(trueArea, row.names = F, file = "data/lcms/trueAreas.csv")

# get in plant info

pla <- read.csv("data/dataSheets/samplePlants.csv")
pla$Accession.. <- gsub("\\*", "_", pla$Accession..)
pla$Accession.. <- gsub("\\-", "_", pla$Accession..)

colnames(area) <- gsub("Norm..Area..SUB15963_KC18_H2OvsACNFA_SPL[[:digit:]]{3}_",
                       "", colnames(area))
colnames(area) <- gsub("\\.raw.*$",
                       "", colnames(area))

dfarea <- t(area) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Accession..") %>%
  inner_join(pla)


# Transpose so samples are rows (what prcomp expects)
mat <- dfarea %>%
  select(-Accession.., -Species.Name, -Plant.Family) %>%
  as.matrix()

# Log-transform (add 1 to handle zeros)
mat_log <- log1p(mat)

# Run PCA
pca <- prcomp(mat_log, center = TRUE, scale. = TRUE)

pca_df <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  family = dfarea$Plant.Family  # your grouping vector
)

ggplot(pca_df, aes(PC1, PC2, color = family)) +
  geom_point(size = 3) +
  stat_ellipse() +
  labs(
    x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)")
  ) +
  theme_minimal()










