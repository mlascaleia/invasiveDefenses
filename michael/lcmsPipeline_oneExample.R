# The goal of this script is to take all the crazy LCMS data I have
# and turn it into within-family comparisons of plant defensive compounds

# load libraries

library(tidyverse)

# load data

# barberry is my first run at the pipeline

canopus <- read_tsv("michael/lcmsData/berberidaceae/canopus_formula_summary-10.tsv")
structure <- read_tsv("michael/lcmsData/berberidaceae/structure_identifications_top-10.tsv",
                      na = "-Infinity") %>%
  select(-overallFeatureQuality)
alignment <- read.csv("michael/lcmsData/berberidaceae/berberidaceaeAligned.csv")

# start by working on combining the classes, structures, and quality
# start by organizing canopus

# get only the top structure from canopus, as it only assigns one class
canopus <- canopus %>%
  filter(formulaRank == 1)

# combine the canopus data with the structure data

siriusOutput <- canopus %>%
  left_join(structure, by = c("alignedFeatureId")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~str_remove(., "\\.x$"), ends_with(".x"))

# Find the most likely compound, especially if rank 1 is some weird non-plant thing

# take out high confidence things that I don't need to include

high_conf <- siriusOutput %>%
  filter(structurePerIdRank == 1, 
         ConfidenceScoreApproximate >= 0.9)

low_conf <- siriusOutput %>%
  filter(ConfidenceScoreApproximate < 0.9 | is.na(ConfidenceScoreApproximate))

# make function for scoring lower-ranked things 

# bonus if it is from a plant metabolite database
is_from_plant_db <- function(links) {
  links <- replace_na(as.character(links), "")
  str_detect(links, "KNAPSACK|LOTUS|PLANTCYC|COCONUT")
}

# I hate to do it but I'm keep Cl and Br in because it IS possible
# Otherwise gets booted if it has non-plant element in it
has_disqualifying_element <- function(formula) {
  formula <- as.character(formula)
  exotic <- str_extract_all(formula, "[A-Z][a-z]?")[[1]]
  exotic <- exotic[!exotic %in% c("C","H","N","O","S","P","Cl","Br")]
  length(exotic) > 0
}

# boot if deut
is_deuterated <- function(name) {
  if(!is.na(name)){
    name <- tolower(as.character(name))
    str_detect(name, "-d\\d|\\[d\\d\\]|deuterio|dideuterio")
  } else F
}

# if it only exists in a synthetic molecule database
is_in_synth_db_only <- function(links) {
  links <- replace_na(as.character(links), "")
  has_synth <- str_detect(links, "DSSTOX|PUBCHEMANNOTATIONSAFETYANDTOXIC")
  has_plant  <- is_from_plant_db(links)
  has_synth & !has_plant
}

# run all rules together
score_bio <- function(name, formula, links, rt_min) {
  
  # hard disqualifiers
  if (has_disqualifying_element(formula)) 
    return(-20)
  if (is_deuterated(name))               
    return(-30)
  if (is_in_synth_db_only(links))        
    return(-40)
  
  # Rule 2: plant database presence
  plant_db <- is_from_plant_db(links)
  
  # Rule 3: RT
  void_volume <- !is.na(rt_min) && rt_min < 2.0
  
  # Score: plant DB is the main signal, RT is tiebreaker
  score <- 0
  if (plant_db)    score <- score + 2
  if (!void_volume) score <- score + 1
  
  return(score)
}

# run the scorer/ranker

low_conf_rescored <- low_conf %>%
  mutate(
    bio_score = mapply(
      score_bio,
      name    = name,
      formula = molecularFormula,
      links   = replace_na(links, ""),
      rt_min  = retentionTimeInMinutes
    )
  ) %>%
  group_by(alignedFeatureId) %>%
  slice_max(bio_score, n = 1, with_ties = FALSE) %>%
  ungroup()

# get out genuine compound winners

valid_compounds <- high_conf %>%
  mutate(bio_score = 10) %>% 
  rbind(low_conf_rescored) %>%
  filter(bio_score >0)

write.csv(valid_compounds, file = "michael/lcmsData/berberidaceae/valid_compounds.csv", row.names = F)

# now time to do filtering to include ONLY plant defensive compounds

primary_metabolite_classes <- c(
  "Purine nucleosides",
  "Pyrimidine nucleosides",
  "Purine nucleotides",
  "Aminoacids", # may be defense
  "Dipeptides", # may be defense
  "Tripeptides", # may be defense
  "Linear peptides", # may be defense
  "Cyclic peptides" # may be defense
)

defensive_pathways <- c(
  "Alkaloids",
  "Shikimates and Phenylpropanoids",
  "Terpenoids",
  "Polyketides"
)

defensive_compounds <- valid_compounds %>%
  filter(
    `NPC#pathway` %in% defensive_pathways,
    !`NPC#class` %in% primary_metabolite_classes,
    `NPC#pathway Probability` >= 0.5,
    !str_detect(replace_na(links, ""), "MACONDA")
  )


# ── Build area matrix from alignment ──────────────────────────────────────

state_matrix <- alignment %>%
  select(id, matches("feature_state"))

area_matrix <- alignment %>%
  select(id, matches("area$"))

# ── Join on id = mappingFeatureId ─────────────────────────────────────────

area_matrix <- area_matrix %>%
  mutate(id = as.character(id))

defensive_compounds <- defensive_compounds %>%
  mutate(mappingFeatureId = as.character(mappingFeatureId))

final_output <- defensive_compounds %>%
  left_join(area_matrix, by = c("mappingFeatureId" = "id"))

write.csv(final_output, "michael/lcmsData/berberidaceae/berberidaceaeFinalized.csv", row.names = FALSE)

named <- final_output %>%
  mutate(across(starts_with("datafile"), ~ replace_na(., 0))) %>%
  group_by(InChIkey2D) %>%
  summarise(across(starts_with("datafile"), ~ sum(.)))

# ── Check results ─────────────────────────────────────────────────────────

sample_area_cols <- names(final_output)[str_detect(names(final_output), "area")]
n_with_data <- rowSums(!is.na(final_output[sample_area_cols]))

unmatchedID <- final_output[n_with_data == 0, ]$mappingFeatureId
area_matrix[area_matrix$id %in% 574, ]

print(paste("Total defensive compound features:", nrow(final_output)))
print(paste("Features with zero sample data:", sum(n_with_data == 0)))
print(paste("Features with data in all samples:", sum(n_with_data == length(sample_area_cols))))

# I will need to turn all the above into a function later
# for now I want to see what piping to a PCA will look like

chems <- named %>%
  # select(mappingFeatureId, starts_with("datafile")) %>%
  mutate(across(starts_with("datafile"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("datafile"), ~ log1p(.))) %>%
  filter(!is.na(InChIkey2D)) %>%
  column_to_rownames(var = "InChIkey2D") %>%
  as.matrix() %>%
  t()

rowSums(chems != 0)

row.names(chems)
chems2 <- chems[-c(15,18,14,1,4,13), ]
bad <- which(apply(chems2, 2, var) == 0)
chems2 <- chems2[,-bad]

# Run PCA
pca <- prcomp(chems)

pca_df <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2]
  # family = dfarea$Plant.Family  # your grouping vector
)

loadings <- as.data.frame(pca$rotation[, 1:2])  # PC1 and PC2 loadings
loadings$var <- rownames(loadings)

# scale vectors so they show up nicely
scale_factor <- 250
loadings$PC1 <- loadings$PC1 * scale_factor
loadings$PC2 <- loadings$PC2 * scale_factor



ggplot(pca_df, aes(PC1, PC2)) +
  geom_point(size = 3) +
  geom_text(aes(label = row.names(pca_df)), nudge_y = -3, size = 3) +
  labs(
    x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)")
  ) +
  # geom_segment(data = loadings,
  #              aes(x = 0, y = 0, xend = PC1, yend = PC2),
  #              arrow = arrow(length = unit(0.2, "cm")),
  #              color = "red") +
  theme_classic()

















