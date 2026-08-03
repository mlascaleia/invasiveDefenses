# this is the script that actually implements the lcms pipeline

source("michael/lcmsPipeline.R")

library(vegan)
library(ggplot2)
library(ggrepel)
library(rcdk)
library(fingerprint)

plas <- read.csv("data/plantSelection/selection/plantSheet_withProvenance.csv")

berberidaceae <- process_lcms_family("berberidaceae")
# adoxaceae <- process_lcms_family("adoxaceae")
caprifoliaceae <- process_lcms_family("caprifoliaceae")

# write.csv(adoxaceae$defensive_compounds, "michael/lcmsData/adoxaceae/adoxaceaeFinalized.csv", row.names = F)
write.csv(berberidaceae$defensive_compounds, "michael/lcmsData/berberidaceae/berberidaceaeFinalized.csv", row.names = F)
# write.csv(caprifoliaceae$defensive_compounds, "michael/lcmsData/caprifoliaceae/caprifoliaceaeFinalized.csv", row.names = F)

b_mat <- berberidaceae$defensive_compounds %>%
  select(starts_with("datafile")) %>%
  as.matrix() %>%
  t()

b_mat[is.na(b_mat)] <- 0

berb <- process_names(b_mat)

nmds_explore(b_mat, berb)


# a_mat <- adoxaceae$defensive_compounds %>%
#   select(starts_with("datafile")) %>%
#   as.matrix() %>%
#   t()
# 
# a_mat[is.na(a_mat)] <- 0
# 
# adox <- process_names(a_mat)
# 
# nmds_explore(a_mat, adox)
# 
# c_mat <- caprifoliaceae$defensive_compounds %>%
#   select(starts_with("datafile")) %>%
#   as.matrix() %>%
#   t()
# 
# c_mat[is.na(c_mat)] <- 0
# 
# capr <- process_names(c_mat)
# 
# nmds_explore(c_mat, capr)

diss <- as.matrix(dist(b_mat))

berb <- berb %>%
  arrange(raw_name)

colnames(diss) <- berb$Binomial
row.names(diss) <- berb$Binomial

tree <- ape::fastme.bal(diss)
plot(tree)

area_cols <- grep("datafile", names(berberidaceae$all_compounds), value = TRUE)

alkaloid_area_by_sample <- berberidaceae$all_compounds %>%
  filter(`NPC#pathway` == "Alkaloids") %>%
  select(all_of(area_cols)) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "raw_name", values_to = "total_alkaloid_area")

getProp <- berberidaceae$all_compounds %>%
  select(all_of(area_cols)) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "raw_name", values_to = "total_metabolite_area") %>%
  left_join(alkaloid_area_by_sample) %>%
  mutate(alkaloidProp = total_alkaloid_area/total_metabolite_area)

# Use process_names to attach species metadata
# process_names expects a matrix with rownames as the datafile strings
dummy_mat <- matrix(0, nrow = length(area_cols), ncol = 1)
rownames(dummy_mat) <- area_cols
labs <- process_names(dummy_mat)

alkData <- getProp %>%
  left_join(labs, by = "raw_name") %>%
  select(Binomial, Provenance, total_alkaloid_area,  alkaloidProp) %>%
  arrange(desc(alkaloidProp))

save(alkData, file = "sereen/Alkaloid Assay/lcmsresults2.rdata") 

aa <- alkaloid_area_by_sample %>%
  arrange(total_alkaloid_area)

ggplot(aa, aes(x = reorder(Binomial, -total_alkaloid_area), 
               y = total_alkaloid_area, 
               fill = Provenance)) +
  geom_col() +
  scale_fill_discrete(labels = c("invasive" = "Invasive", 
                                 "native" = "Native", 
                                 "non-invasive exotic" = "Non-invasive exotic")) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "white", 
                               face = "italic", size = 12),
    axis.text.y = element_text(colour = "white", size = 12),
    axis.title = element_text(colour = "white", size = 14),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(colour = "white"),
    legend.text = element_text(colour = "white", size = 12),
    legend.title = element_text(colour = "white", size = 13),
    legend.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(colour = "white", size = 15)
  ) +
  labs(x = NULL, 
       y = "Total ion area",
       title = "Alkaloid abundance by species (Berberidaceae)")


# Filter to annotated features with SMILES in all_compounds
annotated <- berberidaceae$all_compounds %>%
  filter(!is.na(smiles) & smiles != "") %>%
  select(mappingFeatureId, smiles, `NPC#pathway`)

# Parse only the filtered SMILES, not the whole dataset
mols <- parse.smiles(annotated$smiles)

# Identify any that failed to parse
valid <- !sapply(mols, is.null)
cat("Successfully parsed:", sum(valid), "of", nrow(annotated), "molecules\n")

annotated <- annotated[valid, ]
mols <- mols[valid]

# Compute circular fingerprints
fps <- lapply(mols, get.fingerprint, type = "circular")

# Compute pairwise Tanimoto similarity matrix between compounds
sim_matrix <- fp.sim.matrix(fps, method = "tanimoto")
rownames(sim_matrix) <- annotated$mappingFeatureId
colnames(sim_matrix) <- annotated$mappingFeatureId

# Get area columns
area_cols <- grep("datafile", names(berberidaceae$all_compounds), value = TRUE)

# Build feature abundance matrix using only successfully parsed features
abundance <- berberidaceae$all_compounds %>%
  filter(mappingFeatureId %in% annotated$mappingFeatureId) %>%
  select(mappingFeatureId, all_of(area_cols)) %>%
  column_to_rownames("mappingFeatureId")

# Replace NAs with 0
abundance[is.na(abundance)] <- 0

# Log transform
abundance_log <- log1p(abundance)

# Align sim_matrix to abundance row order
sim_ordered <- sim_matrix[rownames(abundance_log), rownames(abundance_log)]

# Compute sample x sample structural similarity: A^T * S * A
A <- as.matrix(abundance_log)
sample_sim <- t(A) %*% sim_ordered %*% A

# Normalize so diagonal = 1
diag_vals <- diag(sample_sim)
sample_sim_norm <- sample_sim / sqrt(outer(diag_vals, diag_vals))

# Convert to distance
sample_dist <- as.dist(1 - sample_sim_norm)

# NMDS
nmds_structural <- metaMDS(sample_dist, k = 2, trymax = 100)

# Get sample labels using your existing process_names function
# sample_dist has column names that are the datafile.xxx.raw.area strings
labs <- process_names(as.matrix(sample_dist))

# Extract NMDS scores
scores_df <- as.data.frame(scores(nmds_structural, display = "sites"))
scores_df$raw_name <- rownames(scores_df)

stress_label <- paste0("Stress = ", round(nmds_structural$stress, 3))

scores_df <- full_join(scores_df, labs, by = "raw_name")

# Same ggplot code as nmds_explore
ggplot(scores_df, aes(x = NMDS1, y = NMDS2, label = Binomial)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey60") +
  geom_point(size = 3, aes(color = Provenance)) +
  geom_text_repel(size = 3, max.overlaps = Inf,
                  segment.size = 0.3, segment.colour = "grey60",
                  box.padding = 0.4, point.padding = 0.3) +
  stat_ellipse(aes(color = Provenance), type = "t", level = 0.95,
               linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = Inf, y = -Inf, label = stress_label,
           hjust = 1.1, vjust = -0.8, size = 3, colour = "grey40") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(x = "NMDS1", y = "NMDS2",
       title = "Structural similarity NMDS (Tanimoto fingerprints, annotated features only)")


ggplot(berberidaceae$defensive_compounds, aes(x = retentionTimeInMinutes)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(xintercept = 48, color = "red", linetype = "dashed") +
  labs(title = "Berberidaceae feature RT distribution",
       x = "Retention time (min)", y = "Feature count")

