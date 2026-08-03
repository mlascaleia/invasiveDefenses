# the purpose of this script is to look at alkaloids in the LCMS data

library(tidyverse)

# alk <- read.csv("michael/lcmsData/berberidaceae/berberidaceaeFinalized.csv") %>%
#   filter(NPC.pathway == "Alkaloids")
# 
# sort(-table(alk$NPC.class))

# that didn't really work, so we're going more complicated
# I am going to get just the berberine amount out of the lc/ms data

# load data
d <- read.csv("sereen/Alkaloid Assay/rawBerberismzmineData.csv", check.names = FALSE)

# get sample names
samples <- unique(sub("^datafile:(.+)\\.raw:.*$", "\\1",
                      grep("^datafile:", names(d), value = TRUE)))


# function to pull one attribute across all samples
# this will be useful when I know which rows are berberine
pull_attr <- function(attr) {
  cols <- paste0("datafile:", samples, ".raw:", attr)
  out  <- d[, cols, drop = FALSE]
  names(out) <- samples
  out
}

# superduper long df, with one row per feature x sample
long <- bind_rows(lapply(seq_along(samples), function(i) {
  s <- samples[i]
  data.frame(
    id         = d$id,
    mz         = d$mz,
    rt         = d$rt,
    sample     = s,
    area       = as.numeric(d[[paste0("datafile:", s, ".raw:area")]]),
    height     = as.numeric(d[[paste0("datafile:", s, ".raw:height")]]),
    fwhm       = as.numeric(d[[paste0("datafile:", s, ".raw:fwhm")]]),
    sample_rt  = as.numeric(d[[paste0("datafile:", s, ".raw:rt")]]),
    state      = d[[paste0("datafile:", s, ".raw:feature_state")]],
    tailing    = as.numeric(d[[paste0("datafile:", s, ".raw:tailing_factor")]]),
    stringsAsFactors = FALSE)
})) %>%
  mutate(area   = ifelse(is.na(area), 0, area),
         height = ifelse(is.na(height), 0, height))


# get just berberine out of the long dataframe
berb <- long %>%
  filter(abs(mz - 336.1238) < 0.005, rt >= 20, rt <= 23) %>%
  arrange(rt, sample)

# then find out how much berberine was in each sample!
berberine_perSample <- berb %>%
  group_by(sample) %>%
  summarise(berberine      = sum(area),
            berberine_det  = sum(area[state == "DETECTED"]),
            n_frags        = sum(state == "DETECTED"),
            .groups = "drop")

# change names a smidge to align with other frame...
berberine_perSample$sample <- str_replace(berberine_perSample$sample, 
                                          "^[[:digit:]]{3}_", "")
# now bring in the actual names of the plants...

plants <- read.csv("data/plantSelection/selection/plantSheet.csv") %>%
  filter(Family == "Berberidaceae") %>%
  mutate(id = str_replace_all(id, "(\\*|\\-)", "_")) %>%
  select(sample = id, Binomial)

# trying to do this for a number of compounds:

quantify_compound <- function(long, compound, mz_target,
                              rt_lo, rt_hi, mz_tol = 0.005) {
  sel <- long %>%
    filter(abs(mz - mz_target) < mz_tol, rt >= rt_lo, rt <= rt_hi)
  
  if (nrow(sel) == 0) {
    warning("no features found for ", compound); return(NULL)
  }
  message(compound, ": ", n_distinct(sel$id), " features, ",
          "RT ", round(min(sel$rt), 2), "-", round(max(sel$rt), 2))
  
  sel %>%
    group_by(sample) %>%
    summarise(compound     = compound,
              area         = sum(area),
              n_detected   = sum(state == "DETECTED"),
              any_detected = any(state == "DETECTED"),
              max_height   = max(height),
              .groups = "drop")
}

targets <- tibble::tribble(
  ~compound,            ~mz,       ~rt_lo, ~rt_hi,
  "berberine",          336.1238,  21.3,   22.5,
  "palmatine",          352.1543,  22.0,   22.5,
  "jatrorrhizine",      338.1387,  19.8,   21.0,
  "magnoflorine_16.2",  342.1700,  16.0,   16.6,
  "magnoflorine_17.2",  342.1700,  17.0,   17.5,
  "aporphine_15.3",     328.1543,  15.1,   15.6,
  "aporphine_13.4",     328.1543,  13.1,   13.7,
  "reticuline",         330.1700,  14.2,   14.8,
  "bia_300",            300.1594,  11.2,   11.8,
  "nmc_286",            286.1438,   9.7,   10.2
)

alkaloids <- purrr::pmap_dfr(targets, function(compound, mz, rt_lo, rt_hi)
  quantify_compound(long, compound, mz, rt_lo, rt_hi))

# ---- total positive metabolome per sample -----------------------------
# RT 2-45 drops the solvent front and the column wash
metabolome <- long %>%
  filter(rt >= 2, rt <= 45) %>%
  group_by(sample) %>%
  summarise(total_area = sum(area), .groups = "drop")

results <- alkaloids %>%
  left_join(metabolome, by = "sample") %>%
  mutate(rel = area / total_area,
         sample = str_replace(sample, "^[[:digit:]]{3}_", "")) %>%
  left_join(plants, by = "sample")

wide_rel <- results %>%
  select(Binomial, compound, rel) %>%
  tidyr::pivot_wider(names_from = compound, values_from = rel)

write.csv(wide_rel, "sereen/Alkaloid Assay/lcmsResults.csv", row.names = FALSE)

# ---- PCA on normalised, logged values ---------------------------------
alk <- wide_rel %>%
  mutate(across(-Binomial, ~ log10(. + 1e-9)))     # rel values are tiny

pca <- prcomp(alk[,-1], scale. = TRUE)
summary(pca)
round(pca$rotation[, 1:2], 3)

alk$PC1 <- pca$x[,1]
alk$PC2 <- pca$x[,2]

# alk <- alk %>% left_join(plants %>% distinct(Binomial, Provenance), by = "Binomial")

ggplot(alk, aes(x = reorder(Binomial, PC1), y = PC1)) +
  geom_segment(aes(xend = Binomial, y = min(PC1), yend = PC1),
               colour = "grey75", linewidth = 0.3) +
  geom_point(size = 2.5) +
  coord_flip() +
  labs(x = NULL, y = "PC1 (relative alkaloid investment)") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"))

