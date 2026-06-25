# ══════════════════════════════════════════════════════════════════════════════
# process_lcms_family.R
#
# Reusable pipeline for processing MZmine → Sirius LCMS data by plant family.
# Takes canopus, structure, and alignment files and returns:
#   1. All valid plant compounds (bio_score > 0)
#   2. Defensive compounds only (alkaloids, shikimates, terpenoids, polyketides)
#
# Usage:
#   source("process_lcms_family.R")
#   results <- process_lcms_family(
#     canopus_path   = "path/to/canopus_formula_summary.tsv",
#     structure_path = "path/to/structure_identifications_top.tsv",
#     alignment_path = "path/to/familyAligned.csv",
#     family_name    = "berberidaceae",
#     output_dir     = "path/to/output/"
#   )
#
#   results$all_compounds       # all valid plant compounds with area data
#   results$defensive_compounds # defensive subset with area data
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
rm(list = ls())

# ── Helper functions for biological plausibility scoring ─────────────────────

is_from_plant_db <- function(links) {
  links <- replace_na(as.character(links), "")
  str_detect(links, "KNAPSACK|LOTUS|PLANTCYC|COCONUT")
}

has_disqualifying_element <- function(formula) {
  formula <- as.character(formula)
  exotic <- str_extract_all(formula, "[A-Z][a-z]?")[[1]]
  # Keep C, H, N, O, S, P, Cl, Br as plausible plant elements
  exotic <- exotic[!exotic %in% c("C", "H", "N", "O", "S", "P", "Cl", "Br")]
  length(exotic) > 0
}

is_deuterated <- function(name) {
  if (!is.na(name)) {
    name <- tolower(as.character(name))
    str_detect(name, "-d\\d|\\[d\\d\\]|deuterio|dideuterio")
  } else {
    FALSE
  }
}

is_in_synth_db_only <- function(links) {
  links <- replace_na(as.character(links), "")
  has_synth <- str_detect(links, "DSSTOX|PUBCHEMANNOTATIONSAFETYANDTOXIC")
  has_plant <- is_from_plant_db(links)
  has_synth & !has_plant
}

score_bio <- function(name, formula, links, rt_min) {
  # Hard disqualifiers
  
  if (has_disqualifying_element(formula)) return(-20)
  if (is_deuterated(name))               return(-30)
  if (is_in_synth_db_only(links))        return(-40)
  
  # Positive scoring
  plant_db    <- is_from_plant_db(links)
  void_volume <- !is.na(rt_min) && rt_min < 2.0
  
  score <- 0
  if (plant_db)     score <- score + 2
  if (!void_volume) score <- score + 1
  
  return(score)
}


# ── Main processing function ────────────────────────────────────────────────

process_lcms_family <- function(family_name,
                                output_dir = NULL) {
  
  # capture data paths
  canopus_path <- paste0("michael/lcmsData/", family_name, "/canopus_formula_summary-10.tsv")
  structure_path <- paste0("michael/lcmsData/", family_name, "/structure_identifications_top-10.tsv")
  alignment_path <- paste0("michael/lcmsData/", family_name, "/", family_name, "Aligned.csv")
  
  # ── 1. Load data ──────────────────────────────────────────────────────────
  
  message("── Processing: ", family_name, " ──")
  
  canopus <- read_tsv(canopus_path, show_col_types = FALSE)
  structure <- read_tsv(structure_path, na = "-Infinity",
                        show_col_types = FALSE) %>%
    select(-any_of("overallFeatureQuality"))
  alignment <- read.csv(alignment_path)
  
  message("  Loaded ", nrow(canopus), " canopus rows, ",
          nrow(structure), " structure rows, ",
          nrow(alignment), " aligned features")
  
  # ── 2. Combine canopus + structure ────────────────────────────────────────
  
  # Keep only top-ranked formula from canopus
  canopus <- canopus %>%
    filter(formulaRank == 1)
  
  siriusOutput <- canopus %>%
    left_join(structure, by = "mappingFeatureId") %>%
    select(-ends_with(".y")) %>%
    rename_with(~str_remove(., "\\.x$"), ends_with(".x"))
  
  message("  Combined output: ", nrow(siriusOutput), " rows")
  
  # ── 3. Score biological plausibility ──────────────────────────────────────
  
  # High-confidence hits: keep rank 1 as-is
  high_conf <- siriusOutput %>%
    filter(structurePerIdRank == 1,
           ConfidenceScoreApproximate >= 0.9) %>%
    mutate(bio_score = 10)
  
  # Everything else: rescore and pick best candidate per feature
  low_conf <- siriusOutput %>%
    filter(ConfidenceScoreApproximate < 0.9 | is.na(ConfidenceScoreApproximate))
  
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
  
  # Combine and keep only biologically plausible compounds
  valid_compounds <- bind_rows(high_conf, low_conf_rescored) %>%
    filter(bio_score > 0)
  
  message("  Valid compounds: ", nrow(valid_compounds),
          " (", nrow(high_conf), " high-confidence, ",
          nrow(valid_compounds) - nrow(high_conf), " rescored)")
  
  # ── 4. Filter to defensive compounds ─────────────────────────────────────
  
  primary_metabolite_classes <- c(
    "Purine nucleosides",
    "Pyrimidine nucleosides",
    "Purine nucleotides",
    "Aminoacids",
    "Dipeptides",
    "Tripeptides",
    "Linear peptides",
    "Cyclic peptides"
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
  
  message("  Defensive compounds: ", nrow(defensive_compounds))
  
  # ── 5. Join area data from alignment ──────────────────────────────────────
  
  area_matrix <- alignment %>%
    select(id, matches("area$")) %>%
    mutate(id = as.character(id))
  
  join_areas <- function(compounds) {
    compounds %>%
      mutate(mappingFeatureId = as.character(mappingFeatureId)) %>%
      left_join(area_matrix, by = c("mappingFeatureId" = "id"))
  }
  
  all_with_areas        <- join_areas(valid_compounds)
  defensive_with_areas  <- join_areas(defensive_compounds)
  
  # ── 6. Report match quality ──────────────────────────────────────────────
  
  area_cols <- names(all_with_areas)[str_detect(names(all_with_areas), "area$")]
  # Exclude the total "area" column if present, keep only sample-level columns
  sample_area_cols <- area_cols[area_cols != "area"]
  
  if (length(sample_area_cols) > 0) {
    n_with_data_all <- rowSums(!is.na(all_with_areas[sample_area_cols]))
    n_with_data_def <- rowSums(!is.na(defensive_with_areas[sample_area_cols]))
    
    message("  Samples: ", length(sample_area_cols))
    message("  All compounds — features with zero sample data: ",
            sum(n_with_data_all == 0), " / ", nrow(all_with_areas))
    message("  Defensive — features with zero sample data: ",
            sum(n_with_data_def == 0), " / ", nrow(defensive_with_areas))
  }
  
  # ── 7. Write outputs (optional) ──────────────────────────────────────────
  
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    all_path <- file.path(output_dir, paste0(family_name, "_all_compounds.csv"))
    def_path <- file.path(output_dir, paste0(family_name, "_defensive_compounds.csv"))
    
    write.csv(all_with_areas, all_path, row.names = FALSE)
    write.csv(defensive_with_areas, def_path, row.names = FALSE)
    
    message("  Saved: ", all_path)
    message("  Saved: ", def_path)
  }
  
  # ── 8. Return both datasets ──────────────────────────────────────────────
  
  message("── Done: ", family_name, " ──\n")
  
  return(list(
    all_compounds       = all_with_areas,
    defensive_compounds = defensive_with_areas
  ))
}

nmds_explore <- function(mat, labs) {
  
  # Log-transform
  mat_log <- log1p(mat)
  
  # NMDS with Bray-Curtis
  set.seed(42)
  nmds <- metaMDS(
    mat_log,
    distance      = "jaccard",
    k             = 2,
    trymax        = 100,
    autotransform = FALSE
  )
  
  message("Stress: ", round(nmds$stress, 4))
  
  # Plot
  scores_df <- as.data.frame(scores(nmds, display = "sites"))
  scores_df$raw_name <- rownames(scores_df)
  
  stress_label <- paste0("Stress = ", round(nmds$stress, 3))
  
  scores_df <- full_join(scores_df, labs, by = "raw_name")
  
  p <- ggplot(scores_df, aes(x = NMDS1, y = NMDS2, label = Binomial)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey60") +
    geom_point(size = 3, aes(color = Provenance)) +
    geom_text_repel(
      size           = 3,
      max.overlaps   = Inf,
      segment.size   = 0.3,
      segment.colour = "grey60",
      box.padding    = 0.4,
      point.padding  = 0.3
    ) +
    stat_ellipse(
      aes(color = Provenance),
      type = "t",        # t-distribution ellipse (better for small samples)
      level = 0.95,
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    annotate("text", x = Inf, y = -Inf, label = stress_label,
             hjust = 1.1, vjust = -0.8, size = 3, colour = "grey40") +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank()) +
    labs(x = "NMDS1", y = "NMDS2")
  
  print(p)
  invisible(nmds)
}

process_names <- function(chem_mat, names_ref = plas){
  nems <- row.names(chem_mat)
  nems_simple <- str_extract(nems, "(?<=^datafile.[[:digit:]]{3}_).*(?=\\.raw\\.area$)")
  nemmos <- data.frame(raw_name = nems, simple_id = nems_simple)
  plas$simple_id <- gsub("\\-", "_", plas$id)
  plas$simple_id <- gsub("\\*", "_", plas$simple_id)
  
  return(right_join(plas, nemmos, by = "simple_id"))
}




