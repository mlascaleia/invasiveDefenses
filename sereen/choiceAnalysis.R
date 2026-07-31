# the purpose of this script is to rank the choices of plant for R. meadii

# initialize
rm(list = ls())
library(dplyr)
library(brms)

# load data

d <- read.csv("sereen/matchups1.csv", stringsAsFactors = FALSE) %>%
# The outgroups were never eaten so no information can be gained from them
  filter((str_detect(yellow, "^(B|M)"))&
         (str_detect(green, "^(B|M)")))

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

fit <- brm(
  eaten | cens(cens) ~ 0 + species + (1 | trial),  # + (1 | pair) can be added with replicates
  data   = long,
  family = gaussian(),          # Tobit: latent normal clipped to [0, 100]
  prior  = c(prior(normal(0.5, 0.4), class = "b"),
             # prior(normal(0, 0.25),  class = "b", coef = "colorgreen"),
             prior(exponential(2),   class = "sd"),
             prior(exponential(2),   class = "sigma")),
  chains = 4, cores = 4, iter = 4000, seed = 42,
  control = list(adapt_delta = 0.95)
)

summary(fit)

# trying with ordbeta
library(ordbetareg)
fit2 <- ordbetareg(
  eaten ~ 0 + species + (1 | trial),
  data = long,
  coef_prior_mean = 1.5,
  coef_prior_SD = 1,
  chains = 4, cores = 4, iter = 6000, seed = 42,
  control = list(adapt_delta = 0.95)
)

summary(fit2)

# extract the ranking

ab <- as.data.frame(fixef(fit2))
ab$term <- rownames(ab)
ab <- ab %>%
  filter(grepl("^species", term)) %>%
  mutate(species  = sub("^species", "", term),
         Estimate = Estimate - mean(Estimate)) %>%
  arrange(desc(Estimate)) %>% 
  select(species, Estimate, Est.Error, Q2.5, Q97.5)
ab

ggplot(ab, aes(x = reorder(species, Estimate), y = Estimate)) +
  geom_pointrange(aes(ymax = Estimate + 1.96 * Est.Error, 
                      ymin = Estimate - 1.96 * Est.Error)) +
  theme_minimal() +
  # theme(axis.text.x = element_text(angle = 90, vjust = .4)) +
  coord_flip()






# some ai-generated junk below


# post <- as_draws_df(fit)
# thun <- post$b_speciesB.thunbergii
# natv <- (post$b_speciesB.canadensis + post$b_speciesM.aquifolium) / 2
# quantile(thun - natv, c(0.025, 0.5, 0.975))
# 
# 
# # -------------------------------------------------------------
# # 6. Confound checks (once the offerings are measured)
# # -------------------------------------------------------------
# # offer: species, area_ratio (measured area / target area),
# #        n_leaves (leaves per standard offering)
# #
# # Two separate uses:
# #   area_ratio -> corrects a per-species bias in the denominator
# #   n_leaves   -> tests whether the ranking is partly a leaf-
# #                 architecture ranking rather than a chemistry one
# #
# # long <- long %>% left_join(offer, by = "species")
# #
# # fit_conf <- update(fit, formula. = ~ . + n_leaves + area_ratio)
# # summary(fit_conf)
# #
# # For the correction proper, convert to absolute area and move the
# # censoring bound per species:
# #   eaten_abs  <- eaten/100 * measured_area
# #   upper_bound <- measured_area   (cens = "right" at that value)
# 
# 
# # -------------------------------------------------------------
# # 7. Structured model: chemistry, then phylogeny
# # -------------------------------------------------------------
# # chem: species, alkaloid, PC1, PC2, ...  (one row per species)
# 
# long2 <- left_join(long, chem, by = "species")
# 
# fit_chem <- brm(
#   eaten | cens(cens) ~ alkaloid + PC1 + PC2 + color +
#     (1 | trial) + (1 | species),
#   data = long2, family = gaussian(),
#   chains = 4, cores = 4, iter = 4000, seed = 1
# )
# 
# # (1|species) is now each species' deviation from what its
# # chemistry predicts. The B. thunbergii level of this term IS the
# # applied residual check - read it off with an interval.
# ranef(fit_chem)$species[, , "Intercept"]
# 
# # Adding phylogeny. A = phylogenetic correlation matrix with
# # rownames = species, e.g. ape::vcv.phylo(tree, corr = TRUE).
# # Two species terms: one structured by the tree, one iid. The iid
# # term is the residual you want; the phylo term is the nuisance.
# fit_phy <- brm(
#   eaten | cens(cens) ~ alkaloid + color +
#     (1 | trial) + (1 | gr(species, cov = A)) + (1 | species2),
#   data = long2, data2 = list(A = A), family = gaussian(),
#   chains = 4, cores = 4, iter = 4000, seed = 1
# )
# 
# 
# # -------------------------------------------------------------
# # 8. Performance (no-choice)
# # -------------------------------------------------------------
# # perf: one row per caterpillar -
# #   species, days, pupated (0/1), pupal_mass, frass_mg
# 
# # Binary survival is completely separated for the four species with
# # zero pupation and cannot be fit. Time-to-event is defined for all
# # 25. Larvae that pupated are right-censored for time-to-death.
# perf <- perf %>%
#   mutate(cens_surv = ifelse(pupated == 1, "right", "none"))
# 
# fit_surv <- brm(
#   days | cens(cens_surv) ~ alkaloid + (1 | species),
#   data = perf, family = weibull(),
#   chains = 4, cores = 4, iter = 4000, seed = 1
# )
# 
# # Pupal mass exists only for survivors - missing for exactly the
# # four worst hosts. Never interpret it without survival alongside.
# fit_mass <- brm(
#   pupal_mass ~ alkaloid + (1 | species),
#   data = filter(perf, pupated == 1), family = gaussian(),
#   chains = 4, cores = 4, iter = 4000, seed = 1
# )
# 
# # Frass is consumption, not quality - compensatory feeding means
# # high frass can mark a poor host. Interpret against pupal_mass.
# fit_frass <- brm(
#   frass_mg ~ alkaloid + (1 | species),
#   data = perf, family = lognormal(),
#   chains = 4, cores = 4, iter = 4000, seed = 1
# )
# 
# 
# # -------------------------------------------------------------
# # 9. Preference vs performance
# # -------------------------------------------------------------
# # Two-stage, because the choice and no-choice data have different
# # row structures. Pull species-level effects with their SEs from
# # each model, then estimate the correlation carrying that
# # uncertainty forward rather than treating the estimates as known.
# 
# get_sp <- function(m, nm) {
#   r <- ranef(m)$species[, , "Intercept"]
#   data.frame(species = rownames(r), est = r[, "Estimate"], se = r[, "Est.Error"]) %>%
#     setNames(c("species", paste0(nm, "_est"), paste0(nm, "_se")))
# }
# 
# sp_tab <- get_sp(fit_chem, "pref") %>%
#   left_join(get_sp(fit_surv, "perf"), by = "species")
# 
# fit_pp <- brm(
#   perf_est | mi(perf_se) ~ me(pref_est, pref_se),
#   data = sp_tab, chains = 4, cores = 4, iter = 4000, seed = 1
# )
# 
# summary(fit_pp)   # the slope is the preference-performance test