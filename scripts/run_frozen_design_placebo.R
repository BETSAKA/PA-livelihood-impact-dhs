# Placebo 1997-2008 sur le design fige (profile matching ATT cardinality)
#
# Reproduit EXACTEMENT la specification placebo 2x2 deja etablie dans
# 07-estimation_staggered.qmd (chunks "Placebo 1997-2008" de H1 et H2),
# en consommant les nouveaux echantillons apparies data_matched_1997.rds et
# data_matched_2008.rds du design fige. Aucune redefinition econometrique.
#
# Ce script n'estime PAS l'effet principal 2008-2021 et n'execute pas le
# staggered DiD (07b). Aucun resultat 2008-2021 ne doit apparaitre.
#
# Sorties : output/review_v2/frozen_profile_placebo/
#   placebo_h1.csv, placebo_h2.csv, placebo_sample_summary.csv,
#   placebo_balance_reference.csv
#
# Reference : documentation/freeze_profile_matching_and_run_placebo_prompt.md

library(tidyverse)
library(haven)
library(fixest)
library(broom)

dir.create("output/review_v2/frozen_profile_placebo", showWarnings = FALSE)

# --- Chargement : uniquement les vagues du placebo --------------------------
d97 <- read_rds("data/derived/data_matched_1997.rds") %>%
  rename(
    spei_wc_n_2 = spei_wc_1995,
    spei_wc_n_1 = spei_wc_1996,
    spei_wc_n = spei_wc_1997
  ) %>%
  mutate(
    hv219 = zap_labels(hv219), # hhh sex (1/2)
    hv220 = zap_labels(hv220)
  ) # hhh age (num)

d08 <- read_rds("data/derived/data_matched_2008.rds") %>%
  rename(
    spei_wc_n_2 = spei_wc_2006,
    spei_wc_n_1 = spei_wc_2007,
    spei_wc_n = spei_wc_2008
  ) %>%
  mutate(hv219 = zap_labels(hv219), hv220 = zap_labels(hv220))

# --- Preparation identique a 07-estimation_staggered.qmd --------------------
# Date de reference des vagues (1er juin, meme definition que 01)
survey_reference_date <- function(year) {
  as.Date(sprintf("%d-06-01", year))
}

dat <- bind_rows(d97, d08) %>%
  filter(GROUP %in% c("Treatment", "Control")) %>%
  mutate(
    hv219 = factor(hv219, levels = c(1, 2), labels = c("Homme", "Femme")),
    hv220 = as.numeric(hv220),
    treat = as.integer(GROUP == "Treatment"), # geographie de traitement eventuelle (PAP), pas treated_now
    treatment_date = as.Date(treatment_date),
    survey_ref_date = survey_reference_date(DHSYEAR),
    w_svy = hv005 / 1e6,
    w_all = w_svy * weights, # poids d'enquete x poids de matching (design fige : poids de matching = 1)
    id = row_number(),
    cluster_uid = interaction(DHSYEAR, hv001, drop = TRUE)
  )

stopifnot(
  "placebo : date de traitement manquante pour des observations Treatment" = all(
    !is.na(dat$treatment_date[dat$treat == 1])
  ),
  "placebo : date de traitement presente pour des observations Control" = all(is.na(dat$treatment_date[
    dat$treat == 0
  ]))
)

# --- H1 : placebo 1997-2008 (specification exacte du chunk 07) --------------
yvar_h1 <- "wealth_centile_rural_weighted"

pre <- dat %>%
  filter(DHSYEAR %in% c(1997, 2008)) %>%
  mutate(post = as.integer(DHSYEAR == 2008), treat_post = treat * post)

controls_pap <- c(
  "spei_wc_n_1",
  "hv219",
  "hv220",
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)
fs_controls <- paste(controls_pap, collapse = " + ")

f_pre_h1 <- as.formula(paste(
  yvar_h1,
  "~ treat + post + treat_post +",
  fs_controls
))

h1_m_pre_2x2 <- feols(
  f_pre_h1,
  data = pre,
  weights = ~w_all,
  cluster = ~cluster_uid
)

used_h1 <- stats::complete.cases(
  pre[, c(yvar_h1, "w_all", "cluster_uid", "treat", "post", controls_pap)]
)

h1_res <- summary(h1_m_pre_2x2, vcov = ~cluster_uid) %>%
  broom::tidy() %>%
  filter(term == "treat_post") %>%
  transmute(
    outcome = yvar_h1,
    estimate,
    std.error,
    statistic,
    p.value,
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error,
    n_obs = sum(used_h1),
    n_clusters = dplyr::n_distinct(pre$cluster_uid[used_h1])
  )

# --- H2 : placebo 1997-2008 sur le z-score intra-cluster (spec exacte 07) ---
yvar_h2 <- "zscore_wealth"

f_pre_h2 <- as.formula(paste(
  yvar_h2,
  "~ treat + post + treat_post +",
  "spei_wc_n_1 + hv219 + hv220 + treecover_area_2000 + slope_2000 + elevation_2000 + population_count_2000 + traveltime_2000_2000"
))

h2_m_pre_2x2 <- feols(
  f_pre_h2,
  data = pre,
  weights = ~w_all,
  cluster = ~cluster_uid
)

used_h2 <- stats::complete.cases(
  pre[, c(yvar_h2, "w_all", "cluster_uid", "treat", "post", controls_pap)]
)

h2_res <- summary(h2_m_pre_2x2, vcov = ~cluster_uid) %>%
  broom::tidy() %>%
  filter(term == "treat_post") %>%
  transmute(
    outcome = yvar_h2,
    estimate,
    std.error,
    statistic,
    p.value,
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error,
    n_obs = sum(used_h2),
    n_clusters = dplyr::n_distinct(pre$cluster_uid[used_h2])
  )

# --- Resume d'echantillon apparie (1997 et 2008, design fige) ---------------
sample_summary <- dat %>%
  group_by(DHSYEAR, GROUP) %>%
  summarise(
    n_households = n(),
    n_clusters = dplyr::n_distinct(hv001),
    n_pas = dplyr::n_distinct(WDPAID),
    .groups = "drop"
  )

# --- Reference d'equilibre externe (SMD cobalt ATT du design fige) ----------
mv <- c(
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)

balance_reference <- map_dfr(c(1997, 2008), function(y) {
  dm <- readRDS(glue::glue("data/derived/data_matched_{y}.rds"))
  sel <- dm[dm$weights > 0, ]
  sd_t <- apply(
    as.matrix(
      readRDS(glue::glue("data/derived/hr_{y}_final.rds")) |>
        dplyr::filter(GROUP %in% c("Treatment", "Control")) |>
        dplyr::mutate(treatment = if_else(GROUP == "Treatment", 1L, 0L)) |>
        tidyr::drop_na(all_of(mv)) |>
        sf::st_drop_geometry() |>
        dplyr::filter(treatment == 1L) |>
        dplyr::select(all_of(mv))
    ),
    2,
    sd
  )
  X <- as.matrix(sel[mv])
  tr <- sel$treatment == 1L
  smd <- abs(
    (colMeans(X[tr, , drop = FALSE]) -
      colMeans(X[!tr, , drop = FALSE])) /
      sd_t
  )
  worst <- names(smd)[which.max(smd)]
  tibble(
    year = y,
    covariate = mv,
    smd = round(unname(smd), 6),
    max_smd = round(max(smd), 6),
    worst_covariate = worst
  )
}) %>%
  dplyr::select(year, covariate, smd, max_smd, worst_covariate)

write_csv(h1_res, "output/review_v2/frozen_profile_placebo/placebo_h1.csv")
write_csv(h2_res, "output/review_v2/frozen_profile_placebo/placebo_h2.csv")
write_csv(
  sample_summary,
  "output/review_v2/frozen_profile_placebo/placebo_sample_summary.csv"
)
write_csv(
  dplyr::distinct(balance_reference, year, max_smd, worst_covariate),
  "output/review_v2/frozen_profile_placebo/placebo_balance_reference.csv"
)

cat("\n===== H1 placebo 1997-2008 =====\n")
print(h1_res)
cat("\n===== H2 placebo 1997-2008 =====\n")
print(h2_res)
cat("\n===== Echantillon apparie =====\n")
print(sample_summary)
cat("\n===== Equilibre externe (design fige) =====\n")
print(dplyr::distinct(balance_reference, year, max_smd, worst_covariate))
