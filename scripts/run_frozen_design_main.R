# Effet principal 2008-2021 sur le design fige (profile matching ATT cardinality)
#
# Reproduit EXACTEMENT la specification du placebo fige
# (scripts/run_frozen_design_placebo.R, commitee a 7e561af) en changeant
# uniquement la periode : 2008 = pre, 2021 = post.
#
# Aucune modification du matching, de la tolerance, des controles, des
# outcomes, du clustering ou des poids. Le gel d'inference
# (documentation/inference_design_freeze.md, commit ccf195e) s'applique :
# inference primaire = EC clusterises fixest ; robustesse = CRV3 (clubSandwich
# sur refit lm) et WCR wild cluster bootstrap (fwildclusterboot).
#
# Assertions pre-estimation (PAP) :
#   - toutes les observations 2008 Treatment ont treated_now == 0 ;
#   - toutes les observations 2021 Treatment ont treated_now == 1.
#
# Sorties : output/review_v2/frozen_profile_main/
#   main_h1.csv, main_h2.csv, main_inference_comparison.csv,
#   main_sample_summary.csv, main_balance_reference.csv

library(tidyverse)
library(haven)
library(fixest)
library(clubSandwich)
library(fwildclusterboot)
library(broom)

dir.create("output/review_v2/frozen_profile_main", showWarnings = FALSE)

# --- Chargement : uniquement les vagues du principal ------------------------
d08 <- read_rds("data/derived/data_matched_2008.rds") %>%
  rename(
    spei_wc_n_2 = spei_wc_2006,
    spei_wc_n_1 = spei_wc_2007,
    spei_wc_n = spei_wc_2008
  ) %>%
  mutate(hv219 = zap_labels(hv219), hv220 = zap_labels(hv220))

d21 <- read_rds("data/derived/data_matched_2021.rds") %>%
  rename(
    spei_wc_n_2 = spei_wc_2019,
    spei_wc_n_1 = spei_wc_2020,
    spei_wc_n = spei_wc_2021
  ) %>%
  mutate(hv219 = zap_labels(hv219), hv220 = zap_labels(hv220))

# --- Preparation identique a 07-estimation_staggered.qmd --------------------
survey_reference_date <- function(year) {
  as.Date(sprintf("%d-06-01", year))
}

dat <- bind_rows(d08, d21) %>%
  filter(GROUP %in% c("Treatment", "Control")) %>%
  mutate(
    hv219 = factor(hv219, levels = c(1, 2), labels = c("Homme", "Femme")),
    hv220 = as.numeric(hv220),
    treat = as.integer(GROUP == "Treatment"), # GROUP, pas treated_now (PAP)
    treatment_date = as.Date(treatment_date),
    survey_ref_date = survey_reference_date(DHSYEAR),
    w_svy = hv005 / 1e6,
    w_all = w_svy * weights,
    id = row_number(),
    cluster_uid = interaction(DHSYEAR, hv001, drop = TRUE)
  )

# --- Assertions de bornes (PAP) : STOP si violees ---------------------------
stopifnot(
  "2008 : une observation Treatment a treated_now != 0" = all(
    dat$treated_now[dat$DHSYEAR == 2008 & dat$treat == 1] == 0
  ),
  "2021 : une observation Treatment a treated_now != 1" = all(
    dat$treated_now[dat$DHSYEAR == 2021 & dat$treat == 1] == 1
  ),
  "Control : observation avec date de traitement" = all(
    is.na(dat$treatment_date[dat$treat == 0])
  )
)

# --- Spec figee : identique au placebo, periode 2008-2021 -------------------
pre <- dat %>%
  filter(DHSYEAR %in% c(2008, 2021)) %>%
  mutate(post = as.integer(DHSYEAR == 2021), treat_post = treat * post)

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

# --- H1 : effet principal 2008-2021 ------------------------------------------
yvar_h1 <- "wealth_centile_rural_weighted"

f_main_h1 <- as.formula(paste(
  yvar_h1,
  "~ treat + post + treat_post +",
  fs_controls
))

h1_m_main <- feols(
  f_main_h1,
  data = pre,
  weights = ~w_all,
  cluster = ~cluster_uid
)

used_h1 <- stats::complete.cases(
  pre[, c(yvar_h1, "w_all", "cluster_uid", "treat", "post", controls_pap)]
)

h1_res <- summary(h1_m_main, vcov = ~cluster_uid) %>%
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

# --- H2 : effet principal 2008-2021 sur le z-score ---------------------------
yvar_h2 <- "zscore_wealth"

f_main_h2 <- as.formula(paste(
  yvar_h2,
  "~ treat + post + treat_post +",
  fs_controls
))

h2_m_main <- feols(
  f_main_h2,
  data = pre,
  weights = ~w_all,
  cluster = ~cluster_uid
)

used_h2 <- stats::complete.cases(
  pre[, c(yvar_h2, "w_all", "cluster_uid", "treat", "post", controls_pap)]
)

h2_res <- summary(h2_m_main, vcov = ~cluster_uid) %>%
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

# --- Resume d'echantillon apparie (2008 et 2021, design fige) ---------------
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

balance_reference <- map_dfr(c(2008, 2021), function(y) {
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

# --- Inference geometee : CRV3 et wild cluster bootstrap --------------------
ssc_record <- fixest::ssc()
ssc_note <- sprintf(
  "fixest ssc defaults: K.adj=%s, K.fixef='%s', G.adj=%s, G.df='%s', t.df='%s', K.exact=%s",
  ssc_record$K.adj, ssc_record$K.fixef, ssc_record$G.adj,
  as.character(ssc_record$G.df), as.character(ssc_record$t.df), ssc_record$K.exact
)

pu <- pre[used_h1, ]

# CRV3 : refit lm (pas de fixed effects -> WLS numeriquement identique)
h1_lm <- lm(f_main_h1, data = pre, weights = w_all, subset = used_h1)
h2_lm <- lm(f_main_h2, data = pre, weights = w_all, subset = used_h2)
stopifnot(
  isTRUE(all.equal(unname(coef(h1_lm)["treat_post"]), unname(coef(h1_m_main)["treat_post"]))),
  isTRUE(all.equal(unname(coef(h2_lm)["treat_post"]), unname(coef(h2_m_main)["treat_post"])))
)
cl <- pre$cluster_uid[used_h1]

cr3_row <- function(lm_fit, outcome) {
  V <- vcovCR(lm_fit, cluster = cl, type = "CR3")
  ct <- coef_test(lm_fit, V)
  ct <- ct[rownames(ct) == "treat_post", , drop = FALSE]
  est <- unname(coef(lm_fit)["treat_post"])
  se_v <- unname(sqrt(V["treat_post", "treat_post"]))
  df_v <- as.numeric(ct[["df_Satt"]])
  tibble(
    outcome = outcome,
    method = "CRV3 (clubSandwich, lm refit)",
    estimate = est,
    SE = se_v,
    CI_low = est - qt(0.975, df_v) * se_v,
    CI_high = est + qt(0.975, df_v) * se_v,
    p_value = as.numeric(ct[["p_Satt"]]),
    clusters = n_distinct(cl),
    notes = paste0("CR3 Satterthwaite df = ", round(df_v, 2))
  )
}

# Wild cluster bootstrap : WCR, Rademacher, B = 9999, two-tailed, seed fixe
boot_seed <- 8607
boot_row <- function(fe_fit, outcome) {
  set.seed(boot_seed)
  dqrng::dqset.seed(boot_seed)
  bt <- boottest(
    fe_fit,
    param = "treat_post",
    B = 9999,
    clustid = "cluster_uid",
    type = "rademacher",
    impose_null = TRUE,
    p_val_type = "two-tailed",
    engine = "R",
    conf_int = TRUE
  )
  tb <- tidy(bt)
  tibble(
    outcome = outcome,
    method = "WCR wild cluster bootstrap (rademacher, B=9999, null imposed)",
    estimate = unname(coef(fe_fit)["treat_post"]),
    SE = NA_real_,
    CI_low = tb$conf.low,
    CI_high = tb$conf.high,
    p_value = tb$p.value,
    clusters = n_distinct(cl),
    notes = paste0(
      "seed ", boot_seed, " (set.seed + dqrng::dqset.seed); ",
      ssc_note
    )
  )
}

fixest_row <- function(m, outcome, used) {
  tibble(
    outcome = outcome,
    method = "fixest clustered (PRIMARY)",
    estimate = unname(coef(m)["treat_post"]),
    SE = unname(se(m)["treat_post"]),
    CI_low = estimate - 1.96 * SE,
    CI_high = estimate + 1.96 * SE,
    p_value = pvalue(m)["treat_post"],
    clusters = n_distinct(pre$cluster_uid[used]),
    notes = ssc_note
  )
}

# refits sur le sample complet (sans ligne NA : condition technique boottest)
h1_cc <- feols(f_main_h1, data = pu, weights = ~w_all, cluster = ~cluster_uid)
h2_cc <- feols(f_main_h2, data = pu, weights = ~w_all, cluster = ~cluster_uid)
stopifnot(
  isTRUE(all.equal(unname(coef(h1_cc)["treat_post"]), unname(coef(h1_m_main)["treat_post"]))),
  isTRUE(all.equal(unname(coef(h2_cc)["treat_post"]), unname(coef(h2_m_main)["treat_post"])))
)

main_inference_comparison <- bind_rows(
  fixest_row(h1_m_main, yvar_h1, used_h1),
  cr3_row(h1_lm, yvar_h1),
  boot_row(h1_cc, yvar_h1),
  fixest_row(h2_m_main, yvar_h2, used_h2),
  cr3_row(h2_lm, yvar_h2),
  boot_row(h2_cc, yvar_h2)
)

# --- Exports -----------------------------------------------------------------
write_csv(h1_res, "output/review_v2/frozen_profile_main/main_h1.csv")
write_csv(h2_res, "output/review_v2/frozen_profile_main/main_h2.csv")
write_csv(
  main_inference_comparison,
  "output/review_v2/frozen_profile_main/main_inference_comparison.csv"
)
write_csv(
  sample_summary,
  "output/review_v2/frozen_profile_main/main_sample_summary.csv"
)
write_csv(
  dplyr::distinct(balance_reference, year, max_smd, worst_covariate),
  "output/review_v2/frozen_profile_main/main_balance_reference.csv"
)

cat("\n===== H1 principal 2008-2021 =====\n")
print(h1_res)
cat("\n===== H2 principal 2008-2021 =====\n")
print(h2_res)
cat("\n===== Echantillon apparie =====\n")
print(sample_summary)
cat("\n===== Equilibre externe (design fige) =====\n")
print(dplyr::distinct(balance_reference, year, max_smd, worst_covariate))
cat("\n===== Comparaison d'inference =====\n")
print(main_inference_comparison)
