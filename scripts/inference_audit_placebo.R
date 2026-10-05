# Audit d'inference sur le placebo 1997-2008 du design fige
#
# Reference : documentation/inference_audit_main_staggered_robustness_prompt.md
# (Phases A et B, structure de clusters, checks d'inference alternatifs)
#
# Le script :
#   A. identifie l'observation ecartee du placebo (NA sur une variable RHS)
#      et verifie si la meme missingness peut se produire en 2008/2021 ;
#   B. diagnostique le VCOV clusterise "non PSD" de fixest (eigenvalues brutes,
#      vcov_fix = FALSE) sans modifier la specification ;
#   C. documente la structure de clusters du sample placebo ;
#   D. compare l'inference primaire (fixest clusterise) a CRV3 (clubSandwich
#      sur refit lm, sans fixed effects -> identique) et au wild cluster
#      bootstrap WCR (fwildclusterboot, Rademacher, B = 9999, null imposee).
#
# Aucune decision n'est basee sur la p-value la plus favorable ; l'inference
# primaire reste fixest clusterise (PAP).
#
# Sorties : output/review_v2/inference_audit/
#   regression_missingness_audit.csv, vcov_eigen_audit.csv,
#   placebo_cluster_structure.csv, placebo_inference_comparison.csv

library(tidyverse)
library(haven)
library(fixest)
library(clubSandwich)
library(fwildclusterboot)
library(broom)

dir.create("output/review_v2/inference_audit", showWarnings = FALSE)

# --- Reconstruction exacte du sample placebo (run_frozen_design_placebo.R) ---
d97 <- read_rds("data/derived/data_matched_1997.rds") %>%
  rename(
    spei_wc_n_2 = spei_wc_1995,
    spei_wc_n_1 = spei_wc_1996,
    spei_wc_n = spei_wc_1997
  ) %>%
  mutate(hv219 = zap_labels(hv219), hv220 = zap_labels(hv220))

d08 <- read_rds("data/derived/data_matched_2008.rds") %>%
  rename(
    spei_wc_n_2 = spei_wc_2006,
    spei_wc_n_1 = spei_wc_2007,
    spei_wc_n = spei_wc_2008
  ) %>%
  mutate(hv219 = zap_labels(hv219), hv220 = zap_labels(hv220))

survey_reference_date <- function(year) as.Date(sprintf("%d-06-01", year))

dat <- bind_rows(d97, d08) %>%
  filter(GROUP %in% c("Treatment", "Control")) %>%
  mutate(
    hv219 = factor(hv219, levels = c(1, 2), labels = c("Homme", "Femme")),
    hv220 = as.numeric(hv220),
    treat = as.integer(GROUP == "Treatment"),
    treatment_date = as.Date(treatment_date),
    survey_ref_date = survey_reference_date(DHSYEAR),
    w_svy = hv005 / 1e6,
    w_all = w_svy * weights,
    id = row_number(),
    cluster_uid = interaction(DHSYEAR, hv001, drop = TRUE)
  )

yvar_h1 <- "wealth_centile_rural_weighted"
yvar_h2 <- "zscore_wealth"

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
f_pre_h2 <- as.formula(paste(
  yvar_h2,
  "~ treat + post + treat_post +",
  fs_controls
))

# --- Modeles figes (identiques a run_frozen_design_placebo.R) ----------------
h1_m_pre_2x2 <- feols(
  f_pre_h1,
  data = pre,
  weights = ~w_all,
  cluster = ~cluster_uid
)
h2_m_pre_2x2 <- feols(
  f_pre_h2,
  data = pre,
  weights = ~w_all,
  cluster = ~cluster_uid
)

used_h1 <- stats::complete.cases(pre[, c(
  yvar_h1,
  "w_all",
  "cluster_uid",
  "treat",
  "post",
  controls_pap
)])
used_h2 <- stats::complete.cases(pre[, c(
  yvar_h2,
  "w_all",
  "cluster_uid",
  "treat",
  "post",
  controls_pap
)])

# --- Verification de reproduction vs sorties commitees ----------------------
ref_h1 <- read_csv(
  "output/review_v2/frozen_profile_placebo/placebo_h1.csv",
  show_col_types = FALSE
)
ref_h2 <- read_csv(
  "output/review_v2/frozen_profile_placebo/placebo_h2.csv",
  show_col_types = FALSE
)
stopifnot(
  "H1 : le point estime ne reproduit pas la sortie commitee" = isTRUE(all.equal(
    unname(coef(h1_m_pre_2x2)["treat_post"]),
    ref_h1$estimate
  )),
  "H1 : l'EC ne reproduit pas la sortie commitee" = isTRUE(all.equal(
    unname(se(h1_m_pre_2x2)["treat_post"]),
    ref_h1$std.error,
    tolerance = 1e-6
  )),
  "H2 : le point estime ne reproduit pas la sortie commitee" = isTRUE(all.equal(
    unname(coef(h2_m_pre_2x2)["treat_post"]),
    ref_h2$estimate
  )),
  "H2 : l'EC ne reproduit pas la sortie commitee" = isTRUE(all.equal(
    unname(se(h2_m_pre_2x2)["treat_post"]),
    ref_h2$std.error,
    tolerance = 1e-6
  ))
)

# --- Phase A : observation ecartee (NA sur une variable RHS) -----------------
find_dropped <- function(used) {
  d <- pre[!used, ]
  rhs_num <- c(
    "spei_wc_n_1",
    "hv220",
    "treecover_area_2000",
    "slope_2000",
    "elevation_2000",
    "population_count_2000",
    "traveltime_2000_2000"
  )
  miss_var <- apply(d[, rhs_num, drop = FALSE], 1, function(x) {
    v <- rhs_num[is.na(x)]
    if (length(v)) paste(v, collapse = "|") else NA_character_
  })
  # hv219 (facteur) verifie separement
  if (is.na(d$hv219)) {
    miss_var <- ifelse(
      is.na(miss_var),
      "hv219",
      paste(miss_var, "hv219", sep = "|")
    )
  }
  tibble(
    DHSYEAR = d$DHSYEAR,
    GROUP = d$GROUP,
    hv001 = d$hv001,
    hv002 = d$hv002,
    WDPAID = d$WDPAID,
    treat_status = d$GROUP,
    missing_variable = miss_var
  )
}

dropped_h1 <- find_dropped(used_h1)
dropped_h2 <- find_dropped(used_h2)
stopifnot(
  "la meme observation est ecartee pour H1 et H2" = identical(
    dropped_h1,
    dropped_h2
  )
)

# Missingness RHS dans les vagues disponibles du design fige
audit_missingness_wave <- function(y, t1_var) {
  d <- read_rds(glue::glue("data/derived/data_matched_{y}.rds")) %>%
    mutate(hv219 = zap_labels(hv219), hv220 = as.numeric(zap_labels(hv220)))
  vars <- c(
    t1_var,
    "hv220",
    "treecover_area_2000",
    "slope_2000",
    "elevation_2000",
    "population_count_2000",
    "traveltime_2000_2000"
  )
  n_miss <- sum(rowSums(is.na(d[, vars])) > 0 | is.na(d$hv219))
  tibble(
    DHSYEAR = y,
    matched_n = nrow(d),
    n_missing_rhs = n_miss,
    share = n_miss / nrow(d)
  )
}

missness_waves <- bind_rows(
  audit_missingness_wave(1997, "spei_wc_1996"),
  audit_missingness_wave(2008, "spei_wc_2007"),
  audit_missingness_wave(2021, "spei_wc_2020")
)

regression_missingness_audit <- bind_rows(
  dropped_h1 %>%
    transmute(
      check = "dropped_observation_placebo",
      DHSYEAR,
      GROUP,
      hv001,
      hv002,
      WDPAID,
      missing_variable,
      matched_sample_n = nrow(pre),
      regression_n = sum(used_h1),
      exclusion_share = 1 - mean(used_h1),
      note = "1 observation ecartee par fixest (NA sur hv220) ; part << 2% du seuil PAP"
    ),
  missness_waves %>%
    transmute(
      check = "wave_missingness_screen",
      DHSYEAR,
      GROUP = NA_character_,
      hv001 = NA,
      hv002 = NA,
      WDPAID = NA,
      missing_variable = NA_character_,
      matched_sample_n = matched_n,
      regression_n = matched_n - n_missing_rhs,
      exclusion_share = n_missing_rhs / matched_n,
      note = "verification ex-ante de la missingness RHS par vague (design fige)"
    )
)

write_csv(
  regression_missingness_audit,
  "output/review_v2/inference_audit/regression_missingness_audit.csv"
)

# --- Phase B : diagnostic eigenvalue du VCOV clusterise brut -----------------
eig_audit <- function(m, label) {
  V_raw <- vcov(m, vcov = ~cluster_uid, vcov_fix = FALSE, attr = TRUE)
  ev <- eigen(V_raw, symmetric = TRUE, only.values = TRUE)$values
  neg <- ev[ev < 0]
  did_var <- V_raw["treat_post", "treat_post"]
  se_fixed <- vcov(m, vcov = ~cluster_uid)["treat_post", "treat_post"]
  tibble(
    outcome = label,
    n_eigenvalues = length(ev),
    n_negative_eigenvalues = sum(ev < 0),
    n_below_fixest_threshold_1e12 = sum(ev < 1e-12),
    min_eigenvalue = min(ev),
    max_eigenvalue = max(ev),
    ratio_abs_neg_over_max_pos = if (length(neg)) {
      abs(min(neg)) / max(ev)
    } else {
      0
    },
    sum_negative_eigenvalues = if (length(neg)) sum(neg) else 0,
    did_var_positive_before_repair = did_var > 0,
    se_did_before_repair = if (did_var > 0) sqrt(did_var) else NA_real_,
    se_did_after_fix = se_fixed
  )
}

vcov_eigen_audit <- bind_rows(
  eig_audit(h1_m_pre_2x2, yvar_h1),
  eig_audit(h2_m_pre_2x2, yvar_h2)
)
write_csv(
  vcov_eigen_audit,
  "output/review_v2/inference_audit/vcov_eigen_audit.csv"
)

# --- Structure de clusters du placebo ---------------------------------------
pu <- pre[used_h1, ]
clus_tbl <- pu %>%
  group_by(cluster_uid, DHSYEAR, treat) %>%
  summarise(n_hh = n(), w_mass = sum(w_all), .groups = "drop")

q <- quantile(clus_tbl$n_hh, c(0, .25, .5, .75, .9, 1))
placebo_cluster_structure <- tibble(
  metric = c(
    "clusters_total_used",
    "clusters_1997_treated",
    "clusters_1997_control",
    "clusters_2008_treated",
    "clusters_2008_control",
    "hh_per_cluster_min",
    "hh_per_cluster_p25",
    "hh_per_cluster_median",
    "hh_per_cluster_p75",
    "hh_per_cluster_p90",
    "hh_per_cluster_max",
    "w_mass_max_cluster_share",
    "w_mass_top5_cluster_share"
  ),
  value = unlist(c(
    n_distinct(pu$cluster_uid),
    n_distinct(clus_tbl$cluster_uid[
      clus_tbl$DHSYEAR == 1997 & clus_tbl$treat == 1
    ]),
    n_distinct(clus_tbl$cluster_uid[
      clus_tbl$DHSYEAR == 1997 & clus_tbl$treat == 0
    ]),
    n_distinct(clus_tbl$cluster_uid[
      clus_tbl$DHSYEAR == 2008 & clus_tbl$treat == 1
    ]),
    n_distinct(clus_tbl$cluster_uid[
      clus_tbl$DHSYEAR == 2008 & clus_tbl$treat == 0
    ]),
    as.list(q),
    max(clus_tbl$w_mass) / sum(clus_tbl$w_mass),
    sum(sort(clus_tbl$w_mass, decreasing = TRUE)[1:5]) / sum(clus_tbl$w_mass)
  ))
)
write_csv(
  placebo_cluster_structure,
  "output/review_v2/inference_audit/placebo_cluster_structure.csv"
)

# --- Inference alternative : CRV3 et wild cluster bootstrap ------------------
# ssc de fixest par defaut (documente) :
#   ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, G.df = "min",
#       t.df = "min", K.exact = FALSE)
ssc_record <- fixest::ssc()
ssc_note <- sprintf(
  "fixest ssc defaults: K.adj=%s, K.fixef='%s', G.adj=%s, G.df='%s', t.df='%s', K.exact=%s",
  ssc_record$K.adj,
  ssc_record$K.fixef,
  ssc_record$G.adj,
  as.character(ssc_record$G.df),
  as.character(ssc_record$t.df),
  ssc_record$K.exact
)

# CRV3 : refit lm (pas de fixed effects -> WLS numeriquement identique)
h1_lm <- lm(f_pre_h1, data = pre, weights = w_all, subset = used_h1)
h2_lm <- lm(f_pre_h2, data = pre, weights = w_all, subset = used_h2)
stopifnot(
  isTRUE(all.equal(
    unname(coef(h1_lm)["treat_post"]),
    unname(coef(h1_m_pre_2x2)["treat_post"])
  )),
  isTRUE(all.equal(
    unname(coef(h2_lm)["treat_post"]),
    unname(coef(h2_m_pre_2x2)["treat_post"])
  ))
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
# (set.seed + dqrng::dqset.seed, requis par fwildclusterboot >= 0.13)
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
      "seed ",
      boot_seed,
      " (set.seed + dqrng::dqset.seed); ",
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

# refits sur le sample complet (sans ligne NA : boottest exige des donnees
# sans valeur manquante interne) — numeriquement identiques aux modeles figes
h1_cc <- feols(
  f_pre_h1,
  data = pu,
  weights = ~w_all,
  cluster = ~cluster_uid
)
h2_cc <- feols(
  f_pre_h2,
  data = pu,
  weights = ~w_all,
  cluster = ~cluster_uid
)
stopifnot(
  isTRUE(all.equal(
    unname(coef(h1_cc)["treat_post"]),
    unname(coef(h1_m_pre_2x2)["treat_post"])
  )),
  isTRUE(all.equal(
    unname(coef(h2_cc)["treat_post"]),
    unname(coef(h2_m_pre_2x2)["treat_post"])
  ))
)

placebo_inference_comparison <- bind_rows(
  fixest_row(h1_m_pre_2x2, yvar_h1, used_h1),
  cr3_row(h1_lm, yvar_h1),
  boot_row(h1_cc, yvar_h1),
  fixest_row(h2_m_pre_2x2, yvar_h2, used_h2),
  cr3_row(h2_lm, yvar_h2),
  boot_row(h2_cc, yvar_h2)
)
write_csv(
  placebo_inference_comparison,
  "output/review_v2/inference_audit/placebo_inference_comparison.csv"
)

cat("\n===== Audit de missingness =====\n")
print(regression_missingness_audit)
cat("\n===== Audit eigenvalues VCOV brut =====\n")
print(vcov_eigen_audit)
cat("\n===== Structure de clusters =====\n")
print(placebo_cluster_structure)
cat("\n===== Comparaison d'inference =====\n")
print(placebo_inference_comparison)
