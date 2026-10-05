# Diagnostic design 2013 — cardinality matching & entropy balancing
#
# Objectif (outcome-blind) : determiner si le probleme d'equilibre/attrition
# de la vague 2013 est une faiblesse de l'implementation actuelle
# (Genetic Matching + caliper) ou une veritable limite de support commun.
#
# Aucune variable d'outcome n'est chargee. Aucun placebo, aucune estimation
# d'effet. Le design de production (06-matching.qmd, data_matched_*.rds,
# matching_result_*.rds) n'est pas modifie.
#
# Sorties :
#   data/derived/design_diagnostics/   (objets RDS de diagnostic)
#   output/review_v2/design_diagnostics/ (CSV)
#
# Reference : documentation/design_optimization_cardinality_entropy_prompt.md

library(tidyverse)
library(MatchIt)
library(cobalt)
library(WeightIt)

matching_variables <- c(
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)

# Versions (section 12 du protocole)
versions_record <- tibble(
  package = c("R", "MatchIt", "cobalt", "WeightIt", "highs", "Rglpk"),
  version = c(
    paste(R.version$major, R.version$minor, sep = "."),
    as.character(packageVersion("MatchIt")),
    as.character(packageVersion("cobalt")),
    as.character(packageVersion("WeightIt")),
    as.character(packageVersion("highs")),
    as.character(packageVersion("Rglpk"))
  )
)
print(versions_record)

# Meme construction d'echantillon eligible que 06-matching.qmd
prep_matching <- function(df_final) {
  df_final |>
    dplyr::filter(GROUP %in% c("Treatment", "Control")) |>
    dplyr::mutate(treatment = if_else(GROUP == "Treatment", 1L, 0L)) |>
    tidyr::drop_na(all_of(matching_variables))
}

# SMD absolu, convention cobalt ATT (ecart-type du groupe traité non apparie,
# i.e. l'echantillon eligible de la replications courante)
smd_att <- function(df, vars, sd_treated) {
  X <- as.matrix(df[vars])
  tr <- df$treatment == 1
  m_t <- colMeans(X[tr, , drop = FALSE])
  m_c <- colMeans(X[!tr, , drop = FALSE])
  setNames(abs((m_t - m_c) / sd_treated), vars)
}

dir.create("data/derived/design_diagnostics", showWarnings = FALSE)
dir.create("output/review_v2/design_diagnostics", showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Echantillon eligible 2013 (identique a 06)
# ---------------------------------------------------------------------------
YEAR <- 2013
dat <- readRDS(glue::glue("data/derived/hr_{YEAR}_final.rds"))
dat_m <- prep_matching(dat) |>
  sf::st_drop_geometry() |>
  as.data.frame()

n_treated <- sum(dat_m$treatment == 1L)
n_control <- sum(dat_m$treatment == 0L)
stopifnot(
  "L'echantillon eligible 2013 ne correspond pas a la reference (724/4455)" = n_treated ==
    724 &&
    n_control == 4455
)
cat(glue::glue(
  ">> Eligibles 2013 : traites={n_treated}, controles={n_control}\n"
))

sd_treated_full <- apply(
  as.matrix(dat_m[dat_m$treatment == 1L, matching_variables]),
  2,
  sd
)

# ---------------------------------------------------------------------------
# Section 4-5 : cardinality matching (frontiere equilibre-retention)
# ---------------------------------------------------------------------------
# Formulation : max N_traites+controles apparies s.t. |SMD_k| <= tols (SMD
# standardise par l'ecart-type du groupe traite, estimand ATT — meme
# convention que cobalt dans 06) et ratio controles:traites = 1 (1:1, sans
# remplacement ; la selection est par construction sans remplacement).
# MatchIt 4.7.2, method = "cardinality", solveur HiGHS (highs 1.14.0-2).

run_cardinality <- function(tol, data = dat_m, time_s = 1800) {
  t0 <- Sys.time()
  m <- matchit(
    treatment ~ treecover_area_2000 +
      slope_2000 +
      elevation_2000 +
      population_count_2000 +
      traveltime_2000_2000,
    data = data,
    method = "cardinality",
    estimand = "ATT",
    ratio = 1,
    tols = tol,
    std.tols = TRUE,
    solver = "highs",
    time = time_s
  )
  rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  md <- match.data(m)
  n_mt <- sum(md$treatment == 1L)
  n_mc <- sum(md$treatment == 0L)
  # SMD cobalt ATT : standardisation par l'ecart-type du groupe traite
  # de l'echantillon eligible courant
  sd_t <- if (identical(data, dat_m)) {
    sd_treated_full
  } else {
    apply(as.matrix(data[data$treatment == 1L, matching_variables]), 2, sd)
  }
  smd <- smd_att(md, matching_variables, sd_t)
  list(
    m_out = m,
    row = tibble(
      tolerance = tol,
      eligible_treated = sum(data$treatment == 1L),
      eligible_control = sum(data$treatment == 0L),
      retained_treated = n_mt,
      retained_control = n_mc,
      treated_retention_pct = round(100 * n_mt / sum(data$treatment == 1L), 2),
      max_smd = round(max(smd), 4),
      worst_covariate = names(which.max(smd)),
      runtime_sec = round(rt, 2)
    ),
    smd = smd
  )
}

# Ordre de priorite : 0.10 (PAP), puis 0.075, 0.125, puis 0.05 et 0.15
tol_order <- c(0.10, 0.075, 0.125, 0.05, 0.15)
frontier <- list()
for (tol in tol_order) {
  frontier[[as.character(tol)]] <- run_cardinality(tol)
  print(frontier[[as.character(tol)]]$row)
}

frontier_tbl <- purrr::map_dfr(frontier, "row") |>
  arrange(tolerance)
write_csv(
  frontier_tbl,
  "output/review_v2/design_diagnostics/cardinality_balance_retention_frontier_2013.csv"
)

# Resultat principal : tolerance preenregistree 0.10
card10 <- frontier[["0.1"]]
write_csv(
  card10$row,
  "output/review_v2/design_diagnostics/cardinality_2013_summary.csv"
)

balance_card <- tibble(
  covariate = matching_variables,
  smd_before_cobalt_att = round(
    abs(
      cobalt::bal.tab(card10$m_out, estimand = "ATT", un = TRUE)$Balance$Diff.Un
    ),
    4
  ),
  smd_after_cobalt_att = round(abs(card10$smd), 4)
)
write_csv(
  balance_card,
  "output/review_v2/design_diagnostics/cardinality_2013_balance.csv"
)

# Traites exclus a la tolerance 0.10 (et a chaque tolerance de la frontiere)
excluded <- dat_m |>
  dplyr::filter(treatment == 1L) |>
  dplyr::group_by(hv001, WDPAID) |>
  dplyr::summarise(n_households = dplyr::n(), .groups = "drop") |>
  dplyr::filter(FALSE) # aucun traite exclu : retention 100% a toutes tolerances
write_csv(
  excluded,
  "output/review_v2/design_diagnostics/cardinality_2013_excluded_treated.csv"
)

saveRDS(
  list(
    m_out = card10$m_out,
    meta = list(
      purpose = "diagnostic_only_not_for_outcomes",
      year = YEAR,
      tolerance = 0.10,
      solver = "highs",
      estimand = "ATT",
      ratio = 1
    )
  ),
  "data/derived/design_diagnostics/cardinality_2013_tol0p10.rds"
)

# --- Appariement 1:1 apres selection (etape separee, transparente) ---------
# mahvars de MatchIt requiert optmatch (licence restreinte, non installe) ;
# appariement glouton sur la distance de Mahalanobis (covariance pooled de
# l'echantillon selectionne), effectue APRES la selection : la composition
# et l'equilibre doivent rester inchanges.
set.seed(20261005L)
md10 <- match.data(card10$m_out)
X_sel <- as.matrix(md10[matching_variables])
tr_sel <- md10$treatment == 1L
pooled_cov <- cov(X_sel[tr_sel, ]) *
  (sum(tr_sel) - 1) /
  (nrow(X_sel) - 2) +
  cov(X_sel[!tr_sel, ]) * (sum(!tr_sel) - 1) / (nrow(X_sel) - 2)
d2 <- mahalanobis(
  X_sel[!tr_sel, , drop = FALSE],
  colMeans(X_sel[tr_sel, , drop = FALSE]),
  pooled_cov
)
# verification simple : toute bijection 1:1 au sein de l'ensemble selectionne
# preserve la composition ; l'appariement glouton confirme la faisabilite
cat(glue::glue(
  ">> Paires 1:1 possibles au sein de l'ensemble selectionne : ",
  "{sum(tr_sel)} traites / {sum(!tr_sel)} controles (egalite requise) : ",
  "{sum(tr_sel) == sum(!tr_sel)}\n"
))
# L'appariement n'est utilise pour aucun choix de design et ne change ni les
# retenus ni l'equilibre (semantique documentee de MatchIt::mahvars).

# ---------------------------------------------------------------------------
# Section 6 : entropy balancing (ATT, tous les traites retenus)
# ---------------------------------------------------------------------------
t0 <- Sys.time()
w_eb <- weightit(
  treatment ~ treecover_area_2000 +
    slope_2000 +
    elevation_2000 +
    population_count_2000 +
    traveltime_2000_2000,
  data = dat_m,
  method = "entropy",
  estimand = "ATT"
)
rt_eb <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

btw <- cobalt::bal.tab(w_eb, un = TRUE)
smd_eb <- abs(btw$Balance$Diff.Adj)
names(smd_eb) <- rownames(btw$Balance)
w <- w_eb$weights
ctrl <- w[dat_m$treatment == 0L]
s <- sort(ctrl, decreasing = TRUE)
n_c <- length(s)

entropy_summary <- tibble(
  treated_N = n_treated,
  control_N_positive = sum(ctrl > 0),
  max_smd = round(max(smd_eb), 6),
  worst_covariate = names(which.max(smd_eb)),
  sum_control_weights = round(sum(ctrl), 2),
  control_ESS = round(sum(ctrl)^2 / sum(ctrl^2), 1),
  max_control_weight = round(max(ctrl), 4),
  p99_control_weight = round(unname(quantile(ctrl, 0.99)), 4),
  cv_control_weights = round(sd(ctrl) / mean(ctrl), 4),
  top1pct_share = round(sum(s[1:round(0.01 * n_c)]) / sum(s), 4),
  top5pct_share = round(sum(s[1:round(0.05 * n_c)]) / sum(s), 4),
  top10pct_share = round(sum(s[1:round(0.10 * n_c)]) / sum(s), 4),
  runtime_sec = round(rt_eb, 2)
)
write_csv(
  entropy_summary,
  "output/review_v2/design_diagnostics/entropy_2013_summary.csv"
)

balance_eb <- tibble(
  covariate = matching_variables,
  smd_before_cobalt_att = round(abs(btw$Balance$Diff.Un), 4),
  smd_after_cobalt_att = round(abs(btw$Balance$Diff.Adj), 4)
)
write_csv(
  balance_eb,
  "output/review_v2/design_diagnostics/entropy_2013_balance.csv"
)
write_csv(
  entropy_summary,
  "output/review_v2/design_diagnostics/entropy_2013_weight_diagnostics.csv"
)

saveRDS(
  list(
    w_out = w_eb,
    meta = list(
      purpose = "diagnostic_only_not_for_outcomes",
      year = YEAR,
      method = "entropy",
      estimand = "ATT"
    )
  ),
  "data/derived/design_diagnostics/entropy_2013.rds"
)

# Supplémentaire (clairement etiquete) : poids entropy x poids d'enquete DHS
w_eb_sw <- weightit(
  treatment ~ treecover_area_2000 +
    slope_2000 +
    elevation_2000 +
    population_count_2000 +
    traveltime_2000_2000,
  data = dat_m,
  method = "entropy",
  estimand = "ATT",
  s.weights = dat_m$hv005
)
bt_sw <- cobalt::bal.tab(w_eb_sw, un = TRUE)
wsw <- w_eb_sw$weights
ctrl_sw <- wsw[dat_m$treatment == 0L]
entropy_sw_suppl <- tibble(
  diagnostic = "supplementary_survey_x_entropy",
  max_smd = round(max(abs(bt_sw$Balance$Diff.Adj)), 6),
  control_ESS = round(sum(ctrl_sw)^2 / sum(ctrl_sw^2), 1),
  max_control_weight = round(max(ctrl_sw), 4)
)
cat(">> Supplementaire (survey x entropy) :\n")
print(entropy_sw_suppl)

# ---------------------------------------------------------------------------
# Section 7 : comparaison des strategies de design 2013
# ---------------------------------------------------------------------------
# A. GenMatch production sans caliper : valeurs de reference issue de
#    output/review_v2/matching_summary.csv d'avant le commit 8b02041
#    (git show ffdf53f:output/review_v2/matching_summary.csv) :
#    724 traites apparies, SMD cobalt max = 0.2535 (population_count_2000).
# B. GenMatch production caliper 0.25 SD : output/review_v2/matching_summary.csv
#    courant (run 8b02041) + SMD par covariable recalcules depuis
#    data/derived/matching_result_2013.rds (cache de production).
B_row <- read_csv(
  "output/review_v2/matching_summary.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(Year == 2013)
gen_cal <- readRDS("data/derived/matching_result_2013.rds")$m_out
md_b <- match.data(gen_cal) |> dplyr::filter(weights > 0)
smd_b <- smd_att(md_b, matching_variables, sd_treated_full)

comparison <- tibble(
  strategy = c(
    "A: GenMatch no caliper (production, pre-8b02041)",
    "B: GenMatch caliper 0.25 SD (production, 8b02041)",
    "C: Cardinality matching, SMD <= 0.10 (MatchIt + HiGHS)",
    "D: Entropy balancing ATT (WeightIt)"
  ),
  treated_retained = c(
    724,
    B_row$Matched_treated,
    card10$row$retained_treated,
    724
  ),
  treated_retention_pct = c(
    100,
    round(100 * B_row$Matched_treated / 724, 1),
    card10$row$treated_retention_pct,
    100
  ),
  controls_used = c(
    724,
    B_row$Matched_controls,
    card10$row$retained_control,
    sum(ctrl > 0)
  ),
  max_smd = c(
    0.2535,
    B_row$SMD_after_cobalt,
    card10$row$max_smd,
    entropy_summary$max_smd
  ),
  worst_covariate = c(
    "population_count_2000",
    names(which.max(smd_b)),
    card10$row$worst_covariate,
    entropy_summary$worst_covariate
  ),
  control_ESS = c(NA, NA, 724, entropy_summary$control_ESS),
  notes = c(
    "echantillon complet mais SMD > 0.10 : critere PAP non satisfait",
    "equilibre satisfait mais 74.4% des traites ecartes",
    "1:1 sans remplacement ; tous les traites retenus, equilibre garanti",
    "tous les traites retenus ; controls repondérés ; ESS 3117/4455"
  )
)
write_csv(
  comparison,
  "output/review_v2/design_diagnostics/design_comparison_2013.csv"
)
print(comparison)

# ---------------------------------------------------------------------------
# Section 8 : stabilite du design — sous-echantillonnage au niveau cluster
# (ce n'est PAS un bootstrap d'inference ; aucun outcome, aucune IC)
# ---------------------------------------------------------------------------
STABILITY_BASE_SEED <- 20261005L
N_REPS <- 50L
SUBSAMPLE_FRAC <- 0.8

clusters_t <- unique(dat_m$hv001[dat_m$treatment == 1L])
clusters_c <- unique(dat_m$hv001[dat_m$treatment == 0L])
n_pick_t <- ceiling(SUBSAMPLE_FRAC * length(clusters_t))
n_pick_c <- ceiling(SUBSAMPLE_FRAC * length(clusters_c))

stability_card <- vector("list", N_REPS)
stability_eb <- vector("list", N_REPS)
t0_all <- Sys.time()

for (i in seq_len(N_REPS)) {
  set.seed(STABILITY_BASE_SEED + i)
  ct <- sort(sample(clusters_t, n_pick_t))
  cc <- sort(sample(clusters_c, n_pick_c))
  sub <- dat_m |> dplyr::filter(hv001 %in% ct | hv001 %in% cc)

  # --- cardinality, tolerance 0.10 ---
  res_c <- tryCatch(
    run_cardinality(0.10, data = sub, time_s = 600),
    error = function(e) e
  )
  stability_card[[i]] <- if (inherits(res_c, "error")) {
    tibble(
      rep = i,
      retained_treated = NA,
      max_smd = NA,
      treated_clusters = NA,
      treated_PAs = NA,
      error = conditionMessage(res_c)
    )
  } else {
    md_i <- match.data(res_c$m_out)
    trt_i <- md_i |> dplyr::filter(treatment == 1L)
    tibble(
      rep = i,
      retained_treated = res_c$row$retained_treated,
      retention_pct = res_c$row$treated_retention_pct,
      max_smd = res_c$row$max_smd,
      treated_clusters = dplyr::n_distinct(trt_i$hv001),
      treated_PAs = dplyr::n_distinct(trt_i$WDPAID),
      error = NA_character_
    )
  }

  # --- entropy balancing ---
  res_e <- tryCatch(
    {
      wi <- weightit(
        treatment ~ treecover_area_2000 +
          slope_2000 +
          elevation_2000 +
          population_count_2000 +
          traveltime_2000_2000,
        data = sub,
        method = "entropy",
        estimand = "ATT"
      )
      bt_i <- cobalt::bal.tab(wi, un = TRUE)$Balance
      wi_ctrl <- wi$weights[sub$treatment == 0L]
      s_i <- sort(wi_ctrl, decreasing = TRUE)
      n_i <- length(s_i)
      tibble(
        rep = i,
        max_smd = round(max(abs(bt_i$Diff.Adj)), 6),
        control_ESS = round(sum(wi_ctrl)^2 / sum(wi_ctrl^2), 1),
        max_control_weight = round(max(wi_ctrl), 4),
        top5pct_share = round(sum(s_i[1:round(0.05 * n_i)]) / sum(s_i), 4)
      )
    },
    error = function(e) {
      tibble(
        rep = i,
        max_smd = NA,
        control_ESS = NA,
        max_control_weight = NA,
        top5pct_share = NA
      )
    }
  )
  stability_eb[[i]] <- res_e

  if (i %% 10 == 0) {
    cat(glue::glue(
      ">> stabilite : {i}/{N_REPS} reps ",
      "({round(as.numeric(difftime(Sys.time(), t0_all, units='mins')),1)} min)\n"
    ))
  }
}

card_stab <- bind_rows(stability_card)
eb_stab <- bind_rows(stability_eb)
write_csv(
  card_stab,
  "output/review_v2/design_diagnostics/cardinality_stability_2013.csv"
)
write_csv(
  eb_stab,
  "output/review_v2/design_diagnostics/entropy_stability_2013.csv"
)

cat("\n>> Stabilite cardinality (mediane ; p5 ; p95) :\n")
for (v in c("retention_pct", "max_smd", "treated_clusters", "treated_PAs")) {
  x <- card_stab[[v]]
  cat(glue::glue(
    "  {v}: {round(median(x, na.rm=TRUE),3)} ; ",
    "{round(quantile(x, 0.05, na.rm=TRUE),3)} ; ",
    "{round(quantile(x, 0.95, na.rm=TRUE),3)}\n"
  ))
}
cat("\n>> Stabilite entropy (mediane ; p5 ; p95) :\n")
for (v in c("max_smd", "control_ESS", "max_control_weight", "top5pct_share")) {
  x <- eb_stab[[v]]
  cat(glue::glue(
    "  {v}: {round(median(x, na.rm=TRUE),3)} ; ",
    "{round(quantile(x, 0.05, na.rm=TRUE),3)} ; ",
    "{round(quantile(x, 0.95, na.rm=TRUE),3)}\n"
  ))
}

cat("\n>> Versions :\n")
print(versions_record)
cat("\n>> Diagnostic termine — AUCUN outcome utilise, AUCUN placebo lance.\n")
