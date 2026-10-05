# Robustesse au design : cardinality 1:1 et entropy balancing ATT
#
# Reference : documentation/inference_audit_main_staggered_robustness_prompt.md (Section 17)
#
# Designs de robustesse pre-specifies, benches outcome-blind dans
# scripts/design_diagnostic_allwaves.R (commits diagnose(06)) — AUCUN retuning
# apres observation des outcomes :
#   A. cardinality 1:1 : matchit(method="cardinality", estimand="ATT",
#      ratio=1, tols=0.10, std.tols=TRUE, solver="highs")
#   B. entropy balancing ATT : weightit(method="entropy", estimand="ATT"),
#      premiers moments sur les 5 covariables de matching.
#
# Pour chaque design : placebo 1997-2008 (H1, H2), principal 2008-2021 (H1,
# H2), avec la specification de regression figee (mêmes contrôles, mêmes
# poids, meme clustering) et l'inference primaire geomete (EC clusterises
# fixest). Ces designs ne remplacent jamais la specification primaire.
#
# Sortie : output/review_v2/design_robustness_comparison.csv

library(tidyverse)
library(haven)
library(fixest)
library(MatchIt)
library(cobalt)
library(WeightIt)
library(broom)

matching_variables <- c(
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)

prep_matching <- function(df_final) {
  df_final |>
    dplyr::filter(GROUP %in% c("Treatment", "Control")) |>
    dplyr::mutate(treatment = if_else(GROUP == "Treatment", 1L, 0L)) |>
    tidyr::drop_na(all_of(matching_variables))
}

survey_reference_date <- function(year) as.Date(sprintf("%d-06-01", year))

# --- Designs de robustesse (spec outcome-blind figee des diagnostics) -------
run_card1to1 <- function(dat_m) {
  fml <- reformulate(matching_variables, response = "treatment")
  m <- matchit(
    fml,
    data = dat_m,
    method = "cardinality",
    estimand = "ATT",
    ratio = 1,
    tols = 0.10,
    std.tols = TRUE,
    solver = "highs",
    time = 1800
  )
  matched <- match.data(m, data = sf::st_drop_geometry(dat_m)) |>
    dplyr::filter(weights > 0)
  bt <- cobalt::bal.tab(m, estimand = "ATT", un = TRUE)
  list(
    matched = as.data.frame(matched),
    max_smd = max(abs(bt$Balance$Diff.Adj)),
    treated_retention = 100 *
      sum(matched$treatment == 1L) /
      sum(dat_m$treatment == 1L),
    controls_used = sum(matched$treatment == 0L),
    controls_ess = sum(matched$weights[matched$treatment == 0L])^2 /
      sum(matched$weights[matched$treatment == 0L]^2)
  )
}

run_entropy <- function(dat_m) {
  fml <- reformulate(matching_variables, response = "treatment")
  d <- sf::st_drop_geometry(dat_m)
  w_eb <- weightit(fml, data = d, method = "entropy", estimand = "ATT")
  matched <- d
  matched$weights <- w_eb$weights
  bt <- cobalt::bal.tab(w_eb, un = TRUE)
  w_c <- w_eb$weights[d$treatment == 0L]
  list(
    matched = as.data.frame(matched),
    max_smd = max(abs(bt$Balance$Diff.Adj)),
    treated_retention = 100 * sum(d$treatment == 1L) / sum(d$treatment == 1L),
    controls_used = sum(w_c > 0),
    controls_ess = sum(w_c)^2 / sum(w_c^2)
  )
}

# --- Regression figee (identique au placebo/principal du design profile) ----
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

est_2x2 <- function(dlist, years, yvar) {
  # dlist : liste nommee par vague de data frames apparies bruts
  renamed <- lapply(seq_along(years), function(i) {
    d <- dlist[[as.character(years[i])]]
    d |>
      rename(
        spei_wc_n_2 = !!glue::glue("spei_wc_{years[i] - 2}"),
        spei_wc_n_1 = !!glue::glue("spei_wc_{years[i] - 1}"),
        spei_wc_n = !!glue::glue("spei_wc_{years[i]}")
      ) |>
      mutate(
        hv219 = zap_labels(hv219),
        hv220 = zap_labels(hv220),
        DHSYEAR = years[i]
      )
  })
  dat <- bind_rows(renamed) |>
    filter(GROUP %in% c("Treatment", "Control")) |>
    mutate(
      hv219 = factor(hv219, levels = c(1, 2), labels = c("Homme", "Femme")),
      hv220 = as.numeric(hv220),
      treat = as.integer(GROUP == "Treatment"),
      treatment_date = as.Date(treatment_date),
      survey_ref_date = survey_reference_date(DHSYEAR),
      w_svy = hv005 / 1e6,
      w_all = w_svy * weights,
      cluster_uid = interaction(DHSYEAR, hv001, drop = TRUE)
    )
  pre <- dat |>
    mutate(
      post = as.integer(DHSYEAR == years[2]),
      treat_post = treat * post
    )
  f <- as.formula(paste(yvar, "~ treat + post + treat_post +", fs_controls))
  m <- feols(f, data = pre, weights = ~w_all, cluster = ~cluster_uid)
  used <- stats::complete.cases(pre[, c(
    yvar,
    "w_all",
    "cluster_uid",
    "treat",
    "post",
    controls_pap
  )])
  tibble(
    outcome = yvar,
    estimate = unname(coef(m)["treat_post"]),
    SE = unname(se(m)["treat_post"]),
    CI_low = estimate - 1.96 * SE,
    CI_high = estimate + 1.96 * SE,
    p_value = pvalue(m)["treat_post"],
    N = sum(used),
    clusters = dplyr::n_distinct(pre$cluster_uid[used])
  )
}

# --- Boucle : designs x periodes x outcomes ---------------------------------
waves_needed <- c(1997, 2008, 2021)
periods <- list(placebo = c(1997, 2008), main = c(2008, 2021))
outcomes <- c(H1 = "wealth_centile_rural_weighted", H2 = "zscore_wealth")

# echantillons eligibles par vague (communs aux deux designs)
eligible <- lapply(waves_needed, function(y) {
  prep_matching(readRDS(glue::glue("data/derived/hr_{y}_final.rds")))
})
names(eligible) <- as.character(waves_needed)

matched_by_design <- list(
  card1to1 = lapply(eligible, run_card1to1),
  entropy = lapply(eligible, run_entropy)
)

comparison <- list()
for (design in names(matched_by_design)) {
  waves <- matched_by_design[[design]]
  dlist <- lapply(waves, function(r) r$matched)
  for (p in names(periods)) {
    yrs <- periods[[p]]
    for (h in names(outcomes)) {
      res <- est_2x2(dlist, yrs, outcomes[[h]])
      # retention / soutien : min sur les vagues impliquees (conservateur)
      ret <- min(sapply(yrs, function(y) {
        waves[[as.character(y)]]$treated_retention
      }))
      nctrl <- min(sapply(yrs, function(y) {
        waves[[as.character(y)]]$controls_used
      }))
      ess <- min(sapply(yrs, function(y) waves[[as.character(y)]]$controls_ess))
      smd <- max(sapply(yrs, function(y) waves[[as.character(y)]]$max_smd))
      comparison[[length(comparison) + 1]] <- tibble(
        design = design,
        period = p,
        outcome = res$outcome,
        estimate = res$estimate,
        SE = res$SE,
        CI_low = res$CI_low,
        CI_high = res$CI_high,
        p_value = res$p_value,
        N = res$N,
        clusters = res$clusters,
        treated_retention = ret,
        controls_used_or_ESS = if (design == "card1to1") nctrl else ess,
        max_SMD = smd
      )
    }
  }
}

design_robustness_comparison <- bind_rows(comparison)
dir.create("output/review_v2", showWarnings = FALSE)
write_csv(
  design_robustness_comparison,
  "output/review_v2/design_robustness_comparison.csv"
)

cat("\n===== Comparaison de designs (inference primaire fige) =====\n")
print(design_robustness_comparison, n = 13, width = Inf)
