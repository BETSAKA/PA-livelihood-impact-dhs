# Diagnostic design toutes vagues — cardinality 1:1, profile matching ATT,
# entropy balancing ATT
#
# Objectif (outcome-blind) : verifier si le resultat 2013 (cardinality retient
# tous les traites, entropy sain) se generalise aux 7 vagues DHS, avant de
# figer la regle de matching.
#
# AUCUNE variable d'outcome n'est utilisee dans un choix de design. Aucun
# placebo, aucune estimation d'effet. Le design de production (06-matching.qmd,
# data_matched_*.rds, matching_result_*.rds) n'est ni modifie ni remplace.
#
# Sorties :
#   data/derived/design_diagnostics_allwaves/    (RDS de diagnostic)
#   output/review_v2/design_diagnostics_allwaves/ (CSV)
#
# Reference : documentation/allwaves_balanced_design_benchmark_prompt.md

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

YEARS <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)

dir.create("data/derived/design_diagnostics_allwaves", showWarnings = FALSE)
dir.create("output/review_v2/design_diagnostics_allwaves", showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Versions (section 4 du protocole) — aucune mise a niveau de paquet
# ---------------------------------------------------------------------------
versions_record <- tibble(
  package = c("R", "MatchIt", "cobalt", "WeightIt", "highs", "Matching"),
  version = c(
    paste(R.version$major, R.version$minor, sep = "."),
    as.character(packageVersion("MatchIt")),
    as.character(packageVersion("cobalt")),
    as.character(packageVersion("WeightIt")),
    as.character(packageVersion("highs")),
    as.character(packageVersion("Matching"))
  )
)
write_csv(
  versions_record,
  "output/review_v2/design_diagnostics_allwaves/package_versions.csv"
)
print(versions_record)

# ---------------------------------------------------------------------------
# Semantique MatchIt verifiee dans la documentation installee (MatchIt 4.7.2,
# ?method_cardinality), pas supposee :
#   - estimand = "ATT" : cible = moyenne des traites ; l'ecart-type de
#     standardisation des tols est celui du groupe traite.
#   - cardinality 1:1 : ratio = 1 (entier strictement positif), nombre de
#     controles retenus = nombre de traites retenus.
#   - profile matching ATT : estimand = "ATT" ET ratio = NA -> le plus grand
#     ensemble apparié equilibre avec TOUS les traites fixes comme cible.
#   - tols = 0.10, std.tols = TRUE : contrainte |SMD_k| <= 0.10, SMD
#     standardise par l'ecart-type du groupe traite (estimand ATT).
#   - solver = "highs" (recommande et installe).
# ---------------------------------------------------------------------------

# Meme construction d'echantillon eligible que 06-matching.qmd
prep_matching <- function(df_final) {
  df_final |>
    dplyr::filter(GROUP %in% c("Treatment", "Control")) |>
    dplyr::mutate(treatment = if_else(GROUP == "Treatment", 1L, 0L)) |>
    tidyr::drop_na(all_of(matching_variables))
}

# SMD absolu, convention cobalt ATT : standardisation par l'ecart-type du
# groupe traite de l'echantillon eligible (non apparié) de la vague courante.
# Convention verifiee contre cobalt 5.0.0 (voir check ci-dessous).
smd_att <- function(df, vars, sd_treated) {
  X <- as.matrix(df[vars])
  tr <- df$treatment == 1L
  m_t <- colMeans(X[tr, , drop = FALSE])
  m_c <- colMeans(X[!tr, , drop = FALSE])
  setNames(abs((m_t - m_c) / sd_treated), vars)
}

# eCDF empirique ponderee ; evaluee en tous les points observes poolés
wecdf <- function(x, w) {
  o <- order(x)
  xs <- x[o]
  ws <- w[o]
  F <- cumsum(ws) / sum(ws)
  function(q) {
    idx <- findInterval(q, xs)
    out <- numeric(length(q))
    out[idx == 0L] <- 0
    ok <- idx > 0L
    out[ok] <- F[idx[ok]]
    out
  }
}

# eCDF mean / eCDF max (KS-like) entre traites et controles (eventuellement
# ponderes). Definitions documentees : les deux courbes sont evaluees en
# chaque valeur observee poolée ; eCDF max = max |F_t - F_c| ; eCDF mean =
# moyenne de |F_t - F_c| sur ces points.
ecdf_stats <- function(x_t, w_t, x_c, w_c) {
  pts <- sort(unique(c(x_t, x_c)))
  d <- abs(wecdf(x_t, w_t)(pts) - wecdf(x_c, w_c)(pts))
  c(ecdf_mean = mean(d), ecdf_max = max(d))
}

# Diagnostics distributionnels par covariable. md : data.frame d'analyse avec
# colonne treatment ; wvar : nom de la colonne de poids (NULL = non pondere,
# cas des ensembles appariés). SMD via cobalt (convention ATT verifiee),
# variance ratio via cobalt, eCDF via calcul exact manuel.
distributional_balance <- function(m_out = NULL, w_out = NULL, md, vars) {
  if (!is.null(m_out)) {
    md <- match.data(m_out)
    md <- md[md$weights > 0, , drop = FALSE]
    bt <- cobalt::bal.tab(m_out, estimand = "ATT", stats = c("m", "v"))
    wt <- rep(1, sum(md$treatment == 1L))
    wc <- rep(1, sum(md$treatment == 0L))
  } else {
    bt <- cobalt::bal.tab(w_out, estimand = "ATT", stats = c("m", "v"))
    wt <- rep(1, sum(md$treatment == 1L))
    wc <- w_out$weights[md$treatment == 0L]
  }
  bal <- bt$Balance
  smd_cobalt <- abs(bal$Diff.Adj)
  vr_cobalt <- bal[["V.Ratio.Adj"]]
  names(smd_cobalt) <- rownames(bal)
  names(vr_cobalt) <- rownames(bal)
  map_dfr(vars, function(v) {
    xt <- md[[v]][md$treatment == 1L]
    xc <- md[[v]][md$treatment == 0L]
    e <- ecdf_stats(xt, wt, xc, wc)
    tibble(
      covariate = v,
      SMD = round(unname(smd_cobalt[v]), 6),
      variance_ratio = round(unname(vr_cobalt[v]), 6),
      vr_abs_dev_from_1 = round(abs(unname(vr_cobalt[v]) - 1), 6),
      ecdf_mean = round(unname(e["ecdf_mean"]), 6),
      ecdf_max = round(unname(e["ecdf_max"]), 6)
    )
  })
}

# Variante manuelle pour l'ensemble de production (pas d'objet MatchIt) :
# SMD convention ATT identique, variance ratio echantillon, eCDF exacts.
distributional_balance_manual <- function(md, vars, sd_treated) {
  tr <- md$treatment == 1L
  # SMD calcule une seule fois pour toutes les covariables (le passage d'un
  # vecteur sd complet contre une difference scalaire recyclerait a tort)
  smd_all <- smd_att(md, vars, sd_treated)
  map_dfr(vars, function(v) {
    xt <- md[[v]][tr]
    xc <- md[[v]][!tr]
    e <- ecdf_stats(xt, rep(1, length(xt)), xc, rep(1, length(xc)))
    vr <- var(xt) / var(xc)
    tibble(
      covariate = v,
      SMD = round(unname(smd_all[v]), 6),
      variance_ratio = round(vr, 6),
      vr_abs_dev_from_1 = round(abs(vr - 1), 6),
      ecdf_mean = round(unname(e["ecdf_mean"]), 6),
      ecdf_max = round(unname(e["ecdf_max"]), 6)
    )
  })
}

# Concentration de clusters cotes controles. df : data.frame avec hv001 et
# poids (1 pour un controle selectionne, poids entropy pour repondération).
cluster_concentration <- function(df) {
  cl <- df |>
    dplyr::group_by(hv001) |>
    dplyr::summarise(w = sum(weight), .groups = "drop") |>
    dplyr::mutate(share = w / sum(w))
  s <- sort(cl$share, decreasing = TRUE)
  n_cl <- length(s)
  hh <- sum(s^2)
  per_cl <- df |> dplyr::count(hv001) |> dplyr::pull(n)
  tibble(
    distinct_control_clusters = n_cl,
    controls_per_cluster_median = round(median(per_cl), 2),
    controls_per_cluster_p90 = round(unname(quantile(per_cl, 0.9)), 2),
    controls_per_cluster_max = max(per_cl),
    top1_cluster_share = round(s[1], 4),
    top5_cluster_share = round(sum(s[1:min(5, n_cl)]), 4),
    top10_cluster_share = round(sum(s[1:min(10, n_cl)]), 4),
    cluster_HHI = round(hh, 6),
    cluster_ESS = round(1 / hh, 1)
  )
}

run_cardinality <- function(
  data,
  sd_treated,
  ratio,
  tol = 0.10,
  time_s = 1800
) {
  fml <- reformulate(matching_variables, response = "treatment")
  warns <- character(0)
  t0 <- Sys.time()
  m <- withCallingHandlers(
    matchit(
      fml,
      data = data,
      method = "cardinality",
      estimand = "ATT",
      ratio = ratio,
      tols = tol,
      std.tols = TRUE,
      solver = "highs",
      time = time_s
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  md <- match.data(m)
  sel <- md[md$weights > 0, , drop = FALSE]
  n_mt <- sum(sel$treatment == 1L)
  n_mc <- sum(sel$treatment == 0L)
  smd_after <- smd_att(sel, matching_variables, sd_treated)
  smd_before <- smd_att(data, matching_variables, sd_treated)
  status <- if (length(warns) > 0) {
    paste(warns, collapse = " | ")
  } else {
    "optimal"
  }
  list(
    m_out = m,
    row = tibble(
      eligible_treated = sum(data$treatment == 1L),
      eligible_control = sum(data$treatment == 0L),
      retained_treated = n_mt,
      retained_control = n_mc,
      treated_retention_pct = round(
        100 * n_mt / sum(data$treatment == 1L),
        2
      ),
      control_retention_pct = round(
        100 * n_mc / sum(data$treatment == 0L),
        2
      ),
      max_smd = round(max(smd_after), 4),
      worst_covariate = names(which.max(smd_after)),
      runtime_sec = round(rt, 2),
      solver_status = status
    ),
    smd_after = smd_after,
    smd_before = smd_before
  )
}

# ---------------------------------------------------------------------------
# Verification de convention cobalt 5.0.0 (section 4) sur la reference 2013
# ---------------------------------------------------------------------------
ref13 <- readRDS("data/derived/design_diagnostics/cardinality_2013_tol0p10.rds")
bt_check <- cobalt::bal.tab(ref13$m_out, estimand = "ATT", un = TRUE)
dat13_check <- readRDS("data/derived/hr_2013_final.rds") |>
  prep_matching() |>
  sf::st_drop_geometry() |>
  as.data.frame()
sd13 <- apply(
  as.matrix(dat13_check[dat13_check$treatment == 1L, matching_variables]),
  2,
  sd
)
smd_custom13 <- smd_att(dat13_check, matching_variables, sd13)
smd_cobalt13 <- abs(bt_check$Balance$Diff.Un)
names(smd_cobalt13) <- rownames(bt_check$Balance)
stopifnot(
  "cobalt 5.0.0 ne reproduit pas la convention ATT SMD du workflow precedent" = all(
    abs(smd_cobalt13 - smd_custom13) < 1e-6
  )
)
cat(">> Convention cobalt 5.0.0 verifiee : identique au workflow precedent.\n")

# ---------------------------------------------------------------------------
# Boucle principale : 7 vagues x 3 designs (A: cardinality 1:1,
# B: profile matching ATT, C: entropy balancing ATT)
# ---------------------------------------------------------------------------
results <- list()

for (YEAR in YEARS) {
  t0_wave <- Sys.time()
  dat <- readRDS(glue::glue("data/derived/hr_{YEAR}_final.rds")) |>
    prep_matching() |>
    sf::st_drop_geometry() |>
    as.data.frame()
  n_t <- sum(dat$treatment == 1L)
  n_c <- sum(dat$treatment == 0L)
  cat(glue::glue(
    "\n===== Vague {YEAR} : traites={n_t}, controles={n_c} =====\n"
  ))

  sd_treated_full <- apply(
    as.matrix(dat[dat$treatment == 1L, matching_variables]),
    2,
    sd
  )

  # traites eligibles : clusters / AP distincts
  tr_elig <- dat[dat$treatment == 1L, , drop = FALSE]
  tr_elig_clusters <- dplyr::n_distinct(tr_elig$hv001)
  tr_elig_pas <- dplyr::n_distinct(tr_elig$WDPAID)

  # --- Diagnostic A : cardinality 1:1, SMD <= 0.10 -------------------------
  resA <- run_cardinality(dat, sd_treated_full, ratio = 1)
  cat("A (cardinality 1:1) :\n")
  print(resA$row)

  mdA <- match.data(resA$m_out)
  selA <- mdA[mdA$weights > 0, , drop = FALSE]
  trA <- selA[selA$treatment == 1L, , drop = FALSE]
  # traites exclus (identifiants disponibles)
  exclA <- tr_elig |>
    dplyr::anti_join(
      trA |> dplyr::select(hv001, hv002),
      by = c("hv001", "hv002")
    ) |>
    dplyr::transmute(year = YEAR, hv001, hv002, WDPAID)

  # --- Diagnostic B : profile matching ATT (ratio = NA), SMD <= 0.10 -------
  resB <- tryCatch(
    run_cardinality(dat, sd_treated_full, ratio = NA),
    error = function(e) e
  )
  if (inherits(resB, "error")) {
    cat("B (profile ATT) ERREUR :", conditionMessage(resB), "\n")
    selB <- NULL
    concB <- NULL
    distB <- tidyr::crossing(
      covariate = matching_variables,
      SMD = NA_real_,
      variance_ratio = NA_real_,
      vr_abs_dev_from_1 = NA_real_,
      ecdf_mean = NA_real_,
      ecdf_max = NA_real_
    )
  } else {
    cat("B (profile ATT) :\n")
    print(resB$row)
    mdB <- match.data(resB$m_out)
    selB <- mdB[mdB$weights > 0, , drop = FALSE]
  }

  # --- Diagnostic C : entropy balancing ATT, premiers moments --------------
  t0_e <- Sys.time()
  w_eb <- weightit(
    reformulate(matching_variables, response = "treatment"),
    data = dat,
    method = "entropy",
    estimand = "ATT"
  )
  rt_e <- as.numeric(difftime(Sys.time(), t0_e, units = "secs"))
  btw <- cobalt::bal.tab(w_eb, un = TRUE)
  smd_eb <- abs(btw$Balance$Diff.Adj)
  names(smd_eb) <- rownames(btw$Balance)
  w_ctrl <- w_eb$weights[dat$treatment == 0L]
  s_w <- sort(w_ctrl, decreasing = TRUE)
  k_w <- length(s_w)
  cat(
    "C (entropy ATT) : max SMD =",
    round(max(smd_eb), 4),
    "ESS =",
    round(sum(w_ctrl)^2 / sum(w_ctrl^2), 1),
    "\n"
  )

  # --- Concentration clusters controles ------------------------------------
  concA <- selA |>
    dplyr::filter(treatment == 0L) |>
    dplyr::mutate(weight = 1) |>
    cluster_concentration()
  if (!is.null(selB)) {
    concB <- selB |>
      dplyr::filter(treatment == 0L) |>
      dplyr::mutate(weight = 1) |>
      cluster_concentration()
  }
  dat_c <- dat[dat$treatment == 0L, , drop = FALSE]
  dat_c$weight <- w_ctrl
  concC <- cluster_concentration(dat_c)

  # --- Diagnostics distributionnels ----------------------------------------
  distA <- distributional_balance(
    m_out = resA$m_out,
    md = dat,
    vars = matching_variables
  )
  if (!is.null(selB)) {
    distB <- distributional_balance(
      m_out = resB$m_out,
      md = dat,
      vars = matching_variables
    )
  }
  distC <- distributional_balance(
    w_out = w_eb,
    md = dat,
    vars = matching_variables
  )

  # --- Reference production (GenMatch + caliper 0.25, caches existants) ----
  dm_prod <- readRDS(glue::glue("data/derived/data_matched_{YEAR}.rds"))
  selP <- as.data.frame(dm_prod)
  selP <- selP[selP$weights > 0, , drop = FALSE]
  smdP <- smd_att(selP, matching_variables, sd_treated_full)
  distP <- distributional_balance_manual(
    selP,
    matching_variables,
    sd_treated_full
  )
  concP <- selP |>
    dplyr::filter(treatment == 0L) |>
    dplyr::mutate(weight = 1) |>
    cluster_concentration()

  # --- Composition par AP (traites) ----------------------------------------
  pa_elig <- tr_elig |>
    dplyr::count(WDPAID) |>
    dplyr::transmute(year = YEAR, WDPAID, eligible_treated_hh = n)
  pa_rows <- dplyr::bind_rows(
    trA |>
      dplyr::count(WDPAID) |>
      dplyr::transmute(
        year = YEAR,
        method = "A_cardinality_1to1",
        WDPAID,
        retained_treated_hh = n
      ),
    if (!is.null(selB)) {
      selB |>
        dplyr::filter(treatment == 1L) |>
        dplyr::count(WDPAID) |>
        dplyr::transmute(
          year = YEAR,
          method = "B_profile_att",
          WDPAID,
          retained_treated_hh = n
        )
    } else {
      tibble()
    },
    selP |>
      dplyr::filter(treatment == 1L) |>
      dplyr::count(WDPAID) |>
      dplyr::transmute(
        year = YEAR,
        method = "A0_genmatch_cal0p25",
        WDPAID,
        retained_treated_hh = n
      )
  ) |>
    dplyr::left_join(pa_elig, by = c("year", "WDPAID")) |>
    dplyr::mutate(
      retained_treated_hh = tidyr::replace_na(retained_treated_hh, 0L),
      excluded_hh = eligible_treated_hh - retained_treated_hh
    )
  pa_comp <- pa_rows

  results[[as.character(YEAR)]] <- list(
    eligible = tibble(
      year = YEAR,
      eligible_treated = n_t,
      eligible_control = n_c
    ),
    A = list(
      res = resA,
      excluded = exclA,
      dist = distA,
      conc = concA,
      treated_clusters = dplyr::n_distinct(trA$hv001),
      treated_pas = dplyr::n_distinct(trA$WDPAID)
    ),
    B = list(
      res = resB,
      dist = distB,
      conc = concB,
      treated_clusters = if (!is.null(selB)) {
        dplyr::n_distinct(
          selB$hv001[selB$treatment == 1L]
        )
      } else {
        NA
      },
      treated_pas = if (!is.null(selB)) {
        dplyr::n_distinct(
          selB$WDPAID[selB$treatment == 1L]
        )
      } else {
        NA
      }
    ),
    C = list(
      summary = tibble(
        year = YEAR,
        eligible_treated = n_t,
        eligible_control = n_c,
        treated_retained = n_t,
        controls_positive = n_c_pos <- sum(w_ctrl > 0),
        max_smd = round(max(smd_eb), 6),
        worst_covariate = names(which.max(smd_eb)),
        control_ESS = round(sum(w_ctrl)^2 / sum(w_ctrl^2), 1),
        mean_control_weight = round(mean(w_ctrl), 4),
        max_control_weight = round(max(w_ctrl), 4),
        p99_control_weight = round(unname(quantile(w_ctrl, 0.99)), 4),
        cv_control_weights = round(sd(w_ctrl) / mean(w_ctrl), 4),
        top1pct_share = round(
          sum(s_w[seq_len(max(1L, round(0.01 * k_w)))]) / sum(s_w),
          4
        ),
        top5pct_share = round(
          sum(s_w[seq_len(max(1L, round(0.05 * k_w)))]) / sum(s_w),
          4
        ),
        top10pct_share = round(
          sum(s_w[seq_len(max(1L, round(0.10 * k_w)))]) / sum(s_w),
          4
        ),
        runtime_sec = round(rt_e, 2)
      ),
      balance = tibble(
        year = YEAR,
        covariate = matching_variables,
        smd_before_cobalt_att = round(abs(btw$Balance$Diff.Un), 4),
        smd_after_cobalt_att = round(abs(smd_eb), 4)
      ),
      w_out = w_eb,
      dist = distC,
      conc = concC,
      treated_clusters = tr_elig_clusters,
      treated_pas = tr_elig_pas
    ),
    prod = list(
      retained_treated = sum(selP$treatment == 1L),
      retained_control = sum(selP$treatment == 0L),
      max_smd = round(max(smdP), 4),
      worst_covariate = names(which.max(smdP)),
      dist = distP,
      conc = concP
    ),
    pa_comp = pa_comp
  )

  # checkpoint par vague (objets de diagnostic, pas d'outcome)
  saveRDS(
    list(
      A = resA,
      B = resB,
      w_eb = w_eb,
      meta = list(
        purpose = "diagnostic_only_not_for_outcomes",
        year = YEAR,
        tolerance = 0.10,
        estimand = "ATT",
        solver = "highs"
      )
    ),
    glue::glue("data/derived/design_diagnostics_allwaves/allwaves_{YEAR}.rds")
  )
  cat(glue::glue(
    ">> Vague {YEAR} terminee en ",
    "{round(as.numeric(difftime(Sys.time(), t0_wave, units='mins')),1)} min\n"
  ))
}

# ---------------------------------------------------------------------------
# Exports CSV
# ---------------------------------------------------------------------------
out_dir <- "output/review_v2/design_diagnostics_allwaves"
yr_names <- as.integer(names(results))

# A. Cardinality 1:1
card_summary <- map_dfr(results, ~ .x$A$res$row) |>
  mutate(year = yr_names, .before = 1)
write_csv(card_summary, file.path(out_dir, "cardinality_1to1_summary.csv"))

card_balance <- map_dfr(results, function(r) {
  tibble(
    year = r$eligible$year,
    covariate = matching_variables,
    smd_before_cobalt_att = round(abs(r$A$res$smd_before), 4),
    smd_after_cobalt_att = round(abs(r$A$res$smd_after), 4)
  )
})
write_csv(card_balance, file.path(out_dir, "cardinality_1to1_balance.csv"))

excl_all <- map_dfr(results, ~ .x$A$excluded)
write_csv(excl_all, file.path(out_dir, "cardinality_1to1_excluded_treated.csv"))

# B. Profile matching ATT
prof_ok <- !map_lgl(results, ~ inherits(.x$B$res, "error"))
prof_years <- yr_names[prof_ok]
prof_summary <- map_dfr(results[prof_ok], ~ .x$B$res$row) |>
  mutate(year = prof_years, .before = 1)
write_csv(prof_summary, file.path(out_dir, "profile_att_summary.csv"))

prof_balance <- map_dfr(results[prof_ok], function(r) {
  tibble(
    year = r$eligible$year,
    covariate = matching_variables,
    smd_before_cobalt_att = round(abs(r$B$res$smd_before), 4),
    smd_after_cobalt_att = round(abs(r$B$res$smd_after), 4)
  )
})
write_csv(prof_balance, file.path(out_dir, "profile_att_balance.csv"))

# C. Entropy
entropy_summary <- map_dfr(results, ~ .x$C$summary)
write_csv(entropy_summary, file.path(out_dir, "entropy_summary.csv"))
entropy_balance <- map_dfr(results, ~ .x$C$balance)
write_csv(entropy_balance, file.path(out_dir, "entropy_balance.csv"))
entropy_wdiag <- entropy_summary |>
  dplyr::select(
    year,
    control_ESS,
    mean_control_weight,
    max_control_weight,
    p99_control_weight,
    cv_control_weights,
    top1pct_share,
    top5pct_share,
    top10pct_share
  )
write_csv(entropy_wdiag, file.path(out_dir, "entropy_weight_diagnostics.csv"))

# Concentration clusters (method x year)
conc_tbl <- dplyr::bind_rows(
  imap_dfr(results, function(r, yr) {
    tibble(
      year = as.integer(yr),
      method = "A0_genmatch_cal0p25",
      eligible_treated = r$eligible$eligible_treated,
      eligible_control = r$eligible$eligible_control,
      retained_treated = r$prod$retained_treated,
      retained_control = r$prod$retained_control,
      treated_clusters = NA_integer_,
      treated_WDPAIDs = NA_integer_,
      !!!r$prod$conc
    )
  }),
  imap_dfr(results, function(r, yr) {
    tibble(
      year = as.integer(yr),
      method = "A_cardinality_1to1",
      eligible_treated = r$eligible$eligible_treated,
      eligible_control = r$eligible$eligible_control,
      retained_treated = r$A$res$row$retained_treated,
      retained_control = r$A$res$row$retained_control,
      treated_clusters = as.integer(r$A$treated_clusters),
      treated_WDPAIDs = as.integer(r$A$treated_pas),
      !!!r$A$conc
    )
  }),
  imap_dfr(results[prof_ok], function(r, yr) {
    tibble(
      year = as.integer(yr),
      method = "B_profile_att",
      eligible_treated = r$eligible$eligible_treated,
      eligible_control = r$eligible$eligible_control,
      retained_treated = r$B$res$row$retained_treated,
      retained_control = r$B$res$row$retained_control,
      treated_clusters = as.integer(r$B$treated_clusters),
      treated_WDPAIDs = as.integer(r$B$treated_pas),
      !!!r$B$conc
    )
  }),
  imap_dfr(results, function(r, yr) {
    tibble(
      year = as.integer(yr),
      method = "C_entropy_att",
      eligible_treated = r$eligible$eligible_treated,
      eligible_control = r$eligible$eligible_control,
      retained_treated = r$C$summary$treated_retained,
      retained_control = r$C$summary$controls_positive,
      treated_clusters = as.integer(r$C$treated_clusters),
      treated_WDPAIDs = as.integer(r$C$treated_pas),
      !!!r$C$conc
    )
  })
)
write_csv(conc_tbl, file.path(out_dir, "control_cluster_concentration.csv"))

# Diagnostics distributionnels (method x year x covariate)
dist_tbl <- dplyr::bind_rows(
  imap_dfr(results, function(r, yr) {
    tibble(
      year = as.integer(yr),
      method = "A0_genmatch_cal0p25",
      !!!r$prod$dist
    )
  }),
  imap_dfr(results, function(r, yr) {
    tibble(year = as.integer(yr), method = "A_cardinality_1to1", !!!r$A$dist)
  }),
  imap_dfr(results[prof_ok], function(r, yr) {
    tibble(year = as.integer(yr), method = "B_profile_att", !!!r$B$dist)
  }),
  imap_dfr(results, function(r, yr) {
    tibble(year = as.integer(yr), method = "C_entropy_att", !!!r$C$dist)
  })
)
write_csv(dist_tbl, file.path(out_dir, "distributional_balance.csv"))

# Composition par AP
pa_tbl <- results |>
  map_dfr(~ .x$pa_comp) |>
  dplyr::arrange(year, method, WDPAID)
write_csv(pa_tbl, file.path(out_dir, "treated_pa_composition.csv"))

# ---------------------------------------------------------------------------
# Table comparative toutes vagues (section 11)
# ---------------------------------------------------------------------------
comparison <- dplyr::bind_rows(
  imap_dfr(results, function(r, yr) {
    tibble(
      year = as.integer(yr),
      design = "A. GenMatch + caliper 0.25 SD (production)",
      eligible_treated = r$eligible$eligible_treated,
      retained_treated = r$prod$retained_treated,
      treated_retention_pct = round(
        100 * r$prod$retained_treated / r$eligible$eligible_treated,
        1
      ),
      controls_used = r$prod$retained_control,
      distinct_control_clusters = r$prod$conc$distinct_control_clusters,
      max_smd = r$prod$max_smd,
      worst_covariate = r$prod$worst_covariate,
      max_ecdf_diff = round(max(r$prod$dist$ecdf_max), 4),
      max_variance_ratio_deviation = round(
        max(r$prod$dist$vr_abs_dev_from_1),
        4
      ),
      control_ESS = NA_real_,
      cluster_ESS = r$prod$conc$cluster_ESS,
      runtime_sec = NA_real_
    )
  }),
  imap_dfr(results, function(r, yr) {
    tibble(
      year = as.integer(yr),
      design = "B. Cardinality 1:1, SMD<=0.10",
      eligible_treated = r$eligible$eligible_treated,
      retained_treated = r$A$res$row$retained_treated,
      treated_retention_pct = r$A$res$row$treated_retention_pct,
      controls_used = r$A$res$row$retained_control,
      distinct_control_clusters = r$A$conc$distinct_control_clusters,
      max_smd = r$A$res$row$max_smd,
      worst_covariate = r$A$res$row$worst_covariate,
      max_ecdf_diff = round(max(r$A$dist$ecdf_max), 4),
      max_variance_ratio_deviation = round(
        max(r$A$dist$vr_abs_dev_from_1),
        4
      ),
      control_ESS = NA_real_,
      cluster_ESS = r$A$conc$cluster_ESS,
      runtime_sec = r$A$res$row$runtime_sec
    )
  }),
  imap_dfr(results[prof_ok], function(r, yr) {
    tibble(
      year = as.integer(yr),
      design = "C. Profile matching ATT, SMD<=0.10",
      eligible_treated = r$eligible$eligible_treated,
      retained_treated = r$B$res$row$retained_treated,
      treated_retention_pct = r$B$res$row$treated_retention_pct,
      controls_used = r$B$res$row$retained_control,
      distinct_control_clusters = r$B$conc$distinct_control_clusters,
      max_smd = r$B$res$row$max_smd,
      worst_covariate = r$B$res$row$worst_covariate,
      max_ecdf_diff = round(max(r$B$dist$ecdf_max), 4),
      max_variance_ratio_deviation = round(
        max(r$B$dist$vr_abs_dev_from_1),
        4
      ),
      control_ESS = NA_real_,
      cluster_ESS = r$B$conc$cluster_ESS,
      runtime_sec = r$B$res$row$runtime_sec
    )
  }),
  imap_dfr(results, function(r, yr) {
    tibble(
      year = as.integer(yr),
      design = "D. Entropy balancing ATT",
      eligible_treated = r$eligible$eligible_treated,
      retained_treated = r$C$summary$treated_retained,
      treated_retention_pct = 100,
      controls_used = r$C$summary$controls_positive,
      distinct_control_clusters = r$C$conc$distinct_control_clusters,
      max_smd = r$C$summary$max_smd,
      worst_covariate = r$C$summary$worst_covariate,
      max_ecdf_diff = round(max(r$C$dist$ecdf_max), 4),
      max_variance_ratio_deviation = round(
        max(r$C$dist$vr_abs_dev_from_1),
        4
      ),
      control_ESS = r$C$summary$control_ESS,
      cluster_ESS = r$C$conc$cluster_ESS,
      runtime_sec = r$C$summary$runtime_sec
    )
  })
)
write_csv(comparison, file.path(out_dir, "design_comparison_allwaves.csv"))

cat("\n>> CSV exportes dans", out_dir, "\n")
print(comparison, n = Inf, width = Inf)
cat(">> Diagnostic termine — AUCUN outcome utilise, AUCUN placebo lance.\n")

# ---------------------------------------------------------------------------
# Section 12 : stabilite legere — uniquement les vagues signaletiques
# (1997 : concentration de clusters la plus elevee de cardinality ; 2011 :
# ESS entropy le plus faible). 20 reps, sous-echantillonnage 80% des clusters
# au sein de chaque strate. Pas d'outcome, pas d'IC.
# ---------------------------------------------------------------------------
STAB_WAVES <- c(1997, 2011)
N_REPS_STAB <- 20L
STAB_BASE_SEED <- 20261006L
SUB_FRAC <- 0.8

stab_card <- vector("list", length(STAB_WAVES))
stab_eb <- vector("list", length(STAB_WAVES))
names(stab_card) <- as.character(STAB_WAVES)
names(stab_eb) <- as.character(STAB_WAVES)

for (YW in STAB_WAVES) {
  dat_w <- readRDS(glue::glue("data/derived/hr_{YW}_final.rds")) |>
    prep_matching() |>
    sf::st_drop_geometry() |>
    as.data.frame()
  sd_w <- apply(
    as.matrix(dat_w[dat_w$treatment == 1L, matching_variables]),
    2,
    sd
  )
  cl_t <- unique(dat_w$hv001[dat_w$treatment == 1L])
  cl_c <- unique(dat_w$hv001[dat_w$treatment == 0L])
  n_t <- ceiling(SUB_FRAC * length(cl_t))
  n_c <- ceiling(SUB_FRAC * length(cl_c))
  sc <- vector("list", N_REPS_STAB)
  se <- vector("list", N_REPS_STAB)
  for (i in seq_len(N_REPS_STAB)) {
    set.seed(STAB_BASE_SEED + i)
    ct <- sort(sample(cl_t, n_t))
    cc <- sort(sample(cl_c, n_c))
    sub <- dat_w |> dplyr::filter(hv001 %in% ct | hv001 %in% cc)
    rc <- tryCatch(
      run_cardinality(sub, sd_w, ratio = 1, time_s = 600),
      error = function(e) e
    )
    sc[[i]] <- if (inherits(rc, "error")) {
      tibble(rep = i, error = conditionMessage(rc))
    } else {
      mdi <- match.data(rc$m_out)
      tri <- mdi[mdi$weights > 0 & mdi$treatment == 1L, ]
      tibble(
        rep = i,
        retained_treated = rc$row$retained_treated,
        eligible_treated = rc$row$eligible_treated,
        treated_retention_pct = rc$row$treated_retention_pct,
        max_smd = rc$row$max_smd,
        treated_clusters = dplyr::n_distinct(tri$hv001),
        treated_PAs = dplyr::n_distinct(tri$WDPAID),
        control_clusters = NA,
        error = NA_character_
      )
    }
    re <- tryCatch(
      {
        wi <- weightit(
          reformulate(matching_variables, response = "treatment"),
          data = sub,
          method = "entropy",
          estimand = "ATT"
        )
        bti <- cobalt::bal.tab(wi, un = TRUE)$Balance
        wci <- wi$weights[sub$treatment == 0L]
        tibble(
          rep = i,
          retained_treated = NA,
          eligible_treated = NA,
          treated_retention_pct = NA,
          max_smd = round(max(abs(bti$Diff.Adj)), 6),
          treated_clusters = NA,
          treated_PAs = NA,
          control_clusters = dplyr::n_distinct(
            sub$hv001[sub$treatment == 0L][wci > 0]
          ),
          error = NA_character_
        )
      },
      error = function(e) tibble(rep = i, error = conditionMessage(e))
    )
    se[[i]] <- re
  }
  stab_card[[as.character(YW)]] <- bind_rows(sc)
  stab_eb[[as.character(YW)]] <- bind_rows(se)
}

stab_card_out <- imap_dfr(
  stab_card,
  ~ mutate(.x, year = as.integer(.y), .before = 1)
)
stab_eb_out <- imap_dfr(
  stab_eb,
  ~ mutate(.x, year = as.integer(.y), .before = 1)
)
write_csv(
  stab_card_out,
  file.path(out_dir, "cardinality_stability_1997_2011.csv")
)
write_csv(stab_eb_out, file.path(out_dir, "entropy_stability_1997_2011.csv"))

cat("\n>> Stabilite cardinality (mediane ; p5 ; p95) :\n")
for (yw in as.character(STAB_WAVES)) {
  d <- stab_card[[yw]]
  cat(glue::glue(
    "  {yw}: retention={round(median(d$treated_retention_pct, na.rm=TRUE),1)}% ",
    "[{round(quantile(d$treated_retention_pct, 0.05, na.rm=TRUE),1)};",
    "{round(quantile(d$treated_retention_pct, 0.95, na.rm=TRUE),1)}] ",
    "max_smd={round(median(d$max_smd, na.rm=TRUE),3)} ",
    "[{round(quantile(d$max_smd, 0.05, na.rm=TRUE),3)};",
    "{round(quantile(d$max_smd, 0.95, na.rm=TRUE),3)}] ",
    "PAs={round(median(d$treated_PAs, na.rm=TRUE),1)}\n"
  ))
}
cat(">> Stabilite entropy (mediane ; p5 ; p95) :\n")
for (yw in as.character(STAB_WAVES)) {
  d <- stab_eb[[yw]]
  cat(glue::glue(
    "  {yw}: max_smd={round(median(d$max_smd, na.rm=TRUE),5)} ",
    "[{round(quantile(d$max_smd, 0.05, na.rm=TRUE),5)};",
    "{round(quantile(d$max_smd, 0.95, na.rm=TRUE),5)}] ",
    "clusters_controles={round(median(d$control_clusters, na.rm=TRUE),1)}\n"
  ))
}
cat(">> Stabilite legere terminee (1997, 2011).\n")
