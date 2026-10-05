# Diagnostic de tolerance pour le profile matching ATT (outcome-blind)
#
# Objectif : avant de figer la regle de production, tester si une garde
# numerique tols = .095 (au lieu de .100) est faisable sur les deux vagues les
# plus fragiles (1997, 2011), et verifier la tolerance choisie sur les 7 vagues.
#
# Critere substantiel inchange : max |SMD| externe (cobalt, convention ATT)
# <= .10 sur l'echantillon eligible de la vague. Le .095 n'est qu'une garde
# numerique contre les solutions exactement a la borne.
#
# AUCUNE variable d'outcome n'est utilisee. Aucune estimation d'effet. Les
# fichiers de production (06-matching.qmd, data_matched_*.rds,
# matching_result_*.rds) ne sont ni modifies ni remplaces.
#
# Sorties :
#   data/derived/profile_tolerance_diagnostic/     (RDS de diagnostic)
#   output/review_v2/profile_tolerance_diagnostic/ (CSV)
#     profile_tolerance_fullsample.csv
#     profile_tolerance_balance.csv
#     profile_tolerance_stability.csv
#     profile_allwaves_verification.csv
#
# Reference : documentation/freeze_profile_matching_and_run_placebo_prompt.md

library(tidyverse)
library(MatchIt)
library(cobalt)

matching_variables <- c(
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)

YEARS <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)
TOLS <- c(0.100, 0.095)
TEST_YEARS <- c(1997, 2011)

N_REPS_TOL <- 20L
SUBSAMPLE_FRAC_TOL <- 0.80
# Graine deterministe (entier aleatoire 1..10000) pour l'echantillonnage
# de clusters de l'etape de stabilite
STABILITY_TOL_SEED <- 4821L

dir.create("data/derived/profile_tolerance_diagnostic", showWarnings = FALSE)
dir.create("output/review_v2/profile_tolerance_diagnostic", showWarnings = FALSE)

versions_record <- tibble(
  package = c("R", "MatchIt", "cobalt", "highs"),
  version = c(
    paste(R.version$major, R.version$minor, sep = "."),
    as.character(packageVersion("MatchIt")),
    as.character(packageVersion("cobalt")),
    as.character(packageVersion("highs"))
  )
)
write_csv(
  versions_record,
  "output/review_v2/profile_tolerance_diagnostic/package_versions.csv"
)

# Meme construction d'echantillon eligible que 06-matching.qmd
prep_matching <- function(df_final) {
  df_final |>
    dplyr::filter(GROUP %in% c("Treatment", "Control")) |>
    dplyr::mutate(treatment = if_else(GROUP == "Treatment", 1L, 0L)) |>
    tidyr::drop_na(all_of(matching_variables))
}

# SMD absolu convention ATT : standardisation par l'ecart-type du groupe traite
# de l'echantillon eligible. Convention verifiee identique a cobalt 5.0.0
# (design_diagnostic_allwaves.R, stopifnot 1e-6).
smd_att <- function(df, vars, sd_treated) {
  X <- as.matrix(df[vars])
  tr <- df$treatment == 1L
  m_t <- colMeans(X[tr, , drop = FALSE])
  m_c <- colMeans(X[!tr, , drop = FALSE])
  setNames(abs((m_t - m_c) / sd_treated), vars)
}

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
    cluster_HHI = round(hh, 6),
    cluster_ESS = round(1 / hh, 1)
  )
}

run_profile <- function(data, tol) {
  fml <- reformulate(matching_variables, response = "treatment")
  warns <- character(0)
  t0 <- Sys.time()
  m <- withCallingHandlers(
    tryCatch(
      matchit(
        fml,
        data = data,
        method = "cardinality",
        estimand = "ATT",
        ratio = NA,
        tols = tol,
        std.tols = TRUE,
        solver = "highs",
        time = 1800
      ),
      error = function(e) e
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(m, "error")) {
    return(list(error = m, runtime_sec = rt))
  }
  status <- if (length(warns) > 0) paste(warns, collapse = " | ") else "optimal"
  list(m_out = m, runtime_sec = rt, solver_status = status)
}

# Resume externe a partir de l'objet MatchIt : retention, SMD cobalt + SMD
# manuel (controle croise), concentration de clusters cotes controles.
summarize_profile <- function(m_out, data, year, tol, rt, status) {
  md <- match.data(m_out)
  sel <- md[md$weights > 0, , drop = FALSE]
  n_t_elig <- sum(data$treatment == 1L)
  n_c_elig <- sum(data$treatment == 0L)
  n_t <- sum(sel$treatment == 1L)
  n_c <- sum(sel$treatment == 0L)

  sd_treated <- apply(
    as.matrix(data[data$treatment == 1L, matching_variables]),
    2,
    sd
  )

  # SMD externe : cobalt bal.tab (convention ATT verifiee)
  bt <- cobalt::bal.tab(m_out, estimand = "ATT", un = TRUE)
  smd_cobalt <- abs(bt$Balance$Diff.Adj)
  names(smd_cobalt) <- rownames(bt$Balance)

  # Controle croise manuel du pire SMD :
  # (mean_treated - mean_control) / SD_treated_eligible
  smd_manual <- smd_att(sel, matching_variables, sd_treated)

  conc <- sel |>
    dplyr::filter(treatment == 0L) |>
    dplyr::mutate(weight = 1) |>
    cluster_concentration()

  list(
    row = tibble(
      year = year,
      tolerance = tol,
      eligible_treated = n_t_elig,
      eligible_control = n_c_elig,
      retained_treated = n_t,
      retained_control = n_c,
      treated_retention_pct = round(100 * n_t / n_t_elig, 2),
      control_retention_pct = round(100 * n_c / n_c_elig, 2),
      max_smd_cobalt = round(max(smd_cobalt), 6),
      worst_covariate = names(which.max(smd_cobalt)),
      max_smd_manual_worst = round(unname(smd_manual[which.max(smd_cobalt)]), 6),
      manual_cobalt_absdiff = round(
        abs(unname(smd_manual[which.max(smd_cobalt)]) - max(smd_cobalt)),
        8
      ),
      !!!conc,
      runtime_sec = round(rt, 2),
      solver_status = status
    ),
    balance = tibble(
      year = year,
      tolerance = tol,
      covariate = matching_variables,
      smd_before_cobalt_att = round(abs(bt$Balance$Diff.Un), 6),
      smd_after_cobalt_att = round(smd_cobalt, 6),
      smd_after_manual = round(unname(smd_manual), 6)
    ),
    smd_manual = smd_manual
  )
}

# ---------------------------------------------------------------------------
# Etape 1 : 1997 et 2011, full sample, tols = .100 vs .095
# Etape 2 : verification de la tolerance sur les 7 vagues (les deux tols sont
#           evalues pour disposer de l'evidence complete avant de figer)
# ---------------------------------------------------------------------------

# cache des echantillons eligibles par vague
dat_list <- list()
for (YEAR in YEARS) {
  dat_list[[as.character(YEAR)]] <-
    readRDS(glue::glue("data/derived/hr_{YEAR}_final.rds")) |>
    prep_matching() |>
    sf::st_drop_geometry() |>
    as.data.frame()
}

full_rows <- list()
bal_rows <- list()
allwaves_rows <- list()
allwaves_bal <- list()

for (YEAR in YEARS) {
  dat <- dat_list[[as.character(YEAR)]]
  n_t <- sum(dat$treatment == 1L)
  n_c <- sum(dat$treatment == 0L)
  cat(glue::glue(
    "\n===== Vague {YEAR} : traites={n_t}, controles={n_c} =====\n"
  ))
  for (TOL in TOLS) {
    res <- run_profile(dat, TOL)
    if (!is.null(res$error)) {
      cat(glue::glue("tol={TOL} ERREUR : {conditionMessage(res$error)}\n"))
      full_rows[[glue::glue("{YEAR}_{TOL}")]] <- tibble(
        year = YEAR,
        tolerance = TOL,
        eligible_treated = n_t,
        eligible_control = n_c,
        solver_status = paste("ERROR:", conditionMessage(res$error))
      )
      next
    }
    s <- summarize_profile(
      res$m_out, dat, YEAR, TOL, res$runtime_sec, res$solver_status
    )
    cat(glue::glue(
      "tol={TOL} : traites={s$row$retained_treated}/{n_t}, ",
      "controles={s$row$retained_control}, ",
      "maxSMD={s$row$max_smd_cobalt}, ",
      "pire={s$row$worst_covariate}, status={s$row$solver_status}\n"
    ))
    stopifnot(
      "SMD manuel != SMD cobalt" = s$row$manual_cobalt_absdiff < 1e-6
    )
    if (YEAR %in% TEST_YEARS) {
      full_rows[[glue::glue("{YEAR}_{TOL}")]] <- s$row
      bal_rows[[glue::glue("{YEAR}_{TOL}")]] <- s$balance
    }
    allwaves_rows[[glue::glue("{YEAR}_{TOL}")]] <- s$row
    allwaves_bal[[glue::glue("{YEAR}_{TOL}")]] <- s$balance
    saveRDS(
      list(
        m_out = res$m_out,
        meta = list(
          purpose = "diagnostic_only_not_for_outcomes",
          year = YEAR,
          tolerance = TOL,
          estimand = "ATT",
          ratio = NA,
          solver = "highs"
        )
      ),
      glue::glue(
        "data/derived/profile_tolerance_diagnostic/",
        "profile_tol_{YEAR}_{sprintf('%.3f', TOL)}.rds"
      )
    )
  }
}

fullsample <- dplyr::bind_rows(full_rows)
balance <- dplyr::bind_rows(bal_rows)
allwaves <- dplyr::bind_rows(allwaves_rows)
allwaves_balance <- dplyr::bind_rows(allwaves_bal)

out_dir <- "output/review_v2/profile_tolerance_diagnostic"
write_csv(fullsample, file.path(out_dir, "profile_tolerance_fullsample.csv"))
write_csv(balance, file.path(out_dir, "profile_tolerance_balance.csv"))
write_csv(allwaves, file.path(out_dir, "profile_allwaves_verification.csv"))
write_csv(
  allwaves_balance,
  file.path(out_dir, "profile_allwaves_balance.csv")
)

# ---------------------------------------------------------------------------
# Etape 3 : stabilite de design — 1997 et 2011, 20 repetitions,
# 80% des clusters echantillonnes sans remplacement au sein de chaque strate
# (traites / controles). Meme sous-echantillon pour les deux tolerances d'une
# meme repetition. Design stability, pas inference bootstrap.
# ---------------------------------------------------------------------------

stab_rows <- list()
for (YEAR in TEST_YEARS) {
  dat <- dat_list[[as.character(YEAR)]]
  cl_t <- sort(unique(dat$hv001[dat$treatment == 1L]))
  cl_c <- sort(unique(dat$hv001[dat$treatment == 0L]))
  for (REP in seq_len(N_REPS_TOL)) {
    set.seed(STABILITY_TOL_SEED + REP)
    k_t <- max(1L, round(SUBSAMPLE_FRAC_TOL * length(cl_t)))
    k_c <- max(1L, round(SUBSAMPLE_FRAC_TOL * length(cl_c)))
    keep_t <- sort(sample(cl_t, k_t))
    keep_c <- sort(sample(cl_c, k_c))
    sub <- dat[dat$hv001 %in% keep_t | dat$hv001 %in% keep_c, , drop = FALSE]
    for (TOL in TOLS) {
      res <- run_profile(sub, TOL)
      if (!is.null(res$error)) {
        stab_rows[[glue::glue("{YEAR}_{REP}_{TOL}")]] <- tibble(
          year = YEAR,
          rep = REP,
          tolerance = TOL,
          eligible_treated = sum(sub$treatment == 1L),
          retained_treated = NA_integer_,
          retained_controls = NA_integer_,
          treated_retention_pct = NA_real_,
          max_smd_cobalt = NA_real_,
          worst_covariate = NA_character_,
          solver_status = paste("ERROR:", conditionMessage(res$error))
        )
        next
      }
      md <- match.data(res$m_out)
      sel <- md[md$weights > 0, , drop = FALSE]
      bt <- cobalt::bal.tab(res$m_out, estimand = "ATT", un = TRUE)
      smd_cobalt <- abs(bt$Balance$Diff.Adj)
      names(smd_cobalt) <- rownames(bt$Balance)
      n_t_elig <- sum(sub$treatment == 1L)
      stab_rows[[glue::glue("{YEAR}_{REP}_{TOL}")]] <- tibble(
        year = YEAR,
        rep = REP,
        tolerance = TOL,
        eligible_treated = n_t_elig,
        retained_treated = sum(sel$treatment == 1L),
        retained_controls = sum(sel$treatment == 0L),
        treated_retention_pct = round(
          100 * sum(sel$treatment == 1L) / n_t_elig,
          2
        ),
        max_smd_cobalt = round(max(smd_cobalt), 6),
        worst_covariate = names(which.max(smd_cobalt)),
        solver_status = res$solver_status
      )
    }
    cat(glue::glue("stabilite {YEAR} rep {REP}/{N_REPS_TOL}\n"))
  }
}

stability <- dplyr::bind_rows(stab_rows)
write_csv(stability, file.path(out_dir, "profile_tolerance_stability.csv"))

# ---------------------------------------------------------------------------
# Synthese console pour la decision de freeze
# ---------------------------------------------------------------------------
cat("\n===== Synthese full-sample (1997, 2011) =====\n")
print(fullsample, n = Inf)
cat("\n===== Stabilite : repartition du max |SMD| externe =====\n")
print(
  stability |>
    dplyr::group_by(year, tolerance) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_smd_gt_010 = sum(max_smd_cobalt > 0.10, na.rm = TRUE),
      mean_max_smd = round(mean(max_smd_cobalt, na.rm = TRUE), 5),
      p95_max_smd = round(
        unname(quantile(max_smd_cobalt, 0.95, na.rm = TRUE)),
        5
      ),
      max_max_smd = round(max(max_smd_cobalt, na.rm = TRUE), 5),
      min_treated_retention = min(treated_retention_pct, na.rm = TRUE),
      .groups = "drop"
    ),
  n = Inf
)
cat("\n===== Verification 7 vagues =====\n")
print(allwaves, n = Inf)
