# =====================================================================
# Sensibilité de la datation du traitement : décrets de création
# permanents (scénario "permanent")
#
# Logique réutilisable partagée par le chapitre 08
# (08-sensitivity-checks.qmd, section « Sensibilité à la datation du
# traitement »). Elle reproduit verbatim la chaîne de production
# 04-variable-consolidation.qmd (jointure build_year) ->
# 06-matching.qmd (appariement par cardinalité) ->
# 07-estimation_staggered.qmd (préparation dat/dat2 et spécifications
# did2s échelonnées), en l'appliquant à l'export de classification
# « permanent » de 01-treatment-assignation.qmd.
#
# Garanties de conception :
#  - le scénario permanent s'exécute en ISOLEMENT : aucun fichier de
#    production (data/derived/, paper/, docs/) n'est modifié ;
#  - les intermédiaires vivent dans un cache local ignoré par git
#    (documentation/permanent_decree_estimation/data/), invalidé par
#    empreinte md5 des entrées (export permanent, covariables
#    spatiales, fichiers hh_*_rural_simpler.rds) ;
#  - les gardes-fous du design (clés uniques, cohérence
#    export/estimateur, bornes temporelles, rétention des traités,
#    SMD) sont réexécutés à chaque chargement.
#
# Re-générer le cache complet (extraction + appariement, plusieurs
# minutes) :
#   pa_perm_run_substitution()          # force = FALSE par défaut
#   pa_perm_run_substitution(force = TRUE)  # même si cache valide
# =====================================================================

pa_perm_waves <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)

pa_perm_scenario_dir <- function() {
  "documentation/permanent_decree_estimation"
}
pa_perm_data_dir <- function() {
  file.path(pa_perm_scenario_dir(), "data")
}
pa_perm_output_dir <- function() {
  file.path(pa_perm_scenario_dir(), "output")
}
pa_perm_export_permanent <- function() {
  "data/derived/cluster_treatment_classification_staggered_permanent_robustness.csv"
}
pa_perm_export_main <- function() {
  "data/derived/cluster_treatment_classification_staggered.csv"
}
pa_perm_matched_main_paths <- function() {
  sprintf("data/derived/data_matched_%d.rds", pa_perm_waves)
}
pa_perm_matched_perm_paths <- function() {
  file.path(
    pa_perm_data_dir(),
    sprintf("data_matched_%d.rds", pa_perm_waves)
  )
}
pa_perm_cache_manifest <- function() {
  file.path(pa_perm_output_dir(), "cache_inputs.csv")
}

# Date de référence des vagues (même définition que 01-treatment-assignation :
# 1er juin, un seul point de définition)
pa_perm_survey_reference_date <- function(year) {
  as.Date(sprintf("%d-06-01", year))
}

pa_perm_matching_variables <- c(
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)

# Tolérance de profil figée du 06-matching.qmd (production)
pa_perm_profile_tol <- 0.095

# Contrôles du PAP (identiques à 07)
pa_perm_controls_pap <- c(
  "spei_wc_n_1",
  "hv219",
  "hv220",
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)

pa_perm_first_stage <- function() {
  as.formula(paste(
    "~",
    paste(pa_perm_controls_pap, collapse = " + "),
    "| treatment_cohort + DHSYEAR"
  ))
}

pa_perm_es_levels <- c(
  "-5",
  "-4",
  "-3",
  "-2",
  "-1",
  "0",
  "1",
  "2",
  "3",
  "4",
  "5",
  "never"
)

pa_perm_bin_label <- function(k) {
  dplyr::case_when(
    is.na(k) ~ NA_character_,
    k == -5L ~ "min to -9",
    k == -4L ~ "-8 to -7",
    k == -3L ~ "-6 to -5",
    k == -2L ~ "-4 to -3",
    k == -1L ~ "-2 to -1",
    k == 0L ~ "0 to 1",
    k == 1L ~ "2 to 3",
    k == 2L ~ "4 to 5",
    k == 3L ~ "6 to 7",
    k == 4L ~ "8 to 9",
    k == 5L ~ "10 to max",
    TRUE ~ as.character(k)
  )
}

# ------------------------------------------------------------- cache ----

pa_perm_fingerprint <- function() {
  inputs <- c(
    pa_perm_export_permanent(),
    "data/derived/spatial_covars_spei_staggered.rds",
    sprintf("data/derived/hh_%d_rural_simpler.rds", pa_perm_waves),
    pa_perm_matched_main_paths()
  )
  missing <- inputs[!file.exists(inputs)]
  if (length(missing) > 0) {
    stop(
      "pa_perm : entrées manquantes pour le scénario permanent :\n  ",
      paste(missing, collapse = "\n  ")
    )
  }
  tibble::tibble(
    file = inputs,
    md5 = unname(tools::md5sum(inputs)),
    size_b = file.size(inputs),
    mtime = format(file.mtime(inputs), "%Y-%m-%d %H:%M:%S")
  )
}

# État du cache : "current", "stale", "unseeded" ou "missing"
pa_perm_cache_status <- function() {
  paths <- pa_perm_matched_perm_paths()
  if (!all(file.exists(paths))) {
    return("missing")
  }
  mf <- pa_perm_cache_manifest()
  if (!file.exists(mf)) {
    return("unseeded")
  }
  recorded <- readr::read_csv(mf, show_col_types = FALSE) |>
    dplyr::select(file, md5_rec = md5)
  current <- pa_perm_fingerprint()
  merged <- dplyr::left_join(current, recorded, by = "file")
  if (
    nrow(merged) != nrow(current) ||
      any(is.na(merged$md5_rec)) ||
      any(merged$md5 != merged$md5_rec)
  ) {
    "stale"
  } else {
    "current"
  }
}

# Enregistrer l'empreinte courante comme référence du cache. À n'appeler
# qu'après vérification que les intermédiaires correspondent aux entrées
# courantes (le script de substitution le fait automatiquement après une
# exécution complète).
pa_perm_seed_cache <- function() {
  if (!all(file.exists(pa_perm_matched_perm_paths()))) {
    stop(
      "pa_perm_seed_cache : cache incomplet, lancer pa_perm_run_substitution()."
    )
  }
  fp <- pa_perm_fingerprint()
  manifest <- fp |>
    dplyr::mutate(seeded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) |>
    dplyr::select(seeded_at, dplyr::everything())
  readr::write_csv(manifest, pa_perm_cache_manifest())
  invisible(TRUE)
}

# ------------------------------------------------- 04 : jointure ---------
# Reproduit verbatim build_year() de 04-variable-consolidation.qmd
# (millésimes 2000 forcés pour les covariables statiques).
pa_perm_build_year <- function(
  hr_object,
  year,
  hh_rural_path,
  spei_years = (year - 2):year
) {
  spatial_covars_spei <- readRDS(
    "data/derived/spatial_covars_spei_staggered.rds"
  )
  all_covars <- spatial_covars_spei |>
    dplyr::select(
      DHSYEAR,
      DHSCLUST,
      URBAN_RURA,
      treecover_area,
      slope,
      elevation,
      population_count,
      traveltime_2000,
      spei_wc
    )
  all_class <- readr::read_csv(
    pa_perm_export_permanent(),
    show_col_types = FALSE
  )
  vars_to_nest <- c(
    "treecover_area",
    "slope",
    "elevation",
    "population_count",
    "traveltime_2000",
    "spei_wc"
  )
  vintage_override <- c(
    treecover_area = 2000,
    slope = 2000,
    elevation = 2000,
    population_count = 2000,
    traveltime_2000 = 2000
  )
  hr <- hr_object |> dplyr::select(hv001, hv002, hv219, hv220)
  hh_rural <- readr::read_rds(hh_rural_path)
  base <- hh_rural |>
    dplyr::left_join(hr, by = c("hv001", "hv002")) |>
    dplyr::left_join(
      all_covars |> dplyr::filter(DHSYEAR == year) |> dplyr::select(-DHSYEAR),
      by = c("hv001" = "DHSCLUST")
    ) |>
    dplyr::left_join(
      all_class |> dplyr::filter(DHSYEAR == year) |> dplyr::select(-DHSYEAR),
      by = c("hv001" = "DHSCLUST")
    ) |>
    dplyr::mutate(DHSYEAR = year) |>
    dplyr::relocate(DHSYEAR, .before = dplyr::everything())
  df_long <- base |>
    dplyr::select(hv001, hv002, dplyr::any_of(vars_to_nest)) |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(vars_to_nest),
      names_to = "indicator",
      values_to = "data"
    ) |>
    tidyr::unnest(data) |>
    dplyr::filter(indicator != "spei_wc" | year(datetime) %in% spei_years) |>
    dplyr::mutate(
      dt_year = year(datetime),
      dt_year = if_else(
        !is.na(datetime) &
          indicator %in% names(vintage_override) &
          inherits(datetime, "POSIXct") &
          dt_year == vintage_override[indicator] - 1,
        unname(vintage_override[indicator]),
        dt_year
      ),
      year_indicator = paste0(indicator, "_", dt_year)
    ) |>
    dplyr::select(hv001, hv002, year_indicator, value)
  df_wide <- df_long |>
    tidyr::pivot_wider(
      names_from = year_indicator,
      values_from = value,
      names_glue = "{year_indicator}"
    ) |>
    dplyr::group_by(hv001, hv002) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  base |>
    dplyr::select(-dplyr::any_of(vars_to_nest)) |>
    dplyr::distinct(hv001, hv002, .keep_all = TRUE) |>
    dplyr::left_join(df_wide, by = c("hv001", "hv002"))
}

# ------------------------------------------------- 06 : appariement ------
# Règle figée du 06-matching.qmd : cardinalité ATT, tols = 0.095
# (standardisé), solveur highs. Garde-fous : 100 % de rétention des
# traités, toutes les AP traitées représentées, SMD externes <= 0.10.
pa_perm_run_matching_year <- function(year) {
  dat <- readRDS(file.path(
    pa_perm_data_dir(),
    sprintf("hr_%d_final.rds", year)
  ))
  matching_variables <- pa_perm_matching_variables
  dat_m <- dat |>
    dplyr::filter(GROUP %in% c("Treatment", "Control")) |>
    dplyr::mutate(treatment = if_else(GROUP == "Treatment", 1L, 0L)) |>
    tidyr::drop_na(dplyr::all_of(intersect(matching_variables, names(.))))
  n_treat <- sum(dat_m$treatment == 1, na.rm = TRUE)
  n_ctrl <- sum(dat_m$treatment == 0, na.rm = TRUE)
  prop_ids <- readr::read_csv(
    "data/PA_creation/wdpa_data/dynamic_wdpa_metadata.csv",
    show_col_types = FALSE
  ) |>
    dplyr::filter(STATUS == "Proposed") |>
    dplyr::pull(WDPAID)
  stopifnot(
    all(
      dat$GROUP[!is.na(dat$GROUP)] %in% c("Treatment", "Control", "Excluded")
    ),
    all(!is.na(dat_m$GROUP)),
    all(dat_m$treated_now[dat_m$treatment == 1L] %in% c(0L, 1L)),
    !any(dat_m$WDPAID[dat_m$treatment == 1L] %in% prop_ids)
  )
  X_match <- dat_m |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(matching_variables)) |>
    as.data.frame()
  fml <- as.formula(paste(
    "treatment ~",
    paste(matching_variables, collapse = " + ")
  ))
  m_out <- MatchIt::matchit(
    formula = fml,
    data = dat_m,
    method = "cardinality",
    estimand = "ATT",
    ratio = NA,
    tols = pa_perm_profile_tol,
    std.tols = TRUE,
    solver = "highs",
    time = 1800
  )
  matched <- MatchIt::match.data(m_out, data = sf::st_drop_geometry(dat_m)) |>
    dplyr::filter(weights > 0)
  bt <- cobalt::bal.tab(m_out, estimand = "ATT", un = TRUE)
  smd <- abs(bt$Balance$Diff.Adj)
  stopifnot(
    sum(matched$treatment == 1L) == n_treat,
    setequal(
      unique(dat_m$WDPAID[dat_m$treatment == 1L]),
      unique(matched$WDPAID[matched$treatment == 1L])
    ),
    max(smd) <= 0.10 + 1e-8
  )
  saveRDS(
    matched,
    file.path(pa_perm_data_dir(), sprintf("data_matched_%d.rds", year))
  )
  tibble::tibble(
    wave = year,
    n_filt = nrow(dat_m),
    n_treat = n_treat,
    n_ctrl = n_ctrl,
    n_matched = nrow(matched),
    n_treated_matched = sum(matched$treatment == 1L),
    n_control_matched = sum(matched$treatment == 0L),
    max_smd = round(max(smd), 4)
  )
}

# Chaîne complète 04 -> 06 en isolation, puis enregistrement de
# l'empreinte du cache. Ne modifie jamais data/derived/.
pa_perm_run_substitution <- function(force = FALSE) {
  status <- pa_perm_cache_status()
  if (identical(status, "current") && !isTRUE(force)) {
    message("pa_perm : cache déjà valide, rien à recalculer.")
    return(invisible(readr::read_csv(
      file.path(pa_perm_output_dir(), "matching_summary_permanent.csv"),
      show_col_types = FALSE
    )))
  }
  dir.create(pa_perm_data_dir(), showWarnings = FALSE, recursive = TRUE)
  dir.create(pa_perm_output_dir(), showWarnings = FALSE, recursive = TRUE)
  hr_perm <- list()
  hr_perm[["1997"]] <- haven::read_dta(
    "data/raw/dhs/DHS_1997/MDHR31DT/MDHR31FL.DTA"
  ) |>
    pa_perm_build_year(
      1997,
      "data/derived/hh_1997_rural_simpler.rds",
      1995:1997
    )
  hr_perm[["2008"]] <- haven::read_dta(
    "data/raw/dhs/DHS_2008/MDHR51DT/MDHR51FL.DTA"
  ) |>
    pa_perm_build_year(
      2008,
      "data/derived/hh_2008_rural_simpler.rds",
      2006:2008
    )
  hr_perm[["2011"]] <- haven::read_dta(
    "data/raw/dhs/DHS_2011/MDHR61DT/MDHR61FL.DTA"
  ) |>
    pa_perm_build_year(
      2011,
      "data/derived/hh_2011_rural_simpler.rds",
      2009:2011
    )
  hr_perm[["2013"]] <- haven::read_dta(
    "data/raw/dhs/DHS_2013/MDHR6ADT/MDHR6AFL.DTA"
  ) |>
    pa_perm_build_year(
      2013,
      "data/derived/hh_2013_rural_simpler.rds",
      2011:2013
    )
  hr_perm[["2016"]] <- haven::read_dta(
    "data/raw/dhs/DHS_2016/MDHR71DT/MDHR71FL.DTA"
  ) |>
    pa_perm_build_year(
      2016,
      "data/derived/hh_2016_rural_simpler.rds",
      2014:2016
    )
  hr_perm[["2018"]] <- haven::read_spss(
    "data/raw/mics/2018/SPSS datasets/hh.sav"
  ) |>
    dplyr::select(hv001 = HH1, hv002 = HH2, hv219 = HHSEX, hv220 = HHAGE) |>
    pa_perm_build_year(
      2018,
      "data/derived/hh_2018_rural_simpler.rds",
      2016:2018
    )
  hr_perm[["2021"]] <- haven::read_dta(
    "data/raw/dhs/DHS_2021/MDHR81DT/MDHR81FL.DTA"
  ) |>
    pa_perm_build_year(
      2021,
      "data/derived/hh_2021_rural_simpler.rds",
      2019:2021
    )
  for (y in pa_perm_waves) {
    readr::write_rds(
      hr_perm[[as.character(y)]],
      file.path(pa_perm_data_dir(), sprintf("hr_%d_final.rds", y))
    )
  }
  match_perm <- purrr::map_dfr(pa_perm_waves, pa_perm_run_matching_year)
  readr::write_csv(
    match_perm,
    file.path(pa_perm_output_dir(), "matching_summary_permanent.csv")
  )
  pkg_versions <- list(
    R = paste(R.version$major, R.version$minor, sep = "."),
    MatchIt = as.character(packageVersion("MatchIt")),
    cobalt = as.character(packageVersion("cobalt")),
    highs = as.character(packageVersion("highs"))
  )
  jsonlite::write_json(
    pkg_versions,
    file.path(pa_perm_output_dir(), "pkg_versions_substitution.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  pa_perm_seed_cache()
  invisible(match_perm)
}

# Chargement du cache validé. Si le cache est absent ou périmé, la
# fonction s'arrête avec le mode d'emploi (aucun recalcul silencieux) ;
# pa_perm_load_matched(regenerate = TRUE) relance la chaîne complète.
pa_perm_load_matched <- function(regenerate = FALSE) {
  status <- pa_perm_cache_status()
  if (!identical(status, "current")) {
    howto <- paste(
      "Reproduire le scénario permanent :",
      "source(\"utils/pa_permanent_scenario.R\") puis",
      "pa_perm_run_substitution(force = TRUE)"
    )
    if (identical(status, "unseeded") && !isTRUE(regenerate)) {
      stop(paste(
        "pa_perm : cache présent mais non enregistré (pas de",
        "cache_inputs.csv). Vérifier les entrées puis exécuter",
        "pa_perm_seed_cache(), ou régénérer :",
        howto
      ))
    }
    if (!isTRUE(regenerate)) {
      stop(paste0(
        "pa_perm : cache ",
        status,
        " (entrées modifiées depuis l'appariement). ",
        howto
      ))
    }
    pa_perm_run_substitution(force = TRUE)
  }
  purrr::map(pa_perm_waves, function(y) {
    readr::read_rds(file.path(
      pa_perm_data_dir(),
      sprintf("data_matched_%d.rds", y)
    ))
  }) |>
    purrr::set_names(as.character(pa_perm_waves))
}

# ------------------------------------ dat / dat2 (verbatim 07) -----------

pa_perm_mk_wave <- function(y, path) {
  d <- readr::read_rds(path)
  nms <- c(
    spei_wc_n_2 = paste0("spei_wc_", y - 2),
    spei_wc_n_1 = paste0("spei_wc_", y - 1),
    spei_wc_n = paste0("spei_wc_", y)
  )
  d |>
    dplyr::rename(!!!nms) |>
    dplyr::mutate(
      hv219 = haven::zap_labels(hv219),
      hv220 = haven::zap_labels(hv220)
    )
}

pa_perm_build_dat <- function(paths) {
  purrr::map2(pa_perm_waves, paths, pa_perm_mk_wave) |>
    dplyr::bind_rows() |>
    dplyr::filter(GROUP %in% c("Treatment", "Control")) |>
    dplyr::mutate(
      hv219 = factor(hv219, levels = c(1, 2), labels = c("Homme", "Femme")),
      hv220 = as.numeric(hv220),
      treat = as.integer(GROUP == "Treatment"),
      treatment_date = as.Date(treatment_date),
      survey_ref_date = pa_perm_survey_reference_date(DHSYEAR),
      w_svy = hv005 / 1e6,
      w_all = w_svy * weights,
      id = dplyr::row_number(),
      cluster_uid = interaction(DHSYEAR, hv001, drop = TRUE)
    )
}

pa_perm_build_dat2 <- function(dat) {
  dat |>
    dplyr::mutate(
      treat_on = as.integer(
        GROUP == "Treatment" & survey_ref_date >= treatment_date
      ),
      g_eff = {
        ty <- as.integer(format(treatment_date, "%Y"))
        ty[is.na(ty)] <- 2000L
        if_else(
          treat == 1L,
          ty + as.integer(treatment_date > pa_perm_survey_reference_date(ty)),
          NA_integer_
        )
      },
      treatment_cohort = factor(
        if_else(treat == 1L, as.character(g_eff), "never")
      ),
      rel_year_frac = as.numeric(survey_ref_date - treatment_date) / 365.25,
      rel_year = case_when(
        treat == 1L & !is.na(treatment_date) ~ floor(rel_year_frac),
        TRUE ~ Inf
      ),
      rel_year_clipped = case_when(
        is.infinite(rel_year) ~ NA_real_,
        TRUE ~ pmax(pmin(rel_year, 10), -10)
      ),
      rel_year_2yr = case_when(
        is.na(rel_year_clipped) ~ NA_integer_,
        rel_year_clipped <= -9 ~ -5,
        rel_year_clipped <= -7 ~ -4,
        rel_year_clipped <= -5 ~ -3,
        rel_year_clipped <= -3 ~ -2,
        rel_year_clipped <= -1 ~ -1,
        rel_year_clipped <= 1 ~ 0,
        rel_year_clipped <= 3 ~ 1,
        rel_year_clipped <= 5 ~ 2,
        rel_year_clipped <= 7 ~ 3,
        rel_year_clipped <= 9 ~ 4,
        TRUE ~ 5
      ),
      GROUP2 = case_when(
        GROUP == "Treatment" ~ "Treat.",
        GROUP == "Control" ~ "Ctrl."
      )
    ) |>
    dplyr::mutate(
      rel_year_2yr2 = case_when(
        is.na(rel_year_2yr) ~ "never",
        TRUE ~ as.character(rel_year_2yr)
      ),
      rel_year_2yr2 = factor(rel_year_2yr2, levels = pa_perm_es_levels)
    )
}

# ------------------------------------------------- gardes-fous -----------

# Cohérence estimateur/export au niveau grappe-vague (comme
# run_estimation_comparison.R) + unicité des clés ménages + codage des
# contrôles. Retourne la table de réconciliation (agrégée, non
# disclosive).
pa_perm_check_export_exposure <- function(dat2, export_csv) {
  exp_cls <- readr::read_csv(export_csv, show_col_types = FALSE) |>
    dplyr::transmute(
      DHSYEAR = as.integer(DHSYEAR),
      DHSCLUST = as.integer(DHSCLUST),
      GROUP_exp = GROUP,
      treated_now_exp = as.integer(treated_now)
    )
  imp <- dat2 |>
    sf::st_drop_geometry() |>
    dplyr::group_by(DHSYEAR, hv001) |>
    dplyr::summarise(
      treat_on = max(treat_on),
      n_treat_group = sum(treat == 1L),
      .groups = "drop"
    ) |>
    dplyr::mutate(hv001 = as.integer(hv001))
  j <- dplyr::full_join(
    exp_cls,
    imp,
    by = c("DHSYEAR", "DHSCLUST" = "hv001")
  ) |>
    dplyr::mutate(
      in_both = !is.na(GROUP_exp) & !is.na(treat_on),
      eligible_exp = GROUP_exp %in% c("Treatment", "Control")
    )
  recon <- j |>
    dplyr::group_by(DHSYEAR) |>
    dplyr::summarise(
      n_export_eligible = sum(eligible_exp),
      n_compared = sum(in_both & eligible_exp),
      n_mismatch = sum(in_both & eligible_exp & treated_now_exp != treat_on),
      n_export_lost = sum(eligible_exp & is.na(treat_on)),
      .groups = "drop"
    )
  keys <- dat2 |>
    sf::st_drop_geometry() |>
    dplyr::group_by(DHSYEAR, hv001, hv002) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  stopifnot(
    "pa_perm : clé ménage (DHSYEAR, hv001, hv002) non unique" = all(
      keys$n == 1L
    ),
    "pa_perm : date de traitement présente pour un contrôle" = all(is.na(dat2$treatment_date[
      dat2$treat == 0
    ])),
    "pa_perm : date de traitement manquante pour un Treatment" = all(
      !is.na(dat2$treatment_date[dat2$treat == 1])
    ),
    "pa_perm : contrôle avec treat_on = 1" = all(
      dat2$treat_on[dat2$treat == 0] == 0L
    ),
    "pa_perm : contrôle avec g_eff renseigné" = all(is.na(dat2$g_eff[
      dat2$treat == 0
    ]))
  )
  recon
}

# Bornes temporelles (décembre 2008 non traités en 2008 ; cohorte 2022
# absente de l'échantillon estimateur).
pa_perm_boundary_checks <- function(dat2, label) {
  b <- dat2 |>
    sf::st_drop_geometry() |>
    dplyr::filter(treat == 1L, DHSYEAR == 2008) |>
    dplyr::summarise(
      n_treat_2008 = dplyr::n(),
      n_exposed_2008 = sum(treated_now == 1),
      n_cohort2022_clusters = sum(g_eff == 2022)
    )
  stopifnot(
    "pa_perm : actes de décembre 2008 traités à la vague 2008" = all(
      b$n_exposed_2008 == 0
    ),
    "pa_perm : cohorte 2022 (Ankafobe) présente dans l'échantillon" = all(
      b$n_cohort2022_clusters == 0
    )
  )
  invisible(b)
}

# ------------------------------------------------- estimation ------------

pa_perm_fit_static <- function(dat2, yvar) {
  did2s::did2s(
    data = dat2,
    yname = yvar,
    first_stage = pa_perm_first_stage(),
    second_stage = ~ i(treat_on, ref = FALSE),
    treatment = "treat_on",
    cluster_var = "cluster_uid",
    weights = "w_all"
  )
}

pa_perm_fit_es <- function(dat2, yvar) {
  did2s::did2s(
    data = dat2,
    yname = yvar,
    first_stage = pa_perm_first_stage(),
    second_stage = ~ i(rel_year_2yr2, ref = c("-1", "never")),
    treatment = "treat_on",
    cluster_var = "cluster_uid",
    weights = "w_all"
  )
}

# H3 : sous-échantillons strict (IUCN I-IV) / multi (IUCN V-VI),
# définition partagée via utils/iucn_sets.R.
pa_perm_h3_subset <- function(dat2, which) {
  if (which == "strict") {
    dat2 |>
      dplyr::mutate(
        grp = as.integer(GROUP == "Treatment" & is_strict(IUCN_CAT)),
        treat_on = as.integer(grp == 1 & survey_ref_date >= treatment_date)
      ) |>
      dplyr::filter(grp == 1 | GROUP == "Control")
  } else {
    dat2 |>
      dplyr::mutate(
        grp = as.integer(GROUP == "Treatment" & is_multi(IUCN_CAT)),
        treat_on = as.integer(grp == 1 & survey_ref_date >= treatment_date)
      ) |>
      dplyr::filter(grp == 1 | GROUP == "Control")
  }
}

pa_perm_model_specs <- tibble::tribble(
  ~model_id          , ~yvar                           , ~family  , ~sub     ,
  "h1_static"        , "wealth_centile_rural_weighted" , "static" , "all"    ,
  "h1_es"            , "wealth_centile_rural_weighted" , "es"     , "all"    ,
  "h2_static"        , "zscore_wealth"                 , "static" , "all"    ,
  "h2_es"            , "zscore_wealth"                 , "es"     , "all"    ,
  "h3_static_strict" , "wealth_centile_rural_weighted" , "static" , "strict" ,
  "h3_static_multi"  , "wealth_centile_rural_weighted" , "static" , "multi"  ,
  "h3_es_both"       , "wealth_centile_rural_weighted" , "es"     , "all"    ,
  "h3_es_strict"     , "wealth_centile_rural_weighted" , "es"     , "strict" ,
  "h3_es_multi"      , "wealth_centile_rural_weighted" , "es"     , "multi"
)

pa_perm_run_models <- function(
  dat2,
  scenario_label,
  specs = pa_perm_model_specs
) {
  purrr::pmap(specs, function(model_id, yvar, family, sub) {
    d <- if (sub == "all") dat2 else pa_perm_h3_subset(dat2, sub)
    f <- if (family == "static") pa_perm_fit_static else pa_perm_fit_es
    res <- tryCatch(
      f(d, yvar),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(res$error)) {
      tibble::tibble(
        scenario = scenario_label,
        model_id,
        outcome = yvar,
        family,
        term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        nobs = NA_integer_,
        note = paste("NOT ESTIMABLE:", res$error)
      )
    } else {
      tid <- broom::tidy(res, conf.int = TRUE)
      nobs <- length(res$residuals)
      if (family == "static") {
        tid <- tid |> dplyr::filter(term == "treat_on::1")
      } else {
        tid <- tid |> dplyr::filter(grepl("^rel_year_2yr2::", term))
      }
      tibble::tibble(
        scenario = scenario_label,
        model_id,
        outcome = yvar,
        family,
        term = tid$term,
        estimate = tid$estimate,
        std.error = tid$std.error,
        conf.low = tid$conf.low,
        conf.high = tid$conf.high,
        nobs = as.integer(nobs),
        note = NA_character_
      )
    }
  }) |>
    dplyr::bind_rows()
}

# --------------------------------- diagnostic à échantillon commun -------

# Intersection des deux échantillons appariés, poids de l'appariement
# principal maintenus fixes (diagnostic de décomposition, PAS une
# estimation causale de la permanence). Retourne les jeux dat2
# (échantillon commun et sous-ensemble « même AP ») + comptes agrégés.
pa_perm_common_sample_prep <- function(dat_main, dat_perm, dat2_main, dat2_perm) {
  key_main <- dat_main |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      DHSYEAR,
      hv001,
      hv002,
      w_main = w_all,
      group_main = GROUP,
      wdpaid_main = WDPAID,
      treat_main = treat,
      tdate_main = treatment_date
    )
  key_perm <- dat_perm |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      DHSYEAR,
      hv001,
      hv002,
      w_perm = w_all,
      group_perm = GROUP,
      wdpaid_perm = WDPAID,
      treat_perm = treat,
      tdate_perm = treatment_date
    )
  common <- dplyr::inner_join(
    key_main,
    key_perm,
    by = c("DHSYEAR", "hv001", "hv002")
  )
  stopifnot(
    "pa_perm : groupe divergent dans l'échantillon commun" = all(
      common$group_main == common$group_perm
    )
  )
  counts <- common |>
    dplyr::summarise(
      n_hh = dplyr::n(),
      n_treat_main = sum(treat_main == 1),
      n_treat_perm = sum(treat_perm == 1),
      n_treat_disagree = sum(treat_main != treat_perm),
      n_same_pa = sum(
        treat_main == 1 & treat_perm == 1 & wdpaid_main == wdpaid_perm
      ),
      n_pa_reassign = sum(
        treat_main == 1 & treat_perm == 1 & wdpaid_main != wdpaid_perm
      ),
      n_weight_diff = sum(abs(w_main - w_perm) > 1e-12)
    )
  common_keys <- common |> dplyr::select(DHSYEAR, hv001, hv002)
  hold_weights <- function(dat2) {
    dat2 |>
      sf::st_drop_geometry() |>
      dplyr::semi_join(common_keys, by = c("DHSYEAR", "hv001", "hv002")) |>
      dplyr::left_join(
        common |> dplyr::select(DHSYEAR, hv001, hv002, w_main),
        by = c("DHSYEAR", "hv001", "hv002")
      ) |>
      dplyr::mutate(w_all = w_main) |> # poids fixés au scénario principal
      dplyr::select(-w_main)
  }
  dat2_common_main <- hold_weights(dat2_main)
  dat2_common_perm <- hold_weights(dat2_perm)
  stopifnot(
    nrow(dat2_common_main) == nrow(dat2_common_perm),
    nrow(dat2_common_main) == nrow(common)
  )
  samepa_keys <- common |>
    dplyr::filter(
      treat_main == 1 & treat_perm == 1 & wdpaid_main == wdpaid_perm
    ) |>
    dplyr::select(DHSYEAR, hv001, hv002) |>
    dplyr::bind_rows(
      common |>
        dplyr::filter(treat_main == 0 & treat_perm == 0) |>
        dplyr::select(DHSYEAR, hv001, hv002)
    )
  dat2_samepa_main <- dat2_common_main |>
    dplyr::semi_join(samepa_keys, by = c("DHSYEAR", "hv001", "hv002"))
  dat2_samepa_perm <- dat2_common_perm |>
    dplyr::semi_join(samepa_keys, by = c("DHSYEAR", "hv001", "hv002"))
  stopifnot(nrow(dat2_samepa_main) == nrow(dat2_samepa_perm))
  list(
    dat2_common_main = dat2_common_main,
    dat2_common_perm = dat2_common_perm,
    dat2_samepa_main = dat2_samepa_main,
    dat2_samepa_perm = dat2_samepa_perm,
    counts = counts
  )
}

# Support par binne d'event-study (ménages, grappes, AP, cohortes) —
# diagnostics agrégés uniquement.
pa_perm_es_support <- function(dat2, scenario_label) {
  d <- dat2 |> sf::st_drop_geometry()
  treated_side <- d |>
    dplyr::filter(treat == 1L, !is.na(rel_year_2yr)) |>
    dplyr::group_by(bin = rel_year_2yr2) |>
    dplyr::summarise(
      n_hh_treated_group = dplyr::n(),
      n_hh_exposed = sum(treated_now == 1),
      n_clusters = dplyr::n_distinct(interaction(DHSYEAR, hv001, drop = TRUE)),
      n_pas = dplyr::n_distinct(WDPAID),
      n_cohorts = dplyr::n_distinct(g_eff),
      waves = paste(sort(unique(DHSYEAR)), collapse = ";"),
      .groups = "drop"
    ) |>
    dplyr::mutate(bin = as.character(bin))
  never_n <- sum(d$rel_year_2yr2 == "never")
  out <- treated_side |>
    dplyr::mutate(scenario = scenario_label) |>
    dplyr::bind_rows(tibble::tibble(
      bin = "never",
      scenario = scenario_label,
      n_hh_treated_group = 0,
      n_hh_exposed = 0,
      n_clusters = 0,
      n_pas = 0,
      n_cohorts = 0,
      waves = NA_character_,
      n_hh_untreated_group = never_n
    ))
  k <- suppressWarnings(as.integer(out$bin))
  out$bin_label <- if_else(out$bin == "never", "never", pa_perm_bin_label(k))
  out
}

# Deltas d'affectation entre les deux exports (agrégats par vague).
pa_perm_assignment_deltas <- function() {
  main_cls <- readr::read_csv(pa_perm_export_main(), show_col_types = FALSE)
  perm_cls <- readr::read_csv(
    pa_perm_export_permanent(),
    show_col_types = FALSE
  )
  dplyr::inner_join(
    main_cls |>
      dplyr::transmute(
        DHSYEAR,
        DHSCLUST,
        g_main = GROUP,
        tn_main = treated_now,
        wd_main = WDPAID,
        td_main = treatment_date
      ),
    perm_cls |>
      dplyr::transmute(
        DHSYEAR,
        DHSCLUST,
        g_perm = GROUP,
        tn_perm = treated_now,
        wd_perm = WDPAID,
        td_perm = treatment_date
      ),
    by = c("DHSYEAR", "DHSCLUST")
  ) |>
    dplyr::group_by(DHSYEAR) |>
    dplyr::summarise(
      n_clusters = dplyr::n(),
      n_treatment_to_excluded = sum(
        g_main == "Treatment" & g_perm == "Excluded"
      ),
      n_treated_now_flips = sum(
        g_main == "Treatment" & g_perm == "Treatment" & tn_main != tn_perm
      ),
      n_cohort_changes = sum(
        g_main == "Treatment" & g_perm == "Treatment" & td_main != td_perm
      ),
      n_wdpaid_changes = sum(
        g_main == "Treatment" & g_perm == "Treatment" & wd_main != wd_perm
      ),
      .groups = "drop"
    )
}
