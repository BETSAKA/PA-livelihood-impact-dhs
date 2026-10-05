# Diagnostic caliper — outcome-blind, exploratory settings only.
#
# NE PAS utiliser pour le matching de production : pop.size / max.generations
# sont volontairement reduits. Les resultats sont ecrits exclusivement sous
# data/derived/caliper_diagnostics/ et output/review_v2/caliper_diagnostics/
# afin de ne jamais ecraser les fichiers de production.
#
# Aucune variable d'outcome n'est chargee : la selection du caliper repose
# uniquement sur l'equilibre des covariables (cobalt) et la retention.

library(tidyverse)
library(MatchIt)
library(cobalt)

matching_variables <- c(
  "treecover_area_2000", "slope_2000", "elevation_2000",
  "population_count_2000", "traveltime_2000_2000"
)

# memes graines que la production, pour la comparabilite
MATCHING_BASE_SEED <- 20261004L

prep_matching <- function(df_final) {
  df_final %>%
    filter(GROUP %in% c("Treatment", "Control")) %>%
    mutate(treatment = if_else(GROUP == "Treatment", 1L, 0L)) %>%
    drop_na(all_of(intersect(matching_variables, names(.))))
}

# SMD absolu par covariable, convention cobalt ATT (ecart-type du groupe traite)
smd_att <- function(df, vars) {
  X <- as.matrix(df[vars])
  tr <- df$treatment == 1
  m_t <- colMeans(X[tr, , drop = FALSE])
  m_c <- colMeans(X[!tr, , drop = FALSE])
  s_t <- apply(X[tr, , drop = FALSE], 2, sd)
  setNames(abs((m_t - m_c) / s_t), vars)
}

# SMD absolu par covariable, convention pooled (comme 06-matching)
smd_pooled <- function(df, vars) {
  X <- as.matrix(df[vars])
  tr <- df$treatment == 1
  m_t <- colMeans(X[tr, , drop = FALSE])
  m_c <- colMeans(X[!tr, , drop = FALSE])
  v_t <- apply(X[tr, , drop = FALSE], 2, var)
  v_c <- apply(X[!tr, , drop = FALSE], 2, var)
  setNames(abs((m_t - m_c) / sqrt((v_t + v_c) / 2)), vars)
}

run_diag <- function(year, width,
                     pop.size = 100, max.generations = 10,
                     wait.generations = 2) {
  cat("\n=== Diagnostic caliper", width, "| vague", year, "===\n")
  fin_path <- glue::glue("data/derived/hr_{year}_final.rds")
  if (!file.exists(fin_path)) stop("Fichier introuvable : ", fin_path)

  dat <- readRDS(fin_path)
  dat_m <- prep_matching(dat)
  n_treat <- sum(dat_m$treatment == 1L)
  n_ctrl  <- sum(dat_m$treatment == 0L)
  cat(glue::glue(">> Eligibles : traites={n_treat}, controles={n_ctrl}\n"))

  cal <- setNames(rep(width, length(matching_variables)), matching_variables)
  seed <- MATCHING_BASE_SEED + as.integer(year)
  set.seed(seed)

  t0 <- Sys.time()
  m_out <- matchit(
    formula   = reformulate(matching_variables, response = "treatment"),
    data      = dat_m,
    method    = "genetic",
    distance  = "mahalanobis",
    estimand  = "ATT",
    replace   = FALSE,
    ratio     = 1,
    caliper   = cal,
    std.caliper = TRUE,
    pop.size  = pop.size,
    max.generations = max.generations,
    wait.generations = wait.generations
  )
  minutes <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(glue::glue(">> matchit temps = {round(minutes, 1)} min\n"))

  matched <- match.data(m_out, data = sf::st_drop_geometry(dat_m)) %>%
    filter(weights > 0)

  n_mt <- sum(matched$treatment == 1L)
  n_mc <- sum(matched$treatment == 0L)

  smd_cov <- smd_att(matched, matching_variables)
  smd_cov_pooled <- smd_pooled(matched, matching_variables)

  worst <- names(which.max(smd_cov))

  out_row <- tibble(
    year = year,
    caliper = width,
    eligible_treated = n_treat,
    eligible_control = n_ctrl,
    matched_treated = n_mt,
    matched_control = n_mc,
    treated_retention = round(n_mt / n_treat, 4),
    max_smd_cobalt = round(max(smd_cov), 4),
    worst_covariate = worst,
    max_smd_pooled = round(max(smd_cov_pooled), 4),
    runtime_min = round(minutes, 1),
    pop.size = pop.size,
    max.generations = max.generations,
    seed = seed
  )

  # per-covariate SMDs, cobalt convention (wide columns)
  pc <- tibble(
    year = year,
    caliper = width,
    !!!setNames(as.list(round(smd_cov, 4)),
                paste0("smd_", matching_variables))
  )

  # sauvegarde des objets (diagnostic uniquement)
  dir.create("data/derived/caliper_diagnostics", showWarnings = FALSE)
  dir.create("output/review_v2/caliper_diagnostics", showWarnings = FALSE)
  tag <- sub("\\.", "p", format(width, nsmall = 2))
  saveRDS(list(
    m_out = m_out,
    meta = list(
      purpose = "diagnostic_only_not_for_outcomes",
      year = year, caliper = width, seed = seed,
      pop.size = pop.size, max.generations = max.generations,
      wait.generations = wait.generations,
      n_eligible = nrow(dat_m), n_treated = n_treat, n_control = n_ctrl
    )
  ), glue::glue("data/derived/caliper_diagnostics/matching_result_{year}_cal{tag}.rds"))
  saveRDS(matched,
          glue::glue("data/derived/caliper_diagnostics/data_matched_{year}_cal{tag}.rds"))

  # CSV cumulatifs
  res_path <- "output/review_v2/caliper_diagnostics/caliper_diag_results.csv"
  cov_path <- "output/review_v2/caliper_diagnostics/caliper_diag_covariates.csv"
  if (file.exists(res_path)) {
    existing <- read_csv(res_path, show_col_types = FALSE) |>
      filter(!(year == !!year & caliper == !!width))
    write_csv(bind_rows(existing, out_row), res_path)
  } else {
    write_csv(out_row, res_path)
  }
  if (file.exists(cov_path)) {
    existing <- read_csv(cov_path, show_col_types = FALSE) |>
      filter(!(year == !!year & caliper == !!width))
    write_csv(bind_rows(existing, pc), cov_path)
  } else {
    write_csv(pc, cov_path)
  }

  cat(">> Resultat :\n")
  print(out_row)
  cat(">> SMD par covariable (cobalt) :\n")
  print(round(smd_cov, 4))
  invisible(list(row = out_row, smd = smd_cov, matched = matched))
}
