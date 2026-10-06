# Reproduction de l'audit du test conjoint de pre-tendance et du soutien par cohorte
# (did 2.1.2, modele echelonne fige du chapitre 07b).
#
# Ce script re-execute le modele fige (meme outcome, controles PAP, poids,
# clusterisation, est_method = "ipw", never-treated) puis :
#   A. reconstruit manuellement le test conjoint Wpval de att_gt()
#   B. decompose l'event-study dynamique par cellule ATT(g,t) et cohorte
#   C. produit les diagnostics de soutien par cohorte (2011 / 2016 / 2021)
# Aucun choix de design n'est modifie ; rien ici n'est une re-estimation
# alternative. Les tirages bootstrap multiplieur de did ne sont pas seedables :
# les SE bootstrap peuvent varier legerement d'une execution a l'autre ; les CSV
# exportes depuis la session figee restent l'enregistrement canonique.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(did)
  library(sf)
})

out_dir <- "output/review_v2/did_pretrend_audit"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- donnees identiques a 07b-estimation-staggered-did.qmd -----------------
survey_reference_date <- function(year) as.Date(sprintf("%d-06-01", year))
survey_years <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)

dlist <- lapply(survey_years, function(y) {
  read_rds(glue::glue("data/derived/data_matched_{y}.rds")) |>
    rename(spei_wc_n_1 = !!glue::glue("spei_wc_{y-1}")) |>
    mutate(
      hv219 = zap_labels(hv219),
      hv220 = as.numeric(zap_labels(hv220)),
      DHSYEAR = y
    )
})
dat <- bind_rows(dlist) |>
  filter(GROUP %in% c("Treatment", "Control")) |>
  st_drop_geometry() |>
  mutate(
    treat = as.integer(GROUP == "Treatment"),
    treatment_date = as.Date(treatment_date),
    survey_ref_date = survey_reference_date(DHSYEAR),
    hv219_femme = as.integer(hv219 == 2),
    w_svy = hv005 / 1e6,
    w_all = w_svy * weights,
    cluster_uid_num = as.integer(interaction(DHSYEAR, hv001, drop = TRUE))
  )

pa_cohorts <- dat |>
  filter(GROUP == "Treatment") |>
  distinct(WDPAID, treatment_date) |>
  mutate(
    first_treated_survey_year = vapply(
      treatment_date,
      function(td) {
        w <- survey_years[vapply(
          survey_years,
          function(y) survey_reference_date(y) >= td,
          logical(1)
        )]
        if (length(w) == 0) 0 else min(w)
      },
      numeric(1)
    )
  )
did_dat <- dat |>
  left_join(
    pa_cohorts |> select(WDPAID, first_treated_survey_year) |> distinct(),
    by = "WDPAID"
  ) |>
  mutate(g = if_else(GROUP == "Treatment", first_treated_survey_year, 0))

controls_pap <- c(
  "spei_wc_n_1",
  "hv219_femme",
  "hv220",
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)
analysis_vars <- c(
  "wealth_centile_rural_weighted",
  controls_pap,
  "w_all",
  "g",
  "DHSYEAR",
  "cluster_uid_num"
)
did_dat <- did_dat |> filter(if_all(all_of(analysis_vars), complete.cases))

## ---- modele fige ------------------------------------------------------------
STAGGERED_EST_METHOD <- "ipw"
xformla_pap <- as.formula(paste("~", paste(controls_pap, collapse = " + ")))

attgt_out <- att_gt(
  yname = "wealth_centile_rural_weighted",
  tname = "DHSYEAR",
  idname = "cluster_uid_num",
  gname = "g",
  xformla = xformla_pap,
  data = as.data.frame(did_dat),
  panel = FALSE,
  weightsname = "w_all",
  control_group = "nevertreated",
  clustervars = "cluster_uid_num",
  bstrap = TRUE,
  cband = TRUE,
  biters = 1000,
  est_method = STAGGERED_EST_METHOD
)

## ---- A. reconstruction du test conjoint ------------------------------------
att <- attgt_out$att
grp <- attgt_out$group
tt <- attgt_out$t
n <- attgt_out$n
Vd <- as.matrix(attgt_out$V_analytical)
se_ana <- sqrt(diag(Vd) / n)
zero_na <- which(is.na(se_ana) | se_ana <= sqrt(.Machine$double.eps) * 10)
pre <- which(grp > tt)
pre <- pre[!(pre %in% zero_na)]
preatt <- att[pre]
preV <- Vd[pre, pre]
q <- length(pre)

W_man <- drop(n * t(preatt) %*% solve(preV) %*% preatt)
p_man <- 1 - pchisq(W_man, q)

# covariance cluster-robuste depuis les memes fonctions d'influence
# (somme des IF par grappe figee interaction(DHSYEAR, hv001)) ; pas de re-estimation
IFm <- as.matrix(attgt_out$inffunc)
u <- rowsum(IFm, did_dat$cluster_uid_num)
Vcl <- (t(u) %*% u) / n
se_cl <- sqrt(diag(Vcl) / n)
preVcl <- Vcl[pre, pre]
W_cl <- drop(n * t(preatt) %*% solve(preVcl) %*% preatt)
p_cl <- 1 - pchisq(W_cl, q)

wald_export <- data.frame(
  idx = pre,
  group = grp[pre],
  time = tt[pre],
  att = preatt,
  se_bootstrap = attgt_out$se[pre],
  se_analytical = se_ana[pre],
  se_cluster_robust = se_cl[pre],
  W_analytical = W_man,
  df = q,
  p_analytical = p_man,
  W_cluster_robust = W_cl,
  p_cluster_robust = p_cl,
  W_package = unname(attgt_out$W),
  Wpval_package = attgt_out$Wpval
)
write_csv(wald_export, file.path(out_dir, "pretrend_wald_manual.csv"))
write_csv(round(Vd[pre, pre], 6), file.path(out_dir, "pretrend_covariance.csv"))
write_csv(
  round(stats::cov2cor(preV), 6),
  file.path(out_dir, "pretrend_correlation.csv")
)
ev <- eigen(preV, only.values = TRUE)$values
data.frame(
  eigenvalue = ev,
  rank = qr(preV)$rank,
  q = q,
  condition_number = max(ev) / min(ev),
  rcond = rcond(preV)
) |>
  write_csv(file.path(out_dir, "pretrend_eigenvalues.csv"), row.names = FALSE)

cat(sprintf(
  "W package = %.4f | W manuel = %.4f | p manuel = %.3g\n",
  attgt_out$W,
  W_man,
  p_man
))
cat(sprintf("W cluster-robuste = %.4f | p = %.3f\n", W_cl, p_cl))

## ---- B. decomposition dynamique par cellule et cohorte ----------------------
# poids dynamic : pg = part de poids des menages par cohorte (panel=FALSE ->
# chaque menage est sa propre "unite" dans did), normalisee par temps d'evenement
glist_all <- sort(unique(grp))
pg <- sapply(glist_all, function(g) mean(did_dat$w_all * (did_dat$g == g)))
names(pg) <- glist_all

cells_tbl <- data.frame(
  group = grp,
  time = tt,
  estimate = att,
  std.error = attgt_out$se,
  crit_value = unname(attgt_out$c)
)

cell_support <- do.call(
  rbind,
  lapply(seq_len(nrow(cells_tbl)), function(i) {
    g0 <- cells_tbl$group[i]
    t0 <- cells_tbl$time[i]
    dt <- did_dat[
      did_dat$g == g0 & did_dat$DHSYEAR == t0 & did_dat$GROUP == "Treatment",
    ]
    dc <- did_dat[did_dat$DHSYEAR == t0 & did_dat$GROUP == "Control", ]
    data.frame(
      group = g0,
      time = t0,
      treated_hh = nrow(dt),
      treated_clusters = n_distinct(dt$cluster_uid_num),
      treated_pas = n_distinct(dt$WDPAID),
      control_hh = nrow(dc),
      control_clusters = n_distinct(dc$cluster_uid_num)
    )
  })
)

agg_dyn <- aggte(attgt_out, type = "dynamic", na.rm = TRUE)

notna <- !is.na(attgt_out$se)
grp2 <- grp[notna]
tt2 <- tt[notna]
att2 <- att[notna]
eseq <- sort(unique(tt2 - grp2))
dyn_rec <- do.call(
  rbind,
  lapply(eseq, function(e) {
    wh <- which(tt2 - grp2 == e)
    pge <- unname(pg[as.character(grp2[wh])])
    pge <- pge / sum(pge)
    data.frame(
      event_time = e,
      estimate = sum(att2[wh] * pge),
      cells = paste0("(", grp2[wh], ",", tt2[wh], ")", collapse = " "),
      w_2011 = sum(pge[grp2[wh] == 2011]),
      w_2016 = sum(pge[grp2[wh] == 2016]),
      w_2021 = sum(pge[grp2[wh] == 2021]),
      row.names = NULL
    )
  })
)
contrib <- dyn_rec |>
  rowwise() |>
  mutate(cells = list(strsplit(cells, " ")[[1]])) |>
  tidyr::unnest(cells) |>
  mutate(
    group = as.numeric(sub("\\((\\d+),.*", "\\1", cells)),
    time = as.numeric(sub(".*,(\\d+)\\)", "\\1", cells))
  ) |>
  select(-cells) |>
  left_join(
    cells_tbl |>
      select(group, time, cell_att = estimate, cell_se = std.error, crit_value),
    by = c("group", "time")
  ) |>
  left_join(cell_support, by = c("group", "time")) |>
  group_by(event_time) |>
  mutate(agg_weight = pg[as.character(group)] / sum(pg[as.character(group)])) |>
  ungroup() |>
  arrange(event_time, group)
write_csv(contrib, file.path(out_dir, "dynamic_cell_contributions.csv"))
stopifnot(
  "poids dynamiques : ecart avec le package" = max(abs(
    dyn_rec$estimate - agg_dyn$att.egt
  )) <
    1e-6
)

## ---- C. soutien par cohorte --------------------------------------------------
matching_variables <- c(
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)
smd_att <- function(x, tr) {
  xt <- x[tr == 1]
  xc <- x[tr == 0]
  if (length(xt) < 2 || length(xc) < 2) {
    return(NA_real_)
  }
  s2t <- var(xt)
  if (!is.finite(s2t) || s2t == 0) {
    return(NA_real_)
  }
  abs((mean(xt) - mean(xc)) / sqrt(s2t))
}
cohort_wave <- bind_rows(lapply(
  sort(unique(did_dat$g[did_dat$g > 0])),
  function(co) {
    bind_rows(lapply(survey_years, function(w) {
      dt <- did_dat[
        did_dat$g == co & did_dat$DHSYEAR == w & did_dat$GROUP == "Treatment",
      ]
      dc <- did_dat[did_dat$DHSYEAR == w & did_dat$GROUP == "Control", ]
      tr <- c(rep(1, nrow(dt)), rep(0, nrow(dc)))
      dd <- rbind(dt[, matching_variables], dc[, matching_variables])
      smds <- vapply(
        matching_variables,
        function(nm) smd_att(dd[[nm]], tr),
        numeric(1)
      )
      tibble(
        cohort = co,
        DHSYEAR = w,
        n_treated = nrow(dt),
        n_control = nrow(dc),
        n_pas = n_distinct(dt$WDPAID),
        max_smd = if (all(is.na(smds))) NA_real_ else max(smds, na.rm = TRUE)
      )
    }))
  }
))

coh_summary <- bind_rows(lapply(c(2011, 2016, 2021), function(co) {
  dco <- did_dat[did_dat$g == co & did_dat$GROUP == "Treatment", ]
  cw <- cohort_wave[cohort_wave$cohort == co, ]
  cells_co <- cells_tbl[cells_tbl$group == co, ]
  data.frame(
    cohort = co,
    n_pas = n_distinct(dco$WDPAID),
    waves_with_treated = paste(sort(unique(dco$DHSYEAR)), collapse = ";"),
    onset_wave_represented = co %in% unique(dco$DHSYEAR),
    max_smd = max(cw$max_smd, na.rm = TRUE),
    median_smd = median(cw$max_smd, na.rm = TRUE),
    n_estimable_pre_cells = sum(cells_co$time < co & !is.na(cells_co$estimate)),
    n_estimable_post_cells = sum(
      cells_co$time >= co & !is.na(cells_co$estimate)
    )
  )
}))
write_csv(coh_summary, file.path(out_dir, "cohort_support_summary.csv"))

# sequence 2011 extraite du modele fige (diagnostic post-hoc, pas estimateur primaire)
crit <- unname(attgt_out$c)
co2011 <- cells_tbl |>
  filter(group == 2011) |>
  transmute(
    cohort = 2011L,
    wave = time,
    event_time = time - 2011,
    att = estimate,
    se = std.error,
    band_lower = att - crit * se,
    band_upper = att + crit * se,
    label = "post-hoc support diagnostic — not primary estimate"
  )
write_csv(co2011, file.path(out_dir, "cohort2011_diagnostic.csv"))

small <- cells_tbl |>
  filter(group %in% c(2016, 2021)) |>
  mutate(
    event_time = time - group,
    estimable = !is.na(estimate),
    band_lower = estimate - crit_value * std.error,
    band_upper = estimate + crit_value * std.error
  ) |>
  left_join(cell_support, by = c("group", "time")) |>
  left_join(
    cohort_wave |> select(cohort, DHSYEAR, cohort_wave_max_smd = max_smd),
    by = c("group" = "cohort", "time" = "DHSYEAR")
  ) |>
  transmute(
    cohort = group,
    wave = time,
    event_time,
    estimable,
    att = estimate,
    se = std.error,
    band_lower,
    band_upper,
    treated_hh,
    treated_clusters,
    treated_pas,
    cohort_wave_max_smd
  )
write_csv(small, file.path(out_dir, "small_cohort_diagnostics.csv"))

cat("Audit termine. Sorties dans", out_dir, "\n")
