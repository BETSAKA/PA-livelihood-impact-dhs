# =====================================================================
# estimate(07b): head-to-head — step 4: BJS imputation (district FE)
#
# Primary BJS: didimputation 0.3.0, district (ADM3) FE + survey-year FE
# + PAP covariates, w_all weights, district-clustered inference.
# Region FE is robustness only. Post bins use the audited custom wtr
# indicators; pre bins aggregate exact leads from the package's native
# pretrend regression mechanism with district-clustered covariance.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(didimputation)
})

out_dir <- "output/review_v2/staggered_head_to_head"
stacked <- readRDS(file.path(out_dir, "stacked_dat.rds"))

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
outcomes <- c(h1 = "wealth_centile_rural_weighted", h2 = "zscore_wealth")
fs_base <- paste(controls_pap, collapse = " + ")

bin_labels_post <- c("0:1", "2:3", "4:5", "6:7", "8:9", ">=10")
wtr_cols <- c("w_0_1", "w_2_3", "w_4_5", "w_6_7", "w_8_9", "w_ge10")

# common covariate-complete sample (audit dat_cc construction)
dat_cc <- stacked |>
  filter(if_all(all_of(c(controls_pap, "w_all")), complete.cases)) |>
  mutate(
    rel_year_eff = DHSYEAR - g_eff,
    w_0_1 = as.numeric(g_bjs > 0 & between(rel_year_eff, 0, 1)),
    w_2_3 = as.numeric(g_bjs > 0 & between(rel_year_eff, 2, 3)),
    w_4_5 = as.numeric(g_bjs > 0 & between(rel_year_eff, 4, 5)),
    w_6_7 = as.numeric(g_bjs > 0 & between(rel_year_eff, 6, 7)),
    w_8_9 = as.numeric(g_bjs > 0 & between(rel_year_eff, 8, 9)),
    w_ge10 = as.numeric(g_bjs > 0 & rel_year_eff >= 10)
  )

bin_of_pre <- function(e) {
  cut(
    e,
    breaks = c(-Inf, -9, -7, -5, -3, -1),
    labels = c("<=-9", "-8:-7", "-6:-5", "-4:-3", "-2:-1"),
    right = TRUE
  )
}

# ---------------------------------------------------------------------
# first-stage audit with the real outcome (silent-exclusion check)
# ---------------------------------------------------------------------
audit_first_stage_real <- function(dat, fe_col, outcome) {
  fit <- feols(
    as.formula(paste(outcome, "~", fs_base, "|", fe_col, "+ DHSYEAR")),
    data = dat[dat$g_bjs == 0, ],
    weights = ~w_all,
    warn = TRUE,
    notes = FALSE
  )
  pr <- predict(fit, newdata = dat)
  tr <- dat$g_bjs > 0 & dat$DHSYEAR >= dat$g_bjs
  tibble(
    rows_input = nrow(dat),
    treated_rows_targeted = sum(tr),
    treated_rows_imputable = sum(tr & !is.na(pr)),
    treated_rows_unimputable = sum(tr & is.na(pr)),
    treated_positive_weight = sum(tr & dat$w_all > 0),
    treated_zero_weight = sum(tr & dat$w_all == 0),
    zero_weight_rows_total = sum(dat$w_all == 0),
    singleton_fe_levels = sum(table(dat[[fe_col]][dat$g_bjs == 0]) == 1),
    clustering_units = n_distinct(dat[[fe_col]])
  )
}

# ---------------------------------------------------------------------
# pre (lead) regression = didimputation's native pretrend mechanism,
# replicated on the untreated sample with district-clustered covariance
# ---------------------------------------------------------------------
pre_reg <- function(dat, fe_col, cluster_col, outcome) {
  unt <- dat |>
    mutate(
      zz000treat = as.numeric(DHSYEAR >= g_bjs & g_bjs > 0),
      zz000event_time = ifelse(g_bjs == 0, -Inf, as.numeric(DHSYEAR - g_bjs))
    ) |>
    filter(zz000treat == 0)
  leads <- sort(unique(unt$zz000event_time[
    is.finite(unt$zz000event_time) & unt$zz000event_time < 0
  ]))
  fit <- feols(
    as.formula(paste0(
      outcome,
      " ~ i(zz000event_time, keep = c(",
      paste(leads, collapse = ", "),
      ")) + ",
      fs_base,
      " | ",
      fe_col,
      " + DHSYEAR"
    )),
    data = unt,
    weights = ~w_all,
    warn = FALSE,
    notes = FALSE
  )
  V <- vcov(fit, cluster = ~district_id)
  # feols silently drops collinear lead dummies; keep estimable ones only
  k_cols <- paste0("zz000event_time::", leads)
  est_leads <- leads[k_cols %in% rownames(V)]
  dropped <- leads[!k_cols %in% rownames(V)]
  k_est <- paste0("zz000event_time::", est_leads)
  beta <- fit$coefficient[k_est]
  Vl <- V[k_est, k_est]
  # support mass of each lead: w_all mass of untreated rows at that lead
  mass <- sapply(est_leads, function(k) {
    sum(unt$w_all[unt$zz000event_time == k])
  })
  list(
    leads = est_leads,
    beta = beta,
    V = Vl,
    mass = mass,
    se = sqrt(diag(Vl)),
    leads_dropped_collinear = dropped,
    n_unt = nrow(unt)
  )
}

aggregate_pre_bins <- function(pr) {
  bins_pre <- c("<=-9", "-8:-7", "-6:-5", "-4:-3", "-2:-1")
  A <- matrix(
    0,
    length(bins_pre),
    length(pr$leads),
    dimnames = list(bins_pre, as.character(pr$leads))
  )
  for (b in bins_pre) {
    ks <- which(bin_of_pre(pr$leads) == b)
    if (length(ks)) A[b, ks] <- pr$mass[ks] / sum(pr$mass[ks])
  }
  keep <- rowSums(A) > 0
  A <- A[keep, , drop = FALSE]
  bins_kept <- rownames(A)
  bb <- as.numeric(A %*% pr$beta)
  Vb <- A %*% pr$V %*% t(A)
  se_b <- sqrt(diag(Vb))
  # joint Wald test of all displayed pre bins = 0
  W <- drop(t(bb) %*% solve(Vb) %*% bb)
  q <- qr(Vb)$rank
  tbl <- tibble(
    bin = bins_kept,
    estimate = bb,
    std.error = se_b,
    conf_low = bb - qnorm(0.975) * se_b,
    conf_high = bb + qnorm(0.975) * se_b
  ) |>
    left_join(
      bind_rows(lapply(bins_kept, function(b) {
        ks <- which(bin_of_pre(pr$leads) == b)
        tibble(bin = b, n_leads = length(ks), lead_mass = sum(pr$mass[ks]))
      })),
      by = "bin"
    )
  list(tbl = tbl, wald = tibble(W = W, df = q, p_value = 1 - pchisq(W, q)))
}

# ---------------------------------------------------------------------
# per-outcome BJS runs
# ---------------------------------------------------------------------
bjs_one <- function(h) {
  outcome <- outcomes[[h]]
  dat <- dat_cc |> filter(if_all(outcome, complete.cases))
  a_d <- audit_first_stage_real(dat, "district_id", outcome)

  # overall ATT, district FE
  res_overall <- did_imputation(
    data = dat,
    yname = outcome,
    gname = "g_bjs",
    tname = "DHSYEAR",
    idname = "row_id",
    first_stage = as.formula(paste("~", fs_base, "| district_id + DHSYEAR")),
    wname = "w_all",
    cluster_var = "district_id"
  )

  # post bins via audited custom wtr
  res_bins <- did_imputation(
    data = dat,
    yname = outcome,
    gname = "g_bjs",
    tname = "DHSYEAR",
    idname = "row_id",
    first_stage = as.formula(paste("~", fs_base, "| district_id + DHSYEAR")),
    wname = "w_all",
    wtr = wtr_cols,
    cluster_var = "district_id"
  )

  # pre leads: package-native mechanism comparison + manual clustered
  pr <- pre_reg(dat, "district_id", "district_id", outcome)
  res_pkg_pretrend <- did_imputation(
    data = dat,
    yname = outcome,
    gname = "g_bjs",
    tname = "DHSYEAR",
    idname = "row_id",
    first_stage = as.formula(paste("~", fs_base, "| district_id + DHSYEAR")),
    wname = "w_all",
    cluster_var = "district_id",
    pretrends = pr$leads
  )
  pkg_leads <- res_pkg_pretrend |>
    filter(term %in% as.character(pr$leads)) |>
    transmute(
      lead = as.numeric(term),
      pkg_estimate = estimate,
      pkg_std_error = std.error
    )
  lead_check <- pkg_leads |>
    left_join(tibble(lead = pr$leads, man_estimate = pr$beta), by = "lead") |>
    mutate(diff = abs(pkg_estimate - man_estimate))

  pre <- aggregate_pre_bins(pr)

  # post-bin support
  support <- bind_rows(lapply(seq_along(wtr_cols), function(i) {
    sub <- dat |> filter(.data[[wtr_cols[i]]] == 1)
    tibble(
      bin = bin_labels_post[i],
      treated_hh = nrow(sub),
      treated_hh_posweight = sum(sub$w_all > 0),
      treated_clusters = n_distinct(sub$cluster_uid),
      treated_pas = n_distinct(sub$WDPAID),
      treated_districts = n_distinct(sub$district_id),
      cohorts = paste(sort(unique(sub$g_eff)), collapse = ";")
    )
  }))
  post_bins <- res_bins |>
    transmute(
      bin = bin_labels_post[match(term, wtr_cols)],
      estimate,
      std.error
    ) |>
    mutate(
      conf_low = estimate - qnorm(0.975) * std.error,
      conf_high = estimate + qnorm(0.975) * std.error
    ) |>
    left_join(support, by = "bin")

  # region-FE robustness
  res_overall_r <- did_imputation(
    data = dat,
    yname = outcome,
    gname = "g_bjs",
    tname = "DHSYEAR",
    idname = "row_id",
    first_stage = as.formula(paste("~", fs_base, "| region_id + DHSYEAR")),
    wname = "w_all",
    cluster_var = "region_id"
  )
  res_bins_r <- did_imputation(
    data = dat,
    yname = outcome,
    gname = "g_bjs",
    tname = "DHSYEAR",
    idname = "row_id",
    first_stage = as.formula(paste("~", fs_base, "| region_id + DHSYEAR")),
    wname = "w_all",
    wtr = wtr_cols,
    cluster_var = "region_id"
  )

  list(
    h = h,
    outcome = outcome,
    audit = a_d,
    overall = res_overall,
    post_bins = post_bins,
    pre_bins = pre$tbl,
    pre_wald = pre$wald,
    leads = tibble(
      lead = pr$leads,
      estimate = pr$beta,
      std.error = pr$se,
      support_mass = pr$mass
    ),
    lead_check = lead_check,
    region_overall = res_overall_r,
    region_bins = res_bins_r
  )
}

set.seed(8607) # didimputation bootstrap draws, frozen convention
bjs_h1 <- bjs_one("h1")
bjs_h2 <- bjs_one("h2")

# ---------------------------------------------------------------------
# exports
# ---------------------------------------------------------------------
fmt_overall <- function(r, res, audit, fe_label) {
  est <- res$estimate[res$term == "treat"]
  se <- res$std.error[res$term == "treat"]
  tibble(
    estimand = paste("BJS overall ATT,", fe_label),
    estimate = est,
    std.error = se,
    conf_low = est - qnorm(0.975) * se,
    conf_high = est + qnorm(0.975) * se,
    p_value = 2 * pnorm(-abs(est / se)),
    n_targeted = audit$treated_rows_targeted,
    n_positive_weight_targeted = audit$treated_positive_weight,
    n_imputable = audit$treated_rows_imputable,
    pas = NA_integer_,
    districts = NA_integer_
  )
}

for (bb in list(bjs_h1, bjs_h2)) {
  h <- bb$h
  write_csv(
    bb$post_bins,
    file.path(out_dir, paste0("bjs_", h, "_post_bins.csv"))
  )
  write_csv(bb$pre_bins, file.path(out_dir, paste0("bjs_", h, "_pre_bins.csv")))
  # region-FE robustness: overall + post bins, compact
  ro <- bb$region_overall |>
    transmute(
      estimand = "BJS overall ATT, region FE (robustness)",
      estimate,
      std.error,
      conf_low = estimate - qnorm(0.975) * std.error,
      conf_high = estimate + qnorm(0.975) * std.error
    )
  rb <- bb$region_bins |>
    transmute(
      bin = bin_labels_post[match(term, wtr_cols)],
      estimate,
      std.error,
      conf_low = estimate - qnorm(0.975) * std.error,
      conf_high = estimate + qnorm(0.975) * std.error
    )
  write_csv(
    bind_rows(ro, rb),
    file.path(out_dir, paste0("bjs_region_robustness_", h, ".csv"))
  )
}

saveRDS(list(h1 = bjs_h1, h2 = bjs_h2), file.path(out_dir, "bjs_results.rds"))

cat("\n=== BJS H1 overall (district FE) ===\n")
print(bjs_h1$overall)
cat("\n=== BJS H1 audit ===\n")
print(bjs_h1$audit)
cat("\n=== BJS H1 post bins ===\n")
print(bjs_h1$post_bins)
cat("\n=== BJS H1 pre bins ===\n")
print(bjs_h1$pre_bins)
cat("\n=== BJS H1 pre Wald ===\n")
print(bjs_h1$pre_wald)
cat("\n=== BJS H1 lead check (pkg vs manual, max diff) ===\n")
print(max(bjs_h1$lead_check$diff, na.rm = TRUE))
cat("\n=== BJS H2 overall (district FE) ===\n")
print(bjs_h2$overall)
cat("\n=== BJS H2 audit ===\n")
print(bjs_h2$audit)
cat("\n=== BJS H2 post bins ===\n")
print(bjs_h2$post_bins)
cat("\n=== BJS H2 pre bins ===\n")
print(bjs_h2$pre_bins)
cat("\n=== BJS H2 pre Wald ===\n")
print(bjs_h2$pre_wald)
cat("\n=== BJS H2 lead check (max diff) ===\n")
print(max(bjs_h2$lead_check$diff, na.rm = TRUE))
cat("\n=== BJS region robustness ===\n")
print(bjs_h1$region_overall)
print(bjs_h2$region_overall)
