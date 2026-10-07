# =====================================================================
# estimate(07b): head-to-head — step 3: C&S custom historical-bin
# aggregation + pretrends
#
# Aggregation uses exactly the did::aggte(type="dynamic") machinery
# (group-share weights pg, wif weight-estimation correction, clustered
# multiplier bootstrap via getSE/mboot), applied to frozen bins.
# Validation gate: manual native dynamic aggregation must reproduce
# aggte(type="dynamic") estimates and influence functions exactly
# before any binning is trusted (prompt section 10).
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(did)
})

out_dir <- "output/review_v2/staggered_head_to_head"
res <- readRDS(file.path(out_dir, "cs_results.rds"))
stacked <- readRDS(file.path(out_dir, "stacked_dat.rds"))

# frozen two-year event-time bins (prompt section 6)
bin_breaks <- c(-Inf, -9, -7, -5, -3, -1, 1, 3, 5, 7, 9, Inf)
bin_labels <- c(
  "<=-9",
  "-8:-7",
  "-6:-5",
  "-4:-3",
  "-2:-1",
  "0:1",
  "2:3",
  "4:5",
  "6:7",
  "8:9",
  ">=10"
)
bin_of <- function(e) {
  cut(e, breaks = bin_breaks, labels = bin_labels, right = TRUE)
}

BOOT_SEED <- 8607 # frozen convention; reset before each bootstrap so
# every SE is reproducible independently of call order

cs_bins_one <- function(h, outcome) {
  obj <- res[[paste0("attgt_", h)]]
  agg_dyn <- res[[paste0("agg_dyn_", h)]]
  dp <- obj$DIDparams
  est <- !is.na(obj$att)
  group <- obj$group[est]
  t_ <- obj$t[est]
  att <- obj$att[est]
  inffunc1 <- obj$inffunc[, est, drop = FALSE]
  n <- nrow(inffunc1)
  stopifnot(n == nrow(dp$data))
  G <- dp$data$g_eff
  w.ind <- dp$data$.w

  # full support frame: identical complete-case filter as the estimator
  dd_full <- stacked |>
    filter(if_all(
      all_of(c(outcome, controls_pap, "w_all", "g_eff", "DHSYEAR")),
      complete.cases
    )) |>
    mutate(cluster_uid_num = as.integer(cluster_uid)) |>
    filter(if_all("cluster_uid_num", complete.cases))
  stopifnot(nrow(dd_full) == n)

  # group shares exactly as compute.aggte: mean over rows of .w * 1{G==g}
  glist <- sort(unique(group))
  pg <- sapply(glist, function(g) mean(w.ind * (G == g)))
  pg_cell <- pg[match(group, glist)]

  # ---- validation gate: manual native dynamic aggregation ----------
  # Re-run aggte(dynamic) fresh under the frozen seed, then replay the
  # exact same per-event-time mboot call sequence manually: estimates
  # and influence functions must match exactly, SEs to machine precision.
  set.seed(BOOT_SEED)
  agg_dyn <- aggte(obj, type = "dynamic", na.rm = TRUE)
  eseq <- sort(unique(t_ - group))
  dyn_att <- numeric(length(eseq))
  dyn_if <- matrix(NA_real_, n, length(eseq))
  for (j in seq_along(eseq)) {
    whiche <- which(t_ - group == eseq[j])
    pge <- pg_cell[whiche] / sum(pg_cell[whiche])
    wif.e <- did:::wif(whiche, pg_cell, w.ind, G, group)
    dyn_if[, j] <- as.numeric(did:::get_agg_inf_func(
      att,
      inffunc1,
      whiche,
      pge,
      wif.e
    ))
    dyn_att[j] <- sum(att[whiche] * pge)
  }
  native_if <- agg_dyn$inf.function$dynamic.inf.func.e
  val_att <- max(abs(dyn_att - agg_dyn$att.egt[match(eseq, agg_dyn$egt)]))
  val_if <- max(abs(dyn_if - native_if))
  set.seed(BOOT_SEED)
  se_man <- sapply(seq_along(eseq), function(j) did:::getSE(dyn_if[, j], dp))
  val_se <- max(
    abs(se_man - agg_dyn$se.egt[match(eseq, agg_dyn$egt)]) /
      agg_dyn$se.egt[match(eseq, agg_dyn$egt)]
  )
  cat(sprintf(
    "[%s] validation: max|datt| = %.3e, max|dif| = %.3e, max rel dSE = %.4f\n",
    h,
    val_att,
    val_if,
    val_se
  ))
  stopifnot(
    "STOP: manual dynamic aggregation != aggte(dynamic)" = val_att < 1e-8 &
      val_if < 1e-6 &
      val_se < 1e-8
  )

  # ---- frozen-bin aggregation --------------------------------------
  ev <- t_ - group
  bins <- bin_of(ev)
  bins_nonempty <- bin_labels[sapply(bin_labels, function(b) {
    any(bins == b, na.rm = TRUE)
  })]

  bin_if <- matrix(
    NA_real_,
    n,
    length(bins_nonempty),
    dimnames = list(NULL, bins_nonempty)
  )
  bin_rows <- lapply(bins_nonempty, function(b) {
    k <- which(bins == b)
    pge <- pg_cell[k] / sum(pg_cell[k])
    wif_b <- did:::wif(k, pg_cell, w.ind, G, group)
    bin_if[, b] <<- as.numeric(did:::get_agg_inf_func(
      att,
      inffunc1,
      k,
      pge,
      wif_b
    ))
    list(k = k, estimate = sum(att[k] * pge))
  })
  set.seed(BOOT_SEED)
  bin_se <- sapply(bins_nonempty, function(b) did:::getSE(bin_if[, b], dp))
  bin_est <- sapply(bin_rows, `[[`, "estimate")

  # simultaneous band across displayed bins
  set.seed(BOOT_SEED)
  mb <- did:::mboot(bin_if, dp)
  cv_sim <- mb$crit.val

  # support metadata per bin (from the full sample frame; dp$data is
  # pruned by did and lacks WDPAID)
  dd_full$ev_bin <- bin_of(dd_full$DHSYEAR - dd_full$g_eff)
  support <- bind_rows(lapply(bins_nonempty, function(b) {
    k <- which(bins == b)
    post <- ev[k] >= 0
    if (all(post)) {
      rows <- dd_full[
        dd_full$ev_bin == b &
          dd_full$g_eff > 0 &
          dd_full$DHSYEAR >= dd_full$g_eff,
      ]
      tibble(
        bin = b,
        n_cells = length(k),
        cohorts = paste(sort(unique(group[k])), collapse = ";"),
        pas_represented = n_distinct(rows$WDPAID),
        treated_hh = nrow(rows),
        treated_mass = sum(rows$w_all),
        placebo_hh = 0L,
        placebo_mass = 0
      )
    } else {
      rows <- dd_full[
        dd_full$ev_bin == b &
          dd_full$g_eff > 0 &
          dd_full$DHSYEAR < dd_full$g_eff,
      ]
      tibble(
        bin = b,
        n_cells = length(k),
        cohorts = paste(sort(unique(group[k])), collapse = ";"),
        pas_represented = n_distinct(rows$WDPAID),
        treated_hh = 0L,
        treated_mass = 0,
        placebo_hh = nrow(rows),
        placebo_mass = sum(rows$w_all)
      )
    }
  }))

  export <- tibble(
    bin = bins_nonempty,
    estimate = unname(bin_est),
    std.error = unname(bin_se),
    conf_low = unname(bin_est - qnorm(0.975) * bin_se),
    conf_high = unname(bin_est + qnorm(0.975) * bin_se),
    crit_simultaneous = unname(cv_sim),
    band_lower = unname(bin_est - cv_sim * bin_se),
    band_upper = unname(bin_est + cv_sim * bin_se)
  ) |>
    left_join(support, by = "bin")

  # ---- pretrend joint test (cluster-robust, from the same IFs) -----
  pre_bins <- intersect(bin_labels[1:5], bins_nonempty)
  pre_b <- bin_est[match(pre_bins, bins_nonempty)]
  pre_if <- bin_if[, pre_bins, drop = FALSE]
  u <- rowsum(pre_if, dd$cluster_uid_num)
  V_cl <- crossprod(u) / n^2
  q <- length(pre_bins)
  W <- drop(t(pre_b) %*% solve(V_cl) %*% pre_b)
  p_W <- 1 - pchisq(W, q)
  wald <- tibble(
    outcome_h = h,
    test = "C&S binned pre bins = 0 (cluster-robust IF)",
    W = W,
    df = q,
    p_value = p_W,
    native_Wpval_diagnostic = as.numeric(obj$Wpval),
    pre_bins = paste(pre_bins, collapse = ";")
  )

  list(
    export = export,
    wald = wald,
    diagnostics = list(
      validation = tibble(
        h,
        val_att = val_att,
        val_if = val_if,
        val_se = val_se
      ),
      cells = tibble(
        group,
        t = t_,
        event_time = ev,
        bin = as.character(bins),
        att = att,
        pg = pg_cell
      ),
      bin_if = bin_if,
      V_pre_cluster = V_cl,
      mboot_bres = mb$bres
    )
  )
}

outcomes <- c(h1 = "wealth_centile_rural_weighted", h2 = "zscore_wealth")
b1 <- cs_bins_one("h1", outcomes["h1"])
b2 <- cs_bins_one("h2", outcomes["h2"])

write_csv(b1$export, file.path(out_dir, "cs_h1_binned_eventstudy.csv"))
write_csv(b2$export, file.path(out_dir, "cs_h2_binned_eventstudy.csv"))
write_csv(
  bind_rows(b1$wald, b2$wald),
  file.path(out_dir, "cs_pretrend_wald.csv")
)
saveRDS(
  list(h1 = b1$diagnostics, h2 = b2$diagnostics),
  file.path(out_dir, "cs_bin_diagnostics.rds")
)

cat("\n=== H1 binned event study ===\n")
print(b1$export, n = Inf, width = Inf)
cat("\n=== H2 binned event study ===\n")
print(b2$export, n = Inf, width = Inf)
cat("\n=== pretrend Wald ===\n")
print(bind_rows(b1$wald, b2$wald))
