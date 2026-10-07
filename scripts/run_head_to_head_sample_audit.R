# =====================================================================
# estimate(07b): head-to-head — step 5: estimator sample audit
# =====================================================================

suppressPackageStartupMessages(library(tidyverse))

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

cc_cov <- complete.cases(stacked[, controls_pap])

loss_breakdown <- stacked |>
  mutate(
    cov_missing = !cc_cov,
    outcome_missing_h1 = is.na(wealth_centile_rural_weighted),
    outcome_missing_h2 = is.na(zscore_wealth),
    zero_weight = w_all == 0
  ) |>
  group_by(DHSYEAR, GROUP) |>
  summarise(
    rows = n(),
    lost_covariate_missingness = sum(cov_missing & !zero_weight),
    lost_zero_weight_and_cov_missing = sum(cov_missing & zero_weight),
    zero_weight_rows = sum(zero_weight),
    zero_weight_rows_in_cov_complete = sum(zero_weight & !cov_missing),
    zero_weight_treated_now = sum(zero_weight & treated_now == 1),
    outcome_missing_h1 = sum(outcome_missing_h1 & !cov_missing),
    outcome_missing_h2 = sum(outcome_missing_h2 & !cov_missing),
    .groups = "drop"
  )
print(as.data.frame(loss_breakdown))

# estimator samples (both outcomes give the same complete-case sample here;
# verify rather than assume)
cs_sample <- function(outcome) {
  stacked |>
    filter(if_all(
      all_of(c(outcome, controls_pap, "w_all", "g_eff", "DHSYEAR")),
      complete.cases
    ))
}
s1 <- cs_sample(outcomes["h1"])
s2 <- cs_sample(outcomes["h2"])
bjs_sample <- stacked |>
  filter(if_all(all_of(c(controls_pap, "w_all")), complete.cases)) |>
  filter(if_all(outcomes[["h1"]], complete.cases))

summary_tbl <- tibble(
  quantity = c(
    "full stacked rows",
    "positive-weight rows",
    "complete cases on PAP covariates",
    paste("rows used by C&S,", outcomes["h1"]),
    paste("rows used by C&S,", outcomes["h2"]),
    paste("rows used by BJS,", outcomes["h1"]),
    paste("rows used by BJS,", outcomes["h2"]),
    "post-treated rows used (treated_now = 1)",
    "post-treated zero-weight rows in sample",
    "rows lost to covariate missingness",
    "rows lost to outcome missingness (H1)",
    "rows lost to outcome missingness (H2)"
  ),
  value = c(
    nrow(stacked),
    sum(stacked$w_all > 0),
    sum(cc_cov),
    nrow(s1),
    nrow(s2),
    nrow(bjs_sample),
    nrow(bjs_sample),
    sum(s1$treated_now == 1),
    sum(s1$w_all == 0 & s1$treated_now == 1),
    sum(!cc_cov),
    sum(is.na(stacked$wealth_centile_rural_weighted) & cc_cov),
    sum(is.na(stacked$zscore_wealth) & cc_cov)
  )
)
print(as.data.frame(summary_tbl))

# C&S and BJS sample identity check
identical_samples <- setequal(s1$row_id, bjs_sample$row_id)
cat("\nC&S and BJS use identical rows:", identical_samples, "\n")

# BJS unimputable treated rows: districts without untreated support?
bjs_res <- readRDS(file.path(out_dir, "bjs_results.rds"))
dat_bjs_cc <- stacked |>
  filter(if_all(all_of(c(controls_pap, "w_all")), complete.cases)) |>
  filter(if_all(outcomes[["h1"]], complete.cases))
unt_support <- dat_bjs_cc |>
  filter(g_bjs == 0) |>
  count(district_id, name = "untreated_rows")
unimp_check <- dat_bjs_cc |>
  filter(g_bjs > 0, DHSYEAR >= g_bjs) |>
  group_by(district_id) |>
  summarise(treated_rows = n(), .groups = "drop") |>
  left_join(unt_support, by = "district_id") |>
  mutate(untreated_rows = tidyr::replace_na(untreated_rows, 0)) |>
  filter(untreated_rows == 0)
cat(
  "\nTreated rows in districts with zero untreated support:",
  sum(unimp_check$treated_rows),
  "across",
  nrow(unimp_check),
  "districts\n"
)
print(as.data.frame(unimp_check))

write_csv(loss_breakdown, file.path(out_dir, "sample_losses_by_year_group.csv"))
write_csv(summary_tbl, file.path(out_dir, "estimator_sample_comparison.csv"))
