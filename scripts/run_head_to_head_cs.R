# =====================================================================
# estimate(07b): head-to-head — step 2: Callaway–Sant'Anna, annual g_eff
#
# Frozen setup identical to 07b except gname = annual g_eff:
#   panel = FALSE, control_group = nevertreated, est_method = ipw,
#   clustervars/idname = cluster_uid_num, bstrap = TRUE, cband = TRUE,
#   biters = 1000, weightsname = w_all, xformla = PAP covariates.
# No retuning after results (prompt section 22).
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(did)
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

xformla_pap <- as.formula(paste("~", paste(controls_pap, collapse = " + ")))

# frozen bootstrap seed convention (run_frozen_design_main.R: 8607)
set.seed(8607)

cs_fit <- function(outcome) {
  cs_dat <- stacked |>
    filter(if_all(
      all_of(c(outcome, controls_pap, "w_all", "g_eff", "DHSYEAR")),
      complete.cases
    )) |>
    mutate(cluster_uid_num = as.integer(cluster_uid)) |>
    filter(if_all("cluster_uid_num", complete.cases)) |>
    as.data.frame()
  message(
    "C&S sample for ",
    outcome,
    ": ",
    nrow(cs_dat),
    " rows, ",
    n_distinct(cs_dat$cluster_uid),
    " clusters"
  )
  att_gt(
    yname = outcome,
    tname = "DHSYEAR",
    idname = "cluster_uid_num",
    gname = "g_eff",
    xformla = xformla_pap,
    data = cs_dat,
    panel = FALSE,
    weightsname = "w_all",
    control_group = "nevertreated",
    clustervars = "cluster_uid_num",
    bstrap = TRUE,
    cband = TRUE,
    biters = 1000,
    est_method = "ipw"
  )
}

attgt_h1 <- cs_fit(outcomes["h1"])
attgt_h2 <- cs_fit(outcomes["h2"])

agg_simple_h1 <- aggte(attgt_h1, type = "simple", na.rm = TRUE)
agg_cal_h1 <- aggte(attgt_h1, type = "calendar", na.rm = TRUE)
agg_dyn_h1 <- aggte(attgt_h1, type = "dynamic", na.rm = TRUE)
agg_simple_h2 <- aggte(attgt_h2, type = "simple", na.rm = TRUE)
agg_cal_h2 <- aggte(attgt_h2, type = "calendar", na.rm = TRUE)
agg_dyn_h2 <- aggte(attgt_h2, type = "dynamic", na.rm = TRUE)

saveRDS(
  list(
    attgt_h1 = attgt_h1,
    attgt_h2 = attgt_h2,
    agg_simple_h1 = agg_simple_h1,
    agg_cal_h1 = agg_cal_h1,
    agg_dyn_h1 = agg_dyn_h1,
    agg_simple_h2 = agg_simple_h2,
    agg_cal_h2 = agg_cal_h2,
    agg_dyn_h2 = agg_dyn_h2
  ),
  file.path(out_dir, "cs_results.rds")
)

cat("\n=== H1 simple ===\n")
print(agg_simple_h1)
cat("\n=== H1 calendar ===\n")
print(agg_cal_h1)
cat("\n=== H2 simple ===\n")
print(agg_simple_h2)
cat("\n=== H2 calendar ===\n")
print(agg_cal_h2)
