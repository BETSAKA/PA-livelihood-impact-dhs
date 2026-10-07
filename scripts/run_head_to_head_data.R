# =====================================================================
# estimate(07b): head-to-head C&S vs BJS — step 1: common stacked data
#
# Rebuilds the frozen stacked repeated cross-section WITH outcomes,
# joins the audited GADM ADM3 crosswalk, constructs annual
# exact-date-consistent cohorts g_eff, and asserts consistency with
# frozen treated_now. Frozen choices are not modified.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

out_dir <- "output/review_v2/staggered_head_to_head"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

survey_years <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)

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
outcomes <- c("wealth_centile_rural_weighted", "zscore_wealth")

# 1. Stack frozen matched waves (07b construction, outcomes included)
stacked <- map_dfr(survey_years, function(y) {
  d <- read_rds(glue::glue("data/derived/data_matched_{y}.rds"))
  d |>
    rename(spei_wc_n_1 = !!glue::glue("spei_wc_{y-1}")) |>
    mutate(
      hv219 = zap_labels(hv219),
      hv220 = zap_labels(hv220),
      DHSYEAR = y
    )
}) |>
  filter(GROUP %in% c("Treatment", "Control")) |>
  st_drop_geometry() |>
  mutate(
    treatment_date = as.Date(treatment_date),
    hv219_femme = as.integer(hv219 == 2),
    hv220 = as.numeric(hv220),
    w_svy = hv005 / 1e6,
    w_all = w_svy * weights,
    cluster_uid = interaction(DHSYEAR, hv001, drop = TRUE),
    row_id = row_number()
  )

# 2. Audited GADM crosswalk (cluster key DHSYEAR x hv001)
crosswalk <- read_csv(
  "output/review_v2/bjs_feasibility/cluster_admin_crosswalk.csv",
  show_col_types = FALSE
)
stacked <- stacked |>
  left_join(
    crosswalk |>
      select(
        DHSYEAR,
        hv001,
        GID_1,
        NAME_1,
        GID_2,
        NAME_2,
        GID_3,
        NAME_3,
        region_id,
        district_id,
        border_5km,
        assigned_by_nearest,
        dist_border_km
      ),
    by = c("DHSYEAR", "hv001")
  )
stopifnot("crosswalk join incomplete" = !any(is.na(stacked$district_id)))

# 3. Annual exact-date-consistent cohorts (frozen June-1 rule)
stacked <- stacked |>
  mutate(
    td_year = as.integer(format(treatment_date, "%Y")),
    june1_td_year = as.Date(ISOdate(td_year, 6, 1)),
    g_eff = case_when(
      GROUP != "Treatment" ~ 0L,
      treatment_date <= june1_td_year ~ td_year,
      TRUE ~ td_year + 1L
    ),
    treated_now_from_g = as.integer(GROUP == "Treatment" & DHSYEAR >= g_eff),
    g_bjs = if_else(GROUP == "Treatment", g_eff, 0L),
    rel_year_eff = DHSYEAR - g_eff
  )

# Mandatory STOP assertion
bad <- stacked |>
  filter(GROUP == "Treatment", treated_now_from_g != treated_now)
stopifnot("STOP: g_eff inconsistent with frozen treated_now" = nrow(bad) == 0)

# 4. Cohort and support report
cohort_report <- stacked |>
  filter(GROUP == "Treatment", !is.na(g_eff), g_eff > 0) |>
  distinct(WDPAID, g_eff, treatment_date) |>
  count(g_eff, name = "n_PA")
print(cohort_report)
write_csv(cohort_report, file.path(out_dir, "annual_cohorts_pas.csv"))

# Zero-weight rows (inherited from frozen matching/covariate pipeline)
zw <- stacked |>
  group_by(DHSYEAR) |>
  summarise(
    rows = n(),
    zero_weight_rows = sum(w_all == 0),
    zero_weight_treated_now = sum(w_all == 0 & treated_now == 1),
    .groups = "drop"
  )
print(as.data.frame(zw))
write_csv(zw, file.path(out_dir, "zero_weight_rows_by_year.csv"))

# Persist for downstream estimator scripts
saveRDS(stacked, file.path(out_dir, "stacked_dat.rds"))
cat(
  "\nStacked rows:",
  nrow(stacked),
  "| clusters:",
  n_distinct(stacked$cluster_uid),
  "| districts:",
  n_distinct(stacked$district_id),
  "\n"
)
