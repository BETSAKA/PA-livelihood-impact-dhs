# =====================================================================
# audit(07b): test GADM district FE for BJS repeated cross-sections
#
# Outcome-blind feasibility audit. No real outcome is loaded or
# estimated anywhere. A deterministic fake outcome is used only to
# test package mechanics (didimputation 0.3.0).
#
# Sections:
#   0. Setup
#   1. Stack the frozen repeated cross-sections (design cols only)
#   2. g_eff reconstruction + treated_now assertion
#   3. GADM ADM3 download and hierarchy verification
#   4. Cluster-to-ADM3 spatial join + crosswalk export
#   5. Region-FE vs district-FE support audit
#   6. Required figures
#   7. Fake-outcome BJS mechanics (region and district FE)
#   8. Historical post-treatment bins via custom wtr
#   9. Pretrend/lead mechanism findings
#  10. Design-only region vs district comparison
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(sf)
  library(terra)
  library(geodata)
  library(fixest)
  library(didimputation)
})

out_dir <- "output/review_v2/bjs_feasibility"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("data/external/gadm", recursive = TRUE, showWarnings = FALSE)

theme_audit <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92")
  )

# ---------------------------------------------------------------------
# 1. Stack the frozen repeated cross-sections (outcomes excluded)
# ---------------------------------------------------------------------
survey_years <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)

design_cols <- c(
  "DHSYEAR",
  "hv001",
  "hv005",
  "URBAN_RURA",
  "DHSREGNA",
  "GROUP",
  "WDPAID",
  "treatment_date",
  "treatment_year",
  "treated_now",
  "weights",
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000",
  "hv219",
  "hv220"
)

dat <- map_dfr(survey_years, function(y) {
  d <- readRDS(glue::glue("data/derived/data_matched_{y}.rds"))
  stopifnot(all(design_cols %in% names(d)))
  sp <- rlang::as_name(glue::glue("spei_wc_{y-1}"))
  d |>
    select(all_of(design_cols), !!sp) |>
    rename(spei_wc_n_1 = !!sp) |>
    mutate(
      DHSYEAR = y,
      hv219_femme = as.integer(zap_labels(hv219) == 2L),
      w_svy = hv005 / 1e6,
      w_all = w_svy * weights
    )
}) |>
  mutate(
    row_id = row_number(),
    cluster_uid = interaction(DHSYEAR, hv001, drop = TRUE),
    treatment_date = as.Date(treatment_date)
  )
# Households are repeated-cross-section rows; DHS clusters are NOT
# longitudinal units. hv001 is never used as a persistent FE.

# ---------------------------------------------------------------------
# 2. g_eff (June-1 survey reference rule) + mandatory assertion
# ---------------------------------------------------------------------
dat <- dat |>
  mutate(
    td_year = as.integer(format(treatment_date, "%Y")),
    june1_td_year = as.Date(ISOdate(td_year, 6, 1)),
    g_eff = case_when(
      GROUP != "Treatment" ~ 0L,
      treatment_date <= june1_td_year ~ td_year,
      TRUE ~ td_year + 1L
    ),
    treated_bjs = as.integer(GROUP == "Treatment" & DHSYEAR >= g_eff)
  )

chk <- dat |> filter(GROUP == "Treatment")
stopifnot(
  "STOP: treated_bjs != treated_now for some treated observation" = all(
    chk$treated_bjs == chk$treated_now
  ),
  "STOP: control with treatment_date" = all(is.na(dat$treatment_date[
    dat$GROUP != "Treatment"
  ])),
  "STOP: control g_eff != 0" = all(dat$g_eff[dat$GROUP != "Treatment"] == 0L)
)
cat("g_eff assertions passed. Treated observations:", nrow(chk), "\n")

# ---------------------------------------------------------------------
# 3. GADM ADM3: hierarchy verification
# ---------------------------------------------------------------------
adm3 <- geodata::gadm(country = "MDG", level = 3, path = "data/external/gadm")
adm3_att <- as.data.frame(adm3)

admin_counts <- tibble(
  level = c("GID_1", "GID_2", "GID_3"),
  n = c(
    n_distinct(adm3_att$GID_1),
    n_distinct(adm3_att$GID_2),
    n_distinct(adm3_att$GID_3)
  )
)
cat("\nGADM hierarchy counts:\n")
print(admin_counts)
# GADM 4.1 dropped the TYPE_/ENGTYPE_ columns: they are absent (ADM1/ADM2)
# or all-NA (ADM3). ADM3 = districts is confirmed by the NAME_3 entities
# (e.g. Ambohidratrimo, Andramasina, Anjozorobe under Analamanga) and by
# the GADM documented hierarchy provinces > regions > districts.
for (v in intersect(
  c("TYPE_1", "ENGTYPE_1", "TYPE_2", "ENGTYPE_2", "TYPE_3", "ENGTYPE_3"),
  names(adm3_att)
)) {
  cat("\n---", v, "---\n")
  print(table(adm3_att[[v]], useNA = "ifany"))
}
stopifnot(n_distinct(adm3_att$GID_3) == nrow(adm3_att)) # one row per district

reg_ids <- adm3_att |> distinct(GID_2, NAME_2) # 22 pre-2021 regions
dis_ids <- adm3_att |> distinct(GID_3, NAME_3) # 110 district polygons

# ---------------------------------------------------------------------
# 4. Cluster coordinates + spatial join (once per wave x cluster)
# ---------------------------------------------------------------------
gps_paths <- c(
  "1997" = "data/raw/dhs/DHS_1997/MDGE32FL/MDGE32FL.shp",
  "2008" = "data/raw/dhs/DHS_2008/MDGE53FL/MDGE53FL.shp",
  "2011" = "data/raw/dhs/DHS_2011/MDGE61FL/MDGE61FL.shp",
  "2013" = "data/raw/dhs/DHS_2013/MDGE6AFL/MDGE6AFL.shp",
  "2016" = "data/raw/dhs/DHS_2016/MDGE71FL/MDGE71FL.shp",
  "2018" = "data/raw/mics/2018/GPS Datasets/MadagascarMICS2018GPS.shp",
  "2021" = "data/raw/dhs/DHS_2021/MDGE81FL/MDGE81FL.shp"
)

cluster_xy <- imap_dfr(gps_paths, function(p, yr) {
  g <- st_read(p, quiet = TRUE, options = "ENCODING=UTF-8") |>
    st_drop_geometry()
  if ("SVYYEARS" %in% names(g)) {
    tibble(
      DHSYEAR = as.integer(g$SVYYEARS),
      hv001 = as.integer(g$HH1),
      LATNUM = g$LATITUDE,
      LONGNUM = g$LONGITUDE
    )
  } else {
    tibble(
      DHSYEAR = as.integer(g$DHSYEAR),
      hv001 = as.integer(g$DHSCLUST),
      LATNUM = g$LATNUM,
      LONGNUM = g$LONGNUM
    )
  }
}) |>
  filter(!(LONGNUM == 0 & LATNUM == 0)) |>
  distinct()

matched_key <- dat |> distinct(DHSYEAR, hv001)
stopifnot(
  n_distinct(paste(dat$DHSYEAR, dat$hv001)) ==
    n_distinct(paste(cluster_xy$DHSYEAR, cluster_xy$hv001)) |
    TRUE
) # informational: GPS file is a superset
j0 <- matched_key |>
  left_join(cluster_xy, by = c("DHSYEAR", "hv001"))
stopifnot(
  "STOP: matched cluster without unique coordinates" = !any(is.na(j0$LATNUM)),
  "STOP: duplicated cluster coordinates" = !any(duplicated(j0[, c(
    "DHSYEAR",
    "hv001"
  )]))
)

pts <- st_as_sf(
  cluster_xy,
  coords = c("LONGNUM", "LATNUM"),
  crs = 4326,
  remove = FALSE
)
adm3_sf <- st_as_sf(adm3)
keep_cols <- c("GID_1", "NAME_1", "GID_2", "NAME_2", "GID_3", "NAME_3")
j <- st_join(pts, adm3_sf[, keep_cols], left = TRUE, join = st_within)

# Unmatched points: DHS coordinate displacement (up to 5 km) landing in
# boundary gaps. Verified all within 1 km of an ADM3 boundary; resolved
# by nearest-polygon assignment and flagged (not silently dropped).
unass <- is.na(j$GID_3)
j_utm <- j |> st_transform(32738)
adm3_utm <- adm3_sf |> st_transform(32738)
if (any(unass)) {
  nf <- st_nearest_feature(j_utm[unass, ], adm3_utm)
  d_near <- st_distance(j_utm[unass, ], adm3_utm[nf, ]) |> diag()
  stopifnot(all(as.numeric(d_near) / 1000 <= 1))
  j[unass, keep_cols] <- lapply(keep_cols, function(cn) adm3_sf[[cn]][nf])
}
j$assigned_by_nearest <- FALSE
j$assigned_by_nearest[unass] <- TRUE

# distance to nearest ADM3 border (km), for the border-sensitivity flag
adm3_bound <- st_boundary(adm3_sf) |> st_union() |> st_transform(32738)
j_utm$dist_border_km <- as.numeric(st_distance(j_utm, adm3_bound)) / 1000

crosswalk <- j |>
  st_drop_geometry() |>
  left_join(
    j_utm |> st_drop_geometry() |> select(DHSYEAR, hv001, dist_border_km),
    by = c("DHSYEAR", "hv001")
  ) |>
  mutate(
    in_matched_sample = paste(DHSYEAR, hv001) %in%
      paste(matched_key$DHSYEAR, matched_key$hv001),
    region_id = GID_2,
    district_id = GID_3,
    border_5km = dist_border_km <= 5
  ) |>
  arrange(DHSYEAR, hv001)

# Current GADM is used as a HARMONIZED PERSISTENT SPATIAL PARTITION,
# not as survey-year-specific historical administrative codes.
write_csv(crosswalk, file.path(out_dir, "cluster_admin_crosswalk.csv"))

cw_m <- crosswalk |> filter(in_matched_sample)
cat(
  "\nMatched clusters assigned:",
  sum(!is.na(cw_m$district_id)),
  "/",
  nrow(cw_m),
  "| by nearest:",
  sum(cw_m$assigned_by_nearest),
  "| border<=5km:",
  sum(cw_m$border_5km),
  "\n"
)

dat <- dat |>
  left_join(
    cw_m |>
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
        dist_border_km,
        border_5km,
        assigned_by_nearest
      ),
    by = c("DHSYEAR", "hv001")
  )
stopifnot(
  "STOP: matched row not assigned to ADM3" = !any(is.na(dat$district_id))
)

# ---------------------------------------------------------------------
# 5. Region-FE vs district-FE support audit
# ---------------------------------------------------------------------
support_tbl <- function(unit_var, ids, names_, level) {
  s <- dat |>
    group_by(unit = .data[[unit_var]]) |>
    summarise(
      treated_hh = sum(treated_bjs == 1),
      untreated_hh = sum(treated_bjs == 0),
      treated_clusters = n_distinct(cluster_uid[treated_bjs == 1]),
      untreated_clusters = n_distinct(cluster_uid[treated_bjs == 0]),
      treated_pas = n_distinct(WDPAID[treated_bjs == 1]),
      waves_represented = n_distinct(DHSYEAR),
      untreated_waves = n_distinct(DHSYEAR[treated_bjs == 0]),
      .groups = "drop"
    )
  tibble(unit = ids, unit_name = names_) |>
    left_join(select(s, -any_of("unit_name")), by = "unit") |>
    mutate(
      across(treated_hh:untreated_waves, ~ replace_na(.x, 0)),
      represented = unit %in% s$unit,
      fe_identified = untreated_hh > 0,
      level = level
    ) |>
    relocate(level, unit, unit_name, represented, fe_identified)
}

region_support_full <- support_tbl(
  "region_id",
  reg_ids$GID_2,
  reg_ids$NAME_2,
  "region"
)
district_support_full <- support_tbl(
  "district_id",
  dis_ids$GID_3,
  dis_ids$NAME_3,
  "district"
)

for (s in list(region_support_full, district_support_full)) {
  tr <- s |> filter(treated_hh > 0)
  cat("\n===", s$level[1], "FE support ===\n")
  cat(
    "GADM units:",
    nrow(s),
    "| represented:",
    sum(s$represented),
    "| with treated obs:",
    nrow(tr),
    "| fe_identified:",
    sum(tr$fe_identified),
    "\n"
  )
  cat(
    "Median untreated HH:",
    median(tr$untreated_hh),
    "| clusters:",
    median(tr$untreated_clusters),
    "| waves:",
    median(tr$untreated_waves),
    "\n"
  )
}
write_csv(region_support_full, file.path(out_dir, "region_fe_support.csv"))
write_csv(district_support_full, file.path(out_dir, "district_fe_support.csv"))

# unsupported treated units (HH/cluster/PA level)
unsup <- bind_rows(
  region_support_full |>
    filter(treated_hh > 0, !fe_identified) |>
    transmute(
      level,
      unsupported_hh = treated_hh,
      unsupported_clusters = treated_clusters,
      unsupported_pas = treated_pas
    ),
  district_support_full |>
    filter(treated_hh > 0, !fe_identified) |>
    transmute(
      level,
      unsupported_hh = treated_hh,
      unsupported_clusters = treated_clusters,
      unsupported_pas = treated_pas
    )
)
cat("\nUnsupported treated HH/clusters/PAs (both levels):\n")
print(unsup)

# ---------------------------------------------------------------------
# 6. Required figures
# ---------------------------------------------------------------------
p1 <- tibble(
  label = c("ADM1 (provinces)", "ADM2 (regions)", "ADM3 (districts)"),
  n = c(
    n_distinct(adm3_att$GID_1),
    n_distinct(adm3_att$GID_2),
    n_distinct(adm3_att$GID_3)
  )
) |>
  ggplot(aes(label, n)) +
  geom_col(fill = "grey35") +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  labs(
    x = NULL,
    y = "Number of GADM units (vintage 4.1)",
    title = "GADM 4.1 Madagascar administrative hierarchy",
    subtitle = "Pre-2021 vintage: 22 regions (Vatovavy-Fitovinany unsplit), 110 district polygons"
  )
ggsave(
  file.path(out_dir, "gadm_admin_counts.png"),
  p1,
  width = 7,
  height = 4.5,
  dpi = 300
)

wave_hm <- function(unit_var, unit_lab, treat) {
  dat |>
    mutate(unit = .data[[unit_var]]) |>
    filter(treated_bjs == ifelse(treat == "treated", 1L, 0L)) |>
    count(unit, DHSYEAR, name = "n_cl") |>
    complete(
      unit,
      DHSYEAR = sort(unique(dat$DHSYEAR)),
      fill = list(n_cl = 0)
    ) |>
    ggplot(aes(factor(DHSYEAR), reorder(unit, n_cl, FUN = max), fill = n_cl)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    geom_text(aes(label = ifelse(n_cl > 0, n_cl, "")), size = 2.4) +
    scale_fill_gradient(
      low = "#f7f4ef",
      high = "#21618c",
      guide = "none",
      transform = "sqrt"
    ) +
    labs(
      x = "Survey wave",
      y = unit_lab,
      title = paste0(
        stringr::str_to_title(treat),
        " DHS clusters per ",
        unit_lab,
        " and wave"
      ),
      subtitle = "Tile value = number of DHS clusters; blank = zero"
    ) +
    theme_audit
}
ggsave(
  file.path(out_dir, "district_wave_untreated_clusters.png"),
  wave_hm("district_id", "GADM ADM3 district (GID_3)", "untreated"),
  width = 7.5,
  height = 12,
  dpi = 300
)
ggsave(
  file.path(out_dir, "district_wave_treated_clusters.png"),
  wave_hm("district_id", "GADM ADM3 district (GID_3)", "treated"),
  width = 7.5,
  height = 8,
  dpi = 300
)
ggsave(
  file.path(out_dir, "region_wave_untreated_clusters.png"),
  wave_hm("region_id", "GADM ADM2 region (GID_2)", "untreated"),
  width = 7.5,
  height = 6,
  dpi = 300
)

# Row-level counts (per-unit sums double-count PAs whose treated
# households straddle administrative units)
comp_summ <- imap_dfr(
  list("Region FE (ADM2)" = "region_id", "District FE (ADM3)" = "district_id"),
  function(uc, lv) {
    tr <- dat |> filter(treated_bjs == 1)
    unt_units <- dat |>
      filter(treated_bjs == 0) |>
      distinct(unit = .data[[uc]]) |>
      pull(unit)
    supported <- tr[[uc]] %in% unt_units
    tibble(
      level = lv,
      `Treated HH total` = nrow(tr),
      `Treated clusters total` = n_distinct(tr$cluster_uid),
      `Treated PAs total` = n_distinct(tr$WDPAID),
      `Treated HH supported` = sum(supported),
      `Treated clusters supported` = n_distinct(tr$cluster_uid[supported]),
      `Treated PAs supported` = n_distinct(tr$WDPAID[supported])
    )
  }
)
print(comp_summ)
comp_long <- comp_summ |>
  pivot_longer(-level, names_to = "metric", values_to = "n") |>
  separate(metric, into = c("metric", "kind"), sep = " (?=[^ ]+$)") |>
  mutate(
    kind = ifelse(kind == "total", "total", "supported"),
    metric = factor(
      metric,
      levels = c("Treated HH", "Treated clusters", "Treated PAs")
    )
  )
p5 <- ggplot(comp_long, aes(metric, n, fill = kind)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(
    values = c(total = "grey65", supported = "#21618c"),
    labels = c(total = "total", supported = "with untreated support")
  ) +
  facet_wrap(~level, ncol = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, .18))) +
  labs(
    x = NULL,
    y = "Count",
    fill = NULL,
    title = "Treated households, clusters and PAs by FE level: untreated support coverage",
    subtitle = "Support = treated unit has at least one untreated observation (BJS first-stage sample)"
  )
ggsave(
  file.path(out_dir, "treated_fe_support_comparison.png"),
  p5,
  width = 8,
  height = 6,
  dpi = 300
)

dist_sup <- district_support_full |> filter(treated_hh > 0)
d_long <- dist_sup |>
  transmute(
    unit,
    `Untreated HH` = untreated_hh,
    `Untreated clusters` = untreated_clusters,
    `Untreated waves` = untreated_waves
  ) |>
  pivot_longer(-unit, names_to = "metric", values_to = "n")
p6 <- ggplot(d_long, aes(metric, n)) +
  geom_boxplot(fill = "grey80", width = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.6, size = 1.6) +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.05, .12))) +
  labs(
    x = NULL,
    y = "Count per treated district",
    title = "Untreated support among treated ADM3 districts",
    subtitle = "Untreated = BJS first-stage sample (never-treated + not-yet-treated)"
  ) +
  theme_audit
ggsave(
  file.path(out_dir, "district_untreated_support_distribution.png"),
  p6,
  width = 9,
  height = 4.2,
  dpi = 300
)
cat("\nFigures written.\n")

# ---------------------------------------------------------------------
# 7. Fake-outcome BJS mechanics (didimputation 0.3.0)
# ---------------------------------------------------------------------
xvars <- c(
  "spei_wc_n_1",
  "hv219_femme",
  "hv220",
  "treecover_area_2000",
  "slope_2000",
  "elevation_2000",
  "population_count_2000",
  "traveltime_2000_2000"
)
fs_base <- paste(xvars, collapse = " + ")

dat_bjs <- dat |>
  mutate(
    fake_y = sin(row_id / 17) + cos(row_id / 31), # deterministic fake only
    g_bjs = if_else(GROUP == "Treatment", g_eff, 0L)
  )
dat_cc <- dat_bjs |> filter(complete.cases(dat_bjs[, c(xvars, "w_all")]))

run_bjs <- function(fe_col, cluster_col) {
  did_imputation(
    data = dat_cc,
    yname = "fake_y",
    gname = "g_bjs",
    tname = "DHSYEAR",
    idname = "row_id",
    first_stage = as.formula(paste("~", fs_base, "|", fe_col, "+ DHSYEAR")),
    wname = "w_all",
    cluster_var = cluster_col
  )
}
res_region <- run_bjs("region_id", "region_id")
res_district <- run_bjs("district_id", "district_id")
cat("\nRegion FE fake run:\n")
print(res_region)
cat("\nDistrict FE fake run:\n")
print(res_district)

# independent first-stage replication to expose silent exclusions
audit_first_stage <- function(fe_col) {
  fit <- feols(
    as.formula(paste("fake_y ~", fs_base, "|", fe_col, "+ DHSYEAR")),
    data = dat_cc[dat_cc$treated_bjs == 0, ],
    weights = ~w_all,
    warn = TRUE,
    notes = TRUE
  )
  pr <- predict(fit, newdata = dat_cc)
  tibble(
    rows_input = nrow(dat_cc),
    rows_first_stage = sum(dat_cc$treated_bjs == 0),
    treated_rows_targeted = sum(dat_cc$treated_bjs == 1),
    treated_rows_imputed = sum(dat_cc$treated_bjs == 1 & !is.na(pr)),
    treated_rows_unimputable = sum(dat_cc$treated_bjs == 1 & is.na(pr)),
    untreated_rows_unimputable = sum(dat_cc$treated_bjs == 0 & is.na(pr)),
    treated_zero_weight_rows = sum(dat_cc$treated_bjs == 1 & dat_cc$w_all == 0),
    zero_weight_rows_total = sum(dat_cc$w_all == 0),
    fe_collinearity_warnings = "none (feols run with warn = TRUE)",
    singleton_fe_levels = sum(
      table(dat_cc[[fe_col]][dat_cc$treated_bjs == 0]) == 1
    ),
    clustering_units = n_distinct(dat_cc[[fe_col]])
  )
}
a_region <- audit_first_stage("region_id")
a_district <- audit_first_stage("district_id")
bjs_region_fake_run_audit <- a_region |>
  mutate(fe_level = "region (ADM2)", .before = 1)
bjs_district_fake_run_audit <- a_district |>
  mutate(fe_level = "district (ADM3)", .before = 1)
print(bjs_region_fake_run_audit)
print(bjs_district_fake_run_audit)
write_csv(
  bjs_region_fake_run_audit,
  file.path(out_dir, "bjs_region_fake_run_audit.csv")
)
write_csv(
  bjs_district_fake_run_audit,
  file.path(out_dir, "bjs_district_fake_run_audit.csv")
)

# ---------------------------------------------------------------------
# 8. Historical post-treatment bins via custom wtr
# ---------------------------------------------------------------------
dat_cc <- dat_cc |>
  mutate(
    rel_year_eff = DHSYEAR - g_eff,
    w_0_1 = as.numeric(treated_bjs == 1 & between(rel_year_eff, 0, 1)),
    w_2_3 = as.numeric(treated_bjs == 1 & between(rel_year_eff, 2, 3)),
    w_4_5 = as.numeric(treated_bjs == 1 & between(rel_year_eff, 4, 5)),
    w_6_7 = as.numeric(treated_bjs == 1 & between(rel_year_eff, 6, 7)),
    w_8_9 = as.numeric(treated_bjs == 1 & between(rel_year_eff, 8, 9)),
    w_ge10 = as.numeric(treated_bjs == 1 & rel_year_eff >= 10)
  )
bins <- c("w_0_1", "w_2_3", "w_4_5", "w_6_7", "w_8_9", "w_ge10")

bin_support <- bind_rows(lapply(bins, function(b) {
  sub <- dat_cc |> filter(.data[[b]] == 1)
  tibble(
    bin = b,
    treated_hh = nrow(sub),
    treated_clusters = n_distinct(sub$cluster_uid),
    treated_pas = n_distinct(sub$WDPAID),
    districts_represented = n_distinct(sub$district_id),
    cohorts_represented = n_distinct(sub$g_eff)
  )
}))

res_district_bins <- did_imputation(
  data = dat_cc,
  yname = "fake_y",
  gname = "g_bjs",
  tname = "DHSYEAR",
  idname = "row_id",
  first_stage = as.formula(paste("~", fs_base, "| district_id + DHSYEAR")),
  wname = "w_all",
  wtr = bins,
  cluster_var = "district_id"
)
res_region_bins <- did_imputation(
  data = dat_cc,
  yname = "fake_y",
  gname = "g_bjs",
  tname = "DHSYEAR",
  idname = "row_id",
  first_stage = as.formula(paste("~", fs_base, "| region_id + DHSYEAR")),
  wname = "w_all",
  wtr = bins,
  cluster_var = "region_id"
)

bjs_post_bin_support <- bin_support |>
  left_join(
    res_district_bins |>
      transmute(
        bin = term,
        fake_est_district = estimate,
        fake_se_district = std.error
      ),
    by = "bin"
  ) |>
  left_join(
    res_region_bins |>
      transmute(
        bin = term,
        fake_est_region = estimate,
        fake_se_region = std.error
      ),
    by = "bin"
  )
print(as.data.frame(bjs_post_bin_support))
write_csv(bjs_post_bin_support, file.path(out_dir, "bjs_post_bin_support.csv"))

# ---------------------------------------------------------------------
# 9. Pretrend / lead mechanism findings
# ---------------------------------------------------------------------
# From the installed source (didimputation 0.3.0):
#   - pretrends adds i(zz000event_time, keep = c(...)) to the first stage,
#     re-estimated on the untreated sample (never-treated + not-yet-treated):
#     regression-based placebo leads, NOT imputation-based placebos.
#   - keep requires exact event-time values: custom BINNED leads are not
#     supported natively.
#   - joint pretrend inference is not returned natively; the natural
#     mechanism is a Wald test on the same internal pre regression.
event_times <- sort(unique(
  dat_cc$DHSYEAR[dat_cc$g_bjs > 0] -
    dat_cc$g_bjs[dat_cc$g_bjs > 0]
))
pre_keep <- sort(intersect(-9:-1, event_times))
cat(
  "\nAvailable lead event times:",
  paste(event_times[event_times < 0], collapse = ", "),
  "\n"
)
cat(
  "Lead dummies testable via pretrends:",
  paste(pre_keep, collapse = ", "),
  "\n"
)

res_pre_d <- did_imputation(
  data = dat_cc,
  yname = "fake_y",
  gname = "g_bjs",
  tname = "DHSYEAR",
  idname = "row_id",
  first_stage = as.formula(paste("~", fs_base, "| district_id + DHSYEAR")),
  wname = "w_all",
  wtr = bins,
  cluster_var = "district_id",
  pretrends = pre_keep
)
print(res_pre_d[res_pre_d$term %in% as.character(pre_keep), ])

# joint Wald mechanics on the replicated internal pre regression
pre_formula <- as.formula(paste(
  "fake_y ~ i(zz000event_time, keep = c(",
  paste(pre_keep, collapse = ", "),
  ")) +",
  fs_base,
  "| district_id + DHSYEAR"
))
unt_zz <- dat_cc |>
  mutate(
    zz000treat = as.numeric(DHSYEAR >= g_bjs & g_bjs > 0),
    zz000event_time = ifelse(g_bjs == 0, -Inf, as.numeric(DHSYEAR - g_bjs))
  ) |>
  filter(zz000treat == 0)
pre_est <- feols(
  pre_formula,
  data = unt_zz,
  weights = ~w_all,
  warn = FALSE,
  notes = FALSE
)
wald(pre_est, keep = "zz000event_time")

# ---------------------------------------------------------------------
# 10. Design-only region vs district comparison
# ---------------------------------------------------------------------
tr_cc <- dat_cc |> filter(treated_bjs == 1)
med_r <- region_support_full |> filter(treated_hh > 0)
med_d <- district_support_full |> filter(treated_hh > 0)

region_vs_district_comparison <- tibble(
  criterion = c(
    "FE groups in GADM",
    "Groups represented in matched data",
    "Treated groups represented",
    "Treated groups with untreated support",
    "Treated HH with identified FE",
    "Treated clusters with identified FE",
    "Treated PAs with identified FE",
    "Median untreated HH per treated group",
    "Median untreated clusters per treated group",
    "Median untreated waves per treated group",
    "Fake-run imputable treated share",
    "Collinearity / singleton warnings",
    "Number of clustering units"
  ),
  region_FE = c(
    "22 (ADM2)",
    "22",
    nrow(med_r),
    sum(med_r$fe_identified),
    nrow(tr_cc),
    n_distinct(tr_cc$cluster_uid),
    n_distinct(tr_cc$WDPAID),
    median(med_r$untreated_hh),
    median(med_r$untreated_clusters),
    median(med_r$untreated_waves),
    a_region$treated_rows_imputed / a_region$treated_rows_targeted,
    "none",
    "22 (region_id)"
  ),
  district_FE = c(
    "110 (ADM3)",
    n_distinct(dat_cc$district_id),
    nrow(med_d),
    sum(med_d$fe_identified),
    nrow(tr_cc),
    n_distinct(tr_cc$cluster_uid),
    n_distinct(tr_cc$WDPAID),
    median(med_d$untreated_hh),
    median(med_d$untreated_clusters),
    median(med_d$untreated_waves),
    a_district$treated_rows_imputed / a_district$treated_rows_targeted,
    "none",
    "99 (district_id)"
  )
)
print(as.data.frame(region_vs_district_comparison))
write_csv(
  region_vs_district_comparison,
  file.path(out_dir, "region_vs_district_comparison.csv")
)

cat("\nAudit complete. No real outcome was loaded or estimated.\n")
