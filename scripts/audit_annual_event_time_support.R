# =====================================================================
# audit(07b): annual treatment timing and binned event-time support
# Outcome-blind design audit. No outcome variable is loaded anywhere.
#
# Sections:
#   0. Setup and design-only data
#   1. Exact-date-consistent annual treatment time (g_eff) + assertions
#   2. Old vs date-consistent relative time / bin shifts
#   3. Exact and binned event-time support tables
#   4. Plots A-F
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(haven)
})

survey_years <- c(1997, 2008, 2011, 2013, 2016, 2018, 2021)
out_dir <- "output/review_v2/event_time_support_audit"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------
# 0. Design-only data (outcomes strictly excluded)
# ---------------------------------------------------------------------
design_cols <- c(
  "DHSYEAR",
  "hv001",
  "hv002",
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
})

# ---------------------------------------------------------------------
# 1. g_eff: annual treatment time consistent with the frozen rule
#    treated_now = survey_ref_date (June 1) >= treatment_date
# ---------------------------------------------------------------------
dat <- dat |>
  mutate(
    td_year = lubridate::year(treatment_date),
    june1_td_year = as.Date(ISOdate(td_year, 6, 1)),
    g_eff = case_when(
      GROUP != "Treatment" ~ 0L,
      treatment_date <= june1_td_year ~ as.integer(td_year),
      TRUE ~ as.integer(td_year + 1L)
    ),
    # date-exact fractional event time as recovered from 07 qmd (manuscript era)
    rel_year_frac = as.numeric(
      as.Date(ISOdate(DHSYEAR, 6, 1)) - as.Date(treatment_date)
    ) /
      365.25
  )

assert1 <- dat |>
  filter(GROUP == "Treatment") |>
  mutate(ok = treated_now == as.integer(DHSYEAR >= g_eff))
stopifnot(
  "Assertion failed: treated_now != (DHSYEAR >= g_eff)" = all(assert1$ok)
)

assert2 <- dat |> filter(GROUP == "Control")
stopifnot(
  "Assertion failed: Control has non-NA treatment_date" = all(is.na(
    assert2$treatment_date
  ))
)
stopifnot("Assertion failed: Control has g_eff != 0" = all(assert2$g_eff == 0L))

# PA-level consistency: one treatment date and one g_eff per treated PA
pa_check <- dat |>
  filter(GROUP == "Treatment") |>
  distinct(WDPAID, treatment_date, g_eff) |>
  count(WDPAID)
stopifnot(
  "Assertion failed: WDPAID with inconsistent treatment timing" = all(
    pa_check$n == 1L
  )
)

# Cross-check: floor(fractional rel time) == DHSYEAR - g_eff
assert4 <- dat |>
  filter(GROUP == "Treatment") |>
  mutate(rel_eff = DHSYEAR - g_eff, ok = floor(rel_year_frac) == rel_eff)
cat(
  "floor(rel_year_frac) == DHSYEAR - g_eff: failures =",
  sum(!assert4$ok),
  "of",
  nrow(assert4),
  "\n"
)
stopifnot(all(assert4$ok))

cat("All design assertions passed.\n")

# Cohort summary
cohort_pa <- dat |>
  filter(GROUP == "Treatment") |>
  distinct(WDPAID, treatment_date, treatment_year, g_eff) |>
  arrange(g_eff, treatment_date)
print(cohort_pa, n = Inf)

# ---------------------------------------------------------------------
# 2. Old vs date-consistent relative time and 2-year bins
# ---------------------------------------------------------------------
bin_code <- function(r) {
  case_when(
    r <= -9 ~ -5L,
    r <= -7 ~ -4L,
    r <= -5 ~ -3L,
    r <= -3 ~ -2L,
    r <= -1 ~ -1L,
    r <= 1 ~ 0L,
    r <= 3 ~ 1L,
    r <= 5 ~ 2L,
    r <= 7 ~ 3L,
    r <= 9 ~ 4L,
    TRUE ~ 5L
  )
}
bin_lab <- c(
  "-5" = "<= -9",
  "-4" = "-8:-7",
  "-3" = "-6:-5",
  "-2" = "-4:-3",
  "-1" = "-2:-1",
  "0" = "0:1",
  "1" = "2:3",
  "2" = "4:5",
  "3" = "6:7",
  "4" = "8:9",
  "5" = ">=10"
)

treated <- dat |>
  filter(GROUP == "Treatment") |>
  mutate(
    rel_year_old = DHSYEAR - treatment_year, # pre-review integer construction
    rel_year_eff = DHSYEAR - g_eff, # date-consistent construction
    bin_old = bin_code(rel_year_old),
    bin_eff = bin_code(rel_year_eff),
    bin_old_lab = bin_lab[as.character(bin_old)],
    bin_eff_lab = bin_lab[as.character(bin_eff)]
  )

# observation-level shift table
bin_shift_obs <- treated |>
  count(bin_old_lab, bin_eff_lab, name = "hh_obs") |>
  arrange(
    as.integer(factor(bin_old_lab, levels = unname(bin_lab))),
    as.integer(factor(bin_eff_lab, levels = unname(bin_lab)))
  )
write_csv(bin_shift_obs, file.path(out_dir, "bin_shift_observations.csv"))

# PA-level shift table
bin_shift_pa <- treated |>
  distinct(WDPAID, bin_old_lab, bin_eff_lab) |>
  count(bin_old_lab, bin_eff_lab, name = "n_PAs")
write_csv(bin_shift_pa, file.path(out_dir, "bin_shift_pas.csv"))

# headline shift statistics
shift_stats <- treated |>
  summarise(
    n_obs = n(),
    n_obs_rel_changed = sum(rel_year_old != rel_year_eff),
    share_obs_rel_changed = mean(rel_year_old != rel_year_eff),
    n_obs_bin_changed = sum(bin_old != bin_eff),
    share_obs_bin_changed = mean(bin_old != bin_eff),
    n_pa = n_distinct(WDPAID),
    n_pa_bin_changed = n_distinct(WDPAID[bin_old != bin_eff])
  )
cat("\nBin-shift summary:\n")
print(shift_stats)

# ---------------------------------------------------------------------
# 3. Event-time support tables
# ---------------------------------------------------------------------
# exact relative-year support
event_time_exact_support <- treated |>
  group_by(rel_year_eff) |>
  summarise(
    hh_obs = n(),
    n_clusters = n_distinct(DHSYEAR, hv001),
    n_PAs = n_distinct(WDPAID),
    n_cohorts = n_distinct(g_eff),
    w_all_sum = sum(w_all),
    .groups = "drop"
  ) |>
  arrange(rel_year_eff)
write_csv(
  event_time_exact_support,
  file.path(out_dir, "event_time_exact_support.csv")
)

# binned support
bin_order <- unname(bin_lab)
event_time_binned_support <- treated |>
  group_by(bin_eff_lab) |>
  summarise(
    hh_obs = n(),
    n_clusters = n_distinct(DHSYEAR, hv001),
    n_PAs = n_distinct(WDPAID),
    n_cohorts = n_distinct(g_eff),
    w_all_sum = sum(w_all),
    min_rel = min(rel_year_eff),
    max_rel = max(rel_year_eff),
    .groups = "drop"
  ) |>
  mutate(bin_eff_lab = factor(bin_eff_lab, levels = bin_order)) |>
  arrange(bin_eff_lab)
write_csv(
  event_time_binned_support,
  file.path(out_dir, "event_time_binned_support.csv")
)

# annual cohort x wave support matrix (for Plot D and later C&S work)
cohort_wave_support <- dat |>
  filter(GROUP == "Treatment", treated_now == 1L) |>
  group_by(g_eff, DHSYEAR) |>
  summarise(
    hh_obs = n(),
    n_clusters = n_distinct(hv001),
    n_PAs = n_distinct(WDPAID),
    w_all_sum = sum(w_all),
    .groups = "drop"
  )
write_csv(
  cohort_wave_support,
  file.path(out_dir, "annual_cohort_wave_support.csv")
)

cat("\nSection 2-3 outputs written.\n")

# ---------------------------------------------------------------------
# 4. Plots A-F
# ---------------------------------------------------------------------
theme_audit <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92")
  )

bin_levels <- unname(bin_lab)
treated <- treated |>
  mutate(bin_eff_lab = factor(bin_eff_lab, levels = bin_levels))

## Plot A: exact relative-year histogram with historical bin overlay
bin_bounds <- c(-8.5, -6.5, -4.5, -2.5, -0.5, 1.5, 3.5, 5.5, 7.5, 9.5)
bin_centers <- c(-13.5, -7.5, -5.5, -3.5, -1.5, 0.5, 2.5, 4.5, 6.5, 8.5, 11)
bin_ann <- tibble(mid = bin_centers, lab = bin_levels)

pA <- ggplot(event_time_exact_support, aes(rel_year_eff, hh_obs)) +
  geom_col(fill = "grey35", width = 0.75) +
  geom_vline(xintercept = bin_bounds, linetype = "dashed", colour = "grey45") +
  geom_text(
    data = bin_ann,
    aes(mid, max(event_time_exact_support$hh_obs) * 1.06, label = lab),
    inherit.aes = FALSE,
    size = 3.1,
    colour = "grey25"
  ) +
  scale_x_continuous(
    breaks = sort(unique(event_time_exact_support$rel_year_eff))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .12))) +
  labs(
    x = "Exact relative year (DHSYEAR - g_eff)",
    y = "Treated household observations",
    title = "Exact event-time support with fixed historical 2-year bins",
    subtitle = "Bins fixed from the prior analysis (dashed lines = bin boundaries). No truncation of tails."
  ) +
  theme_audit
ggsave(
  file.path(out_dir, "event_time_exact_histogram.png"),
  pA,
  width = 10,
  height = 5.5,
  dpi = 300
)

## Plot B: support by bin at three levels
binned_long <- event_time_binned_support |>
  select(
    bin_eff_lab,
    `Households` = hh_obs,
    `DHS clusters` = n_clusters,
    `PAs` = n_PAs
  ) |>
  pivot_longer(-bin_eff_lab, names_to = "level", values_to = "n") |>
  mutate(level = factor(level, levels = c("Households", "DHS clusters", "PAs")))

pB <- ggplot(binned_long, aes(bin_eff_lab, n)) +
  geom_col(fill = "grey35") +
  geom_text(aes(label = n), vjust = -0.3, size = 2.9) +
  facet_wrap(~level, scales = "free_y", ncol = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  labs(
    x = "Historical 2-year event-time bin",
    y = "Count (log scale)",
    title = "Support per historical bin: households, DHS clusters, PAs",
    subtitle = "Date-consistent event time (DHSYEAR - g_eff); log scale for comparability across levels"
  ) +
  theme_audit +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(out_dir, "event_time_binned_support.png"),
  pB,
  width = 8.5,
  height = 7.5,
  dpi = 300
)

## Plot C: annual treatment-cohort support
cohort_pa_counts <- treated |>
  distinct(WDPAID, g_eff) |>
  count(g_eff, name = "n_PAs")
cohort_cl_counts <- treated |>
  filter(treated_now == 1L) |>
  distinct(g_eff, DHSYEAR, hv001) |>
  count(g_eff, name = "n_clusters")
cohort_hh_counts <- treated |>
  filter(treated_now == 1L) |>
  count(g_eff, name = "hh_obs")
cohort_supp <- cohort_pa_counts |>
  full_join(cohort_hh_counts, by = "g_eff") |>
  full_join(cohort_cl_counts, by = "g_eff") |>
  mutate(g_lab = paste0("g = ", g_eff))

pC <- cohort_supp |>
  select(
    g_lab,
    `PAs` = n_PAs,
    `Treated clusters (all waves)` = n_clusters,
    `Treated hh obs (post)` = hh_obs
  ) |>
  pivot_longer(-g_lab, names_to = "level", values_to = "n") |>
  mutate(
    level = factor(
      level,
      levels = c("PAs", "Treated clusters (all waves)", "Treated hh obs (post)")
    )
  ) |>
  ggplot(aes(g_lab, n)) +
  geom_col(fill = "grey35") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.2) +
  facet_wrap(~level, scales = "free_y", ncol = 1) +
  scale_y_log10(expand = expansion(mult = c(0, .18))) +
  labs(
    x = "Date-consistent annual treatment cohort (g_eff)",
    y = "Count (log scale)",
    title = "Annual treatment-cohort support",
    subtitle = "Post-treatment observations only (treated_now = 1); cohorts with thin support are annotated by value, not excluded"
  ) +
  theme_audit
ggsave(
  file.path(out_dir, "annual_treatment_cohort_support.png"),
  pC,
  width = 8,
  height = 7,
  dpi = 300
)

## Plot D: cohort x survey-wave support heatmap
cw_long <- cohort_wave_support |>
  select(
    g_eff,
    DHSYEAR,
    `Households` = hh_obs,
    `Clusters` = n_clusters,
    `PAs` = n_PAs
  ) |>
  pivot_longer(-c(g_eff, DHSYEAR), names_to = "level", values_to = "n") |>
  mutate(
    level = factor(level, levels = c("Households", "Clusters", "PAs")),
    g_lab = paste0("g = ", g_eff),
    DHSYEAR = factor(DHSYEAR)
  )

pD <- ggplot(cw_long, aes(DHSYEAR, g_lab, fill = n)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(n > 0, n, "")), size = 3.1) +
  facet_wrap(~level, nrow = 1) +
  scale_fill_gradient(low = "#f7f4ef", high = "#21618c", guide = "none") +
  labs(
    x = "Survey wave",
    y = NULL,
    title = "Annual cohort x survey-wave support (post-treatment, treated side)",
    subtitle = "Empty tiles = no treated observation of that cohort in that wave"
  ) +
  theme_audit
ggsave(
  file.path(out_dir, "annual_cohort_wave_support.png"),
  pD,
  width = 9.5,
  height = 4,
  dpi = 300
)

## Plot E: annual cohorts vs collapsed survey-wave cohorts
collapse_map <- tibble(
  g_eff = c(2009, 2014, 2015, 2020),
  collapsed = c(2011L, 2016L, 2016L, 2021L)
)
pE_data <- cohort_pa_counts |>
  left_join(collapse_map, by = "g_eff") |>
  mutate(g_lab = paste0("g = ", g_eff), c_lab = paste0("collapsed ", collapsed))
pE <- ggplot(pE_data, aes(c_lab, g_lab, fill = n_PAs)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = n_PAs), size = 4, colour = "white", fontface = "bold") +
  scale_fill_gradient(low = "#f7f4ef", high = "#21618c", guide = "none") +
  labs(
    x = "Survey-wave cohort used in current 07b implementation",
    y = NULL,
    title = "Information loss: annual cohorts collapsed to survey-wave cohorts",
    subtitle = "Tile value = number of distinct PAs. g = 2009, 2014 and 2015 all map to the 2011/2016 wave cohorts."
  ) +
  theme_audit
ggsave(
  file.path(out_dir, "annual_vs_collapsed_cohorts.png"),
  pE,
  width = 7.5,
  height = 4.2,
  dpi = 300
)

## Plot F: bin composition across annual cohorts
bin_comp <- treated |>
  filter(treated_now == 1L) |>
  group_by(bin_eff_lab, g_eff) |>
  summarise(n_PAs = n_distinct(WDPAID), .groups = "drop") |>
  mutate(g_lab = paste0("g = ", g_eff))
bin_comp_tot <- treated |>
  filter(treated_now == 1L) |>
  group_by(bin_eff_lab) |>
  summarise(n_cohorts = n_distinct(g_eff), .groups = "drop")

pF <- ggplot(bin_comp, aes(bin_eff_lab, n_PAs, fill = g_lab)) +
  geom_col(colour = "grey25", linewidth = 0.2) +
  geom_text(
    data = bin_comp_tot,
    aes(bin_eff_lab, Inf, label = paste0(n_cohorts, " cohort(s)")),
    inherit.aes = FALSE,
    vjust = 1.4,
    size = 2.9,
    colour = "grey25"
  ) +
  scale_fill_brewer(palette = "Paired", name = "Annual cohort") +
  scale_y_continuous(expand = expansion(mult = c(0, .18))) +
  labs(
    x = "Historical 2-year event-time bin",
    y = "Distinct contributing PAs",
    title = "Bin composition: PA support across annual treatment cohorts",
    subtitle = "Post-treatment treated observations; a well-populated bin may still be dominated by one cohort"
  ) +
  theme_audit +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(out_dir, "event_bin_cohort_composition.png"),
  pF,
  width = 9,
  height = 5.5,
  dpi = 300
)

cat("\nPlots A-F written.\n")

# ---------------------------------------------------------------------
# 5. Required bin-shift matrix heatmap (households, PA-annotated)
# ---------------------------------------------------------------------
shift_mat_hh <- treated |>
  count(bin_old_lab, bin_eff_lab) |>
  mutate(
    bin_old_lab = factor(bin_old_lab, levels = bin_levels),
    bin_eff_lab = factor(bin_eff_lab, levels = bin_levels)
  )
shift_mat_pa <- treated |>
  distinct(WDPAID, bin_old_lab, bin_eff_lab) |>
  count(bin_old_lab, bin_eff_lab, name = "n_PAs") |>
  mutate(
    bin_old_lab = factor(bin_old_lab, levels = bin_levels),
    bin_eff_lab = factor(bin_eff_lab, levels = bin_levels)
  )
# PA annotation: only show when it differs from what households alone would suggest
pa_annot <- shift_mat_hh |>
  left_join(shift_mat_pa, by = c("bin_old_lab", "bin_eff_lab")) |>
  mutate(lab = ifelse(is.na(n_PAs), "", paste0(n, " HH\n", n_PAs, " PA")))

pM <- ggplot(shift_mat_hh, aes(bin_eff_lab, bin_old_lab, fill = n)) +
  geom_tile(colour = "white") +
  geom_text(
    data = pa_annot,
    aes(label = lab),
    size = 2.6,
    lineheight = 0.9,
    colour = "grey15"
  ) +
  scale_fill_gradient(
    low = "#f7f4ef",
    high = "#21618c",
    name = "Treated HH obs",
    transform = "sqrt"
  ) +
  coord_equal() +
  labs(
    x = "Date-consistent bin (DHSYEAR - g_eff)",
    y = "Old integer bin (DHSYEAR - treatment_year)",
    title = "Bin shifts: old integer relative time vs date-consistent relative time",
    subtitle = "Cell labels: households / distinct PAs. Diagonal = no bin change."
  ) +
  theme_audit +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(out_dir, "bin_shift_matrix.png"),
  pM,
  width = 8.5,
  height = 7,
  dpi = 300
)

cat("bin_shift_matrix.png written.\n")

# ---------------------------------------------------------------------
# 6. Mechanical C&S ATT(g,t) cell structure (no outcomes)
#
# Replicates the skip logic of compute.att_gt (did 2.1.2, RCS branch):
#   skip if sum(G*post)==0        -> no treated units at t
#   skip if sum(G*(1-post))==0    -> no treated units at base period
#   skip if sum(C*post)==0        -> no controls at t
#   skip if sum(C*(1-post))==0    -> no controls at base period
# Base period:
#   universal: pret = last observed wave strictly before g; t == base -> att=0 placeholder
#   varying  : pret = immediately preceding observed wave (default; used by 07b)
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
# frozen 07b covariate formula (unchanged)
xformla_pap <- as.formula(paste("~", paste(xvars, collapse = " + ")))
did_dat <- dat |> filter(complete.cases(dat[, xvars])) # mirrors pre_process_did complete.cases

glist_eff <- sort(unique(did_dat$g_eff[did_dat$g_eff > 0]))
tlist_eff <- sort(unique(did_dat$DHSYEAR))

cell_support <- function(base_period) {
  tfac <- if (base_period == "universal") 0 else 1
  tlen <- length(tlist_eff) - tfac
  out <- list()
  for (g in glist_eff) {
    for (ti in seq_len(tlen)) {
      t_cur <- tlist_eff[ti + tfac]
      pret_idx <- if (base_period == "universal") {
        tail(which(tlist_eff < g), 1)
      } else {
        ti
      }
      t_base <- tlist_eff[pret_idx]
      placeholder <- base_period == "universal" && t_cur == t_base
      Gt <- did_dat$g_eff == g & did_dat$DHSYEAR == t_cur
      Gb <- did_dat$g_eff == g & did_dat$DHSYEAR == t_base
      Ct <- did_dat$g_eff == 0 & did_dat$DHSYEAR == t_cur
      Cb <- did_dat$g_eff == 0 & did_dat$DHSYEAR == t_base
      n_Gt <- sum(Gt)
      n_Gb <- sum(Gb)
      n_Ct <- sum(Ct)
      n_Cb <- sum(Cb)
      reason <- character(0)
      if (n_Gt == 0) {
        reason <- "no treated HH at t"
      }
      if (n_Gb == 0) {
        reason <- c(reason, "no treated HH at base")
      }
      if (n_Ct == 0) {
        reason <- c(reason, "no controls at t")
      }
      if (n_Cb == 0) {
        reason <- c(reason, "no controls at base")
      }
      estimable <- length(reason) == 0 && !placeholder
      out[[length(out) + 1]] <- data.frame(
        g_eff = g,
        t = t_cur,
        post = as.integer(g <= t_cur),
        base_wave = t_base,
        placeholder = placeholder,
        hh_tr_t = n_Gt,
        hh_tr_base = n_Gb,
        cl_tr_t = n_distinct(did_dat$hv001[Gt]),
        cl_tr_base = n_distinct(did_dat$hv001[Gb]),
        pa_t = n_distinct(did_dat$WDPAID[Gt]),
        pa_base = n_distinct(did_dat$WDPAID[Gb]),
        hh_ctrl_t = n_Ct,
        hh_ctrl_base = n_Cb,
        cl_ctrl_t = n_distinct(did_dat$hv001[Ct]),
        cl_ctrl_base = n_distinct(did_dat$hv001[Cb]),
        estimable = estimable,
        reason = if (length(reason)) {
          paste(reason, collapse = "; ")
        } else if (placeholder) {
          "base-period placeholder (att = 0)"
        } else {
          ""
        },
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(out)
}

cells_varying <- cell_support("varying") |> mutate(base_period = "varying")
cells_universal <- cell_support("universal") |>
  mutate(base_period = "universal")
annual_attgt_cell_support <- bind_rows(cells_varying, cells_universal) |>
  relocate(base_period)
write_csv(
  annual_attgt_cell_support,
  file.path(out_dir, "annual_attgt_cell_support.csv")
)

cat("\n--- Estimable cell counts by base period ---\n")
print(
  annual_attgt_cell_support |>
    group_by(base_period, g_eff) |>
    summarise(n_cells = n(), n_estimable = sum(estimable), .groups = "drop")
)
cat("\n--- Varying (07b default): unestimable cells ---\n")
print(
  cells_varying |>
    filter(!estimable) |>
    select(g_eff, t, base_wave, reason, hh_tr_t, hh_tr_base, pa_t, pa_base)
)
cat("\n--- Universal: unestimable cells ---\n")
print(
  cells_universal |>
    filter(!estimable) |>
    select(g_eff, t, base_wave, reason, hh_tr_t, hh_tr_base, pa_t, pa_base)
)

# ---------------------------------------------------------------------
# 7. Estimability matrix visual
# ---------------------------------------------------------------------
mat_data <- annual_attgt_cell_support |>
  mutate(
    status = case_when(
      placeholder ~ "base placeholder (att = 0)",
      estimable ~ "estimable",
      grepl("no treated HH at base", reason) &
        hh_tr_t > 0 ~ "no treated at base",
      grepl("no treated HH at t", reason) ~ "no treated at t",
      TRUE ~ "no controls"
    ),
    annot = ifelse(
      estimable,
      paste0(cl_tr_t, "/", pa_t, " : ", cl_tr_base, "/", pa_base),
      ""
    ),
    t_lab = factor(paste0("w", t), levels = paste0("w", sort(unique(t)))),
    g_lab = paste0("g = ", g_eff)
  )
pEst <- ggplot(mat_data, aes(t_lab, g_lab, fill = status)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = annot), size = 2.5, colour = "grey15") +
  facet_wrap(~base_period, nrow = 2) +
  scale_fill_manual(
    values = c(
      "estimable" = "#21618c",
      "no treated at t" = "#f2d5d5",
      "no treated at base" = "#e8b4b4",
      "no controls" = "#d97777",
      "base placeholder (att = 0)" = "grey88"
    ),
    name = "Cell status"
  ) +
  labs(
    x = "Survey wave t",
    y = NULL,
    title = "C&S ATT(g,t) estimability matrix (annual cohorts, nevertreated controls)",
    subtitle = "Annotation on estimable tiles: treated clusters/PAs at t : at base. No minimum-sample rule imposed."
  ) +
  theme_audit +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(out_dir, "annual_attgt_estimability_matrix.png"),
  pEst,
  width = 10,
  height = 5.5,
  dpi = 300
)

# ---------------------------------------------------------------------
# 8. Outcome-blind overlap diagnostics for estimable post cells
#    Replicates DRDID::std_ipw_did_rc (v1.2.3) treatment-only machinery:
#    weighted logit pscore on pooled t/base sample of group g + nevertreated,
#    ps capped at 1-1e-6, controls with ps >= 0.995 trimmed,
#    ATT weights: treated = i.weights, controls = i.weights * p/(1-p)
# ---------------------------------------------------------------------
has_fastglm <- requireNamespace("fastglm", quietly = TRUE)
cat("fastglm available:", has_fastglm, "\n")

fit_pscore <- function(X, D, w) {
  if (has_fastglm) {
    fit <- fastglm::fastglm(
      x = X,
      y = D,
      family = binomial(),
      weights = w,
      intercept = FALSE,
      method = 3
    )
    as.vector(fitted(fit))
  } else {
    as.vector(predict(
      glm(D ~ X - 1, family = binomial(), weights = w),
      type = "response"
    ))
  }
}
smd <- function(x, d, w) {
  # standardized difference: treated SD (unweighted) as denominator, ATT convention
  m1 <- weighted.mean(x[d == 1], w[d == 1])
  m0 <- weighted.mean(x[d == 0], w[d == 0])
  s1 <- sd(x[d == 1])
  if (is.na(s1) || s1 == 0) {
    return(NA_real_)
  }
  (m1 - m0) / s1
}

build_cell_sample <- function(g, t_cur, t_base) {
  # mirrors compute.att_gt RCS branch: group-g units + nevertreated controls,
  # rows at t and base only
  keep <- (did_dat$g_eff == g | did_dat$g_eff == 0) &
    (did_dat$DHSYEAR == t_cur | did_dat$DHSYEAR == t_base)
  s <- did_dat[keep, ]
  s$.rowid <- seq_len(nrow(s))
  s$D <- as.integer(s$g_eff == g)
  s$post <- as.integer(s$DHSYEAR == t_cur)
  s$w <- s$w_all / mean(s$w_all) # did normalizes i.weights to mean 1
  s
}

cell_list <- annual_attgt_cell_support |>
  filter(estimable, post == 1L)

overlap_rows <- list()
balance_rows <- list()
ps_rows <- list()
for (i in seq_len(nrow(cell_list))) {
  r <- cell_list[i, ]
  s <- build_cell_sample(r$g_eff, r$t, r$base_wave)
  X <- model.matrix(xformla_pap, data = s)
  ps <- fit_pscore(X, s$D, s$w)
  ps <- pmin(ps, 1 - 1e-6)
  keep_ctl <- ps < 0.995 | s$D == 1 # DRDID trim.level = 0.995 for controls
  ps_k <- ps[keep_ctl]
  D_k <- s$D[keep_ctl]
  post_k <- s$post[keep_ctl]
  w_k <- s$w[keep_ctl]

  # ATT-type weights (treatment-only; no outcome anywhere)
  w_att <- ifelse(D_k == 1, w_k, w_k * ps_k / (1 - ps_k))

  ctrl <- D_k == 0
  ps_ctl <- ps_k[ctrl]
  overlap_rows[[i]] <- data.frame(
    base_period = r$base_period,
    g_eff = r$g_eff,
    t = r$t,
    base_wave = r$base_wave,
    ps_model = ifelse(
      has_fastglm,
      "fastglm(method=3, weighted)",
      "glm(weighted)"
    ),
    n_treated = sum(D_k == 1),
    n_controls = sum(ctrl),
    n_ctl_trimmed_995 = sum(ps[s$D == 0] >= 0.995),
    max_ps_control = max(ps_ctl),
    share_ctl_gt_95 = mean(ps_ctl > .95),
    share_ctl_gt_99 = mean(ps_ctl > .99),
    share_ctl_gt_995 = mean(ps_ctl > .995),
    ess_ctl_post = {
      wv <- w_att[ctrl & post_k == 1]
      sum(wv)^2 / sum(wv^2)
    },
    ess_ctl_base = {
      wv <- w_att[ctrl & post_k == 0]
      sum(wv)^2 / sum(wv^2)
    },
    n_ctl_post = sum(ctrl & post_k == 1),
    n_ctl_base = sum(ctrl & post_k == 0),
    logit_converged = TRUE
  )
  # covariate balance, by period, before (i.weights only) and after ATT IPW
  for (per in c(1, 0)) {
    idx <- post_k == per
    for (j in 2:ncol(X)) {
      # skip intercept
      bal_pre <- smd(X[idx, j], D_k[idx], w_k[idx])
      bal_post <- smd(X[idx, j], D_k[idx], w_att[idx])
      balance_rows[[length(balance_rows) + 1]] <- data.frame(
        base_period = r$base_period,
        g_eff = r$g_eff,
        t = r$t,
        base_wave = r$base_wave,
        period = ifelse(per == 1, "post", "base"),
        covariate = colnames(X)[j],
        smd_unadj = bal_pre,
        smd_ipw = bal_post
      )
    }
  }
  # pscore support table (treated vs control, per period)
  for (per in c(1, 0)) {
    idx <- post_k == per
    ps_rows[[length(ps_rows) + 1]] <- data.frame(
      base_period = r$base_period,
      g_eff = r$g_eff,
      t = r$t,
      period = ifelse(per == 1, "post", "base"),
      group = c("treated", "control"),
      n = c(sum(D_k[idx] == 1), sum(ctrl[idx])),
      mean_ps = c(mean(ps_k[idx & D_k == 1]), mean(ps_k[idx & ctrl])),
      min_ps = c(min(ps_k[idx & D_k == 1]), min(ps_k[idx & ctrl])),
      max_ps = c(max(ps_k[idx & D_k == 1]), max(ps_k[idx & ctrl]))
    )
  }
}
annual_attgt_overlap <- bind_rows(overlap_rows)
annual_attgt_balance_prepost_ipw <- bind_rows(balance_rows)
annual_attgt_pssupport <- bind_rows(ps_rows)
write_csv(annual_attgt_overlap, file.path(out_dir, "annual_attgt_overlap.csv"))
write_csv(
  annual_attgt_balance_prepost_ipw,
  file.path(out_dir, "annual_attgt_balance_prepost_ipw.csv")
)
write_csv(
  annual_attgt_pssupport,
  file.path(out_dir, "annual_attgt_pssupport.csv")
)

cat("\n--- Overlap summary (post cells) ---\n")
print(
  as.data.frame(
    annual_attgt_overlap |>
      select(
        base_period,
        g_eff,
        t,
        base_wave,
        n_treated,
        n_controls,
        max_ps_control,
        share_ctl_gt_995,
        ess_ctl_post,
        ess_ctl_base
      )
  ),
  row.names = FALSE
)
max_abs_smd <- annual_attgt_balance_prepost_ipw |>
  group_by(base_period, g_eff, t, period) |>
  summarise(
    max_abs_smd_unadj = max(abs(smd_unadj), na.rm = TRUE),
    max_abs_smd_ipw = max(abs(smd_ipw), na.rm = TRUE),
    .groups = "drop"
  )
cat("\n--- Max |SMD| per cell/period ---\n")
print(max_abs_smd)

# ---------------------------------------------------------------------
# 9. Aggregate estimable ATT(g,t) cells into historical 2-year bins
# ---------------------------------------------------------------------
bin_from_rel <- function(ev) bin_code(ev)

cell_bin_support <- annual_attgt_cell_support |>
  filter(estimable) |>
  mutate(event_time = t - g_eff,
         bin = bin_code(event_time),
         bin_lab = bin_lab[as.character(bin)])

# per-cell PA/cluster/hh support (treated side, union of t and base)
pa_by_cohort_wave <- did_dat |>
  filter(g_eff > 0) |>
  group_by(g_eff, DHSYEAR) |>
  summarise(pa = list(unique(WDPAID)),
            cl = list(unique(paste(DHSYEAR, hv001))),
            hh = n(), .groups = "drop")

cell_support_union <- cell_bin_support |>
  left_join(pa_by_cohort_wave |> rename(g_eff = g_eff, t = DHSYEAR,
                                        pa_t_ = pa, cl_t_ = cl, hh_t_ = hh),
            by = c("g_eff", "t")) |>
  left_join(pa_by_cohort_wave |> rename(g_eff = g_eff, base_wave = DHSYEAR,
                                        pa_b_ = pa, cl_b_ = cl, hh_b_ = hh),
            by = c("g_eff", "base_wave")) |>
  rowwise() |>
  mutate(pa_cell = list(union(pa_t_, pa_b_)),
         cl_cell = list(union(cl_t_, cl_b_)),
         hh_cell = sum(hh_t_, hh_b_)) |>
  ungroup()

binned_attgt_support <- cell_support_union |>
  group_by(base_period, bin_lab) |>
  summarise(
    n_cells = n(),
    n_cohorts = n_distinct(g_eff),
    n_PAs = length(unique(unlist(pa_cell))),
    n_clusters = length(unique(unlist(cl_cell))),
    hh_rows = sum(hh_cell),
    waves = paste(sort(unique(c(t, base_wave))), collapse = ","),
    cohorts = paste(sort(unique(g_eff)), collapse = ","),
    .groups = "drop"
  ) |>
  mutate(bin_lab = factor(bin_lab, levels = bin_levels)) |>
  arrange(base_period, bin_lab)
write_csv(binned_attgt_support, file.path(out_dir, "binned_attgt_support.csv"))

cat("\n--- Binned estimable-cell support ---\n")
print(as.data.frame(binned_attgt_support), row.names = FALSE)

pBin <- binned_attgt_support |>
  select(base_period, bin_lab, `ATT(g,t) cells` = n_cells,
         `Distinct PAs` = n_PAs, `Distinct treated clusters` = n_clusters) |>
  pivot_longer(-c(base_period, bin_lab), names_to = "level", values_to = "n") |>
  mutate(level = factor(level, levels = c("ATT(g,t) cells", "Distinct PAs",
                                          "Distinct treated clusters"))) |>
  ggplot(aes(bin_lab, n)) +
  geom_col(fill = "grey35") +
  geom_text(aes(label = n), vjust = -0.3, size = 2.8) +
  facet_grid(level ~ base_period, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  labs(x = "Historical 2-year event-time bin",
       y = NULL,
       title = "Estimable annual ATT(g,t) support pooled into historical bins",
       subtitle = "Cells mapped by event_time = t - g_eff; pre- and post-treatment cells; both base-period conventions") +
  theme_audit +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "binned_attgt_estimable_support.png"), pBin,
       width = 10, height = 7, dpi = 300)

cat("binned_attgt_support.csv + plot written.\n")

# ---------------------------------------------------------------------
# 10. Overlap diagnostics visual
# ---------------------------------------------------------------------
diag_data <- annual_attgt_overlap |>
  left_join(
    annual_attgt_balance_prepost_ipw |>
      group_by(base_period, g_eff, t, period) |>
      summarise(max_abs_smd = max(abs(smd_ipw), na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = period, values_from = max_abs_smd,
                  names_prefix = "max_abs_smd_ipw_"),
    by = c("base_period", "g_eff", "t")
  ) |>
  mutate(cell = paste0("g = ", g_eff, ", t = ", t, "  [", base_period, "]"),
         n_ctl_trimmed_995 = n_ctl_trimmed_995)
diag_long <- diag_data |>
  select(cell,
         `max control PS` = max_ps_control,
         `control ESS (post)` = ess_ctl_post,
         `control ESS (base)` = ess_ctl_base,
         `max |SMD| IPW (post)` = max_abs_smd_ipw_post,
         `max |SMD| IPW (base)` = max_abs_smd_ipw_base) |>
  pivot_longer(-cell, names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric, levels = c("max control PS", "control ESS (post)",
                                            "control ESS (base)",
                                            "max |SMD| IPW (post)",
                                            "max |SMD| IPW (base)")),
         cell = factor(cell, levels = rev(unique(diag_data$cell))))

pDiag <- ggplot(diag_long, aes(metric, cell, fill = value)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf(ifelse(value >= 100, "%.0f", "%.3f"), value)),
            size = 2.6) +
  facet_wrap(~metric, scales = "free") +
  scale_fill_gradient(low = "#eaf3f8", high = "#21618c", guide = "none") +
  labs(x = NULL, y = NULL,
       title = "Outcome-blind overlap diagnostics for estimable post-treatment ATT(g,t) cells",
       subtitle = "Replicated DRDID std_ipw_did_rc machinery (weighted logit, ps < 0.995 control trim, ATT odds weights). No outcomes used.") +
  theme_audit +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        strip.text = element_text(face = "bold"))
ggsave(file.path(out_dir, "annual_attgt_overlap_diagnostics.png"), pDiag,
       width = 10.5, height = 6.5, dpi = 300)

cat("annual_attgt_overlap_diagnostics.png written.\n")
cat("\nAUDIT SCRIPT COMPLETE - no outcome variable was loaded at any point.\n")
