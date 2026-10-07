# =====================================================================
# estimate(07b): head-to-head — step 6: comparison tables and figures
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
})

out_dir <- "output/review_v2/staggered_head_to_head"

bin_levels <- c(
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
post_bins <- bin_levels[6:11]

# ---------------------------------------------------------------------
# overall comparison tables (frozen 2x2 read, never rerun)
# ---------------------------------------------------------------------
read_2x2 <- function(csv) {
  x <- read_csv(csv, show_col_types = FALSE)
  tibble(
    estimand = "Frozen 2x2 endpoint (2008 vs 2021), treat_post",
    estimate = x$estimate,
    std.error = x$std.error,
    conf_low = x$conf_low,
    conf_high = x$conf_high,
    p_value = x$p.value,
    n = x$n_obs,
    clusters = x$n_clusters
  )
}

cs_rows <- function(h) {
  res <- readRDS(file.path(out_dir, "cs_results.rds"))
  simple <- res[[paste0("agg_simple_", h)]]
  cal <- res[[paste0("agg_cal_", h)]]
  i21 <- which(res[[paste0("agg_cal_", h)]]$egt == 2021)
  bind_rows(
    tibble(
      estimand = "C&S simple overall ATT (annual cohorts)",
      estimate = simple$overall.att,
      std.error = simple$overall.se,
      conf_low = simple$overall.att - qnorm(0.975) * simple$overall.se,
      conf_high = simple$overall.att + qnorm(0.975) * simple$overall.se,
      p_value = 2 * pnorm(-abs(simple$overall.att / simple$overall.se)),
      n = 36519,
      clusters = 1269
    ),
    tibble(
      estimand = "C&S calendar-2021 ATT",
      estimate = cal$att.egt[i21],
      std.error = cal$se.egt[i21],
      conf_low = cal$att.egt[i21] - qnorm(0.975) * cal$se.egt[i21],
      conf_high = cal$att.egt[i21] + qnorm(0.975) * cal$se.egt[i21],
      p_value = 2 * pnorm(-abs(cal$att.egt[i21] / cal$se.egt[i21])),
      n = 36519,
      clusters = 1269
    )
  )
}

bjs_rows <- function(h) {
  b <- readRDS(file.path(out_dir, "bjs_results.rds"))[[h]]
  ov_d <- b$overall$estimate[b$overall$term == "treat"]
  se_d <- b$overall$std.error[b$overall$term == "treat"]
  ov_r <- b$region_overall$estimate[b$region_overall$term == "treat"]
  se_r <- b$region_overall$std.error[b$region_overall$term == "treat"]
  bind_rows(
    tibble(
      estimand = "BJS overall ATT, district FE (primary)",
      estimate = ov_d,
      std.error = se_d,
      conf_low = ov_d - qnorm(0.975) * se_d,
      conf_high = ov_d + qnorm(0.975) * se_d,
      p_value = 2 * pnorm(-abs(ov_d / se_d)),
      n = b$audit$treated_rows_imputable,
      clusters = 99
    ),
    tibble(
      estimand = "BJS overall ATT, region FE (robustness)",
      estimate = ov_r,
      std.error = se_r,
      conf_low = ov_r - qnorm(0.975) * se_r,
      conf_high = ov_r + qnorm(0.975) * se_r,
      p_value = 2 * pnorm(-abs(ov_r / se_r)),
      n = b$audit$treated_rows_imputable,
      clusters = 22
    )
  )
}

target_desc <- tibble(
  estimand = c(
    "Frozen 2x2 endpoint (2008 vs 2021), treat_post",
    "C&S simple overall ATT (annual cohorts)",
    "C&S calendar-2021 ATT",
    "BJS overall ATT, district FE (primary)",
    "BJS overall ATT, region FE (robustness)"
  ),
  clustering = c(
    "enumeration cluster (CR1, fixest)",
    "enumeration cluster (multiplier bootstrap)",
    "enumeration cluster (multiplier bootstrap)",
    "district (ADM3, didimputation)",
    "region (ADM2, didimputation)"
  ),
  target = c(
    "ATT for matched 2008 and 2021 waves, frozen profile-design weights",
    "weighted avg of estimable ATT(g,t), household-weighted group shares (2009-dominated)",
    "2021 calendar-time aggregation over cohorts observed in 2021",
    "weighted ATT over 4308 imputable post-treatment treated rows, additive untreated model",
    "same target, region FE + region-clustered inference (22 clusters)"
  )
)

for (h in c("h1", "h2")) {
  csv2x2 <- if (h == "h1") {
    "output/review_v2/frozen_profile_main/main_h1.csv"
  } else {
    "output/review_v2/frozen_profile_main/main_h2.csv"
  }
  tab <- bind_rows(read_2x2(csv2x2), cs_rows(h), bjs_rows(h)) |>
    left_join(target_desc, by = "estimand") |>
    mutate(
      outcome = if (h == "h1") {
        "wealth_centile_rural_weighted"
      } else {
        "zscore_wealth"
      },
      .after = estimand
    )
  write_csv(tab, file.path(out_dir, paste0(h, "_overall_comparison.csv")))
  assign(paste0("tab_", h), tab)
}

# ---------------------------------------------------------------------
# event-study data (C&S + BJS on the 11 frozen bins)
# ---------------------------------------------------------------------
es_data <- function(h) {
  cs <- read_csv(
    file.path(out_dir, paste0("cs_", h, "_binned_eventstudy.csv")),
    show_col_types = FALSE
  )
  bjs_post <- read_csv(
    file.path(out_dir, paste0("bjs_", h, "_post_bins.csv")),
    show_col_types = FALSE
  )
  bjs_pre <- read_csv(
    file.path(out_dir, paste0("bjs_", h, "_pre_bins.csv")),
    show_col_types = FALSE
  )

  # cohort composition of BJS pre bins (not-yet-treated rows per lead bin)
  stacked <- readRDS(file.path(out_dir, "stacked_dat.rds"))
  bjs_pre_coh <- stacked |>
    filter(g_bjs > 0, DHSYEAR < g_bjs) |>
    mutate(
      lead = DHSYEAR - g_bjs,
      bin = as.character(cut(
        lead,
        breaks = c(-Inf, -9, -7, -5, -3, -1),
        labels = c("<=-9", "-8:-7", "-6:-5", "-4:-3", "-2:-1"),
        right = TRUE
      ))
    ) |>
    group_by(bin) |>
    summarise(n_cohorts = n_distinct(g_eff), .groups = "drop")

  cs <- cs |>
    transmute(
      bin,
      estimator = "C&S",
      estimate,
      std.error,
      single_cohort = !grepl(";", cohorts),
      pas = pas_represented
    ) |>
    mutate(bin = factor(bin, levels = bin_levels))
  # empty C&S bin(s) kept visible with NA
  missing_cs <- bin_levels[!bin_levels %in% as.character(cs$bin)]

  bjs <- bind_rows(
    bjs_pre |>
      left_join(bjs_pre_coh, by = "bin") |>
      transmute(
        bin,
        estimator = "BJS",
        estimate,
        std.error,
        single_cohort = n_cohorts == 1,
        pas = NA_integer_
      ),
    bjs_post |>
      transmute(
        bin,
        estimator = "BJS",
        estimate,
        std.error,
        single_cohort = !grepl(";", cohorts),
        pas = treated_pas
      )
  ) |>
    mutate(bin = factor(bin, levels = bin_levels))

  bind_rows(cs, bjs) |>
    mutate(
      conf_low = estimate - qnorm(0.975) * std.error,
      conf_high = estimate + qnorm(0.975) * std.error
    )
}

es_h1 <- es_data("h1")
es_h2 <- es_data("h2")

fig_eventstudy <- function(es, title) {
  xsupport <- es |>
    filter(!is.na(pas), as.integer(bin) >= 6) |>
    group_by(bin) |>
    summarise(pas = paste(unique(pas), collapse = "/"), .groups = "drop")
  single_bins <- es |>
    filter(!is.na(single_cohort), single_cohort) |>
    pull(bin) |>
    unique()
  yrange <- range(es$conf_low, es$conf_high, na.rm = TRUE)
  brk <- c(-Inf, -9, -7, -5, -3, -1, 1, 3, 5, 7, 9, Inf)
  ggplot(
    es,
    aes(
      bin,
      estimate,
      colour = estimator,
      shape = single_cohort,
      group = estimator
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = 4.5, linetype = "dashed", colour = "grey40") +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      width = 0.12,
      position = position_dodge(width = 0.5),
      na.rm = TRUE
    ) +
    geom_point(
      size = 2.2,
      position = position_dodge(width = 0.5),
      na.rm = TRUE
    ) +
    scale_shape_manual(
      values = c(`TRUE` = 17, `FALSE` = 16),
      labels = c(`TRUE` = "single annual cohort", `FALSE` = "multiple cohorts"),
      name = NULL,
      drop = FALSE
    ) +
    annotate(
      "text",
      x = 1.2,
      y = yrange[2] - 0.05 * diff(yrange),
      label = "no estimable C&S cells (<=-9): no pre-2009\ncohort wave has an earlier base period",
      size = 2.8,
      hjust = 0,
      colour = "grey30"
    ) +
    geom_text(
      data = xsupport,
      inherit.aes = FALSE,
      aes(
        x = bin,
        y = yrange[1] - 0.13 * diff(yrange),
        label = paste0(pas, " PAs")
      ),
      size = 2.6,
      colour = "grey30"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.22, 0.06))) +
    labs(
      x = "event-time bin (years relative to annual cohort g_eff)",
      y = title,
      caption = "95% pointwise CIs; dashed line = treatment boundary. C&S: multiplier bootstrap clustered at enumeration clusters. BJS: district-clustered."
    ) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(), legend.position = "top")
}

p_es1 <- fig_eventstudy(es_h1, "wealth centile (rural, weighted)")
p_es2 <- fig_eventstudy(es_h2, "wealth z-score")

ggsave(
  file.path(out_dir, "h1_eventstudy_cs_vs_bjs.png"),
  p_es1,
  width = 9.5,
  height = 5.2,
  dpi = 300
)
ggsave(
  file.path(out_dir, "h2_eventstudy_cs_vs_bjs.png"),
  p_es2,
  width = 9.5,
  height = 5.2,
  dpi = 300
)

# ---------------------------------------------------------------------
# overall comparison figures
# ---------------------------------------------------------------------
fig_overall <- function(tab, title) {
  tab |>
    mutate(
      estimand = fct_relevel(
        estimand,
        "Frozen 2x2 endpoint (2008 vs 2021), treat_post",
        "C&S simple overall ATT (annual cohorts)",
        "C&S calendar-2021 ATT",
        "BJS overall ATT, district FE (primary)",
        "BJS overall ATT, region FE (robustness)"
      )
    ) |>
    ggplot(aes(estimand, estimate, colour = estimand)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.25) +
    geom_point(size = 2.5) +
    scale_x_discrete(labels = function(x) {
      paste0(
        c("2x2", "C&S simple", "C&S cal-2021", "BJS district", "BJS region")
      )
    }) +
    labs(x = NULL, y = title, colour = NULL) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "none"
    )
}

ggsave(
  file.path(out_dir, "h1_overall_comparison.png"),
  fig_overall(tab_h1, "wealth centile (rural, weighted)"),
  width = 8,
  height = 4.6,
  dpi = 300
)
ggsave(
  file.path(out_dir, "h2_overall_comparison.png"),
  fig_overall(tab_h2, "wealth z-score"),
  width = 8,
  height = 4.6,
  dpi = 300
)

# ---------------------------------------------------------------------
# support comparison figure
# ---------------------------------------------------------------------
sup_cs <- read_csv(
  file.path(out_dir, "cs_h1_binned_eventstudy.csv"),
  show_col_types = FALSE
) |>
  transmute(
    bin = factor(bin, bin_levels),
    estimator = "C&S",
    treated_hh,
    pas = pas_represented,
    clusters = NA_integer_
  )
sup_bjs <- read_csv(
  file.path(out_dir, "bjs_h1_post_bins.csv"),
  show_col_types = FALSE
) |>
  transmute(
    bin = factor(bin, bin_levels),
    estimator = "BJS",
    treated_hh,
    pas = treated_pas,
    clusters = treated_clusters
  )
sup <- bind_rows(sup_cs, sup_bjs) |>
  filter(!is.na(pas), bin %in% post_bins) |>
  mutate(bin = droplevels(bin))

p_sup <- ggplot(sup, aes(bin, pas, fill = estimator)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = pas),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 2.6
  ) +
  labs(
    x = "post-treatment event-time bin",
    y = "protected areas contributing",
    fill = NULL,
    title = "Post-treatment support: treated PAs per bin (identical rows, different estimands)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

ggsave(
  file.path(out_dir, "staggered_sample_support.png"),
  p_sup,
  width = 8,
  height = 4.6,
  dpi = 300
)

cat("Figures and comparison tables written.\n")
print(tab_h1, n = Inf, width = Inf)
print(tab_h2, n = Inf, width = Inf)
