library(did2s)

run_did2s_2per <- function(
  data,
  years,
  yvar,
  post_year = max(years),
  first_stage = "~ spei_wc_n_2 + spei_wc_n_1 + spei_wc_n + hv219 + hv220 | DHSYEAR",
  cluster_var = "hv001",
  weights = "w_all"
) {
  dat_sub <- data %>%
    filter(DHSYEAR %in% years) %>%
    mutate(treat_on = as.integer(GROUP == "Treatment" & DHSYEAR == post_year))

  m <- did2s(
    data = dat_sub,
    yname = yvar,
    first_stage = stats::as.formula(first_stage),
    second_stage = ~ i(treat_on, ref = FALSE), # yields "treat_on = 1" term
    treatment = "treat_on",
    cluster_var = cluster_var,
    weights = weights
  )

  td <- broom::tidy(m, conf.int = TRUE)

  # extract the treat_on coefficient row
  row <- td %>%
    filter(grepl("treat_on", term)) %>%
    select(term, estimate, conf.low, conf.high, std.error)

  # annotate
  row$period <- paste0(min(years), "–", max(years))
  row$model <- "did2s_2x2"
  row
}

# run for placebo and main
res_placebo <- run_did2s_2per(
  dat,
  years = c(1997, 2008),
  yvar = yvar,
  post_year = 2008
)
res_main <- run_did2s_2per(
  dat,
  years = c(2008, 2021),
  yvar = yvar,
  post_year = 2021
)

res_two <- bind_rows(res_placebo, res_main) %>%
  mutate(period = factor(period, levels = c("1997–2008", "2008–2021")))

# Plot side-by-side
ggplot(res_two, aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  labs(
    x = NULL,
    y = "ATT (did2s)",
    title = "2×2 DID: Placebo and Main comparisons"
  ) +
  theme_minimal(base_size = 13)
