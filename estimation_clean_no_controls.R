library(tidyverse)
library(fixest)
library(haven)

# Load
d97 <- read_rds("data/derived/data_matched_1997.rds") %>%
  rename(spei_wc_n_2 = spei_wc_1995,
         spei_wc_n_1 = spei_wc_1996,
         spei_wc_n = spei_wc_1997) %>%
  mutate(hv220 = zap_labels(hv220))
d08 <- read_rds("data/derived/data_matched_2008.rds") %>%
  rename(spei_wc_n_2 = spei_wc_2006,
         spei_wc_n_1 = spei_wc_2007,
         spei_wc_n = spei_wc_2008) %>%
  mutate(hv220 = zap_labels(hv220))
d21 <- read_rds("data/derived/data_matched_2021.rds")  %>%
  rename(spei_wc_n_2 = spei_wc_2019,
         spei_wc_n_1 = spei_wc_2020,
         spei_wc_n = spei_wc_2021) %>%
  mutate(hv220 = zap_labels(hv220))

# Stack
dat <- bind_rows(d97, d08, d21) %>%
  # keep only Treatment/Control
  filter(GROUP %in% c("Treatment", "Control")) %>%
  # treatment indicator
  mutate(treat = as.integer(GROUP == "Treatment"),
         weights = hv005 / 1e6 * weights)

# DID placebo
pre <- dat %>% 
  filter(DHSYEAR %in% c(1997, 2008)) %>%
  mutate(post = as.integer(DHSYEAR == 2008),
         treat_post  = treat * post)

f_pre <- wealth_centile_rural_simple ~ treat + post + treat_post 

m_pre <- feols(f_pre, data = pre, weights = ~ weights, cluster = ~ hv001)

# DID traitement
main <- dat %>% 
  filter(DHSYEAR %in% c(2008, 2021)) %>%
  mutate(post = as.integer(DHSYEAR == 2021),
         treat_post = treat * post)

f_main <- wealth_centile_rural_simple ~ treat + post + treat_post 

m_main <- feols(f_main, data = main, weights = ~ weights, cluster = ~ hv001)

etable(m_pre, m_main, vcov = list(~ hv001))
# Pour voir les résultats avec SE robustes pour l'heteroskadicité mais sans vcov
# etable(m_pre, m_main, vcov = list("each", "hc3", ~ hv001))


# extract DID row from a fixest summary
did_row <- function(model, year_post, vc = ~ hv001, term = "treat_post") {
  summary(model, vcov = vc) %>%
    broom::tidy() %>%
    filter(term == !!term) %>%
    transmute(year = year_post,
              estimate = estimate,
              se = std.error)
}

did_df <- bind_rows(did_row(m_pre,  year_post = 2008),
                    did_row(m_main, year_post = 2021)) %>%
  mutate(period = factor(ifelse(year == 2008, "1997–2008", "2008–2021"),
                         levels = c("1997–2008", "2008–2021")),
         lo = estimate - 1.96 * se,
         hi = estimate + 1.96 * se)

# plot
ggplot(did_df, aes(x = year, y = estimate, group = 1)) +
  geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.4, linewidth = 0.7) +
  geom_point(size = 3) +
 # geom_line(linewidth = 0.7, alpha = 0.8) +
  scale_x_continuous(breaks = c(2008, 2021)) +
  labs(
    x = "Year (post period of each contrast)",
    y = "DID coefficient on Wealth index centile",
    title = "DID effect over time with PSU-clustered 95% CIs",
    subtitle = 
      "Points at 2008 (placebo 1997–2008) and 2021 (treatment estimate 2008–2021)"
  ) 

# Pour voir les résultats avec SE robustes pour l'heteroskadicité mais sans vcov
# etable(m_pre, m_main, vcov = list("each", "hc3", ~ hv001))
# did_df_hc3 <- bind_rows(did_row(m_pre,  year_post = 2008, vc = "hc3"),
#                     did_row(m_main, year_post = 2021, vc = "hc3")) %>%
#   mutate(period = factor(ifelse(year == 2008, "1997–2008", "2008–2021"),
#                          levels = c("1997–2008", "2008–2021")),
#          lo = estimate - 1.96 * se,
#          hi = estimate + 1.96 * se)
# 
# ggplot(did_df_hc3, aes(x = year, y = estimate, group = 1)) +
#   geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed") +
#   geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.4, linewidth = 0.7) +
#   geom_point(size = 3) +
#   # geom_line(linewidth = 0.7, alpha = 0.8) +
#   scale_x_continuous(breaks = c(2008, 2021)) +
#   labs(
#     x = "Year (post period of each contrast)",
#     y = "DID coefficient on Wealth index centile",
#     title = "DID effect over time WITHOUT PSU-clustered CIs",
#     subtitle = 
#       "Points at 2008 (placebo 1997–2008) and 2021 (treatment estimate 2008–2021)"
#   )


# Testing Quantile treatment effects --------------------------------------
library(qte)

# Avec CiC -------------------

## Traitement

dat_2per <- dat %>%
  filter(DHSYEAR %in% c(2008, 2021)) %>%
  mutate(treat = as.integer(GROUP == "Treatment"))

cic_res <- CiC(
  formla = wealth_centile_rural_simple ~ treat,
  t = 2021, tmin1 = 2008, tname = "DHSYEAR",
  data = dat_2per,
  panel = FALSE, # repeated cross-sections
  se = TRUE, iters = 200, # bootstrap
  probs = seq(0.05, 0.95, 0.05)
)

summary(cic_res)
ggqte(cic_res) + labs(x="Quantiles", y="QTET", title="CiC QTET: 2008-2021")

## placebo

## Placebo: 1997 -> 2008
dat_placebo <- dat %>%
  filter(DHSYEAR %in% c(1997, 2008)) %>%
  mutate(treat = as.integer(GROUP == "Treatment"))

# (Optional) sanity check:
# with(dat_placebo, table(DHSYEAR, treat))

cic_pre <- CiC(
  formla = wealth_centile_rural_simple ~ treat,
  t = 2008, tmin1 = 1997, tname = "DHSYEAR",
  data = dat_placebo,
  panel = FALSE, # repeated cross-sections
  se = TRUE, iters = 200, # bootstrap
  probs = seq(0.05, 0.95, 0.05)
)

summary(cic_pre)

ggqte(cic_pre) +
  labs(x = "Quantiles", y = "QTET",
       title = "Placebo CiC QTET: 1997-2008")

