library(tidyverse)
library(haven)
library(psych)
library(survey)

# Utilitaires

perform_pca <- function(data, vars) {
  keep <- intersect(vars, names(data))
  stopifnot(length(keep) >= 3)
  cor_matrix <- cor(dplyr::select(data, all_of(keep)), use = "pairwise.complete.obs")
  principal(cor_matrix, nfactors = 1, rotate = "none", scores = TRUE, covar = FALSE)
}

remove_zero_var <- function(data, var_list) {
  keep <- intersect(var_list, names(data))
  if (length(keep) == 0) return(character(0))
  zv <- data |>
    dplyr::select(all_of(keep)) |>
    summarise(across(everything(), ~ var(.x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "v", values_to = "var") |>
    filter(!is.finite(var) | var == 0) |>
    pull(v)
  setdiff(keep, zv)
}

# ton add_centile_survey tel quel
add_centile_survey <- function(df, value_col, weight_col = "hv005", cluster_col = "hv001", new_col = "centile") {
  val <- rlang::sym(value_col); wgt <- rlang::sym(weight_col)
  df <- df %>% mutate(.weight_survey = if (weight_col == "hv005") !!wgt / 1e6 else !!wgt)
  design <- svydesign(ids = as.formula(paste0("~", cluster_col)), weights = ~.weight_survey, data = df)
  thresholds <- svyquantile(as.formula(paste0("~", value_col)), design,
                            quantiles = seq(0.01, 1, 0.01), ci = FALSE, na.rm = TRUE)
  thresholds <- as.numeric(thresholds[[1]]) + seq_along(thresholds[[1]]) * 1e-10
  df[[new_col]] <- cut(df[[value_col]], breaks = c(-Inf, thresholds),
                       labels = 1:100, include.lowest = TRUE, right = TRUE) |> as.integer()
  df
}

# Recodage "comme en 2008"

recode_household_recodeVI <- function(hh) {
  hh %>%
    filter(hv015 == 1) %>%
    mutate(
      # Eau
      h2oires = as.integer(hv201 == 11),
      h2oyard = as.integer(hv201 == 12),
      h2opub  = as.integer(hv201 == 13),
      h2owell = as.integer(hv201 == 21),
      h2opwell= as.integer(hv201 == 31),
      h2ouwell= as.integer(hv201 == 32),
      h2osurf = as.integer(hv201 %in% c(43, 41, 42)),    # 2011/13: surface+sources non protégées
      h2ooth  = as.integer(hv201 > 50 & hv201 < 97),
      
      # Toilettes (+ partage hv225)
      flushp  = as.integer((hv205 %in% 11:14) & hv225 == 0),
      flushs  = as.integer((hv205 %in% 11:14) & hv225 == 1),
      latvipp = as.integer(hv205 == 21 & hv225 == 0),
      latvips = as.integer(hv205 == 21 & hv225 == 1),
      latslbp = as.integer(hv205 %in% c(22, 23) & hv225 == 0),
      latslbs = as.integer(hv205 %in% c(22, 23) & hv225 == 1),
      latopp  = as.integer(hv205 == 24 & hv225 == 0),
      latops  = as.integer(hv205 == 24 & hv225 == 1),
      latbush = as.integer(hv205 == 31),
      latothp = as.integer((hv205 %in% 41:43) & hv225 == 0),
      latoths = as.integer((hv205 %in% 41:43) & hv225 == 1),
      
      # Biens
      electric= as.integer(hv206 == 1),
      radio   = as.integer(hv207 == 1),
      tv      = as.integer(hv208 == 1),
      fridge  = as.integer(hv209 == 1),
      bicycle = as.integer(hv210 == 1),
      motobk  = as.integer(hv211 == 1),
      car     = as.integer(hv212 == 1),
      mphone  = as.integer(ifelse("hv243a" %in% names(.), hv243a == 1, 0)),
      watch   = as.integer(ifelse("hv243b" %in% names(.), hv243b == 1, 0)),
      bank    = as.integer(ifelse("hv247"  %in% names(.), hv247  == 1, 0)),
      
      # Sol
      dirtfloo = as.integer(hv213 %in% c(11, 12)),
      plnkfloo = as.integer(hv213 == 21),
      palmfloo = as.integer(hv213 == 22),
      matfloo  = as.integer(hv213 %in% c(23, 96)),
      parqfloo = as.integer(hv213 == 31),
      vinfloo  = as.integer(hv213 == 32),
      tilefloo = as.integer(hv213 == 33),
      cemtfloo = as.integer(hv213 == 34),
      carpfloo = as.integer(hv213 == 35),
      
      # Combustible cuisine
      cookelec = as.integer(hv226 == 1),
      cooklpg  = as.integer(hv226 %in% c(2,3,4,5)),
      cookcoal = as.integer(hv226 %in% c(6,7)),
      cookwood = as.integer(hv226 == 8),
      cookstrw = as.integer(hv226 %in% c(9,10,11)),
      cooknone = as.integer(hv226 %in% c(95,96)),
      
      # Murs
      grnwall  = as.integer(hv214 == 12),
      dirtwall = as.integer(hv214 %in% c(11,13)),
      bamwall  = as.integer(hv214 %in% c(21,22)),
      rwdwall  = as.integer(hv214 %in% c(23,24,25,26)),
      cmtwall  = as.integer(hv214 == 31),
      stncwall = as.integer(hv214 == 32),
      brckwall = as.integer(hv214 == 33),
      blckwall = as.integer(hv214 == 34),
      woodwall = as.integer(hv214 == 36),
      othwall  = as.integer(hv214 == 96),
      
      # Toits
      natroof  = as.integer(hv215 %in% c(11,12)),
      sodroof  = as.integer(hv215 == 13),
      rudroof  = as.integer(hv215 %in% c(21,22,24)),
      plnkroof = as.integer(hv215 == 23),
      ironroof = as.integer(hv215 %in% c(31,95,96)),
      woodroof = as.integer(hv215 == 32),
      cemtroof = as.integer(hv215 %in% c(33,35,36)),
      tileroof = as.integer(hv215 == 34),
      
      # Densité sommeil
      hv216 = if_else(is.na(hv216) | hv216 == 0, hv012, hv216),
      memsleep = hv012 / hv216,
      
      # pondérations
      wt      = hv005/1e6
    ) %>%
    # Élevage: si hv246 != 1 -> 0 (comme tu fais)
    mutate(across(all_of(intersect(c("hv246a","hv246b","hv246c","hv246d","hv246e","hv246f","hv246g","hv246h"),
                                   names(.))),
                  ~ if_else(is.na(.x) | hv246 != 1, 0, .x))) %>%
    # NAs -> 0 pour les dummies
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))
}

vars_recodeVI <- c(
  # mêmes « dich_vars_2008 » que chez toi
  "h2oires","h2oyard","h2opub","h2owell","h2opwell","h2ouwell","h2osurf","h2ooth",
  "flushp","flushs","latvipp","latvips","latslbp","latslbs","latopp","latops","latbush","latothp","latoths",
  "electric","radio","tv","fridge","bicycle","motobk","car","mphone","watch","bank",
  "dirtfloo","plnkfloo","palmfloo","matfloo","parqfloo","vinfloo","tilefloo","cemtfloo","carpfloo",
  "cookelec","cooklpg","cookcoal","cookwood","cookstrw","cooknone",
  "grnwall","dirtwall","bamwall","rwdwall","cmtwall","stncwall","brckwall","blckwall","woodwall","othwall",
  "natroof","sodroof","rudroof","plnkroof","ironroof","woodroof","cemtroof","tileroof",
  "memsleep",
  # élevage (si présents)
  "hv246a","hv246b","hv246c","hv246d","hv246e","hv246f","hv246g","hv246h"
)

# 2011
hh_2011 <- read_dta("data/raw/dhs/DHS_2011/MDHR61DT/MDHR61FL.DTA")
hh_2011_rec <- recode_household_recodeVI(hh_2011)

urban_vars_2011 <- remove_zero_var(filter(hh_2011_rec, hv025 == 1), vars_recodeVI)
rural_vars_2011 <- remove_zero_var(filter(hh_2011_rec, hv025 == 2), vars_recodeVI)

pca_urb_2011 <- perform_pca(filter(hh_2011_rec, hv025 == 1), urban_vars_2011)
pca_rur_2011 <- perform_pca(filter(hh_2011_rec, hv025 == 2), rural_vars_2011)

hh_2011_rec <- hh_2011_rec %>%
  mutate(
    wealth_index_rural = if_else(hv025 == 2,
                                 as.vector(scale(dplyr::select(., all_of(rural_vars_2011))) %*%
                                             pca_rur_2011$loadings[,1]),
                                 NA_real_)
  )

hh_2011_rural <- hh_2011_rec %>%
  filter(hv025 == 2) %>%
  select(hv001, hv002, hv005, hv025, wealth_index_rural) %>%
  add_centile_survey("wealth_index_rural", new_col = "wealth_centile_rural_weighted") %>%
  mutate(wealth_centile_rural_simple = ntile(wealth_index_rural, 100))

# 2013
hh_2013 <- read_dta("data/raw/dhs/DHS_2013/MDHR6ADT/MDHR6AFL.DTA")
hh_2013_rec <- recode_household_recodeVI(hh_2013)

urban_vars_2013 <- remove_zero_var(filter(hh_2013_rec, hv025 == 1), vars_recodeVI)
rural_vars_2013 <- remove_zero_var(filter(hh_2013_rec, hv025 == 2), vars_recodeVI)

pca_urb_2013 <- perform_pca(filter(hh_2013_rec, hv025 == 1), urban_vars_2013)
pca_rur_2013 <- perform_pca(filter(hh_2013_rec, hv025 == 2), rural_vars_2013)

hh_2013_rec <- hh_2013_rec %>%
  mutate(
    wealth_index_rural = if_else(hv025 == 2,
                                 as.vector(scale(dplyr::select(., all_of(rural_vars_2013))) %*%
                                             pca_rur_2013$loadings[,1]),
                                 NA_real_)
  )

hh_2013_rural <- hh_2013_rec %>%
  filter(hv025 == 2) %>%
  select(hv001, hv002, hv005, hv025, wealth_index_rural) %>%
  add_centile_survey("wealth_index_rural", new_col = "wealth_centile_rural_weighted") %>%
  mutate(wealth_centile_rural_simple = ntile(wealth_index_rural, 100))



# On ajoute les z-scores
# Application 2011 — z-score à partir du centile pondéré rural
hh_2011_rural <- hh_2011_rural %>%
  add_zscore_from_centile("wealth_centile_rural_weighted", "hv001",
                          zscore_col = "zscore_centile_rural")

# Application 2013 — z-score à partir du centile pondéré rural
hh_2013_rural <- hh_2013_rural %>%
  add_zscore_from_centile("wealth_centile_rural_weighted", "hv001",
                          zscore_col = "zscore_centile_rural")

# Enregistrement

write_rds(hh_2011_rural, "data/derived/hh_2011_rural.rds")
write_rds(hh_2013_rural, "data/derived/hh_2013_rural.rds")
