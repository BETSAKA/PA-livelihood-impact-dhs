# =====================================================================
# Précalcul du SPEI annuel (échelle 12, référence 1981–2021) pour
# l'ensemble des grappes DHS — produit data/derived/spatial_covars_spei_staggered.rds
#
# Ce calcul (Hargreaves + SPEI mensuel, puis moyenne annuelle par
# grappe) prend plusieurs heures sur l'ensemble des grappes. Il est
# exécuté une fois pour toutes et NE doit PAS être relancé pendant un
# rendu ordinaire du livre : 02bis-spei-calculation.qmd lit et valide
# l'artefact existant.
#
# Régénération explicite (à lancer dans la console R, à la racine du
# projet) :
#   source("utils/spei_precompute.R")
#
# Garantie : le fichier de sortie est réécrit uniquement par cet
# appel explicite ; le schéma produit (colonnes DHSYEAR, DHSCLUST,
# spei_wc au format long {datetime, variable, unit, value}) est celui
# consommé par 04-variable-consolidation.qmd.
# =====================================================================

library(SPEI) # Calcul de l'indice SPEI
library(tidyverse) # Manipulation et visualisation des données
library(sf) # Analyse des données spatiales

stopifnot(
  "spatial_covars_staggered.rds introuvable : exécuter d'abord le précalcul spatial du chapitre 02" = file.exists(
    "data/derived/spatial_covars_staggered.rds"
  )
)

spatial_covars <- readRDS("data/derived/spatial_covars_staggered.rds")

compute_spei_annual <- function(tmin_tbl, tmax_tbl, prec_tbl, lat_deg) {
  # tmin_tbl/tmax_tbl/prec_tbl: tibbles avec colonnes `datetime` (Date) et `value` (num)
  d_tmin <- tibble(date = tmin_tbl$datetime, tmin = tmin_tbl$value)
  d_tmax <- tibble(date = tmax_tbl$datetime, tmax = tmax_tbl$value)
  d_prec <- tibble(date = prec_tbl$datetime, prec = prec_tbl$value)

  d_merged <- reduce(list(d_tmin, d_tmax, d_prec), left_join, by = "date") %>%
    arrange(date)

  d_clean <- drop_na(d_merged) # supprime lignes avec NA

  # Si séries trop courtes, renvoyer squelette 1981:2021 en NA
  if (nrow(d_clean) < 12) {
    return(tibble(year = 1981:2021, spei_mean = NA_real_))
  }

  # PET (Hargreaves), bilan hydrique et SPEI mensuel
  pet <- hargreaves(
    Tmin = d_clean$tmin,
    Tmax = d_clean$tmax,
    Pre = d_clean$prec,
    lat = lat_deg
  )

  wb <- d_clean$prec - pet

  wb_ts <- ts(
    wb,
    start = c(year(min(d_clean$date)), month(min(d_clean$date))),
    frequency = 12
  )

  spei_obj <- spei(
    wb_ts,
    scale = 12,
    ref.start = c(1981, 1),
    ref.end = c(2021, 12)
  )

  tibble(datetime = d_clean$date, spei = as.numeric(spei_obj$fitted)) %>%
    filter(
      datetime >= as.Date("1981-01-01"),
      datetime <= as.Date("2021-12-31")
    ) %>%
    mutate(year = year(datetime)) %>%
    group_by(year) %>%
    summarise(spei_mean = mean(spei, na.rm = TRUE), .groups = "drop") %>%
    complete(year = 1981:2021, fill = list(spei_mean = NA_real_))
}

quiet <- function(x) {
  tmp <- capture.output(res <- x, file = NULL)
  res
}

cat("Précalcul SPEI :", nrow(spatial_covars), "grappes — plusieurs heures.\n")

# Latitude géodésique depuis la géométrie (centroïde)
spatial_covars_spei <- spatial_covars %>%
  mutate(lat = st_coordinates(st_centroid(geometry))[, 2])

spatial_covars_spei <- spatial_covars_spei %>%
  mutate(
    spei_wc = pmap(
      list(temperature_min_wc, temperature_max_wc, precipitation_wc, lat),
      ~ quiet(compute_spei_annual(..1, ..2, ..3, ..4))
    )
  )

# Chaque élément spei_wc devient une table {datetime, variable, unit, value}
spatial_covars_spei <- spatial_covars_spei %>%
  mutate(
    spei_wc = map(
      spei_wc,
      ~ .x %>%
        mutate(
          datetime = as.Date(paste0(.data$year, "-01-01")),
          variable = "spei_scale12_mean",
          unit = "annual",
          value = .data$spei_mean
        ) %>%
        select(datetime, variable, unit, value)
    )
  )

spatial_covars_spei_df <- as.data.frame(spatial_covars_spei)
write_rds(
  spatial_covars_spei_df,
  "data/derived/spatial_covars_spei_staggered.rds"
)
cat(
  "SPEI (annuel, 1981–2021) enregistré dans data/derived/spatial_covars_spei_staggered.rds\n"
)
