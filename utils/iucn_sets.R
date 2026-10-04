# Classification des aires protégées strictes vs multi-usages
# Source commune partagée par 01-treatment-assignation.qmd et
# 07-estimation_staggered.qmd. Toute modification de la classification
# doit se faire ici uniquement.

iucn_sets <- list(
  strict = c("Ia", "Ib", "II", "III", "IV"),
  multi  = c("V", "VI")
)

is_strict <- function(x) x %in% iucn_sets$strict
is_multi  <- function(x) x %in% iucn_sets$multi
