# Évolution de la surface cumulée des aires protégées de Madagascar
# (réseau SAPM, base Ministère de l'Environnement 2017), par groupe
# "Strictes (I-IV)" vs "Non-strictes (V-VI)".
# Mise en forme identique à evo_pa_cumul.R (version WDPA).
# Sortie : paper/figures/evo_pa_cumul_sapm.png
# NB : à exécuter depuis la racine du projet.

library(dplyr)
library(tidyr)
library(ggplot2)

source("utils/iucn_sets.R")

# ------------------------------------------------------------------ import SAPM
sapm <- readRDS("data/PA_creation/sapm_2017.rds")

# NB : la base SAPM note "I" la catégorie que le WDPA décline en "Ia"/"Ib"
# (iucn_sets.R est calé sur la nomenclature WDPA) => on ajoute "I".
strict_sapm <- c(iucn_sets$strict, "I")

sapm <- sapm |>
  mutate(
    annee = as.integer(format(as.Date(DATE_CREAT), "%Y")),
    surface = HECTARES / 100, # ha -> km²
    groupe = ifelse(
      CATEG_IUCN %in% strict_sapm,
      "Strictes (I-IV)",
      "Non-strictes (V-VI)" # inclut "Non définie"
    )
  )

# ------------------------------------------------------- surface cumulée / an
pa_creations <- sapm |>
  st_drop_geometry() |>
  group_by(groupe, annee) |>
  summarise(surface = sum(surface), .groups = "drop")

# Grille complète d'années (1927-2025) : somme cumulée année par année
pa_cumul <- expand_grid(
  annee = 1927:2025,
  groupe = c("Strictes (I-IV)", "Non-strictes (V-VI)")
) |>
  left_join(pa_creations, by = c("annee", "groupe")) |>
  mutate(surface = replace_na(surface, 0)) |>
  group_by(groupe) |>
  arrange(annee, .by_group = TRUE) |>
  mutate(cumul = cumsum(surface)) |>
  ungroup() |>
  mutate(
    groupe = factor(
      groupe,
      levels = c("Strictes (I-IV)", "Non-strictes (V-VI)")
    )
  )

# --------------------------------------------------------------- vagues d'enquête
vagues <- data.frame(
  annee = c(1997, 2008, 2021, 2011, 2013, 2016, 2018),
  enquete = c("DHS", "DHS", "DHS", "MIS", "MIS", "MIS", "MICS")
)

ymax <- max(pa_cumul$cumul)

p <- ggplot(pa_cumul, aes(x = annee, y = cumul)) +
  geom_area(
    aes(fill = groupe),
    colour = "black",
    linewidth = 0.2,
    position = position_stack(reverse = TRUE),
    na.rm = TRUE
  ) +
  scale_fill_manual(
    values = c("Strictes (I-IV)" = "grey30", "Non-strictes (V-VI)" = "grey80")
  ) +
  geom_vline(
    data = vagues,
    aes(xintercept = annee, linetype = enquete),
    linewidth = 0.5
  ) +
  scale_linetype_manual(
    values = c(DHS = "solid", MIS = "longdash", MICS = "dotted")
  ) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE)
  ) +
  scale_x_continuous(
    breaks = seq(1940, 2020, by = 20),
    limits = c(1927, 2025)
  ) +
  labs(
    x = "Année de création",
    y = "Surface cumulée (km\u00b2)",
    fill = "Catégorie UICN",
    linetype = "Enquêtes"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

p

ggsave(
  "paper/figures/evo_pa_cumul_sapm.png",
  plot = p,
  width = 24,
  height = 14,
  units = "cm",
  dpi = 300
)
