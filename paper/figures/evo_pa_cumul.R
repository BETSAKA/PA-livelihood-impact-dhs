# Évolution de la surface cumulée des aires protégées de Madagascar,
# par groupe "Strictes" (IUCN I-IV) vs "Non-strictes" (IUCN V-VI et
# non catégorisées), à partir du WDPA/WDOECM public (MDG).
# Sortie : paper/figures/evo_pa_cumul.png
# NB : à exécuter depuis la racine du projet.

library(dplyr)
library(tidyr)
library(ggplot2)

source("utils/iucn_sets.R")

# ---------------------------------------------------------------- import WDPA
wdpa <- wdpar::wdpa_read(
  "data/PA_creation/wdpa_data/WDPA_WDOECM_Aug2026_Public_MDG.zip"
) |>
  st_drop_geometry()

# Désignations nationales uniquement ; on retire les AP encore "Proposed"
# (jamais officialisées)
pa_nat <- wdpa |>
  filter(DESIG_TYPE == "National", STATUS != "Proposed")

# Garde-fou : aucune désignation internationale dans le filtre national
stopifnot(
  !any(grepl(
    "Ramsar|UNESCO|World Heritage Site",
    pa_nat$DESIG,
    ignore.case = TRUE
  ))
)

# Une AP nationale a STATUS_YR = 0 (année de création manquante dans le
# WDPA) : on l'exclut de la somme cumulée.
if (any(pa_nat$STATUS_YR == 0)) {
  message(
    "Exclusion de ",
    sum(pa_nat$STATUS_YR == 0),
    " AP nationale(s) sans année de création (STATUS_YR = 0) : ",
    paste(pa_nat$NAME[pa_nat$STATUS_YR == 0], collapse = ", ")
  )
  pa_nat <- filter(pa_nat, STATUS_YR != 0)
}

# ------------------------------------------------------- surface cumulée / an
# Surface terrestre (REP_AREA inclut la partie marine)
pa_creations <- pa_nat |>
  transmute(
    STATUS_YR,
    surface = REP_AREA - REP_M_AREA,
    groupe = ifelse(
      IUCN_CAT %in% iucn_sets$strict,
      "Strictes (I-IV)",
      "Non-strictes (V-VI)"
    )
  ) |>
  group_by(groupe, STATUS_YR) |>
  summarise(surface = sum(surface), .groups = "drop")

# Grille complète d'années (1927-2025) : somme cumulée année par année
pa_cumul <- expand_grid(
  annee = 1927:2025,
  groupe = c("Strictes (I-IV)", "Non-strictes (V-VI)")
) |>
  left_join(pa_creations, by = c("annee" = "STATUS_YR", "groupe")) |>
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
  "paper/figures/evo_pa_cumul.png",
  plot = p,
  width = 24,
  height = 14,
  units = "cm",
  dpi = 300
)
