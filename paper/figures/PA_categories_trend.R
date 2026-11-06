# Aires protégées terrestres de Madagascar : surface cumulée par année de
# création, par niveau de protection (IUCN I-IV strictes / V-VI multi-usages /
# non définies), à partir de la base dynamique dynamic_wdpa.gpkg.
#
# Différences par rapport à evo_pa_cumul.R / evo_pa_cumul_sapm.R :
# - surfaces GÉOMÉTRIQUES (intersection avec le contour terrestre national,
#   unions spatiales hiérarchiques : les recouvrements entre AP sont comptés
#   une seule fois), et non surfaces déclarées sommées ;
# - une seule AP par WDPAID (dernier état de la base dynamique) ;
# - année de création = min(STATUS_YR > 0) au fil des états ;
# - statut "Designated" uniquement (ni Proposed, ni Inscribed) ;
# - 3 groupes : "Not defined" est distingué des multi-usages.
#
# Reprend le code de PA_categories_Trend.qmd, nettoyé et exécutable depuis
# la racine du projet. Sorties : paper/figures/outputs/*.png

library(tidyverse)
library(sf)
library(scales)
library(geodata)

# ------------------------------------------------------------- paramètres
annee_debut <- 1920
annee_fin <- 2025

standard_crs <- 4326
mdg_crs <- 29702

categories <- c("I", "II", "III", "IV", "V", "VI", "Not defined")

# Palette en niveaux de gris (I la plus sombre, "Not defined" en blanc),
# pour une impression noir & blanc de la version par catégorie UICN.
palette_couleur <- c(
  "I" = "#262626",
  "II" = "#4D4D4D",
  "III" = "#737373",
  "IV" = "#999999",
  "V" = "#BFBFBF",
  "VI" = "#E0E0E0",
  "Not defined" = "#FFFFFF"
)

cat_strict <- c("I", "II", "III", "IV")
cat_multi <- c("V", "VI")

groupes <- c(
  "Strict categories (IUCN I–IV)",
  "Multi-use categories (IUCN V–VI)",
  "Not defined"
)

palette_nb <- c(
  "Strict categories (IUCN I–IV)" = "#404040",
  "Multi-use categories (IUCN V–VI)" = "#B0B0B0",
  "Not defined" = "#FFFFFF"
)

enquetes <- tribble(
  ~annee , ~type  ,
    1997 , "DHS"  ,
    2008 , "DHS"  ,
    2011 , "MIS"  ,
    2013 , "MIS"  ,
    2016 , "MIS"  ,
    2018 , "MICS" ,
    2021 , "DHS"
) %>%
  mutate(type = factor(type, levels = c("DHS", "MIS", "MICS")))

# ---------------------------------------------------------------- données
dynamic_wdpa <- st_read("data/dynamic_wdpa.gpkg", quiet = TRUE) %>%
  st_transform(standard_crs)

# Contour national (adm0) en arrière-plan
contour_mada0 <- geodata::gadm(
  country = "Madagascar",
  level = 0,
  path = "data/raw/gadm"
) %>%
  st_as_sf()

# Une seule AP par WDPAID : année de création = premier STATUS_YR non nul
# connu, limites = dernier état de la base dynamique.
pa_sf <- dynamic_wdpa %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  mutate(
    annee_creation = as.integer(STATUS_YR),
    cat_iucn = case_when(
      IUCN_CAT %in% c("Ia", "Ib") ~ "I",
      IUCN_CAT %in% c("II", "III", "IV", "V", "VI") ~ as.character(IUCN_CAT),
      TRUE ~ "Not defined"
    ),
    groupe = case_when(
      cat_iucn %in% cat_strict ~ groupes[1],
      cat_iucn %in% cat_multi ~ groupes[2],
      TRUE ~ groupes[3]
    )
  ) %>%
  group_by(WDPAID) %>%
  mutate(
    annee_creation = suppressWarnings(
      min(annee_creation[annee_creation > 0], na.rm = TRUE)
    )
  ) %>%
  arrange(desc(valid_from), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  filter(
    STATUS == "Designated",
    DESIG_TYPE == "National",
    !str_detect(
      DESIG_ENG,
      regex("Ramsar|World Heritage|Biosphere", ignore_case = TRUE)
    ),
    MARINE %in% c("0", "1"),
    is.finite(annee_creation),
    between(annee_creation, 1900, annee_fin)
  ) %>%
  st_transform(standard_crs) %>%
  st_make_valid()

# Restriction au terrestre (intersection avec le contour national)
land_mdg <- contour_mada0 %>%
  st_transform(mdg_crs) %>%
  st_make_valid() %>%
  st_union()

pa_mdg <- pa_sf %>%
  st_transform(mdg_crs) %>%
  st_make_valid() %>%
  st_intersection(land_mdg) %>%
  st_collection_extract("POLYGON") %>%
  filter(!st_is_empty(st_geometry(.))) %>%
  st_make_valid()

# ---------------------------------------------- surface cumulée par année
# À chaque année de création : union spatiale par niveau, moins ce qui est
# déjà couvert par les niveaux précédents (pas de double comptage).
vide <- function() st_sfc(st_polygon(), crs = mdg_crs)
aire_km2 <- function(g) as.numeric(sum(st_area(g))) / 1e6

surface_hierarchique <- function(data, colonne, niveaux) {
  annees_chg <- sort(unique(pmax(data$annee_creation, annee_debut)))
  annees_chg <- annees_chg[annees_chg <= annee_fin]

  map_dfr(annees_chg, function(y) {
    d <- data %>% filter(annee_creation <= y)
    deja <- vide()
    res <- numeric(length(niveaux))

    for (i in seq_along(niveaux)) {
      di <- d %>% filter(.data[[colonne]] == niveaux[i])
      if (nrow(di) == 0) {
        next
      }
      u <- st_union(di)
      res[i] <- aire_km2(st_difference(u, deja))
      deja <- st_union(c(deja, u))
    }
    tibble(annee = y, niveau = niveaux, surface_km2 = res)
  }) %>%
    complete(annee = annee_debut:annee_fin, niveau = niveaux) %>%
    group_by(niveau) %>%
    arrange(annee, .by_group = TRUE) %>%
    fill(surface_km2, .direction = "down") %>%
    replace_na(list(surface_km2 = 0)) %>%
    ungroup() %>%
    mutate(niveau = factor(niveau, levels = rev(niveaux)))
}

surface_cat <- surface_hierarchique(pa_mdg, "cat_iucn", categories)
surface_grp <- surface_hierarchique(pa_mdg, "groupe", groupes)

# ---------------------------------------------------------------- graphes
graphique <- function(
  data,
  palette,
  titre_legende,
  noir_blanc = FALSE,
  xmin = annee_debut,
  xmax = annee_fin,
  legend_box = "horizontal"
) {
  theme_base <- if (noir_blanc) {
    theme_bw(base_size = 11)
  } else {
    theme_minimal(base_size = 11)
  }

  ggplot(data, aes(x = annee, y = surface_km2, fill = niveau)) +
    geom_area(
      position = "stack",
      alpha = if (noir_blanc) 1 else 0.9,
      color = if (noir_blanc) "black" else NA,
      linewidth = 0.2
    ) +
    geom_vline(
      data = enquetes,
      aes(xintercept = annee, linetype = type),
      color = "black",
      linewidth = 0.5,
      show.legend = c(fill = FALSE, linetype = TRUE)
    ) +
    scale_fill_manual(
      values = palette,
      breaks = names(palette),
      drop = FALSE,
      name = titre_legende
    ) +
    scale_linetype_manual(
      values = c(DHS = "solid", MIS = "dashed", MICS = "dotted"),
      name = "Surveys"
    ) +
    scale_x_continuous(
      name = "Year of creation",
      breaks = unique(c(xmin, seq(1930, 2020, by = 10))),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(
      name = expression("Cumulative terrestrial protected area (km"^2 * ")"),
      labels = label_comma(),
      expand = expansion(mult = c(0, 0.05))
    ) +
    # expand = FALSE : sinon coord_cartesian réajoute une expansion de 5 %
    # qui annule la marge à gauche (début de la série visible en 1927)
    coord_cartesian(xlim = c(xmin, xmax), expand = FALSE) +
    guides(
      fill = guide_legend(order = 1, nrow = 1),
      linetype = guide_legend(
        order = 2,
        nrow = 1,
        keywidth = unit(1.2, "cm"),
        override.aes = list(color = "black", linewidth = 0.6)
      )
    ) +
    theme_base +
    theme(
      legend.position = "bottom",
      legend.box = legend_box,
      legend.title = element_text(face = "bold"),
      legend.key = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = if (noir_blanc) "black" else "grey20"),
      axis.title = element_text(color = "black")
    )
}

dir.create("paper/figures/outputs", showWarnings = FALSE, recursive = TRUE)

graph_couleur <- graphique(
  surface_cat,
  palette_couleur,
  "IUCN Category",
  noir_blanc = TRUE
)
# xmin = 1920 : laisse voir le début de la série en 1927 ;
# légende sur deux lignes (boîte verticale) pour éviter le rognage.
graph_nb <- graphique(
  surface_grp,
  palette_nb,
  "Protection level",
  noir_blanc = TRUE,
  xmin = 1920,
  legend_box = "vertical"
)

graph_couleur
graph_nb

ggsave(
  "paper/figures/outputs/cumulative_protected_area_by_IUCN.png",
  plot = graph_couleur,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  "paper/figures/outputs/cumulative_protected_area_black_white.png",
  plot = graph_nb,
  width = 10,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)
