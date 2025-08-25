library(wdpar)
library(tidyverse) #Manipulation et visualisation des données
library(sf) #Analyse des données spatiales
library(tmap) #Analyse cartographique
library(gt) #Mise en forme des tableaux 
library(geodata) # Pour avoir le contour de Madagascar
library(writexl) # Pour faire une sortie sous Excel
library(units)

# Chargement des données 
wdpa <- wdpa_read("data/raw/WDPA_WDOECM_Jul2025_Public_MDG.zip") %>%
  wdpa_clean() |> 
  filter(STATUS == "Designated") %>%
  mutate(MARINE = recode(MARINE,
                         "terrestrial" = "Terrestre",
                         "marine" = "Marine",
                         "partial" = "Mixte"))

# Vérifier le CRS actuel 
st_crs(wdpa)

# Reprojeter en UTM 38S (mètres)
wdpa_mada <- st_transform(wdpa, 32738)

# Calcul superficie en km² 
wdpa_mada$calc_area_km2 <- as.numeric(st_area(wdpa_mada))

# Comparaison avec REP_AREA et AREA_km²
wdpa_mada <- wdpa_mada %>%
  mutate(
    diff_rep_area = calc_area_km2 - REP_AREA,
    diff_area_km2 = calc_area_km2 - AREA_KM2
  )

# Créer un tableau contenant que les noms et les superficies des aires protégées 
wdpa_table <- wdpa_mada %>%
  mutate(
    diff_rep_area = calc_area_km2 - REP_AREA,
    diff_area_km2 = calc_area_km2 - AREA_KM2
  ) %>%
  select(ORIG_NAME, calc_area_km2, REP_AREA, AREA_KM2, diff_rep_area, diff_area_km2)


