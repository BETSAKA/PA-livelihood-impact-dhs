library(wdpar)
library(tidyverse) #Manipulation et visualisation des données
library(sf) #Analyse des données spatiales
library(tmap) #Analyse cartographique
library(gt) #Mise en forme des tableaux 
library(geodata) # Pour avoir le contour de Madagascar
library(writexl) # Pour faire une sortie sous Excel
library(units)

# Systèmes de coordonnées de référence 
standard_crs <- 4326
mdg_crs <- 29702 

# SAPM 2017---------------------------------------------------------
sapm_2017 <- st_read("data/raw/SAPM_2017/SAPM_2017_corrected.shp") |> # en EPSG 9001
  st_transform(standard_crs) %>%
  st_make_valid() %>%
  mutate(YEAR_CREAT = year(ymd(DATE_CREAT)), .after = DATE_CREAT)

# Visualisation
tmap_mode("view")
tm_shape(sapm_2017) +
  tm_polygons(fill =  "green", 
              id = "FULL_NAME",
              popup.vars = c("Superficie (ha)" = "HECTARES")) +
  tm_scalebar(position = c("left", "bottom")) + 
  tmap_options(check_and_fix = TRUE) +
  tm_title("Aires protégées de Madagascar") +
  tm_layout(legend.outside = TRUE)

# SAPM 2023-----------------------------------------------------------
sapm_2023 <- st_read("data/raw/SAPM_2023/dernier version shape_ap_28042023.shx") |> # en EPSG 9001
  st_transform(standard_crs) %>%
  st_make_valid() %>%
  mutate(YEAR_CREAT = year(ymd(DATE_CREAT)), .after = DATE_CREAT)

# WDPA (mise à jour 2025)---------------------------------------------------
wdpa <- wdpa_read("data/raw/WDPA_WDOECM_Jul2025_Public_MDG.zip") %>%
  wdpa_clean() |> 
  filter(STATUS == "Designated") %>%
  mutate(MARINE = recode(MARINE,
                         "terrestrial" = "Terrestre",
                         "marine" = "Marine",
                         "partial" = "Mixte"))

# Vérifier si les 3 dataframes sont identiques 

identiques <- identical(sapm_2017, sapm_2023) && identical(sapm_2023, wdpa)
print(paste("Tous identiques : ", identiques))






