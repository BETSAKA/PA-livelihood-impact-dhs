library(wdpar)

sapm_2017 <- st_read("data/raw/SAPM_2017/SAPM_2017_corrected.shp") |> # en EPSG 9001
  st_transform(standard_crs) %>%
  st_make_valid() %>%
  mutate(YEAR_CREAT = year(ymd(DATE_CREAT)), .after = DATE_CREAT)
# Visualisation
tmap_mode("view")
tm_shape(sapm) +
  tm_polygons(fill =  "green", 
              id = "FULL_NAME",
              popup.vars = c("Superficie (ha)" = "HECTARES")) +
  tm_scalebar(position = c("left", "bottom")) + 
  tmap_options(check_and_fix = TRUE) +
  tm_title("Aires protégées de Madagascar") +
  tm_layout(legend.outside = TRUE)


sapm_2023 <- st_read("data/raw/SAPM_2023/dernier version shape_ap_28042023.shx") |> # en EPSG 9001
  st_transform(standard_crs) %>%
  st_make_valid() %>%
  mutate(YEAR_CREAT = year(ymd(DATE_CREAT)), .after = DATE_CREAT)
# Visualisation
tmap_mode("view")
tm_shape(sapm) +
  tm_polygons(fill =  "green", 
              id = "FULL_NAME",
              popup.vars = c("Superficie (ha)" = "HECTARES")) +
  tm_scalebar(position = c("left", "bottom")) + 
  tmap_options(check_and_fix = TRUE) +
  tm_title("Aires protégées de Madagascar") +
  tm_layout(legend.outside = TRUE)



