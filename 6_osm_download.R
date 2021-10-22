#Download dk border data using Raster package
library(raster);library(sf)
dk_border_raw <- raster::getData("GADM", country = "DNK", level = 0, path = paste0(getwd(), "/data"))
dk_border <- dk_border_raw %>%
  st_as_sf() %>%
  st_crop(xmin = 8, ymin = 54.56, xmax = 14, ymax = 57.76) %>%
  st_transform(25832) %>%
  st_write(paste0(getwd(), "/data/dk_border.sqlite"))

#Download of OSM data for figures
library(osmdata);library(sf)

#Lake Arre/River Pøle network
inset_sites <- st_read(paste0(getwd(), "/data/lake_arre_network.kml")) %>% 
  add_column(name = as.character(c(6, 5, 3, 4, 2, 1)))

inset_sites_bbox <- c("xmin" = 12.025, "xmax" = 12.275, 
                      "ymin" = 55.93, "ymax" = 56.01)

q <- opq(bbox = c(inset_sites_bbox[["xmin"]],inset_sites_bbox[["ymin"]],
                  inset_sites_bbox[["xmax"]], inset_sites_bbox[["ymax"]])) %>%
  add_osm_feature("water") %>%
  add_osm_feature("waterway")

q_sf <- osmdata_sf(q)
saveRDS(q_sf, paste0(getwd(), "/data/arresø_network_osm.rds"))

#Mølleå network
mølleå <- st_read(paste0(getwd(), "/data/mølleå.kml"))

mølleå_bbox <- st_bbox(mølleå)

mølleå_q <- opq(bbox = c(mølleå_bbox[["xmin"]],mølleå_bbox[["ymin"]],
                  mølleå_bbox[["xmax"]], mølleå_bbox[["ymax"]])) %>%
  add_osm_feature("water") %>%
  add_osm_feature("waterway")

mølleå_q_sf <- osmdata_sf(mølleå_q)
saveRDS(mølleå_q_sf, paste0(getwd(), "/data/mølleå_osm.rds"))

#upper Gudenå network
upper_gudenå <- st_read(paste0(getwd(), "/data/upper_gudenå.kml"))

upper_gudenå_bbox <- st_bbox(upper_gudenå)

upper_gudenå_q <- opq(bbox = c(upper_gudenå_bbox[["xmin"]],upper_gudenå_bbox[["ymin"]],
                               upper_gudenå_bbox[["xmax"]], upper_gudenå_bbox[["ymax"]])) %>%
  add_osm_feature("water") %>%
  add_osm_feature("waterway")

upper_gudenåq_sf <- osmdata_sf(upper_gudenå_q)
saveRDS(upper_gudenåq_sf, paste0(getwd(), "/data/upper_gudenå_osm.rds"))
