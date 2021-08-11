#Download of OSM data for figure 5

library(osmdata);library(sf)

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
