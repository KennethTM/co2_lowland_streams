source("libs_and_funcs.R")

#Catchment delineation

p_raster <- paste0(getwd(), "/data/Skjern_basin_cond_p.tif")
src_raster <- paste0(getwd(), "/data/Skjern_basin_src.tif")
sites_kml <- paste0(getwd(), "/data/stream_sites.kml")
sites_sql <- paste0(getwd(), "/data/stream_sites.sqlite")

raster_crs <- st_crs(raster(p_raster))

st_read(sites_kml) %>% 
  st_zm() %>% 
  st_transform(raster_crs) %>% 
  select(Name) %>% 
  st_write(sites_sql)

sites_snap <- paste0(getwd(), "/data/stream_sites_snap.sqlite")

#Snap points to stream network along flow directions
taudem_snap <- paste0(taudem_path, "moveoutletstostrm",
                      " -p ", p_raster,
                      " -src ", src_raster,
                      " -o ", sites_sql,
                      " -om ", sites_snap)
system(taudem_snap)

stream_sites_snap <- st_read(sites_snap) %>% 
  st_transform(25832)

#st_write(stream_sites_snap, paste0(getwd(), "/data/stream_sites_snap_reproj.sqlite"), delete_dsn = TRUE)

#NESTER WATERSHED DELINEATION
#Delineate watershed draining to stream outlets
gw_raster <- paste0(getwd(), "/data/watersheds.tif")

taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                      " -p ", p_raster,
                      " -o ", sites_snap,
                      " -gw ", gw_raster)
system(taudem_gage)

#Raster to polygon
gw_vect <- paste0(getwd(), "/data/watersheds.sqlite")

polygonize <- paste0("pkpolygonize ",
                     " -i ", gw_raster,
                     " -m ", gw_raster,
                     " -o ", gw_vect)
system(polygonize)

#Clean polygons
gw_clean <- st_read(gw_vect) %>% 
  st_transform(25832) %>% 
  st_make_valid() %>% 
  group_by(dn) %>% 
  summarise() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_remove_holes() %>% 
  rename(id = dn) %>% 
  st_join(stream_sites_snap) %>% 
  mutate(nested_area = as.numeric(st_area(geom)))

#st_write(gw_clean, paste0(getwd(), "/data/gw_clean.sqlite"), delete_dsn = TRUE)

#NON-NESTER WATERSHED DELINEATION
sites_snap_raw <- st_read(sites_snap)
for(i in 1:nrow(sites_snap_raw)){
  
  print(paste0("Delineating stream watershed ", i))
  
  st_write(sites_snap_raw[i, ], paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".sqlite"))
  
  #Delineate watershed draining to lake boundary points
  taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                        " -p ", p_raster,
                        " -o ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".sqlite"),
                        " -gw ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"))
  system(taudem_gage)
  
  polygonize <- paste0("pkpolygonize ",
                       " -i ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"),
                       " -m ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"),
                       " -o ", paste0(getwd(), "/data/watershed_tmp/gw_", i, ".sqlite"))
  system(polygonize)
  
  file.remove(paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".sqlite"))
  file.remove(paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"))
}

###

#Load and clean all non-nested watershed files
gw_nonnest_list <- lapply(1:nrow(sites_snap_raw), function(i){
  st_read(paste0(getwd(), "/data/watershed_tmp/gw_", i, ".sqlite")) %>% 
    st_transform(25832) %>% 
    st_make_valid() %>% 
    st_union() %>% 
    st_as_sf() %>% 
    st_cast("MULTIPOLYGON") %>% 
    st_remove_holes() %>% 
    mutate(id = i,
           total_area = as.numeric(st_area(geom)))
})

gw_nonnest_clean <- do.call(what = sf:::rbind.sf, args = gw_nonnest_list) %>% 
  left_join(st_drop_geometry(gw_clean[, c("id", "name")]))

#st_write(gw_nonnest_clean, paste0(getwd(), "/data/gw_nonnest_clean.sqlite"), delete_dsn=TRUE)

#Plot
dk_border_raw <- raster::getData("GADM", country = "DNK", level = 0, path = paste0(getwd(), "/data"))
dk_border <- dk_border_raw %>% 
  st_as_sf() %>% 	
  st_crop(xmin = 8, ymin = 54.56, xmax = 14, ymax = 57.76) %>% 	
  st_transform(25832)	

xlabs <- seq(8, 12, 1)
ylabs <- seq(54.5, 57.5, 0.5)

col_pal <- RColorBrewer::brewer.pal(8, "Dark2")

ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(data = gw_clean, aes(fill=factor(id, levels = id)), show.legend = FALSE, col = NA)+
  geom_sf(data= stream_sites_snap)+
  scale_fill_manual(values=c(rep(col_pal, 4), col_pal[1:3]))+
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))


#Fix Ryå - løber ud forkert sted, 10 m raster? evt tjek i qgis