source("libs_and_funcs.R")

#Flowdirs
p_raster <- paste0(getwd(), "/data/dhym_25m_breach_p.tif")
taudem_flowdir <- paste0(mpi_settings, taudem_path, "d8flowdir ",
                         " -p ", p_raster,
                         " -sd8 ", paste0(getwd(), "/data/dhym_25m_breach_sd8.tif"),
                         " -fel ", paste0(getwd(), "/data/dhym_25m_breach.tif"))
system(taudem_flowdir)

#Flow accumulation weighted by stream start and end points
taudem_acc <- paste0(mpi_settings, taudem_path, "aread8",
                     " -p ", paste0(getwd(), "/data/dhym_25m_breach_p.tif"),
                     " -ad8 ", paste0(getwd(), "/data/dhym_25m_breach_ad8.tif"),
                     " -nc")
system(taudem_acc)

#Threshold stream network
src_raster <- paste0(getwd(), "/data/dhym_25m_breach_src.tif")
taudem_threshold <- paste0(mpi_settings, taudem_path, "threshold",
                                " -src ", src_raster,
                                " -ssa ", paste0(getwd(), "/data/dhym_25m_breach_ad8.tif"),
                                " -thresh 500")
system(taudem_threshold)

#Catchment delineation
sites_kml <- paste0(getwd(), "/data/stream_sites.kml")
sites_sql <- paste0(getwd(), "/data/stream_sites.sqlite")
raster_crs <- st_crs(raster(p_raster))

stream_sites <- st_read(sites_kml) %>% 
  st_zm() %>% 
  st_transform(raster_crs) %>% 
  select(Name)

st_write(stream_sites, sites_sql, delete_dsn = TRUE)

#Snap points to stream network along flow directions
sites_snap <- paste0(getwd(), "/data/stream_sites_snap.sqlite")

taudem_snap <- paste0(taudem_path, "moveoutletstostrm",
                      " -p ", p_raster,
                      " -src ", src_raster,
                      " -o ", sites_sql,
                      " -om ", sites_snap)
system(taudem_snap)

stream_sites_snap <- st_read(sites_snap)

#NESTED WATERSHED DELINEATION
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

st_write(gw_clean, paste0(getwd(), "/data/gw_clean.sqlite"), delete_dsn = TRUE)

#NON-NESTED WATERSHED DELINEATION
for(i in 1:nrow(stream_sites_snap)){
  
  print(paste0("Delineating stream watershed ", i))
  
  st_write(stream_sites_snap[i, ], paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".sqlite"))
  
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

#Load and clean all non-nested watershed files
gw_nonnest_list <- lapply(1:nrow(stream_sites_snap), function(i){
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

st_write(gw_nonnest_clean, paste0(getwd(), "/data/gw_nonnest_clean.sqlite"), delete_dsn=TRUE)
