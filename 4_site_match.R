source("0_libs_and_funcs.R")

#Match sites with observations with discharge and base flow index observations manually
stream_sites_snap <- st_read(paste0(getwd(), "/data/stream_sites_snap.sqlite"))
discharge_merge <- readRDS(paste0(getwd(), "/data/discharge_index.rds"))

#Write discharge sites to vector file
discharge_merge %>% 
  filter(!is.na(utm_x),
         !is.na(utm_x)) %>% 
  st_as_sf(crs = 25832, coords=c("utm_x", "utm_y")) %>% 
  st_write(paste0(getwd(), "/data/discharge_merge.sqlite"))

#Write .csv file
stream_sites_snap %>% 
  st_drop_geometry() %>% 
  select(-dist_moved) %>% 
  write_csv(paste0(getwd(), "/data/stream_sites_snap.csv"))

#Add column matching "name" and "site_id" columns to stream_sites_snap.csv
stream_sites_site_id <- read_excel(paste0(getwd(), "/data/stream_sites_snap_site_id.xlsx"))

#Join to data and write to file
sites_discharge <- stream_sites_site_id %>% 
  left_join(discharge_merge)

write_csv(sites_discharge, paste0(getwd(), "/data/sites_discharge.csv"))
