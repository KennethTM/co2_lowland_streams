library(data.table)

#Data from https://odaforalle.au.dk/
#Discharge in danish streams from 1970-2020 (2020 inclusive)

#Read stream discharge data
discharge <- fread(paste0(getwd(), "/data/discharge.csv"), dec = ",")

#Catchment area data
catchment_area <- fread(paste0(getwd(), "/data/catchment_area.csv"), dec = ",")
catchment_area_clean <- catchment_area[, .(site_id = ObservationsStedNr,
                                           area_km2 = `Oplandsareal Km2`)
                                       ][, area_km2 := ifelse(area_km2 == 0, NA, area_km2)]
catchment_area_clean <- na.omit(catchment_area_clean)

#Get coordinates for each site
discharge_coords <- unique(discharge[, .(site_id = ObservationsStedNr,
                                         utm_x = as.numeric(Xutm_Euref89_Zone32),
                                         utm_y = as.numeric(Yutm_Euref89_Zone32))])

#Clean and filter out negative values
discharge_clean <- discharge[, .(site_id = ObservationsStedNr,
                                 date = as.Date(as.character(Dato), format = "%Y%m%d"),
                                 q = Resultat)][
                                   q > 0][, year := format(date,"%Y")
                                          ][, month := format(date,"%B")]

#Calculate median-monthly-minimum across years and sites
discharge_mmi <- discharge_clean[, .(q_min = min(q),
                                 q_mean = mean(q),
                                 n = .N), by=.(site_id, year, month)
                             ][, .(q_mmi = median(q_min),
                                   q_mean = mean(q_mean),
                                   n = sum(n)), by=.(site_id, year)
                               ][, .(q_mmi_site = median(q_mmi),
                                     q_mean_site = mean(q_mean),
                                     n = sum(n)), by=.(site_id)
                                 ]

#Merge with coordinates and catchment area
#Calculate index and specific discharge in units l/s/km2
#Discharge observations in units m3/s
discharge_merge <- discharge_mmi[discharge_coords, on = "site_id"]
discharge_merge <- catchment_area_clean[discharge_merge, on = "site_id"
                                        ][, index := (q_mmi_site)/(q_mean_site)
                                          ][, q_mmi_site_spec := (q_mmi_site/area_km2)*1000
                                            ][, q_mean_site_spec := (q_mean_site/area_km2)*1000]

#Save data
saveRDS(as.data.frame(discharge_merge), paste0(getwd(), "/data/discharge_index.rds"))
