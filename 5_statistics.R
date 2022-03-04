source("0_libs_and_funcs.R")

#Statistics

#Figure 2A
figure_2a_data <- read_excel(rawdata_path, sheet = "figure_2a") %>% 
  na.omit() %>% 
  mutate(a = 10^log_a,
         co2 = 10^log_co2)

qr_10 <- rq(log_co2~log_a, tau = 0.1, data = figure_2a_data)
summary(qr_10, "boot")
qr_50 <- rq(log_co2~log_a, tau = 0.5, data = figure_2a_data)
summary(qr_50, "boot")
qr_90 <- rq(log_co2~log_a, tau = 0.9, data = figure_2a_data)
summary(qr_90, "boot")

#Figure 2B
figure_2b_data <- read_excel(rawdata_path, sheet = "figure_2b") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening)

figure_2b_lm <- lm(log_co2_evening~log_co2_morning, data = figure_2b_data)
summary(figure_2b_lm)

#Figure 3A
figure_3a_data <- read_excel(rawdata_path, sheet = "figure_3a") %>% 
  na.omit() %>% 
  mutate(`Lake influence` = factor(ifelse(lake == 0, "No lake", "Lake")))

#Figure 3B
figure_3b_data <- read_excel(rawdata_path, sheet = "figure_3b") %>% 
  gather(var_time, co2, -chl, -name, -position) %>% 
  separate(var_time, c("var", "time"), "_") %>% 
  na.omit() %>% 
  filter(position == "down") %>% 
  mutate(log_co2 = log10(co2),
         log_chl = log10(chl),
         Time = factor(ifelse(time == "morning", "Morning", "Afternoon"), levels = c("Morning", "Afternoon")))

figure_3b_lm0 <- lm(log_co2~log_chl*time, data = figure_3b_data)
figure_3b_lm1 <- lm(log_co2~log_chl+time, data = figure_3b_data)
figure_3b_lm2 <- lm(log_co2~log_chl, data = figure_3b_data)
anova(figure_3b_lm0, figure_3b_lm1, figure_3b_lm2)

#Figure 4
figure_4_data <- read_excel(rawdata_path, sheet = "figure_4") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening,
         chl = 10^log_chl)

figure_4_data_lm <- bind_rows(data.frame(wtr = figure_4_data$wtr_morning, log_co2 = figure_4_data$log_co2_morning, log_chl = figure_4_data$log_chl, time = "morning"),
                          data.frame(wtr = figure_4_data$wtr_evening, log_co2 = figure_4_data$log_co2_evening, log_chl = figure_4_data$log_chl, time = "evening"))

figure_4_lm0 <- lm(log_co2~wtr*log_chl*time, data = figure_4_data_lm)
drop1(figure_4_lm0, test = "F")
figure_4_lm1 <- update(figure_4_lm0, . ~ . -wtr:log_chl:time)
drop1(figure_4_lm1, test = "F")
figure_4_lm2 <- update(figure_4_lm1, . ~ . -wtr:log_chl)
drop1(figure_4_lm2, test = "F")
figure_4_lm3 <- update(figure_4_lm2, . ~ . -wtr:time)
drop1(figure_4_lm3, test = "F")
figure_4_lm4 <- update(figure_4_lm3, . ~ . -log_chl:time)
drop1(figure_4_lm4, test = "F")
figure_4_lm5 <- update(figure_4_lm4, . ~ . -time)
drop1(figure_4_lm5, test = "F")

summary(figure_4_lm5)

#Figure 6A
figure_6a_data <- read_excel(rawdata_path, sheet = "figure_6a") %>% 
  filter(!is.na(position)) %>% 
  mutate(log_a = log10(a),
         `Lake influence` = factor(ifelse(position == "up", "No lake", "Lake")))

flux_qr_10 <- rq(flux~log_a, tau = 0.1, data = figure_6a_data)
summary(flux_qr_10, "boot")
flux_qr_50 <- rq(flux~log_a, tau = 0.5, data = figure_6a_data)
summary(flux_qr_50, "boot")
flux_qr_90 <- rq(flux~log_a, tau = 0.9, data = figure_6a_data)
summary(flux_qr_90, "boot")

#Figure 6B
wtr_qr_10 <- rq(flux~wtr, tau = 0.1, data = figure_6a_data)
summary(wtr_qr_10, "boot")
wtr_qr_50 <- rq(flux~wtr, tau = 0.5, data = figure_6a_data)
summary(wtr_qr_50, "boot")
wtr_qr_90 <- rq(flux~wtr, tau = 0.9, data = figure_6a_data)
summary(wtr_qr_90, "boot")

#Figure S1
co2_qr_10 <- rq(flux~co2_morning, tau = 0.1, data = figure_6a_data)
summary(co2_qr_10, "boot")
co2_qr_50 <- rq(flux~co2_morning, tau = 0.5, data = figure_6a_data)
summary(co2_qr_50, "boot")
co2_qr_90 <- rq(flux~co2_morning, tau = 0.9, data = figure_6a_data)
summary(co2_qr_90, "boot")

#Table 1 
table_1_data <- read_excel(rawdata_path, sheet = "table_1") %>% 
  mutate(site = factor(name)) %>% 
  filter(lakes == 0)

#Test if the response of log_co2 as a function of downstream location is the same for all sites
table_1_lm0 <- lm(log_co2~location + site, data = table_1_data)
table_1_lm1 <- lm(log_co2~location * site, data = table_1_data)

#Interaction is not significant
anova(table_1_lm0, table_1_lm1)
anova(table_1_lm0)
summary(table_1_lm0)

#Table 3
discharge <- read_csv(paste0(getwd(), "/data/sites_discharge.csv"))
sites_co2 <- read_tsv(paste0(getwd(), "/data/co2_sites_kaj.txt"))
catchment <- st_read(paste0(getwd(), "/data/gw_nonnest_clean.sqlite")) 

catchment_df <- catchment %>% 
  st_drop_geometry() %>% 
  select(name, total_area, mean_elev, mean_slope)

merge <- discharge %>% 
  left_join(sites_co2) %>% 
  left_join(catchment_df)

#Relationship between co2 flux and index/catch area
table_3_mod <- merge %>% 
  select(co2_flux, index, total_area) %>% 
  na.omit() 

co2_gw_lm0 <- lm(co2_flux~index*log10(total_area), data = table_3_mod)
drop1(co2_gw_lm0, test = "F")
co2_gw_lm1 <- update(co2_gw_lm0, . ~ . -index:log10(total_area))
drop1(co2_gw_lm1, test = "F")

summary(co2_gw_lm1)
