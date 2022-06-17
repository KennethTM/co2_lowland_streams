source("0_libs_and_funcs.R")

#Statistics

#Figure 3
figure_3_data <- read_excel(rawdata_path, sheet = "figure_3") %>%
  mutate(log_a = log10(a),
         log_co2 = log10(co2_morning)) |> 
  select(log_a, log_co2, position) |> 
  filter(!is.infinite(log_co2)) |> 
  na.omit() |> 
  mutate(`Lake influence` = ifelse(position == "up", "No lake", "Lake"))

qr_10 <- rq(log_co2~log_a, tau = 0.1, data = figure_3_data)
summary(qr_10, "boot")
qr_50 <- rq(log_co2~log_a, tau = 0.5, data = figure_3_data)
summary(qr_50, "boot")
qr_90 <- rq(log_co2~log_a, tau = 0.9, data = figure_3_data)
summary(qr_90, "boot")

#Figure 5
figure_5_data <- read_excel(rawdata_path, sheet = "figure_5") %>% 
  gather(var_time, co2, -chl, -name, -position) %>% 
  separate(var_time, c("var", "time"), "_") %>% 
  na.omit() %>% 
  filter(position == "down") %>% 
  mutate(log_co2 = log10(co2),
         log_chl = log10(chl),
         `Time of day` = factor(ifelse(time == "morning", "Morning", "Afternoon"), levels = c("Morning", "Afternoon")))

figure_5_lm0 <- lm(log_co2~log_chl*time, data = figure_5_data)
figure_5_lm1 <- lm(log_co2~log_chl+time, data = figure_5_data)
figure_5_lm2 <- lm(log_co2~log_chl, data = figure_5_data)
anova(figure_5_lm0, figure_5_lm1, figure_5_lm2)

#Figure 6
figure_6_data <- read_excel(rawdata_path, sheet = "figure_6") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening,
         chl = 10^log_chl)

figure_6_data_lm <- bind_rows(data.frame(wtr = figure_6_data$wtr_morning, log_co2 = figure_6_data$log_co2_morning, log_chl = figure_6_data$log_chl, time = "morning"),
                          data.frame(wtr = figure_6_data$wtr_evening, log_co2 = figure_6_data$log_co2_evening, log_chl = figure_6_data$log_chl, time = "evening"))

figure_6_lm0 <- lm(log_co2~wtr*log_chl*time, data = figure_6_data_lm)
drop1(figure_6_lm0, test = "F")
figure_6_lm1 <- update(figure_6_lm0, . ~ . -wtr:log_chl:time)
drop1(figure_6_lm1, test = "F")
figure_6_lm2 <- update(figure_6_lm1, . ~ . -wtr:log_chl)
drop1(figure_6_lm2, test = "F")
figure_6_lm3 <- update(figure_6_lm2, . ~ . -wtr:time)
drop1(figure_6_lm3, test = "F")
figure_6_lm4 <- update(figure_6_lm3, . ~ . -log_chl:time)
drop1(figure_6_lm4, test = "F")
figure_6_lm5 <- update(figure_6_lm4, . ~ . -time)
drop1(figure_6_lm5, test = "F")

summary(figure_6_lm5)

#Figure 8
figure_8_data <- read_excel(rawdata_path, sheet = "figure_3") %>% 
  filter(!is.na(position)) %>% 
  mutate(log_a = log10(a),
         `Lake influence` = factor(ifelse(position == "up", "No lake", "Lake")))

flux_qr_10 <- rq(flux~log_a, tau = 0.1, data = figure_8_data)
summary(flux_qr_10, "boot")
flux_qr_50 <- rq(flux~log_a, tau = 0.5, data = figure_8_data)
summary(flux_qr_50, "boot")
flux_qr_90 <- rq(flux~log_a, tau = 0.9, data = figure_8_data)
summary(flux_qr_90, "boot")

#Figure 8B
wtr_qr_10 <- rq(flux~wtr, tau = 0.1, data = figure_8_data)
summary(wtr_qr_10, "boot")
wtr_qr_50 <- rq(flux~wtr, tau = 0.5, data = figure_8_data)
summary(wtr_qr_50, "boot")
wtr_qr_90 <- rq(flux~wtr, tau = 0.9, data = figure_8_data)
summary(wtr_qr_90, "boot")

#Figure S1
co2_qr_10 <- rq(flux~co2_morning, tau = 0.1, data = figure_8_data)
summary(co2_qr_10, "boot")
co2_qr_50 <- rq(flux~co2_morning, tau = 0.5, data = figure_8_data)
summary(co2_qr_50, "boot")
co2_qr_90 <- rq(flux~co2_morning, tau = 0.9, data = figure_8_data)
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

#Table 2
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
table_2_mod <- read_excel(rawdata_path, sheet = "table_2") %>% 
  select(co2_flux = 7, index = 9, total_area = 10) %>% 
  na.omit() 

co2_gw_lm0 <- lm(co2_flux~index*log10(total_area), data = table_2_mod)
drop1(co2_gw_lm0, test = "F")
co2_gw_lm1 <- update(co2_gw_lm0, . ~ . -index:log10(total_area))
drop1(co2_gw_lm1, test = "F")

summary(co2_gw_lm1)
