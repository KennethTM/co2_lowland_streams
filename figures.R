source("libs_and_funcs.R")

#Figures

#Figure 2A
slide_5 <- read_excel(rawdata_path, sheet = "slide_5") %>% 
  na.omit() %>% 
  mutate(a = 10^log_a,
         co2 = 10^log_co2)

qr_10 <- rq(log_co2~log_a, tau = 0.1, data = slide_5)
summary(qr_10, "boot")
qr_50 <- rq(log_co2~log_a, tau = 0.5, data = slide_5)
summary(qr_50, "boot")
qr_90 <- rq(log_co2~log_a, tau = 0.9, data = slide_5)
summary(qr_90, "boot")

slide_5_pred <- data.frame(log_a = seq(-1.1, 2.1, 0.1)) %>% 
  mutate(pred_10 = predict(qr_10, newdata=.),
         pred_50 = predict(qr_50, newdata=.),
         pred_90 = predict(qr_90, newdata=.)) %>% 
  gather(pred, log_co2, -log_a) %>% 
  separate(pred, c("pred", "q"), "_") %>% 
  mutate(Quantile = factor(as.numeric(q)/100))

slide_5_fig <- slide_5 %>% 
  ggplot(aes(log_a, log_co2)) +
  geom_point(shape=1)+
  geom_line(data=slide_5_pred, aes(col=Quantile), size=1.2)+
  scale_color_viridis_d()+
  scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000))+
  scale_x_continuous(breaks = c(-1, 0, 1, 2), labels = c(0.1, 1, 10, 100))+
  annotate("text", x=1.5, y=3, label = qr_eqn(qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=1.5, y=2.8, label = qr_eqn(qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=1.5, y=2.6, label = qr_eqn(qr_90, "0.9"), parse=TRUE)+
  ylab(expression(CO[2]~"("*mu*M*")"))+
  xlab(expression(Wetted~area~"(m"^{2}*")"))

#Figure 2B
slide_6 <- read_excel(rawdata_path, sheet = "slide_6") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening)

slide_6_lm <- lm(log_co2_evening~log_co2_morning, data = slide_6)

slide_6_fig <- slide_6 %>% 
  ggplot(aes(co2_morning, co2_evening)) +
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point(shape=1)+
  geom_smooth(method="lm", col="black")+
  scale_x_log10(limits=c(1, 1000))+
  scale_y_log10(limits=c(1, 1000))+
  annotate("text", x=10, y=1000, label = lm_eqn(slide_6_lm), parse=TRUE)+
  ylab(expression("Afternoon"~CO[2]~"("*mu*M*")"))+
  xlab(expression("Morning"~CO[2]~"("*mu*M*")"))

fig_2 <- slide_5_fig/slide_6_fig+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_2.png"), fig_2, width = 129, height = 180, units = "mm")

#Figure 4
slide_7_8 <- read_excel(rawdata_path, sheet = "slide_7_8") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening,
         chl = 10^log_chl)

slide_7_8_lm <- bind_rows(data.frame(wtr = slide_7_8$wtr_morning, log_co2 = slide_7_8$log_co2_morning, log_chl = slide_7_8$log_chl, time = "morning"),
                     data.frame(wtr = slide_7_8$wtr_evening, log_co2 = slide_7_8$log_co2_evening, log_chl = slide_7_8$log_chl, time = "evening"))

slide_7_8_lm0 <- lm(log_co2~wtr*log_chl*time, data = slide_7_8_lm)
drop1(slide_7_8_lm0, test = "F")
slide_7_8_lm1 <- update(slide_7_8_lm0, . ~ . -wtr:log_chl:time)
drop1(slide_7_8_lm1, test = "F")
slide_7_8_lm2 <- update(slide_7_8_lm1, . ~ . -wtr:log_chl)
drop1(slide_7_8_lm2, test = "F")
slide_7_8_lm3 <- update(slide_7_8_lm2, . ~ . -wtr:time)
drop1(slide_7_8_lm3, test = "F")
slide_7_8_lm4 <- update(slide_7_8_lm3, . ~ . -log_chl:time)
drop1(slide_7_8_lm4, test = "F")
slide_7_8_lm5 <- update(slide_7_8_lm4, . ~ . -time)
drop1(slide_7_8_lm5, test = "F")

x <- slide_7_8$log_chl
y <- slide_7_8$wtr_morning
z <- slide_7_8$log_co2_morning
fit <- lm(z ~ x + y)

grid.lines <- 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

svg(paste0(figures_path, "fig_4_raw.svg"), width = 5.07874, height = 5.07874)
scatter3D(x, y, z, pch = 19, cex = 1, colvar = NULL, col="black",
          theta = 120, phi = 20, bty="b", ticktype="detailed",
          xlab = "log10(Chl. a)", ylab = "Water temp.", zlab = "log10(CO2)",
          surf = list(x = x.pred, y = y.pred, z = z.pred, facets = TRUE, fit = fitpoints,
                      col=ramp.col(col = c("seagreen1", "dodgerblue3"), n = 300, alpha=0.5)))
dev.off()

#Figure 5
inset_sites <- st_read(paste0(getwd(), "/data/lake_arre_network.kml")) %>% 
  add_column(name = as.character(c(6, 5, 3, 4, 2, 1)))
inset_sites_bbox <- c("xmin" = 12.025, "xmax" = 12.275, 
                      "ymin" = 55.93, "ymax" = 56.01)

# q <- opq(bbox = c(inset_sites_bbox[["xmin"]],inset_sites_bbox[["ymin"]],
#                   inset_sites_bbox[["xmax"]], inset_sites_bbox[["ymax"]])) %>%
#   add_osm_feature("water") %>%
#   add_osm_feature("waterway")
# 
# q_sf <- osmdata_sf(q)
# saveRDS(q_sf, paste0(getwd(), "/data/arresø_network_osm.rds"))

q_sf <- readRDS(paste0(getwd(), "/data/lake_arre_network_osm.rds"))

fig_5_inset_map <- ggplot()+
  geom_sf(data = q_sf$osm_lines, col = "lightblue")+
  geom_sf(data = filter(q_sf$osm_polygons, osm_id != "4761877"), fill = "deepskyblue3", col = "deepskyblue3")+
  geom_sf(data = filter(q_sf$osm_multipolygons, osm_id != "4547983"), fill = "deepskyblue3", col = "deepskyblue3")+
  geom_sf_text(data = inset_sites, aes(label = name))+
  annotate("text", x=12.11, y=55.96, label="Lake Arre")+
  xlab(NULL)+
  ylab(NULL)+
  annotation_scale(location="br")+
  theme(axis.text = element_blank(), axis.ticks = element_blank())

slide_10 <- read_excel(rawdata_path, sheet = "slide_10") %>% 
  na.omit() %>% 
  mutate(co2 = 10^log_co2,
         date = ymd("1995-01-01")+time) %>% 
  rename(Site = site) %>% 
  filter(Site != "Pølebro 2") %>% 
  left_join(st_drop_geometry(inset_sites), by = c("Site" = "Name"))

fig_5 <- slide_10 %>% 
  ggplot(aes(date, co2, col=name))+
  geom_point(size=0.7)+
  geom_line()+
  scale_y_log10()+
  ylab(expression(CO[2]~"("*mu*M*")"))+
  scale_x_date(date_breaks = "3 month", date_labels = "%b")+
  xlab("Month")+
  scale_color_brewer(palette = "Dark2", name = "Site")

ggsave(paste0(figures_path, "fig_5_raw.svg"), fig_5, width = 129, height = 84, units = "mm")
ggsave(paste0(figures_path, "fig_5_inset_map.svg"), fig_5_inset_map, width = 129, height = 84, units = "mm")

#Figure 3A
slide_11 <- read_excel(rawdata_path, sheet = "slide_11") %>% 
  na.omit() %>% 
  mutate(`Lake influence` = factor(ifelse(lake == 0, "No lake", "Lake")))

slide_11_fig <- slide_11 %>% 
  ggplot(aes(`June-Aug`, `Sep-May`, shape = `Lake influence`)) +
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  scale_x_continuous(limits=c(1, 400))+
  scale_y_continuous(limits=c(1, 400))+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 1))+
  ylab(expression("Sep-May CO"[2]~"("*mu*M*")"))+
  xlab(expression("June-Aug CO"[2]~"("*mu*M*")"))

#Figure 3B
slide_13 <- read_excel(rawdata_path, sheet = "slide_13") %>% 
  gather(var_time, co2, -chl, -name, -position) %>% 
  separate(var_time, c("var", "time"), "_") %>% 
  na.omit() %>% 
  filter(position == "down") %>% 
  mutate(log_co2 = log10(co2),
         log_chl = log10(chl),
         Time = factor(ifelse(time == "morning", "Morning", "Afternoon"), levels = c("Morning", "Afternoon")))

slide_13_lm0 <- lm(log_co2~log_chl*time, data = slide_13)
slide_13_lm1 <- lm(log_co2~log_chl+time, data = slide_13)
slide_13_lm2 <- lm(log_co2~log_chl, data = slide_13)
anova(slide_13_lm0, slide_13_lm1, slide_13_lm2)

slide_13_fig <- slide_13 %>% 
  ggplot(aes(chl, co2))+
  geom_smooth(method = "lm", color="black")+
  geom_point(aes(shape=Time))+
  scale_y_log10()+
  scale_x_log10()+
  scale_shape_manual(values=c(1, 19))+
  ylab(expression("CO"[2]~"("*mu*M*")"))+
  xlab(expression("Chl. "*italic(a)~"(mg"~L^{-1}*")"))+
  scale_color_viridis_d()+
  annotate("text", x=50, y=100, label = lm_eqn(slide_13_lm2), parse=TRUE)

fig_3 <- slide_11_fig/slide_13_fig+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_3.png"), fig_3, width = 129, height = 180, units = "mm")

#Figure 6A
slide_16 <- read_excel(rawdata_path, sheet = "slide_16_21") %>% 
  filter(!is.na(position)) %>% 
  mutate(log_a = log10(a),
         `Lake influence` = factor(ifelse(position == "up", "No lake", "Lake")))

flux_qr_10 <- rq(flux~log_a, tau = 0.1, data = slide_16)
summary(flux_qr_10, "boot")
flux_qr_50 <- rq(flux~log_a, tau = 0.5, data = slide_16)
summary(flux_qr_50, "boot")
flux_qr_90 <- rq(flux~log_a, tau = 0.9, data = slide_16)
summary(flux_qr_90, "boot")

slide_16_fig <- slide_16 %>% 
  ggplot(aes(a, flux, shape = `Lake influence`))+
  geom_hline(yintercept = 0, linetype=3)+
  geom_point()+
  geom_smooth(inherit.aes = FALSE, aes(a, flux), method = "loess", color="coral")+
  scale_x_log10()+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 1))+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression("Wetted area (m"^{2}*")"))+
  theme(legend.position = c(0.85, 0.85))

#Figure 6B
wtr_qr_10 <- rq(flux~wtr, tau = 0.1, data = slide_16)
summary(wtr_qr_10, "boot")
wtr_qr_50 <- rq(flux~wtr, tau = 0.5, data = slide_16)
summary(wtr_qr_50, "boot")
wtr_qr_90 <- rq(flux~wtr, tau = 0.9, data = slide_16)
summary(wtr_qr_90, "boot")

slide_20_pred <- data.frame(wtr = seq(8, 23, 0.1)) %>% 
  mutate(pred_10 = predict(wtr_qr_10, newdata=.),
         pred_50 = predict(wtr_qr_50, newdata=.),
         pred_90 = predict(wtr_qr_90, newdata=.)) %>% 
  gather(pred, flux, -wtr) %>% 
  separate(pred, c("pred", "q"), "_") %>% 
  mutate(Quantile = factor(as.numeric(q)/100))

slide_16_wtr_fig <- slide_16 %>% 
  ggplot(aes(wtr, flux)) +
  geom_point(shape=1)+
  geom_line(data=slide_20_pred, aes(col=Quantile), size=1.2)+
  scale_color_viridis_d()+
  annotate("text", x=19, y=500, label = qr_eqn(wtr_qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=19, y=450, label = qr_eqn(wtr_qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=19, y=400, label = qr_eqn(wtr_qr_90, "0.9"), parse=TRUE)+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab("Water temperature (°C)")

fig_6 <- slide_16_fig+slide_16_wtr_fig+plot_layout(guides = "collect", ncol=1)+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_6.png"), fig_6, width = 129, height = 180, units = "mm")

#Figure S1
co2_qr_10 <- rq(flux~co2_morning, tau = 0.1, data = slide_16)
summary(co2_qr_10, "boot")
co2_qr_50 <- rq(flux~co2_morning, tau = 0.5, data = slide_16)
summary(co2_qr_50, "boot")
co2_qr_90 <- rq(flux~co2_morning, tau = 0.9, data = slide_16)
summary(co2_qr_90, "boot")

slide_21_pred <- data.frame(co2_morning = seq(0, 735, 1)) %>% 
  mutate(pred_10 = predict(co2_qr_10, newdata=.),
         pred_50 = predict(co2_qr_50, newdata=.),
         pred_90 = predict(co2_qr_90, newdata=.)) %>% 
  gather(pred, flux, -co2_morning) %>% 
  separate(pred, c("pred", "q"), "_") %>% 
  mutate(Quantile = factor(as.numeric(q)/100))

slide_16_co2_fig <- slide_16 %>% 
  ggplot(aes(co2_morning, flux)) +
  geom_point(shape=1)+
  geom_line(data=slide_21_pred, aes(col=Quantile), size=1.2)+
  scale_color_viridis_d()+
  annotate("text", x=200, y=700, label = qr_eqn(co2_qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=200, y=625, label = qr_eqn(co2_qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=200, y=550, label = qr_eqn(co2_qr_90, "0.9"), parse=TRUE)+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression("CO"[2]~"("*mu*M*")"))

ggsave(paste0(figures_path, "fig_s1.png"), slide_16_co2_fig, width = 129, height = 100, units = "mm")
