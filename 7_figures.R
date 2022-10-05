source("5_statistics.R")

#Figures

#Figure 1

#Denmark main plot
dk_border <- st_read(paste0(getwd(), "/data/dk_border.sqlite"))
stream_sites_snap <- st_read(paste0(getwd(), "/data/stream_sites_snap.sqlite"))

gw_clean <- st_read(paste0(getwd(), "/data/gw_clean.sqlite"))
gw_clean_cent <- as.data.frame(st_coordinates(st_centroid(gw_clean)))
gw_clean_sort <- bind_cols(gw_clean, gw_clean_cent) %>% 
  arrange(X, Y)

xlabs <- seq(8, 12, 1)
ylabs <- seq(54.5, 57.5, 0.5)

col_pal <- brewer.pal(8, "Dark2")
col_vec <- c(rep(col_pal, 5), col_pal[1])

map_fig <- ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "grey")+
  geom_sf(data = gw_clean_sort, aes(fill=factor(id, levels = id)), show.legend = FALSE, col = NA)+
  geom_sf(data= stream_sites_snap, size = 0.7)+
  scale_fill_manual(values=col_vec)+
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))+
  annotate("text", x=673700.6, y=6231868, label = "Zealand")+
  annotate("text", x=5e+05, y=6350781, label = "Jutland")+
  annotate("text", x=581911, y=6162329, label = "Funen")+
  xlab("Latitude")+
  ylab("Longitude")

map_fig

ggsave(paste0(figures_path, "fig_1.png"), map_fig, width = 84, height = 90, units = "mm")
ggsave(paste0(figures_path, "fig_1.pdf"), map_fig, width = 84, height = 90, units = "mm")

#Figure 2
figure_2_pred <- data.frame(log_a = seq(-1.1, 2.1, 0.1)) %>% 
  mutate(pred_10 = predict(qr_10, newdata=.),
         pred_50 = predict(qr_50, newdata=.),
         pred_90 = predict(qr_90, newdata=.)) %>% 
  gather(pred, log_co2, -log_a) %>% 
  separate(pred, c("pred", "q"), "_") %>% 
  mutate(Quantile = factor(as.numeric(q)/100))

figure_2_fig <- figure_2_data %>% 
  ggplot(aes(log_a, log_co2)) +
  geom_point(aes(shape = `Lake influence`))+
  geom_line(data=figure_2_pred, aes(col=Quantile), size=1.2)+
  scale_color_viridis_d()+
  scale_y_continuous(limits = c(-0.2, 3), breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000))+
  scale_x_continuous(breaks = c(-1, 0, 1, 2), labels = c(0.1, 1, 10, 100))+
  annotate("text", x=1.5, y=3, label = qr_eqn(qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=1.5, y=2.8, label = qr_eqn(qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=1.5, y=2.6, label = qr_eqn(qr_90, "0.9"), parse=TRUE)+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 21))+
  ylab(expression(CO[2]~"("*mu*M*")"))+
  xlab(expression(Wetted~area~"(m"^{2}*")"))

figure_2_fig

ggsave(paste0(figures_path, "fig_2.png"), figure_2_fig, width = 129, height = 90, units = "mm")
ggsave(paste0(figures_path, "fig_2.pdf"), figure_2_fig, width = 129, height = 90, units = "mm")

#Figure 3
figure_3_data <- read_excel(rawdata_path, sheet = "figure_3") |> 
  filter(site != "pøle") |> 
  mutate(`Lake influence` = ifelse(lake == 1, "Lake", "No lake"),
         site = factor(site, levels = c("græse", "havelse", "mølle", "guden")))

ann_text <- data.frame(position = 1,
                       co2_morning = 720,
                       lab = c("River Græse", "River Havelse", "River Mølle", "River Guden"),
                       site = factor(c("græse", "havelse", "mølle", "guden"), 
                                     levels = c("græse", "havelse", "mølle", "guden")))

figure_3 <- figure_3_data |> 
  ggplot(aes(position, co2_morning))+
  geom_hline(yintercept = 20, linetype=3)+
  geom_text(data = ann_text, aes(label = lab), hjust=0)+
  geom_line(linetype=1)+
  geom_point(aes(shape = `Lake influence`), fill="white")+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 21))+
  facet_wrap(site~., scale = "free_x")+
  scale_x_continuous(breaks = seq(1, 21, 2))+
  ylim(0, 730)+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom")+
  ylab(expression(CO[2]~"("*mu*M*")"))+
  xlab("Fluvial network position")

figure_3

ggsave(paste0(figures_path, "fig_3.png"), figure_3, width = 129, height = 129, units = "mm")
ggsave(paste0(figures_path, "fig_3.pdf"), figure_3, width = 129, height = 129, units = "mm")

#Figure 4
figure_4_fig <- figure_4_data %>% 
  ggplot(aes(chl, co2))+
  geom_smooth(method = "lm", color="black")+
  geom_point(aes(shape=`Time of day`))+
  scale_y_log10()+
  scale_x_log10()+
  scale_shape_manual(values=c(1, 19))+
  ylab(expression("CO"[2]~"("*mu*M*")"))+
  xlab(expression("Chl. "*italic(a)~"("*mu*g~L^{-1}*")"))+
  scale_color_viridis_d()+
  annotate("text", x=50, y=100, label = lm_eqn(figure_4_lm2), parse=TRUE)+
  theme(legend.position = c(0.2, 0.2))

figure_4_fig

ggsave(paste0(figures_path, "fig_4.png"), figure_4_fig, width = 129, height = 90, units = "mm")
ggsave(paste0(figures_path, "fig_4.pdf"), figure_4_fig, width = 129, height = 90, units = "mm")

#Figure 5
x <- figure_5_data$log_chl
y <- figure_5_data$wtr_morning
z <- figure_5_data$log_co2_morning
fit <- lm(z ~ x + y)

grid.lines <- 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

svg(paste0(figures_path, "fig_5_raw.svg"), width = 5.07874, height = 5.07874)
scatter3D(x, y, z, pch = 19, cex = 1, colvar = NULL, col="black",
          theta = 120, phi = 20, bty="b", ticktype="detailed",
          xlab = "log10(Chl. a)", ylab = "Water temp.", zlab = "log10(CO2)",
          surf = list(x = x.pred, y = y.pred, z = z.pred, facets = TRUE, fit = fitpoints,
                      col=ramp.col(col = c("seagreen1", "dodgerblue3"), n = 300, alpha=0.5)))
dev.off()

#Figure 6
figure_6_data <- read_excel(rawdata_path, sheet = "figure_6") %>% 
  na.omit() %>% 
  mutate(`Lake influence` = factor(ifelse(lake == 0, "No lake", "Lake")))

figure_6_fig <- figure_6_data %>% 
  ggplot(aes(`June-Aug`, `Sep-May`, shape = `Lake influence`)) +
  geom_abline(intercept = 0, slope=1, linetype=2)+
  geom_abline(intercept = 0, slope=2, linetype=2)+
  geom_abline(intercept = 0, slope=3, linetype=2)+
  geom_point()+
  scale_x_continuous(limits=c(1, 425))+
  scale_y_continuous(limits=c(1, 425))+
  annotate("text", x = 425, y = 400, label = "1:1")+
  annotate("text", x = 220, y = 400, label = "1:2")+
  annotate("text", x = 150, y = 400, label = "1:3")+
  annotate("point", x = 16, y = 20, shape = 18, col="coral", size = 4)+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 1))+
  ylab(expression("Sep. to May CO"[2]~"("*mu*M*")"))+
  xlab(expression("June to Aug. CO"[2]~"("*mu*M*")"))+
  theme(legend.position = c(0.7, 0.25))+
  coord_equal()

figure_6_fig

ggsave(paste0(figures_path, "fig_6.png"), figure_6_fig, width = 129, height = 90, units = "mm")
ggsave(paste0(figures_path, "fig_6.pdf"), figure_6_fig, width = 129, height = 90, units = "mm")

#Figure 7
inset_sites <- st_read(paste0(getwd(), "/data/lake_arre_network.kml")) %>% 
  add_column(name = as.character(c(6, 5, 3, 4, 2, 1)))

q_sf <- readRDS(paste0(getwd(), "/data/lake_arre_network_osm.rds"))

fig_7_inset_map <- ggplot()+
  geom_sf(data = q_sf$osm_lines, col = "lightblue")+
  geom_sf(data = filter(q_sf$osm_polygons, osm_id != "4761877"), fill = "deepskyblue3", col = "deepskyblue3")+
  geom_sf(data = filter(q_sf$osm_multipolygons, osm_id != "4547983"), fill = "deepskyblue3", col = "deepskyblue3")+
  geom_sf_text(data = inset_sites, aes(label = name))+
  annotate("text", x=12.11, y=55.96, label="Lake Arre")+
  xlab(NULL)+
  ylab(NULL)+
  annotation_scale(location="br")+
  theme(axis.text = element_blank(), axis.ticks = element_blank())

figure_7_data <- read_excel(rawdata_path, sheet = "figure_7") %>% 
  na.omit() %>% 
  mutate(co2 = 10^log_co2,
         date = ymd("1995-01-01")+time) %>% 
  rename(Site = site) %>% 
  filter(Site != "Pølebro 2") %>% 
  left_join(st_drop_geometry(inset_sites), by = c("Site" = "Name"))

figure_7_fig <- figure_7_data %>% 
  ggplot(aes(date, co2, col=name))+
  geom_point(size=0.7)+
  geom_line()+
  scale_y_log10()+
  ylab(expression(CO[2]~"("*mu*M*")"))+
  scale_x_date(date_breaks = "3 month", date_labels = "%b")+
  xlab("Month")+
  scale_color_brewer(palette = "Dark2", name = "Site")

ggsave(paste0(figures_path, "fig_7_raw.svg"), figure_7_fig, width = 129, height = 84, units = "mm")
ggsave(paste0(figures_path, "fig_7_inset_map.svg"), fig_7_inset_map, width = 129, height = 84, units = "mm")

#Figure 8
figure_8_broken_axis <- figure_8_data %>% 
  filter(flux > 300)

figure_8a_fig <- figure_8_data %>% 
  filter(flux < 300) %>% 
  ggplot(aes(a, flux, shape = `Lake influence`))+
  geom_hline(yintercept = 0, linetype=3)+
  geom_point()+
  geom_smooth(inherit.aes = FALSE, aes(a, flux), method = "loess", color="black")+
  scale_x_log10()+
  geom_point(data = figure_8_broken_axis, aes(a, 300), shape=1)+
  geom_text(data = figure_8_broken_axis, aes(a, 300, label=round(flux, 0)), nudge_x = 0.2, size =3)+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 1))+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression("Wetted area (m"^{2}*")"))+
  theme(legend.position = c(0.85, 0.85))

figure_8b_fig <- figure_8_data %>% 
  filter(flux < 300) %>% 
  ggplot(aes(wtr, flux, shape = `Lake influence`))+
  geom_hline(yintercept = 0, linetype=3)+
  geom_point()+
  geom_smooth(inherit.aes = FALSE, aes(wtr, flux), method = "loess", color="black")+
  geom_point(data = figure_8_broken_axis, aes(wtr, 300), shape=1)+
  geom_text(data = figure_8_broken_axis, aes(wtr, 300, label=round(flux, 0)), nudge_x = 1, size =3)+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 1))+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab("Water temperature (°C)")+
  theme(legend.position = c(0.85, 0.85))

fig_8 <- figure_8a_fig+figure_8b_fig+plot_layout(guides = "collect", ncol=1)+plot_annotation(tag_levels = "A")

fig_8

ggsave(paste0(figures_path, "fig_8.png"), fig_8, width = 129, height = 180, units = "mm")
ggsave(paste0(figures_path, "fig_8.pdf"), fig_8, width = 129, height = 180, units = "mm")

#Figure S1
figure_s1_pred <- data.frame(co2_morning = seq(0, 735, 1)) %>% 
  mutate(pred_10 = predict(co2_qr_10, newdata=.),
         pred_50 = predict(co2_qr_50, newdata=.),
         pred_90 = predict(co2_qr_90, newdata=.)) %>% 
  gather(pred, flux, -co2_morning) %>% 
  separate(pred, c("pred", "q"), "_") %>% 
  mutate(Quantile = factor(as.numeric(q)/100))

figure_s1_co2_fig <- figure_8_data %>% 
  ggplot(aes(co2_morning, flux)) +
  geom_point(shape=1)+
  geom_line(data=figure_s1_pred, aes(col=Quantile), size=1.2)+
  scale_color_viridis_d()+
  annotate("text", x=200, y=500, label = qr_eqn(co2_qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=200, y=450, label = qr_eqn(co2_qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=200, y=400, label = qr_eqn(co2_qr_90, "0.9"), parse=TRUE)+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression("CO"[2]~"("*mu*M*")"))+
  coord_cartesian(ylim = c(0, 505))

ggsave(paste0(figures_path, "fig_s1.png"), figure_s1_co2_fig, width = 129, height = 100, units = "mm")

#Figure S2
figure_s2_a <- co2_gw_mod |> 
  ggplot(aes(index, co2_flux))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~d^{-1}*")"))+
  scale_x_continuous(limits = c(0.35, 1))+
  xlab("Groundwater index")

figure_s2_b <- co2_gw_mod |> 
  ggplot(aes(total_area, co2_flux))+
  geom_point()+
  scale_x_log10(limits=c(10, 1000))+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~d^{-1}*")"))+
  xlab(expression("Catchment area (km"^{2}*")"))

figure_s2 <- figure_s2_a+figure_s2_b+plot_layout(ncol=1)+plot_annotation(tag_levels = "A")

figure_s2

ggsave(paste0(figures_path, "fig_s2.png"), figure_s2, width = 129, height = 160, units = "mm")

#Table 1
merge %>% 
  mutate(catchment_area = total_area*10^-6) %>%
  select(name, co2_flux, co2_flux_weight, index, alkalinity, catchment_area) %>%
  write_csv(paste0(figures_path, "table_1.csv"))

#Table S2
table_1_data %>% 
  mutate(co2 = 10^log_co2) %>% 
  group_by(name) %>% 
  summarise(n = n(), co2_mean = mean(co2), 
            co2_first = co2[which.min(location)], 
            co2_last = co2[which.max(location)]) %>% 
  write_csv(paste0(figures_path, "table_s2.csv"))
