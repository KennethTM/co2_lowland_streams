source("libs_and_funcs.R")
source("statistics.R")

#Figures

#Figure 1

#Plot
# dk_border_raw <- raster::getData("GADM", country = "DNK", level = 0, path = paste0(getwd(), "/data"))
# dk_border <- dk_border_raw %>% 
#   st_as_sf() %>% 	
#   st_crop(xmin = 8, ymin = 54.56, xmax = 14, ymax = 57.76) %>% 	
#   st_transform(25832) %>% 
#   st_write(paste0(getwd(), "/data/dk_border.sqlite"))

dk_border <- st_read(paste0(getwd(), "/data/dk_border.sqlite"))
gw_clean <- st_read(paste0(getwd(), "/data/gw_clean.sqlite"))
stream_sites_snap <- st_read(paste0(getwd(), "/data/stream_sites_snap.sqlite"))
stream_sites_snap_coords <- as.data.frame(st_coordinates(stream_sites_snap))
stream_sites_snap_sort <- bind_cols(stream_sites_snap, stream_sites_snap_coords) %>% 
  arrange(Y, X)

xlabs <- seq(8, 12, 1)
ylabs <- seq(54.5, 57.5, 0.5)

col_pal <- brewer.pal(8, "Dark2")
col_vec <- c(rep(col_pal, 4), col_pal[1:4])

map_fig <- ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "grey")+
  geom_sf(data = gw_clean, aes(fill=factor(id, levels = id)), show.legend = FALSE, col = NA)+
  geom_sf(data= stream_sites_snap, size = 0.7)+
  scale_fill_manual(values=col_vec)+
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))+
  xlab("Latitude")+
  ylab("Longitude")

ggsave(paste0(figures_path, "fig_1.png"), map_fig, width = 129, height = 150, units = "mm")

#Figure 2A
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
  scale_y_continuous(limits = c(-0.2, 3), breaks = c(0, 1, 2, 3), labels = c(1, 10, 100, 1000))+
  scale_x_continuous(breaks = c(-1, 0, 1, 2), labels = c(0.1, 1, 10, 100))+
  annotate("text", x=1.5, y=3, label = qr_eqn(qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=1.5, y=2.8, label = qr_eqn(qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=1.5, y=2.6, label = qr_eqn(qr_90, "0.9"), parse=TRUE)+
  ylab(expression(CO[2]~"("*mu*M*")"))+
  xlab(expression(Wetted~area~"(m"^{2}*")"))

#Figure 2B
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

#Figure 3A
slide_11_fig <- slide_11 %>% 
  ggplot(aes(`June-Aug`, `Sep-May`, shape = `Lake influence`)) +
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  scale_x_continuous(limits=c(1, 425))+
  scale_y_continuous(limits=c(1, 425))+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 1))+
  ylab(expression("Sep-May CO"[2]~"("*mu*M*")"))+
  xlab(expression("June-Aug CO"[2]~"("*mu*M*")"))

#Figure 3B
slide_13_fig <- slide_13 %>% 
  ggplot(aes(chl, co2))+
  geom_smooth(method = "lm", color="black")+
  geom_point(aes(shape=Time))+
  scale_y_log10()+
  scale_x_log10()+
  scale_shape_manual(values=c(1, 19))+
  ylab(expression("CO"[2]~"("*mu*M*")"))+
  xlab(expression("Chl. "*italic(a)~"("*mu*g~L^{-1}*")"))+
  scale_color_viridis_d()+
  annotate("text", x=50, y=100, label = lm_eqn(slide_13_lm2), parse=TRUE)

fig_3 <- slide_11_fig/slide_13_fig+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_3.png"), fig_3, width = 129, height = 180, units = "mm")

#Figure 4
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

#Figure 6A
slide_16_broken_axis <- slide_16 %>% 
  filter(flux > 300)

slide_16_fig <- slide_16 %>% 
  filter(flux < 300) %>% 
  ggplot(aes(a, flux, shape = `Lake influence`))+
  geom_hline(yintercept = 0, linetype=3)+
  geom_point()+
  geom_smooth(inherit.aes = FALSE, aes(a, flux), method = "loess", color="black")+
  scale_x_log10()+
  geom_point(data = slide_16_broken_axis, aes(a, 300), shape=1)+
  geom_text(data = slide_16_broken_axis, aes(a, 300, label=format(flux, digits = 0)), nudge_x = 0.2, size =3)+
  scale_shape_manual(values = c("Lake" = 19, "No lake" = 1))+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression("Wetted area (m"^{2}*")"))+
  theme(legend.position = c(0.85, 0.85))

#Figure 6B
slide_20_pred <- data.frame(wtr = seq(8, 23, 0.1)) %>% 
  mutate(pred_10 = predict(wtr_qr_10, newdata=.),
         pred_50 = predict(wtr_qr_50, newdata=.),
         pred_90 = predict(wtr_qr_90, newdata=.)) %>% 
  gather(pred, flux, -wtr) %>% 
  separate(pred, c("pred", "q"), "_") %>% 
  mutate(Quantile = factor(as.numeric(q)/100))

slide_16_wtr_fig <- slide_16 %>% 
  filter(flux < 300) %>% 
  ggplot(aes(wtr, flux)) +
  geom_point(shape=1)+
  geom_line(data=slide_20_pred, aes(col=Quantile), size=1.2)+
  scale_color_viridis_d()+
  geom_point(data = slide_16_broken_axis, aes(wtr, 300), shape=1)+
  geom_text(data = slide_16_broken_axis, aes(wtr, 300, label=format(flux, digits = 0)), nudge_x = 1, size =3)+
  annotate("text", x=19.5, y=260, label = qr_eqn(wtr_qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=19.5, y=230, label = qr_eqn(wtr_qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=19.5, y=200, label = qr_eqn(wtr_qr_90, "0.9"), parse=TRUE)+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab("Water temperature (°C)")

fig_6 <- slide_16_fig+slide_16_wtr_fig+plot_layout(guides = "collect", ncol=1)+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_6.png"), fig_6, width = 129, height = 180, units = "mm")

#Figure S1
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
  annotate("text", x=200, y=500, label = qr_eqn(co2_qr_10, "0.1"), parse=TRUE)+
  annotate("text", x=200, y=450, label = qr_eqn(co2_qr_50, "0.5"), parse=TRUE)+
  annotate("text", x=200, y=400, label = qr_eqn(co2_qr_90, "0.9"), parse=TRUE)+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression("CO"[2]~"("*mu*M*")"))+
  coord_cartesian(ylim = c(0, 505))

ggsave(paste0(figures_path, "fig_s1.png"), slide_16_co2_fig, width = 129, height = 100, units = "mm")

#Table 1
table_1_data %>% 
  mutate(co2 = 10^log_co2) %>% 
  group_by(name) %>% 
  summarise(n = n(), co2_mean = mean(co2), 
            co2_first = co2[which.min(location)], 
            co2_last = co2[which.max(location)]) %>% 
  write_csv(paste0(figures_path, "table_1.csv"))






#sammenhæng mellem co2 og discharge?
discharge <- read_csv(paste0(getwd(), "/data/sites_discharge.csv"))
sites_co2 <- read_tsv(paste0(getwd(), "/data/co2_sites_kaj.txt"))
catchment <- st_read(paste0(getwd(), "/data/gw_nonnest_clean.sqlite")) 

catchment_df <- catchment %>% 
  st_drop_geometry() %>% 
  select(name, total_area, mean_elev, mean_slope)

merge <- discharge %>% 
  left_join(sites_co2) %>% 
  left_join(catchment_df)

#Table 3
merge %>% 
  mutate(catchment_area = total_area*10^-6) %>%
  select(name, co2_flux, co2_flux_weight, index, alkalinity, catchment_area) %>% 
  write_csv(paste0(figures_path, "table_3.csv"))

tmp_data <- merge %>% 
  select(co2_flux, index, total_area) %>% 
  na.omit() 
 
summary(lm(co2_flux~index+log10(total_area), data = tmp_data))
