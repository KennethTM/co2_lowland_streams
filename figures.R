source("libs_and_funcs.R")

#Figures

#slide 5
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
  ylab(expression(CO[2]~"("*mu*mol~L^{-1}*")"))+
  xlab(expression(Wetted~area~"(m"^{2}*")"))

#slide 6
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
  ylab(expression("Afternoon"~CO[2]~"("*mu*mol~L^{-1}*")"))+
  xlab(expression("Morning"~CO[2]~"("*mu*mol~L^{-1}*")"))

fig_2 <- slide_5_fig/slide_6_fig+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_2.png"), fig_2, width = 129, height = 180, units = "mm")

#slide 7, 8, 9
slide_7_8 <- read_excel(rawdata_path, sheet = "slide_7_8") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening,
         chl = 10^log_chl)

# slide_7 <- bind_rows(data.frame(wtr = slide_7_8$wtr_morning, co2 = slide_7_8$co2_morning, time = "morning"),
#                      data.frame(wtr = slide_7_8$wtr_evening, co2 = slide_7_8$co2_evening, time = "evening"))
# 
# slide_7 %>% 
#   ggplot(aes(wtr, co2, col=time)) +
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_y_log10()+
#   ylab(expression(log[10]*"(CO"[2]~"["*mu*mol~L^{-1}*"])"))+
#   xlab("Water temperature (°C)")+
#   scale_color_viridis_d()
# 
# slide_8 <- slide_7_8 %>% 
#   select(co2_morning, co2_evening, chl) %>% 
#   gather(var_time, co2, -chl) %>% 
#   separate(var_time, c("var", "time"), "_")
# 
# slide_8 %>% 
#   ggplot(aes(chl, co2, col=time))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_y_log10()+
#   scale_x_log10()+
#   ylab(expression(log[10]*"(CO"[2]~"["*mu*mol~l^{-1}*"])"))+
#   xlab(expression(log[10]*"(Chl. a [mg"~L^{-1}*"])"))+
#   scale_color_viridis_d()
# 
# #slide 9
# slide_7_8 %>% 
#   ggplot(aes(wtr_morning, chl)) +
#   geom_point()+
#   scale_y_log10()+
#   ylab(expression(log[10]*"(Chl. a [mg"~L^{-1}*"])"))+
#   xlab("Water temperature (°C)")

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

png(paste0(figures_path, "fig_4.png"), width = 129, height = 129, units = "mm", res = 300)
scatter3D(x, y, z, pch = 19, cex = 1, colvar = NULL, col="black",
          theta = 120, phi = 20, bty="b", ticktype="detailed", #ticktype="simple"
          xlab = "log10(Chl. a)", ylab = "Water temp.", zlab = "log10(CO2)",
          surf = list(x = x.pred, y = y.pred, z = z.pred, facets = TRUE, fit = fitpoints,
                      col=ramp.col(col = c("seagreen1", "dodgerblue3"), n = 300, alpha=0.5))) #, border="black", 
dev.off()

#slide 10
slide_10 <- read_excel(rawdata_path, sheet = "slide_10") %>% 
  na.omit() %>% 
  mutate(co2 = 10^log_co2,
         date = ymd("1995-01-01")+time) %>% 
  rename(Site = site) %>% 
  filter(Site != "Pølebro 2")

fig_5 <- slide_10 %>% 
  ggplot(aes(date, co2, col=Site))+
  geom_point(size=0.7)+
  geom_line()+
  scale_y_log10()+
  ylab(expression(CO[2]~"("*mu*mol~L^{-1}*")"))+
  scale_x_date(date_breaks = "3 month", date_labels = "%b")+
  xlab("Month")+
  scale_color_brewer(palette = "Dark2")

ggsave(paste0(figures_path, "fig_5.png"), fig_5, width = 129, height = 84, units = "mm")

#slide 11
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
  ylab(expression("Sep-May CO"[2]~"("*mu*mol~L^{-1}*")"))+
  xlab(expression("June-Aug CO"[2]~"("*mu*mol~L^{-1}*")"))

#slide 13
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
  ylab(expression("CO"[2]~"("*mu*mol~l^{-1}*")"))+
  xlab(expression("Chl. "*italic(a)~"(mg"~L^{-1}*")"))+
  scale_color_viridis_d()+
  annotate("text", x=50, y=100, label = lm_eqn(slide_13_lm2), parse=TRUE)

fig_3 <- slide_11_fig/slide_13_fig+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_3.png"), fig_3, width = 129, height = 180, units = "mm")

#slide 16-21
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

#slide 20
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

#slide 21
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
  xlab(expression("CO"[2]~"("*mu*mol~l^{-1}*")"))

ggsave(paste0(figures_path, "fig_s1.png"), slide_16_co2_fig, width = 129, height = 100, units = "mm")

#table ?? statistics
