source("libs_and_funcs.R")

#Figures

#slide 5
slide_5 <- read_excel(rawdata_path, sheet = "slide_5") %>% 
  na.omit() %>% 
  mutate(a = 10^log_a,
         co2 = 10^log_co2)

slide_5 %>% 
  ggplot(aes(a, co2)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  ylab(expression(log[10]*"(CO"[2]~"["*mu*mol~L^{-1}*"])"))+
  xlab(expression(log[10]*"(wetted area [m"^{2}*"])"))

#slide 6
slide_6 <- read_excel(rawdata_path, sheet = "slide_6") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening)

slide_6 %>% 
  ggplot(aes(co2_morning, co2_evening)) +
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  scale_x_log10(limits=c(1, 1000))+
  scale_y_log10(limits=c(1, 1000))+
  ylab(expression("Evening"~log[10]*"(CO"[2]~"["*mu*mol~L^{-1}*"])"))+
  xlab(expression("Morning"~log[10]*"(CO"[2]~"["*mu*mol~L^{-1}*"])"))

#slide 7, 8, 9
slide_7_8 <- read_excel(rawdata_path, sheet = "slide_7_8") %>% 
  na.omit() %>% 
  mutate(co2_morning = 10^log_co2_morning,
         co2_evening = 10^log_co2_evening,
         chl = 10^log_chl)

slide_7 <- bind_rows(data.frame(wtr = slide_7_8$wtr_morning, co2 = slide_7_8$co2_morning, time = "morning"),
                     data.frame(wtr = slide_7_8$wtr_evening, co2 = slide_7_8$co2_evening, time = "evening"))

slide_7 %>% 
  ggplot(aes(wtr, co2, col=time)) +
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_log10()+
  ylab(expression(log[10]*"(CO"[2]~"["*mu*mol~L^{-1}*"])"))+
  xlab("Water temperature (°C)")+
  scale_color_viridis_d()

slide_8 <- slide_7_8 %>% 
  select(co2_morning, co2_evening, chl) %>% 
  gather(var_time, co2, -chl) %>% 
  separate(var_time, c("var", "time"), "_")

slide_8 %>% 
  ggplot(aes(chl, co2, col=time))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_log10()+
  scale_x_log10()+
  ylab(expression(log[10]*"(CO"[2]~"["*mu*mol~l^{-1}*"])"))+
  xlab(expression(log[10]*"(Chl. a [mg"~L^{-1}*"])"))+
  scale_color_viridis_d()

#slide 9
slide_7_8 %>% 
  ggplot(aes(wtr_morning, chl)) +
  geom_point()+
  scale_y_log10()+
  ylab(expression(log[10]*"(Chl. a [mg"~L^{-1}*"])"))+
  xlab("Water temperature (°C)")

#slide 10
slide_10 <- read_excel(rawdata_path, sheet = "slide_10") %>% 
  na.omit() %>% 
  mutate(co2 = 10^log_co2)

slide_10 %>% 
  ggplot(aes(time, co2, col=site))+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  ylab(expression(log[10]*"(CO"[2]~"["*mu*mol~l^{-1}*"])"))+
  scale_color_brewer(palette = "Dark2")

#slide 11
slide_11 <- read_excel(rawdata_path, sheet = "slide_11") %>% 
  na.omit() %>% 
  mutate(lake = factor(lake))

slide_11 %>% 
  ggplot(aes(`June-Aug`, `Sep-May`, shape = lake)) +
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  scale_shape_manual(values = c("1" = 1, "0" = 19))+
  ylab(expression("Sep-May CO"[2]~"("*mu*mol~L^{-1}*")"))+
  xlab(expression("June-Aug CO"[2]~"("*mu*mol~L^{-1}*")"))

#slide 13
slide_13 <- read_excel(rawdata_path, sheet = "slide_13") %>% 
  gather(var_time, co2, -chl, -name, -position) %>% 
  separate(var_time, c("var", "time"), "_") %>% 
  na.omit()

slide_13 %>% 
  ggplot(aes(chl, co2, col=position))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_log10()+
  scale_x_log10()+
  ylab(expression(log[10]*"(CO"[2]~"["*mu*mol~l^{-1}*"])"))+
  xlab(expression(log[10]*"(Chl. a [mg"~L^{-1}*"])"))+
  scale_color_viridis_d()

#slide 16-21
slide_16 <- read_excel(rawdata_path, sheet = "slide_16_21")

slide_16 %>% 
  ggplot(aes(a, flux, col = position))+
  geom_hline(yintercept = 0, linetype=3)+
  geom_point()+
  scale_x_log10()+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression(log[10]*"(wetted area [m"^{2}*"])"))

#slide 20
slide_16 %>% 
  ggplot(aes(wtr, flux)) +
  geom_point()+
  geom_smooth(method="lm")+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab("Water temperature (°C)")

#slide 21
slide_16 %>% 
  ggplot(aes(co2_morning, flux)) +
  geom_point()+
  geom_smooth(method="lm")+
  ylab(expression("CO"[2]~"flux (mg C m"^{-2}~h^{-1}*")"))+
  xlab(expression("(CO"[2]~"["*mu*mol~l^{-1}*"])"))
