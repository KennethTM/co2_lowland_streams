#Libraries and functions

library(raster);library(tidyverse);library(lubridate);library(readxl);library(patchwork);library(sf)
library(quantreg);library(plot3D)

#Figure sizing. For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.

Sys.setlocale("LC_TIME", "English")

rawdata_path <- paste0(getwd(), "/data/data.xlsx")
figures_path <- paste0(getwd(), "/figures/")

#ggplot theme used for figures
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

lm_eqn <- function(m){
  eq <- substitute(italic(y) == a~sign~b*italic(x)~"("*italic(R)^2~"="~r2*")", 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(abs(unname(coef(m)[2])), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 2),
                        sign = ifelse(format(unname(coef(m)[2]), digits = 2) < 0, "-", "+")))
  as.character(as.expression(eq))
}

qr_eqn <- function(m, tau){
  eq <- substitute(italic(y)[q] == a~sign~b*italic(x), 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(abs(unname(coef(m)[2])), digits = 2),
                        q = tau,
                        sign = ifelse(format(unname(coef(m)[2]), digits = 2) < 0, "-", "+")))
  as.character(as.expression(eq))
}
