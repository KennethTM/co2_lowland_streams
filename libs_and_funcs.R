#Libraries and functions

library(raster);library(tidyverse);library(lubridate);library(readxl);library(patchwork);library(sf)

#Figure sizing. For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.

rawdata_path <- paste0(getwd(), "/data/data.xlsx")
figures_path <- paste0(getwd(), "/figures/")

#ggplot theme used for figures
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)