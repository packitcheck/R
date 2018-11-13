rm(list=ls())

# inlezen benodigde packages
library(tidyverse)
library(lubridate)
library(BSDA)
library(scales)
library(ggmap)

# geen wetenschappelijke notatie toestaan
options(scipen=999)

pad_2_data_dh_2006_2015 <- "/home/shaam/Pri/R/eindopdr8_R/eindopr8/ongevallen_dh_2006_2015.csv"

ongevallen_dh_2006_2015 <- read.csv(pad_2_data_dh_2006_2015, header = TRUE, sep = ",")

ongevallen_dh_2006_2015$Dag <- factor(ongevallen_dh_2006_2015$Dag ,levels = c("ZO", "MA", "DI", "WO", "DO", "VR", "ZA")) # analyse per dag

map_dh_zoom10 <-get_map(location = "The+Haque", zoom = 10)

ggmap(map_dh_zoom10, base_layer = ggplot(aes(x = x, y = x, size = duration, data = ongevallen_dh_2006_2015))  + geom_point(color="yellow",alpha=0.3)

qmplot(ongevallen_dh_2006_2015$Longitude, ongevallen_dh_2006_2015$Latitude, data = Kaart_zoom10, maptype = "toner-background", color = offense) + 
  facet_wrap(~ ongevallen_dh_2006_2015$OngevalID)
