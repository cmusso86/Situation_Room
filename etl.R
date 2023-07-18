rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf , rio, ggthemes)


## BASES ----

data_raw <-  import("disease.csv")


summary(data_raw)

vac_region <- data_raw %>% 
  group_by(region) %>% 
  summarise(vaccnine = mean(vaccination=="yes")*100)


## MAP -----

map <- st_read("map.gpkg")
st_crs(map) <- 4326
map_transformed <- st_transform(map, crs = 4326)

(ggmap <- spatial_data_transformed %>% 
  ggplot()+
  geom_sf(aes(geometry=geom, fill=hdi))+
  scale_fill_distiller(direction=1)+
  theme_map())


## other plot
plot(map["hdi"])