library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)

SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

drzave <- data.frame(
  imena = c('Italy', 'Finland', 'France', 'Germany', 'Netherlands', 'Sweden', 'United Kingdom'),
  kratice = c('IT', 'FI', 'FR', 'DE', 'NL', 'SE', 'UK'))

ss <- st_transform(SHP_0)
ss <- ss %>% dplyr::filter(geo %in% drzave$kratice)
ss <- ss %>% left_join(drzave, by = c('geo' = 'kratice')) 
ss <- ss %>% left_join(country_ca, by = c('imena' = 'COUNTRY')) 
ss <- ss %>% select(geometry, imena, NUTRIENT_TEXT, mean_nutri)

tmap_mode("view")
carta <-tm_shape(ss, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "mean_nutri", border.col = "black", lwd = 0.5) + 
  tm_fill( popup.vars=c('NUTRIENT_TEXT', 'mean_nutri'))
tmap_leaflet(carta)



