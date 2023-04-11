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
SHP_0 %>% 
  ggplot() +
  geom_sf()

tmap_mode("view")
tm_shape(SHP_0, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "lightblue", 
                             border.col = "black", lwd = 0.5)


SHP_0$NAME <- paste0(SHP_0$geo,"_", 1:nrow(SHP_0))

# select rows 

list = c('IT', 'ITA', 'FI', 'FIN', 'FR', 'FRA', 'DE', 'DEU', 'NL', 'NLD', 'SE', 'SWE', 'UK', 'GBR')


ss <- st_transform(SHP_0)

ss <- ss %>% dplyr::filter(geo %in% list)


tm_shape(ss, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "lightblue", 
                                                     border.col = "black", lwd = 0.5)

drzave <- data.frame(
  imena = c('Italy', 'Finland', 'France', 'Germany', 'Netherlands', 'Sweden', 'United Kingdom'),
  kratice = c('IT', 'FI', 'FR', 'DE', 'NL', 'SE', 'UK'))


ss <- ss %>% left_join(drzave, by = c('geo' = 'kratice')) 
ss <- ss %>% left_join(country_ca, by = c('imena' = 'COUNTRY')) 

tm_shape(ss, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "lightblue", 
                                                     border.col = "black", lwd = 0.5)






