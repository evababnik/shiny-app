library(readxl)
library(eurostat)
library(sf)
library(leaflet)
library(tmap)

FCD <- read_excel("Food_composition_dataset.xlsx")

drzave <- data.frame(
  imena = c('Italy', 'Finland', 'France', 'Germany', 'Netherlands', 'Sweden', 'United Kingdom'),
  kratice = c('IT', 'FI', 'FR', 'DE', 'NL', 'SE', 'UK'))

country <- FCD %>% group_by(COUNTRY, NUTRIENT_TEXT, level1, level2)%>% summarize(mean_nutri = mean(LEVEL)) %>%
  left_join(drzave, by = c('COUNTRY'= 'imena'))

nutrienti_vsi <- unique(country$NUTRIENT_TEXT)

SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

naredi_zemljevid <- function(podatki, nutrient) {
  podatki <- podatki %>% group_by(COUNTRY, kratice, NUTRIENT_TEXT) %>% summarize(mean_value = mean(mean_nutri)) %>%
    dplyr::filter(NUTRIENT_TEXT == nutrient)

  ss <- st_transform(SHP_0)
  ss <- ss %>% dplyr::filter(geo %in% drzave$kratice) %>% left_join(podatki, by = c('geo' = 'kratice')) %>%
    select(geometry, COUNTRY, NUTRIENT_TEXT, mean_value)
  
  tmap_mode("view")
  carta <-tm_shape(ss, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5) + 
    tm_fill( popup.vars=c('NUTRIENT_TEXT', 'mean_value'))
  
  return(tmap_leaflet(carta))
}

tabela1 <- function(podatki, drzava, nutrient) {
  podatki <- podatki %>% dplyr::filter(NUTRIENT_TEXT == nutrient & COUNTRY == drzava) %>%
    group_by(level1) %>% summarize(mean_value = mean(mean_nutri))
  
    return(view(podatki))
}

tabela2 <- function(podatki, drzava, nutrient) {
  podatki <- podatki %>% dplyr::filter(NUTRIENT_TEXT == nutrient & COUNTRY == drzava) %>%
    group_by(level2) %>% summarize(mean_value = mean(mean_nutri))
  
  return(view(podatki))
}


naredi_zemljevid2 <- function(podatki, nutrient) {
  podatki <- podatki %>% group_by(COUNTRY, kratice, NUTRIENT_TEXT) %>% summarize(mean_value = mean(mean_nutri)) %>%
    dplyr::filter(NUTRIENT_TEXT == nutrient)
  
  ss <- st_transform(SHP_0)
  ss <- ss %>% dplyr::filter(geo %in% drzave$kratice) %>% left_join(podatki, by = c('geo' = 'kratice')) %>%
    select(geometry, COUNTRY, NUTRIENT_TEXT, mean_value)
  
  tmap_mode("view")
  carta <-tm_shape(ss, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5) + 
    tm_fill( popup.vars=c('NUTRIENT_TEXT', 'mean_value'))
  
  return(ss)
}

ca <- naredi_zemljevid2(country, 'Calcium (Ca)')

tm_shape(ca, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5) + 
  tm_fill( popup.vars=c('NUTRIENT_TEXT', 'mean_value'))
