library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(bslib)
library(tmap)
library(reactable)
library(shiny)



library(readxl)
library(eurostat)
library(sf)
library(leaflet)


library(reshape2)


FCD <- read_excel("Food_composition_dataset.xlsx")

drzave <- data.frame(
  imena = c('Italy', 'Finland', 'France', 'Germany', 'Netherlands', 'Sweden', 'United Kingdom'),
  kratice = c('IT', 'FI', 'FR', 'DE', 'NL', 'SE', 'UK'))

country <- FCD %>% group_by(COUNTRY, NUTRIENT_TEXT, level1, level2)%>% summarize(mean_nutri = mean(LEVEL)) %>%
  left_join(drzave, by = c('COUNTRY'= 'imena'))


nutrienti_vsi2 <- unique(country$NUTRIENT_TEXT)

SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)


naredi_zemljevid <- function(podatki, nutrient) {
  podatki <- podatki %>% group_by(COUNTRY, kratice, NUTRIENT_TEXT) %>% summarize(mean_value = mean(mean_nutri)) %>%
    dplyr::filter(NUTRIENT_TEXT == nutrient)

  ss <- sf::st_transform(SHP_0)
  ss <- ss %>% dplyr::filter(geo %in% drzave$kratice) %>% left_join(podatki, by = c('geo' = 'kratice')) %>%
    select(geometry, COUNTRY, NUTRIENT_TEXT, mean_value) %>% drop_na(COUNTRY)

  return(ss)
}

tabela1 <- function(podatki, nutrient) {
  podatki <- podatki %>% dplyr::filter(NUTRIENT_TEXT == nutrient) %>% 
    group_by(COUNTRY, level1) %>% summarize(mean_value = mean(mean_nutri)) %>% mutate_if(is.numeric, round, digits = 2) 
  podatki <- podatki %>% spread(key = 'COUNTRY', value = 'mean_value', fill = 0)
  
    return(podatki)
}


tabela2 <- function(podatki, nutrient) {
  podatki <- podatki %>% dplyr::filter(NUTRIENT_TEXT == nutrient) %>%
    group_by(COUNTRY, level1, level2) %>% summarize(mean_value = mean(mean_nutri)) %>% mutate_if(is.numeric, round, digits = 2)
  podatki <- podatki %>% spread(key = 'COUNTRY', value = 'mean_value', fill = 0)
  
  return(podatki)
}

#____________________________

un <- FCD %>% select('NUTRIENT_TEXT', 'UNIT')
un <- unique(un)





