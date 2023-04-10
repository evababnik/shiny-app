library(dplyr)
library(readr)
library(readxl)

library(grid)
library(tidyverse)
library(shadowtext)
library(ggplot2)

library(sf)
library(leaflet)
library(tmap)
library(rgdal)
library(geojsonio)

FCD <- read_excel("Food_composition_dataset.xlsx")

country <- FCD %>% group_by(COUNTRY, NUTRIENT_TEXT, level1, level2)%>% summarize(mean_nutri = mean(LEVEL))
country_ca <- country %>% dplyr::filter(NUTRIENT_TEXT == 'Calcium (Ca)') 


data("World")
tmap_mode("view")
tm_shape(World) + tm_polygons(col = "pop_development", midpoint = 0)                              
                                 
                            
# library(shiny)
# library(tmap)
# data('World')
# ui <- fluidPage(
#   tmapOutput('map')
# )
# server <- function(input, output, session) {
#   output$map <- renderTmap({
#     qtm(World, 'name')})
# }
# shinyApp(ui, server)


nycounties <- rgdal::readOGR("europe-outline-with-countries_223.geojson") 

pal <- colorNumeric("viridis", NULL)

leaflet(nycounties) %>%
  addTiles() 


library(tmap)
data(europe)
tm_shape(europe)+ tm_fill()



require(remotes)
install_version("tmap", version = "1.11-2", repos = "http://cran.us.r-project.org")



