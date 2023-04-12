
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(tmap)

###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  
  output$map <- renderTmap({
    
    ca <- naredi_zemljevid2(country, 'Calcium (Ca)')
    
    tm_shape(ca, bbox = c(-15, 45, 45, 50))+ tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5) + 
      tm_fill( popup.vars=c('NUTRIENT_TEXT', 'mean_value'))
    
  })
 
}