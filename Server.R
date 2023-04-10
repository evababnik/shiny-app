
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(eurostat)
library(tmap)
library(reactable)

###########################
##### Server function #####
###########################
server <- function(input, output, session) {

  ss <- naredi_zemljevid(country, 'Alpha-tocopherol')
  

  output$map2 <- renderTmap({
    tm_shape(ss, bbox = c(-15, 45, 45, 50)) +
      tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5, zindex = 401)
  })

  observe({
    var <- input$var2
    zem <- naredi_zemljevid(country, var)

    tmapProxy("map2", session, {
      tmap_mode("view")
      tm_remove_layer(401) +
        tm_shape(zem, bbox = c(-15, 45, 45, 50)) +
        tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5, zindex = 401) +
        tm_fill( popup.vars=c('NUTRIENT_TEXT', 'mean_value'))
    })
  })
  
  output$table2 <- renderReactable ({
    var <- input$var2
    tab1 <- tabela1(country, var) 
    tab2 <- tabela2(country, var)
    reactable(tab1,
              filterable = TRUE,
              resizable = TRUE,
              compact = TRUE,
              details = function(index) {
                coun <- tab2 %>% filter(level1 == tab1$level1[index]) 
                coun2 <- coun  %>% data.frame() %>% select(-c('level1'))
                tbl <- reactable(coun2, outlined = TRUE, highlight = TRUE, fullWidth = TRUE)
                htmltools::div(style = list(margin = "12px 100px"), tbl)
              },
              onClick = "expand",
              rowStyle = list(cursor = "pointer"),
              defaultPageSize = 30
              )
  })
  
}