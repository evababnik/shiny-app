# INSERT Libraries HERE

library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(bslib)

##########################
##### User interface #####
##########################
ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  "Kalkulator vnosa makrohranil",
  
  tabPanel('Domov', icon = icon("home"),
           h2("Food composition in some EU countries"),
           p("Na zavihku Domov se nahajajo navodila za uporabo aplikacije in njeno delovanje, na prvo stran se lahko shranjuje tudi 
             dnevne podatke o zavžitih kalorijah"),
           p("Na zavihku Kalkulator..."),
           p("Na zavihku Analiza je na voljo zemljevid in tabela z nivojem 1 in 2 živil, ki vsebujejo izbrani nutrient")
          ),
  
  tabPanel('kalkulator', icon = icon("calculator")),
  
  tabPanel('Analiza', icon = icon("table"),
           selectInput("var2", "Variable2", nutrienti_vsi2),
           tmapOutput("map2"),
           reactableOutput('table2'))
)




  
  


