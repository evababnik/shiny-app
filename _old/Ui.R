# INSERT Libraries HERE

library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinydashboard)

##########################
##### User interface #####
##########################
ui <- fluidPage(
  
  header <- dashboardHeader(
    title = "Kalkulator vnosa makrohranil",
    titleWidth = 420
  ),
  
  sidebar <- dashboardSidebar(
    width = 420,
    sidebarMenu(
      tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")),   # to je potrebno vnesti v css datoteko
      menuItem("Domov", tabName="domov", icon = icon("home")),
      menuItem("kalkulator", tabName = "kalk", icon = icon("calculator")),
      menuItem("tabele", tabName="tab", icon = icon("table"))
    )
  ),
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName="kalk",
              fluidRow(
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                ),
                box(plotOutput("plot1", height = 250))
              )),
      
      
      tabItem(tabName="domov",
              h2("Tamplate za 1. seminarsko nalogo"),
              p("Na zavihku kalkulator graf ne deluje, koda je pravilno spisana, nekaj je narobe s povezavo."),
              tmapOutput(outputId = "map")),
      
      
      tabItem(tabName="tab",
              selectInput(inputId = 'nutrient', label = 'Nutrient', choices = sort(country$NUTRIENT_TEXT))
              ))
  ),
  
  dashboardPage(skin = "yellow", header, sidebar, body))

