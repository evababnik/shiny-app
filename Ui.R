# INSERT Libraries HERE

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

##########################
##### User interface #####
##########################
ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  "Nutrition Calculator",
  
  tabPanel('Domov', icon = icon("home"),
           h2("Food composition in some EU countries"),
           p("Na zavihku Domov se nahajajo navodila za uporabo aplikacije in njeno delovanje, na prvo stran se lahko shranjuje tudi 
             dnevne podatke o zavžitih kalorijah"),
           p("Na zavihku Kalkulator..."),
           p("Na zavihku Analiza je na voljo zemljevid in tabela z nivojem 1 in 2 živil, ki vsebujejo izbrani nutrient")
  ),
  
  tabPanel('kalkulator', icon = icon("calculator"),
           sidebarLayout(
             sidebarPanel(textInput("food_query", label = "Search for a food", placeholder = "Enter a food name..."),
                          verbatimTextOutput("recommended_foods"),
                          selectInput("food_name", label = "Select a food", choices = c(), selected = ""),
                          
                          textInput("quantity", label="Select quantity", placeholder= "Enter food quantity (in grams)"),
                          selectInput("gender", label="Select gender", choices = c("Male", "Female")),
                          actionButton("add_food", "Add food"),
                          DT::DTOutput("food_table")
                          
             ),
             mainPanel(
               tabsetPanel(
                 id = "tabs",
                 tabPanel("Nutrition Info", splitLayout(tableOutput("nutrition_table"),
                                                        tableOutput("total_nutrients_table"))),
                          
                          
                 
                 tabPanel("Analyzed Nutrients",
                          fluidRow(
                            column(width = 4, plotOutput("general_plot")),
                            column(width = 4, plotOutput("minerals_plot"))
                            
                          ),
                          fluidRow(
                            column(width = 4, plotOutput("vitamins_plot")),
                            column(width = 4, plotOutput("Protein_plot"))
                            
                          ),
                          fluidRow(
                            
                            column(width = 4, plotOutput("lipids_plot")),
                            column(width = 4, plotOutput("carbohydrate_plot"))
                          )
                          
                 )
                 
               
             )))),
  
  tabPanel('Analiza', icon = icon("table"),
           #  selectInput("var2", "Variable2", nutrienti_vsi2),
           #  tmapOutput("map2"),
           #  reactableOutput('table2')
  )
)

