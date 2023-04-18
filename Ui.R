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

library(gridExtra)
library(reshape2)
source("analiza.R")
##########################
##### User interface #####
##########################
ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  "Nutrition Calculator",
  
  tabPanel('Home', icon = icon("home"),
           HTML("
            <h5>Calculator tab</h5>
  <div style='display:flex;'>
    <div style='flex: 2;'>
      <p>It's intended for food intake, calculation of calories and nutrients, and analysis of daily nutrient intake. All food data on this tab is calculated from <a href='https://fdc.nal.usda.gov/download-datasets.html'>this API address</a>. Due to the huge data set, it is necessary to enter a search string when entering the food, based on which the offered set options are displayed.</p>
      <p>At the top of the page there are 4 infoboxes with the most important information about the total daily intake (calories value, amount of carbohydrates, proteins and fats), the food intake module is displayed on the left side, and the food intake table is written below. The main part is divided into two parts, which are separated as tabs:</p>
      <ol>
        <li>In the first tab (Nutritional Information), there are two tables. The first shows the nutritional value and energy value of the food entered in the left search module. The table is automatically recalculated according to the selected parameters in the module. The second has calculated nutrients of the total daily intake.</li>
        <li>In the second tab (Analyzed Nutrients), there is an analysis of daily nutrient intake. The graphs are colored according to the recommended daily intake levels as follows: <span style='color: yellow;'>yellow</span> if daily intake is below the minimum recommended value, <span style='color: #00FF00;'>green</span> if daily intake is between the minimum and maximum recommended values, and <span style='color: red;'>red</span> if daily intake is above the maximum recommended value.</li>
      </ol>
    </div>
    <div style='flex: 1;'>
      <img src='nutri.jpg', height=300, width=300>
    </div>
  </div>        <h5>Analysis tab</h5>
  <div style='display:flex;'>
    <div style='flex: 1;'>
      <img src='ana.jpg', height=300, width=300>
    </div>
    <p style='flex: 2;'>It's intended for the analysis of nutrients by country in each food at the first and second level according to the classification found on <a href='https://data.europa.eu/data/datasets/food-composition-database?locale=en'>this page</a>. Also, on this page there is an accessible file in excel format from which we draw data. The countries for which the data are selected are displayed on the map. In the table below, we can see more detailed data for the first and second level of food.</p>
  </div>")
  ),
  tabPanel('Calculator', icon = icon("calculator"),
           sidebarLayout(
             sidebarPanel(textInput("food_query", label = "Search for a food", placeholder = "Enter a food name..."),
                          verbatimTextOutput("recommended_foods"),
                          selectInput("food_name", label = "Select a food", choices = c(), selected = ""),
                          
                          textInput("quantity", label="Select quantity (in grams)", value= 100, placeholder= "Enter food quantity (in grams)"),
                          selectInput("gender", label="Select gender", choices = c("Male", "Female")),
                          actionButton("add_food", "Add food")
                          
             ),
             mainPanel(
               fluidRow(
                 valueBoxOutput("caloriesBox", width=3),
                 valueBoxOutput("carbohydrateBox", width=3),
                 valueBoxOutput("proteinBox", width=3),
                 valueBoxOutput("lipidBox", width=3)),
               
               tabsetPanel(
                 id = "tabs",
                 tabPanel("Nutrition Info", 
                          splitLayout(verticalLayout(
                           
                                   div(class = "card border-light mb-3", style = "width: 100%; padding: 0 15px;",
                                       div(class = "card-header", "Added food"),
                                       div(class = "card-body",
                                           DT::DTOutput("food_table"),style = "font-size: 70%;"
                                       )
                                   )
                            , div(class = "card border-light mb-3", style = "width: 100%; padding: 0 15px;",
                                       div(class = "card-header", "Current food nutrition table"),
                                       div(class = "card-body",
                                           DT::DTOutput("nutrition_table"),style = "font-size: 70%;"
                                       ) )),
                            
                                  
                              
                                   div(class ="card border-light mb-3", style = "width: 100%; padding: 0 15px;",
                                       div(class = "card-header", "Total nutrients table"),
                                       div(class = "card-body",
                                           DT::DTOutput("total_nutrients_table"),style = "font-size: 70%;"
                                       )
                                   )
                            
                          ),
                          style = "width: 100%;"
                 )
                 
                 
                 ,  # closing parenthesis for tabPanel
                 
                          
                          
                 
                 tabPanel("Analyzed Nutrients", 
                          fluidRow(
                            column(width = 6,
                                   tagList(
                                     reactableOutput("general_plot"), 
                                     reactableOutput("vitamins_plot"),
                                     reactableOutput("lipids_plot")
                                   )
                            ),
                            column(width = 6,
                                   tagList(
                                     reactableOutput("minerals_plot"),
                                     reactableOutput("Protein_plot"),
                                     reactableOutput("carbohydrate_plot")
                                   )
                            )
                          )
                 )
                 
                 
               
             )))),
  
  tabPanel('Analysis', icon = icon("table"),
            selectInput("var2", "Choose nutrient", nutrienti_vsi2, width="500px"),
           textOutput("unit1"),
            tmapOutput("map2"),
            reactableOutput('table2')
  )
)

