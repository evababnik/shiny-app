library(shiny)
library(httr)
library(dplyr)
library(readr)
library(readxl)


daily_intake <-  read_excel("rdv.xlsx")

# funkcija za poizvedbo API-ja
get_foods <- function(query) {
  url <- "https://api.nal.usda.gov/fdc/v1/foods/search"
  params <- list(
    generalSearchInput = query,
    api_key = "kHbFWekgGusKc2LfzKp4dWpa6508yiI2Qx2IPLs8"
  )
  response <- httr::GET(url, query = params)
  foods <- httr::content(response, "parsed")$foods
  
  return(foods)
}


# UI
ui <- fluidPage(
  titlePanel("Nutrition Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("food_name", label = "Select a food", choices = c(), selected = ""),
      tags$hr(),
      h5("Search for a food"),
      textInput("food_query", label = NULL, placeholder = "Enter a food name..."),
      verbatimTextOutput("recommended_foods"),
      textInput("quantity", label="Select quantity", placeholder= "Enter food quantity (in grams)"),
      selectInput("gender", label="Select gender", choices = c("Male", "Female")),
      actionButton("add_food", "Add food") 
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Nutrition Info", tableOutput("nutrition_table")),
        tabPanel("Total Nutrients", tableOutput("total_nutrients_table")) # Dodajanje nove tabele za prikaz seštevka hranilnih snovi
      )
    )
  )
)

# Server
# Server
server <- function(input, output, session) {
  # reagiraj na spremembe v iskalnem nizu
  recommended_foods <- reactive({
    query <- input$food_query
    if (nchar(query) > 0) {
      foods <- get_foods(query)
      best_food <- foods[[1]]$description
      
      recommended_foods <- unlist(lapply(foods[1:5], function(x) x$description))
      
    } else {
      recommended_foods <- character(0)
    }
    return(recommended_foods)
  })
  
  # prikaži priporočena živila
  output$recommended_foods <- renderPrint({
    recommended_foods()
  })
  
  # nastavi izbirnik živil
  observe({
    choices <- if (nchar(input$food_query) > 0) {
      unlist(lapply(get_foods(input$food_query), function(x) x[["description"]]))
      
    } else {
      character(0)
    }
    updateSelectInput(session, "food_name", choices = choices, selected = "")
  })
  
  # Dodaj hrano
  added_foods <- reactiveValues(data = NULL)
  observeEvent(input$add_food, {
    if (input$food_name != "" & input$quantity != "") {
      food <- get_foods(input$food_name)[[1]]
      nutrients <- food$foodNutrients
      nutrients_df <- data.frame(
        nutrient = sapply(nutrients, function(x) x$nutrientName),
        value = sapply(nutrients, function(x) x$value),
        unit = sapply(nutrients, function(x) x$unitName),
        stringsAsFactors = FALSE
      )
      nutrients_df$value <- nutrients_df$value * (as.numeric(input$quantity)/ 100)
      nutrients_df$value <- round(nutrients_df$value, digits = 2)
      
      added_foods$data <- rbind(added_foods$data, nutrients_df)
    }
  })
  
  # prikaži tabelo z vrednostmi hranilnih snovi za posamezno izbrano hrano
  output$nutrition_table <- renderTable({
    if (input$food_name != "") {
      food <- get_foods(input$food_name)[[1]]
      nutrients <- food$foodNutrients
      nutrients_df <- data.frame(
        nutrient = sapply(nutrients, function(x) x$nutrientName),
        value = sapply(nutrients, function(x) x$value),
        unit = sapply(nutrients, function(x) x$unitName),
        stringsAsFactors = FALSE
      )
      nutrients_df$value <- nutrients_df$value * (as.numeric(input$quantity)/ 100)
      nutrients_df$value <- round(nutrients_df$value, digits = 2)
      
      return(nutrients_df)
    }
  })
  
  # prikaži tabelo s seštevkom hranilnih snovi za dodana živila
  output$total_nutrients_table <- renderTable({
    if (!is.null(added_foods$data)) {
      total_nutrients_df <- added_foods$data %>%
        group_by(nutrient) %>%
        summarise(total_value = sum(value)) %>%
        mutate(total_value = round(total_value, digits=2))
      return(total_nutrients_df)
    } })
  
  ##pokazi grafe za glavne hranilne snovi
}
               


   


shinyApp(ui = ui, server = server)



