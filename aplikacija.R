library(shiny)
library(httr)
library(dplyr)
library(readr)
library(readxl)

library(grid)
library(tidyverse)
library(shadowtext)
library(ggplot2)

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
    sidebarPanel(textInput("food_query", label = "Search for a food", placeholder = "Enter a food name..."),
      verbatimTextOutput("recommended_foods"),
      selectInput("food_name", label = "Select a food", choices = c(), selected = ""),
      
    
      
      textInput("quantity", label="Select quantity", placeholder= "Enter food quantity (in grams)"),
      selectInput("gender", label="Select gender", choices = c("Male", "Female")),
      actionButton("add_food", "Add food") 
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Nutrition Info", tableOutput("nutrition_table")),
        tabPanel("Total Nutrients", tableOutput("total_nutrients_table")),
        tabPanel("Minerals", plotOutput("minerals_plot")),
        tabPanel("Vitamins", plotOutput("vitamins_plot")),
        tabPanel("General", plotOutput("general_plot")),
        tabPanel("Proteins", plotOutput("Protein_plot")),
        tabPanel("Lipids", plotOutput("lipids_plot")),
        tabPanel("Carbohydrate", plotOutput("carbohydrate_plot")))
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
  
  total_nutrients_df <- reactive({
    if (!is.null(added_foods$data)) {
      total_nutrients_df <- added_foods$data %>%
        group_by(nutrient) %>%
        summarise(total_value = sum(value)) %>%
        mutate(total_value = round(total_value, digits=2))
      return(total_nutrients_df)
    } 
  })
  
  output$total_nutrients_table <- renderTable({
    total_nutrients_df() 
  })
 
  ##pokazi grafe za glavne hranilne snovi
  ##minerals
  
  
  output$minerals_plot <- renderPlot({
    if(!is.null(added_foods$data)){
      
     
      minerals <- daily_intake %>% filter(Type=="Mineral" & (Gender==input$gender | Gender == "Both"))
      minerals <- merge(minerals,total_nutrients_df(), by="nutrient", all.x=TRUE)
      minerals$"Average Daily Intake" <- as.numeric(minerals$"Average Daily Intake")
      minerals$"Min Daily Intake" <- as.numeric(minerals$"Min Daily Intake")
      minerals$"Max Daily Intake" <- as.numeric(minerals$"Max Daily Intake")
      minerals$total_value <- ifelse(is.na(minerals$total_value),0, minerals$total_value)

      minerals$percentage <- ifelse(is.na(minerals$"Average Daily Intake"),minerals$total_value , minerals$total_value / (minerals$"Average Daily Intake") * 100)
    
      colours <- minerals %>%
        mutate(
          colours = case_when(
            is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
            `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
            TRUE ~ "green"
          )
        ) %>%
        pull(colours)
    
     
     plot<-  ggplot(minerals) +
        geom_col(aes(percentage, nutrient), fill = colours) +
        scale_fill_identity() +
        labs(x = "Nutrient", y = "Total value (%)")
      return(plot)
    }
  })
  
  


##vitamins

output$vitamins_plot <- renderPlot({
  if(!is.null(added_foods$data)){
    
    
  vitamins <- daily_intake %>% filter(Type=="Vitamin" & (Gender==input$gender | Gender == "Both"))
  vitamins <- merge(vitamins,total_nutrients_df(), by="nutrient", all.x=TRUE)
  vitamins$"Average Daily Intake" <- as.numeric(vitamins$"Average Daily Intake")
  vitamins$"Min Daily Intake" <- as.numeric(vitamins$"Min Daily Intake")
  vitamins$"Max Daily Intake" <- as.numeric(vitamins$"Max Daily Intake")
  vitamins$total_value <- ifelse(is.na(vitamins$total_value),0, vitamins$total_value)
    
    vitamins$percentage <- ifelse(is.na(vitamins$"Average Daily Intake"),vitamins$total_value , vitamins$total_value / (vitamins$"Average Daily Intake") * 100)
    
    colours <- vitamins %>%
      mutate(
        colours = case_when(
          is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
          `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
          `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
          TRUE ~ "green"
        )
      ) %>%
      pull(colours)
    
   
    plot<-  ggplot(vitamins) +
      geom_col(aes(percentage, nutrient), fill = colours) +
      scale_fill_identity() +
      labs(x = "Nutrient", y = "Total value (%)")
    return(plot)
  }
})


###General
output$general_plot <- renderPlot({
  if(!is.null(added_foods$data)){
    
    
    general <- daily_intake %>% filter(Type=="General" & (Gender==input$gender | Gender == "Both"))
    general <- merge(general,total_nutrients_df(), by="nutrient", all.x=TRUE)
    general$"Average Daily Intake" <- as.numeric(general$"Average Daily Intake")
    general$"Min Daily Intake" <- as.numeric(general$"Min Daily Intake")
    general$"Max Daily Intake" <- as.numeric(general$"Max Daily Intake")
    general$total_value <- ifelse(is.na(general$total_value),0, general$total_value)
    
    general$percentage <- ifelse(is.na(general$"Average Daily Intake"),general$total_value , general$total_value / (general$"Average Daily Intake") * 100)
    
    colours <- general %>%
      mutate(
        colours = case_when(
          is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
          `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
          `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
          TRUE ~ "green"
        )
      ) %>%
      pull(colours)
    
    plot<-  ggplot(general) +
      geom_col(aes(percentage, nutrient), fill = colours) +
      scale_fill_identity() +
      labs(x = "Nutrient", y = "Total value (%)")
    return(plot)
  }
})


###Lipids
output$lipids_plot <- renderPlot({
  if(!is.null(added_foods$data)){
    
    
    lipids <- daily_intake %>% filter(Type=="Lipids" & (Gender==input$gender | Gender == "Both"))
    lipids <- merge(lipids,total_nutrients_df(), by="nutrient", all.x=TRUE)
    lipids$"Average Daily Intake" <- as.numeric(lipids$"Average Daily Intake")
    lipids$"Min Daily Intake" <- as.numeric(lipids$"Min Daily Intake")
    lipids$"Max Daily Intake" <- as.numeric(lipids$"Max Daily Intake")
    lipids$total_value <- ifelse(is.na(lipids$total_value),0, lipids$total_value)
    
    lipids$percentage <- ifelse(is.na(lipids$"Average Daily Intake"),lipids$total_value , lipids$total_value / (lipids$"Average Daily Intake") * 100)
    
    colours <- lipids %>%
      mutate(
        colours = case_when(
          is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
          `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
          `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
          TRUE ~ "green"
        )
      ) %>%
      pull(colours)
    
    plot<-  ggplot(lipids) +
      geom_col(aes(percentage, nutrient), fill = colours) +
      scale_fill_identity() +
      labs(x = "Nutrient", y = "Total value (%)")
    return(plot)
  }
})



###Carbohydrate
output$carbohydrate_plot <- renderPlot({
  if(!is.null(added_foods$data)){
    
    
    Carbohydrate <- daily_intake %>% filter(Type=="Carbohydrate" & (Gender==input$gender | Gender == "Both"))
    Carbohydrate <- merge(Carbohydrate,total_nutrients_df(), by="nutrient", all.x=TRUE)
    Carbohydrate$"Average Daily Intake" <- as.numeric(Carbohydrate$"Average Daily Intake")
    Carbohydrate$"Min Daily Intake" <- as.numeric(Carbohydrate$"Min Daily Intake")
    Carbohydrate$"Max Daily Intake" <- as.numeric(Carbohydrate$"Max Daily Intake")
    Carbohydrate$total_value <- ifelse(is.na(Carbohydrate$total_value),0,Carbohydrate$total_value)
    
    Carbohydrate$percentage <- ifelse(is.na(Carbohydrate$"Average Daily Intake"),Carbohydrate$total_value , Carbohydrate$total_value / (Carbohydrate$"Average Daily Intake") * 100)
    
    colours <- Carbohydrate %>%
      mutate(
        colours = case_when(
          is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
          `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
          `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
          TRUE ~ "green"
        )
      ) %>%
      pull(colours)
    
    plot<-  ggplot(Carbohydrate) +
      geom_col(aes(percentage, nutrient), fill = colours) +
      scale_fill_identity() +
      labs(x = "Nutrient", y = "Total value (%)")
    return(plot)
  }
})


###Protein
output$Protein_plot <- renderPlot({
  if(!is.null(added_foods$data)){
    
    
    Protein <- daily_intake %>% filter(Type=="Protein" & (Gender==input$gender | Gender == "Both"))
    Protein <- merge(Protein,total_nutrients_df(), by="nutrient", all.x=TRUE)
    Protein$"Average Daily Intake" <- as.numeric(Protein$"Average Daily Intake")
    Protein$"Min Daily Intake" <- as.numeric(Protein$"Min Daily Intake")
    Protein$"Max Daily Intake" <- as.numeric(Protein$"Max Daily Intake")
    Protein$total_value <- ifelse(is.na(Protein$total_value),0,Protein$total_value)
    
    Protein$percentage <- ifelse(is.na(Protein$"Average Daily Intake"),Protein$total_value , Protein$total_value / (Protein$"Average Daily Intake") * 100)
    
    colours <- Protein %>%
      mutate(
        colours = case_when(
          is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
          `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
          `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
          TRUE ~ "green"
        )
      ) %>%
      pull(colours)
    
    plot<-  ggplot(Protein) +
      geom_col(aes(percentage, nutrient), fill = colours) +
      scale_fill_identity() +
      labs(x = "Nutrient", y = "Total value (%)")
    return(plot)
  }
})

}




shinyApp(ui = ui, server = server)
