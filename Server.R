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
library(httr)
library(dplyr)
library(readr)
library(tidyr)
library(grid)
library(shadowtext)
library(ggplot2)
library(gridExtra)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(data.table)
library(tmaptools)
library(leaflet)
library(tm)
library(bslib)
library(sf)
library(reshape2)
library(htmltools)
source("calculator.R")
source("analiza_cista2.R")



###########################
##### Server function #####
###########################
server <- function(input, output, session) {
source("calculator.R")
source("analiza_cista2.R")
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
                return(reactable(coun,  highlight = TRUE))
              })
  })
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
  
  added_foods <- reactiveValues()
  deleted_food <- reactiveValues()
  # change the 'food_table' reactiveValues to contain only data
  food_table <- shiny::reactiveValues()
  food_table$data <- data.frame(
    food_name = character(),
    quantity = numeric(),
    stringsAsFactors = FALSE
  )
  # add food to the 'food_table'
  observeEvent(input$add_food, {
    if (input$food_name != "" & input$quantity != "") {
      food <- get_foods(input$food_name)[[1]]
      nutrients <- food$foodNutrients
      nutrients_df <- data.frame(
        nutrient = sapply(nutrients, function(x) x$nutrientName),
        value = sapply(nutrients, function(x) x$value),
        unit = sapply(nutrients, function(x) x$unitName),
        stringsAsFactors = FALSE    )
      nutrients_df$value <- nutrients_df$value * (as.numeric(input$quantity)/ 100)
      nutrients_df$value <- round(nutrients_df$value, digits = 2)
      
      
    
      new_row <- data.frame(food_name = input$food_name, quantity = as.numeric(input$quantity), stringsAsFactors = FALSE)
      food_table$data <- rbind(food_table$data, new_row)
      food_table$data <- food_table$data %>% group_by(food_name)%>% summarise(quantity = sum(as.numeric(quantity)))
      
      
      
      nutrients_df_names <- nutrients_df
      nutrients_df_names$food_name <- rep(input$food_name, nrow(nutrients_df))
      nutrients_df_names$quantity <- rep(as.numeric(input$quantity), nrow(nutrients_df))
      
      
      added_foods$data <- rbind(added_foods$data, nutrients_df_names) 
      added_foods$data <- added_foods$data %>% group_by(food_name, nutrient, unit) %>% summarise(quantity=sum(as.numeric(quantity)), value=sum(value))
     } })
  
  
  output$food_table <- renderDataTable({
    if(nrow(food_table$data) == 0) {
      return(data.frame())
    } else {
      DT = food_table$data
      DT[["Actions"]] <- paste0('
      <div class="btn-group" role="group" aria-label="Basic example">
        <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(food_table$data),' onclick="Shiny.onInputChange(\'delete\',  this.id)">Delete</button>
      </div>')
      
      datatable(DT, escape=F)
    }
  })
  


  # delete food
  observeEvent(input$delete, {
    if (!is.null(input$delete)) {
      row_id <- as.numeric(strsplit(input$delete, "_")[[1]][2])
      food_to_delete <- food_table$data[row_id,]
      deleted_nutrients <- added_foods$data[added_foods$data$food_name == food_to_delete$food_name,]
     
      
      added_foods$data <- added_foods$data[added_foods$data$food_name != food_to_delete$food_name,]
      food_table$data <- food_table$data[-row_id,] }})
  
  
  
  
  # prikaži tabelo s seštevkom hranilnih snovi za dodana živila
  
  total_nutrients_df <- reactive({
    if (!is.null(added_foods$data)) {
      total_nutrients_df <- added_foods$data %>%
        group_by(nutrient,unit) %>%
        summarise(total_value = sum(value)) %>%
        mutate(total_value = round(total_value, digits=2))
      total_nutrients_df <- total_nutrients_df[,c(1,3,2)]
      total_nutrients_df <- total_nutrients_df[total_nutrients_df$unit != "kJ",]
      #names(total_nutrients_df) <- c("Nutrient", "Total value", "Unit")
      total_nutrients_df <-  total_nutrients_df %>% arrange(desc(total_value))
      return(total_nutrients_df)} })
    
    
  output$total_nutrients_table <- renderTable({
    total_nutrients_df()  })
  
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
      nutrients_df <- nutrients_df %>% arrange(desc(value))
      return(nutrients_df)
    }
  })
  
  
  
  
  ##pokazi grafe za glavne hranilne snovi
  
  
  ##minerals
  output$minerals_plot <- renderReactable({
    if(!is.null(added_foods$data)){
      table <- daily_intake %>% filter(Type=="Mineral" & (Gender==input$gender | Gender == "Both"))
      nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
      total_nutrients_df <- as.data.frame(total_nutrients_df())
      
      table$total_value <- rep(0, nrow(table))
      
      for (i in 1:nrow(table)) {
        match_rows <- total_nutrients_df %>% 
          filter(nutrient == table$nutrient[i] | 
                   nutrient == table$nutrient2[i] | 
                   nutrient == table$nutrient3[i] | 
                   nutrient == table$nutrient4[i]) %>%
          select(total_value)
        
        if (length(match_rows) > 0) {
          table$total_value[i] <- match_rows$total_value[1]
        }
      }
      
      
      table$"Average Daily Intake" <- as.numeric(table$"Average Daily Intake")
      table$"Min Daily Intake" <- as.numeric(table$"Min Daily Intake")
      table$"Max Daily Intake" <- as.numeric(table$"Max Daily Intake")
      table$total_value <- ifelse(is.na(table$total_value),0,table$total_value)
      
      
      table$percentage <- ifelse(is.na(table$"Average Daily Intake"),table$total_value , round(table$total_value / (table$"Average Daily Intake") * 100,1))
      
      colours <-  table %>%
        mutate(
          colours = case_when(
            `total_value` >= as.numeric(`Max Daily Intake`) & !is.na(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) & !is.na(`Min Daily Intake`)  ~ "yellow",
            TRUE ~ "#00FF00"
          )
        ) %>%
        pull(colours)
      
      table$colours <- colours
      table$total_value <- paste0(table$total_value, table$`Unit Name`)
      table <- as.data.frame(table)
      
      table <- table[,c("nutrient_skrajsano", "total_value", "percentage")]
      print(table)
      
      reactable(
        table,
        columns = list(
          nutrient_skrajsano = colDef(name = "MINERALS", width = 120),
          total_value = colDef(name = "Quantity", align = "right", width = 80, cell = function(value) {
            value
          }),
          percentage = colDef(name = "Daily target (%)", align = "left", cell = function(value, rowIndex) {
            width <- paste0(ifelse(value > 100, 100, value), "%")
            color <- colours[rowIndex]
            bar_chart(paste0(value, "%"), width = width, color = color, background = "#e1e1e1")
          })
        )
      )
    }})
  
  ##vitamins
  
  output$vitamins_plot <- renderReactable({
    if(!is.null(added_foods$data)){
      table <- daily_intake %>% filter(Type=="Vitamin" & (Gender==input$gender | Gender == "Both"))
      nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
      total_nutrients_df <- as.data.frame(total_nutrients_df())
      
      table$total_value <- rep(0, nrow(table))
      
      for (i in 1:nrow(table)) {
        match_rows <- total_nutrients_df %>% 
          filter(nutrient == table$nutrient[i] | 
                   nutrient == table$nutrient2[i] | 
                   nutrient == table$nutrient3[i] | 
                   nutrient == table$nutrient4[i]) %>%
          select(total_value)
        
        if (length(match_rows) > 0) {
          table$total_value[i] <- match_rows$total_value[1]
        }
      }
      
      
      table$"Average Daily Intake" <- as.numeric(table$"Average Daily Intake")
      table$"Min Daily Intake" <- as.numeric(table$"Min Daily Intake")
      table$"Max Daily Intake" <- as.numeric(table$"Max Daily Intake")
      table$total_value <- ifelse(is.na(table$total_value),0,table$total_value)
      
      
      table$percentage <- ifelse(is.na(table$"Average Daily Intake"),table$total_value , round(table$total_value / (table$"Average Daily Intake") * 100,1))
      
      colours <-  table %>%
        mutate(
          colours = case_when(
            `total_value` >= as.numeric(`Max Daily Intake`) & !is.na(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) & !is.na(`Min Daily Intake`)  ~ "yellow",
            TRUE ~ "#00FF00"
          )
        ) %>%
        pull(colours)
      
      table$colours <- colours
      table$total_value <- paste0(table$total_value, table$`Unit Name`)
      table <- as.data.frame(table)
      
      table <- table[,c("nutrient_skrajsano", "total_value", "percentage")]
      print(table)
      
      reactable(
        table,
        columns = list(
          nutrient_skrajsano = colDef(name = "VITAMINS", width = 120),
          total_value = colDef(name = "Quantity", align = "right", width = 80, cell = function(value) {
            value
          }),
          percentage = colDef(name = "Daily target (%)", align = "left", cell = function(value, rowIndex) {
            width <- paste0(ifelse(value > 100, 100, value), "%")
            color <- colours[rowIndex]
            bar_chart(paste0(value, "%"), width = width, color = color, background = "#e1e1e1")
          })
        )
      )
    }})
  
  ###General
  output$general_plot <- renderReactable({
    if(!is.null(added_foods$data)){
      table <- daily_intake %>% filter(Type=="General" & (Gender==input$gender | Gender == "Both"))
      nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
      total_nutrients_df <- as.data.frame(total_nutrients_df())
      
      table$total_value <- rep(0, nrow(table))
      
      for (i in 1:nrow(table)) {
        match_rows <- total_nutrients_df %>% 
          filter(nutrient == table$nutrient[i] | 
                   nutrient == table$nutrient2[i] | 
                   nutrient == table$nutrient3[i] | 
                   nutrient == table$nutrient4[i]) %>%
          select(total_value)
        
        if (length(match_rows) > 0) {
          table$total_value[i] <- match_rows$total_value[1]
        }
      }
      
      
      table$"Average Daily Intake" <- as.numeric(table$"Average Daily Intake")
      table$"Min Daily Intake" <- as.numeric(table$"Min Daily Intake")
      table$"Max Daily Intake" <- as.numeric(table$"Max Daily Intake")
      table$total_value <- ifelse(is.na(table$total_value),0,table$total_value)
      
      
      table$percentage <- ifelse(is.na(table$"Average Daily Intake"),table$total_value , round(table$total_value / (table$"Average Daily Intake") * 100,1))
      
      colours <-  table %>%
        mutate(
          colours = case_when(
            `total_value` >= as.numeric(`Max Daily Intake`) & !is.na(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) & !is.na(`Min Daily Intake`)  ~ "yellow",
            TRUE ~ "#00FF00"
          )
        ) %>%
        pull(colours)
      
      table$colours <- colours
      table$total_value <- paste0(table$total_value, table$`Unit Name`)
      table <- as.data.frame(table)
      
      table <- table[,c("nutrient_skrajsano", "total_value", "percentage")]
      print(table)
      
      reactable(
        table,
        columns = list(
          nutrient_skrajsano = colDef(name = "GENERAL", width = 120),
          total_value = colDef(name = "Quantity", align = "right", width = 80, cell = function(value) {
            value
          }),
          percentage = colDef(name = "Daily target (%)", align = "left", cell = function(value, rowIndex) {
            width <- paste0(ifelse(value > 100, 100, value), "%")
            color <- colours[rowIndex]
            bar_chart(paste0(value, "%"), width = width, color = color, background = "#e1e1e1")
          })
        )
      )
    }})
  
  ###Lipids
output$lipids_plot <- renderReactable({
  if(!is.null(added_foods$data)){
    table <- daily_intake %>% filter(Type=="Lipids" & (Gender==input$gender | Gender == "Both"))
    nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
    total_nutrients_df <- as.data.frame(total_nutrients_df())
    
    table$total_value <- rep(0, nrow(table))
    
    for (i in 1:nrow(table)) {
      match_rows <- total_nutrients_df %>% 
        filter(nutrient == table$nutrient[i] | 
                 nutrient == table$nutrient2[i] | 
                 nutrient == table$nutrient3[i] | 
                 nutrient == table$nutrient4[i]) %>%
        select(total_value)
      
      if (length(match_rows) > 0) {
        table$total_value[i] <- match_rows$total_value[1]
      }
    }
    
    
    table$"Average Daily Intake" <- as.numeric(table$"Average Daily Intake")
    table$"Min Daily Intake" <- as.numeric(table$"Min Daily Intake")
    table$"Max Daily Intake" <- as.numeric(table$"Max Daily Intake")
    table$total_value <- ifelse(is.na(table$total_value),0,table$total_value)
    
    
    table$percentage <- ifelse(is.na(table$"Average Daily Intake"),table$total_value , round(table$total_value / (table$"Average Daily Intake") * 100,1))
    
    colours <-  table %>%
      mutate(
        colours = case_when(
          `total_value` >= as.numeric(`Max Daily Intake`) & !is.na(`Max Daily Intake`) ~ "red",
          `total_value` < as.numeric(`Min Daily Intake`) & !is.na(`Min Daily Intake`)  ~ "yellow",
          TRUE ~ "#00FF00"
        )
      ) %>%
      pull(colours)
    
    table$colours <- colours
    table$total_value <- paste0(table$total_value, table$`Unit Name`)
    table <- as.data.frame(table)
    
    table <- table[,c("nutrient_skrajsano", "total_value", "percentage")]
    print(table)
    
    reactable(
      table,
      columns = list(
        nutrient_skrajsano = colDef(name = "LIPIDS", width = 120),
        total_value = colDef(name = "Quantity", align = "right", width = 80, cell = function(value) {
          value
        }),
        percentage = colDef(name = "Daily target (%)", align = "left", cell = function(value, rowIndex) {
          width <- paste0(ifelse(value > 100, 100, value), "%")
          color <- colours[rowIndex]
          bar_chart(paste0(value, "%"), width = width, color = color, background = "#e1e1e1")
        })
      )
    )
  }})


  
  
  
  ###Carbohydrate
  output$carbohydrate_plot <- renderReactable({
    if(!is.null(added_foods$data)){
    table <- daily_intake %>% filter(Type=="Carbohydrate" & (Gender==input$gender | Gender == "Both"))
      nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
      total_nutrients_df <- as.data.frame(total_nutrients_df())
   
      table$total_value <- rep(0, nrow(table))
  
      for (i in 1:nrow(table)) {
          match_rows <- total_nutrients_df %>% 
          filter(nutrient == table$nutrient[i] | 
                   nutrient == table$nutrient2[i] | 
                   nutrient == table$nutrient3[i] | 
                   nutrient == table$nutrient4[i]) %>%
          select(total_value)
  
        if (length(match_rows) > 0) {
          table$total_value[i] <- match_rows$total_value[1]
        }
      }
      
      
      table$"Average Daily Intake" <- as.numeric(table$"Average Daily Intake")
      table$"Min Daily Intake" <- as.numeric(table$"Min Daily Intake")
      table$"Max Daily Intake" <- as.numeric(table$"Max Daily Intake")
      table$total_value <- ifelse(is.na(table$total_value),0,table$total_value)
     
      
      table$percentage <- ifelse(is.na(table$"Average Daily Intake"),table$total_value , round(table$total_value / (table$"Average Daily Intake") * 100,1))
     
      colours <-  table %>%
        mutate(
          colours = case_when(
            `total_value` >= as.numeric(`Max Daily Intake`) & !is.na(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) & !is.na(`Min Daily Intake`)  ~ "yellow",
            TRUE ~ "#00FF00"
          )
        ) %>%
        pull(colours)
      
      table$colours <- colours
      table$total_value <- paste0(table$total_value, table$`Unit Name`)
      table <- as.data.frame(table)
      
      table <- table[,c("nutrient_skrajsano", "total_value", "percentage")]
      print(table)
      
      reactable(
        table,
        columns = list(
          nutrient_skrajsano = colDef(name = "CARBOHYDRATES", width = 120),
          total_value = colDef(name = "Quantity", align = "right", width = 80, cell = function(value) {
            value
          }),
          percentage = colDef(name = "Daily target (%)", align = "left", cell = function(value, rowIndex) {
            width <- paste0(ifelse(value > 100, 100, value), "%")
            color <- colours[rowIndex]
            bar_chart(paste0(value, "%"), width = width, color = color, background = "#e1e1e1")
          })
        )
      )
    }})
   
  
  
  ###Protein
  output$Protein_plot <- renderReactable({
    if(!is.null(added_foods$data)){
      table <- daily_intake %>% filter(Type=="Protein" & (Gender==input$gender | Gender == "Both"))
      nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
      total_nutrients_df <- as.data.frame(total_nutrients_df())
      
      table$total_value <- rep(0, nrow(table))
      
      for (i in 1:nrow(table)) {
        match_rows <- total_nutrients_df %>% 
          filter(nutrient == table$nutrient[i] | 
                   nutrient == table$nutrient2[i] | 
                   nutrient == table$nutrient3[i] | 
                   nutrient == table$nutrient4[i]) %>%
          select(total_value)
        
        if (length(match_rows) > 0) {
          table$total_value[i] <- match_rows$total_value[1]
        }
      }
      
      
      table$"Average Daily Intake" <- as.numeric(table$"Average Daily Intake")
      table$"Min Daily Intake" <- as.numeric(table$"Min Daily Intake")
      table$"Max Daily Intake" <- as.numeric(table$"Max Daily Intake")
      table$total_value <- ifelse(is.na(table$total_value),0,table$total_value)
      
      
      table$percentage <- ifelse(is.na(table$"Average Daily Intake"),table$total_value , round(table$total_value / (table$"Average Daily Intake") * 100,1))
      
      colours <-  table %>%
        mutate(
          colours = case_when(
            `total_value` >= as.numeric(`Max Daily Intake`) & !is.na(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) & !is.na(`Min Daily Intake`)  ~ "yellow",
            TRUE ~ "#00FF00"
          )
        ) %>%
        pull(colours)
      
      table$colours <- colours
      table$total_value <- paste0(table$total_value, table$`Unit Name`)
      table <- as.data.frame(table)
      
      table <- table[,c("nutrient_skrajsano", "total_value", "percentage")]
      print(table)
      
      reactable(
        table,
        columns = list(
          nutrient_skrajsano = colDef(name = "PROTEINS", width = 120),
          total_value = colDef(name = "Quantity", align = "right", width = 80, cell = function(value) {
            value
          }),
          percentage = colDef(name = "Daily target (%)", align = "left", cell = function(value, rowIndex) {
            width <- paste0(ifelse(value > 100, 100, value), "%")
            color <- colours[rowIndex]
            bar_chart(paste0(value, "%"), width = width, color = color, background = "#e1e1e1")
          })
        )
      )
    }})
  
  
 
}
  
  
 


