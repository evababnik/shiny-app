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
library(shinymanager)
library(fontawesome)
remotes::install_github("deepanshu88/summaryBox")
library(summaryBox)




###########################
##### Server function #####
###########################
server <- function(input, output, session) {

  # _____________________________________ CALCULATOR ________________________________________

  source("calculator.R")
  
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
      nutrients_df <- vnos_tabela(input$food_name, input$quantity)
      
    
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
    
    
  output$total_nutrients_table <- DT::renderDataTable({
    total_nutrients_df()  })
  
  # prikaži tabelo z vrednostmi hranilnih snovi za posamezno izbrano hrano
  output$nutrition_table <- DT::renderDataTable({
    if (input$food_name != "") {
      nutrients_df <- vnos_tabela(input$food_name, input$quantity)
      nutrients_df <- nutrients_df %>% arrange(desc(value))
      
      #ostalo <- nutrients_df %>% filter(!nutrient %in% c('Energy', 'Protein', 'Total lipid (fat)', 'Carbohydrate, by difference'))
      
      return(nutrients_df)
    }
  })
  
  

  #______________________________________________________________________________________________________________________________
  
  ##pokazi grafe za glavne hranilne snovi
  
  # create a function to calculate nutrient values
  calculate_nutrient_values <- function(nutrient_type, gender) {
    daily_intake_filtered <- daily_intake %>% filter(Type == nutrient_type & (Gender == gender | Gender == "Both"))
    total_nutrients_df <- as.data.frame(total_nutrients_df())
    daily_intake_filtered <- as.data.frame(daily_intake_filtered)
    nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
    
    daily_intake_filtered$total_value <- rep(0, nrow(daily_intake_filtered))
   
    
    for (i in 1:nrow(daily_intake_filtered)) {
     
      match_rows <- total_nutrients_df %>% 
        filter(nutrient == daily_intake_filtered$nutrient[i] | 
                 nutrient == daily_intake_filtered$nutrient2[i] | 
                 nutrient == daily_intake_filtered$nutrient3[i] | 
                 nutrient == daily_intake_filtered$nutrient4[i]) %>%
        select(total_value)
      
      if (length(match_rows) > 0) {
        daily_intake_filtered$total_value[i] <- match_rows$total_value[1]
      }
    }
    
    daily_intake_filtered$"Average Daily Intake" <- as.numeric(daily_intake_filtered$"Average Daily Intake")
    daily_intake_filtered$"Min Daily Intake" <- as.numeric(daily_intake_filtered$"Min Daily Intake")
    daily_intake_filtered$"Max Daily Intake" <- as.numeric(daily_intake_filtered$"Max Daily Intake")
    daily_intake_filtered$total_value <- ifelse(is.na(daily_intake_filtered$total_value), 0, daily_intake_filtered$total_value)
    
    
    daily_intake_filtered$percentage <- ifelse(is.na(daily_intake_filtered$"Average Daily Intake"), 
                                               daily_intake_filtered$total_value, 
                                               round(daily_intake_filtered$total_value / (daily_intake_filtered$"Average Daily Intake") * 100, 1))
  
    colours <- daily_intake_filtered %>%
      mutate(
        colours = case_when(
          `total_value` >= as.numeric(`Max Daily Intake`) & !is.na(`Max Daily Intake`) ~ "red",
          `total_value` < as.numeric(`Min Daily Intake`) & !is.na(`Min Daily Intake`)  ~ "yellow",
          TRUE ~ "#00FF00"
        )
      ) %>%
      pull(colours)
  
    
   
    daily_intake_filtered$colours <- colours
    daily_intake_filtered$total_value <- paste0(daily_intake_filtered$total_value, daily_intake_filtered$`Unit Name`)
   daily_intake_filtered <- daily_intake_filtered[, c("nutrient_skrajsano", "total_value", "percentage", "colours")]
    
    return(daily_intake_filtered)
  }
  
  # use the function to generate the minerals plot
  output$minerals_plot <- renderReactable({
    if (!is.null(added_foods$data)) {
      table <- calculate_nutrient_values("Mineral", input$gender)
      table<-as.data.frame(table)
      colours <- table$colours
      table1 <- table[, c("nutrient_skrajsano", "total_value", "percentage")]
     
      reactable(
        table1,
        columns = list(
          nutrient_skrajsano = colDef(name = "MINERALS", width = 130, style = "font-size: 90%"),
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
  
  output$vitamins_plot <- renderReactable({"Vitamin"
    if(!is.null(added_foods$data)){
      table <- calculate_nutrient_values("Vitamin", input$gender)
      table<-as.data.frame(table)
      colours <- table$colours
      table1 <- table[, c("nutrient_skrajsano", "total_value", "percentage")]
      reactable(
        table1,
        columns = list(
          nutrient_skrajsano = colDef(name = "VITAMINS", width = 130, style = "font-size: 90%"),
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
      table <- calculate_nutrient_values("General", input$gender)
      
      table<-as.data.frame(table)
      colours <- table$colours
      table1 <- table[, c("nutrient_skrajsano", "total_value", "percentage")]
      reactable(
        table1,
        columns = list(
          nutrient_skrajsano = colDef(name = "GENERAL", width = 130, style = "font-size: 90%"),
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
    table <- calculate_nutrient_values("Lipids", input$gender)
    table<-as.data.frame(table)
    colours <- table$colours
    table1 <- table[, c("nutrient_skrajsano", "total_value", "percentage")]
    reactable(
      table1,
      columns = list(
        nutrient_skrajsano = colDef(
          name = "LIPIDS", 
          width = 130, 
          cell = function(value) {
            if (value == "Total lipid" | value == "Total fat") {
              tags$b(value, style = "font-weight:bold; font-size: 90%;")
            } else {
              tags$span(style = "padding-left: 10px; font-size: 90%;", value)
            }
          }
        ),
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
  output$carbohydrate_plot <- renderReactable({"Carbohydrate"
    if(!is.null(added_foods$data)){
      
      table <- calculate_nutrient_values("Carbohydrate", input$gender)
      table<-as.data.frame(table)
      table <- table %>% 
        arrange(desc(nutrient_skrajsano == "Carbohydrates"), nutrient_skrajsano)
      colours <- table$colours
      table1 <- table[, c("nutrient_skrajsano", "total_value", "percentage")]
      reactable(
        table1,
        columns = list(
          nutrient_skrajsano  = colDef(
            name = "CARBOHYDRATES", 
            width = 130, 
            cell = function(value) {
              if (value == "Carbohydrates") {
                tags$b(value, style = "font-weight:bold; font-size: 90%;")
              } else {
                tags$span(style = "padding-left: 10px; font-size: 90%;", value)
              }
            }
          ),
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
      
      table <- calculate_nutrient_values("Protein", input$gender)
      table<-as.data.frame(table)
      colours <- table$colours
      table1 <- table[, c("nutrient_skrajsano", "total_value", "percentage")]
      
     reactable(
        table1,
        columns = list(
          nutrient_skrajsano  = colDef(
            name = "PROTEINS", 
            width = 130, 
            cell = function(value) {
              if (value == "Protein") {
                tags$b(value, style = "font-weight:bold; font-size: 90%;")
              } else {
                tags$span(style = "padding-left: 10px; font-size: 90%;", value)
              }
            }
          ),
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
      
  
  #____________________________________________ ANALYSIS (MAP AND TABLE) ____________________________________
  
  source("analiza.R")
 
  #--------------------------------------------------
  
  ss <- naredi_zemljevid(country, 'Alpha-tocopherol', SHP_0)
  
  map2 <- reactive({
    tm_shape(ss, bbox = c(-15, 45, 45, 50)) +
      tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5, zindex = 401)
  })
  
  # Render the tmap object using renderTmap()
  output$map2 <- renderTmap({
    map2()
  })
  
  # Create an observe block to update the tmap
  observe({
    var <- input$var2
    zem <- naredi_zemljevid(country, var,SHP_0 )
    
    tryCatch({
      tmapProxy("map2", session, {
        tmap_mode("view")
        tm_remove_layer(401) +
          tm_shape(zem, bbox = c(-15, 45, 45, 50)) +
          tm_polygons(col = "mean_value", border.col = "black", lwd = 0.5, zindex = 401) +
          tm_fill( popup.vars=c('NUTRIENT_TEXT', 'mean_value'))
      })
    }, error = function(e) {
      # print the error message to the console
      print(e$message)
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
  
  output$unit1 <- renderText({ 
    var <- input$var2
    u <- un[un$NUTRIENT_TEXT == var, 2]
    paste('Unit:', u)
  })
  
  
  output$caloriesBox <- renderValueBox({
    value <- sum(as.data.frame(total_nutrients_df())[as.data.frame(total_nutrients_df())$nutrient %in% c("Energy", "Energy (kcal)"), "total_value"])
    color <- ifelse(value <= 1600 & input$gender == "Female","warning",
                    ifelse(value <= 2400 & input$gender == "Female", "success", 
                           ifelse(value > 2400 & input$gender == "Female","danger",
                                  ifelse(value <= 2000 & input$gender == "Male","warning",
                                         ifelse(value <= 3000 & input$gender == "Male", "success",
                                                "danger")))))
    
    (summaryBox2( 'Calories',
                  paste0(value, " kcal"),
                  
                  icon = "fas fa-fire",
                  style = color
    ))
    
  })
  
  output$carbohydrateBox <- renderValueBox({
    value <- sum(as.data.frame(total_nutrients_df())[grep("^Carbohydrate", as.data.frame(total_nutrients_df())$nutrient), "total_value"])
    color <- ifelse(value <= 130, "warning", ifelse(value <= 325, "success","danger"))
    
    (summaryBox2('Carbohydrates',
                 paste0(value, " g"),
                 
                 icon = "fas fa-wheat-awn",
                 style = color
    ))
  })
  
  
  output$proteinBox <- renderValueBox({
    value <- sum(as.data.frame(total_nutrients_df())[as.data.frame(total_nutrients_df())$nutrient %in% c("Protein", "Adjusted Protein"), "total_value"])
    color <- ifelse(value <= 46 & input$gender == "Female", "warning",
                    ifelse(value <= 112 & input$gender == "Female","success", 
                           ifelse(value > 112 & input$gender == "Female","danger",
                                  ifelse(value <= 56 & input$gender == "Male","warning",
                                         ifelse(value <= 168 & input$gender == "Male", "success",
                                                "danger")))))
    
    (summaryBox2('Proteins',
                 paste0(value, " g"),
                 
                 icon = "fas fa-egg",
                 style = color
                 
    ))
  })
  
  
  
  
  
  output$lipidBox <- renderValueBox({
    value <- sum(as.data.frame(total_nutrients_df())[as.data.frame(total_nutrients_df())$nutrient %in% c("Total lipid (fat)", "Lipids"), "total_value"])
    color <- ifelse(value <= 20, "warning", ifelse(value <= 150, "success", "danger"))
    
    div(summaryBox2('Lipids',
                    paste0(value, " g"),
                    
                    icon = "fas fa-cheese",
                    style = color, width = 20)
    )
  })
  
}
  
  
 


