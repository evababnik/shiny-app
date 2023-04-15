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
  
  
  output$minerals_plot <- renderPlot({
    if(!is.null(added_foods$data)){
      
      
      minerals <- daily_intake %>% filter(Type=="Mineral" & (Gender==input$gender | Gender == "Both"))
      minerals <- merge(minerals,total_nutrients_df(), by="nutrient", all.x=TRUE)
      minerals$"Average Daily Intake" <- as.numeric(minerals$"Average Daily Intake")
      minerals$"Min Daily Intake" <- as.numeric(minerals$"Min Daily Intake")
      minerals$"Max Daily Intake" <- as.numeric(minerals$"Max Daily Intake")
      minerals$total_value <- ifelse(is.na(minerals$total_value),0, minerals$total_value)
      
      
      minerals$total_value <- ifelse(is.na(minerals$total_value),0, minerals$total_value)
      minerals$percentage <- ifelse(is.na(minerals$"Average Daily Intake"), minerals$total_value , round(minerals$total_value / (minerals$"Average Daily Intake") * 100,1))
      minerals$missing_percentage <- ifelse(minerals$percentage < 100, 100 - minerals$percentage, 0)
      minerals$full_percentage <- ifelse(minerals$percentage > 100, 100,  minerals$percentage)
      
      
      minerals<- pivot_longer(minerals, 
                              cols = c("full_percentage", "missing_percentage"),
                              names_to = "type",
                              values_to = "value")
      
      
      
      
      colours <-  minerals%>%
        mutate(
          colours = case_when(
            is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
            `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
            TRUE ~ "green"
          )
        ) %>%
        pull(colours)
      
      minerals$type <- factor(minerals$type, levels = c( "missing_percentage","full_percentage"))
      
      fill_colors <-ifelse(minerals$type == "full_percentage",  "white",colours)
      
      # Plot the data with the manual fill colors and reordered 'type' column
      plot<-  ggplot(minerals, aes(x = nutrient_skrajsano, y = value, fill = type)) + 
        geom_bar(stat = "identity") +
        geom_text(aes(label=ifelse(type=="full_percentage", paste0(percentage, "%"), " ")))+
        
        
        coord_flip() +
        scale_fill_manual(values = fill_colors, guide = "none")+
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
      
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
      vitamins$percentage <- ifelse(is.na(vitamins$"Average Daily Intake"), vitamins$total_value , round(vitamins$total_value / (vitamins$"Average Daily Intake") * 100,1))
      vitamins$missing_percentage <- ifelse(vitamins$percentage < 100, 100 -  vitamins$percentage, 0)
      vitamins$full_percentage <- ifelse(vitamins$percentage > 100, 100,  vitamins$percentage)
      
      
      vitamins<- pivot_longer(vitamins, 
                              cols = c("full_percentage", "missing_percentage"),
                              names_to = "type",
                              values_to = "value")
      
      
      
      
      colours <-  vitamins%>%
        mutate(
          colours = case_when(
            is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
            `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
            TRUE ~ "green"
          )
        ) %>%
        pull(colours)
      
      vitamins$type <- factor(vitamins$type, levels = c( "missing_percentage","full_percentage"))
      
      fill_colors <-ifelse(vitamins$type == "full_percentage",  "white",colours)
      
      # Plot the data with the manual fill colors and reordered 'type' column
      plot<-  ggplot(vitamins, aes(x = nutrient_skrajsano, y = value, fill = type)) + 
        geom_bar(stat = "identity") +
        geom_text(aes(label=ifelse(type=="full_percentage", paste0(percentage, "%"), " ")))+
        
        
        coord_flip() +
        scale_fill_manual(values = fill_colors, guide = "none")+
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
      
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
      
      general$percentage <- ifelse(is.na(general$"Average Daily Intake"), general$total_value , round(general$total_value / (general$"Average Daily Intake") * 100,1))
      general$missing_percentage <- ifelse(general$percentage < 100, 100 -  general$percentage, 0)
      general$full_percentage <- ifelse(general$percentage > 100, 100,  general$percentage)
      
      
      general<- pivot_longer(general, 
                             cols = c("full_percentage", "missing_percentage"),
                             names_to = "type",
                             values_to = "value")
      
      
      
      
      colours <-  general%>%
        mutate(
          colours = case_when(
            is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
            `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
            TRUE ~ "green"
          )
        ) %>%
        pull(colours)
      
      general$type <- factor(general$type, levels = c( "missing_percentage","full_percentage"))
      
      fill_colors <-ifelse(general$type == "full_percentage",  "white",colours)
     
      plot<-  ggplot(general, aes(x = nutrient_skrajsano, y = value, fill = type)) + 
        geom_bar(stat = "identity") +
        geom_text(aes(label=ifelse(type=="full_percentage", paste0(percentage, "%"), " ")))+
        
        
        coord_flip() +
        scale_fill_manual(values = fill_colors, guide = "none")+
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
      
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
      
      
      lipids$percentage <- ifelse(is.na(lipids$"Average Daily Intake"), lipids$total_value , round(lipids$total_value / (lipids$"Average Daily Intake") * 100,1))
      lipids$missing_percentage <- ifelse(lipids$percentage < 100, 100 -  lipids$percentage, 0)
      lipids$full_percentage <- ifelse(lipids$percentage > 100, 100,  lipids$percentage)
      
      
      lipids <- pivot_longer(lipids, 
                             cols = c("full_percentage", "missing_percentage"),
                             names_to = "type",
                             values_to = "value")
      
      
      
      
      colours <-  lipids%>%
        mutate(
          colours = case_when(
            is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
            `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
            TRUE ~ "green"
          )
        ) %>%
        pull(colours)
      
      
      lipids$type <- factor(lipids$type, levels = c( "missing_percentage","full_percentage"))
      
      fill_colors <-ifelse(lipids$type == "full_percentage",  "white",colours)
      
      
      plot<-  ggplot(lipids, aes(x = nutrient_skrajsano, y = value, fill = type)) + 
        geom_bar(stat = "identity") +
        geom_text(aes(label=ifelse(type=="full_percentage", paste0(percentage, "%"), " ")))+
        
        
        coord_flip() +
        scale_fill_manual(values = fill_colors, guide = "none")+
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
      
      return(plot)
    }
  })
  
  
  
  ###Carbohydrate
  output$carbohydrate_plot <- renderPlot({
    if(!is.null(added_foods$data)){
      
      
      Carbohydrate <- daily_intake %>% filter(Type=="Carbohydrate" & (Gender==input$gender | Gender == "Both"))
       
    
      nutrient_cols <- c("nutrient", "nutrient2", "nutrient3","nutrient4")
      total_nutrients_df <- as.data.frame(total_nutrients_df())
     
     
      # Ustvarite novo prazno polje v tabeli Carbohydrate za shranjevanje skupne vrednosti
      Carbohydrate$total_value <- rep(0, nrow(Carbohydrate))
      
      # Za vsako vrstico v tabeli Carbohydrate poiščite ustrezno vrednost v tabeli total_nutrients_df
      for (i in 1:nrow(Carbohydrate)) {
        # Poiščite vrednost v tabeli total_nutrients_df, ki se ujema z vrednostmi v stolpcih nutrient1, nutrient2 ali nutrient3
        match_rows <- total_nutrients_df %>% 
          filter(nutrient == Carbohydrate$nutrient[i] | 
                   nutrient == Carbohydrate$nutrient2[i] | 
                   nutrient == Carbohydrate$nutrient3[i] | 
                   nutrient == Carbohydrate$nutrient4[i]) %>%
          select(total_value)
   
       
        # Če je bila najdena ujemanje, dodelite ustrezno vrednost v polje total_value v tabeli Carbohydrate
        if (length(match_rows) > 0) {
          Carbohydrate$total_value[i] <- match_rows$total_value[1]
        }
      }
      
      
      
      
   
      
      Carbohydrate$"Average Daily Intake" <- as.numeric(Carbohydrate$"Average Daily Intake")
      Carbohydrate$"Min Daily Intake" <- as.numeric(Carbohydrate$"Min Daily Intake")
      Carbohydrate$"Max Daily Intake" <- as.numeric(Carbohydrate$"Max Daily Intake")
      Carbohydrate$total_value <- ifelse(is.na(Carbohydrate$total_value),0,Carbohydrate$total_value)
      
      Carbohydrate$percentage <- ifelse(is.na(Carbohydrate$"Average Daily Intake"), Carbohydrate$total_value , round( Carbohydrate$total_value / ( Carbohydrate$"Average Daily Intake") * 100,1))
      Carbohydrate$missing_percentage <- ifelse(Carbohydrate$percentage < 100, 100 -  Carbohydrate$percentage, 0)
      Carbohydrate$full_percentage <- ifelse(Carbohydrate$percentage > 100, 100,  Carbohydrate$percentage)
      
      
      Carbohydrate <- pivot_longer(Carbohydrate, 
                                   cols = c("full_percentage", "missing_percentage"),
                                   names_to = "type",
                                   values_to = "value")
      
      
      
      
      colours <-  Carbohydrate %>%
        mutate(
          colours = case_when(
            is.na(`Max Daily Intake`) | is.na(`Min Daily Intake`) ~ "green",
            `total_value` >= as.numeric(`Max Daily Intake`) ~ "red",
            `total_value` < as.numeric(`Min Daily Intake`) ~ "yellow",
            TRUE ~ "green"
          )
        ) %>%
        pull(colours)
      
      
      Carbohydrate$type <- factor(Carbohydrate$type, levels = c( "missing_percentage","full_percentage"))
      Carbohydrate <- as.data.frame(Carbohydrate)
      fill_colors <-ifelse(Carbohydrate$type == "full_percentage",  "white",colours)
    
      plot<-  ggplot(Carbohydrate, aes(x = nutrient_skrajsano, y = value, fill = type)) + 
        geom_bar(stat = "identity") +
        geom_text(aes(label=ifelse(type=="full_percentage", paste0(percentage, "%"), " ")))+
        
        
        coord_flip() +
        scale_fill_manual(values = fill_colors, guide = "none")+
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
      
      return(plot)
    }
  })
  
  
  ###Protein
  output$Protein_plot <- renderPlot({
    if(!is.null(added_foods$data)){
      
      
      Protein <- daily_intake %>% filter(Type == "Protein" & (Gender==input$gender | Gender == "Both"))
      
      Protein <- merge(Protein,total_nutrients_df(), by="nutrient", all.x=TRUE)
      Protein$"Average Daily Intake" <- as.numeric(Protein$"Average Daily Intake")
      Protein$"Min Daily Intake" <- as.numeric(Protein$"Min Daily Intake")
      Protein$"Max Daily Intake" <- as.numeric(Protein$"Max Daily Intake")
      Protein$total_value <- as.numeric(ifelse(is.na(Protein$total_value),0,Protein$total_value))
      
      Protein$percentage <- ifelse(is.na(Protein$"Average Daily Intake"),Protein$total_value , round(Protein$total_value / (Protein$"Average Daily Intake") * 100,1))
      Protein$white <- ifelse(Protein$percentage < 100, 100 - Protein$percentage, 0)
      Protein$full_percentage <- ifelse(Protein$percentage > 100, 100, Protein$percentage)
      
      Protein$red <- 0
      Protein$red[Protein$total_value >= Protein$`Max Daily Intake` & !is.na(Protein$`Max Daily Intake`)] <- Protein$full_percentage[Protein$total_value >= Protein$`Max Daily Intake`& !is.na(Protein$`Max Daily Intake`)]
      Protein$yellow <- 0
      Protein$yellow[Protein$total_value <= Protein$`Min Daily Intake` & !is.na(Protein$`Min Daily Intake`)] <- Protein$full_percentage[Protein$total_value <= Protein$`Min Daily Intake`& !is.na(Protein$`Min Daily Intake`)]
     
      Protein$green <- ifelse(Protein$yellow == 0 & Protein$red == 0, Protein$full_percentage, 0)
      ##Protein$text <- ifelse(Protein$white == 100, "0%",paste0(max(Protein$red, Protein$yellow, Protein$green), "%"))
      
      Protein <- pivot_longer(Protein, 
                              cols = c("green", "yellow", "red", "white"),
                              names_to = "type",
                              values_to = "value")
      
    Protein$text <- rep(" ", nrow(Protein))
     Protein$text[Protein$type == "white"] <- " "
     Protein$text[Protein$value != 0 & Protein$type != "white"] <- paste0(Protein$percentage[Protein$value != 0 & Protein$type != "white"], "%")
     Protein$text[lag(Protein$value) == 0 & lead(Protein$value) == 0 & Protein$value == 0 & Protein$type == "yellow"] <- "0%"
     
 
   
 Protein <- as.data.frame(Protein)
 Protein$type <- factor(Protein$type, levels = c("white","green","yellow", "red"))
    plot<-ggplot(Protein, aes(x = nutrient_skrajsano, y = value, fill = type)) + 
      geom_bar(stat = "identity") +
      coord_flip()+
      geom_text(aes(label = text))+
     
      scale_fill_manual(values = c("green" = "green", "yellow" = "yellow", "red"="red", "white"= "white"), guide="none")+
      theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank(),
                        axis.title.y = element_blank()) 
                
      

  #    
      return(plot)
    }
  })
}
  
  
 


