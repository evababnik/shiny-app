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

library(readr)
daily_intake <- read_delim("rdv.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)
daily_intake$`Min Daily Intake` <- as.numeric(daily_intake$`Min Daily Intake`)
daily_intake$`Average Daily Intake` <- as.numeric(daily_intake$`Average Daily Intake`)
daily_intake$`Max Daily Intake` <- as.numeric(daily_intake$`Max Daily Intake`)

#daily_intake <-  read_excel("rdv.xlsx")


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

bar_chart <- function(label, value, width = "100%", height = "1rem", color = "#00bfc4", background = NULL) {
  label_div <- div(style = list(position = "absolute", left = "90%", top = "50%", transform = "translate(-50%, -50%)"), label)
  bar <- div(style = list(position = "relative", background = color, width = width, height = height), label_div)
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), chart)
}

vnos_tabela <- function(vnos, kol) {
  food <- get_foods(vnos)[[1]]
  nutrients <- food$foodNutrients
  nutrients_df <- data.frame(
    nutrient = sapply(nutrients, function(x) x$nutrientName),
    value = sapply(nutrients, function(x) x$value),
    unit = sapply(nutrients, function(x) x$unitName),
    stringsAsFactors = FALSE
  )
  nutrients_df$value <- nutrients_df$value * (as.numeric(kol)/ 100)
  nutrients_df$value <- round(nutrients_df$value, digits = 2) 
  nutrients_df <- nutrients_df %>% arrange(desc(value))
  
  return(nutrients_df)
}

###summary boxi
loadFontAwesome <- function() {
  
  list(
    
    # Font Awesome
    htmltools::htmlDependency(name = "font-awesome",
                              version = "5.13.0",
                              src = "fontawesome",
                              package = "fontawesome",
                              stylesheet = c("css/all.min.css", "css/v4-shims.min.css")),
    
    # Custom CSS
    htmltools::htmlDependency(
      name = "summarybox-style",
      version = "0.1.0",
      src = "css",
      package = "summaryBox",
      stylesheet = "style.css"
    )
    
  )
  
}
summaryBox2 <- function(title, value, width = 4, icon = "fas fa-chart-bar", style = "info") {
  
  valuetag  <- tags$div(
    class = paste0("col-md-", width),
    tags$div(
      class = paste("card-counter", style),
      tags$i(class = icon),
      tags$span(
        class = "count-numbers",
        value
      ),
      tags$span(
        class = "count-name",
        title
      )
    )
  )
  
  htmltools::htmlDependencies(valuetag) <- loadFontAwesome()
  htmltools::browsable(valuetag)
  
}