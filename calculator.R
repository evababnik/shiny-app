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
