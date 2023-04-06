
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinydashboard)


###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
}