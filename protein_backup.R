library(htmltools)
bar_chart <- function(label, value, width = "100%", height = "1rem", color = "#00bfc4", background = NULL) {
  label_div <- div(style = list(position = "absolute", left = "90%", top = "50%", transform = "translate(-50%, -50%)"), label)
  bar <- div(style = list(position = "relative", background = color, width = width, height = height), label_div)
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), chart)
}

data <- MASS::Cars93[20:24, c("Make", "MPG.city", "MPG.highway")]

data$MPG.highway <- c(33.1, 28, 28, 26, 0)
colours <- c("green", "yellow", "red", "red", "white")

reactable(
  data,
  columns = list(
    Make = colDef(name = "Make", width = 100),
    MPG.city = colDef(name = "MPG (city)", align = "right", width = 50, cell = function(value) {
      value
    }),
    MPG.highway = colDef(name = "MPG (highway)", align = "left", cell = function(value, rowIndex) {
      width <- paste0(ifelse(value > 28, 28, value), "%")
      color <- colours[rowIndex]
      bar_chart(paste0(value, "%"), width = width, color = color, background = "#e1e1e1")
    })
  )
)


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


