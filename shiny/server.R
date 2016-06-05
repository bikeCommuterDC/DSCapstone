library(shiny)

library(dplyr)
library(stringr)
library(reshape2)

source("helper.R")

shinyServer(function(input, output){
        
        prediction <- reactive({
                
                predictNextWord(input$textEntry)
                
        })
        
        output$text <- renderText({
                prediction()
        })
        
        
        
        
    
})