library(shiny)
library(shinythemes)

shinyUI(navbarPage("Coursera Data Science Capstone: Word Prediction Application",
                   
        theme = shinytheme("cosmo"),
        mainPanel(
                h3("App Information"),
                h5("This application will predict the next word in
                    the entered phrase. Up to 5 words will be predicted.
                    Words are returned in order of their probability
                    (highest to lowest)."),
                
                h3("Instructions"),
                h5("1. Type in an incomplete phrase."),
                h5("2. Click the 'Submit' button."),
                textInput("textEntry",
                          h4("Type in any phrase"),
                          value = "Pick up a case of"),
                
                submitButton("Submit"),
                h3("Predicted Words:"),
                h1(textOutput("text"))
                )
        
        
))

