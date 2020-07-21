library(tidyverse)
library(tau)
library(tm)
source("text-input.R")
source("functions.R")

server <- function(input, output, session) {
  getText <- eventReactive(input$getResult, {
    myText <- trimws(input$myText)
    myText <- gsub('\\s+', ' ', myText)
    detector(myText)
  })
  
  # Displays input text
  output$outputText <- renderText({
    getText()
  })
  
  
  # Fucntion that displays scores when get result button is clicked
  getScores <- eventReactive(input$getResult, {
    "Internal consistency score: 0.919 <br> External consistency score: 0.956"
  })
  # Displays scores
  output$outputScores <- renderText({
    getScores()
  })
  
  # Disable get result button and clear button when there is no text data
  observe({
    toggleState(id = "getResult", condition = nchar(input$myText) > 0)
    toggleState(id = "clear", condition = nchar(input$myText) > 0)
  })
  
  # Load demo data when the corresponding button is clicked
  observe({
    input$loadDemo
    updateTextInput(session, "myText", value = demoInput)
  })
  
  # Clear text input area
  observe({
    input$clear
    updateTextInput(session, "myText", value = "")
  })
}
