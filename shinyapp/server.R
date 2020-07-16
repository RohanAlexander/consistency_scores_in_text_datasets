library(tidyverse)
library(tau)
library(tm)
library(hash)
source("text-input.R")
load("hashtable.Rdata")
server <- function(input, output, session) {
  getText <- eventReactive(input$getResult, {
    myText <- trimws(input$myText)
    myText <- gsub('\\s+', ' ', myText)
    pred.sentence <- paste(myText, "...")
    words <- strsplit(tolower(myText), split = ' ')[[1]]
    if (length(words) >= 2) {
      history <- paste0(tail(words, 2), collapse = ' ')
      candidates <- h[[history]]$candidate
      candidates[candidates == 'i'] <- 'I'
      if (length(candidates)) {
        maxDisplay <- min(5, length(candidates))
        for (i in 1:maxDisplay) {
          pred.sentence <- paste(pred.sentence,
                                 paste(candidates[i], sep = ' '),
                                 sep = '\n')
        }
      }
      pred.sentence
    } else {
      myText
    }
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
