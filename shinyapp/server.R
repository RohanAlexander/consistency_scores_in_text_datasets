library(shiny)
library(tidyverse) # tidyverse is needed for hardcoding string replacement, may be removed once we move to next phase
source("text-input.R")

function(input, output, session) {
  # Fucntion that displays input text when get result button is clicked
  getText <- eventReactive(input$getResult, {
    # Hardcode text correction by replacing typos manually
    str_replace_all(input$myText, c("Reeal" = "Reed", "tbere" = "there", "tbat" = "that"))
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
library(shiny)
runGitHub("consistency_scores_in_text_datasets", "RohanAlexander", subdir = "shinyapp/")
