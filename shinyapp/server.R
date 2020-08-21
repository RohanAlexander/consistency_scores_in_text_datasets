library(tidyverse)
library(tau)
library(tm)
source("training_data.R")
source("internal_consistency_func.R")

server <- function(input, output, session) {
  getResults <- eventReactive(input$getResult, {
    internalTrainingData <- trimws(input$internalTrainingData)
    myTextData <- trimws(input$myTextData)
    myTextData <- gsub('\\s+', ' ', myTextData)
    get_results(create_ngrams_tibble(internalTrainingData, is_training = TRUE), create_ngrams_tibble(myTextData, is_training = FALSE))
  })
  
  # Displays input text
  output$unexpected_words <- renderTable({
    getResults()[2]
  })
  
  # Displays scores
  output$internal_consistency_score <- renderTable({
    getResults()[1]
  })
  
  # Disable get result button and clear button when there is no text data
  observe({
    toggleState(id = "getResult", condition = nchar(input$myTextData) > 0)
    toggleState(id = "clearText", condition = nchar(input$myTextData) > 0)
    toggleState(id = "clearTraining", condition = nchar(input$internalTrainingData) > 0)
  })
  
  observeEvent(input$getResult, {
    shinyjs::show("results_row")
  })
  
  # Load demo data when the corresponding button is clicked
  observe({
    input$loadDemo
    updateTextInput(session, "myTextData", value = demoTextData)
  })
  
  observe({
    input$loadTrainingDemo
    updateTextInput(session, "internalTrainingData", value = demoInternalTrainingData)
  })
  
  # Clear text input area
  observe({
    input$clearTraining
    updateTextInput(session, "internalTrainingData", value = "")
  })
  # Clear text input area
  observe({
    input$clearText
    updateTextInput(session, "myTextData", value = "")
  })
}
