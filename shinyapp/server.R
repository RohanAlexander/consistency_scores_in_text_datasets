library(tidyverse)
library(tau)
library(tm)
source("training_data.R")
source("internal_consistency_func.R")
source("external_consistency_func.R")
source("OCR_text_func.R")

#### Dorpbox authentication ####
library(rdrop2)
drop_auth()
token <- drop_auth()
saveRDS(token, file = "token.rds")
drop_dir() %>% 
  filter(.tag == "Arianna")
trigramsDF <- drop_read_csv("Arianna/trigramsDF.csv")
#trigramsDF <- read_csv("trigramsDF.csv")
server <- function(input, output, session) {
#### Internal consistency score ####
  getInternalResults <- eventReactive(input$getInternalResult, {
    internalTrainingData <- trimws(input$internalTrainingData)
    myTextData <- trimws(input$myTextData)
    myTextData <- gsub('\\s+', ' ', myTextData)
    get_results(create_ngrams_tibble(internalTrainingData, is_training = TRUE), create_ngrams_tibble(myTextData, is_training = FALSE))
  })
  
  # Displays input text
  output$unexpected_words <- renderTable({
    getInternalResults()[2]
  })
  
  # Displays scores
  output$internal_consistency_score <- renderTable({
    getInternalResults()[1]
  })
  
#### Exteranl consistency score ####
  getExternalResults <- eventReactive(input$getExternalResult, {
    # Get the DF of the split trigrams
    myTextData_e <- trimws(input$myTextData_e)
    split_trigrams <- create_bigrams_for_predictions(myTextData_e)
    return(predict_words(split_trigrams))
  })
  
  # Displays input text
  output$unexpected_words_e <- renderTable({
    getExternalResults()
  })
  
  
  # Disable get result button and clear button when there is no text data
  observe({
    toggleState(id = "getInternalResult", condition = nchar(input$myTextData) > 0)
    toggleState(id = "getExternalResult", condition = nchar(input$myTextData_e) > 0)
    toggleState(id = "clearText", condition = nchar(input$myTextData) > 0)
    toggleState(id = "clearText_e", condition = nchar(input$myTextData_e) > 0)
    toggleState(id = "clearTraining", condition = nchar(input$internalTrainingData) > 0)
  })
  
  observeEvent(
    input$getInternalResult, {
    shinyjs::show("internal_results_row")
    }
  )
  
  observeEvent(
    input$getExternalResult, {
      shinyjs::show("external_results_row")
    }
  )

  # Load demo data when the corresponding button is clicked
  observe({
    input$loadDemo
    updateTextInput(session, "myTextData", value = demoTextData)
  })
  
  # Load demo data when the corresponding button is clicked in external consistency panel
  observe({
    input$loadDemo_e
    updateTextInput(session, "myTextData_e", value = demoTextData)
  })
  
  observe({
    input$loadTrainingDemo
    updateTextInput(session, "internalTrainingData", value = demoInternalTrainingData)
  })
  
  # Clear internal training text input area
  observe({
    input$clearTraining
    updateTextInput(session, "internalTrainingData", value = "")
  })
  
  # Clear text input area
  observe({
    input$clearText
    updateTextInput(session, "myTextData", value = "")
  })
  
  # Clear text input area of external consistency 
  observe({
    input$clearText_e
    updateTextInput(session, "myTextData_e", value = "")
  })
  
  inFile <- reactive({input$btn_getOCR})
  observe({  
    if(!is.null(inFile())){
      text_OCR <- get_OCR_text(input$btn_getOCR$datapath)
      updateTextInput(session, "myTextData_e", value = as.character(text_OCR))
      output$OCR_image <- renderImage({
        list(src = input$btn_getOCR$datapath[1],
             alt = "Image failed to render", width="100%", height = "auto")
      }, deleteFile = FALSE)
    }

  })
}
