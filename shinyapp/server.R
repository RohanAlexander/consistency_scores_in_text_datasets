# library(aRianna)
source("training_data.R")
source("OCR_text_func.R")

server <- function(input, output, session) {
#### Internal consistency score ####
  getInternalResults <- eventReactive(input$getInternalResult, {
    internalTrainingData <- aRianna::make_internal_consistency_dataset(input$internalTrainingData)
    aRianna::generate_internal_consistency_score(input$myTextData, internalTrainingData)
  })

  # Displays input text
  output$unexpected_words <- renderTable({
    unexpected_words <- getInternalResults()[[1]]
    colnames(unexpected_words) <- c("As Expected", "Unexpected", "Consistency Score")
    unexpected_words
  })

  # Displays scores
  output$internal_consistency_score <- renderTable({
    internal_consistency_score <- getInternalResults()[[2]]
    colnames(internal_consistency_score) <- c("First Words", "Last Word", "Last Word Expected", "Is Expected")
    internal_consistency_score
  })

#### Exteranl consistency score ####
  getExternalResults <- eventReactive(input$getExternalResult, {
    # Get the DF of the split trigrams
    myTextData_e <- trimws(input$myTextData_e)
    aRianna::generate_external_consistency_score(myTextData_e)
  })

  # Displays input text
  output$unexpected_words_e <- renderTable({
    unexpected_words <-getExternalResults()[[1]]
    colnames(unexpected_words) <- c("As Expected", "Count", "Consistency Score")
    unexpected_words
  })

  # Displays scores
  output$external_consistency_score <- renderTable({
    external_consistency_score <- getExternalResults()[[2]]
    colnames(external_consistency_score) <- c("First Words", "Last Word", "Last Word Expected", "Is Expected")
    external_consistency_score
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
    updateTextInput(session, "myTextData", value = demoInternalTextData)
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
