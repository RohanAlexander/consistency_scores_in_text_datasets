function(input, output, session) {

  output$outputText <- renderText({
    as.character(input$my_text)
  })

  observe({
    # Run whenever clear button is pressed
    input$clear

    # Send an update to my_text, resetting its value
    updateTextInput(session, "my_text", value = "")
  })
}
