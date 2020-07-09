source("text-input.R")

fluidPage(
  titlePanel("Text Consistency Score"),

  fluidRow(
    column(6, wellPanel(
      textInput("my_text", label = h3("Text input"), value = ""),
      actionButton("clear", "Clear")
    )),
    column(6, wellPanel(
      verbatimTextOutput("outputText")
    ))
  )
)
