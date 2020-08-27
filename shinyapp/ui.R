library(shiny)
library(shinythemes)
library(shinyjs)
library(shiny)
library(tidyverse)
library(tau)
library(tm)

navbarPage("Arianna",
   theme = shinytheme("cosmo"),
   tabPanel("Internal Consistency",
      useShinyjs(),
      fluidRow(
        # Input Panel: Internal Training Data
        column(6, wellPanel(
          h4("Internal Training Data"),
          actionButton("loadTrainingDemo", "Load Demo Data", class = "btn-sm btn-primary"),
          textAreaInput("internalTrainingData", "", value = "", height = "300px"),
          actionButton("clearTraining", "Clear", class = "btn-sm btn-outline-secondary"),
        )),
        # Input Panel
        column(6, wellPanel(
          h4("Your Text Data"),
          actionButton("loadDemo", "Load Demo Data", class = "btn-sm btn-primary"),
          textAreaInput("myTextData", "", value = "", height = "300px"),
          actionButton("clearText", "Clear", class = "btn-sm btn-outline-secondary"),
        )),
      ),
      # Action button row
      fluidRow(
        column(12,
               actionButton("getInternalResult", "Get Result", class = "btn-primary"),
        )
      ),
      # Result row
      shinyjs::hidden(
        fluidRow( id = "internal_results_row",
          column(6,
                 h5("Internal Consistency Score"),
                 tableOutput("unexpected_words"),
          ),
          column(6,
                 h5("Unexpected Words"),
                 tableOutput("internal_consistency_score"),
          )
        )
      )
   ),
   tabPanel("External Consistency",
      useShinyjs(),
      fluidRow(
        # Input Panel
        column(6, wellPanel(
          h4("Your Text Data"),
          fileInput("btn_getOCR", "Choose a file", accept = c('image/png', 'image/jpeg'), multiple = FALSE),
          actionButton("loadDemo_e", "Load Demo Data", class = "btn-sm btn-primary"),
          textAreaInput("myTextData_e", "", value = "", height = "300px"),
          actionButton("clearText_e", "Clear", class = "btn-sm btn-outline-secondary"),
        )),
        column(6,
          imageOutput("OCR_image")
        )
      ),
      # Action button row
      fluidRow(
        column(12,
               actionButton("getExternalResult", "Get Result", class = "btn-primary"),
        )
      ),
      # Result row
      shinyjs::hidden(
        fluidRow( id = "external_results_row",
          column(6,
                 h5("External Consistency Score"),
                 tableOutput("unexpected_words_e"),
          ),
          column(6,
                 h5("Unexpected Words"),
                 tableOutput("external_consistency_score"),
          )
        )
      )
   )
)


