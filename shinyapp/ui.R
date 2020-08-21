library(shiny)
library(shinythemes)
library(shinyjs)
library(shiny)
library(tidyverse)
library(tau)
library(tm)

fluidPage(
  useShinyjs(),
  theme = shinytheme("cosmo"),
  titlePanel("Text Consistency Score"),
  # Input row
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
      actionButton("getResult", "Get Result", class = "btn-primary"),
    )
  ),
  # Result row
  shinyjs::hidden(
    fluidRow( id = "results_row",
      column(8, 
        h5("Unexpected Words"),
        tableOutput("unexpected_words"),
      ),
      column(4, 
        h5("Internal Consistency Score"),
        tableOutput("internal_consistency_score"),
      )
    )
  )
)
