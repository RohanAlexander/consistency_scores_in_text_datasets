library(shiny)
library(shinythemes)
library(shinyjs)
library(shiny)
library(tidyverse)
library(tau)
library(tm)
library(hash)
#source("functions.R")
#load("hashtable.Rdata")

fluidPage(
  useShinyjs(),
  theme = shinytheme("cosmo"),
  titlePanel("Text Consistency Score"),

  fluidRow(
    column(6, wellPanel(
      h4("Place your data here"),
      actionButton("loadDemo", "Load Demo Data", class = "btn-sm btn-primary"),
      textAreaInput("myText", "", value = "", height = "500px"),
      actionButton("getResult", "Get Result", class = "btn-primary"),
      actionButton("clear", "Clear", class = "btn-outline-secondary"),

    )),
    column(6, wellPanel(
      h4("See your result here"),
      tableOutput("outputText"),
      hr(),
      tableOutput("outputScores"),
    ))
  )
)
