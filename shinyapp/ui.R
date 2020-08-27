library(shiny)
library(shinythemes)
library(shinyjs)
library(shiny)

introduction <- c("Alphabets Really In Absolute Need of New Alphabet-ing or `aRianna` is a package that helps identify potential issues in text datasets by comparing the actual text with that expected by various natural language models. The package also provides a 'consistency score' that can be used to monitor how the text dataset changes during the cleaning and preparation process.")
package_repo <- c("Github repository is available at: ")
more_information <- c("More information is available at: ")
installation_code <- c("devtools::install_github('RohanAlexander/arianna')")
github_arianna <- c("https://github.com/RohanAlexander/aRianna")
github_consistency <- c("https://github.com/RohanAlexander/consistency_scores_in_text_datasets")
author1 <- c("Rohan Alexander: \t rohan.alexander@utoronto.ca")
author2 <- c("Ke-Li Chiu: \t keli.chiu@mail.utoronto.ca")

navbarPage("aRianna",
   theme = shinytheme("cosmo"),
   tabPanel("About",
      fluidRow(
        # Input Panel: Internal Training Data
        column(12, wellPanel(
          h1("Introduction to aRianna"),
          p(introduction),
          p(package_repo),
          a(github_arianna),
          p(more_information),
          a(github_consistency),
          h3("Installation"),
          code(installation_code),
          h3("Authors"),
          p(author1),
          p(author2),

        ))
      ),
    ),
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


