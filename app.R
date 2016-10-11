library(readr)
library(stringr)
library(dplyr)
library(ngram)
library(feather)

library(shiny)

source("helpers.R")

bigram_table <- read_feather("data/bigram.feather")
trigram_table <- read_feather("data/trigram.feather")


ui <- fluidPage(theme = "style.css",  fluidRow(column(
  width = 12,
  
  div(
    id = "input_area",
    class = "jumbotron text-center",
    h1("Swiftkey Text Predictor"),
    p("Ready? Enter your text"),
    div(
      class = "form-inline",
      textInput(
        "input",
        label = "",
        width = "220",
        placeholder = "Start typing..."
      ),
      actionButton(inputId = "predict", label = "Submit")
      
    )
  )
))
,

div(
  fluidRow(class = 'w3-row-padding w3-center w3-margin-top', div(box(
    "You entered:",
    textOutput("myinput"),
    icon("download", lib = "glyphicon")
  )),
  div(
    box(
      " Next predicted word:",
      textOutput("value"),
      icon("ok-circle", lib = "glyphicon")
    )
  ),
  div(
    box(
      "Other candidates:",
      textOutput("candidates"),
      icon("cog", lib = "glyphicon")
    )
  ),
  div(box(
    "How?",
    p(class = "description", "Using ngram model and a simple backoff algorithm"),
    icon("info-sign", lib = "glyphicon")
  )))
))




server <- shinyServer(function(input, output) {
  input_phrase <- eventReactive(input$predict, {
    myfile <- input(input$input)
  })
  
  dataset <- reactive({
    predictor(input_phrase(),
              bigram_table = bigram_table,
              trigram_table = trigram_table)
    
  })
  output$value <- renderText({
    as.character(dataset()$temp1[1])
    
  })
  
  output$myinput <- renderText({
    input$input
  })
  
  output$candidates <- renderText({
    paste(dataset()$candidates, collapse = ", ")
    
    
  })
  
  
})


shinyApp(ui = ui, server = server)
