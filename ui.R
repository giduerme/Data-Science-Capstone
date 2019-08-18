library(shiny)

# Define UI for application that predicts next word
shinyUI(fluidPage(
  
    # Application title
    titlePanel("What is the Next Word?"),
    p("A Data Science Capstone Final Project"),
    br(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        textInput("Input", "Enter any word or words", value = ""),
        actionButton("do", "Predict")
      ),
    
      # Show a plot of the generated distribution
      mainPanel(
        h5("Next predicted word:"), 
        fluidRow(column(5, verbatimTextOutput("PredictedWord", placeholder = TRUE)))
      )
    )
))