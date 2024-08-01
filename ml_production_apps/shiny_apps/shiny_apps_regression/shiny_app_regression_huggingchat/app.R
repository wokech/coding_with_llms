# Regression - HuggingChat

# Load required libraries
library(shiny)
library(tidyverse)
library(broom)

# Load the trained regression model
load("regression_model.RData")

# Define the UI
ui <- fluidPage(
  # Input fields for user to enter data
  sidebarLayout(
    sidebarPanel(
      numericInput("crim", "Per capita crime rate by town"),
      numericInput("zn", "Proportion of residential land zoned for lots over 25,000 sq.ft."),
      numericInput("indus", "Proportion of non-retail business acres per town"),
      numericInput("chas", "Charles River dummy variable"),
      numericInput("nox", "Nitrogen oxide concentration (parts per 10 million)"),
      numericInput("rm", "Average number of rooms per dwelling"),
      numericInput("age", "Proportion of owner-occupied units built prior to 1940"),
      numericInput("dis", "Weighted distances to five Boston employment centers"),
      numericInput("rad", "Index of accessibility to radial highways"),
      numericInput("tax", "Full-value property-tax rate per $10,000"),
      numericInput("ptratio", "Pupil-teacher ratio by town"),
      numericInput("black", "1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town"),
      numericInput("lstat", "Lower status of the population")
    ),
    # Output field to display the predicted value
    mainPanel(
      textOutput("prediction")
    )
  )
)

# Define the server function
server <- function(input, output) {
  # Create a reactive expression to generate the prediction
  prediction <- reactive({
    # Create a data frame with the user input values
    new_data <- data.frame(
      crim = input$crim,
      zn = input$zn,
      indus = input$indus,
      chas = input$chas,
      nox = input$nox,
      rm = input$rm,
      age = input$age,
      dis = input$dis,
      rad = input$rad,
      tax = input$tax,
      ptratio = input$ptratio,
      black = input$black,
      lstat = input$lstat
    )
    # Make the prediction using the trained model
    predict(regression_model, new_data)
  })
  
  # Output the prediction
  output$prediction <- renderText({
    paste("Predicted median house price:", round(prediction(), 2))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)