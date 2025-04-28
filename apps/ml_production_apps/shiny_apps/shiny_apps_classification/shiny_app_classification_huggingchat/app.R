# Classification - HuggingChat

# Load required libraries
library(shiny)
library(tidyverse)
library(broom)

# Load the trained classification model
load("classification_model.RData")

# Define the UI
ui <- fluidPage(
  # Input fields for user to enter data
  sidebarLayout(
    sidebarPanel(
      numericInput("fixed_acidity", "Fixed Acidity"),
      numericInput("volatile_acidity", "Volatile Acidity"),
      numericInput("citric_acid", "Citric Acid"),
      numericInput("residual_sugar", "Residual Sugar"),
      numericInput("chlorides", "Chlorides"),
      numericInput("free_sulfur_dioxide", "Free Sulfur Dioxide"),
      numericInput("total_sulfur_dioxide", "Total Sulfur Dioxide"),
      numericInput("density", "Density"),
      numericInput("pH", "pH"),
      numericInput("sulphates", "Sulphates"),
      numericInput("alcohol", "Alcohol")
    ),
    # Output field to display the predicted class
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
      fixed_acidity = input$fixed_acidity,
      volatile_acidity = input$volatile_acidity,
      citric_acid = input$citric_acid,
      residual_sugar = input$residual_sugar,
      chlorides = input$chlorides,
      free_sulfur_dioxide = input$free_sulfur_dioxide,
      total_sulfur_dioxide = input$total_sulfur_dioxide,
      density = input$density,
      pH = input$pH,
      sulphates = input$sulphates,
      alcohol = input$alcohol
    )
    # Make the prediction using the trained model
    predict(classification_model, new_data, type = "class")
  })
  
  # Output the prediction
  output$prediction <- renderText({
    paste("Predicted wine quality:", prediction())
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)