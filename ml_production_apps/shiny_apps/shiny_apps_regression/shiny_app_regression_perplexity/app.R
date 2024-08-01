# Regression - Perplexity

# Libraries
install.packages("shiny")
install.packages("tidyverse")
install.packages("broom")  # For tidy model outputs

# App

# Load required libraries
library(shiny)
library(tidyverse)

# Load your pre-trained model (replace with your actual model)
# For demonstration, we will train a simple model on the Concrete dataset
concrete_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls")
model <- lm(compressive_strength ~ ., data = concrete_data)

# Define UI
ui <- fluidPage(
  titlePanel("Concrete Compressive Strength Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("cement", "Cement (kg/m^3)", value = 300, min = 0),
      numericInput("blast_furnace_slag", "Blast Furnace Slag (kg/m^3)", value = 100, min = 0),
      numericInput("fly_ash", "Fly Ash (kg/m^3)", value = 50, min = 0),
      numericInput("water", "Water (kg/m^3)", value = 150, min = 0),
      numericInput("superplasticizer", "Superplasticizer (kg/m^3)", value = 1.2, min = 0),
      numericInput("coarse_aggregate", "Coarse Aggregate (kg/m^3)", value = 1000, min = 0),
      numericInput("fine_aggregate", "Fine Aggregate (kg/m^3)", value = 650, min = 0),
      numericInput("age", "Age (days)", value = 28, min = 0),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      h3("Predicted Compressive Strength"),
      verbatimTextOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- data.frame(
      cement = input$cement,
      blast_furnace_slag = input$blast_furnace_slag,
      fly_ash = input$fly_ash,
      water = input$water,
      superplasticizer = input$superplasticizer,
      coarse_aggregate = input$coarse_aggregate,
      fine_aggregate = input$fine_aggregate,
      age = input$age
    )
    
    prediction <- predict(model, new_data)
    output$prediction <- renderText({ paste("Predicted Compressive Strength:", round(prediction, 2), "MPa") })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)