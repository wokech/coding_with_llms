library(shiny)
library(tidyverse)
library(readr)
library(reticulate)

# load the model .......

ui <- fluidPage(
  titlePanel("Regression Model Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("feature1", "Feature 1", value = 0),
      numericInput("feature2", "Feature 2", value = 0),
      # Add more input fields for other features
    ),
    mainPanel(
      h3("Predicted Value"),
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {
  prediction <- reactive({
    features <- data.frame(
      feature1 = input$feature1,
      feature2 = input$feature2,
      # Add other features
    )
    py$prediction = model.predict(features)
    return(py$prediction)
  })
  
  output$prediction <- renderText({
    paste("Predicted value:", prediction())
  })
}

shinyApp(ui = ui, server = server)
