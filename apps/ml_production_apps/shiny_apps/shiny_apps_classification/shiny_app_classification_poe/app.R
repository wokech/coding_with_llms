# Classification - Poe

# Load necessary libraries
library(shiny)
library(tidymodels)

# Load the model
iris_workflow <- readRDS("iris_workflow.rds")

# Define the UI
ui <- fluidPage(
  titlePanel("Iris Flower Classifier"),
  sidebarLayout(
    sidebarPanel(
      numericInput("sepal_length", "Sepal Length", 5.8, min = 4.0, max = 8.0),
      numericInput("sepal_width", "Sepal Width", 3.0, min = 2.0, max = 5.0),
      numericInput("petal_length", "Petal Length", 3.8, min = 1.0, max = 7.0),
      numericInput("petal_width", "Petal Width", 1.2, min = 0.1, max = 2.5)
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)

# Define the server
server <- function(input, output) {
  output$prediction <- renderText({
    new_data <- data.frame(
      sepal_length = input$sepal_length,
      sepal_width = input$sepal_width,
      petal_length = input$petal_length,
      petal_width = input$petal_width
    )
    
    prediction <- predict(iris_workflow, new_data)$class
    paste0("Predicted Iris Flower Species: ", prediction)
  })
}

# Run the app
shinyApp(ui, server)