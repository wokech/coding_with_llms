# Regression - Claude

# Libraries

install.packages(c("shiny", "tidymodels"))

# App

library(shiny)
library(tidymodels)

# Load the trained model (assuming you've saved it)
final_model <- readRDS("final_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Auto MPG Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("cylinders", "Number of Cylinders:", value = 4, min = 3, max = 8),
      numericInput("displacement", "Displacement:", value = 150, min = 50, max = 500),
      numericInput("horsepower", "Horsepower:", value = 100, min = 50, max = 300),
      numericInput("weight", "Weight:", value = 3000, min = 1500, max = 5000),
      numericInput("acceleration", "Acceleration:", value = 12, min = 8, max = 25),
      numericInput("model_year", "Model Year:", value = 76, min = 70, max = 82),
      selectInput("origin", "Origin:", choices = c("1", "2", "3")),
      actionButton("predict", "Predict MPG")
    ),
    mainPanel(
      h3("Predicted MPG:"),
      textOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  prediction <- eventReactive(input$predict, {
    new_data <- tibble(
      cylinders = input$cylinders,
      displacement = input$displacement,
      horsepower = input$horsepower,
      weight = input$weight,
      acceleration = input$acceleration,
      model_year = input$model_year,
      origin = factor(input$origin)
    )
    predict(final_model, new_data)$.pred
  })
  
  output$prediction <- renderText({
    round(prediction(), 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)