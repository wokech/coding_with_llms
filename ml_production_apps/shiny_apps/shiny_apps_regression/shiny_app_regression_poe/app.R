# Regression - Poe

# Load necessary libraries
library(shiny)
library(tidymodels)

# Load the model
boston_workflow <- readRDS("boston_workflow.rds")

# Define the UI
ui <- fluidPage(
  titlePanel("Boston Housing Price Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("crim", "Crime Rate", 0.1, min = 0, max = 100),
      numericInput("zn", "Proportion of Residential Land Zoned for Lots Over 25,000 Sq.Ft.", 12.5, min = 0, max = 100),
      numericInput("indus", "Proportion of Non-Retail Business Acres Per Town", 11.36, min = 0, max = 100),
      numericInput("chas", "Charles River Dummy Variable (= 1 if Tract Bounds River; 0 Otherwise)", 0, min = 0, max = 1),
      numericInput("nox", "Nitric Oxides Concentration (Parts per 10 Million)", 0.54, min = 0, max = 1),
      numericInput("rm", "Average Number of Rooms Per Dwelling", 6.28, min = 0, max = 10),
      numericInput("age", "Proportion of Owner-Occupied Units Built Prior to 1940", 65.2, min = 0, max = 100),
      numericInput("dis", "Weighted Distances to Five Boston Employment Centres", 3.79, min = 0, max = 10),
      numericInput("rad", "Index of Accessibility to Radial Highways", 9, min = 0, max = 25),
      numericInput("tax", "Full-Value Property-Tax Rate Per $10,000", 408, min = 0, max = 800),
      numericInput("ptratio", "Pupil-Teacher Ratio by Town", 18.9, min = 0, max = 25),
      numericInput("black", "1000(Bk - 0.63)^2 Where Bk is the Proportion of Black Residents by Town", 392.83, min = 0, max = 500),
      numericInput("lstat", "Percentage of Lower Status of the Population", 12.65, min = 0, max = 50)
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
    
    prediction <- predict(boston_workflow, new_data)$result
    paste0("Predicted Median House Value: $", round(prediction, 2))
  })
}

# Run the app
shinyApp(ui, server)