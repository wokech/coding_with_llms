# Regression - ChatGPT

# Libraries
install.packages(c("shiny", "tidymodels", "readxl"))

library(shiny)
library(tidymodels)
library(readxl)

# Load or train the model
# Assuming lm_fit is your trained linear regression model
# load("lm_model.RData")

# UI
ui <- fluidPage(
  titlePanel("Concrete Compressive Strength Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("cement", "Cement (kg/m^3):", 540),
      numericInput("slag", "Blast Furnace Slag (kg/m^3):", 0),
      numericInput("flyash", "Fly Ash (kg/m^3):", 0),
      numericInput("water", "Water (kg/m^3):", 162),
      numericInput("superplasticizer", "Superplasticizer (kg/m^3):", 2.5),
      numericInput("coarseagg", "Coarse Aggregate (kg/m^3):", 1040),
      numericInput("fineagg", "Fine Aggregate (kg/m^3):", 676),
      numericInput("age", "Age (days):", 28),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- tibble(
      cement = input$cement,
      slag = input$slag,
      flyash = input$flyash,
      water = input$water,
      superplasticizer = input$superplasticizer,
      coarseagg = input$coarseagg,
      fineagg = input$fineagg,
      age = input$age
    )
    
    prediction <- predict(lm_fit, new_data)
    output$result <- renderText({
      paste("Predicted Compressive Strength:", round(prediction, 2), "MPa")
    })
  })
}

shinyApp(ui = ui, server = server)
