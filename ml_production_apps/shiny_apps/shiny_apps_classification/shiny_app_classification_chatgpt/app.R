# Classification ChatGPT

# Libraries
# install.packages(c("shiny", "tidymodels", "randomForest"))

# Shiny App

library(shiny)
library(tidymodels)
library(randomForest)

# Load or train the model
# Assuming rf_fit is your trained random forest model
readRDS("C:/R_Files/coding_with_llms/ml_production_apps/saved_models/rf_model.RDS")

# UI
ui <- fluidPage(
  titlePanel("Heart Disease Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age:", 30, min = 1, max = 100),
      selectInput("sex", "Sex:", choices = c("Male" = 1, "Female" = 0)),
      numericInput("trestbps", "Resting Blood Pressure:", 120),
      numericInput("chol", "Cholesterol:", 200),
      numericInput("thalach", "Maximum Heart Rate Achieved:", 150),
      selectInput("cp", "Chest Pain Type:", choices = 0:3),
      selectInput("fbs", "Fasting Blood Sugar > 120 mg/dl:", choices = c("Yes" = 1, "No" = 0)),
      selectInput("restecg", "Resting Electrocardiographic Results:", choices = 0:2),
      selectInput("exang", "Exercise Induced Angina:", choices = c("Yes" = 1, "No" = 0)),
      numericInput("oldpeak", "ST Depression:", 1.0),
      selectInput("slope", "Slope of the Peak Exercise ST Segment:", choices = 0:2),
      selectInput("ca", "Number of Major Vessels Colored by Fluoroscopy:", choices = 0:3),
      selectInput("thal", "Thalassemia:", choices = 1:3),
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
      age = input$age,
      sex = as.numeric(input$sex),
      cp = as.factor(input$cp),
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = as.factor(input$fbs),
      restecg = as.factor(input$restecg),
      thalach = input$thalach,
      exang = as.factor(input$exang),
      oldpeak = input$oldpeak,
      slope = as.factor(input$slope),
      ca = as.factor(input$ca),
      thal = as.factor(input$thal)
    )
    
    prediction <- predict(rf_fit, new_data)
    output$result <- renderText({
      paste("Prediction:", ifelse(prediction == 1, "Heart Disease", "No Heart Disease"))
    })
  })
}

shinyApp(ui = ui, server = server)

