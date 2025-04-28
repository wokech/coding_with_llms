# Classification - Perplexity

# Libraries

# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("broom")  # For tidy model outputs

# App

# Load required libraries
library(shiny)
library(tidyverse)

model <- readRDS("class_perplexity.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Income Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age", value = 40, min = 0),
      selectInput("workclass", "Work Class", choices = unique(adult_data$workclass)),
      numericInput("fnlwgt", "Final Weight", value = 123456, min = 0),
      selectInput("education", "Education", choices = unique(adult_data$education)),
      numericInput("education.num", "Education Number", value = 13, min = 0),
      selectInput("marital.status", "Marital Status", choices = unique(adult_data$marital.status)),
      selectInput("occupation", "Occupation", choices = unique(adult_data$occupation)),
      selectInput("relationship", "Relationship", choices = unique(adult_data$relationship)),
      selectInput("race", "Race", choices = unique(adult_data$race)),
      selectInput("sex", "Sex", choices = unique(adult_data$sex)),
      numericInput("capital.gain", "Capital Gain", value = 0, min = 0),
      numericInput("capital.loss", "Capital Loss", value = 0, min = 0),
      numericInput("hours.per.week", "Hours per Week", value = 40, min = 0),
      selectInput("native.country", "Native Country", choices = unique(adult_data$native.country)),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      h3("Predicted Income"),
      verbatimTextOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    new_data <- data.frame(
      age = input$age,
      workclass = input$workclass,
      fnlwgt = input$fnlwgt,
      education = input$education,
      education.num = input$education.num,
      marital.status = input$marital.status,
      occupation = input$occupation,
      relationship = input$relationship,
      race = input$race,
      sex = input$sex,
      capital.gain = input$capital.gain,
      capital.loss = input$capital.loss,
      hours.per.week = input$hours.per.week,
      native.country = input$native.country
    )
    
    prediction <- predict(model, new_data)
    output$prediction <- renderText({ paste("Predicted Income:", prediction$.pred_class) })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)