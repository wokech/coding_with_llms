# Obesity Shiny App

library(shiny)
library(tidymodels)
library(tidyverse)
library(bslib)

# Load the saved model
model <- readRDS("obesity_prediction_model.rds")

# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  
  # Title
  titlePanel("Obesity Level Prediction"),
  
  # Sidebar with input fields
  sidebarLayout(
    sidebarPanel(
      h4("Personal Information"),
      numericInput("age", "Age", value = 30, min = 1, max = 100),
      selectInput("age_group", "Age Group", choices = c("Teen", "Young Adult", "Adult", "Middle Age", "Senior")),
      selectInput("gender", "Gender", choices = c("Male", "Female")),
      numericInput("height", "Height (cm)", value = 170, min = 100, max = 250),
      numericInput("weight", "Weight (kg)", value = 70, min = 30, max = 300),
      
      h4("Lifestyle Factors"),
      selectInput("family_history", "Family History of Overweight", 
                  choices = c("Yes", "No")),
      selectInput("favc", "Frequent Consumption of High Caloric Food", 
                  choices = c("Yes", "No")),
      sliderInput("fcvc", "Frequency of Vegetable Consumption", 
                  min = 1, max = 3, value = 2, step = 0.1),
      sliderInput("ncp", "Number of Main Meals", 
                  min = 1, max = 4, value = 3, step = 1),
      selectInput("caec", "Consumption of Food Between Meals", 
                  choices = c("No", "Sometimes", "Frequently", "Always")),
      selectInput("smoke", "Smoking", choices = c("Yes", "No")),
      sliderInput("ch2o", "Daily Water Consumption (L)", 
                  min = 1, max = 3, value = 2, step = 0.1),
      selectInput("scc", "Calories Consumption Monitoring", 
                  choices = c("Yes", "No")),
      sliderInput("faf", "Physical Activity Frequency", 
                  min = 0, max = 3, value = 1, step = 0.1),
      sliderInput("tue", "Time Using Technology Devices (hours)", 
                  min = 0, max = 5, value = 2, step = 0.1),
      selectInput("calc", "Alcohol Consumption", 
                  choices = c("No", "Sometimes", "Frequently", "Always")),
      selectInput("mtrans", "Transportation Used", 
                  choices = c("Automobile", "Bike", "Motorbike", "Public_Transportation", "Walking"))
    ),
    
    # Main panel with output
    mainPanel(
      h3("Prediction Result"),
      verbatimTextOutput("prediction"),
      h4("Interpretation"),
      textOutput("interpretation"),
      h4("BMI Information"),
      textOutput("bmi_info"),
      plotOutput("feature_importance")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to create input data frame
  input_data <- reactive({
    data.frame(
      gender = input$gender,
      age = input$age,
      age_group = input$age_group,
      height = input$height,
      weight = input$weight,
      family_history = input$family_history,
      favc = input$favc,
      fcvc = input$fcvc,
      ncp = input$ncp,
      caec = input$caec,
      smoke = input$smoke,
      ch2o = input$ch2o,
      scc = input$scc,
      faf = input$faf,
      tue = input$tue,
      calc = input$calc,
      mtrans = input$mtrans,
      bmi = input$weight / (input$height / 100)^2
    )
  })
  
  # Make prediction
  prediction <- reactive({
    predict(model, new_data = input_data(), type = "class")
  })
  
  # Output prediction
  output$prediction <- renderText({
    paste("Predicted Obesity Level:", prediction())
  })
  
  # Interpretation of the result
  output$interpretation <- renderText({
    result <- prediction()
    interpretations <- c(
      "Insufficient_Weight" = "Your weight is below the healthy range. Please consult a healthcare professional for advice on gaining weight safely.",
      "Normal_Weight" = "Your weight is within the healthy range. Keep up your good habits!",
      "Overweight_Level_I" = "You are slightly overweight. Consider making small changes to your diet and increasing physical activity.",
      "Overweight_Level_II" = "You are moderately overweight. It's recommended to consult a healthcare professional for personalized advice on weight management.",
      "Obesity_Type_I" = "You fall into the obesity category. It's important to consult a healthcare professional for a comprehensive weight management plan.",
      "Obesity_Type_II" = "You fall into the severe obesity category. Please consult a healthcare professional urgently for a comprehensive weight management plan.",
      "Obesity_Type_III" = "You fall into the very severe obesity category. It's crucial to seek immediate medical attention for a comprehensive weight management plan."
    )
    interpretations[unlist(result)]
  })
  
  # BMI information
  output$bmi_info <- renderText({
    bmi <- input$weight / (input$height / 100)^2
    paste("Your BMI is:", round(bmi, 2))
  })
  
  # Feature importance plot
  output$feature_importance <- renderPlot({
    vip::vip(model, num_features = 10)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)