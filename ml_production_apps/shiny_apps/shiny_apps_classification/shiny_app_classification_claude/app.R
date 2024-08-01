# Classification - Claude

# Libraries

install.packages(c("shiny", "tidymodels"))

# App

library(shiny)
library(tidymodels)

# Load the trained model (assuming you've saved it)
final_model <- readRDS("final_wine_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Wine Classification Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("alcohol", "Alcohol:", value = 13, min = 11, max = 15, step = 0.1),
      numericInput("malic_acid", "Malic Acid:", value = 2, min = 0, max = 6, step = 0.1),
      numericInput("ash", "Ash:", value = 2.3, min = 1, max = 4, step = 0.1),
      numericInput("alcalinity_of_ash", "Alcalinity of Ash:", value = 20, min = 10, max = 30),
      numericInput("magnesium", "Magnesium:", value = 100, min = 70, max = 170),
      numericInput("total_phenols", "Total Phenols:", value = 2.5, min = 0, max = 4, step = 0.1),
      numericInput("flavanoids", "Flavanoids:", value = 2.5, min = 0, max = 6, step = 0.1),
      numericInput("nonflavanoid_phenols", "Nonflavanoid Phenols:", value = 0.3, min = 0, max = 1, step = 0.1),
      numericInput("proanthocyanins", "Proanthocyanins:", value = 1.5, min = 0, max = 4, step = 0.1),
      numericInput("color_intensity", "Color Intensity:", value = 5, min = 1, max = 15, step = 0.1),
      numericInput("hue", "Hue:", value = 1, min = 0, max = 2, step = 0.1),
      numericInput("od280_od315_of_diluted_wines", "OD280/OD315 of Diluted Wines:", value = 3, min = 1, max = 4, step = 0.1),
      numericInput("proline", "Proline:", value = 1000, min = 300, max = 2000),
      actionButton("predict", "Predict Wine Class")
    ),
    mainPanel(
      h3("Predicted Wine Class:"),
      textOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  prediction <- eventReactive(input$predict, {
    new_data <- tibble(
      alcohol = input$alcohol,
      malic_acid = input$malic_acid,
      ash = input$ash,
      alcalinity_of_ash = input$alcalinity_of_ash,
      magnesium = input$magnesium,
      total_phenols = input$total_phenols,
      flavanoids = input$flavanoids,
      nonflavanoid_phenols = input$nonflavanoid_phenols,
      proanthocyanins = input$proanthocyanins,
      color_intensity = input$color_intensity,
      hue = input$hue,
      od280_od315_of_diluted_wines = input$od280_od315_of_diluted_wines,
      proline = input$proline
    )
    predict(final_model, new_data, type = "class")$.pred_class
  })
  
  output$prediction <- renderText({
    paste("Class", prediction())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)