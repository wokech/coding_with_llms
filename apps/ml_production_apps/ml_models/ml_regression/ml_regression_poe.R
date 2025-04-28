# ML Regression Exercise with Poe

# Prompt

# You are a machine learning scientist training undergraduates on 
# the basics of machine learning. Select a dataset from the UC Irvine 
# ML repo to do regression. Use a tidymodel approach to create a 
# tutorial for solving a common machine learning problem. 
# Don't use iris dataset

# Dataset

# The Boston Housing dataset contains information about various 
# features of houses in the Boston area, such as the crime rate, 
# number of rooms, and proximity to important landmarks. The goal 
# is to create a model that can predict the median value of the 
# houses based on these features.

# Steps

# 1. Loaded the necessary libraries, including the tidyverse and 
# tidymodels packages.
# 2. Loaded the Boston Housing dataset and explored the data using 
# the skim() function.
# 3. Split the data into training and testing sets using 
# initial_split().
# 4. Defined a recipe to preprocess the data, including 
# normalizing the numeric predictors and creating dummy variables 
# for the categorical variables.
# 5. Defined a linear regression model using the linear_reg() 
# function.
# 6. Created a workflow that combines the recipe and the model.
# 7. Fitted the model to the training data using the workflow.
# 8. Evaluated the model's performance on the test set using 
# various metrics such as root mean squared error (RMSE), 
# R-squared, and mean absolute error (MAE).

# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(skimr)

# Load the Boston Housing dataset
boston_data <- read_csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/boston.csv")

# Explore the data
skim(boston_data)

# Split the data into training and testing sets
set.seed(123)
boston_split <- initial_split(boston_data, prop = 0.8)
boston_train <- training(boston_split)
boston_test <- testing(boston_split)

# Define the recipe
boston_recipe <- recipe(medv ~ ., data = boston_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Define the model
boston_model <- linear_reg() %>%
  set_engine("lm")

# Create the workflow
boston_workflow <- workflow() %>%
  add_recipe(boston_recipe) %>%
  add_model(boston_model)

# Fit the model
boston_fit <- boston_workflow %>%
  fit(data = boston_train)

# Evaluate the model on the test set
boston_pred <- predict(boston_fit, boston_test)
boston_metrics <- metric_set(rmse, rsq, mae)
boston_metrics(boston_pred, boston_test$medv)

# Save the model

# Save the classification model
saveRDS(boston_fit, "boston_fit.rds")