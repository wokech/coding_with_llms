# ML Classification Exercise with Poe

# Prompt

# You are a machine learning scientist training undergraduates 
# on the basics of machine learning. Select a dataset from the 
# UC Irvine ML repo to do classification. Use a tidymodel approach 
# to create a tutorial for solving a common machine learning problem. 
# Don't use iris dataset.

# Dataset

# The Titanic dataset contains information about passengers on the 
# famous Titanic ship that sank in 1912. The goal is to create a 
# predictive model that can determine whether a passenger survived 
# or not based on various features such as age, class, gender, 
# and others.

# Steps

# 1. Loaded the necessary libraries, including the tidyverse and
# tidymodels packages.
# 2. Loaded the Titanic dataset and explored the data using the 
# skim() function.
# 3. Split the data into training and testing sets using 
# initial_split().
# 4. Defined a recipe to preprocess the data, including handling 
# missing values and converting categorical variables to dummy 
# variables.
# 5. Defined a logistic regression model using the logistic_reg() 
# function.
# 6. Created a workflow that combines the recipe and the model.
# 7. Fitted the model to the training data using the workflow.
# 8. Evaluated the model's performance on the test set using 
# various metrics such as accuracy, precision, recall, and F1-score.

# Code Block

# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(skimr)

# Load the Titanic dataset
titanic_data <- read_csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

# Explore the data
skim(titanic_data)

# Split the data into training and testing sets
set.seed(123)
titanic_split <- initial_split(titanic_data, prop = 0.8)
titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)

# Define the recipe
titanic_recipe <- recipe(Survived ~ ., data = titanic_train) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Define the model
titanic_model <- logistic_reg() %>%
  set_engine("glm")

# Create the workflow
titanic_workflow <- workflow() %>%
  add_recipe(titanic_recipe) %>%
  add_model(titanic_model)

# Fit the model
titanic_fit <- titanic_workflow %>%
  fit(data = titanic_train)

# Evaluate the model on the test set
titanic_pred <- predict(titanic_fit, titanic_test)
titanic_metrics <- metric_set(accuracy, precision, recall, f1_score)
titanic_metrics(titanic_pred, titanic_test$Survived)

# Save the model

# Save the classification model
saveRDS(titanic_fit, "titanic_fit.rds")

