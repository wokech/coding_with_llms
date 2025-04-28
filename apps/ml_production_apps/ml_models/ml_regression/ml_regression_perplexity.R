# ML Regression Exercise with Perplexity

# Prompt

# You are a machine learning scientist training undergraduates on 
# the basics of machine learning. Select a dataset from the UC Irvine 
# ML repo to do regression. Use a tidymodel approach to create a 
# tutorial for solving a common machine learning problem. 
# Don't use iris dataset

# Dataset

# We will use the Concrete Compressive Strength dataset. 
# This dataset aims to predict the concrete compressive strength 
# based on various ingredients and properties.
# 
# The Concrete Compressive Strength dataset consists of 1,030 
# instances and 9 attributes, including cement, blast furnace slag, 
# fly ash, water, superplasticizer, coarse aggregate, fine aggregate, 
# age, and concrete compressive strength. The goal is to predict 
# the concrete compressive strength based on the given features.

# Step 1: Load Required Libraries

library(tidyverse)
library(tidymodels)

# Step 2: Load the Dataset

# Load the dataset
concrete_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls")

# Step 3: Data Preprocessing

# Remove rows with missing values
concrete_data <- concrete_data %>% drop_na()

# Step 4: Split the Data

set.seed(123)  # For reproducibility
data_split <- initial_split(concrete_data, prop = 0.75, strata = age)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 5: Create a Recipe

recipe <- recipe(compressive_strength ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors())

# Step 6: Specify the Model

model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Step 7: Create a Workflow

workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model)

# Step 8: Train the Model

fit <- workflow %>%
  fit(data = train_data)

# Step 9: Evaluate the Model

predictions <- predict(fit, test_data) %>%
  bind_cols(test_data)

metrics <- predictions %>%
  metrics(truth = compressive_strength, estimate = .pred)

print(metrics)

# Step 10: Make Predictions

new_data <- tibble(
  cement = 300,
  blast_furnace_slag = 100,
  fly_ash = 50,
  water = 150,
  superplasticizer = 1.2,
  coarse_aggregate = 1000,
  fine_aggregate = 650,
  age = 28
)

new_prediction <- predict(fit, new_data)
print(new_prediction)

# Save models

# Save the regression model
saveRDS(regression_model, "regression_model.rds")