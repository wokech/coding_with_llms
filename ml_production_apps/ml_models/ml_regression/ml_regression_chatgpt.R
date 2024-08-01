# ML Regression Exercise with ChatGPT

# Prompt

# You are a machine learning scientist training undergraduates on 
# the basics of machine learning. Select a dataset from the UC Irvine 
# ML repo to do regression. Use a tidymodel approach to create a 
# tutorial for solving a common machine learning problem. 
# Don't use iris dataset

# Dataset

# Let's select the "Concrete Compressive Strength" dataset from the 
# UCI Machine Learning Repository for this regression tutorial. 
# This dataset contains information on the ingredients used in 
# concrete and their respective compressive strengths, which we 
# will predict.

# Step 1: Load Required Libraries

# Install tidymodels if not already installed
install.packages("tidymodels")
install.packages("skimr")

# Load libraries
library(tidymodels)
library(skimr)

# Step 2: Load and Explore the Dataset

# Load the dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"
concrete_data <- readxl::read_excel(url)

# Display the first few rows and summary
head(concrete_data)
skim(concrete_data)

# Step 3: Data Preprocessing

# Split the data into training and testing sets
set.seed(123)
data_split <- initial_split(concrete_data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 4: Create a Recipe

concrete_recipe <- recipe(Concrete_compressive_strength ~ ., data = train_data) %>%
  step_normalize(all_predictors())

# Step 5: Specify the Model

lm_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Step 6: Create a Workflow

concrete_workflow <- workflow() %>%
  add_recipe(concrete_recipe) %>%
  add_model(lm_model)

# Step 7: Train the Model

lm_fit <- concrete_workflow %>%
  fit(data = train_data)

# Step 8: Evaluate the Model

# Make predictions
lm_predictions <- predict(lm_fit, new_data = test_data) %>%
  bind_cols(test_data)

# Evaluate performance
lm_metrics <- lm_predictions %>%
  metrics(truth = Concrete_compressive_strength, estimate = .pred)

lm_metrics

# Step 9: Visualize the Results

# Scatter plot of actual vs. predicted values
lm_predictions %>%
  ggplot(aes(x = Concrete_compressive_strength, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Values",
       x = "Actual Compressive Strength",
       y = "Predicted Compressive Strength")

# Step 10: Summary

# Print metrics
lm_metrics

# Discuss:
# - Model performance (R-squared, RMSE, MAE)
# - Potential feature engineering or model tuning
# - Alternative models to try (e.g., Random Forest, XGBoost)

# Save and load the model to create Shiny App

# Assuming lm_fit is your trained linear regression model
saveRDS(lm_fit, file = "lm_model.RDS")

lm_fit <- readRDS("lm_model.RDS")