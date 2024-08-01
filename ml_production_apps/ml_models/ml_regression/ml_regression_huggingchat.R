# ML Regression Exercise with HuggingChat

# Prompt

# You are a machine learning scientist training undergraduates on 
# the basics of machine learning. Select a dataset from the UC Irvine 
# ML repo to do regression. Use a tidymodel approach to create a 
# tutorial for solving a common machine learning problem. 
# Don't use iris dataset

# Dataset

# Let's use the "Boston Housing" dataset from the UC Irvine ML repo. 
# This dataset contains 506 samples of housing data from Boston, 
# each described by 13 features (e.g. number of rooms, distance to 
# employment centers, etc.) and a target variable 
# (median house price).
# 
# Our goal will be to predict the median house price based on the 
# features.

# Step 1: Load necessary libraries and data

library(tidymodels)
library(tidyverse)

# Load the Boston Housing dataset
data("BostonHousing", package = "MASS")

# Take a look at the data
glimpse(BostonHousing)

# Step 2: Preprocess the data

# Split the data into training and testing sets
set.seed(123)
boston_split <- initial_split(BostonHousing, prop = 0.8)
boston_train <- training(boston_split)
boston_test <- testing(boston_split)

# Create a recipe to preprocess the data
recipe <- recipe(medv ~., data = boston_train) %>%
  step_normalize(all_predictors()) %>%
  step_interact(~ poly(rm, degree = 2) + poly(age, degree = 2))

# Prepare the recipe
prepared_recipe <- prep(recipe, training = boston_train)

# Bake the training and testing data
boston_train_baked <- bake(prepared_recipe, new_data = boston_train)
boston_test_baked <- bake(prepared_recipe, new_data = boston_test)

# Step 3: Create a regression model

# Create a linear regression model
lm_model <- linear_reg() %>%
  set_engine("lm")

# Fit the model to the training data
lm_fit <- lm_model %>%
  fit(medv ~., data = boston_train_baked)

# Step 4: Evaluate the model

# Predict the median house prices for the testing data
lm_pred <- predict(lm_fit, new_data = boston_test_baked)

# Evaluate the model using metrics
lm_metrics <- metrics(lm_pred, truth = boston_test$medv)
lm_metrics

# Step 5: Tune the model

# Create a tuning grid for the linear regression model
lm_grid <- expand.grid(penalty = c(0, 0.1, 1, 10))

# Perform a grid search to find the best hyperparameters
lm_tune <- tune_grid(
  lm_model,
  grid = lm_grid,
  metrics = metric_set(rmse, rsq),
  control = control_grid(save_preds = "all")
)

# Evaluate the best model
lm_best <- lm_tune %>%
  collect_metrics() %>%
  arrange(desc(mean)) %>%
  slice(1)
lm_best

# Step 6: Finalize the model

# Finalize the best model
final_lm_model <- lm_model %>%
  finalize_workflow(lm_best$workflow)

# Fit the final model to the entire training data
final_lm_fit <- final_lm_model %>%
  fit(medv ~., data = boston_train_baked)

# Step 7: Make predictions and visualize results

# Make predictions on new data
new_data <- data.frame(
  crim = 3.5,
  zn = 10,
  indus = 2.5,
  chas = 0,
  nox = 0.5,
  rm = 6,
  age = 65,
  dis = 4,
  rad = 1,
  tax = 280,
  ptratio = 15,
  black = 0.5,
  lstat = 5
)

new_pred <- predict(final_lm_fit, new_data = new_data)
new_pred

# Visualize the results
ggplot(boston_test, aes(x = medv, y = pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Median House Price", y = "Predicted Median House Price")

# Save the models

# Save the regression model
saveRDS(final_log_reg_fit, "regression_model.rds")