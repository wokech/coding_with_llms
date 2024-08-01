# ML Classification Exercise with HuggingChat

# Prompt

# You are a machine learning scientist training undergraduates 
# on the basics of machine learning. Select a dataset from the 
# UC Irvine ML repo to do classification. Use a tidymodel approach 
# to create a tutorial for solving a common machine learning problem. 
# Don't use iris dataset.

# Dataset

# Let's use the "Wine Quality" dataset from the UC Irvine ML repo. 
# This dataset contains 4898 samples of white wine, each described 
# by 11 physico-chemical properties (e.g. pH, acidity, etc.) and 
# a quality score (ranging from 3 to 9).

# Our goal will be to predict the quality score of a wine based 
# on its physico-chemical properties.

# Step 1: Load necessary libraries and data

library(tidymodels)
library(tidyverse)

# Load the wine quality dataset
data("wine_quality", package = "dslabs")

# Take a look at the data
glimpse(wine_quality)

# Step 2: Preprocess the data

# Split the data into training and testing sets
set.seed(123)
wine_quality_split <- initial_split(wine_quality, prop = 0.8)
wine_quality_train <- training(wine_quality_split)
wine_quality_test <- testing(wine_quality_split)

# Create a recipe to preprocess the data
recipe <- recipe(quality ~., data = wine_quality_train) %>%
  step_normalize(all_predictors()) %>%
  step_dummy(all_predictors(), one_hot = TRUE)

# Prepare the recipe
prepared_recipe <- prep(recipe, training = wine_quality_train)

# Bake the training and testing data
wine_quality_train_baked <- bake(prepared_recipe, new_data = wine_quality_train)
wine_quality_test_baked <- bake(prepared_recipe, new_data = wine_quality_test)

# Step 3: Create a classification model

# Create a logistic regression model
log_reg_model <- logistic_reg() %>%
  set_engine("glm")

# Fit the model to the training data
log_reg_fit <- log_reg_model %>%
  fit(quality ~., data = wine_quality_train_baked)

# Step 4: Evaluate the model

# Predict the quality scores for the testing data
log_reg_pred <- predict(log_reg_fit, new_data = wine_quality_test_baked)

# Evaluate the model using metrics
log_reg_metrics <- metrics(log_reg_pred, truth = wine_quality_test$quality)
log_reg_metrics

# Step 5: Tune the model

# Create a tuning grid for the logistic regression model
log_reg_grid <- expand.grid(penalty = c(0.1, 1, 10))

# Perform a grid search to find the best hyperparameters
log_reg_tune <- tune_grid(
  log_reg_model,
  grid = log_reg_grid,
  metrics = metric_set(accuracy, roc_auc),
  control = control_grid(save_preds = "all")
)

# Evaluate the best model
log_reg_best <- log_reg_tune %>%
  collect_metrics() %>%
  arrange(desc(mean)) %>%
  slice(1)
log_reg_best

# Step 6: Finalize the model

# Finalize the best model
final_log_reg_model <- log_reg_model %>%
  finalize_workflow(log_reg_best$workflow)

# Fit the final model to the entire training data
final_log_reg_fit <- final_log_reg_model %>%
  fit(quality ~., data = wine_train)

# Make predictions on the test data
final_log_reg_pred <- final_log_reg_fit %>%
  predict(wine_test)

# Evaluate the final model
final_log_reg_metrics <- final_log_reg_pred %>%
  metrics(wine_test$quality)

# Create a confusion matrix
final_log_reg_conf_mat <- final_log_reg_pred %>%
  conf_mat(wine_test$quality)

# Save the models

# Save the classification model
saveRDS(final_log_reg_fit, "classification_model.rds")