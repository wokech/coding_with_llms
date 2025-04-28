# ML Classification Exercise with Claude

# Prompt

# You are a machine learning scientist training undergraduates 
# on the basics of machine learning. Select a dataset from the 
# UC Irvine ML repo to do classification. Use a tidymodel approach 
# to create a tutorial for solving a common machine learning problem. 
# Don't use iris dataset.

# Dataset

# Problem: Customer churn, the loss of customers, is a critical 
# issue for businesses. Predicting which customers are likely to 
# churn can help companies implement targeted retention strategies.

# Dataset: We will use the "Telco Customer Churn" dataset from 
# the UC Irvine ML repository for this tutorial. It contains 
# information about customers, including demographic data, 
# services subscribed, and churn status.

# Goal: To build a classification model to predict whether a 
# customer will churn or not.

# 1. Libraries

library(tidyverse)
library(tidymodels)
library(readr)
library(caret)
library(skimr)
library(recipes)
library(rsample)
library(glmnet)
library(xgboost)
library(ranger)
library(yardstick)
library(parsnip)

# 2. Data Import and Exploration

# Import data
churn_data <- read_csv("https://raw.githubusercontent.com/uciml/UCI-ML-wine-quality/main/winequality-red.csv")

# Explore data
skim(churn_data)

# 3. Data Preprocessing

# Split data into training and testing sets
set.seed(123)
split <- initial_split(churn_data, prop = 0.7)
train <- training(split)
test <- testing(split)

# Create a recipe for preprocessing
rec <- recipe(churn ~ ., data = train) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_numeric())

# Prepare data for modeling
prep <- prep(rec)
train_data <- bake(prep, new_data = train)
test_data <- bake(prep, new_data = test)

# 4. Model Building and Training

# Define model specifications
glmnet_spec <- logistic_reg(penalty = "elasticnet") %>%
  set_engine("glmnet")

xgboost_spec <- boost_tree(tree_depth = 6) %>%
  set_engine("xgboost")

ranger_spec <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Tune hyperparameters
glmnet_grid <- expand_grid(penalty = c(0, 0.1, 0.5), mixture = c(0, 0.5, 1))
xgboost_grid <- expand_grid(tree_depth = c(3, 6, 9), mtry = c(2, 4, 6))
ranger_grid <- expand_grid(mtry = c(2, 4, 6), min_n = c(5, 10, 15))

# Create workflows
glmnet_workflow <- workflow() %>%
  add_model(glmnet_spec) %>%
  add_recipe(rec)

xgboost_workflow <- workflow() %>%
  add_model(xgboost_spec) %>%
  add_recipe(rec)

ranger_workflow <- workflow() %>%
  add_model(ranger_spec) %>%
  add_recipe(rec)

# Tune models
glmnet_tune <- tune_grid(
  glmnet_workflow,
  resamples = vfold_cv(train_data, v = 5),
  grid = glmnet_grid,
  metrics = metric_set(accuracy, roc_auc)
)

# Similar tuning for xgboost_tune and ranger_tune

# 5. Model Evaluation

# Select best models
best_glmnet <- select_best(glmnet_tune, metric = "roc_auc")
best_glmnet_final <- finalize(glmnet_spec, best_glmnet)
glmnet_final_fit <- fit(glmnet_workflow %>% update_model(best_glmnet_final), data = train_data)

# Similar finalization and fitting for xgboost and ranger models

# Evaluate models on test data
glmnet_pred <- predict(glmnet_final_fit, new_data = test_data)
glmnet_metrics <- metric_set(accuracy, roc_auc, sensitivity, specificity)(test_data$churn, glmnet_pred$.pred_class)

# Similar evaluation for xgboost and ranger models

# Save the model

# Assuming you have a trained model named 'model'

# Save the model to a file
saveRDS(model, file = "your_model.rds")
