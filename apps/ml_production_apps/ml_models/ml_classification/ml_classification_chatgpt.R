# ML Classification Exercise with ChatGPT

# Prompt

# You are a machine learning scientist training undergraduates 
# on the basics of machine learning. Select a dataset from the 
# UC Irvine ML repo to do classification. Use a tidymodel approach 
# to create a tutorial for solving a common machine learning problem. 
# Don't use iris dataset.

# Dataset

# "Heart Disease" dataset from the UCI Machine Learning Repository 
# for this tutorial. This dataset contains information on patients, 
# and the goal is to classify the presence or absence of heart 
# disease based on various features.

# Step 1: Load Required Libraries

# Install tidymodels if not already installed
# install.packages("tidymodels")
# install.packages("skimr")
# install.packages("themis")

# Load libraries
library(tidymodels)
library(skimr)
library(themis) # For handling imbalanced datasets

# Step 2: Load and Explore the Dataset

# Load the dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart_data <- read.csv(url, header = FALSE, na.strings = "?")

# Assign column names
colnames(heart_data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")

# Display the first few rows and summary
head(heart_data)
skim(heart_data)

# Step 3: Data Preprocessing
# Handle missing values, convert categorical variables, 
# and split the dataset.

# Handle missing values by removing rows with NA values
heart_data <- na.omit(heart_data)

# Convert target variable to a binary factor (0: no disease, 1: disease)
heart_data$target <- ifelse(heart_data$target > 0, 1, 0)
heart_data$target <- as.factor(heart_data$target)

# Convert categorical variables to factors
heart_data$sex <- as.factor(heart_data$sex)
heart_data$cp <- as.factor(heart_data$cp)
heart_data$fbs <- as.factor(heart_data$fbs)
heart_data$restecg <- as.factor(heart_data$restecg)
heart_data$exang <- as.factor(heart_data$exang)
heart_data$slope <- as.factor(heart_data$slope)
heart_data$ca <- as.factor(heart_data$ca)
heart_data$thal <- as.factor(heart_data$thal)

# Split the data into training and testing sets
set.seed(123)
data_split <- initial_split(heart_data, prop = 0.8, strata = target)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 4: Create a Recipe

# Define a preprocessing recipe.

heart_recipe <- recipe(target ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(target) # Handle class imbalance using SMOTE

# Step 5: Specify the Model

# Choose a classification model. For this example, 
# we'll use a Random Forest.

rf_model <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Step 6: Create a Workflow
# Combine the recipe and the model into a workflow.

heart_workflow <- workflow() %>%
  add_recipe(heart_recipe) %>%
  add_model(rf_model)

# Step 7: Train the Model
# Fit the model to the training data.

rf_fit <- heart_workflow %>%
  fit(data = train_data)

# Step 8: Evaluate the Model
# Make predictions on the test set and evaluate the performance.

# Make predictions
rf_predictions <- predict(rf_fit, new_data = test_data) %>%
  bind_cols(test_data)

# Evaluate performance
rf_metrics <- rf_predictions %>%
  metrics(truth = target, estimate = .pred_class)

rf_conf_mat <- rf_predictions %>%
  conf_mat(truth = target, estimate = .pred_class)

rf_metrics
rf_conf_mat

# Step 9: Visualize the Results
# Visualize the confusion matrix.

autoplot(rf_conf_mat, type = "heatmap") +
  labs(title = "Confusion Matrix for Random Forest Model")

# Step 10: Summary
# Summarize the findings and discuss potential improvements.

# Print metrics
rf_metrics

# Discuss:
# - Model accuracy
# - Precision, recall, and F1-score
# - Handling class imbalance
# - Potential feature engineering or model tuning

# Save and load the model to create Shiny App

# Assuming rf_fit is your trained random forest model
saveRDS(rf_fit, file = "ml_production_apps/saved_models/model_classification/class_chatgpt.RDS")
