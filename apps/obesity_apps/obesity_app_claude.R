# Obesity App with Claude

# Notes: https://www.tmwr.org/
# Source: https://archive.ics.uci.edu/dataset/544/estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition
# Prompt

# You are a machine learning scientist training students on the basics of machine 
# learning and prediction. Using the "Estimation of Obesity Levels Based On Eating 
# Habits and Physical Condition" dataset from the UCI machine learning repository, 
# create a classification/prediction model that can determine the obesity level 
# from various input features. In this project, use the tidymodels package from R 
# plus any other machine learning tools in R to generate the prediction model. 
# Ensure that the data is loaded properly, cleaned, and that the columns or 
# features are named appropriately. Use a variety of models, perform feature 
# engineering, and create ensemble models if needed. At the end, save the 
# appropriate model as an RDS file.

# Dataset

# Steps

# 1. Load and prepare the data
# 2. Explore and clean the data
# 3. Perform feature engineering
# 4. Split the data into training and testing sets
# 5. Create and compare multiple models
# 6. Create an ensemble model
# 7. Evaluate the final model
# 8. Save the model as an RDS file

# Load necessary libraries
library(tidymodels)
library(tidyverse)
library(readr)
library(vip)
library(themis)
library(stacks)

# Step 1: Load and prepare the data
obesity_data <- read_csv("obesity_apps/obesity.csv")

# Step 2: Explore and clean the data
glimpse(obesity_data)

# Rename columns to be more R-friendly
obesity_data <- obesity_data %>%
  rename(
    gender = Gender,
    age = Age,
    height = Height,
    weight = Weight,
    family_history = `family_history_with_overweight`,
    favc = FAVC,
    fcvc = FCVC,
    ncp = NCP,
    caec = CAEC,
    smoke = SMOKE,
    ch2o = CH2O,
    scc = SCC,
    faf = FAF,
    tue = TUE,
    calc = CALC,
    mtrans = MTRANS,
    obesity_level = NObeyesdad
  )

# Convert categorical variables to factors
obesity_data <- obesity_data %>%
  mutate(across(c(gender, family_history, favc, caec, smoke, scc, calc, mtrans, obesity_level), as.factor))

# Step 3: Perform feature engineering
obesity_data <- obesity_data %>%
  mutate(
    bmi = weight / (height)^2,
    age_group = cut(age, breaks = c(0, 18, 30, 45, 60, Inf), labels = c("Teen", "Young Adult", "Adult", "Middle Age", "Senior"))
  )

# Step 4: Split the data into training and testing sets
set.seed(123)
obesity_split <- initial_split(obesity_data, prop = 0.8, strata = obesity_level)
obesity_train <- training(obesity_split)
obesity_test <- testing(obesity_split)

# Create a recipe for preprocessing
obesity_recipe <- recipe(obesity_level ~ ., data = obesity_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_smote(obesity_level)

# Step 5: Create and compare multiple models

# Random Forest
rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_workflow <- workflow() %>%
  add_recipe(obesity_recipe) %>%
  add_model(rf_model)

# XGBoost
xgb_model <- boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
                        min_n = tune(), sample_size = tune(), trees = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_workflow <- workflow() %>%
  add_recipe(obesity_recipe) %>%
  add_model(xgb_model)

# Support Vector Machine
svm_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_workflow <- workflow() %>%
  add_recipe(obesity_recipe) %>%
  add_model(svm_model)

# Create a control object for tuning
ctrl <- control_grid(save_pred = TRUE, parallel_over = "everything", save_workflow = TRUE)

# Set up cross-validation
obesity_folds <- vfold_cv(obesity_train, v = 5, strata = obesity_level)

# Tune hyperparameters for each model
rf_tuned <- rf_workflow %>%
  tune_grid(
    resamples = obesity_folds,
    grid = 20,
    control = ctrl,
    metrics = metric_set(roc_auc, accuracy)
  )

xgb_tuned <- xgb_workflow %>%
  tune_grid(
    resamples = obesity_folds,
    grid = 20,
    control = ctrl,
    metrics = metric_set(roc_auc, accuracy)
  )

svm_tuned <- svm_workflow %>%
  tune_grid(
    resamples = obesity_folds,
    grid = 20,
    control = ctrl,
    metrics = metric_set(roc_auc, accuracy)
  )

# Compare model performance
model_comparison <- bind_rows(
  rf_tuned %>% collect_metrics() %>% mutate(model = "Random Forest"),
  xgb_tuned %>% collect_metrics() %>% mutate(model = "XGBoost"),
  svm_tuned %>% collect_metrics() %>% mutate(model = "SVM")
)

# Visualize model comparison
ggplot(model_comparison, aes(x = model, y = mean, fill = .metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~.metric, scales = "free_y") +
  labs(title = "Model Comparison", y = "Performance")

# Based on results - work with xgboost or RF
# Here, we choose xgboost

# Select best hyperparameters
best_xgb <- xgb_tuned %>%
  select_best(metric = "roc_auc")

# Finalize workflow with best hyperparameters
final_xgb <- xgb_workflow %>%
  finalize_workflow(best_xgb)

# Fit the final model
final_xgb_fit <- final_xgb %>%
  fit(data = obesity_train)

# Step 6: Evaluate the final model
xgb_preds <- final_xgb_fit %>%
  predict(obesity_test) %>%
  bind_cols(obesity_test)

# Calculate accuracy
accuracy <- xgb_preds %>%
  accuracy(truth = obesity_level, .pred_class)

# Create confusion matrix
conf_mat <- xgb_preds %>%
  conf_mat(truth = obesity_level, .pred_class)

# Print results
print(paste("XGBoost Model Accuracy:", accuracy$.estimate))
print(conf_mat)

# Feature importance
vip(final_xgb_fit, num_features = 20)

# Step 7: Save the model as an RDS file
saveRDS(final_xgb_fit, "obesity_apps/obesity_prediction_model.rds")