# ML Regression Exercise with Claude

# Prompt

# You are a machine learning scientist training undergraduates on 
# the basics of machine learning. Select a dataset from the UC Irvine 
# ML repo to do regression. Use a tidymodel approach to create a 
# tutorial for solving a common machine learning problem. 
# Don't use iris dataset

# Dataset

# Dataset: Auto MPG Dataset
# Problem Type: Regression
# Source: UC Irvine ML Repository

# 1. Load required libraries

library(tidymodels)
library(readr)
library(skimr)

# 2. Load and prepare the data

# Load the Auto MPG dataset
auto_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
auto_data <- read_table(auto_url, col_names = FALSE)

# Add column names
colnames(auto_data) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                         "acceleration", "model_year", "origin", "car_name")

# Convert horsepower to numeric and handle missing values
auto_data$horsepower <- as.numeric(auto_data$horsepower)
auto_data <- auto_data %>% 
  filter(!is.na(horsepower)) %>%
  select(-car_name)  # Remove car_name as it's not useful for prediction

# Display summary statistics
skim(auto_data)

# 3. Split the data into training and testing sets

set.seed(123)
auto_split <- initial_split(auto_data, prop = 0.75, strata = mpg)
auto_train <- training(auto_split)
auto_test <- testing(auto_split)

# 4. Create a recipe for preprocessing

auto_recipe <- recipe(mpg ~ ., data = auto_train) %>%
  step_normalize(all_predictors()) %>%
  step_dummy(all_nominal_predictors())

# 5. Define the model

lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# 6. Create a workflow

auto_workflow <- workflow() %>%
  add_recipe(auto_recipe) %>%
  add_model(lasso_model)

# 7. Set up cross-validation

auto_folds <- vfold_cv(auto_train, v = 5)

# 8. Tune hyperparameters

lambda_grid <- grid_regular(penalty(), levels = 50)

lasso_tuning <- tune_grid(
  auto_workflow,
  resamples = auto_folds,
  grid = lambda_grid
)

# 9. Select the best model

best_lasso <- select_best(lasso_tuning, metric = "rmse")

# 10. Finalize the workflow and fit the model

final_workflow <- finalize_workflow(auto_workflow, best_lasso)
final_model <- fit(final_workflow, data = auto_train)

# 11. Evaluate the model on the test set

test_predictions <- predict(final_model, auto_test)
test_results <- auto_test %>%
  select(mpg) %>%
  bind_cols(test_predictions)

rmse <- rmse(test_results, truth = mpg, estimate = .pred)
rsq <- rsq(test_results, truth = mpg, estimate = .pred)

print(rmse)
print(rsq)

# 12. Visualize predictions vs actual values

ggplot(test_results, aes(x = mpg, y = .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  labs(x = "Actual MPG", y = "Predicted MPG", title = "Actual vs Predicted MPG")

# Save the models

# Assuming you have already trained your model and it's called 'final_model'
library(tidymodels)

# Save the model
saveRDS(final_model, "auto_mpg_model.rds")

# To load the model later:
# loaded_model <- readRDS("auto_mpg_model.rds")