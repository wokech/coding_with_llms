# ML Classification Exercise with Claude

# Prompt

# You are a machine learning scientist training undergraduates 
# on the basics of machine learning. Select a dataset from the 
# UC Irvine ML repo to do classification. Use a tidymodel approach 
# to create a tutorial for solving a common machine learning problem. 
# Don't use iris dataset.

# Dataset

# Dataset: Wine Dataset
# Problem Type: Multi-class Classification
# Source: UC Irvine ML Repository

# 1. Load required libraries

library(tidymodels)
library(readr)
library(skimr)

# 2. Load and prepare the data

# Load the wine dataset
wine_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine_data <- read_csv(wine_url, col_names = FALSE)

# Add column names
colnames(wine_data) <- c("class", "alcohol", "malic_acid", "ash", "alcalinity_of_ash", 
                         "magnesium", "total_phenols", "flavanoids", "nonflavanoid_phenols", 
                         "proanthocyanins", "color_intensity", "hue", 
                         "od280_od315_of_diluted_wines", "proline")

# Convert class to factor
wine_data$class <- as.factor(wine_data$class)

# Display summary statistics
skim(wine_data)

# 3. Split the data into training and testing sets

set.seed(123)
wine_split <- initial_split(wine_data, prop = 0.75, strata = class)
wine_train <- training(wine_split)
wine_test <- testing(wine_split)

# 4. Create a recipe for preprocessing

wine_recipe <- recipe(class ~ ., data = wine_train) %>%
  step_normalize(all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.9)

# 5. Define the model

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# 6. Create a workflow

wine_workflow <- workflow() %>%
  add_recipe(wine_recipe) %>%
  add_model(rf_model)

# 7. Set up cross-validation

wine_folds <- vfold_cv(wine_train, v = 5, strata = class)

# 8. Tune hyperparameters

rf_grid <- grid_regular(
  mtry(range = c(2, 8)),
  trees(range = c(100, 500)),
  min_n(range = c(2, 10)),
  levels = 3
)

rf_tuning <- tune_grid(
  wine_workflow,
  resamples = wine_folds,
  grid = rf_grid
)

# 9. Select the best model

best_rf <- select_best(rf_tuning, metric = "accuracy")

# 10. Finalize the workflow and fit the model

final_workflow <- finalize_workflow(wine_workflow, best_rf)
final_model <- fit(final_workflow, data = wine_train)

# 11. Evaluate the model on the test set

test_predictions <- predict(final_model, wine_test)
test_results <- wine_test %>%
  select(class) %>%
  bind_cols(test_predictions)

accuracy <- accuracy(test_results, truth = class, estimate = .pred_class)
conf_mat <- conf_mat(test_results, truth = class, estimate = .pred_class)

print(accuracy)
print(conf_mat)

# Save the model

# Assuming you have already trained your model and it's called 'final_model'
library(tidymodels)

# Save the model
saveRDS(final_model, "wine_classification_model.rds")

# To load the model later:
# loaded_model <- readRDS("wine_classification_model.rds")