# ML Classification Exercise with Perplexity

# Prompt

# You are a machine learning scientist training undergraduates 
# on the basics of machine learning. Select a dataset from the 
# UC Irvine ML repo to do classification. Use a tidymodel approach 
# to create a tutorial for solving a common machine learning problem. 
# Don't use iris dataset.

# Dataset

# We will use the Adult Income dataset. This dataset is often used 
# to predict whether an individual's income exceeds $50,000 per year 
# based on census data.

# The Adult dataset consists of 32,561 instances and 15 attributes, 
# including both categorical and continuous variables. The goal is 
# to classify individuals based on their income levels. The 
# attributes include features such as age, work class, education, 
# marital status, occupation, relationship, race, sex, hours worked 
# per week, and country of origin.

# Step 1: Load Required Libraries

library(tidyverse)
library(tidymodels)

# Step 2: Load the Dataset

# Load the dataset
adult_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                       header = FALSE, na.strings = " ?", 
                       col.names = c("age", "workclass", "fnlwgt", "education", 
                                     "education.num", "marital.status", "occupation", 
                                     "relationship", "race", "sex", 
                                     "capital.gain", "capital.loss", "hours.per.week", 
                                     "native.country", "income"))

# Step 3: Data Preprocessing

# Remove rows with missing values
adult_data <- adult_data %>% drop_na()

# Convert categorical variables to factors
adult_data <- adult_data %>%
  mutate(across(c(workclass, education, marital.status, occupation, 
                  relationship, race, sex, native.country), as.factor),
         income = as.factor(income))

# Step 4: Split the Data

set.seed(123)  # For reproducibility
data_split <- initial_split(adult_data, prop = 0.75, strata = income)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 5: Create a Recipe

recipe <- recipe(income ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Step 6: Specify the Model

model <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

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
  metrics(truth = income, estimate = .pred_class)

print(metrics)

# Step 10: Make Predictions

new_data <- tibble(
  age = 40,
  workclass = "Private",
  fnlwgt = 123456,
  education = "Bachelors",
  education.num = 13,
  marital.status = "Married-civ-spouse",
  occupation = "Tech-support",
  relationship = "Husband",
  race = "White",
  sex = "Male",
  capital.gain = 0,
  capital.loss = 0,
  hours.per.week = 40,
  native.country = "United-States"
)

new_prediction <- predict(fit, new_data)
print(new_prediction)

# Save models

# Save the classification model
saveRDS(fit, "ml_production_apps/saved_models/model_classification/class_perplexity.rds")
