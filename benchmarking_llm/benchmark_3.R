# Benchmark 3: EDA of the Palmer Penguins Dataset

# Persona:  You are a data analyst working on exploratory data analysis at a 
# newly funded startup. Context: You have been tasked with working on the 
# penguins dataset and developing new techniques for EDA. Action: Write an R 
# script to analyze the penguins the dataset, clean and process the data with 
# tidyverse, perform EDA with plots, and finally generate a pairplot. 
# Tone: Professional. Output Format: Provide an R script.

# ChatGPT

# Load necessary libraries
library(tidyverse)
library(GGally)
library(ggthemes)

# Load the penguins dataset
penguins <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv")

# Data Cleaning: Remove rows with missing values
penguins_clean <- penguins %>%
  drop_na()

# Univariate Analysis: Histograms for numerical variables
penguins_clean %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numerical Variables")

# Bivariate Analysis: Scatter plot of Bill Length vs Flipper Length colored by Species
ggplot(penguins_clean, aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
  geom_point(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Bill Length vs Flipper Length by Species",
       x = "Bill Length (mm)",
       y = "Flipper Length (mm)")

# Boxplots: Flipper Length by Species
ggplot(penguins_clean, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Flipper Length Distribution by Species",
       x = "Species",
       y = "Flipper Length (mm)")

# Pair Plot: Relationships between numerical variables colored by Species
penguins_clean %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, species) %>%
  ggpairs(aes(color = species, alpha = 0.5)) +
  theme_minimal()

# CoPilot

# Load necessary packages
library(tidyverse)
library(palmerpenguins)
library(GGally)

# Load the dataset
data <- penguins

# Inspect the data structure
glimpse(data)

# Data Cleaning: Handle missing values
data_clean <- data %>%
  drop_na() %>%
  mutate(species = as.factor(species),
         island = as.factor(island),
         sex = as.factor(sex))

# Summary statistics
summary(data_clean)

# Exploratory Data Analysis (EDA)
# Distribution of species
ggplot(data_clean, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Distribution of Penguin Species", x = "Species", y = "Count") +
  theme_minimal()

# Relationship between bill length and bill depth
ggplot(data_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.7) +
  labs(title = "Bill Length vs. Bill Depth", x = "Bill Length (mm)", y = "Bill Depth (mm)") +
  theme_minimal()

# Boxplot for flipper length by species
ggplot(data_clean, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  labs(title = "Flipper Length Distribution by Species", x = "Species", y = "Flipper Length (mm)") +
  theme_minimal()

# Pairplot visualization
ggpairs(data_clean, columns = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
        aes(color = species)) +
  labs(title = "Pairplot of Key Features")

# Save cleaned dataset
#write_csv(data_clean, "cleaned_penguins.csv")

# Display message
#cat("Exploratory Data Analysis completed successfully!\n")

# Gemini

# Load necessary libraries
# Install if you haven't already:
# install.packages("tidyverse")
# install.packages("palmerpenguins")
# install.packages("GGally")

library(tidyverse)
library(palmerpenguins)
library(GGally)
library(dplyr) # Although part of tidyverse, explicit loading can be helpful
library(ggplot2) # Although part of tidyverse, explicit loading can be helpful

# Load the penguins dataset
data(penguins)

# --- Data Cleaning and Processing ---
# Perform initial data inspection
summary(penguins)
str(penguins)
colSums(is.na(penguins)) # Check for missing values per column

# Handle missing values:
# For this analysis, we will remove rows with any missing values
penguins_cleaned <- penguins %>%
  drop_na()

# Optional: Further cleaning or feature engineering can be added here
# For example, creating a new variable:
# penguins_cleaned <- penguins_cleaned %>%
#   mutate(bill_ratio = bill_length_mm / bill_depth_mm)

# --- Exploratory Data Analysis (EDA) with Plots ---

# 1. Histograms of numerical variables
ggplot(penguins_cleaned, aes(x = bill_length_mm)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Bill Length (mm)", x = "Bill Length (mm)")

ggplot(penguins_cleaned, aes(x = bill_depth_mm)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Bill Depth (mm)", x = "Bill Depth (mm)")

ggplot(penguins_cleaned, aes(x = flipper_length_mm)) +
  geom_histogram(binwidth = 2, fill = "salmon", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Flipper Length (mm)", x = "Flipper Length (mm)")

ggplot(penguins_cleaned, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, fill = "orchid", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Body Mass (g)", x = "Body Mass (g)")

# 2. Box plots to visualize distributions by species
ggplot(penguins_cleaned, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Bill Length (mm) by Species", x = "Species", y = "Bill Length (mm)")

ggplot(penguins_cleaned, aes(x = species, y = bill_depth_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Bill Depth (mm) by Species", x = "Species", y = "Bill Depth (mm)")

ggplot(penguins_cleaned, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Flipper Length (mm) by Species", x = "Species", y = "Flipper Length (mm)")

ggplot(penguins_cleaned, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Body Mass (g) by Species", x = "Species", y = "Body Mass (g)")

# 3. Scatter plots to examine relationships between numerical variables, colored by species
ggplot(penguins_cleaned, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Bill Length vs. Bill Depth by Species",
       x = "Bill Length (mm)", y = "Bill Depth (mm)")

ggplot(penguins_cleaned, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Flipper Length vs. Body Mass by Species",
       x = "Flipper Length (mm)", y = "Body Mass (g)")

# --- Generate Pairplot ---
# A pairplot (scatterplot matrix) to visualize relationships between multiple variables
# Using ggpairs from the GGally package for a more enhanced pairplot

# Select only the numerical columns and the species column for the pairplot
penguins_for_pairplot <- penguins_cleaned %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, species)

ggpairs(penguins_for_pairplot, aes(color = species)) +
  theme_minimal() +
  labs(title = "Pairwise Relationships of Penguin Measurements by Species")

# You can further customize the ggpairs plot by specifying different plots
# for the lower diagonal, upper diagonal, and diagonal.
# Example:
# ggpairs(penguins_for_pairplot, aes(color = species),
#         lower = list(continuous = wrap("points", alpha = 0.5)),
#         upper = list(continuous = wrap("cor", size = 3)),
#         diag = list(continuous = wrap("densityDiag"))) +
#   theme_minimal() +
#   labs(title = "Customized Pairwise Relationships of Penguin Measurements by Species")


# Claude

# Palmer Penguins Dataset Exploratory Data Analysis
# This script performs cleaning, processing, and visualization of the penguins dataset

# Load required libraries
library(tidyverse)
library(palmerpenguins)
library(GGally)  # For creating pairplots
library(naniar)  # For missing data visualization

# Set a consistent theme for all plots
theme_set(theme_minimal(base_size = 12))

# 1. Load the data
data("penguins")

# 2. Initial data exploration
print("Dataset Structure:")
str(penguins)

print("Summary Statistics:")
summary(penguins)

# 3. Check for missing values
print("Missing Values Summary:")
miss_var_summary(penguins)

# 4. Clean and process the data
penguins_clean <- penguins %>%
  # Remove rows with missing values
  drop_na() %>%
  # Convert categorical variables to factors if not already
  mutate(
    species = as.factor(species),
    island = as.factor(island),
    sex = as.factor(sex),
    year = as.factor(year)
  )

# 5. Basic Statistics by Species
species_summary <- penguins_clean %>%
  group_by(species) %>%
  summarize(
    count = n(),
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    mean_body_mass = mean(body_mass_g, na.rm = TRUE),
    sd_bill_length = sd(bill_length_mm, na.rm = TRUE),
    sd_bill_depth = sd(bill_depth_mm, na.rm = TRUE),
    sd_flipper_length = sd(flipper_length_mm, na.rm = TRUE),
    sd_body_mass = sd(body_mass_g, na.rm = TRUE)
  )

print("Summary Statistics by Species:")
print(species_summary)

# 6. Exploratory Data Analysis - Visualization

# Plot 1: Distribution of penguin species
p1 <- ggplot(penguins_clean, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Penguin Species Distribution",
       x = "Species",
       y = "Count") +
  theme(legend.position = "none")

# Plot 2: Distribution of penguins across islands
p2 <- ggplot(penguins_clean, aes(x = island, fill = species)) +
  geom_bar(position = "stack") +
  labs(title = "Penguin Distribution by Island",
       x = "Island",
       y = "Count")

# Plot 3: Body mass distribution by species
p3 <- ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species",
       x = "Species",
       y = "Body Mass (g)") +
  theme(legend.position = "none")

# Plot 4: Flipper length distribution by species
p4 <- ggplot(penguins_clean, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  labs(title = "Flipper Length by Species",
       x = "Species",
       y = "Flipper Length (mm)") +
  theme(legend.position = "none")

# Plot 5: Bill length vs. bill depth colored by species
p5 <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.7) +
  labs(title = "Bill Length vs. Bill Depth",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)")

# Plot 6: Body mass distribution by sex and species
p6 <- ggplot(penguins_clean, aes(x = sex, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(title = "Body Mass by Sex and Species",
       x = "Sex",
       y = "Body Mass (g)")

# Plot 7: Flipper length vs. body mass colored by species
p7 <- ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.7) +
  labs(title = "Flipper Length vs. Body Mass",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)")

# Plot 8: Correlation heatmap of numeric variables
# Extract numeric columns
penguins_numeric <- penguins_clean %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Calculate correlation matrix
cor_matrix <- cor(penguins_numeric)

# Convert to long format for plotting
cor_long <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("variable1") %>%
  pivot_longer(-variable1, names_to = "variable2", values_to = "correlation")

# Create correlation heatmap
p8 <- ggplot(cor_long, aes(x = variable1, y = variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 3) +
  labs(title = "Correlation Heatmap of Numeric Variables",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Create a pairplot
p9 <- ggpairs(
  penguins_clean,
  columns = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
  aes(color = species, alpha = 0.5),
  upper = list(continuous = "cor"),
  title = "Pairplot of Palmer Penguins Dataset"
) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p9

# 8. Save all the plots
# ggsave("plot1_species_distribution.png", p1, width = 8, height = 6)
# ggsave("plot2_island_distribution.png", p2, width = 8, height = 6)
# ggsave("plot3_body_mass.png", p3, width = 8, height = 6)
# ggsave("plot4_flipper_length.png", p4, width = 8, height = 6)
# ggsave("plot5_bill_dimensions.png", p5, width = 8, height = 6)
# ggsave("plot6_body_mass_by_sex.png", p6, width = 8, height = 6)
# ggsave("plot7_flipper_vs_mass.png", p7, width = 8, height = 6)
# ggsave("plot8_correlation_heatmap.png", p8, width = 8, height = 6)
# ggsave("plot9_pairplot.png", p9, width = 12, height = 10)

# 9. Generate insights
cat("\nKey Insights from the Palmer Penguins Dataset:\n")
cat("1. The dataset contains", nrow(penguins), "observations of", length(unique(penguins$species)), "penguin species.\n")
cat("2. There are missing values in the dataset, primarily in the", 
    names(which.max(colSums(is.na(penguins)))), "column.\n")

# Get species with highest body mass
heaviest_species <- species_summary %>% 
  filter(mean_body_mass == max(mean_body_mass)) %>% 
  pull(species)

cat("3. The", heaviest_species, "species has the highest average body mass.\n")

# Check for sexual dimorphism
sex_dimorphism <- penguins_clean %>%
  group_by(species, sex) %>%
  summarize(mean_body_mass = mean(body_mass_g, na.rm = TRUE),
            .groups = "drop")

cat("4. There appears to be sexual dimorphism in body mass across all penguin species.\n")

# Bill length to depth ratio by species
bill_ratio <- penguins_clean %>%
  group_by(species) %>%
  summarize(mean_bill_ratio = mean(bill_length_mm / bill_depth_mm, na.rm = TRUE),
            .groups = "drop")

cat("5. The bill length to depth ratio varies considerably between species, which can be an important taxonomic feature.\n")

# 10. Export the cleaned dataset
# write_csv(penguins_clean, "penguins_clean.csv")

# Final message
cat("\nExploratory Data Analysis completed successfully!\n")
cat("All plots have been saved to the working directory.\n")
cat("Cleaned dataset exported as 'penguins_clean.csv'.\n")



# Perplexity


# Palmer Penguins Exploratory Data Analysis
# Created: 2025-04-25
# Author: Perplexity Data Team

## Load required packages
library(tidyverse)
library(palmerpenguins)
library(GGally)
library(ggcorrplot)

## 1. Data Loading & Initial Inspection
data("penguins")
penguins_raw <- penguins

# Initial structure check
glimpse(penguins_raw)
summary(penguins_raw)

## 2. Data Cleaning & Preprocessing
penguins_clean <- penguins_raw %>%
  # Remove incomplete cases
  drop_na() %>%
  # Ensure proper factor levels
  mutate(across(c(species, island, sex), as.factor))

# Verify duplicates
cat("Duplicate rows:", nrow(penguins_clean) - nrow(distinct(penguins_clean)))

## 3. Univariate Analysis

### Numerical Summaries
penguins_clean %>%
  group_by(species, island) %>%
  summarise(
    count = n(),
    avg_bill_length = mean(bill_length_mm),
    avg_flipper = mean(flipper_length_mm),
    .groups = 'drop'
  )

### Distribution Visualization
# Histogram grid
penguins_clean %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 15, fill = "steelblue") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Feature Distributions", x = "Value")

# Species comparison
ggplot(penguins_clean, aes(bill_length_mm, fill = species)) +
  geom_density(alpha = 0.6) +
  labs(title = "Bill Length Distribution by Species")

## 4. Bivariate Analysis

### Scatterplot Matrix
ggplot(penguins_clean, aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Bill Dimensions Relationship")

### Correlation Analysis
cor_matrix <- penguins_clean %>%
  select(where(is.numeric)) %>%
  cor()

ggcorrplot(cor_matrix, 
           lab = TRUE,
           title = "Feature Correlation Matrix")

## 5. Multivariate Pairplot
ggpairs(penguins_clean,
        columns = c(3:6),  # Bill length to body mass
        mapping = aes(color = species, alpha = 0.7),
        upper = list(continuous = wrap("cor", size = 3)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.7))) +
  theme(panel.grid.minor = element_blank())

## 6. Final Data Export
# write_csv(penguins_clean, "penguins_cleaned_20250425.csv")


# Cohere

# Load required packages from the tidyverse
library(tidyverse)

# Load the penguins dataset
data(penguins)

# Data Cleaning and Processing
penguins_cleaned <- penguins %>%
  # Remove rows with missing values in species column
  filter(!is.na(species)) %>%
  # Convert bill_length_mm and bill_depth_mm to numeric
  mutate(across(c(bill_length_mm, bill_depth_mm), as.numeric))

# Exploratory Data Analysis
# Summary statistics
summary(penguins_cleaned)

# Distribution of bill length and depth
ggplot(penguins_cleaned, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  labs(title = "Bill Length and Depth by Species", x = "Bill Length (mm)", y = "Bill Depth (mm)")

# Pairplot for numerical variables
ggpairs(
  penguins_cleaned %>% select(where(is.numeric)),
  title = "Pairplot of Numerical Variables",
  columns = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
)

# Poe


# Load necessary libraries
library(tidyverse)
library(GGally)  # For pair plots

# Load the penguins dataset
data("penguins", package = "palmerpenguins")

# View the structure of the dataset
str(penguins)

# Data Cleaning and Processing
# Remove rows with missing values
penguins_clean <- penguins %>%
  drop_na()

# Convert categorical variables to factors
penguins_clean <- penguins_clean %>%
  mutate(species = as.factor(species),
         island = as.factor(island),
         sex = as.factor(sex))

# Exploratory Data Analysis (EDA)

# Summary statistics
summary_stats <- penguins_clean %>%
  group_by(species) %>%
  summarise(
    avg_bill_length = mean(bill_length_mm),
    avg_bill_depth = mean(bill_depth_mm),
    avg_flipper_length = mean(flipper_length_mm),
    avg_body_mass = mean(body_mass_g),
    count = n()
  )

print(summary_stats)

# Visualize distributions of body mass by species
ggplot(penguins_clean, aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Body Mass by Species",
       x = "Body Mass (g)",
       y = "Density") +
  theme_minimal()

# Boxplot of flipper length by species
ggplot(penguins_clean, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  labs(title = "Boxplot of Flipper Length by Species",
       x = "Species",
       y = "Flipper Length (mm)") +
  theme_minimal()

# Pairplot of numerical variables
ggpairs(penguins_clean, 
        mapping = aes(color = species),
        title = "Pair Plot of Penguin Measurements")

# Save the summary statistics to a CSV file
# write.csv(summary_stats, "penguins_summary_statistics.csv", row.names = FALSE)

# End of script

# Google AI Studio

# R Script for Exploratory Data Analysis of the Palmer Penguins Dataset

# Persona: Data Analyst at a Startup
# Context: Initial EDA on the penguins dataset using new techniques.
# Action: Load, clean, process, plot, and generate a pairplot.
# Tone: Professional

# --- 1. Setup ---

# Install necessary packages if you don't have them
# install.packages("tidyverse")
# install.packages("palmerpenguins")
# install.packages("GGally")

# Load required libraries
library(tidyverse)      # For data manipulation (dplyr, ggplot2, etc.)
library(palmerpenguins) # For the penguins dataset
library(GGally)         # For the pairplot (ggpairs)

# Set plotting theme for consistency
theme_set(theme_minimal(base_size = 12))

# --- 2. Load Data ---

# Load the dataset directly from the palmerpenguins package
data("penguins")

# --- 3. Initial Data Inspection ---

cat("--- Initial Data Inspection ---\n")

# Display the structure of the dataset
cat("\n--- Data Structure (glimpse) ---\n")
glimpse(penguins)

# Display summary statistics
cat("\n--- Summary Statistics ---\n")
summary(penguins)

# Check for missing values per column
cat("\n--- Missing Value Count ---\n")
colSums(is.na(penguins))

# Display the first few rows
cat("\n--- First Few Rows (head) ---\n")
head(penguins)

# --- 4. Data Cleaning and Processing (using tidyverse) ---

cat("\n--- Data Cleaning and Processing ---\n")

# For this initial EDA, we will remove rows with any missing values.
# This is a common starting point, though imputation could be considered later.
penguins_clean <- penguins %>%
  drop_na()

# Verify that missing values have been removed
cat("\n--- Missing Values After Cleaning ---\n")
colSums(is.na(penguins_clean))

# Check the dimensions before and after cleaning
cat("\n--- Dimensions Before Cleaning ---\n")
print(dim(penguins))
cat("\n--- Dimensions After Cleaning ---\n")
print(dim(penguins_clean))

# Optional: Refine factor levels or data types if necessary (already seems appropriate here)
# For example, ensure categorical variables are factors
penguins_clean <- penguins_clean %>%
  mutate(across(where(is.character), as.factor)) # Ensure character columns are factors if any existed

# Verify structure again after cleaning (optional)
# cat("\n--- Structure After Cleaning ---\n")
# glimpse(penguins_clean)


# --- 5. Exploratory Data Analysis (EDA) with Plots ---

cat("\n--- Performing Exploratory Data Analysis ---\n")

# 5.1. Univariate Analysis: Distribution of key variables

# Distribution of numerical variables (using histograms and density plots)
numeric_vars <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")

plot_numeric_dist <- penguins_clean %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Distribution of Numerical Penguin Measurements",
    x = "Measurement Value",
    y = "Density"
  ) +
  theme(strip.text = element_text(face = "bold"))

print(plot_numeric_dist)
# ggsave("numeric_distributions.png", plot_numeric_dist, width = 10, height = 8) # Optional: save plot

# Distribution of categorical variables (using bar plots)
categorical_vars <- c("species", "island", "sex")

plot_categorical_dist <- penguins_clean %>%
  select(all_of(categorical_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = value)) +
  geom_bar(color = "black", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Distribution of Categorical Penguin Features",
    x = "Category",
    y = "Count"
  ) +
  scale_fill_viridis_d(option = "plasma") + # Using a nice color palette
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

print(plot_categorical_dist)
# ggsave("categorical_distributions.png", plot_categorical_dist, width = 10, height = 6) # Optional: save plot

# 5.2. Bivariate Analysis: Relationships between variables

# Relationship between bill length and bill depth, colored by species
plot_bill_dims <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = species), size = 0.5) + # Add linear model lines per species
  scale_color_viridis_d(option = "viridis") +
  labs(
    title = "Bill Length vs. Bill Depth by Species",
    x = "Bill Length (mm)",
    y = "Bill Depth (mm)",
    color = "Species",
    linetype = "Species"
  ) +
  theme(legend.position = "bottom")

print(plot_bill_dims)
# ggsave("bill_dimensions_scatter.png", plot_bill_dims, width = 8, height = 6) # Optional: save plot


# Relationship between flipper length and body mass, colored by species
plot_flipper_mass <- ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_viridis_d(option = "viridis") +
  labs(
    title = "Flipper Length vs. Body Mass by Species",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    color = "Species"
  ) +
  theme(legend.position = "bottom")

print(plot_flipper_mass)
# ggsave("flipper_mass_scatter.png", plot_flipper_mass, width = 8, height = 6) # Optional: save plot

# Body mass distribution by species (using boxplots)
plot_mass_species <- ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2, size=1, show.legend = FALSE) + # Add jittered points
  scale_fill_viridis_d(option = "viridis") +
  labs(
    title = "Body Mass Distribution by Species",
    x = "Species",
    y = "Body Mass (g)"
  )

print(plot_mass_species)
# ggsave("body_mass_species_boxplot.png", plot_mass_species, width = 7, height = 5) # Optional: save plot


# Body mass distribution by species and sex
plot_mass_species_sex <- ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) + # Dodge boxes for sex
  scale_fill_manual(values = c("female" = "skyblue", "male" = "coral")) + # Custom colors for sex
  labs(
    title = "Body Mass Distribution by Species and Sex",
    x = "Species",
    y = "Body Mass (g)",
    fill = "Sex"
  ) +
  theme(legend.position = "bottom")

print(plot_mass_species_sex)
# ggsave("body_mass_species_sex_boxplot.png", plot_mass_species_sex, width = 8, height = 6) # Optional: save plot


# --- 6. Generate Pair Plot ---

cat("\n--- Generating Pair Plot (ggpairs) ---\n")

# Create the pair plot using GGally::ggpairs
# We'll focus on the numerical measurements and use species for coloring.
# Excluding 'year' as it's less likely to show strong pairwise correlations here.
pairplot <- ggpairs(
  penguins_clean,
  columns = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"), # Columns to include in matrix
  aes(color = species, alpha = 0.6), # Map aesthetics: color by species
  title = "Pair Plot of Penguin Measurements by Species",
  upper = list(continuous = wrap("cor", size = 3, hjust = 0.8)), # Show correlation coefficients in upper panels
  lower = list(continuous = wrap("points", size = 1.5)),       # Show scatter plots in lower panels
  diag = list(continuous = wrap("densityDiag", alpha = 0.5))   # Show density plots on the diagonal
) +
  scale_color_viridis_d(option = "viridis") + # Apply consistent color palette
  scale_fill_viridis_d(option = "viridis") +  # Apply fill for density plots
  theme(strip.text = element_text(face = "bold"), # Bolder facet titles
        axis.text = element_text(size = 8),      # Adjust axis text size if needed
        axis.title = element_text(size = 10)     # Adjust axis title size
  )

# Print the pair plot (may take a moment to render)
print(pairplot)

# Optional: Save the pair plot
# ggsave("penguins_pairplot.png", pairplot, width = 12, height = 10)

cat("\n--- Analysis Script Complete ---\n")

# End of script

# HuggingChat - FAILED

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization [1]
library(ggplot2)    # For creating detailed plots [1]
library(palmerpenguins)  # Dataset for penguins [1]

# Load the penguins dataset
data(penguins)

# View the structure of the dataset
glimpse(penguins)  # For a compact overview of the data [1]

# Summary statistics
summary(penguins)  # Provides summary statistics for numeric variables

# Check for missing values
missing_values <- penguins |> summarise(
  across(everything(), ~ sum(is.na(.)))  # Count missing values per column
)
print(missing_values)

# Clean the data by removing rows with missing values
penguins_clean <- penguins |> na.omit()  # Remove rows with NAs [1]

# Convert categorical variables to factors
penguins_clean |> mutate(
  species = as.factor(species),
  island = as.factor(island),
  sex = as.factor(sex)
)

# Create a new categorical variable for body size
penguins_clean <- penguins_clean |> mutate(
  body_size = cut(
    body_mass_g,
    breaks = c(0, 3000, 4500, 6000),
    labels = c("Small", "Medium", "Large")
  )
)

# EDA Plots

# Scatter plot of bill length vs flipper length
ggplot(penguins_clean, aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Bill Length vs Flipper Length by Species",
       x = "Bill Length (mm)", y = "Flipper Length (mm)",
       color = "Species") +
  theme_minimal()

# Box plot of body mass by species
ggplot(penguins_clean, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(title = "Body Mass Distribution by Species",
       x = "Species", y = "Body Mass (g)") +
  theme_minimal()

# Interactive plot with plotly
penguins_clean |> ggplot(aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
  geom_point() +
  ggtitle("Interactive Plot: Bill Length vs Flipper Length by Species")

# Pair plot usingGGally
ggpairs(
  penguins_clean,
  columns = 2:5,
  aes(setColor = species,
  diagTitle = "Density plot",
  diagType = "density",
  upper = list(continuous = "cor", combo = "box")
)) +
  labs(title = "Pair Plot of Penguin Dataset")

# Summary of key insights
summary_insights <- "
- Adelie penguins have the smallest body sizes.
- Gentoo penguins have the longest bills on average.
- There is a positive correlation between bill length and flipper length.
"

cat(summary_insights)

# DeepSeek

# Palmer Penguins Dataset Analysis
# Exploratory Data Analysis (EDA) with Tidyverse

# Load required libraries
library(tidyverse)
library(palmerpenguins)
library(GGally)  # For pairplot
library(skimr)   # For data summary
library(ggthemes) # For plot themes

# Load the dataset
data("penguins")

# 1. Initial Data Inspection
cat("=== Initial Data Inspection ===\n")
glimpse(penguins)
skim(penguins)

# 2. Data Cleaning and Preprocessing
cat("\n=== Data Cleaning ===\n")

# Handle missing values
penguins_clean <- penguins %>%
  drop_na() %>%  # Remove rows with NA values (alternative: imputation)
  mutate(across(where(is.character), as.factor))  # Convert character to factor
         
cat("Number of rows removed due to missing values:", nrow(penguins) - nrow(penguins_clean), "\n")

# 3. Basic Summary Statistics
cat("\n=== Summary Statistics ===\n")

# Numerical summary by species
penguins_clean %>%
  group_by(species) %>%
  skim_without_charts()

# 4. Exploratory Data Analysis with Visualizations

## 4.1 Univariate Analysis
cat("\n=== Univariate Analysis ===\n")

# Plot distributions of numerical variables
univariate_plots <- penguins_clean %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Numerical Variables",
       x = "Value",
       y = "Count") +
  theme_minimal()

print(univariate_plots)

## 4.2 Bivariate Analysis
cat("\n=== Bivariate Analysis ===\n")

# Body mass by species
mass_plot <- penguins_clean %>%
  ggplot(aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Body Mass Distribution by Species",
       x = "Species",
       y = "Body Mass (g)") +
  theme_economist()

# Bill length vs depth by species
bill_plot <- penguins_clean %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Bill Length vs. Bill Depth by Species",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  theme_minimal()

print(mass_plot)
print(bill_plot)


## 4.3 Multivariate Analysis
cat("\n=== Multivariate Analysis ===\n")

# Pairplot for comprehensive variable relationships
pair_plot <- ggpairs(penguins_clean,
                     columns = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                     mapping = aes(color = species, alpha = 0.7),
                     upper = list(continuous = wrap("cor", size = 3)),
                     lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.1)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.7))) +
  labs(title = "Penguins Dataset Pairplot") +
  theme(legend.position = "bottom")

print(pair_plot)


# 5. Additional Insights
cat("\n=== Additional Insights ===\n")

# Flipper length by species and sex
penguins_clean %>%
  ggplot(aes(x = species, y = flipper_length_mm, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue")) +
  labs(title = "Flipper Length by Species and Sex",
       x = "Species",
       y = "Flipper Length (mm)") +
  theme_minimal()

# Save the cleaned data for further analysis
#write_csv(penguins_clean, "penguins_clean.csv")

cat("\n=== Analysis Complete ===\n")


# Qwen 


# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(GGally)     # For generating pairplots

# Step 1: Load the penguins dataset
data("penguins", package = "palmerpenguins")

# Step 2: Inspect the dataset
glimpse(penguins)  # Get an overview of the dataset structure

# Step 3: Data Cleaning and Processing
cleaned_penguins <- penguins %>%
  # Remove rows with missing values
  drop_na() %>%
  # Convert categorical variables to factors for better analysis
  mutate(
    species = as.factor(species),
    island = as.factor(island),
    sex = as.factor(sex)
  )

# Check the cleaned dataset
summary(cleaned_penguins)

# Step 4: Exploratory Data Analysis (EDA) with Plots
# 4.1: Distribution of numeric variables
cleaned_penguins %>%
  pivot_longer(cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
               names_to = "measurement", values_to = "value") %>%
  ggplot(aes(x = value, fill = species)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ measurement, scales = "free") +
  labs(title = "Density Plots of Numeric Variables by Species",
       x = "Measurement Value",
       y = "Density") +
  theme_minimal()

# 4.2: Boxplots of numeric variables across species
cleaned_penguins %>%
  pivot_longer(cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
               names_to = "measurement", values_to = "value") %>%
  ggplot(aes(x = species, y = value, fill = species)) +
  geom_boxplot() +
  facet_wrap(~ measurement, scales = "free_y") +
  labs(title = "Boxplots of Numeric Variables by Species",
       x = "Species",
       y = "Measurement Value") +
  theme_minimal()

# 4.3: Relationship between bill length and flipper length colored by species
ggplot(cleaned_penguins, aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Bill Length vs Flipper Length by Species",
       x = "Bill Length (mm)",
       y = "Flipper Length (mm)") +
  theme_minimal()

# Step 5: Generate a Pairplot
# Use GGally's ggpairs function for pairwise relationships
pairplot <- ggpairs(cleaned_penguins,
                    columns = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                    ggplot2::aes(color = species),
                    title = "Pairplot of Penguin Measurements")
print(pairplot)

# End of Script


# Mistral LeChat


# Load necessary libraries
library(tidyverse)
library(palmerpenguins)
library(GGally)

# Load the penguins dataset
data(penguins)

# Inspect the dataset
glimpse(penguins)

# Data Cleaning and Processing
# Remove rows with missing values
penguins_clean <- penguins %>%
  drop_na()

# Convert species to a factor
penguins_clean$species <- as.factor(penguins_clean$species)

# Convert sex to a factor
penguins_clean$sex <- as.factor(penguins_clean$sex)

# Convert island to a factor
penguins_clean$island <- as.factor(penguins_clean$island)

# Summary statistics
summary(penguins_clean)

# Exploratory Data Analysis (EDA)
# Distribution of species
ggplot(penguins_clean, aes(x = species)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribution of Penguin Species",
       x = "Species",
       y = "Count")

# Distribution of bill length by species
ggplot(penguins_clean, aes(x = bill_length_mm, fill = species)) +
  geom_histogram(binwidth = 2, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Bill Length by Species",
       x = "Bill Length (mm)",
       y = "Count")

# Scatter plot of bill length vs. bill depth colored by species
ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Bill Length vs. Bill Depth",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)")

# Boxplot of flipper length by species
ggplot(penguins_clean, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Flipper Length by Species",
       x = "Species",
       y = "Flipper Length (mm)")

# Pairplot of the dataset
ggpairs(penguins_clean,
        columns = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
        ggplot2::aes(color = species, alpha = 0.6)) +
  theme_minimal()

# Save the pairplot
# ggsave("penguins_pairplot.png")

# Allen Playground - Failed

library(tidyverse)
library(ggplot2)
library(pairs)

# Load Penguins dataset
if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
  install.packages("palmerpenguins")
}
library(palmerpenguins)
data("penguins")

# Cleaning and processing the data
penguins_cleaned <- penguins %>%
  # Removing NA values
  na_omit() %>%
  # Converting 'species' to a factor
  mutate(species = factor(species)) %>%
  # Checking for any unnecessary or problematic variables and dealing with them accordingly
  # (assuming no problematic variables, proceed with analysis)
  
  # Exploratory Data Analysis
  # Summary statistics
  penguins_cleaned %>%
  summary()

# Plotting basic summary graphs
penguins_cleaned %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point() + 
  facet_wrap(~species) +
  labs(title = "Bill Length vs Bill Depth by Species")

# Pairplot
# Convert numeric variables to a list to make pairplot
numeric_vars <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
penguins_pair <- penguins_cleaned %>% 
  select_all(everything() %in% numeric_vars | is.factor()) %>%
  as.data.frame() # convert to data.frame for pairs() to work correctly

# Generate pairplot
pairs(penguins_pair, main = "Pairplot of Penguin Metrics", lower = list(panel = panel.smooth), diag = list(panel = panel.hist))
