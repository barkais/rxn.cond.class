# rxn.cond.class

An R package for classification modeling, probability analysis, and data visualization with a focus on chemical reaction condition classification.

## Overview

`rxn.cond.class` provides comprehensive tools for developing, evaluating, and visualizing classification models. The package specializes in handling class imbalance, feature selection, and creating detailed visualizations of model performance and prediction probabilities.

## Features

- **Classification Modeling**:
  - Build and evaluate ordinal and multinomial logistic regression models
- **Feature Selection & Processing**:
  - Remove highly correlated features using multiple criteria (correlation, mutual information, feature importance, variance)
- **Visual Model Assessment**:
  - Generate detailed confusion matrix heatmaps with performance metrics
  - Create probability heatmaps to visualize prediction confidence
  - Build validation results tables for model comparison
- **Stratified Sampling & Cross-Validation**:
  - Sample observations based on similarity to class centroids
  - Perform k-fold cross-validation with stratification options
  - Iterative cross-validation for robust performance estimates
- **Model Comparison Tools**: 
  - Generate all-subsets models with a user defined range of feature combinations
  - Evaluate using various metrics including McFadden's R², J-statistic
  - Retrain best models with customizable parameters

### Installation from Github 

First, install the `remotes` package from CRAN.
The `repos = getCRANmirrors()[1,"URL"]` addition helps installation in linux interactive sessions.

```r
install.packages('remotes', repos = getCRANmirrors()[1,"URL"])
```

Once `remotes` is properly installed, use the `install_github` function to install `rxn.con.class`.
For convenience, load the package.

```r
# Install
remotes::install_github('https://github.com/barkais/rxn.cond.class.git')

# Load
library('rxn.con.class')
```

## Key Functions

### Model Building & Evaluation

- `sub_model_log()`: Generate and evaluate models with different feature combinations using McFadden's R²
- `sub_model_log_Jstat()`: Generate models evaluated using the J-statistic (Youden's Index) for imbalanced data
- `mod.info()`: Calculate model performance metrics (accuracy, McFadden's R²)
- `retrain_best_model()`: Retrain a selected model with specified parameters

### Visualization Functions

- `ct_plot()`: Create confusion matrix heatmaps with accuracy, precision, and recall metrics
- `prob.heatmap()`: Generate probability heatmaps showing prediction confidence
- `patchwork_prob_heatmap()`: Create paginated probability heatmaps for large datasets
- `validation_table()`: Create visual tables of validation results
- `combine_plots_with_legend()`: Combine multiple plots with shared legends

### Data Processing

- `clean_correlated_features()`: Remove highly correlated features based on various criteria
- `simi.sampler()`: Sample observations based on similarity to class centroids
- `stratified()`: Create stratified samples with custom size parameters

### Cross-Validation

- `k.fold.log.cv()`: Perform k-fold cross-validation for classification models
- `k.fold.log.iter()`: Perform iterative k-fold cross-validation with summary statistics

## Example Usage

```r
library(rxn.cond.class)

# Clean correlated features
data_cleaned <- clean_correlated_features(my_data, 
                                         corr_threshold = 0.85, 
                                         method = "mutual_information")

# Find the best model formulas
top_models <- sub_model_log(data_cleaned, max = 5)
best_formula <- top_models$formula[1]

# Train a model
model <- nnet::multinom(best_formula, data = data_cleaned, maxit = 2000)

# Evaluate the model and create visualizations
model_results <- mod.info(model, data_cleaned)
print(paste("Accuracy:", model_results$accuracy, "%"))
print(paste("McFadden's R²:", model_results$McFadden_R2))

# Create a confusion matrix visualization
confusion_plot <- ct_plot(model_results$class.table, 
                         "Model Evaluation Results", 
                         "Training Dataset")
print(confusion_plot$plot)

# Generate a probability heatmap
prob_map <- prob.heatmap(model, data_cleaned, 
                         "Prediction Probabilities", 
                         "Model: Logistic Regression")

# Create similarity-based stratified sampling for cross-validation
class1_samples <- simi.sampler(data_cleaned, 1, sample.size = 20)
class2_samples <- simi.sampler(data_cleaned, 2, sample.size = 20)
training_indices <- c(class1_samples, class2_samples)
training_set <- data_cleaned[training_indices, ]
testing_set <- data_cleaned[-training_indices, ]

# K-fold cross-validation
cv_results <- k.fold.log.cv(best_formula, data_cleaned, ordinal = FALSE, folds = 5)
print(paste("Cross-validation accuracy:", cv_results[[1]], "%"))
```

## Advanced Functionality

### Similarity-Based Sampling
The `simi.sampler()` function calculates the similarity of each observation to its class's centroid vector using cosine similarity. This enables representative sampling from imbalanced datasets by selecting points that span the feature space of each class.

### Model Comparison and Selection
The package provides multiple approaches for model evaluation:
- `sub_model_log()`: Ranks models by McFadden's R²
- `sub_model_log_Jstat()`: Ranks models by J-statistic (Sensitivity + Specificity - 1)

### Visualization Tools
The package includes specialized visualization tools:
- `ct_plot()`: Creates heatmaps with correct/incorrect classifications, class sizes, and precision metrics
- `prob.heatmap()`: Shows probability distributions for each prediction with color coding for correct/incorrect classifications
- `patchwork_prob_heatmap()`: Handles large datasets with paginated visualizations

## Citation

If you use this package in your research, please cite:

```
Barkai, S. (2025). rxn.cond.class: Classify Chemical Reaction Conditions.
R package version 0.1.0. https://github.com/barkais/rxn.cond.class
```

## License

This package is licensed under the MIT License.
