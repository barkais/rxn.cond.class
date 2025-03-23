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

### Model Search

This section demonstrates how to clean data, perform similarity-based sampling, rank models, and split data into training and testing sets.

```r
# Load data
data <- rxn.cond.class::example_training_data

# Clean and organize data
row.names(data) <- data[,2]
data$class <- as.factor(data$class)
data <- data[,-c(1:2)] # Remove name and tag

# Perform similarity-based sampling
one <- simi.sampler(data, 1) # Class 1 with itself
two <- simi.sampler(data, 2) # Class 2 with itself
three <- simi.sampler(data, 3) # Class 3 with itself
one_three <- simi.sampler(data, 1, 3) # Class 1 with Class 3
two_three <- simi.sampler(data, 2, 3) # Class 2 with Class 3

# Combine similarities
similarties <- c(union(one, one_three), union(two, two_three), three)

# Rank ordinal models
models.ordinal <- sub_model_log(data = data[similarties, ],
                                min = 2,
                                max = 2, 
                                ordinal = TRUE)

# Rank non-ordinal models
models.non.ordinal <- sub_model_log(data = data[similarties, ],
                                    min = 2,
                                    max = 2, 
                                    ordinal = FALSE)

# Define training and test sets
Train.set <- data[similarties, ]
Test.set <- data[-similarties, ]

# Load and organize external validation data
External.set <- rxn.cond.class::example_validation_data
RN <- External.set$V1
External.set <- External.set[,-1]
External.set$class <- as.factor(External.set$class)
row.names(External.set) <- RN

# Load and organize prediction of new substrates data
Prediction.set <- rxn.cond.class::example_prediction_data
RN <- Prediction.set$V1
Prediction.set <- Prediction.set[,-1]
row.names(Prediction.set) <- RN
```

### Ordinal Model Example

#### Model Ranking

```r
# Present the ranked list of ordinal models
knitr::kable(models.ordinal)
```

#### Training Set

```r
# Use the first ranked ordinal model
test.form <- models.ordinal[1, 1]

# Define starting coefficients
num.of.vars <- stringi::stri_count(test.form, fixed = '+')
start <- c(rep(0, num.of.vars + 2), 1)

# Train model
test <- fit_polr(formula = test.form, data = Train.set)

# Cross-validation (smallest-group's-fold)
k.fold.log.iter(formula = test.form, 
                data = Train.set, 
                ordinal = TRUE, 
                stratify = TRUE, 
                iterations = 20, 
                verbose = TRUE)

# Leave-one-out cross-validation
k.fold.log.iter(formula = test.form, 
                data = Train.set, 
                ordinal = TRUE, 
                folds = nrow(Train.set), 
                stratify = FALSE, 
                iterations = 1, 
                verbose = TRUE)
```

#### Model Information and Visualization (Training Set)

```r
# Display model information and confusion matrix plot
model.info <- mod.info(test, Train.set, TRUE, TRUE)

# Classification table plot
confusion_matrix <- ct_plot(model.info$class.table, 
                            plot.title = 'Training Set', 
                            conformation = '1. 1st Place')

confusion_matrix$plot

# Prediction probability heatmap
prob.heatmap(test, Train.set, 
             plot.title = 'Training Set', 
             conformation = '1. 1st Place')
```

#### Test Set

```r
# Evaluate the model on the test set
model.info <- mod.info(test, Test.set, FALSE, FALSE)

# Classification table plot
confusion_matrix <- ct_plot(model.info$class.table, 
                            plot.title = 'Test Set', 
                            conformation = '1. 1st Place')

confusion_matrix$plot

# Prediction probability heatmap
prob.heatmap(test, Test.set, 
             plot.title = 'Test Set', 
             conformation = '1. 1st Place')
```

#### External Validation

```r
# Evaluate the model on the external validation set
model.info <- mod.info(test, External.set, FALSE)

# Classification table plot
confusion_matrix <- ct_plot(model.info$class.table, 
                            plot.title = 'External Validation', 
                            conformation = '1. 1st Place')

confusion_matrix$plot

# Prediction probability heatmap
prob.heatmap(test, External.set, 
             plot.title = 'External Validation', 
             conformation = '1. 1st Place')
```

#### Prediction of New Substartes

```r
knitr::kable(cbind(predict(test, Prediction.set, 'probs') * 100,
      predicted_class = predict(test, Prediction.set, 'class')))
```
### Non-ordinal Model Example

#### Model Ranking

```r
# Present the ranked list of non-ordinal models
knitr::kable(models.non.ordinal)
```

#### Training Set

```r
# Use the first ranked non-ordinal model
test.form <- models.non.ordinal[1, 1]

# Train the non-ordinal multinomial regression model
test <- nnet::multinom(test.form,
                       data = Train.set,
                       maxit = 2000, 
                       trace = FALSE)

# Cross-validation (smallest-group's-fold)
k.fold.log.iter(formula = test.form, 
                data = Train.set, 
                ordinal = FALSE, 
                stratify = TRUE, 
                iterations = 20, 
                verbose = TRUE)

# Leave-one-out cross-validation
k.fold.log.iter(formula = test.form, 
                data = Train.set, 
                ordinal = FALSE, 
                folds = nrow(Train.set), 
                stratify = FALSE, 
                iterations = 1, 
                verbose = TRUE)
```

#### Model Information and Visualization (Training Set)

```r
# Display model information and confusion matrix plot
model.info <- mod.info(test, Train.set, TRUE, TRUE)

# Classification table plot
confusion_matrix <- ct_plot(model.info$class.table, 
                            plot.title = 'Training Set', 
                            conformation = '1. 1st Place')

confusion_matrix$plot

# Prediction probability heatmap
prob.heatmap(test, Train.set, 
             plot.title = 'Training Set', 
             conformation = '1. 1st Place')
```

#### Test Set

```r
# Evaluate the model on the test set
model.info <- mod.info(test, Test.set, FALSE, FALSE)

# Classification table plot
confusion_matrix <- ct_plot(model.info$class.table, 
                            plot.title = 'Test Set', 
                            conformation = '1. 1st Place')

confusion_matrix$plot

# Prediction probability heatmap
prob.heatmap(test, Test.set, 
             plot.title = 'Test Set', 
             conformation = '1. 1st Place')
```

#### External Validation

```r
# Evaluate the model on the external validation set
model.info <- mod.info(test, External.set, FALSE)

# Classification table plot
confusion_matrix <- ct_plot(model.info$class.table, 
                            plot.title = 'External Validation', 
                            conformation = '1. 1st Place')

confusion_matrix$plot

# Prediction probability heatmap
prob.heatmap(test, External.set, 
             plot.title = 'External Validation', 
             conformation = '1. 1st Place')
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

## License

This package is licensed under the MIT License.
