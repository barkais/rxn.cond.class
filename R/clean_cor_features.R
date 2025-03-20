#' Clean Correlated Features Based on Multiple Criteria
#'
#' This function cleans up highly correlated features in a dataset using a user-defined threshold and a selection criterion.
#' Users can choose to retain the feature with the higher correlation to the outcome, higher mutual information,
#' higher feature importance from a random forest, or higher variance.
#'
#' @param data A data frame containing the features and outcome.
#' @param outcome_col A character string specifying the name of the outcome column.
#' @param corr_threshold A numeric value specifying the threshold for identifying highly correlated features. Defaults to 0.9.
#' @param method A character string specifying the method to choose between correlated features. Options include:
#'   \itemize{
#'     \item \code{"correlation"}: Select based on higher absolute correlation with the outcome.
#'     \item \code{"mutual_information"}: Select based on higher mutual information with the outcome.
#'     \item \code{"feature_importance"}: Select based on higher feature importance from a random forest model.
#'     \item \code{"variance"}: Select based on higher variance in the feature values.
#'   }
#'   Defaults to \code{"correlation"}.
#' @param outcome_type A character string specifying the type of the outcome. Options are:
#'   \itemize{
#'     \item \code{"numeric"}: Use if the outcome is continuous.
#'     \item \code{"categorical"}: Use if the outcome is categorical.
#'     \item \code{"auto"}: Automatically detect the type of the outcome. Defaults to \code{"auto"}.
#'   }
#' @return A cleaned data frame where correlated features have been removed based on the chosen criterion.
#'
#' @details The function computes the correlation matrix for the input features and identifies pairs of features
#' that exceed the specified correlation threshold. For each pair of correlated features, the function selects
#' one feature to retain based on the selected \code{method}.
#'
#' - When using \code{"correlation"}, the function retains the feature with the higher absolute correlation to the outcome.
#' - When using \code{"mutual_information"}, mutual information is computed to select the feature that provides more information about the outcome.
#' - The \code{"feature_importance"} method fits a random forest model to calculate feature importance and selects the more important feature.
#' - The \code{"variance"} method retains the feature with the higher variance.
#'
#' The function works with both numeric and categorical outcome variables. It automatically adjusts calculations
#' based on the type of the outcome, which can be inferred by the function or specified by the user.
#'
#'
#' # Example 1: Using correlation to clean features (numeric outcome)
#' data(iris)
#' cleaned_data <- clean_correlated_features(data = iris, outcome_col = "Sepal.Length",
#'                                           corr_threshold = 0.8, method = "correlation")
#'
#' # Example 2: Using mutual information to clean features (categorical outcome)
#' iris$Species <- as.factor(iris$Species)
#' cleaned_data <- clean_correlated_features(data = iris, outcome_col = "Species",
#'                                           corr_threshold = 0.8, method = "mutual_information", outcome_type = "categorical")
#'
#' # Example 3: Using feature importance from a random forest (numeric outcome)
#' cleaned_data <- clean_correlated_features(data = iris, outcome_col = "Sepal.Length",
#'                                           corr_threshold = 0.8, method = "feature_importance")
#'
#' @importFrom randomForest randomForest importance
#' @importFrom infotheo mutinformation discretize
#' @importFrom stats cor var cor.test
#' @export
clean_correlated_features <- function(data, outcome_col = dim(data)[2], corr_threshold = 0.9, method = "correlation", outcome_type = "auto") {

  # Extract the outcome column
  outcome <- data[[outcome_col]]

  # Determine outcome type if not provided explicitly
  if (outcome_type == "auto") {
    outcome_type <- if (is.factor(outcome) || is.character(outcome)) "categorical" else "numeric"
  }

  # Extract features, excluding the outcome column
  features <- data[, -outcome_col]

  # Compute correlation matrix for features
  corr_matrix <- cor(features)

  # Identify pairs of highly correlated features
  high_corr_pairs <- which(abs(corr_matrix) > corr_threshold, arr.ind = TRUE)
  high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] != high_corr_pairs[, 2], ]

  # Keep track of features to drop
  drop_features <- c()

  # Iterate over each correlated pair
  for (i in 1:nrow(high_corr_pairs)) {
    feat1 <- colnames(corr_matrix)[high_corr_pairs[i, 1]]
    feat2 <- colnames(corr_matrix)[high_corr_pairs[i, 2]]

    # Skip if either feature is already marked for dropping
    if (feat1 %in% drop_features || feat2 %in% drop_features) next

    if (method == "correlation") {
      # Calculate correlation with the outcome (numeric or categorical)
      if (outcome_type == "numeric") {
        corr_feat1 <- abs(cor(features[, feat1], outcome))
        corr_feat2 <- abs(cor(features[, feat2], outcome))
      } else {
        # For categorical outcome, use point-biserial correlation or a similar method
        corr_feat1 <- abs(cor.test(as.numeric(outcome), features[, feat1])$estimate)
        corr_feat2 <- abs(cor.test(as.numeric(outcome), features[, feat2])$estimate)
      }

    } else if (method == "mutual_information") {
      # Calculate mutual information with the outcome
      if (outcome_type == "numeric") {
        mi_feat1 <- mutinformation(discretize(features[[feat1]]), discretize(outcome))
        mi_feat2 <- mutinformation(discretize(features[[feat2]]), discretize(outcome))
      } else {
        # Categorical outcome - direct mutual information
        mi_feat1 <- mutinformation(discretize(features[[feat1]]), as.factor(outcome))
        mi_feat2 <- mutinformation(discretize(features[[feat2]]), as.factor(outcome))
      }

      corr_feat1 <- mi_feat1
      corr_feat2 <- mi_feat2

    } else if (method == "feature_importance") {
      # Calculate feature importance using Random Forest
      if (outcome_type == "numeric") {
        model <- randomForest(x = features, y = outcome)
      } else {
        model <- randomForest(x = features, y = as.factor(outcome))
      }

      importance <- importance(model)
      corr_feat1 <- importance[row.names(importance) == feat1, ]
      corr_feat2 <- importance[row.names(importance) == feat2, ]

    } else if (method == "variance") {
      # Compare variance
      corr_feat1 <- var(features[[feat1]])
      corr_feat2 <- var(features[[feat2]])
    }

    # Drop the feature with the lower score
    if (corr_feat1 < corr_feat2) {
      drop_features <- c(drop_features, feat1)
    } else {
      drop_features <- c(drop_features, feat2)
    }
  }

  # Return the cleaned dataset without the dropped features
  return(data[, setdiff(names(data), drop_features), drop = FALSE])
}
