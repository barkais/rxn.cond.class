#' Submodel Logistic Regression (Ordinal/Multinomial)
#'
#' This function performs logistic regression by generating and fitting various combinations of predictor
#' variables. It can handle both ordinal logistic regression (via `MASS::polr`) and multinomial logistic
#' regression (via `nnet::multinom`). The models are evaluated using McFadden's R-squared value to rank
#' model performance.
#'
#' @param data A dataframe containing the data.
#' @param out.col The column number of the outcome variable. Defaults to the "class" column.
#' @param min The minimum number of predictor variables to include in combinations. Defaults to 1.
#' @param max The maximum number of predictor variables to include in combinations. Defaults to one-fifth of
#'            the number of rows in the dataset.
#' @param ordinal A boolean indicating whether to fit an ordinal logistic regression (`TRUE`) or a multinomial
#'                logistic regression (`FALSE`). Defaults to `TRUE`.
#'
#' @return A dataframe containing the top 10 models sorted by McFadden's R-squared value. The output includes:
#'         - `formula`: The model formula.
#'         - `McFadden R2`: McFadden's R-squared value, indicating model fit.
#'
#' @details
#' - This function generates combinations of predictor variables and fits logistic regression models for
#'   each combination.
#' - McFadden's R-squared value is used to evaluate model fit, with higher values indicating better fit.
#' - For ordinal regression, the `MASS::polr` function is used, while multinomial regression uses
#'   `nnet::multinom`.
#' - The function handles special characters in variable names by wrapping them in backticks.
#' @export
sub_model_log <- function(data, out.col = which(colnames(data) == 'class'),
                          min = 1, max = floor(dim(data)[1] / 5),
                          ordinal = T) {

  # Prepare the outcome variable name
  output <- stringr::str_c('`', names(data[out.col]), '`')

  # Get predictor variable names, excluding 'flag' (if present)
  vars <- names(data[, -out.col])
  vars <- vars[vars != 'flag']

  # Wrap predictor variable names with backticks to handle special characters
  for (i in 1:length(vars)) {
    vars[i] <- stringr::str_c('`', vars[i], '`')
  }

  # Initialize lists to store combinations of variables and R-squared values
  comb.list <- list()
  R2.list <- list()

  # Generate combinations of predictors from 'min' to 'max' variables
  for (i in min:max) {
    comb.list[[i]] <- data.frame(aperm(combn(vars, i)), stringsAsFactors = FALSE)

    # Concatenate variable names to create model formulas
    comb.list[[i]][, dim(comb.list[[i]])[2] + 1] <- do.call(paste,
                                                            c(comb.list[[i]][names(comb.list[[i]])], sep = " + "))

    # Name the new column as 'formula'
    names(comb.list[[i]])[dim(comb.list[[i]])[2]] <- 'formula'

    # Remove individual variable columns, keeping only the formula
    for (co in names(comb.list[[i]])[1:length(names(comb.list[[i]])) - 1]) {
      comb.list[[i]][co] <- NULL
    }
  }

  # Remove empty combinations (if any) and combine them into a single data frame
  comb.list <- plyr::compact(comb.list)
  forms <- do.call(rbind, comb.list)
  names(forms) <- 'formula'

  # Define a helper function to fit an ordinal logistic regression model
  fit_polr_model <- function(formula, data, start_vector) {
    tryCatch({
      # Try fitting the model using MASS::polr
      model <- MASS::polr(formula, data = data, Hess = TRUE, start = start_vector, control = list(maxit = 100))
      return(list(success = TRUE, model = model))
    }, error = function(e) {
      return(list(success = FALSE, error = e))
    })
  }

  # Fit models for each formula
  if (ordinal == T) {
    for (i in 1:dim(forms)[1]) {

      # Construct the full formula with the outcome variable
      forms$formula[i] <- stringr::str_c(output, ' ~ ', forms$formula[i])

      # Count the number of variables in the formula (for initializing start vector)
      num.of.vars <- stringi::stri_count(forms$formula[i], fixed = '+')

      # Initialize start vector for model fitting (coefficients)
      start <- c(rep(0, num.of.vars + 2), 1)

      # Flag to control the loop in case the initial model fitting fails
      success <- FALSE

      # Keep trying to fit the model until it succeeds
      while (!success) {
        result <- fit_polr_model(forms$formula[i], data, start)

        if (result$success) {
          success <- TRUE
          test.1 <- result$model
        } else {
          # If model fitting fails, extend the start vector and retry
          start <- c(0, start)
        }
      }

      # Fit a null model (class ~ 1) for comparison
      test.0 <- MASS::polr(class ~ 1, data = data, Hess = TRUE)

      # Calculate log-likelihoods for the null and fitted models
      loglik.0 <- as.numeric(get_loglik_multinom(test.0))
      loglik.1 <- as.numeric(get_loglik_multinom(test.1))

      # Calculate McFadden's R-squared value and store it
      R2.list[i] <- round((1 - (loglik.1 / loglik.0)), digits = 3)
    }
  } else {
    for (i in 1:dim(forms)[1]) {
      # Construct the full formula with the outcome variable
      forms$formula[i] <- stringr::str_c(output,' ~ ',forms$formula[i])

      test.1 <- nnet::multinom(forms$formula[i],
                               data = data,
                               maxit = 2000, trace = F)
      test.0 <- nnet::multinom(class ~ 1, data = data, maxit = 2000, trace = F)
      loglik.0 <- as.numeric(get_loglik_multinom(test.0))
      loglik.1 <- as.numeric(get_loglik_multinom(test.1))
      R2.list[i] <- round((1 - (loglik.1/loglik.0)),digits = 3)
    }
  }


  # Add McFadden's R2 values to the forms data frame
  forms[, 2] <- do.call(rbind, R2.list)
  names(forms)[2] <- 'McFadden R2'

  # Return the top 10 models sorted by McFadden's R2
  out.models <- head(dplyr::arrange(forms, desc(forms[, 2])), 10)
  return(out.models)
}

#' J Statistic-based Submodel Logistic Regression
#'
#' This function is a variant of sub_model_log that ranks models using the minimum J statistic
#' (Youden's Index) across all classes instead of McFadden's R². This makes it particularly
#' suitable for imbalanced classification problems where performance on minority classes is important.
#'
#' The key difference from sub_model_log is the evaluation metric:
#' - sub_model_log uses McFadden's R² to assess overall model fit
#' - sub_model_log_Jstat uses minimum J statistic to focus on class-specific performance
#'
#' The J statistic (Sensitivity + Specificity - 1) is calculated for each class, and the minimum
#' value is used for ranking to ensure good performance even on the most challenging class.
#'
#' @param data A dataframe containing the data.
#' @param out.col The column number of the outcome variable. Defaults to the "class" column.
#' @param min The minimum number of predictor variables to include in combinations. Defaults to 1.
#' @param max The maximum number of predictor variables to include in combinations. Defaults to one-fifth of
#'            the number of rows in the dataset.
#' @param ordinal A boolean indicating whether to fit an ordinal logistic regression (`TRUE`) or a multinomial
#'                logistic regression (`FALSE`). Defaults to `TRUE`.
#'
#' @return A dataframe containing the top 10 models sorted by minimum J statistic. The output includes:
#'         - `formula`: The model formula.
#'         - `Min J Stat`: The minimum J statistic across all classes (3 decimal places).
#'         - `McFadden R2`: McFadden's R-squared value.
#'         - `Smallest Class`: The name of the smallest class in the dataset.
#'
#' @export
sub_model_log_Jstat <- function(data, out.col = which(colnames(data) == 'class'),
                                min = 1, max = floor(dim(data)[1] / 5),
                                ordinal = T) {

  # Prepare the outcome variable name
  output <- stringr::str_c('`', names(data[out.col]), '`')

  # Get predictor variable names, excluding 'flag' (if present)
  vars <- names(data[, -out.col])
  vars <- vars[vars != 'flag']

  # Wrap predictor variable names with backticks
  for (i in 1:length(vars)) {
    vars[i] <- stringr::str_c('`', vars[i], '`')
  }

  # Initialize lists for combinations, J statistics, and McFadden R²
  comb.list <- list()
  J.list <- list()
  R2.list <- list()

  # Find the smallest class
  class_counts <- table(data[[out.col]])
  smallest_class <- names(class_counts)[which.min(class_counts)]

  # Calculate J statistic for a binary classification case
  calculate_j_stat <- function(conf_matrix) {
    # For binary classification, calculate sensitivity and specificity
    # Class 1 metrics
    TP1 <- conf_matrix[1,1]
    FN1 <- conf_matrix[1,2]
    FP1 <- conf_matrix[2,1]
    TN1 <- conf_matrix[2,2]

    # Class 2 metrics
    TP2 <- conf_matrix[2,2]
    FN2 <- conf_matrix[2,1]
    FP2 <- conf_matrix[1,2]
    TN2 <- conf_matrix[1,1]

    # Calculate metrics for both classes
    sensitivity1 <- TP1 / (TP1 + FN1 + .Machine$double.eps)
    specificity1 <- TN1 / (TN1 + FP1 + .Machine$double.eps)
    j_stat1 <- round(sensitivity1 + specificity1 - 1, digits = 3)

    sensitivity2 <- TP2 / (TP2 + FN2 + .Machine$double.eps)
    specificity2 <- TN2 / (TN2 + FP2 + .Machine$double.eps)
    j_stat2 <- round(sensitivity2 + specificity2 - 1, digits = 3)

    # Return the minimum J statistic between the two classes
    min_j <- min(j_stat1, j_stat2)
    return(min_j)
  }


  # Generate combinations of predictors
  for (i in min:max) {
    comb.list[[i]] <- data.frame(aperm(combn(vars, i)), stringsAsFactors = FALSE)
    comb.list[[i]][, dim(comb.list[[i]])[2] + 1] <- do.call(paste,
                                                            c(comb.list[[i]][names(comb.list[[i]])], sep = " + "))
    names(comb.list[[i]])[dim(comb.list[[i]])[2]] <- 'formula'

    for (co in names(comb.list[[i]])[1:length(names(comb.list[[i]])) - 1]) {
      comb.list[[i]][co] <- NULL
    }
  }

  # Combine all formulas
  comb.list <- plyr::compact(comb.list)
  forms <- do.call(rbind, comb.list)
  names(forms) <- 'formula'

  # Function to fit ordinal model
  fit_polr_model <- function(formula, data, start_vector) {
    tryCatch({
      model <- MASS::polr(formula, data = data, Hess = TRUE, start = start_vector,
                          control = list(maxit = 100))
      return(list(success = TRUE, model = model))
    }, error = function(e) {
      return(list(success = FALSE, error = e))
    })
  }

  # Fit models and calculate J statistics
  for (i in 1:dim(forms)[1]) {
    forms$formula[i] <- stringr::str_c(output, ' ~ ', forms$formula[i])


    if (ordinal) {
      # Fit ordinal model
      num.of.vars <- stringi::stri_count(forms$formula[i], fixed = '+')
      start <- c(rep(0, num.of.vars + 2), 1)
      success <- FALSE

      while (!success) {
        result <- fit_polr_model(forms$formula[i], data, start)
        if (result$success) {
          success <- TRUE
          model <- result$model
        } else {
          start <- c(0, start)
        }
      }

      # Get predictions
      pred <- predict(model, newdata = data, type = "class")

    } else {
      # Fit multinomial model
      model <- nnet::multinom(forms$formula[i], data = data, maxit = 2000, trace = F)
      pred <- predict(model, newdata = data, type = "class")
    }

    # Calculate confusion matrix and J statistic
    conf_matrix <- table(data[[out.col]], pred)
    J.list[[i]] <- calculate_j_stat(conf_matrix)

    # Calculate McFadden's R²
    if (ordinal) {
      test.0 <- MASS::polr(as.formula(paste(output, "~ 1")), data = data, Hess = TRUE)
      loglik.0 <- as.numeric(get_loglik_multinom(test.0))
      loglik.1 <- as.numeric(get_loglik_multinom(model))
    } else {
      test.0 <- nnet::multinom(as.formula(paste(output, "~ 1")), data = data, maxit = 2000, trace = F)
      loglik.0 <- as.numeric(get_loglik_multinom(test.0))
      loglik.1 <- as.numeric(get_loglik_multinom(model))
    }
    R2.list[i] <- round(1 - (loglik.1/loglik.0), digits = 3)
  }

  # Add J statistics and McFadden R² to the forms data frame
  forms[, 2] <- do.call(rbind, J.list)
  forms[, 3] <- do.call(rbind, R2.list)
  names(forms)[2:3] <- c('Min J Stat', 'McFadden R2')
  forms$SmallestClass <- smallest_class

  # Return top 10 models sorted by minimum J statistic
  out.models <- head(dplyr::arrange(forms, desc(forms[, 2])), 10)
  return(out.models)
}
