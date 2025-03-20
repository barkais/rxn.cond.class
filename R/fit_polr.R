#' Fit a Proportional Odds Logistic Regression Model
#'
#' @description
#' `fit_polr_model` fits a proportional odds logistic regression model using the `MASS::polr` function.
#' The function attempts to fit the model with a provided formula, data, and starting coefficient values,
#' and includes error handling to ensure robustness.
#'
#' @param formula A formula representing the model structure. This should be in the standard R formula notation.
#' @param data A data frame containing the variables in the model.
#' @param start_vector A numeric vector of starting values for the model coefficients.
#'
#'
#' @details
#' The function internally calls `MASS::polr` to fit the model. It includes error handling using `tryCatch`
#' to capture and return any errors that occur during model fitting.
#'
#'
#' @export
fit_polr_model <- function(formula, data, start_vector) {
  tryCatch({
    model <- MASS::polr(formula, data = data, Hess = TRUE, start = start_vector, control = list(maxit = 100))
    return(list(success = TRUE, model = model))
  }, error = function(e) {
    return(list(success = FALSE, error = e))
  })
}

#' Fit a Proportional Odds Logistic Regression (POLR) Model
#'
#' This function iteratively fits a proportional odds logistic regression (POLR) model using the
#' provided formula and dataset. If the initial model fails to converge, the function adjusts the
#' starting coefficient vector and retries fitting the model until it succeeds.
#'
#' @param formula A formula specifying the model structure (e.g., `class ~ var1 + var2 + ...`).
#' @param data A data frame containing the variables in the formula and the response variable.
#' @return A fitted `polr` model object.
#'
#' @details
#' This function calls \code{fit_polr_model()} to fit a \code{MASS::polr()} model. If the model
#' fails to converge, it expands the starting coefficient vector and retries fitting the model
#' in a while loop until successful convergence is achieved.
#'
#' @export
fit_polr <- function(formula, data) {
  num.of.vars <- stringi::stri_count(formula, fixed = '+')
  start <- c(rep(0, num.of.vars + 2), 1)

  # Flag to control the loop
  success <- FALSE

  # While loop to keep trying until the model fits successfully
  while (!success) {
    result <- fit_polr_model(formula, data, start)

    if (result$success) {
      success <- TRUE
      test <- result$model
    } else {
      # Modify the start vector by adding another 0
      start <- c(0, start)
    }
  }
  return(test)
}
