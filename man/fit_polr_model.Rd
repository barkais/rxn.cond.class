% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_polr.R
\name{fit_polr_model}
\alias{fit_polr_model}
\title{Fit a Proportional Odds Logistic Regression Model}
\usage{
fit_polr_model(formula, data, start_vector)
}
\arguments{
\item{formula}{A formula representing the model structure. This should be in the standard R formula notation.}

\item{data}{A data frame containing the variables in the model.}

\item{start_vector}{A numeric vector of starting values for the model coefficients.}
}
\description{
`fit_polr_model` fits a proportional odds logistic regression model using the `MASS::polr` function.
The function attempts to fit the model with a provided formula, data, and starting coefficient values,
and includes error handling to ensure robustness.
}
\details{
The function internally calls `MASS::polr` to fit the model. It includes error handling using `tryCatch`
to capture and return any errors that occur during model fitting.
}
