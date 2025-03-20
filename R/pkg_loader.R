#' Package Initialization Function
#'
#' This function sets default options when the package is loaded.
#' It configures data.frame defaults and glm control parameters
#' to ensure consistent behavior across package functions.
#'
#' @param libname Character string giving the library directory where the package is installed
#' @param pkgname Character string giving the name of the package
#'
#' @details
#' - Sets `check.names = FALSE` as the default for data.frame creation using the default package
#' - Sets maximum iterations for glm functions to 100 using stats::glm.control
#'
#' @importFrom default default
#' @importFrom stats glm.control
#'
#' @note This function is automatically called when the package is loaded and should not be called directly.
#'
#' @export
.onLoad <- function(libname, pkgname) {
  default::default(data.frame) <- list(check.names = FALSE)
  stats::glm.control(maxit = 100)
  # No assignment of logLik.multinom here
}

#' Get Log-Likelihood for Multinomial Models
#'
#' This function safely accesses the unexported logLik.multinom function from the nnet package.
#'
#' @param model A model object typically of class 'multinom' or 'polr'
#' @return The numeric log-likelihood value
#' @importFrom utils getFromNamespace
#' @keywords internal
get_loglik_multinom <- function(model) {
  logLik.multinom <- getFromNamespace("logLik.multinom", "nnet")
  return(as.numeric(logLik.multinom(model)))
}


