#' Define global variables
#'
#' This file is used to declare global variables to avoid R CMD check notes
#' about no visible binding for global variables.
#'
#' @importFrom utils globalVariables
#' @noRd

utils::globalVariables(c(
  # Variable names used in plotting and data manipulation
  "Acc.print", "Exp", "Freq", "Legend", "Name", "Pred",
  "class.table", "correct", "desc", "flag", "label", "prop",
  "test", "validation.result", "value", "variable", "x", "y",

  # Variables created by '<<-' assignments - explicitly listed
  "Low.Recall", "class.table", "Acc.print", "validation.result"
))
