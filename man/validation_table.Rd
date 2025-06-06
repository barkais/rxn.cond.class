% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_analysis_tools.R
\name{validation_table}
\alias{validation_table}
\title{Create a Validation Results Table}
\usage{
validation_table(
  validation_df,
  plot.title = "Validation Results",
  subtitle = "Predicted vs Experimental Classes",
  conformation = "",
  cell_height = 0.5
)
}
\arguments{
\item{validation_df}{Data frame containing validation results with columns for compound names,
probabilities, predicted classes, and experimental classes}

\item{plot.title}{String containing the title of the plot (default: "Validation Results")}

\item{subtitle}{String containing the subtitle (default: "Predicted vs Experimental Classes")}

\item{conformation}{String containing any additional information}

\item{cell_height}{Numeric value to control the height of cells in the table (default: 0.5)}
}
\value{
A ggplot2 object containing the validation results table
}
\description{
Generates a ggplot2 visualization of model validation results in a table format,
showing compounds, probabilities, and predicted vs experimental classes.
}
