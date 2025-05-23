% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kfold_cv_LR.R
\name{stratified}
\alias{stratified}
\title{Stratified Sampling Function}
\usage{
stratified(df, group, size)
}
\arguments{
\item{df}{A dataframe to sample from.}

\item{group}{The grouping variable(s) by which the stratified sampling will be performed.}

\item{size}{Desired sample size, which can be:
- A fixed number (>=1) of samples per group.
- A proportion (<1) to sample from each group.
- A vector of specific sample sizes for each group.}
}
\value{
A dataframe containing the stratified sample.
}
\description{
This function returns a stratified sample from a dataframe `df` based on a grouping variable `group`
and the desired sample size `size`. The function accommodates different sample size scenarios,
including varying sample sizes per group, proportions, and handling of groups with fewer observations
than the requested sample size.
}
\details{
The function handles different sampling scenarios:
- If `size` is a vector, it must match the number of groups or be named to match group levels.
- If `size` is less than 1, it is treated as a proportion of each group's size.
- If `size` is a fixed number, the function checks if each group has enough samples; if not, it returns all samples from smaller groups.


# Example 1: Stratified sample with 10 samples per group
# result <- stratified(df = my_data, group = "my_group", size = 10)

# Example 2: Stratified sample where 20% of each group is sampled
# result <- stratified(df = my_data, group = "my_group", size = 0.2)

# Example 3: Stratified sample with varying sizes per group
# result <- stratified(df = my_data, group = "my_group", size = c(A = 5, B = 10, C = 3))
}
