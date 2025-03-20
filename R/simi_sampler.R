#' Similarity-Based Sampling Function
#'
#' This function samples a subset of observations from a dataset based on their similarity
#' to class-specific centroids (average class feature vectors). It calculates the similarity
#' of each observation to its own class as well as to other classes and returns the indices of
#' the sampled subset. It can optionally plot the similarity before and after sampling.
#'
#' @param data A data frame containing features and class labels. The class label column should be named 'class'.
#' @param class The class from which to sample observations based on similarity.
#' @param compare.with The index of the class similarity to compare (default is 0, which refers to the "same class" similarity).
#' @param plot Logical. If `TRUE`, plots of similarity before and after group truncation will be displayed. Default is `FALSE`.
#' @param sample.size The number of observations to sample from the specified class. Default is the size of the smallest class in the data.
#'
#' @return A vector of indices corresponding to the rows in the dataset that have been sampled.
#'
#' @details
#' - **Data Scaling**:
#'   - The feature columns are scaled to have mean zero and unit variance before similarity calculations.
#'   - The columns `flag` and `tag` (if present) are excluded from the analysis.
#' - **Similarity Calculation**:
#'   - For each class, a centroid vector is computed by averaging the scaled feature vectors of observations in that class.
#'   - The similarity between each observation and its class's centroid vector is calculated using the cosine similarity formula.
#' - **Sampling**:
#'   - The function samples observations from the specified class by evenly selecting points across the similarity range.
#'   - The sample size is specified by `sample.size`, and the sampled points are those closest to evenly spaced intervals within the range of similarities.
#' - **Plotting**:
#'   - If `plot = TRUE`, the function will generate two plots using `ggplot2`:
#'     - **Before Truncation**: A plot showing the similarity of each instance to its class centroid before sampling.
#'     - **After Truncation**: A plot showing the similarity of the sampled instances after truncation.
#'   - Both plots will use colors to represent different classes.
#'
#' @importFrom ggplot2 ggplot aes geom_point element_line element_text
#'             element_blank xlab ggtitle theme
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr mutate
#' @importFrom gridExtra grid.arrange
#'
#' @export
simi.sampler <- function(data, class,
                         compare.with = 0,
                         plot = FALSE,
                         sample.size = min(summary(as.factor(data$class)))) {

  # Identify the output column and relevant variables
  out.col <- which(colnames(data) == 'class')
  vars <- names(data[,-out.col])
  vars <- vars[vars != 'flag' & vars != 'tag']  # Exclude 'flag' and 'tag' columns

  # Prepare data for sampling by scaling numeric values
  sampler.data <- data[vars]
  sampler.data <- data.frame(apply(sampler.data, 2, as.numeric))
  sampler.data <- data.frame(scale(sampler.data, TRUE, TRUE))  # Scale data

  classes <- length(unique(data$class))  # Count unique classes

  # Compute mean values and magnitudes for each class
  for (i in 1:classes) {
    assign(paste('class.', i, '.vector', sep = ''),
           apply(sampler.data[data$class == i, , drop = FALSE], 2, mean))
    assign(paste('class.', i, '.mag', sep = ''),
           sqrt(sum(apply(sampler.data[data$class == i, , drop = FALSE], 2, mean)^2)))
  }

  # Compute similarity of each instance with its class
  new.col <- ncol(sampler.data) + 1
  for (r in 1:nrow(sampler.data)) {
    for (i in 1:classes) {
      if (data$class[r] == i) {
        vec <- get(ls()[grepl(paste('.', i, '.vector', sep = ''), ls())])
        mag <- get(ls()[grepl(paste('.', i, '.mag', sep = ''), ls())])
        sampler.data[r, new.col] <- sum(vec * sampler.data[r, 1:(new.col - 1)]) /
          (mag * sqrt(sum(sampler.data[r, 1:(new.col - 1)]^2)))
      }
    }
  }

  # Compute similarity between instances and all classes
  simi.df <- data.frame(matrix(ncol = classes, nrow = nrow(data)))
  for (i in 1:nrow(simi.df)) {
    for (j in 1:classes) {
      vec <- get(ls()[grepl(paste('.', j, '.vector', sep = ''), ls())])
      mag <- get(ls()[grepl(paste('.', j, '.mag', sep = ''), ls())])
      simi.df[i, j] <- sum(vec * sampler.data[i, 1:(new.col - 1)]) /
        (mag * sqrt(sum(sampler.data[i, 1:(new.col - 1)]^2)))
    }
  }
  names(simi.df) <- as.character(seq(1, classes))  # Name columns by class numbers

  # Prepare similarity table
  names(sampler.data)[new.col] <- 'class.similarity'
  simi.table <- cbind(sampler.data[new.col], simi.df)
  simi.table <- dplyr::mutate(simi.table, class = data$class)
  simi.table <- dplyr::mutate(simi.table, Name = row.names(data))
  simi.table <- dplyr::mutate(simi.table, flag = data$flag)
  colnames(simi.table)[c(1, (2 + classes):ncol(simi.table))] <- c('same.class', 'class', 'Name', 'flag')

  # Plot similarity before truncation
  plot.sim.before <- ggplot(simi.table, aes(simi.table[, compare.with + 1], y = class, label = Name)) +
    geom_point(aes(color = class)) +
    geom_text_repel(aes(label = Name), max.overlaps = 25) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          axis.text.x = element_text(colour = "black", size = 12, face = 'bold'),
          axis.text.y = element_text(colour = "black", size = 12, face = 'bold'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank(),
          legend.position = c(2, 2)) +
    xlab(names(simi.table)[compare.with + 1]) +
    ggtitle('Similarity Before Group Truncation')

  # Sampling based on similarity
  simi.class <- simi.table[simi.table$class == class, compare.with + 1]
  steps <- sort(seq(min(simi.class), max(simi.class), (max(simi.class) - min(simi.class)) / (sample.size - 1)))

  dis.mat <- matrix(ncol = length(steps), nrow = length(sort(simi.class)))
  for (i in 1:nrow(dis.mat)) {
    dis.mat[i, ] <- abs(simi.class[i] - steps)
  }

  pts <- vector()
  row.names(dis.mat) <- as.character(simi.table$flag[simi.table$class == class])

  # Determine which points to keep
  if (length(steps) < length(simi.class)) {
    for (i in 1:ncol(dis.mat)) {
      drop <- which.min(dis.mat[, i])
      pts[i] <- simi.table[as.numeric(names(drop)), 1]
      dis.mat <- dis.mat[-drop, ]
    }
  } else {
    pts <- simi.table[as.numeric(row.names(dis.mat)), 1]
  }

  keep <- as.numeric(simi.table$flag[simi.table$same.class %in% pts])

  # Determine rows to truncate
  class.rows <- simi.table$flag[simi.table$class == class]
  if (min(class.rows) != 1) {
    truncated <- unique(c(1:(min(class.rows) + 1), keep, (max(class.rows) + 1):nrow(data)))
  } else {
    truncated <- unique(c(keep, (max(class.rows) + 1):nrow(data)))
  }

  # Prepare data for after truncation plot
  simi.plot.data <- simi.table[truncated, ][complete.cases(simi.table[truncated, ]), ]
  plot.sim.after <- ggplot(simi.plot.data,
                           aes(x = simi.plot.data[, compare.with + 1], y = class, label = Name)) +
    geom_point(aes(color = class)) +
    geom_text_repel(aes(label = Name), max.overlaps = 25) +
    theme(axis.line = element_line(size = 1, colour = "black"),
          axis.text.x = element_text(colour = "black", size = 12, face = 'bold'),
          axis.text.y = element_text(colour = "black", size = 12, face = 'bold'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank(),
          legend.position = c(2, 2)) +
    xlab(names(simi.table)[compare.with + 1]) +
    ggtitle('Similarity After Group Truncation')

  # Display plots if requested
  if (isTRUE(plot)) gridExtra::grid.arrange(plot.sim.before, plot.sim.after, ncol = 2)

  return(keep)  # Return the indices of samples to keep
}
