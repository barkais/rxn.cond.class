#' Stratified Sampling Function
#'
#' This function returns a stratified sample from a dataframe `df` based on a grouping variable `group`
#' and the desired sample size `size`. The function accommodates different sample size scenarios,
#' including varying sample sizes per group, proportions, and handling of groups with fewer observations
#' than the requested sample size.
#'
#' @param df A dataframe to sample from.
#' @param group The grouping variable(s) by which the stratified sampling will be performed.
#' @param size Desired sample size, which can be:
#'             - A fixed number (>=1) of samples per group.
#'             - A proportion (<1) to sample from each group.
#'             - A vector of specific sample sizes for each group.
#' @return A dataframe containing the stratified sample.
#'
#' @details
#' The function handles different sampling scenarios:
#' - If `size` is a vector, it must match the number of groups or be named to match group levels.
#' - If `size` is less than 1, it is treated as a proportion of each group's size.
#' - If `size` is a fixed number, the function checks if each group has enough samples; if not, it returns all samples from smaller groups.
#'
#'
#' # Example 1: Stratified sample with 10 samples per group
#' # result <- stratified(df = my_data, group = "my_group", size = 10)
#'
#' # Example 2: Stratified sample where 20% of each group is sampled
#' # result <- stratified(df = my_data, group = "my_group", size = 0.2)
#'
#' # Example 3: Stratified sample with varying sizes per group
#' # result <- stratified(df = my_data, group = "my_group", size = c(A = 5, B = 10, C = 3))
#' @export

stratified <- function(df, group, size) {
  # Create a factor by interacting the grouping variable(s) and dropping unused levels.
  df.interaction <- interaction(df[group], drop = TRUE)

  # Create a table of the counts of each group level.
  df.table <- table(df.interaction)

  # Split the dataframe into a list, where each element is a subset of the dataframe for a group level.
  df.split <- split(df, df.interaction)

  # Handle case where `size` is a vector of sizes for each group.
  if (length(size) > 1) {
    # Ensure the number of sizes matches the number of group levels.
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))

    # If the size vector is unnamed, assign names based on group levels.
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
    } else {
      # Ensure the named size vector matches the group levels.
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)], # Reorder size vector to match group levels.
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  }
  # Handle case where `size` is a proportion (less than 1) to sample from each group.
  else if (size < 1) {
    n <- round(df.table * size, digits = 0)  # Calculate the number of samples as a proportion of the group size.
  }
  # Handle case where `size` is a fixed number of samples per group.
  else if (size >= 1) {
    # Ensure all groups have enough samples or replace samples if necessary.
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))  # Assign the fixed size to each group.
    } else {
      # Provide a message if some groups have fewer samples than the desired size.
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")

      # Return all samples from groups that have fewer observations than the requested size.
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }

  # For each group, sample `n[x]` observations without replacement.
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = F), ])

  # Combine all the sampled subsets into a single dataframe.
  set1 <- do.call("rbind", temp)

  return(set1)  # Return the stratified sample.
}


#' K-Fold Logistic Cross-Validation
#'
#' This function performs K-fold cross-validation (CV) for ordinal or multinomial logistic regression
#' models. It allows stratification of data, calculates performance metrics, and returns accuracy,
#' the confusion matrix, and other relevant statistics.
#'
#' @param formula A formula specifying the model structure.
#' @param data A dataframe containing the data to fit the model.
#' @param ordinal A boolean indicating whether to fit an ordinal logistic regression (`TRUE`)
#'                or a multinomial logistic regression (`FALSE`). Defaults to `TRUE`.
#' @param folds Number of folds for cross-validation. If `NULL`, stratified sampling can be used.
#' @param outcome.column The column number of the outcome variable. Defaults to the "class" column.
#' @param stratify A boolean indicating whether to stratify the data for sampling. Defaults to `FALSE`.
#' @param sample.vector A vector specifying the number of samples to be drawn from each class during stratified sampling.
#'                      Defaults to the ratio of the smallest class size.
#'
#' @return A list containing:
#'         - `acc`: Accuracy of the model across folds.
#'         - `J.small`: J statistic (Youden's Index) for the smallest class.
#'         - `ct`: Confusion matrix.
#'         - `probs`: Dataframe of predicted probabilities and class predictions.
#'
#' @details
#' - If `folds` is specified, the function performs K-fold cross-validation.
#' - If `stratify` is `TRUE`, the data is stratified based on `sample.vector` before performing CV.
#' - Models can either be ordinal (`MASS::polr`) or multinomial (`nnet::multinom`).
#' - Performance metrics like accuracy and J statistic (for each class) are calculated.
#' @export
k.fold.log.cv <- function(formula, data, ordinal = T,
                          folds = NULL,
                          outcome.column = which(colnames(data) == 'class'),
                          stratify = F,
                          sample.vector = floor(round(summary(data$class)/min(summary(data$class))))) {
  # Initialize lists to store models, probabilities, accuracy, and class predictions.
  models <- list()
  probalities <- list()
  acc <- list()
  class.pred <- list()

  # Stratify the data if requested and no folds are provided.
  if (stratify == TRUE && !is.null(sample.vector) && is.null(folds)) {
    sets <- list()  # List to store stratified sets.

    # Create stratified folds based on sample.vector.
    for (i in 1:ceiling(dim(data)[1] / sum(sample.vector))) {
      # Create a pool of data to sample from.
      if (length(sets) == 0) {
        pool.data <- data
      } else {
        pool.data <- dplyr::setdiff(data, do.call(rbind, sets))
      }

      # If the pool contains fewer observations than the desired sample size, add them all to the set.
      if (dim(pool.data)[1] < sum(sample.vector) & dim(pool.data)[1] > 0) {
        sets[[i]] <- pool.data
        pool.data <- data.frame()  # Clear pool after adding remaining data.
      }

      # Handle cases where there are fewer observations than desired in any group.
      if (nrow(pool.data) > 0 & any(summary(pool.data$class) < sample.vector)) {
        for (j in 1:nrow(pool.data)) {
          # Distribute remaining data across sets.
          ifelse(j <= length(sets),
                 sets[[j]] <- rbind(sets[[j]], pool.data[j, ]),
                 sets[[(j - length(sets))]] <- rbind(sets[[(j - length(sets))]], pool.data[j, ]))
        }
        is_empty <- function(x) (nrow(x) == 0 || is.null(x))
        sets <- sets[sapply(sets, is_empty) == FALSE]  # Remove empty sets.
      } else {
        # Stratify the pool data if there are enough observations.
        if (nrow(pool.data) > 0) suppressWarnings(sets[[i]] <- stratified(pool.data, 'class', sample.vector))
      }

      # Check if all data has been stratified as desired.
      if (i == ceiling(dim(data)[1] / sum(sample.vector))) {
        check <- dim(do.call(rbind, sets)) == dim(data)
        if (!all(check)) {
          print("Wasn't able to stratify as wanted. Check with another sample vector.")
        }
      }
    }

    # Assign each set to a split and arrange by a custom flag.
    for (i in 1:length(sets)) {
      sets[[i]] <- dplyr::mutate(sets[[i]], split.assign = i)
    }
    new_dat <- do.call(rbind, sets)
    new_dat <- dplyr::arrange(new_dat, flag)

    # Train models and compute probabilities and class predictions for each split.
    for (i in 1:length(sort(unique(new_dat$split.assign)))) {
      train <- new_dat[new_dat$split.assign != i,]
      test <- new_dat[new_dat$split.assign == i,]

      # Define starting values for the ordinal logistic regression.
      num.of.vars <- stringi::stri_count(formula, fixed = '+')
      start <- c(rep(0, (num.of.vars + 2)), 1)

      # Fit the ordinal logistic regression model.
      models[[match(i,sort(unique(new_dat$split.assign)))]] <- if (ordinal == T) {
        MASS::polr(formula, data = train, Hess = T, start = start, control = list(maxit = 1000))
      } else {
        nnet::multinom(formula,data = train, maxit = 2000, trace = FALSE)
      }
      # Predict probabilities and classes.
      probalities[[match(i,sort(unique(new_dat$split.assign)))]] <- data.frame(predict(models[[match(i,sort(unique(new_dat$split.assign)))]],
                                                                                       newdata = test, "probs")*100)
      class.pred[[match(i,sort(unique(new_dat$split.assign)))]] <- data.frame(predict(models[[match(i,sort(unique(new_dat$split.assign)))]],
                                                                                      newdata = test, "class"))

      # Combine probabilities, predictions, and flags.
      probalities[[match(i,sort(unique(new_dat$split.assign)))]] <- cbind(probalities[[match(i,sort(unique(new_dat$split.assign)))]],
                                                                          class.pred[[match(i,sort(unique(new_dat$split.assign)))]],
                                                                          test$flag)
      names(probalities[[match(i,sort(unique(new_dat$split.assign)))]])[dim(probalities[[1]])[2] - 1] <- 'prediction'
      names(probalities[[match(i,sort(unique(new_dat$split.assign)))]])[dim(probalities[[1]])[2]] <- 'flag'
    }
  } else {
    # Handle case when folds are specified or folds equal the number of rows (Leave-One-Out Cross Validation).
    if (folds == nrow(data)) {
      split.assign <- sample(1:folds, nrow(data), replace = FALSE)  # LOO case.
    } else {
      split.assign <- caret::createFolds(1:dim(data)[1], folds, list = FALSE)  # K-fold CV.
    }
    new_dat <- cbind(data, split.assign)

    # Train models and compute probabilities for each fold.
    for (i in 1:folds) {
      train <- new_dat[split.assign != i,]
      test <- new_dat[split.assign == i,]

      num.of.vars <- stringi::stri_count(formula, fixed = '+')
      start <- c(rep(0, (num.of.vars + 2)), 1)

      models[[match(i,1:folds)]] <- if (ordinal == T) {
        MASS::polr(formula, data = train, Hess = T, start = start, control = list(maxit = 1000))
      } else {
        nnet::multinom(formula, data = train, maxit = 2000, trace = FALSE)
      }

      # Predict probabilities and classes.
      if (folds == nrow(data)) {
        probalities[[match(i,1:folds)]] <- data.table::transpose(data.frame(predict(models[[match(i,1:folds)]],
                                                                                    newdata = test, "probs")*100))
      } else {
        probalities[[match(i,1:folds)]] <- data.frame(predict(models[[match(i,1:folds)]],
                                                              newdata = test, "probs")*100)
      }

      class.pred[[match(i,1:folds)]] <- data.frame(predict(models[[match(i,1:folds)]], newdata = test, "class"))

      # Combine probabilities, predictions, and flags.
      probalities[[match(i,1:folds)]] <- cbind(probalities[[match(i,1:folds)]],
                                               class.pred[[match(i,1:folds)]], test$flag)
      names(probalities[[match(i,1:folds)]])[dim(probalities[[1]])[2] - 1] <- 'prediction'
      names(probalities[[match(i,1:folds)]])[dim(probalities[[1]])[2]] <- 'flag'
    }
  }

  # Aggregate probabilities, predictions, and flags across all folds.
  probs <- data.frame(do.call(rbind, probalities))
  probs <- probs[order(probs$flag),]
  probs[,1:(dim(probalities[[1]])[2] - 2)] <- round(probs[,1:(dim(probalities[[1]])[2] - 2)], digits = 0)

  # Extract predictions and actual outcomes.
  pred <- probs[,(dim(probalities[[1]])[2] - 1)]
  actual <- data[[outcome.column]]

  # Calculate confusion table and accuracy.
  ct <- table(actual, pred)
  acc <- round((sum(diag(ct)) / sum(ct)) * 100, 2)

  # Calculate performance metrics (J statistic) for each class.
  ct.df <- data.frame(ct)
  TP <- ct.df$Freq[ct.df$actual == ct.df$pred]
  TP_FN <- list()
  TN <- list()
  TN_FP <- list()
  J <- list()

  for (i in levels(ct.df$actual)) {
    TP_FN[[i]] <- sum(ct.df$Freq[ct.df$actual == i])
    TP_FN[[i]] <- TP[which(levels(ct.df$actual) == i)] / TP_FN[[i]]
    TN[[i]] <- sum(ct.df$Freq[dplyr::intersect(which(ct.df$pred != i), which(ct.df$actual != i))])
    TN_FP[[i]] <- sum(ct.df$Freq[(ct.df$actual != i)])
    TN_FP[[i]] <- TN[[i]] / TN_FP[[i]]
    J[[i]] <- TP_FN[[i]] + TN_FP[[i]] - 1
  }

  # Return the overall accuracy and the J statistic for the smallest class.
  J.small <- J[which.min(summary(data$class))]
  return(list(acc, J.small, ct, probs))
}

#' Iterative K-Fold Logistic Cross-Validation
#'
#' This function performs multiple iterations of K-fold cross-validation (CV) for ordinal or multinomial
#' logistic regression models, allowing for repeated assessment of accuracy across multiple random splits.
#' The results include overall accuracy, best/worst iteration accuracy, and confusion tables for both best
#' and worst iterations.
#'
#' @param formula A formula specifying the model structure.
#' @param data A dataframe containing the data to fit the model.
#' @param ordinal A boolean indicating whether to fit an ordinal logistic regression (`TRUE`) or a multinomial
#'                logistic regression (`FALSE`). Defaults to `TRUE`.
#' @param folds Number of folds for cross-validation. If `NULL`, stratified sampling can be used.
#' @param out.col The column number of the outcome variable. Defaults to the "class" column.
#' @param stratify A boolean indicating whether to stratify the data for sampling. Defaults to `FALSE`.
#' @param sample.vector A vector specifying the number of samples to be drawn from each class during stratified sampling.
#'                      Defaults to the ratio of the smallest class size.
#' @param iterations Number of iterations to repeat the cross-validation. Each iteration results in a different random split.
#' @param verbose A boolean to control whether to print the classification tables and accuracy results.
#'                Defaults to `FALSE`.
#'
#' @return A list containing:
#'         - `over.all.accuracy`: Overall average accuracy across all iterations.
#'         - `best`: The best accuracy obtained across iterations.
#'         - `worst`: The worst accuracy obtained across iterations.
#'         - `cts`: A formatted table showing the best and worst confusion matrices.
#'
#' @details
#' - This function repeatedly calls the `k.fold.log.cv` function to perform cross-validation for the specified
#'   number of iterations.
#' - The function computes and prints the average accuracy, best and worst accuracies, and corresponding
#'   confusion matrices if `verbose` is set to `TRUE`.
#' @export
k.fold.log.iter <- function(formula, data, ordinal = T, folds = NULL,
                            out.col = which(colnames(data) == 'class'),
                            stratify = FALSE,
                            sample.vector = floor(round(summary(data$class) / min(summary(data$class)))),
                            iterations, verbose = FALSE) {

  # Initialize lists to store accuracy and classification table results
  iter.list <- list()  # Stores the accuracy for each iteration
  ct.list <- list()    # Stores the classification tables for each iteration

  # Loop over the specified number of iterations
  for (i in 1:iterations) {

    # Call the k-fold ordinal logistic regression function for each iteration
    mod <- k.fold.log.cv(formula, data, ordinal, folds, out.col, stratify, sample.vector)

    # Store the accuracy result from mod[[1]] and the classification table from mod[[3]]
    iter.list[[match(i, 1:iterations)]] <- mod[[1]]
    ct.list[[match(i, 1:iterations)]] <- mod[[3]]
  }

  # Calculate overall average accuracy and j score across all iterations
  over.all.accuracy <- round(Reduce(`+`, iter.list) / iterations, digits = 2)

  # Identify the best and worst accuracy
  best <- iter.list[which.max(iter.list)]  # Best accuracy
  worst <- iter.list[which.min(iter.list)] # Worst accuracy

  # Create a table showing overall accuracy, best accuracy, and worst accuracy
  Accuracies <- knitr::kable(cbind(over.all.accuracy, best, worst))

  # Create a combined classification table for the best and worst accuracies
  tab <- suppressMessages(cbind(
    reshape2::dcast(data.frame(ct.list[which.max(iter.list)]), actual ~ pred),  # Best classification table
    rep("***", length(unique(data[, out.col]))),                                # Separator
    reshape2::dcast(data.frame(ct.list[which.min(iter.list)]), actual ~ pred)   # Worst classification table
  ))

  # Rename the column separating the two classification tables
  names(tab)[length(unique(data[, out.col])) + 2] <- ''

  # Format the combined classification table as a knitr table
  cts <- knitr::kable(tab)

  # If verbose is TRUE, print classification tables and accuracy results
  if (verbose == TRUE) {
    print(cts)
    print(Accuracies)
  }

  # Return the overall accuracy (invisibly)
  invisible(over.all.accuracy)
}
