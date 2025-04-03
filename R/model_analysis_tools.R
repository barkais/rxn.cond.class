#' Extract Numbers from a String
#'
#' This function extracts all numbers from a given string, including integers,
#' decimal numbers, and negative numbers.
#'
#' @param str A character string from which to extract numbers.
#'
#' @return A numeric vector containing all numbers found in the input string.
#' If no numbers are found, returns an empty numeric vector.
#'
#' @details
#' The function uses regular expressions to identify number patterns in the string:
#' - It matches integers (e.g., 123)
#' - It matches decimal numbers (e.g., 123.45)
#' - It matches negative numbers (e.g., -123, -123.45)
#'
#' @examples
#' extract_numbers("The model achieved 95.4% accuracy with 23 features")
#' # Returns: c(95.4, 23)
#'
#' extract_numbers("Temp range from -10.5 to 37.8 degrees")
#' # Returns: c(-10.5, 37.8)
#'
#' extract_numbers("No numbers here")
#' # Returns: numeric(0)
#'
#' @export
extract_numbers <- function(str) {
  # Use regular expression to match numbers (including decimals and negatives)
  # \d+ matches one or more digits
  # (\.\d+)? matches an optional decimal part
  # -? matches an optional negative sign
  matches <- regmatches(str, gregexpr("-?\\d+(\\.\\d+)?", str))

  # Convert the matches from character to numeric
  # matches is a list with one element, so we need to extract that element
  if (length(matches[[1]]) > 0) {
    return(as.numeric(matches[[1]]))
  } else {
    return(numeric(0))  # Return empty numeric vector if no matches
  }
}

#' Model Information and Performance Evaluation
#'
#' This function computes and displays key performance metrics for a classification model.
#'
#' @param model A trained classification model.
#' @param data A data frame containing the data used for evaluation.
#' @param verbose Logical; if `TRUE` (default), prints classification tables and statistics.
#' @param save.acc Logical; if `TRUE`, formats the accuracy for display. Default is `FALSE`.
#'
#' @return A list containing:
#'   \item{class.table}{The confusion matrix comparing actual and predicted classes.}
#'   \item{accuracy}{The computed accuracy (as percentage).}
#'   \item{accuracy_print}{The formatted accuracy string (if save.acc is TRUE).}
#'   \item{McFadden_R2}{The McFadden pseudo R-squared value.}
#'
#' @details
#' The function generates predictions using the fitted model and computes performance metrics.
#' The confusion matrix, accuracy, and McFadden's R-squared are calculated and returned.
#'
#' @importFrom stats as.formula coef complete.cases cor cor.test glm.control predict setNames var
#' @importFrom utils combn globalVariables head
#' @importFrom nnet multinom
#' @export
mod.info <- function(model, data, verbose = TRUE, save.acc = FALSE) {

  # Generate predictions using the fitted model on the full dataset
  pred <- predict(model, newdata = data, 'class')

  # Extract the actual class labels from the dataset
  actual <- data$class

  # Create a confusion matrix (classification table) comparing actual vs. predicted classes
  class.table <- table(actual, pred)

  # Calculate accuracy
  accuracy <- (sum(diag(class.table)) / sum(class.table)) * 100

  # Format accuracy for display if requested
  accuracy_print <- NULL
  if (save.acc == TRUE) {
    accuracy_print <- round(accuracy, 2)
  }

  # Format accuracy for output
  accuracy_formatted <- paste(round(accuracy, 2), "%", sep = '')

  # Null model (intercept-only model) for comparison to calculate McFadden's R-squared
  if (length(grep('MASS::polr', model$call)) > 0) {
    test.0 <- MASS::polr(class ~ 1, data = data, Hess = TRUE, control = list(maxit = 100))
  } else {
    test.0 <- nnet::multinom(class ~ 1, data = data, maxit = 2000, trace = F)
  }

  # Log-likelihood of the fitted model and null model
  loglik.0 <- as.numeric(get_loglik_multinom(test.0))  # Log-likelihood of the null model
  loglik.1 <- as.numeric(get_loglik_multinom(model))   # Log-likelihood of the fitted model

  # Compute McFadden's pseudo R-squared
  McFadden_R2 <- round((1 - (loglik.1 / loglik.0)), digits = 3)

  # Create a table of accuracy and McFadden's R-squared
  st <- cbind(accuracy_formatted, McFadden_R2)
  names(st) <- c("Accuracy", "McFadden_R2")

  # Display the model statistics (Accuracy and Pseudo-R2) as a knitr table
  stats <- knitr::kable(st, caption = "\n\nFull Model Stats - Overall Accuracy and Pseudo-R2")

  # Display the model coefficients as a knitr table
  ce <- knitr::kable(coef(model), caption = "\n\nModel Coefficients")

  # If verbose is TRUE, print the model statistics and coefficients
  if (verbose == TRUE) {
    print(stats)
    print(ce)
  }

  # Return results as a list instead of using global variables
  return(list(
    class.table = class.table,
    accuracy = accuracy,
    accuracy_print = accuracy_print,
    McFadden_R2 = McFadden_R2
  ))
}


#' Confusion Matrix Heatmap Plot
#'
#' This function generates a heatmap visualization of a confusion matrix using `ggplot2`.
#' It calculates proportions for True Positives, False Positives, Total Size, Precision,
#' and Accuracy, and presents them as part of the plot. The plot includes both counts
#' and percentages of correct/incorrect classifications, along with other summary metrics.
#'
#' @param class.table A confusion matrix in table or matrix format.
#' @param plot.title The title to be displayed on the plot.
#' @param conformation A subtitle or caption to be displayed on the plot (e.g., model information).
#'
#' @return A list containing:
#'   \item{plot}{A heatmap plot visualizing the confusion matrix with counts, percentages, and classification metrics.}
#'   \item{true_recall}{A data frame of recall values for each class.}
#'
#' @details
#' - The function takes a confusion matrix and computes the total row and column sums.
#' - It calculates proportions for recall, precision, accuracy, and sizes, and uses these metrics to create
#'   a heatmap. True classifications are marked as 'True' and incorrect ones as 'False'.
#' - Additional metrics like overall accuracy, class size, and precision are calculated and displayed.
#' - The heatmap tiles are colored based on the classification outcome, and the alpha level (transparency)
#'   of the tiles represents the computed proportions for better visual distinction.
#' - The plot uses `ggplot2` with customized aesthetics, and the plot title and caption can be personalized.
#' - Class-wise recall values are also computed and returned in the results list.
#'
#' @importFrom ggplot2 ggplot aes geom_tile coord_fixed geom_text scale_fill_manual scale_alpha
#'             theme element_blank element_text margin guide_legend unit labs
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate
#'
#' @export
ct_plot <- function(class.table, plot.title, conformation) {

  # Convert the confusion matrix to a matrix format
  ct <- as.matrix(class.table)
  classes <- nrow(ct)  # Number of classes

  # Initialize a vector for row totals and append to the confusion matrix
  total <- rep(NA, nrow(ct))
  ct <- cbind(ct, total)

  # Calculate total counts for each row
  for (i in 1:dim(ct)[1]) {
    ct[i, (dim(ct)[1] + 1)] <- sum(ct[i, 1:(dim(ct)[2] - 1)])
  }

  # Initialize a vector for column totals and append to the confusion matrix
  total <- rep(NA, ncol(ct))
  ct <- rbind(ct, total)

  # Calculate total counts for each column
  for (i in 1:dim(ct)[2]) {
    ct[dim(ct)[2], i] <- sum(ct[1:(dim(ct)[1] - 1), i])
  }

  # Reshape the confusion matrix for ggplot
  ct <- reshape2::melt(ct)
  names(ct) <- c('Exp', 'Pred', 'Freq')  # Rename columns for clarity
  ct$Exp <- as.factor(ct$Exp)  # Convert to factors for ordered plotting
  ct$Pred <- as.factor(ct$Pred)

  # Initialize columns for legend and proportion
  ct <- dplyr::mutate(ct, Legend = rep(NA, nrow(ct)))
  ct <- dplyr::mutate(ct, prop = rep(NA, nrow(ct)))

  # Calculate proportions and legends for each class
  for (i in as.numeric(row.names(ct[ct$Exp != 'total' & ct$Pred != 'total', ]))) {
    for (j in 1:classes) {
      if (ct$Exp[[i]] == as.character(j)) {
        ct$prop[[i]] <- ct$Freq[[i]] / sum(ct$Freq[ct$Exp == as.character(j) & ct$Pred != 'total'])
      }
    }
    ct$Legend[i] <- ifelse(ct$Exp[i] == ct$Pred[i], 'True', 'False')  # Classify as True or False
  }

  # Round and adjust proportions for display
  ct$prop <- round(ct$prop, digits = 2) * 100

  # Handle total predictions for size and precision
  for (i in as.numeric(row.names(ct[ct$Exp == 'total' | ct$Pred == 'total', ]))) {
    if (ct$Pred[i] == 'total' & ct$Exp[i] != 'total') {
      ct$prop[i] <- round((ct$Freq[i] / ct$Freq[ct$Exp == 'total' & ct$Pred == 'total']), 2) * 100
      ct$Legend[i] <- 'Size'
    }
    if (ct$Pred[i] != 'total' & ct$Exp[i] == 'total') {
      ct$prop[[i]] <- round((ct$Freq[ct$Exp == ct$Pred[i] & ct$Pred == ct$Pred[i]] / ct$Freq[i]), 2) * 100
      ct$Legend[i] <- 'Precision'
    }
    if (ct$Pred[i] == 'total' & ct$Exp[i] == 'total') {
      ct$prop[[i]] <- round((sum(diag(class.table)) / ct$Freq[i]), 2) * 100
      ct$Legend[i] <- 'Accuracy'
    }
  }

  ct$prop <- round(ct$prop, digits = 2)  # Final rounding of proportions

  # Create a column for plotting
  ct <- dplyr::mutate(ct, value = ct$prop)

  # Set proportions for non-true/false entries to a constant value for visualization
  for (i in 1:nrow(ct)) {
    if (ct$Legend[i] != 'True' & ct$Legend[i] != 'False') {
      ct$prop[i] <- 45  # Constant value for other metrics
    }
  }

  ct[is.na(ct)] <- 0  # Replace NA values with 0

  # Calculate overall accuracy for caption enhancement
  overall_accuracy <- ct$value[ct$Legend == 'Accuracy']

  # Enhance colors for better contrast and readability
  color_true <- '#178A17'       # Darker green for true predictions
  color_false <- '#A00000'      # Darker red for false predictions
  color_size <- '#FCDEBE'       # Light orange for size
  color_precision <- '#E09932'  # Medium orange for precision
  color_accuracy <- '#0057A0'   # Darker blue for accuracy

  # Create the heatmap using ggplot2
  base <- ggplot(data = ct,
                 mapping = aes(x = ordered(Pred, levels = sort(unique(Pred))),
                               y = ordered(Exp, levels = rev(sort(unique(Exp)))),
                               fill = Legend,
                               alpha = prop)) +
    geom_tile(color = 'black', size = 0.75) +  # Tile borders
    coord_fixed() +
    geom_text(aes(label = paste(Freq, "\n", '(', value, '%', ')', sep = '')),
              size = 5, vjust = .5, fontface = "bold", alpha = 1) +  # Text labels on tiles
    scale_fill_manual(values = c(True = color_true,
                                 False = color_false,
                                 Size = color_size,
                                 Precision = color_precision,
                                 Accuracy = color_accuracy),
                      guide = guide_legend(title = NULL)) +  # Color mapping
    scale_alpha(guide = "none") +  # Hide prop legend
    theme(
      # Remove grid elements
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),

      # Improve text styling
      axis.text.x = element_text(size = 14, face = 'bold'),
      axis.text.y = element_text(size = 14, face = 'bold'),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),

      # Enhanced title and caption styling
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
      plot.caption = element_text(size = 11, hjust = 0, margin = margin(t = 15), lineheight = 1.2),

      # Legend styling
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.75, "lines"),
      legend.margin = margin(t = 10),

      # Overall plot margins
      plot.margin = margin(15, 15, 15, 15)
    )

  # Extract recall for true classifications
  true_recall <- data.frame(ct$prop[ct$Exp == ct$Pred & ct$Exp != 'total'])
  row.names(true_recall) <- unique(as.character(ct$Exp[ct$Exp != 'total']))
  names(true_recall) <- 'Class Recall'

  # Instead of global assignment, we'll return this as part of the result
  # Low.Recall <<- true_recall  # Original code with global assignment

  enhanced_caption <- conformation

  # Add enhanced titles and captions to the plot
  plot <- base + labs(title = plot.title,
                      caption = enhanced_caption)

  # Return a list with both the plot and the recall values
  return(list(plot = plot, true_recall = true_recall))
}

#' Probability Heatmap Plot
#'
#' This function generates a heatmap to visualize the predicted probabilities of class membership
#' for a classification model using `ggplot2`. It highlights correct and incorrect predictions
#' with different colors and displays the predicted probabilities for each class as part of the plot.
#'
#' @param model A fitted classification model used to make predictions.
#' @param data A dataset with the same structure as the training data, including the true class labels.
#' @param plot.title The title to be displayed on the plot.
#' @param conformation A subtitle or caption to be displayed on the plot (e.g., model version or information).
#'
#' @return A heatmap plot visualizing the predicted probabilities for each class, along with
#' indicators for correct and incorrect predictions.
#'
#' @details
#' - **Predictions and Probabilities**:
#'   - The function predicts the class membership for each instance in the dataset and also retrieves
#'     the predicted class probabilities from the model.
#'   - These probabilities are converted into percentages and visualized in the heatmap.
#'   - Each row represents an observation, and the columns represent the predicted probabilities for each class.
#' - **Color Coding**:
#'   - Correct predictions are colored green.
#'   - Misclassifications where the second-highest probability corresponds to the true class are colored orange.
#'   - Other misclassifications are colored red.
#' - **Heatmap**:
#'   - The heatmap shows the predicted probabilities using a gradient from white to blue.
#'   - Text labels show the exact probability values in each tile.
#'   - For each observation, the true class is indicated by a separate tile and labeled with its class number.
#' - **Customization**:
#'   - The title and caption of the plot can be customized via the function parameters.
#'
#' @importFrom ggplot2 ggplot aes geom_tile coord_fixed geom_text scale_fill_gradient guide_colorbar
#'             theme element_text element_blank scale_x_discrete labs unit
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#'
#' @export
prob.heatmap <- function(model, data, plot.title, conformation) {
  # Determine number of classes
  classes <- length(model$lev)

  # Generate predictions and probabilities
  pred <- predict(model, newdata = data, 'class')
  r.w <- pred == data$class
  probs <- predict(model, newdata = data, 'probs') * 100  # Convert to percentage

  # Adjust probabilities if binary classification
  if (dim(data.frame(probs))[2] == 1) {
    probs <- cbind(100 - data.frame(probs), data.frame(probs))
  }

  # Create data frame with expected and predicted classes, and correct/wrong classification
  verif <- data.frame(cbind(data$class, pred, r.w, probs, rep(NA, nrow(probs))))
  row.names(verif) <- row.names(probs)
  colnames(verif)[c(1, dim(verif)[2])] <- c('Exp', 'color')  # Rename columns

  # Assign colors based on correct classification, second-highest probability, or incorrect prediction
  for (i in 1:dim(verif)[1]) {
    second <- sort(as.numeric(verif[i, 4:(4 + classes)]), decreasing = T)[2]  # Get 2nd highest probability
    where.is.sec <- which(as.numeric(verif[i, 4:(4 + (classes - 1))]) == second)
    if (verif$r.w[i] == 1) {
      verif$color[i] <- "#66a180"  # Green for correct predictions
    } else {
      if (as.numeric(verif$Exp[i]) == where.is.sec) {
        verif$color[i] <- 'tan1'  # Tan if second-highest probability matches actual class
      } else {
        verif$color[i] <- '#d1505b'  # Red for incorrect prediction
      }
    }
  }

  # Prepare data for plotting
  pro.df <- data.frame(probs)
  pro.df <- tibble::rownames_to_column(pro.df)
  pro.df[, 1] <- factor(pro.df[, 1], levels = pro.df[, 1])  # Order row names as factors
  pro.df[, (dim(pro.df)[2] + 1)] <- as.numeric(data$class)
  colnames(pro.df) <- c('Name', model$lev, 'Exp')  # Rename columns for plotting
  row.names(pro.df) <- row.names(probs)

  # Convert data to long format for ggplot
  long <- reshape2::melt(pro.df, id.vars = 'Name')
  long[,3] <- round(long[,3], digits = 0)  # Round probabilities to integers for display
  long <- dplyr::mutate(long, exp_shape = rep(NA, nrow(long)))  # Add column for expected shapes

  # Assign expected shapes based on class
  for (i in 1:nrow(long)) {
    for (j in 1:classes) {
      if (long$variable[i] == 'Exp' & long$value[i] == j) {
        long$exp_shape[i] <- j
      }
    }
  }

  # Prepare color vector for labels
  col_vec <- vector(mode = 'numeric')
  coloring <- c("darkgoldenrod4", "slateblue", 'darksalmon',
                'darkblue', 'navajowhite4',
                'darkcyan', 'chocolate4',
                "coral3","cornsilk4",'darkslateblue')  # Define colors for each class

  for (i in 1:length(long[long$variable == 'Exp',4])) {
    col_vec[i] <- coloring[long[long$variable == 'Exp',4][i]]
  }

  # Prepare labels for text display
  label.vec <- as.character(long[long$variable == 'Exp',4])

  # Generate the probability heatmap
  prob.heatmap <- ggplot2::ggplot(mapping = ggplot2::aes(x = variable,
                                                         y = ordered(Name,
                                                                     levels = rev(factor(pro.df$Name,
                                                                                         levels = pro.df$Name))))) +
    ggplot2::geom_tile(data = long[long$variable != 'Exp',],
                       color = 'black', ggplot2::aes(fill = value)) +  # Add tiles for probabilities
    ggplot2::coord_fixed(ratio = 0.5) +  # Adjust cell height with ratio
    ggplot2::geom_text(data = long[long$variable != 'Exp',],
                       ggplot2::aes(label = value)) +  # Display probability values in each cell
    ggplot2::scale_fill_gradient(low = "#FFFFFF",
                                 high = "dodgerblue2",
                                 guide = ggplot2::guide_colorbar(title = NULL,
                                                                 frame.colour = "black",
                                                                 ticks.colour = "black")) +  # Set fill gradient for probabilities
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, face = 'bold'),
                   axis.text.y = ggplot2::element_text(size = 10, face = 'bold',
                                                       colour = rev(verif$color)),  # Color y-axis labels by verification result
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.text = element_text(size = 5),  # Smaller legend text
                   legend.key.size = unit(0.75, "lines")) +  # Place legend at bottom of plot
    ggplot2::scale_x_discrete(position = "top", limits = levels(long$variable)) +  # Display x-axis labels at top
    ggplot2::geom_tile(data = long[long$variable == 'Exp',],
                       alpha = 0, inherit.aes = F,
                       ggplot2::aes(x = rev(variable),
                                    y = ordered(Name, levels = rev(factor(pro.df$Name,
                                                                          levels = pro.df$Name))))) +  # Add empty tiles for 'Exp' rows
    ggplot2::geom_text(data = long[long$variable == 'Exp',], label = label.vec,
                       size = 4, color = col_vec, fontface = 'bold')  # Display 'Exp' labels in bold

  # Add titles and caption to the plot
  prob.heatmap + labs(title = plot.title,
                      subtitle = 'Probability Heatmap',
                      caption = conformation)
}

#' Create a Patchwork Probability Heatmap
#'
#' Generates a probability heatmap visualization for classification model results using the patchwork package.
#' The function supports multi-page display for large datasets, keeping appropriate row distributions.
#'
#' @param model A classification model object with prediction capabilities
#' @param data The data frame containing the data to be visualized
#' @param plot.title String containing the title of the plot
#' @param conformation String containing any additional information to display as caption
#' @param max_rows_per_plot Integer specifying the maximum number of rows to display per plot (default: 15)
#' @param show_legend Logical, whether to display the color legend (default: FALSE)
#'
#' @return A ggplot2 object containing the combined probability heatmap
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient
#'             theme element_text element_blank unit scale_x_discrete
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#'
#' @export
patchwork_prob_heatmap <- function(model, data, plot.title, conformation,
                                   max_rows_per_plot = 15,  # Maximum rows per page
                                   show_legend = FALSE) {    # Add parameter to control legend visibility
  # Check if patchwork is installed
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is needed for this function. Please install it with install.packages('patchwork')")
  }

  # Determine number of classes
  classes <- length(model$lev)

  # Generate predictions and probabilities
  pred <- predict(model, newdata = data, 'class')
  r.w <- pred == data$class
  probs <- predict(model, newdata = data, 'probs') * 100  # Convert to percentage

  # Adjust probabilities if binary classification
  if (dim(data.frame(probs))[2] == 1) {
    probs <- cbind(100 - data.frame(probs), data.frame(probs))
  }

  # Create data frame with expected and predicted classes, and correct/wrong classification
  verif <- data.frame(cbind(data$class, pred, r.w, probs, rep(NA, nrow(probs))))
  row.names(verif) <- row.names(probs)
  colnames(verif)[c(1, dim(verif)[2])] <- c('Exp', 'color')  # Rename columns

  # Assign colors based on correct classification, second-highest probability, or incorrect prediction
  for (i in 1:dim(verif)[1]) {
    second <- sort(as.numeric(verif[i, 4:(4 + classes)]), decreasing = TRUE)[2]  # Get 2nd highest probability
    where.is.sec <- which(as.numeric(verif[i, 4:(4 + (classes - 1))]) == second)
    if (verif$r.w[i] == 1) {
      verif$color[i] <- "#008000"  # Green for correct predictions
    } else {
      if (as.numeric(verif$Exp[i]) == where.is.sec) {
        verif$color[i] <- "#FF8C00"  # Orange if second-highest probability matches actual class
      } else {
        verif$color[i] <- "#FF0000"  # Red for incorrect prediction
      }
    }
  }

  # Prepare data for plotting
  pro.df <- data.frame(probs)
  for(i in 1:ncol(probs)) names(pro.df)[i] <- paste0(names(pro.df)[i], as.character(i))
  pro.df <- tibble::rownames_to_column(pro.df)
  pro.df[, 1] <- factor(pro.df[, 1], levels = pro.df[, 1])  # Order row names as factors
  pro.df[, (dim(pro.df)[2] + 1)] <- as.numeric(data$class)
  colnames(pro.df) <- c('Name', model$lev, 'Exp')  # Rename columns for plotting
  row.names(pro.df) <- row.names(probs)

  # Add flag column for proper sorting
  if("flag" %in% colnames(data)) {
    pro.df$flag <- data$flag
  } else {
    pro.df$flag <- 1:nrow(pro.df)
  }

  # Convert data to long format for ggplot
  long <- reshape2::melt(pro.df, id.vars = c('Name', 'flag'))
  long[,4] <- round(long[,4], digits = 0)  # Round probabilities to integers for display
  long <- dplyr::mutate(long, exp_shape = rep(NA, nrow(long)))  # Add column for expected shapes

  # Define color palette for class labels
  coloring <- c("darkgoldenrod4", "slateblue", 'darksalmon',
                'darkblue', 'navajowhite4',
                'darkcyan', 'chocolate4',
                "coral3","cornsilk4",'darkslateblue')

  # Assign expected shapes based on class
  for (i in 1:nrow(long)) {
    for (j in 1:classes) {
      if (long$variable[i] == 'Exp' & long$value[i] == j) {
        long$exp_shape[i] <- j
      }
    }
  }

  # Calculate the number of plots needed with balanced row distribution
  total_rows <- length(unique(long$Name))

  # Calculate optimal number of plots and rows per plot to keep them balanced
  if (total_rows <= max_rows_per_plot) {
    # If total rows is less than max, just use one plot
    num_plots <- 1
    rows_per_plot <- total_rows
  } else {
    # Try to distribute rows evenly
    num_plots <- ceiling(total_rows / max_rows_per_plot)

    # Calculate balanced rows per plot (round up to avoid having too few rows in the last plot)
    rows_per_plot <- ceiling(total_rows / num_plots)

    # Check if this makes sense (adjust if needed)
    if (rows_per_plot * (num_plots - 1) >= total_rows) {
      # We can fit all data in fewer plots
      num_plots <- ceiling(total_rows / rows_per_plot)
    }
  }

  plot_list <- list()  # List to store plots

  # Create a plotting function - without subtitle and legend
  create_plot <- function(data_subset, row_colors, include_legend = FALSE) {
    # Calculate appropriate text size based on number of rows
    rows_in_subset <- length(unique(data_subset$Name))
    text_size <- max(6, 10 - (rows_in_subset - 10) / 5)
    plot_ratio <- ifelse(rows_in_subset > 10, 0.3 + (10 / rows_in_subset), 0.5)

    # Create color vector for class labels
    unique_names <- unique(data_subset$Name)
    exp_data <- data_subset[data_subset$variable == 'Exp',]
    col_vec <- vector(mode = 'character', length = nrow(exp_data))

    for (i in 1:nrow(exp_data)) {
      class_idx <- exp_data$value[i]
      color_idx <- (class_idx %% length(coloring))
      if (color_idx == 0) color_idx <- length(coloring)
      col_vec[i] <- coloring[color_idx]
    }

    # Set up the colorbar guide based on whether we want to show the legend
    colorbar_guide <- if(include_legend && show_legend) {
      ggplot2::guide_colorbar(title = "Probability (%)",
                              frame.colour = "black",
                              ticks.colour = "black")
    } else {
      "none"  # Hide guide completely if legend is not needed
    }

    # Generate the probability heatmap
    p <- ggplot2::ggplot(mapping = ggplot2::aes(x = variable,
                                                y = ordered(Name, levels = rev(unique_names)))) +
      # Add background tiles
      ggplot2::geom_tile(data = data_subset[data_subset$variable != 'Exp',],
                         color = 'black', size = 0.5,
                         ggplot2::aes(fill = value)) +
      # Adjust plot ratio
      ggplot2::coord_fixed(ratio = plot_ratio) +
      # Add value text
      ggplot2::geom_text(data = data_subset[data_subset$variable != 'Exp',],
                         ggplot2::aes(label = value),
                         size = text_size/2.5,
                         color = "black") +
      # Color scale for cells
      ggplot2::scale_fill_gradient(low = "#FFFFFF",
                                   high = "dodgerblue2",
                                   guide = colorbar_guide) +
      # Theme customization
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = text_size, face = 'bold'),
        axis.text.y = ggplot2::element_text(size = text_size, face = 'bold',
                                            colour = rev(row_colors)),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = if(include_legend && show_legend) "bottom" else "none",
        plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      ) +
      # Position x-axis labels at top
      ggplot2::scale_x_discrete(position = "top") +
      # Add empty tiles for Exp column
      ggplot2::geom_tile(data = data_subset[data_subset$variable == 'Exp',],
                         alpha = 0, inherit.aes = FALSE,
                         ggplot2::aes(x = variable,
                                      y = ordered(Name, levels = rev(unique_names)))) +
      # Add expected class labels
      ggplot2::geom_text(data = data_subset[data_subset$variable == 'Exp',],
                         ggplot2::aes(label = value),
                         size = text_size/2,
                         color = col_vec,
                         fontface = 'bold')

    return(p)
  }

  # Create individual plots - only include legend on the last plot if show_legend is TRUE
  for (i in 1:num_plots) {
    # Calculate row range for this page with balanced distribution
    start_idx <- (i-1) * rows_per_plot + 1
    end_idx <- min(i * rows_per_plot, total_rows)

    # Get names for rows on this page
    row_names_ordered <- unique(long$Name)
    page_rows <- row_names_ordered[start_idx:end_idx]

    # Filter data for this page
    page_data <- long[long$Name %in% page_rows,]

    # Get row colors for this page
    page_row_colors <- verif$color[match(page_data$Name[page_data$variable == page_data$variable[1]], row.names(verif))]

    # Create the plot - only include legend for the last plot if show_legend is TRUE
    include_legend <- (i == num_plots)
    plot_list[[i]] <- create_plot(page_data, page_row_colors, include_legend)
  }

  # Combine the plots - using patchwork namespace
  combined_plot <- plot_list[[1]]
  if (num_plots > 1) {
    for (i in 2:num_plots) {
      combined_plot <- combined_plot + plot_list[[i]]
    }
  }

  # Add a single title, subtitle and caption
  combined_plot <- combined_plot +
    patchwork::plot_annotation(
      title = plot.title,
      caption = conformation,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
      )
    )

  # Add layout settings - only collect guides if we're showing the legend
  if (show_legend) {
    combined_plot <- combined_plot +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  }

  return(combined_plot)
}

#' Combine Multiple Plots with a Shared Legend
#'
#' Combines multiple ggplot2 plots using the patchwork package while preserving a single legend.
#' The function allows specifying which plot's legend to use and adds shared title elements.
#'
#' @param plot_list List of ggplot2 objects to combine
#' @param legend_from_plot_index Integer specifying which plot's legend to use (default: the last plot)
#' @param title Optional title for the combined plot
#' @param subtitle Optional subtitle for the combined plot
#' @param caption Optional caption for the combined plot
#'
#' @return A patchwork object containing the combined plots with a shared legend
#'
#' @importFrom ggplot2 theme element_text margin
#' @importFrom patchwork wrap_plots wrap_elements plot_layout plot_annotation
#'
#' @export
combine_plots_with_legend <- function(plot_list,
                                      legend_from_plot_index = length(plot_list),
                                      title = NULL,
                                      subtitle = NULL,
                                      caption = NULL) {

  # Check if required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork package is required")
  }

  # Get the plot with the legend we want to use
  legend_plot <- plot_list[[legend_from_plot_index]]

  # Extract the legend properly using grid and gtable
  get_legend <- function(plot) {
    # Create a version of the plot with legend at bottom
    plot <- plot + ggplot2::theme(legend.position = "bottom")

    # Build the plot
    built_plot <- ggplot2::ggplot_build(plot)

    # Create the gtable
    plot_table <- ggplot2::ggplot_gtable(built_plot)

    # Find the legend
    legend_index <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")

    if (length(legend_index) == 0) {
      warning("No legend found in the specified plot")
      return(NULL)
    } else {
      # Extract the legend
      return(plot_table$grobs[[legend_index]])
    }
  }

  # Extract the legend
  legend_grob <- get_legend(legend_plot)

  # Remove legends from all plots
  plot_list_no_legends <- lapply(plot_list, function(p) {
    p + ggplot2::theme(legend.position = "none")
  })

  # Combine plots without legends
  combined_plot <- patchwork::wrap_plots(plot_list_no_legends)

  # Add the legend if it exists
  if (!is.null(legend_grob)) {
    # Create a legend panel with reduced top margin
    legend_plot_panel <- patchwork::wrap_elements(panel = legend_grob)

    # Combine the plots with the legend using a single plot_layout call
    combined_plot <- combined_plot /
      legend_plot_panel +
      patchwork::plot_layout(heights = c(20, 1), guides = "collect")
  }

  # Add title, subtitle, and caption
  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    combined_plot <- combined_plot +
      patchwork::plot_annotation(
        title = title,
        subtitle = subtitle,
        caption = caption,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5, margin = ggplot2::margin(b = 15)),
          plot.caption = ggplot2::element_text(size = 10, hjust = 0, margin = ggplot2::margin(t = 10)),
          # Reduce overall plot margins
          plot.margin = ggplot2::margin(5, 5, 5, 5)
        )
      )
  }

  return(combined_plot)
}

#' Create a Validation Results Table
#'
#' Generates a ggplot2 visualization of model validation results in a table format,
#' showing compounds, probabilities, and predicted vs experimental classes.
#'
#' @param validation_df Data frame containing validation results with columns for compound names,
#'        probabilities, predicted classes, and experimental classes
#' @param plot.title String containing the title of the plot (default: "Validation Results")
#' @param subtitle String containing the subtitle (default: "Predicted vs Experimental Classes")
#' @param conformation String containing any additional information
#' @param cell_height Numeric value to control the height of cells in the table (default: 0.5)
#'
#' @return A ggplot2 object containing the validation results table
#'
#' @importFrom ggplot2 ggplot geom_segment geom_text aes scale_color_identity
#'             xlim ylim theme_void theme element_text margin labs coord_fixed
#'
#' @export
validation_table <- function(validation_df,
                             plot.title = "Validation Results",
                             subtitle = "Predicted vs Experimental Classes",
                             conformation = "",
                             cell_height = 0.5) {  # Controls cell height

  # Required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }

  # Make a copy of the input data
  df <- validation_df

  # Check for correct prediction
  df$Correct <- df[, 2] == df[, 3]

  # Calculate accuracy
  accuracy <- round(mean(df$Correct) * 100, 1)

  # Create a new data frame for the plot
  plot_df <- data.frame(
    Compound = row.names(df),
    Probability = df[, 1],
    Predicted = df[, 2],
    Experimental = df[, 3],
    Correct = df$Correct,
    stringsAsFactors = FALSE
  )

  # Number of rows
  n_rows <- nrow(plot_df)

  # Format the ggplot - starting with an empty plot
  plot <- ggplot2::ggplot() +

    # Set up the grid for the table - but with equal-height cells and NO empty first row
    # Bottom of table at y=0, top of table at y=n_rows

    # Vertical grid lines (columns)
    ggplot2::geom_segment(
      data = data.frame(x = c(0, 1, 2, 3)),
      mapping = ggplot2::aes(
        x = x, xend = x,
        y = 0, yend = n_rows
      ),
      color = "black", size = 0.5
    ) +

    # Horizontal grid lines (rows) - with EQUAL height cells
    ggplot2::geom_segment(
      data = data.frame(y = 0:n_rows),
      mapping = ggplot2::aes(
        x = 0, xend = 3,
        y = y, yend = y
      ),
      color = "black", size = 0.5
    ) +

    # Column headers directly at the top of the table
    ggplot2::geom_text(
      data = data.frame(
        x = c(0.5, 1.5, 2.5),
        y = rep(n_rows + 0.5, 3),
        label = c("", "P(%)", "Pred | Exp"),
        stringsAsFactors = FALSE
      ),
      mapping = ggplot2::aes(
        x = x, y = y,
        label = label
      ),
      size = 4.5, fontface = "bold"
    ) +

    # Compound names
    ggplot2::geom_text(
      data = data.frame(
        x = rep(0.5, n_rows),
        y = n_rows:1 - 0.5,  # Reverse order for y to match the table image
        label = plot_df[, 1],
        stringsAsFactors = FALSE
      ),
      mapping = ggplot2::aes(
        x = x, y = y,
        label = label
      ),
      size = 4, fontface = "bold"
    ) +

    # Probabilities
    ggplot2::geom_text(
      data = data.frame(
        x = rep(1.5, n_rows),
        y = n_rows:1 - 0.5,  # Reverse order for y
        label = plot_df$Probability,
        stringsAsFactors = FALSE
      ),
      mapping = ggplot2::aes(
        x = x, y = y,
        label = label
      ),
      size = 4
    ) +

    # Predicted/Experimental combined
    ggplot2::geom_text(
      data = data.frame(
        x = rep(2.5, n_rows),
        y = n_rows:1 - 0.5,  # Reverse order for y
        label = paste(plot_df$Predicted, "|", plot_df[, 4]),
        correct = plot_df$Correct,
        stringsAsFactors = FALSE
      ),
      mapping = ggplot2::aes(
        x = x, y = y,
        label = label,
        color = ifelse(correct, "#008000", "#FF0000")
      ),
      size = 4, fontface = "bold"
    ) +

    # Use colors directly
    ggplot2::scale_color_identity() +

    # Set limits to include headers above the table
    ggplot2::xlim(-0.5, 3.5) +
    ggplot2::ylim(-0.5, n_rows + 1) +

    # Theme customization
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0, margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    ) +

    # Add title, subtitle and caption
    ggplot2::labs(
      title = plot.title,
      subtitle = subtitle
    ) +

    # Set aspect ratio - make cells rectangular but maintain fixed width-to-height ratio
    ggplot2::coord_fixed(ratio = 0.33, clip = "off")

  return(plot)
}

#' Retrain the Best Model
#'
#' Retrains a selected model from a list of top models, evaluates performance on training and test sets,
#' and produces visualization plots for model assessment.
#'
#' @param top_models_file String path to a file containing top models information
#' @param which_model Integer specifying which model to retrain (default: 1)
#' @param title.of.analysis String containing the title for analysis plots
#' @param one_ratio Numeric ratio of class 1 samples to include
#' @param two_ratio Numeric ratio of class 2 samples to include
#'
#' @return When return_results is TRUE, a list containing:
#'   \item{train_plots}{A list with the plots for the training set}
#'   \item{test_plots}{A list with the plots for the test set}
#'   \item{combined_plot}{The combined confusion matrix plots}
#'   \item{validation_result}{A data frame with validation results}
#'   \item{validation_plot}{A plot visualizing the validation results}
#'   \item{fitted_model}{The fitted model object}
#'   When return_results is FALSE (default), the function returns NULL invisibly
#'
#' @importFrom data.table fread
#' @importFrom rxn.cond.class simi.sampler clean_correlated_features
#' @importFrom nnet multinom
#'
#' @export
retrain_best_model <- function(top_models_file, which_model = 1, title.of.analysis, one_ratio, two_ratio) {

  top_models <- data.frame(data.table::fread(top_models_file))
  data <- data.frame(data.table::fread(top_models$dataset[which_model]), check.names = F)
  row.names(data) <- data[, 1]
  data <- data[, -1]
  data$class <- as.factor(data$class)

  # Split the data into training and testing using 75% of the data for training

  # Similarity of Class 1 with itself
  one <- rxn.cond.class::simi.sampler(data, 1, compare.with = 0, sample.size = round(sum(data$class == 1) * one_ratio))

  # Similarity of Class 2 with itself
  two <- rxn.cond.class::simi.sampler(data, 2, compare.with = 0, sample.size = round(sum(data$class == 2) * two_ratio))

  # Combine the training data
  similarities <- c(one, two)

  # Lower the dimensionality of the data with a primary feature importance cleanup
  low.dim.data <- rxn.cond.class::clean_correlated_features(data, corr_threshold = 0.85, method = "mutual_information")

  valid.indices.evaluate <- extract_numbers(top_models[, 10][[which_model]])
  Train.set <- low.dim.data[similarities, ]
  Test.set <- low.dim.data[-similarities, ]
  valid.set <- Test.set[valid.indices.evaluate, ]
  Test.set <- Test.set[-valid.indices.evaluate, ]

  test.form <- top_models$formula[which_model]

  # Train the non-ordinal multinomial regression model
  test <- nnet::multinom(test.form,
                         data = Train.set,
                         maxit = 2000,
                         trace = FALSE)

  # Compute train accuracy
  train_results <- mod.info(test, Train.set, F, TRUE)

  train.acc <- train_results$accuracy_print

  # Classification table plot for training set
  p1_result <- ct_plot(train_results$class.table,
                       plot.title = paste0('Training Set - model #', as.character(which_model), collapse = ''),
                       conformation = '')

  p1 <- p1_result$plot

  # Prediction probability heatmap for training set
  p2 <- patchwork_prob_heatmap(test,
                               Train.set,
                               plot.title = '',
                               conformation = '',
                               show_legend = F)

  # Compute test accuracy
  test_results <- mod.info(test, Test.set, F, TRUE)

  test.acc <- test_results$accuracy_print

  # Classification table plot for test set
  p3_result <- ct_plot(test_results$class.table,
                       plot.title = 'Test Set',
                       conformation = '')

  p3 <- p3_result$plot

  # Prediction probability heatmap for test set
  p4 <- patchwork_prob_heatmap(test,
                               Test.set,
                               plot.title = '',
                               conformation = '',
                               show_legend = F)

  # Display the combined plots
  combined_ct_plot <- combine_plots_with_legend(
    list(p1, p3), 2,
    title = paste(title.of.analysis, '- Confusion Matrices', collapse = ' '),
    subtitle = top_models$dataset[which_model]
  )

  suppressWarnings(
    combined_hm_plot <- combine_plots_with_legend(
      list(p2, p4), 2,
      title = paste(title.of.analysis, '- Probability Heatmaps', collapse = ' '),
      subtitle = top_models$dataset[which_model]
    )
  )
  # Create validation results
  validation_result <- data.frame(
    'Inhibition Probability' = round(predict(test, valid.set, 'probs'), 2) * 100,
    'Inhibition Class' = predict(test, valid.set, 'class'),
    'Experimental' = valid.set$class,
    row.names = row.names(valid.set)
  )

  # Create validation plot
  valid_plot <- validation_table(validation_result, cell_height = 0.2)

  # Only return the results if requested
  return(list(
      combined_ct_plot = combined_ct_plot,
      combined_hm_plot = combined_hm_plot,
      validation_plot = valid_plot,
      validation_result = validation_result,
      fitted_model = test
    ))

}
