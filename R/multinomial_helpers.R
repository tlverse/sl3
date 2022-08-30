get_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  } else {
    return(sort(unique(x)))
  }
}

#' Pack multidimensional predictions into a vector (and unpack again)
#' @rdname pack_predictions
#' @param pred_matrix a matrix of prediciton values
#' @export
pack_predictions <- function(pred_matrix) {
  packed <- apply(pred_matrix, 1, function(row) {
    packed_row <- list(row)
    class(packed_row) <- "packed_predictions"
    return(packed_row)
  })
  return(as.matrix(packed))
}

#' @rdname pack_predictions
#' @param x a packed prediction list
#' @export
unpack_predictions <- function(x) {
  do.call(rbind, lapply(x, `[[`, 1))
}

print.packed_predictions <- function(x) {
  print(unlist(x))
}

normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}


#' Convert Factors to indicators
#'
#' replicates the functionality of model.matrix, but faster
#'
#' @param x the factor to expand
#' @param ind_ref_mat a matrix used for expansion, if NULL generated automatically
#' @rdname factors_to_indicators
#' @export
factor_to_indicators <- function(x, ind_ref_mat = NULL) {
  x_vals <- get_levels(x)
  
  if (length(x_vals) == 1L) {
    stop("Categorical variables must have more than one levels")
  }
  
  if (is.null(ind_ref_mat)) {
    ind_ref_mat <- sapply(x_vals[-1], function(x_val) as.numeric(x_val == x_vals))
  }

  ind_mat <- ind_ref_mat[as.numeric(x), , drop = FALSE]
  return(ind_mat)
}

#' Convert Factors to indicators
#'
#' Replicates the functionality of \code{model.matrix}, but faster
#'
#' @param dt the dt to expand
#' @rdname factors_to_indicators
#' @export
dt_expand_factors <- function(dt) {
  raw <- lapply(dt, function(dt_col) {
    if (is.factor(dt_col)) {
      fi <- factor_to_indicators(dt_col)
      colnames(fi) <- make.names(colnames(fi))
      return(fi)
    } else {
      return(dt_col)
    }
  })
  as.data.table(raw)
}

#' Predict Class from Predicted Probabilities
#'
#' Returns the most likely class label for each row of predicted class
#' probabilities
#'
#' @param predictions the nxc matrix where each row are predicted probabilities
#'  for one observation for each of c classes.
#' @return a vector of length n, the predicted class labels as a factor variable
#' @export
predict_classes <- function(predictions) {
  class_names <- colnames(predictions)
  pred_classes <- class_names[apply(predictions, 1, which.max)]
  pred_classes <- factor(pred_classes, levels = class_names)

  return(pred_classes)
}
