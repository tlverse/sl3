impute_median <- function(x) {
  value <- median(as.numeric(x[!is.na(x)]))
  x[is.na(x)] <- value
  x
}

#' @importFrom stats aggregate
impute_mode <- function(x) {
  count_df <- aggregate(count ~ x, data = data.frame(count = 1, x = x), sum)
  value <- count_df$x[which.max(count_df$count)]
  x[is.na(x)] <- value
}

impute_by_type <- function(x) {
  if (is.factor(x) || is.character(x)) {
    return(impute_mode(x))
  } else {
    return(impute_median(x))
  }
}

sl3_process_missing <- function(task, drop_missing_outcome = TRUE, max_p_missing = 0.5) {
  covars <- task$nodes$covariates
  X <- task$X
  Y <- task$Y


  # median impute the covariates and build indicators
  p_missing <- sapply(X, function(x) mean(is.na(x)))


  # nodes that are already complete
  no_missing <- names(p_missing[p_missing == 0])
  processed <- X[, no_missing, with = FALSE]

  # nodes to impute
  to_impute <- names(p_missing[(0 < p_missing) & (p_missing < max_p_missing)])
  if (length(to_impute) > 0) {
    missing_indicators <- X[, lapply(.SD, function(x) as.numeric(is.na(x))), .SDcols = to_impute]
    missing_names <- sprintf("delta_%s", to_impute)
    setnames(missing_indicators, missing_names)

    imputed <- X[, lapply(.SD, impute_by_type), .SDcols = to_impute]
    processed <- cbind(processed, imputed, missing_indicators)
  } else {
    missing_names <- c()
  }

  # nodes with too much missingness
  to_drop <- names(p_missing[(max_p_missing < p_missing)])

  # drop rows where there is outcome missingness
  # TODO: this wipes folds
  if (drop_missing_outcome) {
    missing_Y <- is.na(Y)
    task <- task[!missing_Y]
    processed <- processed[!missing_Y]
  }

  new_columns <- task$add_columns(processed)
  processed_task <- task$next_in_chain(column_list = new_columns)



  return(processed_task)
}
