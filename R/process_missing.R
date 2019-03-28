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

sl3_process_missing <- function(task, drop_missing_outcome = FALSE, max_p_missing = 0.5) {
  
  if(drop_missing_outcome){
    task <- task[!is.na(task$Y)]
  }
  
  covars <- task$nodes$covariates
  X <- task$X


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

  new_columns <- task$add_columns(processed)
  covariates <- c(task$nodes$covariates, missing_names)
  processed_task <- task$next_in_chain(covariates = covariates ,column_names = new_columns)



  return(processed_task)
}
