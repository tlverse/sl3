# the functions re-index folds to account for new long data structure
reindex <- function(index, fold_index) {
  which(index %in% fold_index)
}

reindex_fold <- function(fold, index) {
  new_fold <- origami::make_fold(
    fold$v,
    reindex(index, fold$training_set),
    reindex(index, fold$validation_set)
  )
}


#' Generate A Pooled Hazards Task from a Failure Time (or Categorical) Task
#'
#' @param task A \code{\link{sl3_Task}} where the outcome is failure time.
#' @param trim If \code{true}, remove entries after failure time for each
#'  observation.
#'
#' @export
#
pooled_hazard_task <- function(task, trim = TRUE) {
  # extract outcome levels
  outcome_levels <- task$outcome_type$levels
  n_levels <- length(outcome_levels)
  level_index <- seq_len(n_levels)

  # repeat task across levels of the outcome
  underlying_data <- data.table::copy(task$internal_data$raw_data)
  row_index <- task$row_index
  if (!is.null(row_index)) {
    underlying_data <- underlying_data[row_index]
  }

  # force ids to exist so that we can use them after repeating the task
  id_name <- paste0(UUIDgenerate(), "_id")
  data.table::set(underlying_data, j = id_name, value = task$id)
  column_names <- c(task$column_names, list(id = id_name))

  # generate repeated task
  index <- rep(seq_len(task$nrow), n_levels)
  repeated_data <- underlying_data[index]
  new_folds <- origami::id_folds_to_folds(task$folds, index)

  repeated_task <- task$next_in_chain(
    column_names = column_names,
    data = repeated_data, id = "id",
    folds = new_folds
  )

  # make bin indicators
  bin_number <- rep(level_index, each = task$nrow)
  outcome <- repeated_task$Y
  outcome_level <- match(outcome, outcome_levels)
  in_bin <- as.numeric(outcome_level == bin_number)

  # add new columns for indicator (new outcome) and bin index (as covariate)
  new_columns <- repeated_task$add_columns(data.table(
    bin_number = bin_number,
    in_bin = in_bin
  ))
  new_covariates <- c(task$nodes$covariates, "bin_number")
  hazard_task <- repeated_task$next_in_chain(
    column_names = new_columns,
    outcome = "in_bin",
    covariates = new_covariates
  )
  if (!trim) {
    return(hazard_task)
  }

  # trim entries for observations that are in previous bins
  subset_index <- which(bin_number <= outcome_level)
  trimmed_hazard_task <- hazard_task[subset_index]
  return(trimmed_hazard_task)
}
