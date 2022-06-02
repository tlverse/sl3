#' Process Data
#'
#' A function called upon creating a task that uses the data provided to the
#' task in order to process the covariates and identify missingness in the
#' outcome. See parameters and details for more information.
#'
#' @details If the data provided to the task contains missing covariate values,
#' then a few things will happen. First, for each covariate with missing values,
#' if the proportion of missing values is greater than
#' \code{getOption("sl3.max_p_missing")}, the covariate will be dropped. (The
#' default option \code{"sl3.max_p_missing"} is 0.5 and it can be modified to
#' say, 0.75, by setting \code{options("sl3.max_p_missing" = 0.75)}). Also,
#' for each covariate with missing values that was not dropped, a so-called
#' "missingness indicator" (that takes the name of the covariate with prefix
#' "delta_") will be added as an additional covariate. The missingness
#' indicator will take a value of 0 if the covariate value was missing and 1
#' if not. Also, imputation will be performed for each covariate with missing
#' values: continuous covariates are imputed with the median, and discrete
#' covariates are imputed with the mode. This coupling of imputation and
#' missingness indicators removes the missing covariate values, while
#' preserving the pattern of missingness, respectively. To avoid this default
#' imputation, users can perform imputation on their analytic dataset before
#' supplying it to \code{\link{make_sl3_Task}}. We generally recommend the
#' missingness indicators be added regardless of the imputation strategy,
#' unless missingness is very rare.
#'
#' This function also coverts any character covariates to factors, and one-hot
#' encodes factor covariates.
#'
#' Lastly, if the \code{outcome} is supplied in creating the
#' \code{\link{sl3_Task}} and if missing outcome values are detected in
#' \code{data}, then a warning will be thrown. If
#' \code{drop_missing_outcome = TRUE} then observations with missing outcomes
#' will be dropped.
#'
#' @param data A \code{data.table} containing the analytic dataset. In
#'  creating the \code{\link{sl3_Task}}, the \code{data} passed to the task is
#'  supplied for this argument.
#' @param nodes A list of character vectors for \code{covariates},
#'  \code{outcome}, \code{id}, \code{weights}, and \code{offset}, which is
#'  generated when creating the \code{\link{sl3_Task}} if not already specified
#'  as an argument to \code{make_sl3_Task}.
#' @param column_names A named list of column names in the data, which is
#'  generated when creating the \code{\link{sl3_Task}} if not already specified
#'  as an argument to \code{make_sl3_Task}.
#' @param flag Logical (default \code{TRUE}) indicating whether to notify the
#'  user when there are outcomes that are missing, which can be modified when
#'  creating the \code{\link{sl3_Task}} by setting \code{flag = FALSE}.
#' @param drop_missing_outcome Logical (default \code{FALSE}) indicating
#'  whether to drop observations with missing outcomes, which can be modified
#'  when creating the \code{\link{sl3_Task}} by setting
#'  \code{drop_missing_outcome = TRUE}.
#'
#' @importFrom imputeMissings impute
#'
#' @return A list of processed data, nodes and column names
#'
#' @export
process_data <- function(data, nodes, column_names, flag = TRUE,
                         drop_missing_outcome = FALSE) {

  # force a copy so we can mutate data in place w/o distrupting a user's data
  if (inherits(data, "data.table")) {
    data <- data.table::copy(data)
  } else {
    data <- as.data.table(data)
  }

  all_nodes <- unlist(nodes)


  if (length(all_nodes) == 0) {
    return(list(data = data, nodes = nodes, column_names = column_names))
  }
  node_columns <- unlist(column_names[all_nodes])
  covariates_columns <- unlist(column_names[nodes$covariates])
  outcome_columns <- unlist(column_names[nodes$outcome])

  factorized <- FALSE
  dropped <- FALSE
  imputed <- FALSE

  # process characters
  is_character <- which(data[, sapply(.SD, is.character), .SDcols = node_columns])
  char_cols <- node_columns[is_character]
  char_vars <- all_nodes[is_character]
  if (length(char_cols) > 0) {
    warning(sprintf(
      "Character variables found: %s;\nConverting these to factors",
      paste0(char_vars, collapse = ", ")
    ))

    # convert data
    for (char_col in char_cols) {
      set(data, , char_col, as.factor(unlist(data[, char_col, with = FALSE])))
    }
    factorized <- TRUE
  }

  # process missing
  has_missing <- data[, sapply(.SD, function(x) any(is.na(x))), .SDcols = node_columns]
  miss_cols <- node_columns[has_missing]
  miss_vars <- all_nodes[has_missing]

  missing_Y <- any(nodes$outcome %in% miss_vars)
  missing_covar_cols <- intersect(miss_cols, covariates_columns)
  missing_covar_vars <- intersect(miss_vars, nodes$covariates)

  if (length(miss_cols) > 0) {
    if (missing_Y && drop_missing_outcome) {
      if (flag) {
        warning("Missing outcome data detected: dropping outcomes.")
      }
      keep_rows <- stats::complete.cases(data[, outcome_columns, with = FALSE])
      data <- data[keep_rows, ]
    }

    if (length(missing_covar_cols) > 0) {
      warning(sprintf(
        "Imputing missing values and adding missingness indicators for the following covariates with missing values: %s. See documentation of the process_data function for details.",
        paste0(missing_covar_cols, collapse = ", ")
      ))
      # make indicators and add to data
      missing_indicators <- data[, lapply(.SD, function(x) as.numeric(!is.na(x))),
        .SDcols = missing_covar_cols
      ]

      missing_indicator_cols <- sprintf("delta_%s", missing_covar_cols)
      missing_indicator_vars <- sprintf("delta_%s", missing_covar_vars)
      setnames(missing_indicators, missing_indicator_cols)
      set(data, , missing_indicator_cols, missing_indicators)

      # add inidicators to column map and covariate list
      column_names[missing_indicator_vars] <- missing_indicator_cols
      nodes$covariates <- c(nodes$covariates, missing_indicator_vars)
    }
    # impute covariates
    imputed <- impute(data[, missing_covar_cols, with = FALSE], flag = FALSE)

    # update data
    set(data, , missing_covar_cols, imputed)
  }

  na_Y <- (!is.null(nodes$outcome) && any(is.na(data[, outcome_columns, with = F])))
  if (na_Y && flag) {
    warning("Missing outcome data detected. This is okay for prediction, but will likely break training. \n You can drop observations with missing outcomes by setting drop_missing_outcome=TRUE in make_sl3_Task.")
  }
  list(data = data, nodes = nodes, column_names = column_names)
}
