#' @importFrom imputeMissings impute
process_data <- function(data, nodes, column_names, flag = TRUE, drop_missing_outcome = FALSE) {

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
      "Character covariates found: %s;\nConverting these to factors",
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
      warning("Missing outcome data detected: dropping outcomes.")
      keep_rows <- stats::complete.cases(data[, outcome_columns, with = FALSE])
      data <- data[keep_rows, ]
    }

    if (length(missing_covar_cols) > 0) {
      warning("Missing covariate data detected: imputing covariates.")
      if (flag) {
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

    missing_Y <- (!is.null(nodes$outcome) && any(is.na(data[, outcome_columns, with = FALSE])))
    imputed <- TRUE
  }

  if (missing_Y) {
    warning("Missing outcome data detected. This is okay for prediction, but will likely break training. \n You can drop observations with missing outcomes by setting drop_missing_outcome=TRUE in make_sl3_Task.")
  }


  list(data = data, nodes = nodes, column_names = column_names)
}
