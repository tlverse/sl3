#' @importFrom imputeMissings impute
process_data <- function(data, covariates, outcome = NULL, column_names = NULL, flag = TRUE, save_flag_cols = TRUE, drop_missing_outcome = FALSE) {
  # for efficiency, character & missing processing are not seperated
  if (!is.null(column_names)) {
    covariates_data = sapply(covariates, function(s) column_names[[s]])
    outcome_data = sapply(outcome, function(s) column_names[[s]])
  } else {
    covariates_data = covariates
    outcome_data = outcome
  }
  
  data <- data.table(data)
  covars <- data[, colnames(data) %in% covariates_data, with = FALSE]
  origin_cols <- colnames(data)
  convert_cols <- vector()

  factorized <- FALSE
  dropped <- FALSE
  imputed <- FALSE

  # process characters
  char_cols <- names(which(sapply(covars, data.class) == "character"))
  if (length(char_cols) > 0) {
    warning(sprintf(
      "Character covariates found: %s;\nConverting these to factors",
      paste0(char_cols, collapse = ", ")
    ))
    # convert data
    `for`(s, char_cols, `=`(covars[[s]], factor(covars[[s]])))
    # convert names
    convert_cols <- sapply(char_cols, function(s) paste0(s, "_factorized"))
    names(convert_cols) <- char_cols

    factorized <- TRUE
  }

  # process missing
  miss_cols <- names(which(sapply(
    covars,
    function(l) TRUE %in% is.na(l)
  ) == TRUE))
  missing_Y <- (!is.null(outcome) && any(is.na(data[, outcome_data, with = FALSE])))
  if (length(miss_cols) > 0) {
    if (missing_Y && drop_missing_outcome) {
      warning("Missing Outcome Data Found. Dropping outcomes.")
      keep_cols <- complete.cases(data[, outcome_data, with = FALSE])
      covars <- covars[keep_cols, ]
      data <- data[keep_cols, ]

      dropped <- TRUE
    }
    warning("Missing Covariate Data Found. Imputing covariates.")
    # convert data
    covars <- impute(covars, flag = flag)
    # convert names
    for (s in miss_cols) {
      if (s %in% char_cols) {
        convert_cols[s] <- paste0(convert_cols[s], "_imputed")
      } else {
        convert_cols <- setNames(
          c(convert_cols, paste0(s, "_imputed")),
          c(names(convert_cols), s)
        )
      }
    }

    missing_Y <- (!is.null(outcome) && any(is.na(data[, outcome_data, with = FALSE])))
    imputed <- TRUE
  }

  if (missing_Y) {
    warning("Missing Outcome Data Found. This is okay for prediction, but will likely break training. \n You can drop observations with missing outcomes by setting drop_missing_outcome=TRUE in make_sl3_Task.")
  }

  # add new columns to data
  if (length(convert_cols) > 0) {
    `for`(i, 1:length(convert_cols), `=`(
      data[[convert_cols[i]]],
      covars[[names(convert_cols)[i]]]
    ))

    if (length(covars) > length(covariates)) {
      flag_cols <- colnames(covars)[!(colnames(covars) %in% covariates_data)]
      `for`(s, flag_cols, `=`(data[[s]], covars[[s]]))
      `if`(save_flag_cols, `=`(covariates, c(covariates, flag_cols)))
    }
  }

  # process column mapping
  if (!is.null(column_names)) {
    map <- column_names
  } else {
    map <- as.list(origin_cols)
    names(map) <- map
  }

  if (factorized || imputed) {
    `for`(s, names(convert_cols), `=`(map[map == s], convert_cols[s]))
    `if`(imputed && flag, `=`(map, setNames(c(map, flag_cols), c(names(map), flag_cols))))
  }

  list(data = data, covariates = covariates, map = map)
}
