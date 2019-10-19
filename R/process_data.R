#' @importFrom stats median
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
  return(x)
}

impute_by_type <- function(x) {
  if (is.factor(x) || is.character(x)) {
    return(impute_mode(x))
  } else {
    return(impute_median(x))
  }
}

sl3_process_missing <- function(task, drop_missing_outcome = FALSE,
                                max_p_missing =
                                  getOption("sl3.max_p_missing")) {
  if (drop_missing_outcome) {
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
    missing_indicators <- X[, lapply(.SD, function(x) as.numeric(is.na(x))),
      .SDcols = to_impute
    ]
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
  processed_task <- task$next_in_chain(
    covariates = covariates,
    column_names = new_columns
  )
  return(processed_task)
}


### new ###

#' @importFrom imputeMissings impute
process_data <- function(data, covariates, outcome = NULL, flag = TRUE, save_flag_cols = TRUE, drop_missing_outcome = FALSE) {
  # for efficiency, character & missing processing are not seperated
  data <- data.table(data)
  covars <- data[, colnames(data) %in% covariates, with=FALSE]
  origin_cols <- colnames(data)
  convert_cols <- vector()
  
  factorized = FALSE
  dropped = FALSE
  imputed = FALSE
  
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
    convert_cols = sapply(char_cols, function(s) paste0(s, "_factorized"))
    names(convert_cols) = char_cols
    
    factorized = TRUE
  }
  
  # process missing
  miss_cols <- names(which(sapply(
    covars,
    function(l) TRUE %in% is.na(l)
  ) == TRUE))
  missing_Y <- (!is.null(outcome) && any(is.na(data[, outcome, with=FALSE])))
  if (length(miss_cols) > 0) {
    if (missing_Y && drop_missing_outcome) {
      warning("Missing Outcome Data Found. Dropping outcomes.")
      keep_cols = complete.cases(data[, outcome, with=FALSE])
      covars = covars[keep_cols, ]
      data = data[keep_cols, ]
      
      dropped = TRUE
    }
    warning("Missing Covariate Data Found. Imputing covariates.")
    # convert data
    covars = impute(covars, flag = flag)
    # convert names
    for (s in miss_cols) {
      if (s %in% char_cols) {
        convert_cols[s] = paste0(convert_cols[s], "_imputed")
      } else {
        convert_cols = setNames(c(convert_cols, paste0(s, "_imputed")), 
                                c(names(convert_cols), s))
      }
    }
    
    missing_Y <- (!is.null(outcome) && any(is.na(data[, outcome, with=FALSE])))
    imputed = TRUE
  }
  
  if (missing_Y) {
    warning("Missing Outcome Data Found. This is okay for prediction, but will likely break training. \n You can drop observations with missing outcomes by setting drop_missing_outcome=TRUE in make_sl3_Task.")
  }
  
  # add new columns to data
  if (length(convert_cols) > 0) {
    `for`(i, 1:length(convert_cols), `=`(data[[convert_cols[i]]], 
                                         covars[[names(convert_cols)[i]]]))
    
    if (length(covars) > length(covariates)) {
      flag_cols <- colnames(covars)[!(colnames(covars) %in% covariates)]
      `for`(s, flag_cols, `=`(data[[s]], covars[[s]]))
      `if`(save_flag_cols, `=`(covariates, c(covariates, flag_cols)))
    }
  }
  
  # process column mapping
  map <- as.list(origin_cols)
  names(map) <- map
  
  if (factorized || imputed) {
    `for`(s, names(convert_cols), `=`(map[s], convert_cols[s]))
    `if`(imputed && flag, `=`(map, setNames(c(map, flag_cols), c(names(map), flag_cols))))
  }
  
  list(data = data, covariates = covariates, map = map)
}