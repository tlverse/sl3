#' Truncates predictions to ensure loss function is bounded.
#'
#' @param preds A \code{numeric} vector of predictions to to be bounded.
#' @param bounds Either a \code{numeric} vector of length two, giving the
#'  closed interval (lower, upper), or just a lower bound. In the latter case,
#'  the upper bound is computed as (1 - lower). The default is 0.001.
#'
#' @return Truncated predictions.
#'
#' @keywords internal
bound <- function(preds, bounds = 0.001) {
  lower <- bounds[[1]]
  if (length(bounds) > 1) {
    upper <- bounds[[2]]
  } else {
    upper <- 1 - lower
  }
  preds_bounded <- pmin(pmax(preds, lower), upper)
  return(preds_bounded)
}

################################################################################

# if warning is in ignoreWarningList, ignore it; otherwise post it as usual
SuppressGivenWarnings <- function(expr, warningsToIgnore) {
  h <- function(w) {
    if (w$message %in% warningsToIgnore) invokeRestart("muffleWarning")
  }
  withCallingHandlers(expr, warning = h)
}

################################################################################

GetWarningsToSuppress <- function(update.step = FALSE) {
  warnings.to.suppress <- c(
    paste(
      "glm.fit: fitted probabilities numerically 0",
      "or 1 occurred"
    ),
    paste(
      "prediction from a rank-deficient fit may be",
      "misleading"
    ),
    "non-integer #successes in a binomial glm!",
    "the matrix is either rank-deficient or indefinite",
    "glm.fit: algorithm did not converge"
  )
  return(warnings.to.suppress)
}

################################################################################

#' Streamline Function Arguments
#'
#' Reduce a list of function argsuments by taking a function body and returning
#' only the arguments that belong to the function signature.
#'
#' @param Args A \code{list} of function arguments to be streamlined.
#' @param fun A \code{function} whose signature will be used to reduce the
#'  arguments passed in.
#'
#' @keywords internal
keep_only_fun_args <- function(Args, fun) {
  keepArgs <- intersect(names(Args), names(formals(fun)))
  # captures optional arguments given by user
  if (length(keepArgs) > 0) {
    Args <- Args[keepArgs]
  } else {
    Args <- NULL
  }
  return(Args)
}

################################################################################

#' Call with filtered argument list
#'
#' Call a function with a list of arguments, eliminating any that aren't
#' matched in the function prototype
#'
#' @param fun A \code{function} whose signature will be used to reduce the
#' @param args A \code{list} of function arguments to use.
#' @param other_valid A \code{list} of function arguments names that are valid,
#'   but not formals of \code{fun}.
#' @param keep_all A \code{logical} don't drop arguments, even if they aren't
#'   matched in either the function prototype or other_valid.
#' @param silent A \code{logical} indicating whether to pass \code{message}s
#'  when arguments not found in \code{formals} are passed to \code{fun}.
#' @param ignore A \code{character} vector indicating which arguments should be dropped
#'
#' @keywords internal
call_with_args <- function(fun, args, other_valid = list(), keep_all = FALSE,
                           silent = FALSE, ignore = c()) {
  
  # drop ignore args
  args <- args[!(names(args)%in%ignore)]
  if (!keep_all) {
    # catch arguments to be kept
    formal_args <- names(formals(fun))
    all_valid <- c(formal_args, other_valid)

    # find invalid arguments based on combination of formals and other_valid
    invalid <- names(args)[which(!(names(args) %in% all_valid))]

    # subset arguments to pass
    args <- args[which(names(args) %in% all_valid)]

    # return warnings when dropping arguments
    if (!silent & length(invalid) > 0) {
      message(sprintf(
        "Learner called function %s with unknown args: %s. These will be dropped.\nCheck the params supported by this learner.",
        as.character(substitute(fun)), paste(invalid, collapse = ", ")
      ))
    }
  }
  do.call(fun, args)
}

################################################################################

#' Replace an argument in \code{mainArgs} if it also appears in \code{userArgs}
#'
#' Add any argument from \code{userArgs} that appears in \code{formals(fun)} of
#' the function \code{fun}.
#'
#' @keywords internal
replace_add_user_args <- function(mainArgs, userArgs, fun) {
  replaceArgs <- intersect(names(mainArgs), names(userArgs))
  # captures main arguments that were overridden by user
  if (length(replaceArgs) > 0) {
    mainArgs[replaceArgs] <- userArgs[replaceArgs]
    userArgs[replaceArgs] <- NULL
  }
  newArgs <- intersect(names(formals(fun)), names(userArgs))
  # captures any additional args given by user that are not in mainArgs
  if (length(newArgs) > 0) {
    mainArgs <- c(mainArgs, userArgs[newArgs])
  }
  return(mainArgs)
}

################################################################################

#' Estimate object size using serialization
#'
#' Attempts to get a better estimate of object size than that returned by
#' \code{\link{object.size}}
#'
#' @param obj the object to get the size of
#'
#' @return the size of \code{obj} in bytes
#'
#' @keywords internal
true_obj_size <- function(obj) {
  length(serialize(obj, NULL))
}

################################################################################

#' Drop components from learner fits
#'
#' Given learner fit, sequentially drop components from internal fit object,
#' keeping track of which components are needed for prediction.
#'
#' @param learner_fit A \code{Lrnr_*} object, inheriting from
#'  \code{\link{Lrnr_base}}, that has been trained.
#'
#' @keywords internal
reduce_fit_test <- function(learner_fit) {
  # learner_fit = glm_fit
  original_fit <- learner_fit$fit_object
  task <- learner_fit$training_task

  original_size <- true_obj_size(original_fit)
  original_predict <- learner_fit$predict()

  components <- names(original_fit)
  reduced <- learner_fit$clone()
  reduced_fit <- original_fit
  for (component in components) {
    backup <- reduced_fit[component]
    reduced_fit[component] <- NULL
    reduced$set_train(reduced_fit, task)
    reduced_predict <- NULL
    try(
      {
        reduced_predict <- reduced$predict()
      },
      silent = TRUE
    )
    if (!identical(original_predict, reduced_predict)) {
      reduced_fit[component] <- backup
    }
  }
  reduced_components <- names(reduced_fit)
  reduced_size <- true_obj_size(reduced_fit)
  reduction <- as.numeric(1 - reduced_size / original_size)
  try()
}

################################################################################

#' Subset data.table columns
#'
#' @param dt A \code{\link[data.table]{data.table}}.
#' @param cols Columns to subset.
#'
#' @return Subset of input \code{\link[data.table]{data.table}}.
#'
#' @keywords internal
subset_dt_cols <- function(dt, cols) {
  return(dt[, cols, with = FALSE, drop = FALSE])
}

################################################################################

#' Get all arguments of parent call (both specified and defaults) as list
#'
#' @return A \code{list} of all arguments for the parent function call.
#'
#' @export
args_to_list <- function() {
  # get calling environment, call object, and function to call
  parent <- sys.parent()
  call <- sys.call(parent)
  fn <- sys.function(parent)

  # get specified args
  expanded <- match.call(
    definition = fn,
    call = call,
    envir = parent.frame(n = 2L)
  )
  args <- as.list(expanded[-1])

  # get default args
  all_args <- formals(fn)

  # drop dot args from formals if it exists
  all_args$`...` <- NULL

  # add in specified args
  all_args[names(args)] <- args

  # evaluate args
  for (i in seq_along(all_args)) {
    if (!is.null(all_args[[i]])) {
      evaled <- eval(
        expr = all_args[[i]],
        envir = all_args,
        enclos = parent.frame(n = 2L)
      )
      all_args[i] <- list(evaled)
    }
  }
  return(all_args)
}

################################################################################

#' dim that works for vectors too
#'
#' \code{safe_dim} tries to get dimensions from \code{dim} and falls back on
#' \code{length} if \code{dim} returns \code{NULL}
#'
#' @param x the object to get dimensions from
#'
#' @export
safe_dim <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    d <- length(x)
  }
  return(d)
}

################################################################################

#' Generate a file containing a template \code{sl3} Learner
#'
#' Generates a template file that can be used to write new \pkg{sl3} Learners.
#' For more information, see the
#' \href{../docs/articles/custom_lrnrs.html}{Defining New Learners} vignette.
#'
#' @param file the path where the file should be written
#'
#' @return the return from \code{\link{file.copy}}. \code{TRUE} if writing the
#'  template was successful.
#'
#' @export
write_learner_template <- function(file) {
  template_file <- system.file(
    "templates/Lrnr_template.R",
    package = "sl3",
    mustWork = TRUE
  )
  file.copy(template_file, file)
}

################################################################################

# Miscellaneous setting that was the sole contents of a file "Untitled.R" prior
# to commit b8cc1e5. Preserved here.
list_name <- list()
