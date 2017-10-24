#if warning is in ignoreWarningList, ignore it; otherwise post it as usual
SuppressGivenWarnings <- function(expr, warningsToIgnore) {
  h <- function (w) {
    if (w$message %in% warningsToIgnore) invokeRestart("muffleWarning")
  }
  withCallingHandlers(expr, warning = h)
}

################################################################################

GetWarningsToSuppress <- function(update.step=FALSE) {
  warnings.to.suppress <- c("glm.fit: fitted probabilities numerically 0 or 1 occurred",
                            "prediction from a rank-deficient fit may be misleading",
                            "non-integer #successes in a binomial glm!",
                            "the matrix is either rank-deficient or indefinite",
                            "glm.fit: algorithm did not converge")
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
#
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
#' @param args A \code{list} of function arguments to use
#' @param other_valid A \code{list} of function arguments names that are valid, but not formals of \code{fun}
#' @param keep_all A \code{boolean} don't drop arguments, even if they aren't matched in either the function prototype or other_valid
#' @keywords internal#
call_with_args <- function(fun, args, other_valid=list(), keep_all=FALSE) {
  if(!keep_all){
    formal_args <- names(formals(fun))
    all_valid <- c(formal_args, other_valid)
    args <- args[which(names(args)%in%all_valid)]
  }
  do.call(fun, args)
}
################################################################################

#' Replace an argument in \code{mainArgs} if it also appears in \code{userArgs}.
#' Add any argument from \code{userArgs} that appears in \code{formals(fun)} of
#' the function \code{fun}.
#'
#' @keywords internal
#
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

true_obj_size <- function(obj) {
    length(serialize(obj, NULL))
}

################################################################################

reduce_fit_test <- function(learner_fit) {
    # given a learner fit, sequentially drop components from the internal fit object,
    # keeping track of which components are needed for prediction

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
        try({
            reduced_predict <- reduced$predict()
        }, silent = TRUE)
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

subset_dt_cols = function(dt, cols) {
  #setDF(dt)
  #subset = dt[,cols, drop = FALSE]
  #setDT(subset)
  #setDT(dt)
  #return(subset)
  return(dt[, cols, with = FALSE, drop = FALSE])
}

################################################################################
#' Get all args of parent call (both specified and defaults) as list
#'
#' @return a list of all for the parent function call
#' @export
args_to_list <- function(){
  parent <- sys.parent()
  call <- sys.call(parent)
  fn <- sys.function(parent)

  # get specified args
  expanded <- match.call(fn, call, envir=parent.frame(2L))
  args <- as.list(expanded[-1])

  # get default args
  all_args <- formals(fn)

  # drop dots from formals if it exists
  all_args$`...` <- NULL

  # add in specified args
  all_args[names(args)] <- args

  # evaluate args
  num_args <- length(all_args)
  for(i in seq_len(num_args)){
    if(!is.null(all_args[[i]])){
      all_args[[i]]=eval(all_args[[i]], envir=all_args, enclos=parent.frame(2L))
    }
  }
  # evaled <- lapply(all_args, eval, envir=parent.frame(2L))

  return(all_args)
}



#' dim that works for vectors too
#' 
#' \code{safe_dim} tries to get dimensions from \code{dim} and falls back on \code{length} if \code{dim} returns \code{NULL}
#' @param x the object to get dimensions from
#' @export
safe_dim <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    d <- length(x)
  }
  
  return(d)
}