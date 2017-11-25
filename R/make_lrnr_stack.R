#' Make a stack of sl3 learners
#'
#' Produce a stack of learners by passing in a list with IDs for the learners.
#' The resultant stack of learners may then be processed as normal.
#'
#' @param lrnrs_list A \code{list} containing IDs for each of the learners to be
#'   built into the \code{Stack} to be produced. The IDs must be the full names
#'   of the learners. For example, to build a stack from \code{Lrnr_mean} and
#'   \code{Lrnr_xgboost}, simply input \code{list("Lrnr_mean", "Lrnr_xgboost")}
#'   for this argument.
#' @param lrnrs_args A \code{list} of \code{list}s containing extra parameters
#'   to be used in constructing learners that end up in the \code{Stack}. If the
#'   default is not used, then the number of \code{list}s must be equivalent to
#'   the number of learners in the argument \code{lrnrs_list} above. In order to
#'   pass in no extra arguments for a learner yet provide extra arguments for
#'   other learners, construct a list containing \code{NA} for the former. See
#'   the examples below for details.
#'
#' @return An \code{sl3} \code{Stack} consisting of the learners passed in with
#'   the \code{list} argument to this function. This \code{Stack} has all of the
#'   standard methods associated with such objects.
#'
#' @export
#'
#' @examples
#' # constructing learners with default settings
#' sl_lrnrs_list <- list("Lrnr_mean", "Lrnr_xgboost")
#' sl_stack <- make_learner_stack(sl_lrnrs_list)
#'
#' # constructing learners with arguments passed in
#' sl_lrnrs_list <- list("Lrnr_mean", "Lrnr_condensier")
#' lrnrs_args_list <- list(list(NA), list(nbins = 5, bin_method = "equal.len",
#'                                        pool = FALSE))
#' sl_stack <- make_learner_stack(sl_lrnrs_list, lrnrs_args_list)
#
make_learner_stack <- function(lrnrs_list,
                               lrnrs_args = list(list(NA))) {
  # enforce that input is a list
  stopifnot(class(lrnrs_list) == "list")

  # pad lrnrs_args arguments with NULL if left as default
  if ((length(lrnrs_args) < length(lrnrs_list)) &
      is.na(unlist(lrnrs_args[[1]]))) {
    lrnrs_args <- as.list(rep(NA, length(lrnrs_list)))
  }

  # add standard prefix to learners list
  lrnrs_list_in <- list()
  for (i in seq_along(lrnrs_list)) {
    lrnr_eval <- list(parse(text = lrnrs_list[[i]]))
    names(lrnr_eval) <- "learner_class"
    # pass in learner arguments if provided
    if (any(!is.na(unlist(lrnrs_args[[i]])))) {
      make_lrnr_args <- unlist(list(lrnr_eval, lrnrs_args[[i]],
                                    recursive = FALSE))
      lrnrs_list_in[[i]] <- do.call(make_learner, make_lrnr_args)
    } else {
      lrnrs_list_in[[i]] <- make_learner(lrnrs_list[[i]])
    }
  }
  # generate output Stack object and return model stack
  out <- make_learner(Stack, lrnrs_list_in)
  return(out)
}

