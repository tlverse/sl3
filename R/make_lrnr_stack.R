#' Make a stack of sl3 learners
#'
#' Produce a stack of learners by passing in a list with IDs for the learners.
#' The resultant stack of learners may then be used as normal.
#'
#' @param ... Each argument is a list that will be passed to
#'  \code{\link{make_learner}}
#'
#' @return An \code{sl3} \code{Stack} consisting of the learners passed in as
#'  arguments the \code{list} argument to this function. This \code{Stack} has
#'  all of the standard methods associated with such objects.
#'
#' @export
#'
#' @examples
#' # constructing learners with default settings
#' sl_stack_easy <- make_learner_stack("Lrnr_mean", "Lrnr_glm_fast",
#'                                     "Lrnr_xgboost")
#'
#' # constructing learners with arguments passed in
#' sl_stack <- make_learner_stack("Lrnr_mean",
#'                                list("Lrnr_condensier", nbins = 5,
#'                                     bin_method = "equal.len", pool = FALSE))

make_learner_stack <- function(...) {
  learner_lists <- list(...)

  learners <- lapply(learner_lists, function(learner_list) {
    if (!is.list(learner_list)) {
      learner_list <- list(learner_list)
    }

    learner <- do.call(make_learner, learner_list)
    return(learner)
  })

  # generate output Stack object and return model stack
  out <- make_learner(Stack, learners)
  return(out)
}
