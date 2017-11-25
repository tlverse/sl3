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
#'
#' @return An \code{sl3} \code{Stack} consisting of the learners passed in with
#'   the \code{list} argument to this function. This \code{Stack} has all of the
#'   standard methods associated with such objects.
#'
#' @export
#'
#' @examples
#' sl_lrnrs_list <- list("Lrnr_mean", "Lrnr_xgboost")
#' sl_stack <- make_learner_stack(sl_lrnrs_list)
#
make_learner_stack <- function(lrnrs_list) {
  # enforce that input is a list
  stopifnot(class(lrnrs_list) == "list")

  # add standard prefix to learners list
  lrnrs_list_in <- list()
  for (i in seq_along(lrnrs_list)) {
    #if ("character" %in% class(lrnrs_list[[i]])) {
      lrnrs_list_in[[i]] <- eval(parse(text = lrnrs_list[[i]]))
      #lrnrs_list_in[[i]] <- eval(parse(text = paste("sl3::Lrnr",
                                                    #lrnrs_list[[i]],
                                                    #sep = "_")))
    #} else if ("Lrnr_base" %in% class(lrnrs_list[[i]])) {
      #lrnrs_list_in[[i]] <- eval(lrnrs_list[[i]])
    #} else {
      #stop(paste("Learners in input list are not R6 objects or ID strings.",
                 #"\nPlease fix this then try again."))
    #}
  }
  # make learners in list, generate output Stack object, return model stack
  learners <- lapply(lrnrs_list_in, make_learner)
  out <- make_learner(Stack, learners)
  return(out)
}

