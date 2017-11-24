#' Make a stack of sl3 learners
#'
make_learner_stack <- function(...) {
  learners <- do.call(make_learner, ...)
  return(make_learner(Stack, learners))
}

