#' Automatically Defined Metalearner
#'
#' A sensible metalearner based on \code{\link{Lrnr_solnp}} is chosen based on
#' outcome type. This amounts to choosing an appropriate loss function and
#' combination function.
#'
#' @details
#'  | Outcome Type | Combination Function | Loss Function |
#'  |:-------------|:---------------------| :-------------|
#'  | binomial | metalearner_logistic_binomial | loss_squared_error |
#'  | categorical | metalearner_linear_multinomial | loss_loglik_multinomial |
#'  | continuous | metalearner_linear | loss_squared_error |
#'  | multivariate | metalearner_linear_multivariate | loss_squared_error_multivariate |
#'
#' @param outcome_type a Variable_Type object
default_metalearner <- function(outcome_type) {
  outcome_type <- outcome_type$type
  if (outcome_type %in% c("constant", "binomial")) {
    learner <- make_learner(
      Lrnr_solnp, metalearner_logistic_binomial,
      loss_squared_error
    )
  } else if (outcome_type == "categorical") {
    learner <- make_learner(
      Lrnr_solnp, metalearner_linear_multinomial,
      loss_loglik_multinomial
    )
  } else if (outcome_type == "continuous") {
    learner <- make_learner(
      Lrnr_solnp, metalearner_linear,
      loss_squared_error
    )
  } else if (outcome_type == "multivariate") {
    learner <- make_learner(
      Lrnr_solnp, metalearner_linear_multivariate,
      loss_squared_error_multivariate
    )
  } else {
    stop(sprintf("Outcome type %s does not have a default metalearner."))
  }
  return(learner)
}
