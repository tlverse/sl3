#' Automatically Defined Metalearner
#'
#' A sensible metalearner based on \code{\link{Lrnr_solnp}} is chosen based on outcome type.
#' This amounts to choosing an appropriate loss function and combination function.
#' \tabular{rcc}{
#' Outcome Type \tab Combination Function \tab Loss Function \cr
#' binomial \tab metalearner_logistic_binomial \tab loss_squared_error \cr
#' categorical \tab metalearner_linear_multinomial \tab loss_loglik_multinomial \cr
#' continuous \tab metalearner_linear \tab loss_squared_error \cr
#' multivariate \tab metalearner_linear_multivariate \tab loss_squared_error_multivariate \cr
#' }
#'
#' @param outcome_type a Variable_Type object
default_metalearner <- function(outcome_type) {
  outcome_type <- outcome_type$type
  if (outcome_type %in% c("constant", "binomial")) {
    learner <- make_learner(Lrnr_solnp, metalearner_logistic_binomial, loss_squared_error)
  } else if (outcome_type == "categorical") {
    learner <- make_learner(Lrnr_solnp, metalearner_linear_multinomial, loss_loglik_multinomial)
  } else if (outcome_type == "continuous") {
    learner <- make_learner(Lrnr_solnp, metalearner_linear, loss_squared_error)
  } else if (outcome_type == "multivariate") {
    learner <- make_learner(Lrnr_solnp, metalearner_linear_multivariate, loss_squared_error_multivariate)
  } else {
    stop(sprintf("Outcome type %s does not have a default metalearner. Please specify your own"))
  }

  return(learner)
}
