#' Automatically Defined Metalearner
#'
#' A sensible metalearner is chosen based on the outcome type.
#'
#' @details
#'  For binary and continuous outcome types, the default metalearner is
#'  non-negative least squares (NNLS) regression (\code{\link{Lrnr_nnls}}), and
#'  for others the metalearner is \code{\link{Lrnr_solnp}} with an appropriate
#'  loss and combination function, shown in the table below.
#'
#'  | Outcome Type | Combination Function | Loss Function |
#'  |:-------------|:---------------------| :-------------|
#'  | categorical | metalearner_linear_multinomial | loss_loglik_multinomial |
#'  | multivariate | metalearner_linear_multivariate | loss_squared_error_multivariate |
#'
#' @param outcome_type a Variable_Type object
default_metalearner <- function(outcome_type) {
  outcome_type <- outcome_type$type
  if (outcome_type %in% c("constant", "binomial")) {
    learner <- make_learner(Lrnr_nnls)
  } else if (outcome_type == "categorical") {
    learner <- make_learner(
      Lrnr_solnp, metalearner_linear_multinomial,
      loss_loglik_multinomial
    )
  } else if (outcome_type == "continuous") {
    learner <- make_learner(Lrnr_nnls)
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
