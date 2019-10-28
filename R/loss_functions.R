#' Loss Function Definitions
#'
#' Loss functions for use in evaluating learner fits
#'
#' @param pred A vector of predicted values
#' @param observed A vector of observed values
#'
#' @return A vector of loss values
#'
#' @name loss_functions
#

# SQUARED ERROR LOSS
#
#' @rdname loss_functions
#'
#' @export
#
loss_squared_error <- function(pred, observed) {
  out <- (pred - observed)^2
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS
#
# assumes pred is p(Y = observed); therefore, observed is not actually used
#
#' @rdname loss_functions
#'
#' @export
#
loss_loglik_true_cat <- function(pred, observed) {
  out <- -log(pred)
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS FOR BINOMIAL OUTCOMES
#
#' @rdname loss_functions
#'
#' @export
#
loss_loglik_binomial <- function(pred, observed) {
  out <- -1 * ifelse(observed == 1, log(pred), log(1 - pred))
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS FOR MULTINOMIAL OUTCOMES
#
# Assumes predicted probabilities are "packed" into a single vector
#
#' @rdname loss_functions
#'
#' @export
#
loss_loglik_multinomial <- function(pred, observed) {
  # make index matrix
  index_mat <- cbind(seq_along(observed), observed)
  unpacked <- unpack_predictions(pred)
  class_liks <- log(unpacked[index_mat])
  return(-1 * class_liks)
}

#' Risk Esimation
#'
#' Estimates a risk for a given set of predictions and loss function.
#'
#' @param pred A vector of predicted values.
#' @param observed A vector of observed values.
#' @param loss A loss function. For options, see \link{loss_functions}.
#' @param weights A vector of weights.
#'
#' @importFrom stats weighted.mean
#'
#' @export
#
risk <- function(pred, observed, loss = loss_squared_error, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, length(observed))
  }
  risk <- weighted.mean(loss(observed, pred), weights)
  return(risk)
}

#' @importFrom stats sd
cv_risk <- function(learner, loss_fun, coefs = NULL) {
  # warning(paste("cv_risks are for demonstration purposes only.",
  # "Don't trust these for now."))
  if (!("cv" %in% learner$properties)) {
    stop("learner is not cv-aware")
  }

  task <- learner$training_task
  task <- task$revere_fold_task("validation")
  preds <- learner$predict_fold(task, "validation")
  if (!is.data.table(preds)) {
    preds <- data.table(preds)
    setnames(preds, names(preds), learner$name)
  }
  losses <- preds[, lapply(.SD, loss_fun, task$Y)]
  # multiply each loss (L(O_i)) by the weights (w_i):
  losses_by_id <- losses[, lapply(.SD, function(loss) {
    task$weights *
      loss
  })]
  # for clustered data, this will first evaluate the mean weighted loss
  # within each cluster (subject) before evaluating SD
  losses_by_id <- losses_by_id[, lapply(.SD, function(loss) {
    mean(loss, na.rm = TRUE)
  }), by = task$id]
  losses_by_id[, "task" := NULL]

  # n_obs for clustered data (person-time observations), should be equal to
  # number of independent subjects
  n_obs <- nrow(losses_by_id)
  # evaluate risk SE for each learner incorporating: a) weights and b) using
  # the number of independent subjects
  se <- unlist((1 / sqrt(n_obs)) * losses_by_id[, lapply(
    .SD, sd,
    na.rm = TRUE
  )])

  # get fold specific risks
  validation_means <- function(fold, losses, weight) {
    risks <- lapply(
      origami::validation(losses), weighted.mean,
      origami::validation(weight)
    )
    return(as.data.frame(risks))
  }

  fold_risks <- lapply(
    task$folds,
    validation_means,
    losses,
    task$weights
  )

  fold_risks <- rbindlist(fold_risks)
  fold_mean_risk <- apply(fold_risks, 2, mean)
  fold_min_risk <- apply(fold_risks, 2, min)
  fold_max_risk <- apply(fold_risks, 2, max)
  fold_SD <- apply(fold_risks, 2, sd)

  learner_names <- names(preds)

  risk_dt <- data.table::data.table(
    learner = learner_names,
    coefficients = NA * 0.0,
    mean_risk = fold_mean_risk,
    SE_risk = se,
    fold_SD = fold_SD,
    fold_min_risk = fold_min_risk,
    fold_max_risk = fold_max_risk
  )
  if (!is.null(coefs)) {
    set(risk_dt, , "coefficients", coefs)
  }
  return(risk_dt)
}

# Squared Error loss for multivariate (loss averaged across outcomes)
#
# Assumes predicted probabilities are "packed" into a single vector
#
#' @rdname loss_functions
#'
#' @export
#
loss_squared_error_multivariate <- function(pred, observed) {
  unpacked <- unpack_predictions(pred)
  losses <- rowMeans((unpacked - observed)^2)
  return(losses)
}
