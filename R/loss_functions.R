#' Loss Function Definitions
#'
#' Loss functions for use in evaluating learner fits.
#'
#' @param pred A vector of predicted values
#' @param observed A vector of observed values
#'
#' @return A vector of loss values
#'
#' @name loss_functions

# SQUARED ERROR LOSS
#
#' @rdname loss_functions
#'
#' @export
loss_squared_error <- function(pred, observed) {
  out <- (pred - observed)^2
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS
#
# Assumes pred is p(Y = observed); therefore, observed is not actually used.
#
#' @rdname loss_functions
#'
#' @export
loss_loglik_true_cat <- function(pred, observed) {
  out <- -log(bound(pred))
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS FOR BINOMIAL OUTCOMES
#
#' @rdname loss_functions
#'
#' @export
loss_loglik_binomial <- function(pred, observed) {
  out <- -1 * ifelse(observed == 1, log(bound(pred)), log(1 - bound(pred)))
  return(out)
}

# NEGATIVE LOG-LIKELIHOOD LOSS FOR MULTINOMIAL OUTCOMES
#
# Assumes predicted probabilities are "packed" into a single vector
#
#' @rdname loss_functions
#'
#' @export
loss_loglik_multinomial <- function(pred, observed) {
  # make index matrix
  index_mat <- cbind(seq_along(observed), observed)
  unpacked <- unpack_predictions(pred)
  class_liks <- log(bound(unpacked[index_mat]))
  return(-1 * class_liks)
}

#' SQUARED ERROR LOSS FOR MULTIVARIATE (LOSS AVERAGED ACROSS OUTCOMES)
#'
#' @note Assumes predicted probabilities are "packed" into a single vector.
#'
#' @rdname loss_functions
#'
#' @export
loss_squared_error_multivariate <- function(pred, observed) {
  unpacked <- unpack_predictions(pred)
  losses <- rowMeans((unpacked - observed)^2)
  return(losses)
}

#' Risk Estimation
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
risk <- function(pred, observed, loss = loss_squared_error, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, length(observed))
  }
  risk <- weighted.mean(loss(observed, pred), weights)
  return(risk)
}

utils::globalVariables(c("id", "loss", "obs", "pred", "wts"))
#' Cross-validated Risk Estimation
#'
#' Estimates the cross-validated risk for a given learner and loss function.
#'
#' @param learner A trained learner object.
#' @param loss_fun A valid loss function. See \code{\link{loss_functions}}.
#' @param coefs A \code{numeric} vector of coefficients.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table ":=" set setnames setorderv
#' @importFrom origami cross_validate validation fold_index
#' @importFrom stats sd
cv_risk <- function(learner, loss_fun, coefs = NULL) {
  assertthat::assert_that("cv" %in% learner$properties,
    msg = "learner is not cv-aware"
  )

  task <- learner$training_task$revere_fold_task("validation")
  preds <- learner$predict_fold(task, "validation")
  if (!is.data.table(preds)) {
    preds <- data.table::data.table(preds)
    data.table::setnames(preds, names(preds), learner$name)
  }
  
  # \begin ----- check if hierarchical
  # TBD find the names of sub learners in Lrnr_cv
  learner_names <- sapply(learner$params$learner$params$learners, function(x) x$name)
  # TBD find the names of sub learners in Lrnr_sl fit
  if ('Lrnr_sl' %in% class(learner) & learner$is_trained){
    learner_names <- names(learner$learner_fits)
  }
  if (length(grep("Lrnr_aggregate", learner_names)) != 0) {
    task <- aggregate_task(task)
  }
  # \end ----- check if hierarchical
  
  get_obsdata <- function(fold, task) {
      list(loss_dt = data.table::data.table(
        fold_index = origami::fold_index(),
        index = origami::validation(),
        obs = origami::validation(task$Y),
        id = origami::validation(task$id),
        wts = origami::validation(task$weights)
      ))
    }
  loss_dt <- origami::cross_validate(get_obsdata, task$folds, task)$loss_dt
  data.table::setorderv(loss_dt, c("index", "fold_index"))
  loss_dt <- cbind(loss_dt, preds)
  pred_cols <- colnames(preds)
  loss_long <- melt(loss_dt,
    measure.vars = pred_cols,
    variable.name = "learner",
    value.name = "pred"
  )
  loss_long[, `:=`(loss = wts * loss_fun(pred, obs))]

  # average loss in id-fold cluster
  loss_by_id <- loss_long[, list(loss = mean(loss, na.rm = TRUE)),
    by = list(learner, id, fold_index)
  ]

  # get learner level loss statistics
  loss_stats <- loss_by_id[, list(
    coefficients = NA_real_,
    risk = mean(loss, na.rm = TRUE),
    se = (1 / sqrt(.N)) * stats::sd(loss)
  ), by = list(learner)]

  # get fold-learner level loss statistics
  loss_fold_stats <- loss_by_id[, list(risk = mean(loss, na.rm = TRUE)),
    by = list(learner, fold_index)
  ]
  loss_stats_fold <- loss_fold_stats[, list(
    fold_sd = stats::sd(risk, na.rm = TRUE),
    fold_min_risk = min(risk, na.rm = TRUE),
    fold_max_risk = max(risk, na.rm = TRUE)
  ), by = list(learner)]

  risk_dt <- loss_stats <- merge(loss_stats, loss_stats_fold, by = "learner")

  if (!is.null(coefs)) {
    data.table::set(risk_dt, , "coefficients", coefs)
  }
  return(risk_dt)
}
