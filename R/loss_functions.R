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
  # get losses
  losses <- loss(pred, observed)

  # get weights
  if (is.null(weights)) {
    weights <- rep(1, length(observed))
  }

  # calculate risk
  risk <- weighted.mean(losses, weights)
  return(risk)
}

#' FACTORY RISK FUNCTION FOR ROCR PERFORMANCE MEASURES WITH BINARY OUTCOMES
#'
#' Factory function for estimating an ROCR-based risk for a given ROCR measure,
#' and the risk is defined as one minus the performance measure.
#'
#' @note This risk does not take into account weights. In order to use this
#' risk, it must first be instantiated with respect to the \pkg{ROCR}
#' performance measure of interest, and then the user-defined function can be
#' used.
#'
#' @param pred A vector of predicted values.
#' @param observed A vector of binary observed values.
#' @param measure A character indicating which \code{ROCR} performance measure
#' to use for evaluation. The \code{measure} must be either cutoff-dependent
#' so a single value can be selected (e.g., "tpr"), or it's value is a scalar
#' (e.g., "aucpr"). For more information, see \code{\link[ROCR]{performance}}.
#' @param cutoff A numeric value specifying the cutoff for choosing a single
#' performance measure from the returned set. Only used for performance measures
#' that are cutoff-dependent and default is 0.5. See
#' \code{\link[ROCR]{performance}} for more detail.
#' @param name An optional character string for user to supply their desired
#' name for the performance measure, which will be used for naming subsequent
#' risk-related tables and metrics (e.g., \code{cv_risk} column names). When
#' \code{name} is not supplied, the \code{measure} will be used for naming.
#' @param ... Optional arguments to specific \code{ROCR} performance
#' measures. See \code{\link[ROCR]{performance}} for more detail.
#'
#' @rdname risk_functions
#'
#' @importFrom ROCR prediction performance
#'
#' @export
#'
custom_ROCR_risk <- function(measure, cutoff = 0.5, name = NULL, ...) {
  function(pred, observed) {

    # remove NA, NaN, Inf values
    if (any(is.na(pred)) | any(is.nan(pred)) | any(is.infinite(pred))) {
      to_rm <- unique(which(is.na(pred) | is.nan(pred) | is.infinite(pred)))
      pred <- pred[-to_rm]
      observed <- observed[-to_rm]
    }

    # get ROCR performance object
    args <- list(...)
    args$measure <- measure
    args$prediction.obj <- ROCR::prediction(pred, observed)
    performance_object <- do.call(ROCR::performance, args)

    # get single performance value from the performance object
    performance <- unlist(performance_object@y.values)
    if (length(performance) != 1) {
      if (performance_object@x.name != "Cutoff") {
        stop(
          "Multiple performance values returned, but the measure is not",
          "cutoff-depedent. Check that the supplied measure is valid."
        )
      } else {
        # select the performance closest to the supplied cutoff
        performance <- performance[
          which.min(abs(unlist(performance_object@x.values) - cutoff))
        ]
      }
    }

    risk <- 1 - performance
    attributes(risk)$risk <- TRUE
    attributes(risk)$transform <- "performance"
    attributes(risk)$name <- ifelse(is.null(name), measure, name)
    return(risk)
  }
}

utils::globalVariables(c("id", "loss", "obs", "pred", "wts"))
#' Cross-validated Risk Estimation
#'
#' Estimates the cross-validated risk for a given learner and evaluation
#' function, which can be either a loss or a risk function.
#'
#' @param learner A trained learner object.
#' @param eval_fun A valid loss or risk function. See
#' \code{\link{loss_functions}} and \code{\link{risk_functions}}.
#' @param coefs A \code{numeric} vector of coefficients.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table data.table ":=" set setnames setorderv
#' @importFrom origami cross_validate validation fold_index
#' @importFrom stats sd
cv_risk <- function(learner, eval_fun = NULL, coefs = NULL) {
  assertthat::assert_that(
    "cv" %in% learner$properties,
    msg = "learner is not cv-aware"
  )

  task <- learner$training_task$revere_fold_task("validation")
  preds <- learner$predict_fold(task, "validation")
  if (!is.data.table(preds)) {
    preds <- data.table::data.table(preds)
    data.table::setnames(preds, names(preds), learner$name)
  }

  get_obsdata <- function(fold, task) {
    list(dt = data.table::data.table(
      fold_index = origami::fold_index(),
      index = origami::validation(),
      obs = origami::validation(task$Y),
      id = origami::validation(task$id),
      wts = origami::validation(task$weights)
    ))
  }
  dt <- origami::cross_validate(get_obsdata, task$folds, task)$dt
  data.table::setorderv(dt, c("index", "fold_index"))
  dt <- cbind(dt, preds)
  dt_long <- melt(
    dt,
    measure.vars = colnames(preds),
    variable.name = "learner",
    value.name = "pred"
  )

  eval_result <- eval_fun(dt_long[["pred"]], dt_long[["obs"]])

  if (!is.null(attr(eval_result, "risk"))) {
    # try stratifying by id
    eval_by_id <- tryCatch(
      {
        dt_long[, list(
          eval_result = eval_fun(pred, obs)
        ), by = list(learner, id, fold_index)]
      },
      error = function(c) {
        dt_long[, list(
          eval_result = eval_fun(pred, obs)
        ), by = list(learner, fold_index)]
      }
    )

    # transform as required
    if (!is.null(attr(eval_result, "transform"))) {
      type <- attr(eval_result, "transform")
      eval_by_id[, eval_result := transform_risk(eval_result, type)]
    }
  } else {
    dt_long <- cbind(dt_long, eval_result = dt_long[["wts"]] * eval_result)

    eval_by_id <- dt_long[, list(
      eval_result = mean(eval_result, na.rm = TRUE)
    ), by = list(learner, id, fold_index)]
  }

  # get learner-level evaluation statistics
  eval_stats <- eval_by_id[, list(
    coefficients = NA_real_,
    risk = mean(eval_result, na.rm = TRUE),
    se = (1 / sqrt(.N)) * stats::sd(eval_result)
  ), by = list(learner)]

  # get learner-level evaluation statistics by fold
  eval_fold_stats <- eval_by_id[, list(
    risk = mean(eval_result, na.rm = TRUE)
  ), by = list(learner, fold_index)]
  eval_stats_fold <- eval_fold_stats[, list(
    fold_sd = stats::sd(risk, na.rm = TRUE),
    fold_min_risk = min(risk, na.rm = TRUE),
    fold_max_risk = max(risk, na.rm = TRUE)
  ), by = list(learner)]

  risk_dt <- eval_stats <- merge(eval_stats, eval_stats_fold, by = "learner")

  if (!is.null(coefs)) {
    data.table::set(risk_dt, , "coefficients", coefs)
  }

  if (!is.null(attr(eval_result, "name"))) {
    colnames(risk_dt) <- gsub(
      "risk", attr(eval_result, "name"), colnames(risk_dt)
    )
  }
  return(risk_dt)
}

#' Transform risks for a risk function with transform attribute
#'
#' @param risk Numeric vector or scalar of risk(s) returned by a risk function
#' @param type A valid transformation type
transform_risk <- function(risk, type) {
  if (type == "performance") {
    transformed_risk <- 1 - risk
  } else if (type != "performance") {
    stop(paste0("Supplied transformation type, ", type, ", is not supported"))
  }
  return(transformed_risk)
}
