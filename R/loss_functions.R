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

#' FACTORY LOSS FUNCTION FOR ROCR PERFORMANCE MEASURES WITH BINARY OUTCOMES
#'
#' Factory function for estimating an ROCR-based loss for a given ROCR measure.
#' The ROCR-based loss is simply one minus the performance measure.
#'
#' @note This loss does not take into account weights. In order to use this
#' loss, it must first be defined with respect to the \code{\link[ROCR]}}
#' performance measure of interest, and then the user-defined loss can be used.
#'
#' @param pred A vector of predicted values.
#' @param observed A vector of binary observed values.
#' @param measure A character indicating which \pkg{ROCR} performance measure
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
#' @param ... Optional arguments to specific \code{\link[ROCR]} performance
#' measures. See \code{\link[ROCR]{performance}} for more detail.
#'
#' @rdname loss_functions
#'
#' @importFrom ROCR prediction performance
#'
#' @export
#'
custom_loss_performance <- function(measure, cutoff = 0.5, name = NULL, ...) {
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

    loss <- 1 - performance
    attributes(loss)$transform <- "performance"
    attributes(loss)$name <- ifelse(is.null(name), measure, name)
    return(loss)
  }
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
  if (!is.null(attr(losses, "transform"))) {
    losses <- transform_losses(losses, attr(losses, "transform"))
  }

  # get weights
  if (is.null(weights)) {
    if (length(losses) == length(observed)) {
      weights <- rep(1, length(observed))
    } else if (length(losses) == 1) {
      weights <- 1
    }
  } else {
    if (length(losses) == 1) {
      warning(
        "Dropping observation-level weights from the loss calculation, ",
        "since supplied loss function outputs a single value, not a vector."
      )
      weights <- 1
    }
  }

  # calculate risk
  risk <- weighted.mean(losses, weights)
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
cv_risk <- function(learner, loss_fun = NULL, coefs = NULL) {
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
  losses <- loss_fun(loss_long[["pred"]], loss_long[["obs"]])

  if (length(unique(losses)) == 1) {
    if (task$has_node("weights")) {
      warning(
        "Dropping observation-level weights from the loss calculations, ",
        "since supplied loss function outputs a scalar, not a vector."
      )
    }

    # try stratifying by id
    loss_by_id <- tryCatch(
      {
        loss_long[, list(loss = loss_fun(pred, obs)),
          by = list(learner, id, fold_index)
        ]
      },
      error = function(c) {
        loss_long[, list(loss = loss_fun(pred, obs)),
          by = list(learner, fold_index)
        ]
      }
    )

    # transform as required
    if (!is.null(attr(losses, "transform"))) {
      loss_by_id[, loss := transform_losses(loss, attr(losses, "transform"))]
    }
  } else {
    loss_long <- cbind(loss_long, loss = loss_long[["wts"]] * losses)

    if (!is.null(attr(losses, "transform"))) {
      loss_long[, loss := transform_losses(loss, attr(losses, "transform"))]
    }

    loss_by_id <- loss_long[, list(loss = mean(loss, na.rm = TRUE)),
      by = list(learner, id, fold_index)
    ]
  }

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

  if (!is.null(attr(losses, "name"))) {
    colnames(risk_dt) <- gsub("risk", attr(losses, "name"), colnames(risk_dt))
  }
  return(risk_dt)
}

#' Transform losses for loss functions with transform attribute
#'
#' @param losses Numeric vector of losses returned by a loss function
#' @param type A valid transformation type.
transform_losses <- function(losses, type) {
  if (type == "performance") {
    transformed_losses <- 1 - losses
  }
  return(transformed_losses)
}
