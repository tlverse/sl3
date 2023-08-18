#' Cross-validated Super Learner
#'
#' @return A list of containing the following: the table of cross-validated
#'  risk estimates of the super learner and the candidate learners used to
#'  construct it, and either a matrix of coefficients for the super learner
#'  on each fold or a list for the metalearner fit on each fold.
#'
#'
#' @param lrnr_sl a \code{\link{Lrnr_sl}} object specifying the Super Learner.
#'  Note that the \code{cv_control} argument of \code{\link{Lrnr_sl}} can be
#'  specified to control the inner cross-validation of \code{lrnr_sl}, as shown
#'  in the example.
#' @param eval_fun the evaluation function, either a loss or risk function, for
#'  evaluating the Super Learner's predictions.
#'
#' @importFrom stats coef
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(cpp_imputed)
#' cpp_task <- sl3_Task$new(
#'   data = cpp_imputed,
#'   covariates = c("apgar1", "apgar5", "parity", "gagebrth", "mage"),
#'   outcome = "haz"
#' )
#' glm_lrn <- Lrnr_glm$new()
#' ranger_lrn <- Lrnr_ranger$new()
#' lasso_lrn <- Lrnr_glmnet$new()
#' sl <- Lrnr_sl$new(
#'   learners = list(glm_lrn, ranger_lrn, lasso_lrn),
#'   cv_control = list(V = 5),
#'   verbose = FALSE
#' )
#' cv_sl_object <- cv_sl(
#'   lrnr_sl = sl, eval_fun = loss_squared_error
#' )
#' }
cv_sl <- function(lrnr_sl, eval_fun) {
  # check arguments
  if (!inherits(lrnr_sl, "Lrnr_sl")) {
    stop("lrnr_sl must be a Lrnr_sl object")
  }

  # cross-validate the SL
  cv_sl_lrnr <- make_learner(Lrnr_cv, lrnr_sl, full_fit = TRUE)
  cv_sl_fit <- cv_sl_lrnr$train(task = lrnr_sl$training_task)

  # gather the fold-specific SL coefficients
  if (!is.null(names(cv_sl_fit$fit_object$full_fit$coefficients))) {
    coefs <- do.call(
      rbind,
      lapply(seq_along(cv_sl_fit$fit_object$fold_fits), function(i) {
        sl_coefs_i <- stats::coef(
          cv_sl_fit$fit_object$fold_fits[[i]]$fit_object$cv_meta_fit
        )
        c("fold" = i, sl_coefs_i)
      })
    )
  } else {
    coefs <- lapply(seq_along(cv_sl_fit$fit_object$fold_fits), function(i) {
      cv_sl_fit$fit_object$fold_fits[[i]]$fit_object$cv_meta_fit
    })
  }

  # gather the fold-specific SL and candidate learner predictions and observations
  cv_obs_preds <- do.call(
    rbind,
    lapply(seq_along(cv_sl_fit$fit_object$fold_fits), function(i) {
      # fold i validation task
      valid_task_i <- cv_sl_fit$training_task$subset_task(
        cv_sl_fit$training_task$folds[[i]]$validation_set
      )
      # candidate predictions for fold i validation task
      cand_pred_i <- lapply(
        seq_along(cv_sl_fit$fit_object$fold_fits[[i]]$learner_fits), function(k) {
          cv_sl_fit$fit_object$fold_fits[[i]]$learner_fits[[k]]$predict(valid_task_i)
        }
      )
      cand_pred_i <- data.table::as.data.table(do.call(cbind, cand_pred_i))
      colnames(cand_pred_i) <- names(cv_sl_fit$fit_object$fold_fits[[i]]$learner_fits)

      # SL predictions for fold i validation task (NOT SEEN BY SL)
      sl_pred_i <- cv_sl_fit$fit_object$fold_fits[[1]]$predict(valid_task_i)

      # compile CV predictions & observed data for fold i's validation task
      data.table::as.data.table(cbind(
        "index" = valid_task_i$row_index,
        "fold_index" = rep(i, valid_task_i$nrow),
        "id" = valid_task_i$id,
        "wts" = valid_task_i$weights,
        "obs" = valid_task_i$Y,
        cand_pred_i,
        "SuperLearner" = sl_pred_i
      ))
    })
  )

  # retain the CV predictions for output
  preds_cols <- colnames(cv_obs_preds)[6:ncol(cv_obs_preds)]
  cv_preds <- cv_obs_preds[order(index), preds_cols, with = F]

  # calculate CV risk
  data.table::setorderv(cv_obs_preds, c("index", "fold_index"))
  dt_long <- melt(
    cv_obs_preds,
    measure.vars = preds_cols,
    variable.name = "learner",
    value.name = "pred"
  )
  eval_result <- eval_fun(dt_long[["pred"]], dt_long[["obs"]])

  if (!is.null(attr(eval_result, "loss")) && !attr(eval_result, "loss")) {
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
  } else {
    dt_long <- cbind(dt_long, eval_result = dt_long[["wts"]] * eval_result)

    eval_by_id <- dt_long[, list(
      eval_result = mean(eval_result, na.rm = TRUE)
    ), by = list(learner, id, fold_index)]
  }

  # get learner-level evaluation statistics
  eval_stats <- eval_by_id[, list(
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

  if (!is.null(attr(eval_result, "name"))) {
    colnames(risk_dt) <- gsub(
      "risk", attr(eval_result, "name"), colnames(risk_dt)
    )
  }

  print("Cross-validated risk:")
  print(risk_dt)

  return(list("cv_risk" = risk_dt, "cv_sl_coefs" = coefs, "cv_preds" = cv_preds))
}
