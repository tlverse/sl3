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
#' @param task the task used for training and performance assessment.
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
#'   lrnr_sl = sl, task = cpp_task, eval_fun = loss_squared_error
#' )
#' }
cv_sl <- function(lrnr_sl, task, eval_fun) {
  # check arguments
  if (!inherits(lrnr_sl, "Lrnr_sl")) {
    stop("lrnr_sl must be a Lrnr_sl object")
  }
  # cross-validate the SL
  cv_sl_lrnr <- make_learner(Lrnr_cv, lrnr_sl, full_fit = TRUE)
  cv_sl_fit <- cv_sl_lrnr$train(task)
  full_fit <- cv_sl_fit$fit_object$full_fit
  full_risk <- full_fit$cv_risk(eval_fun)
  sl_risk <- cv_sl_fit$cv_risk(eval_fun)
  # replace revere cv sl risk with nested cv sl risk as needed
  stack_risks <- full_risk[full_risk$learner != "SuperLearner"]
  set(sl_risk, , "learner", "SuperLearner")
  risks <- rbind(stack_risks, sl_risk)

  # remove full-fit SL coefficients from the table
  risks <- risks[, -"coefficients", with = FALSE]

  # gather the fold-specific SL coefficients and validation set predictions
  if (!is.null(names(full_fit$coefficients))) {
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

  cv_preds <- do.call(
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
      cand_pred_i <- data.table(do.call(cbind, cand_pred_i))
      colnames(cand_pred_i) <- names(cv_sl_fit$fit_object$fold_fits[[i]]$learner_fits)

      # SL predictions for fold i validation task (NOT SEEN BY SL)
      sl_pred_i <- cv_sl_fit$fit_object$fold_fits[[1]]$predict(valid_task_i)

      data.table(
        "row_index" = cv_sl_fit$training_task$folds[[i]]$validation_set,
        "SuperLearner" = sl_pred_i,
        cand_pred_i
      )
    })
  )
  cv_preds <- cv_preds[order(row_index), -"row_index", with = F]

  print("Cross-validated risk:")
  print(risks)

  return(list("cv_risk" = risks, "cv_sl_coefs" = coefs, "cv_preds" = cv_preds))
}
