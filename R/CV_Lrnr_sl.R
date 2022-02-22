#' Estimates cross-validated risk of the Super Learner
#' @param lrnr_sl a \code{Lrnr_sl} object specifying the Super Learner.
#' @param task the task used for training and performance assessment.
#' @param eval_fun the evaluation function, either a loss or risk function, for
#'  evaluating the Super Learner's predictions.
#' @export
CV_lrnr_sl <- function(lrnr_sl, task, eval_fun) {
  # check arguments
  if (!inherits(lrnr_sl, "Lrnr_sl")) {
    stop("lrnr_sl must be a Lrnr_sl object")
  }
  # cross-validate the SL
  cv_sl <- make_learner(Lrnr_cv, lrnr_sl, full_fit = TRUE)
  cv_sl_fit <- cv_sl$train(task)
  full_fit <- cv_sl_fit$fit_object$full_fit
  # TODO: extract loss function from lrnr_sl where possible
  full_risk <- full_fit$cv_risk(eval_fun)
  sl_risk <- cv_sl_fit$cv_risk(eval_fun)
  # replace revere cv sl risk with nested cv sl risk as needed
  stack_risks <- full_risk[full_risk$learner != "SuperLearner"]
  set(sl_risk, , "learner", "SuperLearner")
  risks <- rbind(stack_risks, sl_risk)

  # remove full-fit SL coefficients from the table
  risks <- risks[, -"coefficients", with = FALSE]

  # gather the fold-specific SL coefficients
  if (!is.null(names(full_fit$coefficients))) {
    coefs <- do.call(
      rbind,
      lapply(seq_along(cv_sl_fit$fit_object$fold_fits), function(i) {
        coefs <- coef(cv_sl_fit$fit_object$fold_fits[[i]]$fit_object$cv_meta_fit)
        c("fold" = i, coefs)
      })
    )
  } else {
    coefs <- lapply(seq_along(cv_sl_fit$fit_object$fold_fits), function(i) {
      cv_sl_fit$fit_object$fold_fits[[i]]$fit_object$cv_meta_fit
    })
  }

  print("Cross-validated risk:")
  print(risks)

  return_obj <- list("coef" = coefs, "cv_risk" = risks)
  return(return_obj)
}
